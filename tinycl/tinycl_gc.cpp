#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Garbage Collector
// tinycl_gc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_gc.cpp#7 $
//
#define DEBUG_GC        1
#define DEBUG_RS        0
#define DEBUG_TOSPACE   1
#include "./tinycl_gc.h"

#if DEBUG_GC
    #define GC_PRINTF(mp_fmt, ...) CPRINTF(mp_fmt, __VA_ARGS__)
#else
    #define GC_PRINTF(mp_fmt, ...) __noop(__VA_ARGS__)
#endif

namespace TinyCl
{

int Gc::sm_iMaxAge;

static Mm::Area* mapPtrToArea(void* pv)
{
    Int iv = reinterpret_cast<Int>(pv) & ~0xffff;
    Mm::Area* pArea = reinterpret_cast<Mm::Area*>(iv);

    if (pArea < Mm::GetStart() || pArea >= Mm::GetEnd())
    {
        return NULL;
    }

    // FIXME 2008-01-12 yosi@msn.com This test isn't enough. We should use
    // another method to map address to Area.
    while (pArea->m_pSelf != pArea)
    {
        --pArea;
        pArea = reinterpret_cast<Mm::Area*>(pArea->ToInt()& ~0xffff);
    } // while

    return pArea;
} // mapPtrToArea

ForwardCell::ForwardCell(Val x)
{
    #if DEBUG_GC
    {
        Mm::Area* pArea = mapPtrToArea(this);
        ASSERT(pArea->IsFromSpace());
    }
    #endif

    m_classd = reinterpret_cast<Val>(Marker);
    m_value  = x;
} // ForwardCell::ForwardCell

Int ForwardCell::Decode_(const Datum* const x)
{
    switch (x->GetTag())
    {
    case Arch::Tag_FunObj:
        return x->StaticCast<CodeObject>()->ToInt();

    case Arch::Tag_Cons:
        return x->StaticCast<Cons>()->ToInt();

    case Arch::Tag_Record:
        return x->StaticCast<Record>()->ToInt();

    default:
        return NULL;
    } // switch tag
} // ForwardCell::Decode_

bool ForwardCell::Is_(const Datum* const x)
{
    void* pv;

    switch (x->GetTag())
    {
    case Arch::Tag_FunObj:
        pv = x->StaticCast<CodeObject>();
        break;

    case Arch::Tag_Cons:
        pv = x->StaticCast<Cons>();
        break;

    case Arch::Tag_Record:
        pv = x->StaticCast<Record>();
        break;

    default:
        return false;
    } // switch tag

    return reinterpret_cast<ForwardCell*>(pv)->m_classd ==
           reinterpret_cast<Val>(Marker);
} // ForwardCell::Is_

class RsArea : public Mm::Area
{
    private: static const int RehashThreshold = 65;

    public: struct Entry
    {
        Val m_val;
    }; // Entry

    // [E]
    public: class EnumEntry
    {
        private: size_t m_cb;
        private: Entry* m_pBtm;
        private: Entry* m_p;

        public: EnumEntry(const RsArea* p) :
            m_cb(p->m_ofsFree - sizeof(Area)),
            m_pBtm(p->GetBtm<Entry>()),
            m_p(p->GetTop<Entry>() - 1)
        {
            if (! AtEnd()) Next();
        } // EnumEntry

        public: bool AtEnd() const { return 0 == m_cb; }
        public: Entry* Get() const { ASSERT(!AtEnd()); return m_p; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            for (;;)
            {
                m_p++;
                if (0 != m_p->m_val) break;
            }
            m_cb -= sizeof(Entry);
        } // Next
    }; // EnumEntry

    // [G]
    public: const Entry* Get(Val x) const
    {
        Entry* pTop = GetTop<Entry>();
        Entry* pBtm = GetBtm<Entry>();
        Entry* pStart = pTop + hash(x) % (pBtm - pTop);
        Entry* pRunner = pStart;
        do
        {
            if (x == pRunner->m_val) return pRunner;
            if (0 == pRunner->m_val) return NULL;
            pRunner++;
            if (pRunner >= pBtm) pRunner = pTop;
        } while (pRunner != pStart);

        // RsArea is full!
        CAN_NOT_HAPPEN();
    } // Get

    // [H]
    private: static int hash(Val x)
        { return x->ToInt() & Fixnum::MostPositive; }

    // [I]
    public: static bool Is_(const Area* p)
        { return Area::ScanType_Rs == p->GetScanType(); }

    public: bool IsFull() const
        { return m_ofsFree * 100 > m_cbArea * RehashThreshold; }

    // [P]
    public: void Put(Val x)
    {
        Entry* pTop = GetTop<Entry>();
        Entry* pBtm = GetBtm<Entry>();
        Entry* pStart = pTop + hash(x) % (pBtm - pTop);
        Entry* pRunner = pStart;
        do
        {
            if (x == pRunner->m_val)
            {
                return;
            }

            if (0 == pRunner->m_val)
            {
                pRunner->m_val = x;
                m_ofsFree += sizeof(Entry);
                return;
            }

            pRunner++;
            if (pRunner >= pBtm) pRunner = pTop;
        } while (pRunner != pStart);

        // RsArea is full!
        CAN_NOT_HAPPEN();
    } // Put
}; // RsArea

/// <summary>
///   The Remembered Set manager.
/// </summary>
class RsManager : public Gc
{
    static RsArea* sm_rgpRs[Area::Age_Max + 1];

    // ctor
    public: RsManager()
    {
        ::ZeroMemory(&sm_rgpRs, sizeof(sm_rgpRs));
    } // RsManager

    // [F]
    public: static const RsArea::Entry* Find(int iAge, Val x)
    {
        RsArea* pRs = sm_rgpRs[iAge];
        if (NULL == pRs) return NULL;
        return pRs->Get(x);
    } // Find

    // [R]
    public: static void Register(int iAge, Val val)
    {
        RsArea* pRs = sm_rgpRs[iAge];
        if (NULL == pRs)
        {
            pRs = Mm::GetDataArea(
                RsArea::ScanType_Rs | iAge,
                sizeof(RsArea::Entry) )->
                    StaticCast<RsArea>();

            sm_rgpRs[iAge] = pRs;

            #if DEBUG_GC
                GC_PRINTF("new %p %d/%d\n",
                    pRs, pRs->m_ofsFree, pRs->m_cbArea );
            #endif
        }
        else if (pRs->IsFull())
        {
            RsArea* pNewRs = Mm::GetDataArea(
                RsArea::ScanType_Rs | iAge,
                pRs->m_cbArea )->
                    StaticCast<RsArea>();

            sm_rgpRs[iAge] = pNewRs;

            foreach (RsArea::EnumEntry, oEnum, pRs)
            {
                pNewRs->Put(oEnum.Get()->m_val);
            } // for each entry

            #if DEBUG_GC
                GC_PRINTF("grow %p %d/%d\n",
                    pNewRs, pNewRs->m_ofsFree, pNewRs->m_cbArea );
            #endif

            Gc::AddFreeArea(pRs);
        } // if

        pRs->Put(val);
    } // Register

    // [S]
    /// <summary>
    ///   For Gc::UpdateRs
    /// </summary>
    public: static void Set(Area* pArea)
    {
        int iAge = pArea->GetAge();
        ASSERT(NULL == sm_rgpRs[iAge]);
        sm_rgpRs[iAge] = pArea->StaticCast<RsArea>();
    } // Set
}; // RsManager

RsArea* RsManager::sm_rgpRs[Area::Age_Max + 1];

/// <summary>
///   The lisp memory checker
/// </summary>
class Checker : protected Gc
{
    public: void Run()
    {
        foreach (Mm::EnumArea, oEnum, Thread::Get())
        {
            Area* pArea = oEnum.Get();
            switch (pArea->GetScanType())
            {
            case Area::ScanType_Cons:
            case Area::ScanType_Record:
                checkRange(
                    pArea->GetAge(),
                    pArea->GetTop<Val>(),
                    pArea->GetFree<Val>() );
                break;
            } // switch scanType
        } // for each area
    } // Run

    // [C]
    private: void checkRange(int iAge, Val* pStart, Val* pEnd)
    {
        for (const Val* p = pStart; p < pEnd; p++)
        {
            Val obj = *p;
            if (const Area* pArea = mapToArea(obj))
            {
                switch (pArea->GetScanType())
                {
                case Area::ScanType_Code:
                    if (! obj->Is<CodeObject>())
                    {
                        Debugger::Fail("%p isn't in code area.", obj);
                    }
                    break;

                case Area::ScanType_Cons:
                    if (! obj->Is<Cons>())
                    {
                        Debugger::Fail("%p isn't in cons area.", obj);
                    }
                    break;

                case Area::ScanType_Record:
                    if (! obj->Is<Record>())
                    {
                        Debugger::Fail("%p isn't in record area.", obj);
                    }
                    break;

                case Area::ScanType_None:
                    Debugger::Fail("%p points free area.", p);
                    break;
                } // switch scanType

                if (pArea->GetAge() < iAge)
                {
                    Val ptr = Fixnum::Encode(p);
                    const RsArea::Entry* pEntry = RsManager::Find(iAge, ptr);
                    if (NULL == pEntry || pEntry->m_val != ptr)
                    {
                        Debugger::Fail("%p isn't  in RS.", p);
                    }
                } // if old to young
            } else {
                switch (obj->GetTag())
                {
                case Arch::Tag_Cons:
                case Arch::Tag_FunObj:
                    Debugger::Fail("%p isn't in area.", obj);

                case Arch::Tag_Record:
                    if (! obj->Is<Character>())
                    {
                        Debugger::Fail("%p isn't in area.", obj);
                    }
                    break;
                } // switch obj
            } // if
        } // for p
    } // checkRange
}; // Checker

/// <summary>
///   Represents "To Space".
/// </summary>
class ToSpace : protected Gc
{
    private: static AreaManager* sm_rgpAreaMan[Area::Age_Max+1];

    /// <summary>
    ///   List of areas in "To Space" for scanning.
    /// </summary>
    private: static Areas sm_oToSpaceAreas;

    // ctor
    public: ToSpace()
    {
        ASSERT(sm_oToSpaceAreas.IsEmpty());

        int nLimitAge = Gc::GetMaxAge() + 1;
        for (int iAge = 1; iAge <= nLimitAge; iAge++)
        {
            if (NULL == sm_rgpAreaMan[iAge])
            {
                sm_rgpAreaMan[iAge] = new AreaManager;
            }
            else
            {
                sm_rgpAreaMan[iAge]->Reset();
            }
        } // for iAge
    } // ToSpace

    public: ~ToSpace()
    {
        sm_oToSpaceAreas.DeleteAll();
    } // ~ToSpace

    // [A]
    public: static void* Alloc(Area* pFrom, size_t cb)
    {
        int iAge = min(pFrom->GetAge() + 1, Gc::GetMaxAge());

        Area::Scan eScan = pFrom->GetScan();
        uint nFlags = pFrom->GetScanType() | iAge;
        AreaManager* pAreaMan = sm_rgpAreaMan[iAge];
        Area* pArea = pAreaMan->Get(eScan);
        for (;;)
        {
            if (void* pv = pArea->Alloc(cb))
            {
                return pv;
            }

            if (! pArea->IsEmptyArea())
            {
                sm_oToSpaceAreas.Append(pArea);
            }

            pArea = pAreaMan->Alloc(nFlags, cb);

            #if DEBUG_GC
                GC_PRINTF("%p %s.%d\n",
                    pArea, pArea->GetString(), pArea->GetAge() );
            #endif
        } // for
    } // Alloc

    // [E]
    public: class EnumArea
    {
        private: Area::Scan m_eScan;
        private: int        m_iAge;
        private: int        m_nMaxAge;
        private: Area*      m_pArea;

        public: EnumArea(int nMaxAge) :
            m_eScan(Area::Scan_Code),
            m_iAge(1),
            m_nMaxAge(nMaxAge),
            m_pArea(sm_rgpAreaMan[1]->Get(Area::Scan_Code)) {}

        public: bool AtEnd() const
        {
            return m_iAge > m_nMaxAge && Area::Scan_Record == m_eScan;
        } // AtEnd

        public: Area* Get() const
        {
            ASSERT(! AtEnd());
            return m_pArea;
        } // Get

        public: void Next()
        {
            ASSERT(! AtEnd());
            while (m_pArea->m_ofsScan >= m_pArea->m_ofsFree)
            {
                switch (m_eScan)
                {
                case Area::Scan_Code:
                    m_eScan = Area::Scan_Cons;
                    break;

                case Area::Scan_Cons:
                    m_eScan = Area::Scan_Record;
                    break;

                case Area::Scan_Record:
                    m_iAge += 1;
                    if (AtEnd()) return;
                    m_eScan = Area::Scan_Code;
                    break;
                } // switch scan

                m_pArea = sm_rgpAreaMan[m_iAge]->Get(m_eScan);
            } // while
        } // Next
    }; // EnumArea

    // [G]
    public: static Area* GetArea(Area* pArea)
    {
        AreaManager* pAreaMan = sm_rgpAreaMan[pArea->GetAge()];
        return pAreaMan->Get(pArea->GetScan());
    } // GetArea

    // [S]
    public: static void Scan()
    {
        #if DEBUG_GC
            int nNth = 0;
        #endif

        for (;;)
        {
            #if DEBUG_GC
                GC_PRINTF("[%d]\n", ++nNth);
            #endif

            bool fMore = false;

            foreach (Areas::Enum, oEnum, &sm_oToSpaceAreas)
            {
                scanArea(oEnum.Get());
            }

            foreach (ToSpace::EnumArea, oEnum, Gc::GetMaxAge() + 1)
            {
                Area* pArea = oEnum.Get();
                if (scanArea(pArea))
                {
                    fMore = true;
                }
            } // for each area

            if (! fMore)
            {
                break;
            }
        } // for
    } // Scan

    public: static void SetArea(Area* pArea)
    {
        AreaManager* pAreaMan = sm_rgpAreaMan[pArea->GetAge()];
        pAreaMan->Set(pArea);
    } // SetArea
}; // ToSpace

Mm::AreaManager* ToSpace::sm_rgpAreaMan[Area::Age_Max+1];
Mm::Areas ToSpace::sm_oToSpaceAreas;

/// <summary>
///   Represents "From Space". "From Space" contains list of all from space
///   area.
/// </summary>
class FromSpace : protected Gc
{
    private: size_t     m_cbFromArea;
    private: Areas      m_oFromAreas;
    private: Areas      m_oOldAreas;
    private: RsArea*    m_rgpRs[Area::Age_Max + 1];

    // ctor
    public: FromSpace() :
        m_cbFromArea(0)
    {
        sm_oFreeAreas.DeleteAll();

        ::ZeroMemory(m_rgpRs, sizeof(m_rgpRs));

        Mm::EnumArea oEnum(Thread::Get());

        while (! oEnum.AtEnd())
        {
            Area* pArea = oEnum.Get();
            ASSERT(0 == (pArea->m_nFlags & Area::FromSpace));
            oEnum.Next();

            #if DEBUG_GC
                GC_PRINTF("%p %s.%d %d/%d\n",
                    pArea, pArea->GetString(), pArea->GetAge(),
                    pArea->m_ofsFree, pArea->m_cbArea );
            #endif

            switch (pArea->GetScanType())
            {
            case Area::ScanType_BinObj:
            case Area::ScanType_Code:
            case Area::ScanType_Cons:
            case Area::ScanType_Record:
                classifyObjectArea(pArea);
                break;

            case Area::ScanType_DllLink:
            case Area::ScanType_SxHash:
                break;

            case Area::ScanType_None:
                sm_oFreeAreas.Append(pArea);
                break;

            case Area::ScanType_Rs:
                if (pArea->GetAge() > Gc::GetMaxAge())
                {
                    ASSERT(NULL == m_rgpRs[pArea->GetAge()]);
                    m_rgpRs[pArea->GetAge()] = pArea->StaticCast<RsArea>();
                }
                else
                {
                    rememberFreeArea(pArea);
                }
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch scanType
        } // for each area

        #if DEBUG_GC
            GC_PRINTF("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
        #endif
    } // FromSpace

    public: ~FromSpace()
    {
        #if DEBUG_GC
            GC_PRINTF("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
        #endif

        #if 0
        for (int iAge = 0; iAge < lengthof(m_rgpRs); iAge += 1)
        {
            if (Area* pRs = m_rgpRs[iAge])
            {
                AddFreeArea(pRs);
            } // if
        } // for iAge
        #endif

        while (Area* pArea = m_oFromAreas.GetFirst())
        {
            DEBUG_PRINTF("%p\n", pArea);
            ASSERT(pArea->IsFromSpace());
            m_oFromAreas.Delete(pArea);
            AddFreeArea(pArea);
        } // while
    } // ~FromSpace

    // [C]
    private: void classifyObjectArea(Area* pArea)
    {
        if (pArea->IsReadOnly())
        {
            return;
        }

        if (pArea->GetAge() > Gc::GetMaxAge())
        {
            if (pArea->GetScanType() != Area::ScanType_BinObj)
            {
                // We don't need to scan bino area
                m_oOldAreas.Append(pArea);
            }
        }
        else
        {
            DEBUG_PRINTF("From Area: %p\n", pArea);
            pArea->m_nFlags |= Area::FromSpace;
            m_cbFromArea += pArea->m_cbArea;
            m_oFromAreas.Append(pArea);
        }
    } // classifyObjectArea

    // [G]
    public: size_t GetSize() const
        { return m_cbFromArea; }

    // [R]
    // FIXME 2008-09-02 yosi@msn.com Why do we need to have rememberFreeArea?
    private: void rememberFreeArea(Area* pArea)
    {
        #if DEBUG_GC
            GC_PRINTF("%p\n", pArea);
        #endif

        pArea->Reset();

        if (Area* pLast = sm_oFreeAreas.GetFirst())
        {
            if (pLast->GetBtm<Area>() == pArea)
            {
                // Merge to previous
                pLast->m_cbArea += pArea->m_cbArea;
                ::ZeroMemory(pArea, sizeof(*pArea));
                return;
            }
        } // if last

        sm_oFreeAreas.Append(pArea);
    } // rememberFreeArea

    // [U]
    public: void UpdateOld()
    {
        #if DEBUG_GC
            GC_PRINTF("====================\n");
        #endif

        int iLimitAge = Gc::GetMaxAge() + 1;

        foreach (Areas::Enum, oEnum, &m_oOldAreas)
        {
            Area* pArea = oEnum.Get();
            if (pArea->IsModified())
            {
                pArea->m_ofsScan = sizeof(Area);
                scanArea(pArea);
            } // if

            if (pArea->GetAge() == iLimitAge)
            {
                Area* pAllocArea = ToSpace::GetArea(pArea);
                if (pAllocArea->GetFreeSize() < pArea->GetFreeSize())
                {
                    ToSpace::SetArea(pArea);
                }
            } // if
        } // for each area
    } // UpdateOld

    public: void UpdateRs()
    {
        #if DEBUG_GC
            GC_PRINTF("====================\n");
        #endif

        for (
            int iAge = Gc::GetMaxAge() + 1;
            iAge <= Area::Age_Static;
            iAge += 1 )
        {
            RsArea* pRs = m_rgpRs[iAge];
            if (NULL == pRs)
            {
                continue;
            }

            ASSERT(pRs->GetAge() == iAge);

            #if DEBUG_GC
                GC_PRINTF("%p rs.%d %d/%d\n",
                    pRs, iAge, pRs->m_ofsFree, pRs->m_cbArea );
            #endif

            foreach (RsArea::EnumEntry, oEnum, pRs)
            {
                RsArea::Entry* pEntry = oEnum.Get();
                Val val = pEntry->m_val;

                if (val->Is<Fixnum>())
                {
                    updateCell(iAge, val->To<Val>());
                }
                else if (val->Is<CodeObject>())
                {
                    scanCode(iAge, val);
                }
                else
                {
                    CAN_NOT_HAPPEN();
                }
            } // for each entry

            // We don't need RS before GC anymore.
            AddFreeArea(pRs);
        } // for iAge
    } // UpdateRs
}; // FromSpace

/// <summary>
///   The entry point of garbage collector.
/// </summary>
/// <param name="nMax">A maximum age of collecting garbage.</param>
/// <param name="pth">A thread to execute garbage collection.</param>
void Gc::Run(Thread* pth, int nMax)
{
    if (! canGc())
    {
        return;
    }

    sm_iMaxAge = min(max(nMax, MinAge), MaxAge);

    #if DEBUG_GC
        GC_PRINTF("===== GC START %d =============================\n",
            sm_iMaxAge );
    #endif

    sm_cbAllocSoFar = 0;

    foreach (Executive::EnumThread, oEnum, Executive::Get())
    {
        Thread* pThread = oEnum.Get();
        pThread->ResetAlloc();
    } // for each thread

    {
        FromSpace oFromSpace;
        ToSpace   oToSpace;
        RsManager oRsManager;

        foreach (Executive::EnumThread, oEnum, Executive::Get())
        {
            Thread* pThread = oEnum.Get();
            updateThread(pThread);
        } // for each thread

        updateGcAnchors();

        oFromSpace.UpdateOld();
        oFromSpace.UpdateRs();

        // FIXME 2008-01-05 yosi@msn.com NYI Weak object

        #if DEBUG_GC
            GC_PRINTF("***** SCAN ************************************\n");
        #endif

        oToSpace.Scan();

        fixSxHashTable();

        resetWriteWatch();

        pth->m_n = two;
        pth->mv_value[0] = Fixnum::Encode(oFromSpace.GetSize());
        pth->mv_value[1] = Fixnum::Encode(sm_cbAllocSoFar);
    }

    sm_cbAllocSoFar = 0;

    #if DEBUG_GC
        GC_PRINTF("===== GC END =====================================\n");
    #endif

    {
        Checker oChecker;
        oChecker.Run();
    }
} // Gc::Run

// [A]
int Gc::ageOf(Val x)
{
    if (Area* pArea = mapToArea(x))
    {
        return pArea->GetAge();
    } // if

    return Area::Age_Static;
} // Gc::ageOf

// [M]
/// <summary>
///   Map an object to a area containing area.
/// </summary>
/// <param name="x">An object for mapping area.</param>
/// <returns>An area containing specified object.</returns>
Mm::Area* Gc::mapToArea(Val x)
{
    switch (x->GetTag())
    {
    case Arch::Tag_Fixnum:
    case Arch::Tag_Fixnum1:
    case Arch::Tag_Null:
        return NULL;

    case Arch::Tag_Cons:
        return mapPtrToArea(x->StaticCast<Cons>());

    case Arch::Tag_FunObj:
        return mapPtrToArea(x->StaticCast<CodeObject>());

    case Arch::Tag_Record:
        return mapPtrToArea(x->StaticCast<Record>());
    } // switch tag

    CAN_NOT_HAPPEN();
} // Gc::mapToArea

/// <summary>
///   Move survived cons object to area in "To Space".
///   <para>
///     We try to list in linear.
///   </para>
/// </summary>
/// <param name="pArea">A "To Space" area to move.</param>
/// <param name="x">A survived cons object.</param>
Val Gc::moveCons(Area* pArea, Val x)
{
    int iAge = pArea->GetAge();
    Cons* py = reinterpret_cast<Cons*>(ToSpace::Alloc(pArea, sizeof(Cons)));
    Val y = py->Encode();
    Cons* px = x->StaticCast<Cons>();
    for (;;)
    {
        py->m_car = px->m_car;
        Val next  = px->m_cdr;
        py->m_cdr = next;

        new(px) ForwardCell(py->Encode());

        if (! consp(next))
        {
            break;
        }

        if (ForwardCell* p = next->DynamicCast<ForwardCell>())
        {
            py->m_cdr = p->Get();
            break;
        } // if

        if (ageOf(next) != iAge)
        {
            break;
        }

        px = next->StaticCast<Cons>();

        Cons* pNext = reinterpret_cast<Cons*>(
            ToSpace::Alloc(pArea, sizeof(Cons)) );

        py->m_cdr = pNext->Encode();
        py = pNext;
    } // for
    return y;
} // Gc::moveCons

/// <summary>
///   Move survived function object to area in "To Space".
/// </summary>
/// <param name="pArea">A "To Space" area to move.</param>
/// <param name="x">A survived function object.</param>
Val Gc::moveFunObj(Area* pArea, Val x)
{
    CodeObject* p = reinterpret_cast<CodeObject*>(
        x->ToInt() - CodeObject::Tag );

    size_t cb = p->m_length->ToInt();

    void* pvy = ToSpace::Alloc(pArea, cb);
    ::CopyMemory(pvy, p, cb);

    Val y = reinterpret_cast<Val>(
        reinterpret_cast<Int>(pvy) + CodeObject::Tag );

    prepareCode(y, x);

    new(p) ForwardCell(y);

    return y;
} // Gc::moveFunObj

/// <summary>
///   Move survived instance object to area in "To Space".
/// </summary>
/// <param name="pArea">A "To Space" area to move.</param>
/// <param name="x">A survived instance object.</param>
Val Gc::moveInstance(Area* pArea, Val x)
{
    Instance* p = reinterpret_cast<Instance*>(
        x->ToInt() - Instance::Tag );

    void* pvy;

    if (p->m_storage->Is<ForwardCell>())
    {
        const size_t cb = sizeof(Instance);
        pvy = ToSpace::Alloc(pArea, cb);
        ::CopyMemory(pvy, p, cb);
    }
    else
    {
        size_t cb = sizeof(Instance);
        cb += p->GetClassD()->GetFixedSize();
        pvy = ToSpace::Alloc(pArea, cb);
        ::CopyMemory(pvy, p, cb);

        Instance* q = reinterpret_cast<Instance*>(pvy);
        Storage*  pNewStorage = reinterpret_cast<Storage*>(q + 1);
        q->m_storage = pNewStorage->Encode();

        new(p + 1) ForwardCell(q->m_storage);
    } // if

    return reinterpret_cast<Instance*>(pvy)->Encode();
} // Gc::moveInstance

/// <summary>
///   Move survived object to area in "To Space" if needed. This is
///   dispatcher based on object layout.
/// </summary>
/// <param name="pArea">A "To Space" area to move.</param>
/// <param name="x">A survived object.</param>
Val Gc::moveObject(Val x)
{
    Area* pArea = mapToArea(x);
    if (NULL == pArea)
    {
        return x;
    }

    if (! pArea->IsFromSpace())
    {
        return x;
    }

    if (ForwardCell* p = x->DynamicCast<ForwardCell>())
    {
        return p->Get();
    }

    switch (x->GetTag())
    {
    case Arch::Tag_Cons:
        return moveCons(pArea, x);

    case Arch::Tag_FunObj:
        return moveFunObj(pArea, x);

    case Arch::Tag_Record:
        return moveRecord(pArea, x);

    default:
        CAN_NOT_HAPPEN();
    } // switch tag
} // Gc::moveObject

/// <summary>
///   Move survived record object to area in "To Space".
/// </summary>
/// <param name="pArea">A "To Space" area to move.</param>
/// <param name="x">A survived record object.</param>
Val Gc::moveRecord(Area* pArea, Val x)
{
    Record* p = reinterpret_cast<Record*>(x->ToInt() - Record::Tag);
    p->m_classd = resolve(p->m_classd);
    ClassD* pClassD = p->m_classd->StaticCast<ClassD>();

    Val y;
    if (pClassD->m_format->ToInt() == ClassD::Format_Instance)
    {
        y = moveInstance(pArea, x);
    }
    else
    {
        size_t cb = p->GetSize();
        void* pvy = ToSpace::Alloc(pArea, cb);
        ::CopyMemory(pvy, p, cb);
        y = reinterpret_cast<Record*>(pvy)->Encode();
    } // if

    new(p) ForwardCell(y);

    return y;
} // Gc::moveRecord

// [R]
void Gc::remember(int iAge, Val* pval)
{
    if (iAge > ageOf(*pval))
    {
        #if DEBUG_RS
            DEBUG_PRINTF("age=%d loc=%p -> %p\n", iAge, pval, *pval);
        #endif
        RsManager::Register(iAge, Fixnum::Encode(pval));
    }
} // Gc::remember

void Gc::rememberCode(int iAge, Val fn)
{
    RsManager::Register(iAge, fn);
} // Gc::rememberCode

Val Gc::resolve(Val x)
{
    Area* pArea = mapToArea(x);
    if (NULL == pArea) return x;

    if (ForwardCell* p = x->DynamicCast<ForwardCell>())
    {
        return p->Get();
    } // if

    return x;
} // Gc::resolve

// [S]
bool Gc::scanArea(Area* pArea)
{
    #if DEBUG_TOSPACE
        GC_PRINTF("%p %s.%d scan=%d/%d\n",
            pArea, pArea->GetString(), pArea->GetAge(),
            pArea->m_ofsScan, pArea->m_ofsFree );
    #endif

    class EnumCodeScan : public Mm::Area::EnumCode
    {
        public: EnumCodeScan(Area*p) :
            Mm::Area::EnumCode(p) {}
    }; // EnumCodeScan

    if (pArea->m_ofsScan == pArea->m_ofsFree)
    {
        return false;
    }

    uint iAge = pArea->GetAge();

    switch (pArea->GetScanType())
    {
    case Area::ScanType_BinObj:
        return false;

    case Area::ScanType_Cons:
    case Area::ScanType_Record:
        do
        {
            size_t ofsScanEnd = pArea->m_ofsFree;
            scanRange(iAge, pArea->GetScan<Val>(), pArea->GetFree<Val>());
            pArea->m_ofsScan = ofsScanEnd;
        } while (pArea->m_ofsScan != pArea->m_ofsFree);
        break;

    case Area::ScanType_Code:
        do
        {
            size_t ofsScanEnd = pArea->m_ofsFree;
            foreach (EnumCodeScan, oEnum, pArea)
            {
                Val fn = oEnum.Get()->Encode();
                scanCode(iAge, fn);
            } // for each fun
            pArea->m_ofsScan = ofsScanEnd;
        } while (pArea->m_ofsScan != pArea->m_ofsFree);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch type

    return true;
} // Gc::scanArea

void Gc::scanRange(int iAge, Val* pStart, Val* pEnd)
{
    for (Val* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        updateCell(iAge, pRunner);
    } // for
} // Gc::scanRange

// [U]
void Gc::updateCell(uint iAge, Val* pval)
{
    Val obj = moveObject(*pval);
    *pval = obj;
    remember(iAge, pval);
} // Gc::updateCell

void Gc::updateGcAnchors()
{
    #if DEBUG_GC
    {
        int n = 0;
        foreach (Mm::GcAnchors::Enum, oEnum, &Mm::sm_oGcAnchors)
        {
            n += 1;
        } // for each anchor
        GC_PRINTF("#anchors=%d\n", n);
    }
    #endif

    foreach (Mm::GcAnchors::Enum, oEnum, &Mm::sm_oGcAnchors)
    {
        GcAnchor* pAnchor = oEnum.Get();
        pAnchor->m_value = moveObject(pAnchor->Get());
    } // for each anchor
} // Gc::updateGcAnchors

void Gc::updateRange(Val* pStart, Val* pEnd)
{
    for (Val* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner = moveObject(*pRunner);
    } // for
} // Gc::updateRange

/// <summary>
///   Update remembered set before saving image.
/// </summary>
void Gc::UpdateRs()
{
    RsManager oRsManager;

    Areas oCodeAreas;
    Areas oConsAreas;
    Areas oRecoAreas;

    foreach (Mm::EnumArea, oEnum, Thread::Get())
    {
        Area* pArea = oEnum.Get();

        if (0 == pArea->GetAge())
        {
            continue;
        }

        if (pArea->IsReadOnly())
        {
            continue;
        }

        // FIXME 2008-09-02 yosi@msn.com We MUST NOT add pArea to list.

        switch (pArea->GetScanType())
        {
        case Area::ScanType_Code:
            oCodeAreas.Append(pArea);
            break;

        case Area::ScanType_Cons:
            oConsAreas.Append(pArea);
            break;

        case Area::ScanType_Record:
            oRecoAreas.Append(pArea);
            break;

        case Area::ScanType_Rs:
            oRsManager.Set(pArea);
            break;
        } // switch scanType
    } // for each area

    foreach (Areas::Enum, oEnum, &oCodeAreas)
    {
        Area* pArea = oEnum.Get();
        int iAge = pArea->GetAge();
        foreach (Area::EnumCode, oEnum, pArea)
        {
            CodeObject* p = oEnum.Get();
            rememberCodeIf(iAge, p->Encode());
        } // for each code
    } // for each area

    foreach (Areas::Enum, oEnum, &oConsAreas)
    {
        Area* pArea = oEnum.Get();
        int iAge = pArea->GetAge();
        for (
            Val* p = pArea->GetTop<Val>();
            p < pArea->GetFree<Val>();
            p++ )
        {
            remember(iAge, p);
        } // for p
    } // for each area

    foreach (Areas::Enum, oEnum, &oRecoAreas)
    {
        Area* pArea = oEnum.Get();
        int iAge = pArea->GetAge();
        for (
            Val* p = pArea->GetTop<Val>();
            p < pArea->GetFree<Val>();
            p++ )
        {
            remember(iAge, p);
        } // for p
    } // for each area
} // Gc::UpdateRs

void Gc::updateThread(Thread* pth)
{
    #if DEBUG_GC
        GC_PRINTF("%p\n", pth);
    #endif

    pth->m_fn = moveObject(pth->m_fn);

    // mv_value
    {
        int n = static_cast<int>(Fixnum::Decode_(pth->m_n));

        #if DEBUG_GC
            GC_PRINTF("%p thread.mv_value %d\n", pth, n);
        #endif

        updateRange(&pth->mv_value[0], &pth->mv_value[n]);

        ::ZeroMemory(
            &pth->mv_value[n],
            &pth->mv_value[lengthof(pth->mv_value)] - &pth->mv_value[n] );
    }

    // mv_tlv
    {
        Int n = Fixnum::Decode_(VAR(Atlv_indexA));

        #if DEBUG_GC
            GC_PRINTF("%p thread.m_tlv %d\n", pth, n);
        #endif

        updateRange(&pth->mv_tlv[1], &pth->mv_tlv[n+1]);
    }

    // statck
    foreach (Thread::EnumStack, oEnum, pth)
    {
        Frame* pFrame = oEnum.Get();
        updateFrame(pth, pFrame);
    } // for each frame
} // Gc::updateThread

defun(collect_garbageV, (Thread* pth))
{
    Val max_age = Fixnum::Encode(Gc::MaxAge);

    KeyArg rgoKey[] =
    {
        KEYARG(max_age),
    }; // rgoKey

    parseKeys(pth, 0, rgoKey, lengthof(rgoKey));

    check_type(max_age, fixnum);

    Gc::Run(pth, Fixnum::Decode_(max_age));

    return pth->mv_value[0];
} // collect_garbage

} // TinyCl
