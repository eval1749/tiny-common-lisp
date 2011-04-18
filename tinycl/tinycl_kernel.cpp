#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Common Kernel
// tinycl_kernel.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_kernel.cpp#17 $
//
#define DEBUG_CLASSD 0
#include "./tinycl.h"

// warning C4291: 'void *Kernel::Area::operator new(size_t,void *)' :
// no matching operator delete found; memory will not be freed if
// initialization throws an exception
#pragma warning(disable: 4291)

namespace TinyCl
{

using namespace Private;

size_t          Mm::sm_cbAllocSoFar;
Mm::Area        Mm::sm_oEmptyArea;
Mm::Areas       Mm::sm_oFreeAreas;
Mm::GcAnchors   Mm::sm_oGcAnchors;
Mm::Area*       Mm::sm_pCommit;
Mm::Area*       Mm::sm_pEnd;
Mm::Area*       Mm::sm_pStart;

Int Thread::sm_nThread;

Executive* Executive::sm_pExecutive;

static inline size_t computeBinVecSize(
    const ClassD*   const pClassD,
    Val             const length )
{
    ASSERT(NULL != pClassD);

    size_t const n = Fixnum::Decode_(length);

    size_t const bits =
        (n + pClassD->GetExtraLength()) * pClassD->GetElementSize();

    size_t const cbObj = 
        Ceiling(bits, sizeof(Val) * 8) * sizeof(Val) +
        pClassD->GetFixedSize();

    size_t const cb = RoundUp(cbObj, Arch::Align_BinVec);

    return cb;
} // computeBinVecSize

static inline size_t computeVecSize(
    const ClassD*   const pClassD,
    Val             const length )
{
    ASSERT(NULL != pClassD);

    size_t const n = Fixnum::Decode_(length);

    size_t const cbObj = sizeof(Val) * n + pClassD->GetFixedSize();

    size_t const cb = RoundUp(cbObj, Arch::Align_Record);

    return cb;
} // computeVecSize

Thread* Executive::AddThread(Thread* const pThread)
{
    ASSERT(NULL != pThread);
    if (NULL == m_pLastThread)
    {
        m_pFirstThread = pThread;
    }
    else
    {
        m_pLastThread->m_pNext = pThread;
    }

    pThread->m_pPrev = m_pLastThread;
    pThread->m_pNext = NULL;

    m_pLastThread = pThread;

    return pThread;
} // Executive::AddThread

void Executive::EnumThread::Next()
{
    ASSERT(!AtEnd());
    m_pRunner = m_pRunner->m_pNext;
} // Executive::EnumThread::Next

size_t Record::GetSize() const
{
    const ClassD* const pClassD = GetClassD();

    #if DEBUG_CLASSD
        DEBUG_FORMAT("~X~X ~S~%", 
            Fixnum::Encode(ToInt() >> 4),
            Fixnum::Encode(ToInt() & 15),
            pClassD->m_class );
    #endif

    switch (pClassD->GetFormat())
    {
    case ClassD::Format_BinVec:
    {
        Val const length =
            reinterpret_cast<const Layout_data_vector*>(this)->m_length;

        size_t const cb = computeBinVecSize(pClassD, length);

        return cb;
    } // BinVec

    case ClassD::Format_Instance:
        return sizeof(Layout_instance);

    case ClassD::Format_Record:
        return pClassD->GetFixedSize();

    case ClassD::Format_Storage:
    {
        Val storaged = Encode()->StaticCast<Storage>()->m_storaged;
        size_t cb = storaged->StaticCast<ClassD>()->GetFixedSize();
        return cb;
    } // Storage

    case ClassD::Format_Vector:
    {
        Val const length =
            reinterpret_cast<const Layout_data_vector*>(this)->m_length;

        size_t const cb = computeVecSize(pClassD, length);

        return cb;
    } // Vector

    default:
        CAN_NOT_HAPPEN();
    } // switch format
} // Record::GetSize

/// <summary>
///   Return an area to free area.
/// </summary>
void Mm::AddFreeArea(Area* const pArea)
{
    pArea->Reset();

    foreach (Areas::Enum, oEnum, &sm_oFreeAreas)
    {
        Area* pCurr = oEnum.Get();

        if (pCurr->GetBtm<Area>() == pArea)
        {
            // pCurr ... pArea
            pCurr->m_cbArea += pArea->m_cbArea;
            ::ZeroMemory(pArea, sizeof(*pArea));

            if (Area* pNext = pCurr->GetNext())
            {
                if (pCurr->GetBtm<Area>() == pNext)
                {
                    // pCurr ... pArea ... pNext
                    pCurr->m_cbArea += pNext->m_cbArea;
                    ::ZeroMemory(pNext, sizeof(*pNext));
                }
            } // if
            return;
        } // if

        if (pArea->GetBtm<Area>() == pCurr)
        {
            // pArea ... pCurr
            sm_oFreeAreas.InsertBefore(pArea, pCurr);
            sm_oFreeAreas.Delete(pCurr);
            pArea->m_cbArea += pCurr->m_cbArea;
            ::ZeroMemory(pCurr, sizeof(*pCurr));
            return;
        } // if

        if (pCurr > pArea)
        {
            sm_oFreeAreas.InsertBefore(pArea, pCurr);
           return;
        }
    } // for each area

    sm_oFreeAreas.Append(pArea);
} // Mm::AddFreeArea

void Mm::Area::EnumCode::Next()
{
    ASSERT(!AtEnd());
    m_pCurr = reinterpret_cast<CodeObject*>(
        m_pCurr->ToInt() + m_pCurr->GetSize() );
} // Next

void Mm::Area::EnumRecord::Next()
{
    ASSERT(!AtEnd());
    size_t cb = m_pCurr->GetSize();
    ASSERT(0 == cb % Arch::Align_Record);
    m_pCurr = reinterpret_cast<Record*>(m_pCurr->ToInt() + cb);
} // Mm::Area::EnumRecord::Next

void* Mm::Area::Alloc(size_t cbData)
{
    size_t ofsNext = m_ofsFree + cbData;
    when (ofsNext > m_cbArea) return NULL;
    void* pv = reinterpret_cast<void*>(ToInt() + m_ofsFree);
    m_ofsFree = ofsNext;
    return pv;
} // Mm::Area::Alloc

Mm::GcAnchor* Mm::CreateGcAnchor(Val object)
{
    GcAnchor* pGcAnchor = new GcAnchor(object);
    return sm_oGcAnchors.Append(pGcAnchor);
} // Mm::CreateGcAnchor

void Mm::DestroyGcAnchor(GcAnchor* pGcAnchor)
{
    sm_oGcAnchors.Delete(pGcAnchor);
} // Mm::DestroyGcAnchor

Mm::AreaManager::AreaManager()
{
    Reset();
} // Mm::AreaManager::AreaManager

Mm::Area* Mm::AreaManager::Alloc(uint nFlags, size_t cb)
{
    Area::Scan eScan = static_cast<Area::Scan>(
        (nFlags & Area::Flags_ScanMask) >> Area::Scan_Shift );

    Area* pArea;
    if (Area::Scan_Code == eScan)
    {
        pArea = Mm::GetCodeArea(nFlags, cb);
    }
    else
    {
        pArea = Mm::GetDataArea(nFlags, cb);
    }

    return m_rgpArea[eScan] = pArea;
} // Mm::AreaManager::Alloc

void Mm::AreaManager::Reset()
{
    for (int k = Area::Scan_First; k < Area::Scan_Limit; k++)
    {
        m_rgpArea[k] = Mm::GetEmptyArea();
    } // for
} // Mm::AreaManager::Reset

void Mm::AreaManager::Set(Area* pArea)
{
    m_rgpArea[pArea->GetScan()] = pArea;
} // Mm::AreaManager::Set

bool Mm::IsHeapObject(Val x)
{
    switch (x->GetTag())
    {
    case Arch::Tag_Fixnum:
    case Arch::Tag_Fixnum1:
    case Arch::Tag_Null:
        return false;

    case Arch::Tag_Cons:
        return IsHeapPtr(x->StaticCast<Cons>());

    case Arch::Tag_FunObj:
        return IsHeapPtr(x->StaticCast<CodeObject>());

    case Arch::Tag_Record:
        return IsHeapPtr(x->StaticCast<Record>());
    } // switch tag

    CAN_NOT_HAPPEN();
} // Mm::IsHeapObject

/// <summary>
///   Constructs thread object with specified id.
/// </summary>
Thread::Thread(Val id)
{
    m_classd = CLASSD_thread;
    m_id     = id;
    m_name   = Fixnum::Encode(++sm_nThread);

    Reset();
} // Thread::Thread

void Thread::Reset()
{
    m_fn = 0;
    m_fp = 0;
    m_n  = 0;
    ResetAlloc();
} // Thread::Reset

void Thread::ResetAlloc()
{
    m_pBinoArea = Mm::GetEmptyArea();
    m_pCodeArea = Mm::GetEmptyArea();
    m_pConsArea = Mm::GetEmptyArea();
    m_pDataArea = Mm::GetEmptyArea();
} // Thread::Reset

void Thread::Restart()
{
    Reset();

    // FIXME 2007-09-08 yosi@msn.com We should initialize TLV from
    // tlvrec.m_value.
    Val* pRunner = VAR(Atlv_vectorA)->StaticCast<SimpleVector>()->GetStart();
    for (int i = 0; i < lengthof(mv_tlv); i++)
    {
        Val tlvrec = *pRunner++;
        if (TlvRecord* p = tlvrec->DynamicCast<TlvRecord>())
        {
            mv_tlv[i] = p->m_value;
        }
        else
        {
            mv_tlv[i] = nil;
        }
    } // for i

    // FIXME 2007-08-14 yosi@msn.com What value do we initialize
    // *print-pprint-dispatch* w/o pretty printer?

    // FIXME 2007-08-15 yosi@msn.com When will we have readtable?

    PlatformRestart();
} // Thread::Restart

/// <summary>
///   Represents DataVector object in heap.
/// </summary>
class DataVectorInHeap : 
    public HeapObject_<SimpleVector>
{
    private: ~DataVectorInHeap();

    public: DataVectorInHeap(Val classd, Val length)
    {
        m_classd = classd;
        m_length = length;
    } // DataVectorInHeap
}; // DataVectorInHeap

/// <summary>
///   Represents Record object in heap.
/// </summary>
class RecordInHeap : public HeapObject_<Record>
{
    private: ~RecordInHeap();
    
    public: RecordInHeap(Val classd)
    {
        m_classd = classd;
    } // RecordInHeap
}; // RecordInHeap

CASSERT(0 == offsetof(RecordInHeap, m_classd));

Val Thread::AllocBinObj(Val classd)
{
    const size_t cb = classd->StaticCast<ClassD>()->GetFixedSize();

    for (;;)
    {
        void* pvObj = m_pBinoArea->Alloc(cb);
        if (NULL != pvObj)
        {
            RecordInHeap* pObj = new(pvObj) RecordInHeap(classd);
            return pObj->Encode();
        }

        m_pBinoArea = Mm::GetDataArea(Mm::Area::ScanType_BinObj, cb);
    } // for
} // Thread::AlocBinObj

Val Thread::AllocBinVec(Val const classd, Val const length)
{
    ClassD* const pClassD = classd->StaticCast<ClassD>();

    size_t const cb = computeBinVecSize(pClassD, length);

    for (;;)
    {
        void* const pvObj = m_pBinoArea->Alloc(cb);
        if (NULL != pvObj)
        {
            DataVectorInHeap* pObj = new(pvObj) DataVectorInHeap(
                classd,
                length );

            ASSERT(reinterpret_cast<Record*>(pObj)->GetSize() == cb);

            return pObj->Encode();
        } // if

        m_pBinoArea = Mm::GetDataArea(Mm::Area::ScanType_BinObj, cb);
    } // for
} // Thread::AlocBinVec

/// <summary>
///   Represents FunObj object in heap.
/// </summary>
class FunObjInHeap : 
    public HeapObject_<Layout_native_code_function>
{
    public: enum Constant
    {
        Tag = Arch::Tag_FunObj,
    }; // Constant

    private: ~FunObjInHeap();

    public: FunObjInHeap(Val classd, size_t cbFunction)
    {
        m_classd = classd;
        m_length = reinterpret_cast<Val>(cbFunction);
    } // FunObjInHeap

    public: Val Encode() const
    {
        return reinterpret_cast<Val>(reinterpret_cast<Int>(this) + Tag);
    } // Encode
}; // FunObjInHeap

//////////////////////////////////////////////////////////////////////
//
// Thread::AllocCode
//
Val Thread::AllocCode(Val const classd, size_t const cb)
{
    ASSERT(0 == cb % Arch::Align_FunObj);

    for (;;)
    {
        void* const pvObj = m_pCodeArea->Alloc(cb);
        if (NULL != pvObj)
        {
            FunObjInHeap* pObj = new(pvObj) FunObjInHeap(classd, cb);
            return pObj->Encode();
        }

        m_pCodeArea = Mm::GetCodeArea(Mm::Area::ScanType_Code, cb);
    } // for
} // Thread::AlocCode

//////////////////////////////////////////////////////////////////////
//
// ConsInHeap
//
class ConsInHeap : public HeapObject_<Cons>
{
    private: ~ConsInHeap();

    public: ConsInHeap(Val a, Val b)
    {
        m_car = a;
        m_cdr = b;
    } // ConsInHeap
}; // ConsInHeap

CASSERT(sizeof(Layout_cons) == sizeof(ConsInHeap));

//////////////////////////////////////////////////////////////////////
//
// Thread::AllocCons
//
Val Thread::AllocCons(Val a, Val b)
{
    const size_t cb = sizeof(ConsInHeap);

    for (;;)
    {
        void* pvObj = m_pConsArea->Alloc(cb);
        if (NULL != pvObj)
        {
            ConsInHeap* pObj = new(pvObj) ConsInHeap(a, b);
            return pObj->Encode();
        }

        m_pConsArea = Mm::GetDataArea(Mm::Area::ScanType_Cons, cb);
    } // for
} // Thread::AlocCons

//////////////////////////////////////////////////////////////////////
//
// InstanceInHeap
//
class InstanceInHeap : public HeapObject_<Instance>
{
    private: ~InstanceInHeap();
    
    public: InstanceInHeap(Val classd)
    {
        Storage* const s = reinterpret_cast<Storage*>(this+1);
        m_classd      = classd;
        m_storage     = s->Encode();
        s->m_classd   = CLASSD_storage;
        s->m_storaged = classd;

        Val* p = s->GetStart();

        Val* const e = reinterpret_cast<Val*>(
            reinterpret_cast<Int>(p) +
            classd->StaticCast<ClassD>()->GetFixedSize() );

        while (p < e)
        {
            *p++ = MARKER_unbound;
        } // while
    } // InstanceInHeap
}; // InstanceInHeap

CASSERT(sizeof(InstanceInHeap) == sizeof(Layout_instance));

//////////////////////////////////////////////////////////////////////
//
// Thread::AllocInstance
//
Val Thread::AllocInstance(Val classd)
{
    size_t const cb = 
        classd->StaticCast<ClassD>()->GetFixedSize() +
        sizeof(Instance);

    ASSERT(0 == (cb % Arch::Align_Instance));

    for (;;)
    {
        void* const pvObj = m_pDataArea->Alloc(cb);
        if (NULL != pvObj)
        {
            InstanceInHeap* const pObj = new(pvObj) InstanceInHeap(classd);
            return pObj->Encode();
        }

        m_pDataArea = Mm::GetDataArea(Mm::Area::ScanType_Record, cb);
    } // for
} // Thread::AllocInstance

//////////////////////////////////////////////////////////////////////
//
// Thread::AllocRecord
//
Val Thread::AllocRecord(Val const classd)
{
    ClassD* const pClassD = classd->StaticCast<ClassD>();
    size_t const cb = pClassD->GetFixedSize();
    ASSERT(0 == (cb % (sizeof(Val)*2)));

    for (;;)
    {
        void* const pvObj = m_pDataArea->Alloc(cb);
        if (NULL != pvObj)
        {
            RecordInHeap* pObj = new(pvObj) RecordInHeap(classd);
            return pObj->Encode();
        }

        m_pDataArea = Mm::GetDataArea(Mm::Area::ScanType_Record, cb);
    } // for
} // Thread::AlocRecord

//////////////////////////////////////////////////////////////////////
//
// Thread::AllocVector
//
Val Thread::AllocVector(Val const classd, Val const length)
{
    const ClassD* const pClassD = classd->StaticCast<ClassD>();

    size_t cb = computeVecSize(pClassD, length);

    for (;;)
    {
        void* const pvObj = m_pDataArea->Alloc(cb);
        if (NULL != pvObj)
        {
            DataVectorInHeap* pObj = new(pvObj) DataVectorInHeap(
                classd,
                length );
            return pObj->Encode();
        }

        m_pDataArea = Mm::GetDataArea(Mm::Area::ScanType_Record, cb);
    } // for
} // Thread::AlocVector

// [V]
Val Thread::ValuesFromList(Val vals)
{
    Val* p = mv_value;
    foreach (List::Enum, oEnum, vals)
    {
        if (p - mv_value > lengthof(mv_value))
        {
            error("Too many values");
        }
        *p++ = oEnum.Get();
    } // for each elt

    m_n = Fixnum::Encode(p - mv_value);

    if (m_n == zero)
    {
        mv_value[0] = nil;
    }

    return mv_value[0];
} // Thread::ValuesFromList

/// <summary>
///   Convert values to list.
/// </summary>
Val Thread::ValuesToList(int iStart) const
{
    const Val* pxStart = mv_value + iStart;
    const Val* pxEnd   = mv_value + Fixnum::Decode_(m_n);

    if (pxStart >= pxEnd)
    {
        return nil;
    }

    Val vals = list(*pxStart);
    Val tail = vals;
    for (const Val* pxRunner = pxStart + 1; pxRunner < pxEnd; pxRunner++)
    {
        tail = setf_cdr(list(*pxRunner), tail);
    } // for i
    return vals;
} // Thread::ValuesToList

} // TinyCl
