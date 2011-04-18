#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Sxhash
// tinycl_sxhash.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_sxhash.cpp#5 $
//
#include "./tinycl_sxhash.h"

#include "./tinycl_gc.h"

namespace TinyCl
{

SxHashArea* SxHashArea::sm_pSxHashArea;

namespace Private
{

/// <summary>
///   A random number generator. We use this simple pseudo random number
///   for hash-code of object.
/// </summary>
class Random
{
    private: static Val sm_random;

    // [C]
    public: static Val Compute()
    {
        Int nHashCode = Fixnum::Decode_(sm_random);
        nHashCode = nHashCode * 1664525 + 1013904233;
        nHashCode += 1;
        return sm_random = Fixnum::Encode(nHashCode & Fixnum::MostPositive);
    } // random

    // [I]
    public: static void Initialize()
    {
        sm_random = Fixnum::Encode(::GetTickCount() & Fixnum::MostPositive);
    } // Initialize
}; // Random

Val Random::sm_random;

/// <summary>
///   SxHash Table abstract class.
/// </summary>
class SxHashTable : public SxHashArea
{
    static const uint kRehashThreshold = 65;

    public: struct Entry
    {
        Val m_key;
        Val m_value;

        public: Val GetKey() const
        {
            return m_key;
        } // GetKey

        public: Val GetValue() const
        {
            return m_value;
        } // GetValue

        public: bool IsEmpty() const
        {
            return zero == m_key;
        } // IsEmpty

        public: bool IsRemoved() const
        {
            return nil == m_key;
        } // IsRemoved

        public: void MarkEmpty()
        {
            m_key = zero;
        } // MarkRemoved

        public: void MarkRemoved()
        {
            m_key = nil;
        } // MarkRemoved

        public: Val SetEntry(Val key, Val val)
        {
            m_key = key;
            return m_value = val;
        } // SetEntry
    }; // Entry

    public: class EnumAll
    {
        protected: SxHashTable* m_p;
        protected: Entry*       m_pBtm;
        protected: Entry*       m_pRunner;
        protected: Entry*       m_pTop;

        public: EnumAll(SxHashTable* p) :
            m_p(p),
            m_pBtm(p->GetBtm<Entry>()),
            m_pRunner(p->GetTop<Entry>()),
            m_pTop(p->GetTop<Entry>()) {}

        public: bool AtEnd() const
            { return m_pRunner >= m_pBtm; }

        public: Entry* Get() const
        {
            ASSERT(! AtEnd());
            return m_pRunner;
        } // Get

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner++;
        } // Next

        public: void Remove()
        {
            ASSERT(! AtEnd());
            m_p->removeEntry(m_pRunner);
        } // Remove
    }; // EnumAll

    public: class Enum : public EnumAll
    {
        private: uint m_nRest;

        public: Enum(SxHashTable* p) :
            EnumAll(p),
            m_nRest(p->GetCount())
        {
            if (m_nRest > 0)
            {
                --m_pRunner;
                next();
            }
        } // Enum

        public: bool AtEnd() const
            { return 0 == m_nRest || EnumAll::AtEnd(); }

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_nRest -= 1;
            if (0 == m_nRest)
            {
                m_pRunner = m_pBtm;
            }
            else
            {
                next();
            }
        } // Next

        private: void next()
        {
            for (;;)
            {
                m_pRunner++;
                Entry* pEntry = Get();
                if (! pEntry->IsEmpty() && ! pEntry->IsRemoved())
                {
                    break;
                }
            } // for
        } // next
    }; // Enum

    public: class EnumWrap
    {
        private: bool           m_fWrap;
        private: SxHashTable*   m_p;
        private: Entry*         m_pBtm;
        private: Entry*         m_pRunner;
        private: Entry*         m_pStart;
        private: Entry*         m_pTop;

        public: struct Arg
        {
            Val             m_key;
            SxHashTable*    m_p;

            Arg(SxHashTable* p, Val key) :
                m_key(key),
                m_p(p) {}
        }; // Arg

        public: EnumWrap(Arg oArg) :
            m_fWrap(false),
            m_pBtm(oArg.m_p->GetBtm<Entry>()),
            m_pRunner(
                oArg.m_p->GetTop<Entry>() +
                hash(oArg.m_key) % oArg.m_p->GetSize() ),
            m_pTop(oArg.m_p->GetTop<Entry>())
        {
            m_pStart = m_pRunner;
        } // EnumWrap

        public: bool AtEnd() const
            { return m_fWrap && m_pRunner == m_pStart; }

        public: Entry* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner++;
            if (m_pRunner >= m_pBtm)
            {
                m_pRunner = m_pTop;
            }
        } // Next
    }; // EnumWrap

    // [A]
    private: Val addEntry(Entry* pEntry, Val key, Val val)
    {
        ASSERT(pEntry >= GetTop<Entry>());
        ASSERT(pEntry < GetBtm<Entry>());
        ASSERT(m_ofsFree + sizeof(Entry) <= m_cbArea);
        m_ofsFree += sizeof(Entry);
        return pEntry->SetEntry(key, val);
    } // addEntry

    // [C]
    public: static Val Compute(Val x)
    {
        for (;;)
        {
            Val hash_code = Get()->tryCompute(x, nil);
            if (nil != hash_code)
            {
                return hash_code;
            }

            sm_pSxHashArea = copy(Get(), Get()->m_cbArea + k_cbUnit);
        } // for
    } // Compute

    private: Entry* computeNextEntry(Entry* pEntry)
    {
        ASSERT(pEntry >= GetTop<Entry>());
        ASSERT(pEntry < GetBtm<Entry>());

        pEntry++;
        if (pEntry >= GetBtm<Entry>())
        {
            pEntry = GetTop<Entry>();
        }
        return pEntry;
    } // computeNextEntry

    private: Entry* computePrevEntry(Entry* pEntry)
    {
        ASSERT(pEntry >= GetTop<Entry>());
        ASSERT(pEntry < GetBtm<Entry>());

        if (pEntry <= GetTop<Entry>())
        {
            pEntry = GetBtm<Entry>();
        }
        --pEntry;
        return pEntry;
    } // computePrevEntry

    private: static SxHashTable* copy(
        SxHashTable*    pOld,
        size_t          cbNew )
    {
        SxHashTable* pNew = makeSxHashArea(cbNew)->
            StaticCast<SxHashTable>();

        uint nRest = pOld->GetCount();
        foreach (EnumAll, oEnum, pOld)
        {
            Entry* pEntry = oEnum.Get();
            if (pEntry->IsEmpty())
            {
                continue;
            }

            if (pEntry->IsRemoved())
            {
                continue;
            }

            pNew->mustPut(pEntry);

            nRest -= 1;
            if (0 == nRest)
            {
                break;
            }
        } // for each entry

        Mm::AddFreeArea(pOld);

        return pNew;
    } // copy

    // [G]
    public: static SxHashTable* Get()
        { return SxHashArea::Get()->StaticCast<SxHashTable>(); }

    public: uint GetCount() const
    {
        return static_cast<uint>(GetFree<Entry>() - GetTop<Entry>());
    } // GetCount

    public: uint GetSize() const
    {
        return static_cast<uint>(GetBtm<Entry>() - GetTop<Entry>());
    } // GetSize

    // [H]
    private: static Int hash(Val x)
    {
        return x->ToInt() >> Arch::TagBits;
    } // Hash

    // [I]
    private: bool isNeedEnlarge() const
    {
        return GetCount() * 100 > GetSize() * kRehashThreshold;
    } // isNeedEnlarge

    private: void inplaceRehash()
    {
        foreach (Enum, oEnum, this)
        {
            Entry* pEntry = oEnum.Get();
            Entry oEntry = *pEntry;
            removeEntry(pEntry);
            mustPut(&oEntry);
        } // for entry
    } // inplaceRehash

    // [M]
    private: void mustPut(const Entry* pEntry)
    {
        Val succeeded = tryCompute(pEntry->GetKey(), pEntry->GetValue());
        ASSERT(nil != succeeded);
    } // mustPut

    // [R]
    public: static void Rehash()
    {
        sm_pSxHashArea = Get()->rehash();
    } // Rehash

    private: SxHashTable* rehash()
    {
        uint cbArea = 0;
        uint cbData = 0;
        uint cLives = GetCount();
        for (;;)
        {
            cbArea += k_cbUnit;
            cbData += k_cbUnit - sizeof(Mm::Area);
            if (cLives * kRehashThreshold <
                (cbData / sizeof(SxHashTable::Entry)) * 100 )
            {
                break;
            }
        } // for

        if (m_cbArea == cbArea)
        {
            inplaceRehash();
            return this;
        }
        else
        {
            return copy(this, cbArea);
        }
    } // rehash

    private: void removeEntry(Entry* pEntry)
    {
        ASSERT(pEntry >= GetTop<Entry>());
        ASSERT(pEntry < GetBtm<Entry>());

        ASSERT(m_ofsFree >= sizeof(Area) + sizeof(Entry));
        m_ofsFree -= sizeof(Entry);

        if (computeNextEntry(pEntry)->IsEmpty() &&
            computePrevEntry(pEntry)->IsEmpty() )
        {
            pEntry->MarkEmpty();
        }
        else
        {
            pEntry->MarkRemoved();
        }
    } // removeEntry

    // [T]
    private: Val tryCompute(Val key, Val value)
    {
        Entry* pHome = NULL;

        foreach (EnumWrap, oEnum, EnumWrap::Arg(this, key))
        {
            Entry* pEntry = oEnum.Get();
            if (pEntry->IsEmpty())
            {
                if (isNeedEnlarge())
                {
                    return nil;
                }

                if (NULL == pHome)
                {
                    pHome = pEntry;
                }

                if (nil == value)
                {
                    value = Random::Compute();
                }

                return addEntry(pHome, key, value);
            } // if empty

            if (pEntry->GetKey() == key)
            {
                return pEntry->GetValue();
            } // if found

            if (pEntry->IsRemoved())
            {
                if (NULL != pHome)
                {
                    pHome = pEntry;
                }
            } // if removed
        } // for each entry

        CAN_NOT_HAPPEN();
    } // tryCompute
}; // SxHashTable

} // Private

using namespace Private;

/// <summary>
///  Fix SxHash Table at post GC.
/// </summary>
void Gc::fixSxHashTable()
{
    foreach (SxHashTable::Enum, oEnum, SxHashTable::Get())
    {
        SxHashTable::Entry* pEntry = oEnum.Get();

        Area* pArea = mapToArea(pEntry->m_key);
        if (NULL == pArea)
        {
            // pEntry->m_key is an immediate object.
            continue;
        }

        if (! pArea->IsFromSpace())
        {
            continue;
        }

        if (ForwardCell* p = pEntry->m_key->DynamicCast<ForwardCell>())
        {
            pEntry->m_key = p->Get();
        }
        else
        {
            oEnum.Remove();
        }
    } // for entry

    SxHashTable::Rehash();
} // Gc::fixSxHashTable

/// <summary>
///   Initialize SxHash Table.
/// </summary>
void SxHashArea::Initialize()
{
    Random::Initialize();
    sm_pSxHashArea = makeSxHashArea(k_cbUnit);
} // SxHashArea::Initialize

/// <summary>
///   Reinitialize SxHash Table at image loading time.
/// </summary>
void SxHashArea::Reinitialize()
{
    ASSERT(NULL == sm_pSxHashArea);
    Random::Initialize();
    sm_pSxHashArea = this;
} // SxHashArea::Reinitializ

/// <summary>
///   Compute sxhash-eq hash code of specified object.
/// </summary>
/// <param name="x">An object to compute hash code.</param>
Val SxHash::Compute(Val x)
{
    switch (x->GetTag())
    {
    case Arch::Tag_Cons:
    case Arch::Tag_FunObj:
        return SxHashTable::Compute(x);

    case Arch::Tag_Fixnum:
    case Arch::Tag_Fixnum1:
        return x;

    case Arch::Tag_Null:
        return zero;

    case Arch::Tag_Record:
    {
        if (Symbol* p = x->DynamicCast<Symbol>())
        {
            return p->m_hash_code;
        }

        if (Character* p = x->DynamicCast<Character>())
        {
            return Fixnum::Encode(p->ToCode());
        }

        if (SetfCell* p = x->DynamicCast<SetfCell>())
        {
            return Compute(p->m_name);
        }

        return SxHashTable::Compute(x);
    } // record

    default:
        error("Invalid object ~S", x);
    } // switch tag
} // SxHash::Compute

defun(sxhash_eq, (Val x))
{
    if (Symbol* p = x->DynamicCast<Symbol>())
    {
        return p->m_hash_code;
    } // if symbol

    if (Class* p = x->DynamicCast<Class>())
    {
        Val hash1 = sxhash_eq(p->m_name);
        Val hash2 = Qclass->StaticCast<Symbol>()->m_hash_code;
        return Fixnum::Encode(hash1->ToInt() ^  hash2->ToInt());
    } // if class

    if (ClassD* p = x->DynamicCast<ClassD>())
    {
        Val hash1 = sxhash_eq(p->m_class);
        Val hash2 = Qclass_description->StaticCast<Symbol>()->m_hash_code;
        return Fixnum::Encode(hash1->ToInt() ^  hash2->ToInt());
    } // if class

    return SxHash::Compute(x);
} // sxhash_eq

#if _DEBUG
void PrintSxHashTable()
{
    Val nth = zero;
    foreach (SxHashTable::Enum, oEnum, SxHashTable::Get())
    {
        format(t, "[~D] ~S~%",
            Fixnum::Encode(nth),
            oEnum.Get()->GetKey() );
        nth = xxadd(nth, one);
    } // for each entry
} // PrintSxHashTable
#endif // _DEBUG

} // TinyCl
