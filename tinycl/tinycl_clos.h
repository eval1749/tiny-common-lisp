//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl CLOS Definitions
// tinycl_clos.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_clos.h#6 $
//
#if !defined(INCLUDE_tinycl_clos_h)
#define INCLUDE_tinycl_clos_h

#include "./tinycl.h"

namespace TinyCl
{

class DSlotD : public Instance_<DSlotD, Layout_direct_slot_definition>
{
    public: static Int Decode_(const Datum* const x)
    {
        return Storage::Decode_(x->StaticCast<Instance>()->m_storage);
    } // Decode_
}; // DSlotD

class ESlotD : public Instance_<ESlotD, Layout_effective_slot_definition>
{
    public: static Int Decode_(const Datum* const x)
    {
        return Storage::Decode_(x->StaticCast<Instance>()->m_storage);
    } // Decode_
}; // ESlotD

class FuncallableInstance :
    public Datum_<FuncallableInstance, Arch::Tag_FunObj>,
    public Layout_funcallable_instance
{
    public: template<typename T> T* GetStorage() const
        { return reinterpret_cast<T*>(m_storage->StaticCast<Storage>()); }
}; // FuncallableInstance

class GenericFunction : 
    public Datum_<GenericFunction, Arch::Tag_FunObj>,
    public Layout_generic_function
{
    // [D]
    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        FuncallableInstance* p = x->StaticCast<FuncallableInstance>();
        return Storage::Decode_(p->m_storage);
    } // Decode_

    // [I]
    public: static bool Is_(const Datum* const x)
    {
        if (x->GetTag() != Tag) return false;

        FuncallableInstance* p =
            reinterpret_cast<FuncallableInstance*>(x->ToInt() - Tag);

        return p->m_storage->Is<Storage>();
    } // Is_
}; // GenericFunction

class Method : public Instance_<Method, Layout_standard_method>
{
    public: static Val Class_() { return CLASS_standard_method; }
}; // Method

class MethodCache : public Record_<MethodCache, Layout_method_cache>
{
    public: static Val ClassD_() { return CLASSD_method_cache; }

    // [C]
    private: Int computeDirectHash(const Val* const pStart) const
    {
        const Int cSpecializers = GetSpecializerCount();
        const Val* const pEnd = pStart + cSpecializers;
        Int nHash = 0;
        for (const Val* p = pStart; p < pEnd; p++)
        {
            nHash = Hash(*p, nHash);
        } // for p
        return nHash;
    } // computeDirectHash

    // [E]
    private: void enlarge()
    {
        const Int cSpecializers = GetSpecializerCount();

        const Int nLineSize = cSpecializers + 1;

        SimpleVector* pCurVec = m_vector->StaticCast<SimpleVector>();
        const Int nCurLen = Fixnum::Decode_(pCurVec->m_length);
        const Int cCurEnts = nCurLen / nLineSize;
        const Int cNewEnts = (cCurEnts * 13) / 10;
        const Int nNewLen  = cNewEnts * nLineSize;

        m_vector = make_vector(Fixnum::Encode(nNewLen));
        Val* pNew = m_vector->StaticCast<SimpleVector>()->GetStart();

        const Val* pCur = pCurVec->GetStart();

        Int nCount = Fixnum::Decode_(m_count);
        for (Int nCurIndex = 0; nCurIndex < nCurLen; nCurIndex += nLineSize)
        {
            if (0 != pCur[nCurIndex])
            {
                Int nHash = computeDirectHash(pCur + nCurIndex);
                Int nNth  = nHash % cNewEnts;

                Int nNewIndex = nNth * nLineSize;
                while (0 != pNew[nNewIndex])
                {
                    nNewIndex += nLineSize;
                    if (nNewIndex >= nNewLen)
                    {
                        nNewIndex = 0;
                    }
                } // while

                for (Int j = 0; j <= cSpecializers; j++)
                {
                    pNew[nNewIndex + j] = pCur[nCurIndex + j];
                } // for j

                nCount -= 1;
                if (0 == nCount)
                {
                    break;
                }
            } // if
        } // for nCurIndex
    } // enlarge

    // [G]
    public: Val Get(Thread* pth, const Int nHash) const
    {
        const Int nLineSize     = Fixnum::Decode_(m_line_size);
        const Int cSpecializers = nLineSize - 1;

        SimpleVector* pCurVec = m_vector->StaticCast<SimpleVector>();
        const Int nCurLen = Fixnum::Decode_(pCurVec->m_length);
        const Int cCurEnts = nCurLen / nLineSize;

        const Val* pCur = pCurVec->GetStart();

        Int nCurStart = (nHash % cCurEnts) * nLineSize;
        Int nCurIndex = nCurStart;
        do
        {
            if (0 == pCur[nCurIndex])
            {
                return nil;
            }

            if (match(pth, pCur + nCurIndex))
            {
                return pCur[nCurIndex + cSpecializers];
            }

            nCurIndex += nLineSize;
            if (nCurIndex >= nCurLen)
            {
                nCurIndex = 0;
            }
        } while (nCurIndex != nCurStart);
        error("Broken method cache: ~S", Encode());
    } // Get

    public: Int GetSpecializerCount() const
    {
        return Fixnum::Decode_(m_line_size) - 1;
    } // GetSpecializerCount

    // [H]
    public: static Int Hash(Val classd, Int nHash = 0)
    {
        nHash = ComputeHash(
            Fixnum::Decode_(classd->StaticCast<ClassD>()->m_hash_code),
            nHash );
        return nHash & Fixnum::MostPositive;
    } // Hash

    // [M]
    private: bool match(Thread* pth, const Val* pCacheLine) const
    {
        const Val* pRunner = pCacheLine;
        foreach (
            SimpleVector::Enum,
            oEnum,
            m_order->StaticCast<SimpleVector>() )
        {
            Int nNth = Fixnum::Decode_(oEnum.Get());
            Val classd = classd_of(pth->mv_value[nNth]);
            if (*pRunner != classd)
            {
                return false;
            }
            pRunner++;
        } // for each arg
        return true;
    } // match

    // [P]
    public: void Put(Thread* pth, Int nHash, Val emf)
    {
        const Int nLineSize = Fixnum::Decode_(m_line_size);

        SimpleVector* pCurVec = m_vector->StaticCast<SimpleVector>();
        const Int nCurLen = Fixnum::Decode_(pCurVec->m_length);
        const Int cCurEnts = nCurLen / nLineSize;

        Val* pCur = pCurVec->GetStart();

        Int nCurStart = (nHash % cCurEnts) * nLineSize;
        Int nCurIndex = nCurStart;
        while (0 != pCur[nCurIndex])
        {
            nCurIndex += nLineSize;
            if (nCurIndex >= nCurLen)
            {
                nCurIndex = 0;
            }
            ASSERT(nCurIndex != nCurStart);
        } // while

        Val* pRunner = pCur + nCurIndex;
        foreach (
            SimpleVector::Enum,
            oEnum,
            m_order->StaticCast<SimpleVector>() )
        {
            Int nNth = Fixnum::Decode_(oEnum.Get());
            *pRunner++ = classd_of(pth->mv_value[nNth]);
        } // for each arg

        *pRunner = emf;

        m_count = xxadd(m_count, one);
        if (Fixnum::Decode_(m_count) * 100 >= cCurEnts * 65)
        {
            enlarge();
        }
    } // Put

    public: Val StaticGet(const Val* const v_classd) const
    {
        const Int nLineSize     = Fixnum::Decode_(m_line_size);
        const Int cSpecializers = nLineSize - 1;

        SimpleVector* pCurVec = m_vector->StaticCast<SimpleVector>();
        const Int nCurLen = Fixnum::Decode_(pCurVec->m_length);
        const Int cCurEnts = nCurLen / nLineSize;

        const Val* pCur = pCurVec->GetStart();

        Int nHash = computeDirectHash(v_classd);

        Int nCurStart = (nHash % cCurEnts) * nLineSize;
        Int nCurIndex = nCurStart;
        do
        {
            if (0 == pCur[nCurIndex])
            {
                return nil;
            }

            Int i;
            for (i = 0; i < cSpecializers; i++)
            {
                if (pCur[nCurIndex + i] != v_classd[i])
                {
                    break;
                }
            } // for i

            if (i == cSpecializers)
            {
                return pCur[nCurIndex + cSpecializers];
            }

            nCurIndex += nLineSize;
            if (nCurIndex >= nCurLen)
            {
                nCurIndex = 0;
            }
        } while (nCurIndex != nCurStart);
        error("Broken method cache: ~S", Encode());
    } // StaticGet

    public: void StaticPut(const Val* const v_classd, Val emf)
    {
        const Int nLineSize     = Fixnum::Decode_(m_line_size);
        const Int cSpecializers = nLineSize - 1;

        SimpleVector* pCurVec = m_vector->StaticCast<SimpleVector>();
        const Int nCurLen = Fixnum::Decode_(pCurVec->m_length);
        const Int cCurEnts = nCurLen / nLineSize;

        Val* pCur = pCurVec->GetStart();

        const Int nHash = computeDirectHash(v_classd);

        Int nCurStart = (nHash % cCurEnts) * nLineSize;
        Int nCurIndex = nCurStart;
        while (0 != pCur[nCurIndex])
        {
            nCurIndex += nLineSize;
            if (nCurIndex >= nCurLen)
            {
                nCurIndex = 0;
            }
            ASSERT(nCurIndex != nCurStart);
        } // while

        for (Int i = 0; i < cSpecializers; i++)
        {
            pCur[nCurIndex + i] = v_classd[i];
        } // for i

        pCur[nCurIndex + cSpecializers] = emf;

        m_count = xxadd(m_count, one);
        if (Fixnum::Decode_(m_count) * 100 >= cCurEnts * 65)
        {
            enlarge();
        }
    } // StaticPut
}; // MethodCache

} // TinyCl

#endif //!defined(INCLUDE_tinycl_clos_h)
