// -*- Mode: C++ -*-
//
// TinyCl - TinyCl Compiler
// compiler/tinycl_c_defs.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_dfa.h#2 $
//
#if !defined(INCLUDE_tinycl_c_ir_dfa_h)
#define INCLUDE_tinycl_c_ir_dfa_h

#include "../tinycl_c_defs.h"

namespace TinyCl
{

namespace Compiler
{
class BitVec
{
    public: void  operator delete(void*, void*) { CAN_NOT_HAPPEN(); }
    public: void* operator new(size_t, void* pv) { return pv; }

    private: enum Constants
    {
        BitsInWord = sizeof(uint) * 8,
    }; // Constants

    private: uint m_cAllocedWords;
    private: uint m_cBits;
    private: uint m_cWords;

    private: static BitVec s_oEmpty;

    // ctor
    private: BitVec(uint const cBits, uint const cWords) :
        m_cAllocedWords(cWords),
        m_cBits(cBits),
        m_cWords(cWords) {}

    // [A]
    public: BitVec* Adjust(uint const cBits)
    {
        uint const cWords = static_cast<uint>(Ceiling(cBits, BitsInWord));
        if (cWords <= m_cAllocedWords)
        {
            m_cBits = cBits;
            return this;
        }

        return Make(cBits);
    } // Adjust

    #define DefBitOp(mp_name, mp_u1, mp_op, mp_u2) \
        public: BitVec* mp_name(const BitVec* const that) \
        { \
            for (uint w = 0; w < m_cWords; w++) \
            { \
                setWord( \
                    w, \
                    (mp_u1 this->getWord(w)) \
                    mp_op \
                    (mp_u2 that->getWord(w)) ); \
            } \
            return this; \
        } // DefBitOp

    DefBitOp(AndC2, +, &, ~)
    DefBitOp(Ior,   +, |, +)

    // [C]
    public: BitVec* Copy(const BitVec* const that)
    {
        ::CopyMemory(
            this->getStart(),
            that->getStart(),
            sizeof(uint) * m_cWords );
        return this;
    } // Copy

    public: uint CountOne() const
    {
        uint nCount = 0;
        for (uint k = 0; k < m_cBits; k++)
        {
            if (IsOne(k))
            {
                nCount += 1;
            }
        } // for
        return nCount;
    } // CountOne

    // [E]
    public: bool Equal(const BitVec* const that) const
    {
        if (GetLength() != that->GetLength())
        {
            return false;
        }

        uint const cWords = static_cast<uint>(Ceiling(m_cBits, BitsInWord));

        for (uint i = 0; i < cWords - 1; i++)
        {
            if (this->getWord(i) != that->getWord(i))
            {
                return false;
            }
        } // for i

        uint const k = m_cBits % BitsInWord;
        uint const m = k ? (1 << k) - 1 : static_cast<uint>(-1);

        return (this->getWord(cWords - 1) & m) ==
               (that->getWord(cWords - 1) & m);
    } // Equal

    // [F]
    public: BitVec* FillOne()
    {
        for (uint w = 0; w < m_cWords; w++)
        {
            setWord(w, static_cast<uint>(-1));
        } // for w
        return this;
    } // FillOne

    public: BitVec* FillZero()
    {
        for (uint w = 0; w < m_cWords; w++)
        {
            setWord(w, 0);
        } // for w
        return this;
    } // FillZero

    public: int FindLastOne() const
    {
        uint k = m_cBits;
        while (k > 0)
        {
            k -= 1;
            if (IsOne(k))
            {
                return k;
            }
        } // for

        return -1;
    } // FindLastOne

    // [G]
    public: uint GetLength() const
        { return m_cBits; }

    private: uint* getStart() const
    {
        return reinterpret_cast<uint*>(const_cast<BitVec*>(this) + 1);
    } // getStart

    private: uint getWord(uint const k) const
    {
        return getStart()[k];
    } // getWord

    // [H]
    public: uint Hash() const
    {
        uint nHashCode = m_cBits;
        for (uint i = 0; i < m_cWords; i++)
        {
            nHashCode = RotateLeft(nHashCode, 5);
            nHashCode ^= getWord(i);
        } // for
        return nHashCode;
    } // Hash

    // [I]
    public: bool IsOne(uint const k) const
    {
        return ! IsZero(k);
    } // IsOne

    public: bool IsZero(uint const k) const
    {
        ASSERT(k < GetLength());
        uint nWordPosn = k / BitsInWord;
        uint nBitPosn  = k % BitsInWord;
        return 0 == (getWord(nWordPosn) & (1 << nBitPosn));
    } // IsZero

    // [M]
    public: static BitVec* Make(uint const cBits)
    {
        uint const cWords = static_cast<uint>(Ceiling(cBits, BitsInWord));
        size_t const cb = sizeof(BitVec) + sizeof(uint) * cWords;
        return new(Context::Get()->Alloc(cb)) BitVec(cBits, cWords);
    } // Make

    // [N]
    public: static BitVec* Empty()
        { return &s_oEmpty; }

    // [S]
    public: void SetOne(uint const k)
    {
        ASSERT(k < GetLength());
        uint const w = k / BitsInWord;
        uint const b = k % BitsInWord;
        setWord(w, getWord(w) | (1 << b));
    } // SetOne

    private: uint setWord(uint const k, uint const w) const
    {
        return getStart()[k] = w;
    } // getWord

    public: void SetZero(uint const k)
    {
        ASSERT(k < GetLength());
        uint const w = k / BitsInWord;
        uint const b = k % BitsInWord;
        setWord(w, getWord(w) & ~(1 << b));
    } // SetZero
}; // BitVec

class DataFlowData : public LocalObject
{
    public: BitVec* m_pIn;
    public: BitVec* m_pKill;
    public: BitVec* m_pOut;

    public: DataFlowData()
    {
        m_pIn   = BitVec::Empty();
        m_pKill = BitVec::Empty();
        m_pOut  = BitVec::Empty();
    } // DataFlowData
}; // DataFlowData

class DataFlowBB : public BBlock
{
    // [C]
    public: void ClearIn(int k)   const { GetIn()->SetZero(k); }
    public: void ClearKill(int k) const { GetKill()->SetZero(k); }
    public: void ClearOut(int k)  const { GetOut()->SetZero(k); }

    // [G]
    public: BitVec* GetIn()   const { return m_pDfData->m_pIn; }
    public: BitVec* GetKill() const { return m_pDfData->m_pKill; }
    public: BitVec* GetOut()  const { return m_pDfData->m_pOut; }

    // [I]
    public: void InitDataFlow(uint const cRegs)
    {
        if (NULL == m_pDfData)
        {
            m_pDfData = new DataFlowData;
        }

        m_pDfData->m_pIn   = GetIn()->Adjust(cRegs)->FillZero();
        m_pDfData->m_pKill = GetKill()->Adjust(cRegs)->FillZero();
        m_pDfData->m_pOut  = GetOut()->Adjust(cRegs)->FillZero();
    } // InitDataFlow

    public: bool IsIn(int k)   const { return GetIn()->IsOne(k); }
    public: bool IsKill(int k) const { return GetKill()->IsOne(k); }
    public: bool IsOut(int k)  const { return GetOut()->IsOne(k); }

    // [S]
    public: BitVec* SetIn(BitVec* p)   { return m_pDfData->m_pIn   = p; }
    public: void    SetIn(int k)       { GetIn()->SetOne(k); }
    public: BitVec* SetKill(BitVec* p) { return m_pDfData->m_pKill = p; }
    public: void    SetKill(int k)     { GetKill()->SetOne(k); }
    public: BitVec* SetOut(BitVec* p)  { return m_pDfData->m_pOut  = p; }
    public: void    SetOut(int k)      { GetOut()->SetOne(k); }
}; // DataFlowBB

class Dfa
{
    public: static void SolveBackward(Function* const);
}; // Dfa

} // Compiler
} // TinyCl

#endif // !defined(INCLUDE_tinycl_c_ir_dfa_h)
