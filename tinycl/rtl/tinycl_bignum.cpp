#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Bignum
// tinycl_bignum.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_bignum.cpp#3 $
//
// Reference:
//  [Warrn03] Henry S. Warren, Jr. "Hacker's Delight". Addison Wesley. 2003
//  [White86] Jon L White, Reconfigurable, Retargetable Bignums: A Case Study
//  in Efficient, Portable Lisp System Building, Proceedings of the ACM
//  conference on Lisp & FP, 1986.
//
#include "../tinycl.h"

#include "./tinycl_bignum.h"
#include "./tinycl_float.h"
#include "./tinycl_ratio.h" // for BignumImpl::Div

namespace TinyCl
{

namespace Internal
{

template<class Operation_>
class LogArith_
{
    public: static Val Compute(
        const BignumImpl* const pA,
        const BignumImpl* const pB )
    {
        ObStackScope oObStackScope;

        // +1 for sign
        StackBignum oC = StackBignum::Alloc(
            max(pA->GetLength(), pB->GetLength()) + 1,
            pA );

        BignumImpl* const pC = oC.Get();

        BignumImpl::EnumLtoM oEnumC(pC);

        foreach (BignumImpl::EnumLtoM, oEnumB, pB)
        {
            oEnumC.Set(Operation_::Compute(oEnumC.Get(), oEnumB.Get()));
            oEnumC.Next();
        } // for b

        Bigit nSignB = pB->GetSign();
        while (! oEnumC.AtEnd())
        {
            oEnumC.Set(Operation_::Compute(oEnumC.Get(), nSignB));
            oEnumC.Next();
        } // while

        return pC->Pin();
    } // Compute
}; // LogArith_

#define define_logarith(mp_name, mp_op2, mp_op) \
    class Operation_ ## mp_name \
    { \
        public: static Bigit Compute(Bigit const a, Bigit const b) \
            { return mp_op (a mp_op2 b); } \
    }; \
    Val BignumImpl::mp_name( \
        const BignumImpl* const pA, \
        const BignumImpl* const pB ) \
    { return LogArith_<Operation_ ## mp_name>::Compute(pA, pB); }

/// <summary>
///   Represents bignum on stack
/// </summary>

// [A]
Val BignumImpl::Add(
    const BignumImpl* const pA,
    const BignumImpl* const pB )
{
    if (pA->IsZero()) return pB->Pin();
    if (pB->IsZero()) return pA->Pin();

    ObStackScope oObStackScope;

    StackBignum oC = StackBignum::Alloc(
        max(pA->GetLength(), pB->GetLength() + 1),  // +1 for carry
        pA );

    BignumImpl* const pC = oC.Get();

    EnumLtoM oEnumC(pC);
    Bigit nCarry = 0;
    foreach (EnumLtoM, oEnumB, pB)
    {
        DoubleBigit oAcc(nCarry, 0);
        oAcc += oEnumC.Get();
        oAcc += oEnumB.Get();

        oEnumC.Set(oAcc.low);
        oEnumC.Next();

        nCarry = oAcc.high;
    } // for B

    Bigit nB = pB->GetSign();
    while (! oEnumC.AtEnd())
    {
        DoubleBigit oAcc(nCarry, 0);
        oAcc += oEnumC.Get();
        oAcc += nB;

        oEnumC.Set(oAcc.low);
        oEnumC.Next();

        nCarry = oAcc.high;
    } // while

    return pC->Pin();
} // BignumImpl::Add

// [C]

int BignumImpl::Cmp(
    Val const a,
    Val const b )
{
    if (fixnump(a))
    {
        Int iA = Fixnum::Decode_(a);
        if (fixnump(b))
        {
            Int iB = Fixnum::Decode_(b);

            Int iDiff = iA - iB;

            if (0 == iDiff) return 0;
            return iDiff > 0 ? 1 : -1;
        } // if

        if (bignump(b))
        {
            const BignumImpl* const pB = b->StaticCast<BignumImpl>();

            if (iA >= 0)
            {
                return pB->IsMinus() ? 1 : -1;
            }
            else
            {
                return pB->IsPlus() ? 1 : -1;
            }
        } // if

        SignalTypeError(b, Qinteger);
    } // if fixnum

    if (bignump(a))
    {
        const BignumImpl* const pA = a->StaticCast<BignumImpl>();

        if (fixnump(b))
        {
            return pA->IsPlus() ? 1 : -1;
        }

        if (bignump(b))
        {
            const BignumImpl* const pB = b->StaticCast<BignumImpl>();
            return Cmp(pA, pB);
        } // if

        SignalTypeError(b, Qinteger);
    } // if bignum

    SignalTypeError(a, Qinteger);
} // BignumImpl::Cmp

int BignumImpl::Cmp(
    const BignumImpl* const pA,
    const BignumImpl* const pB )
{
    if (pA->IsPlus())
    {
        if (! pB->IsPlus())
        {
            return 1;
        }

        Int const nA = pA->GetLength();
        Int const nB = pB->GetLength();

        if (nA != nB)
        {
            return nA > nB ? 1 : -1;
        }

        EnumMtoL oEnumB(pB);
        foreach (EnumMtoL, oEnumA, pA)
        {
            if (oEnumA.Get() > oEnumB.Get()) return 1;
            if (oEnumA.Get() < oEnumB.Get()) return -1;
            oEnumB.Next();
        } // for a

        return 0;
    }
    else if (pA->IsMinus())
    {
        if (! pB->IsMinus())
        {
            return -1;
        }

        Int const nA = pA->GetLength();
        Int const nB = pB->GetLength();

        if (nA != nB)
        {
            return nA > nB ? 1 : -1;
        }

        EnumMtoL oEnumB(pB);
        foreach (EnumMtoL, oEnumA, pA)
        {
            if (oEnumA.Get() > oEnumB.Get()) return 1;
            if (oEnumA.Get() < oEnumB.Get()) return -1;
            oEnumB.Next();
        } // for a

        return 0;
    }
    else
    {
        if (pB->IsPlus()) return -1;
        if (pB->IsMinus()) return 1;
        return 0;
    }
} // BignumImpl::Cmp

uint BignumImpl::ComputeIntegerLength() const
{
    uint cBigits = static_cast<uint>(GetLength());

    SignedBigit const iMsb = GetMsb();

    if (iMsb >= 0)
    {
        if (0 == iMsb)
        {
            cBigits -= 1;
        }

        uint cBits = (cBigits - 1) * BigitBits;
        Bigit nBigit = GetStart()[cBigits - 1];
        while (0 != nBigit)
        {
            nBigit >>= 1;
            cBits += 1;
        } // while
        return cBits;
    }
    else
    {
        uint cBits = (cBigits - 1) * BigitBits;

        SignedBigit iBigit = GetStart()[cBigits - 1];
        while (-1 != iBigit)
        {
            iBigit >>= 1;
            cBits += 1;
        } // while
        return cBits;
    }
} // BignumImpl::ComputeIntegerLength

/// <summary>
///   Computes minium number of bigits to represent this bignum.
/// </summary>
uint BignumImpl::ComputeMinLen() const
{
    EnumMtoL oEnum(this);

    uint nCount = static_cast<uint>(GetLength());

    if (IsPlus())
    {
        while (nCount >= 2)
        {
            if (0 != oEnum.Get())
            {
                break;
            }

            oEnum.Next();

            if (static_cast<SignedBigit>(oEnum.Get()) < 0)
            {
                break;
            }

            nCount -= 1;
        } // while
    }
    else
    {
        while (nCount >= 2)
        {
            if (static_cast<Bigit>(-1) != oEnum.Get())
            {
                break;
            }

            oEnum.Next();

            if (static_cast<SignedBigit>(oEnum.Get()) >= 0)
            {
                break;
            }

            nCount -= 1;
        } // while
    }

    return nCount;
} // ComputeMinLen

// [D]
Val BignumImpl::Div(
    const BignumImpl* const pA,
    const BignumImpl* const pB )
{
    if (pB->IsOne())
    {
        return pB->Pin();
    }

    return RatioImpl::Make(pA->Encode(), pB->Encode());
} // BignumImpl::Div

// [L]
define_logarith(Logand, &, +)
define_logarith(Logeqv, ^, ~)
define_logarith(Logior, |, +)
define_logarith(Logxor, ^, +)

// [M]

/// <summary>
///  Signe multiplication.
///  See [Warrn03] p.130
/// </summary>
//
// Test Cases:
//   (* -268435456 2) = -536870912 (fixnum)
//   (* 4294967295 251658240) = 1080863910317260800
//
Val BignumImpl::Mul(
    const BignumImpl* const pAin,
    const BignumImpl* const pBin )
{
    const BignumImpl* pA = pAin;
    const BignumImpl* pB = pBin;

    if (pA->IsZero()) return zero;
    if (pB->IsZero()) return zero;
    if (pA->IsOne())  return pB->Pin();
    if (pB->IsOne())  return pA->Pin();

    Int nA = pA->GetLength();
    Int nB = pB->GetLength();

    // Make inner loop longer than outer loop
    if (nA < nB)
    {
        swap(pA, pB);
        swap(nA, nB);
    }

    ObStackScope oObStackScope;

    // Allocate result bigits
    // [White86] m x n requires m+n-1 or m+n bigits.
    // We need m+n bigits only if a=2^n and b=-2^m.
    StackBignum const oC = StackBignum::AllocZero(nA + nB);
    BignumImpl* const pC = oC.Get();

    if (1 == nB)
    {
        // m bigit x one bigit
        Bigit const nB0 = pB->GetStart()[0];
        Bigit nCarry = 0;
        EnumLtoM oEnumC(pC);
        foreach (EnumLtoM, oEnumA, pA)
        {
            DoubleBigit oAcc = DoubleBigit::Mul(oEnumA.Get(), nB0);
            oAcc += oEnumC.Get();
            oAcc += nCarry;
            nCarry = oAcc.high;
            oEnumC.Set(oAcc.low);
            oEnumC.Next();
        } // for a
        oEnumC.Set(nCarry);
    }
    else
    {
        // m bigit x n bigit
        EnumLtoM oEnumCj(pC);
        foreach (EnumLtoM, oEnumB, pB)
        {
            EnumLtoM oEnumC(&oEnumCj);
            Bigit nCarry = 0;
            foreach (EnumLtoM, oEnumA, pA)
            {
                DoubleBigit oAcc = DoubleBigit::Mul(
                    oEnumA.Get(),
                    oEnumB.Get() );

                oAcc += oEnumC.Get();
                oAcc += nCarry;
                nCarry = oAcc.high;
                oEnumC.Set(oAcc.low);
                oEnumC.Next();
            } // for a
            oEnumC.Set(nCarry);
            oEnumCj.Next();
        } // for b
    } // if

    // Now pC has unsigned product. Correct by
    //  subtracting v x 2^(32+m) if u < 0 and
    //  subtracting u x 2^(32+n) if v < 0
    if (pA->IsMinus())
    {
        Bigit nBorrow = 0;
        EnumLtoM oEnumC(pC, nA);
        foreach (EnumLtoM, oEnumB, pB)
        {
            DoubleBigit oAcc(oEnumC.Get(), 0);
            oAcc -= oEnumB.Get();
            oAcc -= nBorrow;
            nBorrow -= static_cast<SignedBigit>(oAcc.high);
            oEnumC.Set(oAcc.low);
            oEnumC.Next();
        } // for b
        oEnumC.Set(nBorrow ? BigitMinus1 : 0);
    } // if a < 0

    if (pB->IsMinus())
    {
        Bigit nBorrow = 0;
        EnumLtoM oEnumC(pC, nB);
        foreach (EnumLtoM, oEnumA, pA)
        {
            DoubleBigit oAcc(oEnumC.Get(), 0);
            oAcc -= oEnumA.Get();
            oAcc -= nBorrow;
            nBorrow -= static_cast<SignedBigit>(oAcc.high);
            oEnumC.Set(oAcc.low);
            oEnumC.Next();
        } // for a
    } // if b < 0

    return pC->Pin();
} // BignumImpl::Mul

// [N]
Val BignumImpl::Neg() const
{
    BignumInt oA(0);
    return Sub(&oA, this);
} // BignumImpl::Neg

// [P]
Val BignumImpl::Pin() const
{
    uint cBigits = ComputeMinLen();

    if (1 == cBigits)
    {
        SignedBigit iVal = static_cast<SignedBigit>(GetLsb());
        if (iVal >= Fixnum::MostNegative && iVal <= Fixnum::MostPositive)
        {
            return Fixnum::Encode(iVal);
        }
    }

    if (Mm::IsHeapPtr(this) && static_cast<uint>(GetLength()) == cBigits)
    {
        return Encode();
    }

    Val b = Thread::Get()->AllocBinVec(
        CLASSD_bignum, 
        Fixnum::Encode(cBigits) );

    ::CopyMemory(
        b->StaticCast<Bignum>()->GetStart(),
        GetStart(),
        sizeof(Bigit) * cBigits );

    return b;
} // BignumImpl::Pin

Val StackBignumImpl::PinNeg() const
{
    Bigit nCarry = 1;
    foreach (EnumLtoM, oEnum, this)
    {
        DoubleBigit oAcc(~oEnum.Get(), 0);
        oAcc += nCarry;
        oEnum.Set(oAcc.low);
        nCarry = oAcc.high;
    } // for each bigit

    return Pin();
} // StackBignumImpl::PinNeg

// [S]

Val BignumImpl::ShiftLeft(uint const nCount) const
{
    ASSERT(nCount >= 1);

    const BignumImpl* const pA = this;
    Int const nA = pA->GetLength();
    Int const cBits = nA * BigitBits + nCount;
    Int const cBigits = Ceiling(cBits, BigitBits);

    ObStackScope oObStackScope;

    StackBignum oB = StackBignum::AllocZero(cBigits);
    BignumImpl* const pB = oB.Get();

    uint const nMod = static_cast<uint>(nCount % BigitBits);
    uint const nPos = static_cast<uint>(nCount / BigitBits);

    ::CopyMemory(
        pB->GetStart() + nPos,
        pA->GetStart(),
        sizeof(Bigit) * nA );

    if (0 != nMod)
    {
        if (pA->IsMinus())
        {
            pB->GetStart()[cBigits - 1] = static_cast<Bigit>(-1);
        }

        EnumLtoM oEnumB(pB, nPos);
        Bigit nCarry = 0;
        do
        {
            DoubleBigit oAcc = DoubleBigit::Shl(oEnumB.Get(), nMod);
            oAcc.low |= nCarry;
            oEnumB.Set(oAcc.low);
            nCarry = oAcc.high;
            oEnumB.Next();
        } while (! oEnumB.AtEnd());
    }

    return pB->Pin();
} // BignumImpl::ShiftLeft

Val BignumImpl::ShiftRight(uint const nCount) const
{
    ASSERT(nCount >= 1);

    const BignumImpl* const pA = this;
    Int const nA = pA->GetLength();
    Int const cBits = nA * BigitBits - nCount;

    if (cBits <= 0)
    {
        return zero;
    }

    int const nMod = nCount % BigitBits;

    Bigit nCarry = pA->GetSign() << (BigitBits - nMod);

    if (cBits <= Fixnum::Bits)
    {
        return Fixnum::Encode(nCarry | (pA->GetMsb() >> nMod));
    }
    else
    {
        ObStackScope oObStackScope;

        Int const cBigits = Ceiling(cBits, BigitBits);

        StackBignum oB = pA->IsMinus() ?
            StackBignum::AllocMinusOne(cBigits) :
            StackBignum::AllocMinusOne(cBigits);

        BignumImpl* const pB = oB.Get();

        BignumImpl::EnumMtoL oEnumA(pA);

        foreach (BignumImpl::EnumMtoL, oEnumB, pB)
        {
            oEnumB.Set(nCarry | (oEnumA.Get() >> nMod));
            nCarry = oEnumA.Get() << (BigitBits - nMod);
            oEnumA.Next();
        } // for

        return pB->Pin();
    }
} // BignumImpl::ShiftRight

Val BignumImpl::Sub(
    const BignumImpl* const pA,
    const BignumImpl* const pB )
{
    if (pB->IsZero()) return pA->Pin();

    Int const nA = pA->GetLength();
    Int const nB = pB->GetLength();

    ObStackScope oObStackScope;
    StackBignum oC = StackBignum::Alloc(max(nA, nB) + 1, pA);
    BignumImpl* const pC = oC.Get();

    EnumLtoM oEnumC(pC);

    Bigit nBorrow = 0;
    foreach (EnumLtoM, oEnumB, pB)
    {
        DoubleBigit oAcc(oEnumC.Get(), 0);
        oAcc -= oEnumB.Get();
        oAcc -= nBorrow;

        oEnumC.Set(oAcc.low);
        oEnumC.Next();

        nBorrow = -static_cast<SignedBigit>(oAcc.high);
    } // for b

    Bigit const nSignB = pB->GetSign();
    if (nSignB != nBorrow)
    {
        while (! oEnumC.AtEnd())
        {
            DoubleBigit oAcc(oEnumC.Get(), 0);
            oAcc -= nSignB;
            oAcc -= nBorrow;

            oEnumC.Set(oAcc.low);
            oEnumC.Next();

            nBorrow = -static_cast<SignedBigit>(oAcc.high);
        } // while
    } // if

    return pC->Pin();
} // BignumImpl::Sub

// [T]

// See also tinycl_float.h
//  Convert bignum to float32 if x is not
//    (<= (ash #x-FFFFFF 104) x (ash x #xFFFFFF 104)),
//      104 = ExponentMax - ExponentBias - NormalPrecision
//          = 255 - 127 - 24
//  returns positive or negative infinity.
//
//  o Extract 24bit from MSB.
float32 BignumImpl::ToFloat32(Val x)
{
    if (fixnump(x))
    {
        return static_cast<float32>(Fixnum::Decode_(x));
    }

    if (BignumImpl* p = x->DynamicCast<BignumImpl>())
    {
        return p->ToFloat32();
    }

    if (RatioImpl* p = x->DynamicCast<RatioImpl>())
    {
        return p->ToFloat32();
    }

    SignalTypeError(x, Qrational);
} // BignumImpl::ToFloat32

// See also tinycl_float.h
//  Convert bignum to float64 if x is not
//    (<= (ash (1+ (ash -1 53)) 971) x (ash x (1- (ash 1 53)) 971))
//      971 = ExponentMax - ExponentBias - NormalPrecision
//          = 2047 - 1023 - 53
//  returns positive or negative infinity.
//
//  o Extract 53bit from MSB.
float64 BignumImpl::ToFloat64(Val x)
{
    if (fixnump(x))
    {
        return static_cast<float64>(Fixnum::Decode_(x));
    }

    if (BignumImpl* p = x->DynamicCast<BignumImpl>())
    {
        return p->ToFloat64();
    }

    if (RatioImpl* p = x->DynamicCast<RatioImpl>())
    {
        return p->ToFloat64();
    }

    SignalTypeError(x, Qrational);
} // BignumImpl::ToFloat64

// Note: For D6 testing (B=32),
//  we can use following from 4500/501 where B=10
//
//  (setq u #x7FFFFFFF800000000000000000000000)
//          ; 170141183420855150474555134919112130560
//  (setq v #x800000000000000000000001)
//          ; 39614081257132168796771975169
//  (/ u v)
//      56713727806951716824851711639704043520/13204693752377389598923991723
//      #x2AAAAAAA800000000000000000000000/2AAAAAAAAAAAAAAAAAAAAAAB
//
void BignumImpl::Truncate(
    const BignumImpl*   const pA,
    const BignumImpl*   const pB,
    Val*                const out_q,
    Val*                const out_r )
{
    class TruncateAux
    {
        public: static void Dispatch(
            int                 const rgfSign,
            const BignumImpl*   const pA,
            const BignumImpl*   const pB,
            Val*                const out_q,
            Val*                const out_r )
        {
            if (BignumImpl::Cmp(pA, pB) < 0)
            {
                if (NULL != out_r) *out_r = pA->Pin();
                if (NULL != out_q) *out_q = zero;
                return;
            } // if a < b

            ObStackScope oObStackScope;

            StackBignum oU = StackBignum::Alloc(pA->GetLength() + 1, pA);
            BignumImpl* const pU = oU.Get();

            StackBignum oV = StackBignum::Alloc(pB->GetLength(), pB);
            BignumImpl* const pV = oV.Get();

            // +1 for m+1
            // +1 for normalize (sign bigit)
            StackBignum oQ = StackBignum::AllocZero(
                pU->GetLength() - pV->GetLength() + 2 );

            StackBignumImpl* const pQ = oQ.Get();
            // oQ[m+n]=0, oQ[m-n+1]=0

            if (NULL == out_r)
            {
                ASSERT(NULL != out_q);

                if (1 == pV->GetLength())
                {
                    truncateOne(pU, pV->GetStart()[0], pQ, NULL);
                }
                else
                {
                    truncateBig(pU, pV, pQ, NULL);
                }

                switch (rgfSign)
                {
                case Signs_Pos_Pos:  // P / P => P, P
                    *out_q = pQ->Pin();
                    break;

                case Signs_Neg_Pos: // N / P => N, N
                    *out_q = pQ->PinNeg();
                    break;

                case Signs_Pos_Neg: // P / N => N, P
                    *out_q = pQ->PinNeg();
                    break;

                case Signs_Neg_Neg: // N / N => P, N
                    *out_q = pQ->Pin();
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // switch rgfSign
            }
            else
            {
                // +1 for normalize
                StackBignum oR = StackBignum::AllocZero(pV->GetLength() + 1);
                StackBignumImpl* const pR = oR.Get();

                if (1 == pV->GetLength())
                {
                    truncateOne(pU, pV->GetStart()[0], pQ, pR);
                }
                else
                {
                    truncateBig(pU, pV, pQ, pR);
                }

                switch (rgfSign)
                {
                case Signs_Pos_Pos:  // P / P => P, P
                    if (NULL != out_q) *out_q = pQ->Pin();
                    *out_r = pR->Pin();
                    break;

                case Signs_Neg_Pos: // N / P => N, N
                    if (NULL != out_q) *out_q = pQ->PinNeg();
                    *out_r = pR->PinNeg();
                    break;

                case Signs_Pos_Neg: // P / N => N, P
                    if (NULL != out_q) *out_q = pQ->PinNeg();
                    *out_r = pR->Pin();
                    break;

                case Signs_Neg_Neg: // N / N => P, N
                    if (NULL != out_q) *out_q = pQ->Pin();
                    *out_r = pR->PinNeg();
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // switch rgfSign
            } // if
        } // Dispatch

        private: static void truncateBig(
            const BignumImpl*   const pU,
            const BignumImpl*   const pV,
            BignumImpl*         const pQ,
            BignumImpl*         const pR )
        {
            Int m = pU->GetLength() - 1;
            Int n = pV->GetLength();

            Bigit* un = pU->GetStart();
            Bigit* vn = pV->GetStart();

            // D1: Normalize
            uint nShift = 0;
            {
                {
                    Bigit dv = vn[n - 1];
                    if (0 == dv)
                    {
                        n -= 1;
                        dv = vn[n - 1];
                    }
                    ASSERT(0 != dv);

                    while (dv < BaseDiv2)
                    {
                        dv <<= 1;
                        nShift += 1;
                    }
                } // nShift

                if (0 == nShift)
                {
                    un[m] = 0;
                }
                else
                {
                    {
                        Bigit nCarry = 0;
                        foreach (EnumLtoM, oEnum, pU)
                        {
                            Bigit const nHigh =
                                oEnum.Get() >> (BigitBits - nShift);

                            oEnum.Set((oEnum.Get() << nShift) | nCarry);
                            nCarry = nHigh;
                        } // for u
                    }

                    {
                        Bigit nCarry = 0;
                        foreach (EnumLtoM, oEnum, pV)
                        {
                            Bigit const nHigh =
                                oEnum.Get() >> (BigitBits - nShift);

                            oEnum.Set((oEnum.Get() << nShift) | nCarry);
                            nCarry = nHigh;
                        } // for v
                    }
                } // if
            } // D1

            {
                Bigit* q = pQ->GetStart();

                // D2: Q[m-n-1...0] := U[m-1...n] / V[n-1...0]
                for (Int j = m - n; j >= 0; j -= 1)
                {
                    // D3: Calculate qhat
                    Bigit qhat;
                    {
                        Bigit rhat;

                        if (un[j+n] >= vn[n-1])
                        {
                            qhat = BigitMinus1;
                            rhat = un[j+n-1];
                            goto overflow;
                        }
                        else
                        {
                            DoubleBigit::Div oDiv =
                                DoubleBigit::Truncate(un[j+n-1], un[j+n], vn[n-1]);

                            qhat = oDiv.q;
                            rhat = oDiv.r;
                        }

                        loop:
                            {
                                DoubleBigit aa = DoubleBigit::Mul(qhat, vn[n-2]);
                                DoubleBigit bb = DoubleBigit(un[j+n-2], rhat);

                                if (aa <= bb) goto done;
                            }

                            qhat -= 1;

                        overflow:
                            {
                                bool fCarry;
                                rhat = DoubleBigit::Add(rhat, vn[n-1], &fCarry);
                                if (! fCarry) goto loop;
                            }
                        done: ;
                    } // qhat

                    q[j] = qhat;

                    // D4: mul and sub
                    //  U[n+j...j] := U[n+j..j] - qhat * V[n-1...0]
                    bool fBorrow;
                    {
                        Bigit nBorrow = 0;
                        for (Int i = 0; i < n; i++)
                        {
                            DoubleBigit ullM = DoubleBigit::Mul(qhat, vn[i]);
                            DoubleBigit ullS(un[i+j], 0);
                                ullS -= nBorrow;
                                ullS -= ullM.low;
                            un[i+j]  = ullS.low;
                            nBorrow = ullM.high - ullS.high;
                        } // for i

                        Bigit ujn = un[j+n] - nBorrow;
                        un[j+n] = ujn;

                        fBorrow = static_cast<int>(ujn) < 0;
                    } // D4

                    if (fBorrow)
                    {
                        q[j] -= 1;

                        // D6: Add Back
                        //  U[j+n...j] := U[j+n...j] + V[n-1...0]
                        {
                            Bigit nCarry = 0;
                            for (Int i = 0; i < n; i++)
                            {
                                DoubleBigit ullS(nCarry, 0);
                                    ullS += un[i+j];
                                    ullS += vn[i];
                                un[i+j] = ullS.low;
                                nCarry  = ullS.high;
                            } // for v
                            un[j+n] += nCarry;
                        } // D6
                    } // if fBorrow
                } // for j
            }

            // D8: Shift Right
            if (NULL != pR)
            {
                ::memcpy(
                    pR->GetStart(),
                    pU->GetStart(),
                    sizeof(Bigit) * pR->GetLength() );

                if (0 != nShift)
                {
                    Bigit  nCarry = 0;
                    foreach (EnumMtoL, oEnum, pR)
                    {
                        Bigit nLowBits = oEnum.Get() << (BigitBits - nShift);
                        oEnum.Set((oEnum.Get() >> nShift) | nCarry);
                        nCarry = nLowBits;
                    } // for
                } // if
            } // if
        } // truncateBig

        private: static void truncateOne(
            const BignumImpl*   const pU,
            Bigit               const nV,
            BignumImpl*         const pQ,
            BignumImpl*         const pR )
        {
            Bigit nRem = 0;

            if (NULL != pQ)
            {
                BignumImpl::EnumMtoL oEnumQ(pQ);
                oEnumQ.Set(0);
                oEnumQ.Next();
                foreach (BignumImpl::EnumMtoL, oEnumU, pU)
                {
                    DoubleBigit::Div oDiv = DoubleBigit::Truncate(
                        oEnumU.Get(),
                        nRem,
                        nV );
                    oEnumQ.Set(oDiv.q);
                    nRem = oDiv.r;
                    oEnumQ.Next();
                } // for U
            }
            else
            {
                foreach (BignumImpl::EnumMtoL, oEnumU, pU)
                {
                    DoubleBigit::Div oDiv = DoubleBigit::Truncate(
                        oEnumU.Get(),
                        nRem,
                        nV );
                    nRem = oDiv.r;
                } // for U
            } // if

            if (NULL != pR)
            {
                pR->GetStart()[0] = nRem;
                pR->GetStart()[1] = 0;
            }
        } // truncateOne
    }; // TruncateAux

    if (pB->IsZero())
    {
        error(Qdivision_by_zero,
            Koperation, Qtruncate,
            Koperands,  list(pA->Pin(), pB->Pin()) );
    }

    int rgfSign = 0;
    if (pA->IsMinus()) rgfSign |= Signs_Neg_Pos;
    if (pB->IsMinus()) rgfSign |= Signs_Pos_Neg;

    switch (rgfSign)
    {
    case Signs_Pos_Pos:
        TruncateAux::Dispatch(rgfSign, pA, pB, out_q, out_r);
        break;

    case Signs_Neg_Pos:
    {
        ObStackScope oObStackScope;
        StackBignum oA = StackBignum::AllocNeg(pA);
        TruncateAux::Dispatch(rgfSign, oA.Get(), pB, out_q, out_r);
        break;
    } // Signs_Neg_Pos

    case Signs_Pos_Neg:
    {
        ObStackScope oObStackScope;
        StackBignum oB = StackBignum::AllocNeg(pB);
        TruncateAux::Dispatch(rgfSign, pA, oB.Get(), out_q, out_r);
        break;
    } // Signs_Neg_Pos

    case Signs_Neg_Neg:
    {
        ObStackScope oObStackScope;
        StackBignum oA = StackBignum::AllocNeg(pA);
        StackBignum oB = StackBignum::AllocNeg(pB);
        TruncateAux::Dispatch(rgfSign, oA.Get(), oB.Get(), out_q, out_r);
        break;
    } // Signs_Neg_Neg

    default:
        CAN_NOT_HAPPEN();
    } // switch sign
} // BignumImpl::Truncate

} // Internal
} // TinyCl
