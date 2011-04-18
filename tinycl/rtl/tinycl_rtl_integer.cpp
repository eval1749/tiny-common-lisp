#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Integer
// tinycl_integer.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_integer.cpp#2 $
//
#include "../tinycl.h"

#include "./tinycl_bignum.h"
#include "./tinycl_float.h"

namespace TinyCl
{

using namespace Internal;

enum OpTypes
{
    Operands_Big_Big = 0,
    Operands_Big_Fix = 1,
    Operands_Fix_Big = 2,
    Operands_Fix_Fix = 3,
}; // OpTypes

template<class Operation_>
class LogArith_
{
    public: static Val Dispatch(Val const a, Val const b)
    {
        int rgfOperands = 0;
        if (fixnump(a)) rgfOperands |= Operands_Fix_Big;
        if (fixnump(b)) rgfOperands |= Operands_Big_Fix;

        switch (rgfOperands)
        {
        case Operands_Fix_Fix:
        {
            Int const iC = Operation_::Compute(
                Fixnum::Decode_(a),
                Fixnum::Decode_(b) );
            return MakeInt(iC);
        } // Operation_Fix_Fix

        case Operands_Fix_Big:
        {
            if (! bignump(b)) SignalTypeError(b, Qinteger);
            BignumInt oA(Fixnum::Decode_(a));
            const BignumImpl* pB = b->StaticCast<BignumImpl>();
            return Operation_::Compute(&oA, pB);
        } // Operands_Fix_Big

        case Operands_Big_Fix:
        {
            if (! bignump(a)) SignalTypeError(a, Qinteger);
            const BignumImpl* pA = a->StaticCast<BignumImpl>();
            BignumInt oB(Fixnum::Decode_(b));
            return Operation_::Compute(pA, &oB);
        } // Operands_Fix_Big

        case Operands_Big_Big:
        {
            if (! bignump(a)) SignalTypeError(a, Qinteger);
            if (! bignump(b)) SignalTypeError(b, Qinteger);
            const BignumImpl* pA = a->StaticCast<BignumImpl>();
            const BignumImpl* pB = b->StaticCast<BignumImpl>();
            return Operation_::Compute(pA, pB);
        } // Operands_Fix_Big

        default:
            CAN_NOT_HAPPEN();
        } // switch operands
    } // Dispatch
}; // LogArith_

#define defun_logarith(mp_name, mp_Name, mp_op2, mp_op) \
    class Op ## mp_Name \
    { \
        public: static Int Compute(Int const a, Int const b) \
            { return mp_op (a mp_op2 b); } \
\
        public: static Val Compute( \
            const BignumImpl* const pA, \
            const BignumImpl* const pB ) \
            { return BignumImpl::mp_Name (pA, pB); } \
    }; \
    defun(mp_name, (Val const a, Val const b)) \
        { return LogArith_<Op ## mp_Name>::Dispatch(a, b); }

// [A]

defun(CommonLisp::ash, (Val x, Val n))
{
    if (fixnump(n))
    {
        Int iN = Fixnum::Decode_(n);

        if (fixnump(x))
        {
            Int iX = Fixnum::Decode_(x);

            if (iN > 0)
            {
                BignumInt oX(iX);
                return ash(oX, n);
            }

            if (iN < 0)
            {
                return Fixnum::Encode(iX >> -iN);
            }

            return x;
        } // if fixnum

        if (bignump(x))
        {
            const BignumImpl* const pX = x->StaticCast<BignumImpl>();
            if (iN > 0)
            {
                return pX->ShiftLeft(static_cast<uint>(iN));
            }

            if (iN < 0)
            {
                return pX->ShiftRight(static_cast<uint>(-iN));
            }

            return x;
        } // if bignum
    } // if n is fixum

    if (bignump(n))
    {
        if (fixnump(x) || bignump(x))
        {
            error("Too larget shift count ~D.", n);
        }

        SignalTypeError(n, Qinteger);
    }

    SignalTypeError(x, Qinteger);
} // ash

// [C]

/// <summary>
///   Coerces bignum to float32. This function is used for implementing
///   function float.
/// </summary>
/// <seealso name="coerceSbignumSfloat64"/>
defun(coerceSbignumSfloat32, (Val n))
{
    if (BignumImpl* p = n->DynamicCast<BignumImpl>())
    {
        return Float32Impl::Make(p->ToFloat32());
    }

    SignalTypeError(n, Qbignum);
} // coerceSbignumSfloat32

/// <summary>
///   Coerces bignum to float64. This function is used for implementing
///   function float.
/// </summary>
/// <seealso name="coerceSbignumSfloat32"/>
defun(coerceSbignumSfloat64, (Val n))
{
    if (BignumImpl* p = n->DynamicCast<BignumImpl>())
    {
        return Float64Impl::Make(p->ToFloat64());
    }

    SignalTypeError(n, Qbignum);
} // coerceSbignumSfloat64

// [G]

defun(gcdS2, (Val a, Val b))
{
    if (fixnump(a))
    {
        Int iA = Fixnum::Decode_(a);
        if (iA < 0)
        {
            BignumInt oA(-iA);
            return gcdS2(oA, b);
        }
    }
    else if (bignump(a))
    {
        if (a->StaticCast<BignumImpl>()->IsMinus())
        {
            a = sub(zero, a);
        }
    }
    else
    {
        SignalTypeError(a, Qinteger);
    }

    if (fixnump(b))
    {
        Int iB = Fixnum::Decode_(b);
        if (iB < 0)
        {
            BignumInt oB(-iB);
            return gcdS2(a, oB);
        }
    }
    else if (bignump(b))
    {
        if (b->StaticCast<BignumImpl>()->IsMinus())
        {
            b = sub(zero, b);
        }
    }
    else
    {
        SignalTypeError(b, Qinteger);
    }

    for (;;)
    {
        int rgfOperands = 0;
        if (fixnump(a)) rgfOperands |= Operands_Fix_Big;
        if (fixnump(b)) rgfOperands |= Operands_Big_Fix;

        switch (rgfOperands)
        {
        case Operands_Fix_Fix:
        {
            Int m = Fixnum::Decode_(a);
            Int n = Fixnum::Decode_(b);
            while (0 != n)
            {
                Int temp = m;
                m = n;
                n = temp % n;
            } // while
            return Fixnum::Encode(m);
        } // fixnum fixnum

        case Operands_Fix_Big:
            swap(a, b);
            // FALLTHROUGH

        case Operands_Big_Fix:
            if (zero == b)
            {
                return a;
            }
            // FALLTHROUGH

        case Operands_Big_Big:
        {
            Val temp = a;
            a = b;
            b = rem(temp, b);
            break;
        } // bignum bignum

        default:
            CAN_NOT_HAPPEN();
        } // switch type
    } // for
} // gcdS2

// [L]
defun_logarith(logandS2, Logand, &, +)
defun_logarith(logeqvS2, Logeqv, ^, ~)
defun_logarith(logiorS2, Logior, |, +)
defun_logarith(logxorS2, Logxor, ^, +)

} // TinyCl
