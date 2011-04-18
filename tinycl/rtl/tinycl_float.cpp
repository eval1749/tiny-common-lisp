#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Float
// tinycl_float.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_float.cpp#1 $
//
#include "../tinycl.h"
#include "./tinycl_bignum.h"
#include "./tinycl_float.h"

extern "C"
{
    float __cdecl ceilf(float);
    double __cdecl ceil(double);

    float __cdecl floorf(float);
    double __cdecl floor(double);

    float __cdecl rintf(float);
    double __cdecl rint(double);

    float __cdecl truncatef(float);
    double __cdecl truncate(double);
} // extern "C"

namespace TinyCl
{

namespace Internal
{

// [C]

FLOAT_IMPL_(FpClass, Classify)(const Layout* p)
{
    uint_ const uS = Self_::GetSignificand(p);
    switch (p->m_nExponent)
    {
    case Self_::ExponentMin:
        if (0 == uS)
        {
            return FpClass_Zero;
        }
        else
        {
            return FpClass_Subnormal;
        }

    case Self_::ExponentMax:
        if (0 == uS)
        {
            return FpClass_Infinity;
        }
        else
        {
            return
                uS & static_cast<uint_>(1) << (Self_::SignificandBits - 1) ?
                    FpClass_NaN :
                    FpClass_SNaN;
        }

    default:
        return FpClass_Normal;
    } // switch exp
} // FloatImpl_::Classify

// [F]

// For BignumImpl::ToFloat{32,64}
FLOAT_IMPL_(float_, FromBignum)(
    const BignumImpl* const pA,
    uint              const nSign )
{
    ASSERT(pA->IsPlus());

    float_ flt = 0;

    Self_::Layout* p = reinterpret_cast<Self_::Layout*>(
        &flt );

    p->m_nSign = nSign;

    const BignumImpl* const pMostPositive =
        Self_::MostPositiveInBignum_()->StaticCast<BignumImpl>();

    if (BignumImpl::Cmp(pA, pMostPositive) > 0)
    {
        p->m_nExponent = Self_::ExponentMax;
        return flt;
    }

    uint const k = pA->ComputeIntegerLength();
    int  const m = k - Self_::NormalPrecision;
    uint const e = k - 1 + Self_::ExponentBias;
    ASSERT(e < Self_::ExponentMax);

    p->m_nExponent = e;

    if (m > 0)
    {
        Self_::SetSignificand(p, pA->ShiftRight(m));
    }
    else if (m < 0)
    {
        Self_::SetSignificand(p, pA->ShiftLeft(-m));
    }
    else
    {
        CAN_NOT_HAPPEN();
    }

    return flt;
} // FloatImpl_::FromBignum

// [M]

Val Float32Impl::Make(float32 flt)
{
    Val x = Thread::Get()->AllocBinObj(ClassD_());
    x->StaticCast<Float32Impl>()->m_flt = flt;
    return x;
} // Float32Impl::Make

Val Float64Impl::Make(float64 dbl)
{
    Val x = Thread::Get()->AllocBinObj(ClassD_());
    x->StaticCast<Float64Impl>()->m_dbl = dbl;
    return x;
} // Float64Impl::Make

// [S]

/// <summary>
///   Sets significand of this float64 number from the least 52 bit of
///   specified bignum.
///   <p>
///     We used this function for implementing FromBignum method. This
///     bignum is 53 bit integer and MSB is always one for normal
///     floating number.
///   </p>
/// </summary>
void Float64Impl::SetSignificand(
    Layout* const p,
    Val     const a )
{
    uint64 u64;

    #if 4 == SIZEOF_VAL
    {
        if (fixnump(a))
        {
            u64 = Fixnum::Decode_(a);
        }
        else
        {
            const BignumImpl* const pA = a->StaticCast<BignumImpl>();
            u64 = pA->GetStart()[1];
            u64 <<= BigitBits;
            u64 |= pA->GetStart()[0];
        }
    }
    #elif 8 == SIZEOF_VAL
    {
        u64 = Fixnum::Decode_(a);
    }
    #else
        #error "Unsupported SIZEOF_VAL"
    #endif

    p->m_nSignificandL = static_cast<uint>(
        u64 & static_cast<uint>(-1) );

    p->m_nSignificandH = static_cast<uint>(
        u64 >> 32 );
} // Float64Impl::SetSignificand

// [T]

float32 BignumImpl::ToFloat32() const
{
    if (IsPlus())
    {
        return Float32Impl::FromBignum(this, 0);
    }
    else
    {
        ObStackScope oScope;
        StackBignum oA = StackBignum::AllocNeg(this);
        return Float32Impl::FromBignum(oA.Get(), 1);
    }
} // BignumImpl::ToFloat32

//  Convert bignum to float64 if x is not
//    (<= (ash (1+ (ash -1 53)) 971) x (ash x (1- (ash 1 53)) 971))
//      971 = ExponentMax - ExponentBias - NormalPrecision
//          = 2047 - 1023 - 53
//  returns positive or negative infinity.
//
//  o Extract 53bit from MSB.
//
float64 BignumImpl::ToFloat64() const
{
    if (IsPlus())
    {
        return Float64Impl::FromBignum(this, 0);
    }
    else
    {
        ObStackScope oScope;
        StackBignum oA = StackBignum::AllocNeg(this);
        return Float64Impl::FromBignum(oA.Get(), 1);
    }
} // BignumImpl::ToFloat64()

FLOAT_IMPL_(Val, toInt)(const Layout_* const p)
{
    switch (Self_::Classify(p))
    {
    case FpClass_Normal:
    {
        int_ ll = 1;
        ll <<= Self_::SignificandBits;
        ll |= Self_::GetSignificand(p);
        ll *= (1 - p->m_nSign * 2);

        Bignum_ oNum(ll);

        int const iExponent = p->m_nExponent - Self_::ExponentBias;

        return ash(oNum, Fixnum::Encode(iExponent - Self_::NormalPrecision + 1));
    } // FpClass_Normal

    case FpClass_Subnormal:
    case FpClass_Zero:
        return zero;

    case FpClass_Infinity:
    case FpClass_NaN:
    case FpClass_SNaN:
    {
        float_ const flt = *reinterpret_cast<const float_*>(p);
        error(Qarithmetic_error,
            Qoperation, Qrational,
            Koperands, list(Self_::Make(flt)) );
    } // FpClass_Infinity

    default:
        CAN_NOT_HAPPEN();
    } // switch fpclass
} // FloatImpl_::ToInt

Val Float32Impl::ToInt() const
    { return toInt(GetLayout()); }

Val Float64Impl::ToInt() const
    { return toInt(GetLayout()); }

float32 Float32Impl::Truncate_1(float32 fltA)
    { return ::truncatef(fltA); }

float64 Float64Impl::Truncate_1(float64 fltA)
    { return ::truncate(fltA); }

} // Internal
} // TinyCl
