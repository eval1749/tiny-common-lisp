#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Ratio
// tinycl_ratio.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_ratio.cpp#3 $
//
// References:
//  [Knuth98] Donald E Knuth. The Art of Computer Programming. Volume 2, 1998
#include "../tinycl.h"
#include "./tinycl_bignum.h"
#include "./tinycl_ratio.h"

namespace TinyCl
{

namespace Internal
{

// [A]

/// <summary>
///   Adds two ratios.
///   <example>
///     n1   n2   n1d2 + n2d1
///      -- + -- = ----------
///      d1   d2      d1d2
///   </example>
/// </summary>
Val RatioImpl::Add(
    const RatioImpl* const pA,
    const RatioImpl* const pB )
{
    Val const n1 = pA->m_numerator;
    Val const d1 = pA->m_denominator;

    Val const n2 = pB->m_numerator;
    Val const d2 = pB->m_denominator;

    return Make(
        add(mul(n1, d2), mul(n2, d1)),
        mul(d1, d2) );
} // RatioImpl::Add

// [C]

/// <summary>
///   Compares two ratios. We use simple implementation described in
///   [Knuth98] 4.5.1 Fraction, Exercises 1.
/// </summary>
//= <FIXME date="2008-12-23" by="yosi@msn.com">
//    We should handle when n1 x d2 and n2 x d1 is large.
//    [Knuth98] doesn't have answer of this.
//= </FIXME>
int RatioImpl::Cmp(
    const RatioImpl* const pA,
    const RatioImpl* const pB )
{
    Val n1 = pA->m_numerator;
    Val d1 = pA->m_denominator;

    Val n2 = pB->m_numerator;
    Val d2 = pB->m_denominator;

    return BignumImpl::Cmp(mul(n1, d2), mul(n2, d1));
} // RatioImpl::Cmp

// [D]

/// <summary>
///   Divides ratio.
///   <example>
///     n1   n2   n1d2
///     -- / -- = -----
///     d1   d2   d1n2
///   </example>
/// </summary>
Val RatioImpl::Div(
    const RatioImpl* const pA,
    const RatioImpl* const pB )
{
    Val const n1 = pA->m_numerator;
    Val const d1 = pA->m_denominator;

    Val const n2 = pB->m_numerator;
    Val const d2 = pB->m_denominator;

    return Make(mul(n1, d2), mul(d1, n2));
} // RatioImpl::Div

// [M]
/// <summary>
///   Makes ratio object from specified numerator and denominator. Denominator
///   of the ratio is always positive integer.
/// </summary>
/// <param name="num">An integer for numerator.</param>
/// <param name="den">An integer for denominator.</param>
Val RatioImpl::Make(Val num, Val den)
{
    if (zero == num)
    {
        return zero;
    }

    Val k = gcdS2(num, den);

    // num and den can be stack bignum
    num = truncate(num, k);
    den = truncate(den, k);

    if (one == den)
    {
        return num;
    }

    if (minus_one == den)
    {
        return sub(zero, num);
    }

    if (lt(den, zero))
    {
        num = sub(0, num);
        den = sub(0, den);
    }

    Val const x = Thread::Get()->AllocRecord(ClassD_());
    RatioImpl* const p = x->StaticCast<RatioImpl>();
    p->m_denominator = den;
    p->m_numerator   = num;
    return x;
} // RatioImpl::Make

/// <summary>
///   Multiply ratio.
///   <example>
///     n1   n2   n1n2
///     -- x -- = ----
///     d1   d2   d1d2
///   </example>
/// </summary>
//= <FIXME date="2008-12-23" by="yosi@msn.com">
//=   Should we use method described in [Knuth98] 4.5.1 Fractions?
//=     g1 = (gcd n1 d2)
//=     g2 = (gcd d1 n2)
//=     { (n1/g1) x (n2/g2) } / { (d1/g2) x (d2/g1) }
//= </FIXME>
Val RatioImpl::Mul(
    const RatioImpl* const pA,
    const RatioImpl* const pB )
{
    Val const n1 = pA->m_numerator;
    Val const d1 = pA->m_denominator;

    Val const n2 = pB->m_numerator;
    Val const d2 = pB->m_denominator;

    return Make(mul(n1, n2), mul(d1, d2));
} // RatioImpl::Mul

// [N]
Val RatioImpl::Neg() const
{
    return Make(sub_1(m_numerator), m_denominator);
} // RatioImpl::Neg

// [S]
/// <summary>
///   Substract ratio.
///   <example>
///     n1   n2   n1d2 - n2d1
///     -- - -- = ----------
///     d1   d2      d1d2
///   </example>
/// </summary>
Val RatioImpl::Sub(
    const RatioImpl* const pA,
    const RatioImpl* const pB )
{
    Val const n1 = pA->m_numerator;
    Val const d1 = pA->m_denominator;

    Val const n2 = pB->m_numerator;
    Val const d2 = pB->m_denominator;

    Val const n = sub(mul(n1, d2), mul(n2, d1));
    Val const d = mul(d1, d2);
    return Make(n, d);
} // RatioImpl::Sub


// [T]

/// <summary>
///   Convert this ratio into float64.
/// </summary>
/// <seealso name="RatioImpl::ToFloat64"/>
//= <FIXME date="2008-12-23" by="yosi@msn.com">
//=   We should consider overflow of numerator and denominator but
//=   result.
//= </FIXME>
float32 RatioImpl::ToFloat32() const
{
    float32 fltNum = BignumImpl::ToFloat32(m_numerator);
    float32 fltDen = BignumImpl::ToFloat32(m_denominator);
    return fltNum / fltDen;
} // RatioImpl::ToFloat32

/// <summary>
///   Convert this ratio into float64.
/// </summary>
/// <seealso name="RatioImpl::ToFloat32"/>
float64 RatioImpl::ToFloat64() const
{
    float64 fltNum = BignumImpl::ToFloat64(m_numerator);
    float64 fltDen = BignumImpl::ToFloat64(m_denominator);
    return fltNum / fltDen;
} // RatioImpl::ToFloat64

/// <summary>
///   Truncates ratio. This method is used for implementing lisp function
///   TRUNCATE.
/// </summary>
void RatioImpl::Truncate(
    const RatioImpl*    const pA,
    const RatioImpl*    const pB,
    Val*                const out_q,
    Val*                const out_r )
{
    Val const n1 = pA->m_numerator;
    Val const d1 = pA->m_denominator;

    Val const n2 = pB->m_numerator;
    Val const d2 = pB->m_denominator;

    truncate(mul(n1, d2), mul(n2, d1), out_q, out_r);
} // RatioImpl::Truncate

} // Internal
} // TinyCl
