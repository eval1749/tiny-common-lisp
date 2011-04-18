//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Ratio Implementation
// tinycl_ratio.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_ratio.h#2 $
//
#if !defined(INCLUDE_tinycl_ratio_h)
#define INCLUDE_tinycl_ratio_h

namespace TinyCl
{

namespace Internal
{

class RatioImpl : public Record_<RatioImpl, Layout_ratio>
{
    public: static Val ClassD_() { return CLASSD_ratio; }

    // [A]
    public: static Val Add(const RatioImpl*, const RatioImpl*);

    // [C]
    public: static void Ceiling(
        const RatioImpl*,
        const RatioImpl*,
        Val*,
        Val* );

    public: static int Cmp(const RatioImpl*, const RatioImpl*);

    // [D]
    public: static Val Div(const RatioImpl*, const RatioImpl*);

    // [F]
    public: static void Floor(
        const RatioImpl*,
        const RatioImpl*,
        Val*,
        Val* );

    // [M]
    public: static Val Make(Val, Val);
    public: static Val Mul(const RatioImpl*, const RatioImpl*);

    // [N]
    public: Val Neg() const;

    // [R]
    public: static void Round(
        const RatioImpl*,
        const RatioImpl*,
        Val*,
        Val* );

    // [S]
    public: static Val Sub(const RatioImpl*, const RatioImpl*);

    // [T]
    public: float32 ToFloat32() const;
    public: float64 ToFloat64() const;

    public: static void Truncate(
        const RatioImpl*,
        const RatioImpl*,
        Val*,
        Val* );
}; // RatioImpl

class ALIGN_RECORD StackRatio : public RatioImpl
{
    public: StackRatio(Val num)
    {
        m_denominator = one;
        m_numerator   = num;
    } // StackRatio
}; // StackRatio

} // Internal
} // TinyCl

#endif //!defined(INCLUDE_tinycl_ratio_h)
