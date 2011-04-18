//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Complex Implementation
// tinycl_complex.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_complex.h#2 $
//
#if !defined(INCLUDE_tinycl_complex_h)
#define INCLUDE_tinycl_complex_h

namespace TinyCl
{

namespace Internal
{
#define CLASSD_float32_complex  CLASSD_single_float_complex
#define CLASSD_float64_complex  CLASSD_double_float_complex

#define ClassDIndex_float32_complex  ClassDIndex_single_float_complex
#define ClassDIndex_float64_complex ClassDIndex_double_float_complex

#define Layout_float32_complex  Layout_single_float_complex
#define Layout_float64_complex  Layout_double_float_complex

template<
    class Self_,
    class Layout_,
    class Component_ >
class Complex_ : public Record_<Self_, Layout_>
{
    // (ra + rb) + (ia + ib)i
    public: static Val Add(
        const Self_* const pA,
        const Self_* const pB )
    {
        Component_ ra = pA->GetReal();
        Component_ ia = pA->GetImag();

        Component_ rb = pB->GetReal();
        Component_ ib = pB->GetImag();

        return Self_::Make(Self_::RealAdd(ra, rb), Self_::RealAdd(ia, ib));
    } // Add

    // (ra x rb + ia a ib) + (rb x ra - ra x rb)i
    // -------------------------------------------
    //          rb x rb + ib x ib
    public: static Val Div(
        const Self_* const pA,
        const Self_* const pB )
    {
        Component_ ra = pA->GetReal();
        Component_ ia = pA->GetImag();

        Component_ rb = pB->GetReal();
        Component_ ib = pB->GetImag();

        Component_ den = Self_::RealAdd(
            Self_::RealMul(rb, rb),
            Self_::RealMul(ib, ib) );

        return Self_::Make(
            Self_::RealDiv(
                Self_::RealAdd(
                    Self_::RealMul(ra, rb),
                    Self_::RealMul(ia, ib) ),
                den ),
            Self_::RealDiv(
                Self_::RealAdd(
                    Self_::RealMul(ra, ra),
                    Self_::RealMul(ra, rb) ),
                den ) );
    } // Div

    // ra x rb - ia x ib + (ra x ib + rb x ia)i
    public: static Val Mul(
        const Self_* const pA,
        const Self_* const pB )
    {
        Component_ ra = pA->GetReal();
        Component_ ia = pA->GetImag();

        Component_ rb = pB->GetReal();
        Component_ ib = pB->GetImag();

        return Self_::Make(
            Self_::RealAdd(Self_::RealMul(ra, rb), Self_::RealMul(ia, ib)),
            Self_::RealAdd(Self_::RealMul(ra, ib), Self_::RealMul(rb, ia)) );
    } // Mul

    // (ra - rb) + (ia - ib)i
    public: static Val Sub(
        const Self_* const pA,
        const Self_* const pB )
    {
        Component_ ra = pA->GetReal();
        Component_ ia = pA->GetImag();

        Component_ rb = pB->GetReal();
        Component_ ib = pB->GetImag();

        return Self_::Make(Self_::RealSub(ra, rb), Self_::RealSub(ia, ib));
    } // Sub

}; // Complex_

class Float32ComplexImpl :
    public Complex_<Float32ComplexImpl, Layout_float32_complex, float32>
{
    public: static Val ClassD_() { return CLASSD_float32_complex; }

    public: static float32 RealAdd(float32 const a, float32 const b)
        { return a + b; }

    public: static float32 RealDiv(float32 const a, float32 const b)
        { return a / b; }

    public: float32 GetImag() const
        { return m_fltImag; }

    public: float32 GetReal() const
        { return m_fltReal; }

    public: static Val Make(float32 const a, float32 const b)
    {
        Val const c = Thread::Get()->AllocBinObj(ClassD_());
        Float32ComplexImpl* const p = c->StaticCast<Float32ComplexImpl>();
        p->m_fltReal = a;
        p->m_fltImag = b;
        return c;
    } // Make

    public: Val Neg() const;

    public: static float32 RealMul(float32 const a, float32 const b)
        { return a * b; }

    public: static float32 RealSub(float32 const a, float32 const b)
        { return a - b; }
}; // Float32ComplexImpl

class Float64ComplexImpl :
    public Complex_<Float64ComplexImpl, Layout_float64_complex, float64>
{
    public: static Val ClassD_() { return CLASSD_float64_complex; }

    public: static float64 RealAdd(float64 const a, float64 const b)
        { return a + b; }

    public: static float64 RealDiv(float64 const a, float64 const b)
        { return a / b; }

    public: float64 GetImag() const
        { return m_dblImag; }

    public: float64 GetReal() const
        { return m_dblReal; }

    public: static Val Make(float64 const a, float64 const b)
    {
        Val const c = Thread::Get()->AllocBinObj(ClassD_());
        Float64ComplexImpl* const p = c->StaticCast<Float64ComplexImpl>();
        p->m_dblReal = a;
        p->m_dblImag = b;
        return c;
    } // Make

    public: Val Neg() const;

    public: static float64 RealMul(float64 const a, float64 const b)
        { return a * b; }

    public: static float64 RealSub(float64 const a, float64 const b)
        { return a - b; }
}; // Float64ComplexImpl

class RationalComplexImpl :
    public Complex_<RationalComplexImpl, Layout_rational_complex, Val>
{
    public: static Val ClassD_() { return CLASSD_rational_complex; }

    public: static Val RealAdd(Val const a, Val const b)
        { return add(a, b); }

    public: static Val RealDiv(Val const a, Val const b)
        { return div(a, b); }

    public: Val GetImag() const
        { return m_imag; }

    public: Val GetReal() const
        { return m_real; }

    public: static Val Make(Val const a, Val const b)
    {
        Val const c = Thread::Get()->AllocRecord(ClassD_());
        RationalComplexImpl* const p = c->StaticCast<RationalComplexImpl>();
        p->m_real = a;
        p->m_imag = b;
        return c;
    } // Make

    public: Val Neg() const;

    public: static Val RealMul(Val const a, Val const b)
        { return mul(a, b); }

    public: static Val RealSub(Val const a, Val const b)
        { return sub(a, b); }
}; // RationalComplexImpl

class ALIGN_BINOBJ StackFloat32Complex : public Float32ComplexImpl
{
    public: StackFloat32Complex(float32 a, float32 b = 0)
    {
        m_classd = ClassD_();
        m_fltReal = a;
        m_fltImag = b;
    } // StackFloat32Complex
}; // StackFloat32Complex

class ALIGN_BINOBJ StackFloat64Complex : public Float64ComplexImpl
{
    public: StackFloat64Complex(float64 a, float64 b = 0)
    {
        m_classd = ClassD_();
        m_dblReal = a;
        m_dblImag = b;
    } // StackFloat64Complex
}; // StackFloat64Complex

class ALIGN_BINOBJ StackRationalComplex : public RationalComplexImpl
{
    public: StackRationalComplex(Val a, Val b = zero)
    {
        m_classd = ClassD_();
        m_real = a;
        m_imag = b;
    } // StackRationalComplex
}; // StackRationalComplex

} // Internal
} // TinyCl

#endif //!defined(INCLUDE_tinycl_complex_h)
