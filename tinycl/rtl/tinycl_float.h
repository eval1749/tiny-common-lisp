//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Float Implementation
// tinycl_float.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_float.h#2 $
//
#if !defined(INCLUDE_tinycl_float_h)
#define INCLUDE_tinycl_float_h

namespace TinyCl
{

namespace Internal
{

#define CLASSD_float32      CLASSD_single_float
#define ClassDIndex_float32 ClassDIndex_single_float
#define Layout_float32      Layout_single_float

#define CLASSD_float64      CLASSD_double_float
#define ClassDIndex_float64 ClassDIndex_double_float
#define Layout_float64      Layout_double_float

class BignumImpl;
class BignumInt;
class BignumInt64;

enum FpClass
{
    FpClass_Zero,
    FpClass_Normal,
    FpClass_Subnormal,
    FpClass_Infinity,
    FpClass_NaN,
    FpClass_SNaN,
}; // FpClass

#if FLOAT_BIGENDIAN
struct Float32Layout
{
    unsigned int    m_nSign         : 1;
    unsigned int    m_nExponent     : 8;
    unsigned int    m_nSignificand  : 23;
}; //  Float32Layout
#else
struct Float32Layout
{
    unsigned int    m_nSignificand  : 23;
    unsigned int    m_nExponent     : 8;
    unsigned int    m_nSign         : 1;
}; // Float32Layout
#endif // FLOAT_BIGENDIAN

#if FLOAT_BIGENDIAN
struct Float64Layout
{
    unsigned int    m_nSign         : 1;
    unsigned int    m_nExponent     : 11;
    unsigned int    m_nSignificandH : 20;
    unsigned int    m_nSignificandL : 32;
}; // Float64Layout
#else
struct Float64Layout
{
    unsigned int    m_nSignificandL : 32;
    unsigned int    m_nSignificandH : 20;
    unsigned int    m_nExponent     : 11;
    unsigned int    m_nSign         : 1;
}; // Float64Layout
#endif // FLOAT_BIGENDIAN

#define FLOAT_IMPL_(mp_ty, mp_name) \
    template< \
        class Self_, \
        class ObjectLayout_, \
        class Layout_, \
        class float_, \
        typename int_, \
        typename uint_, \
        typename Bignum_ > \
    mp_ty \
        FloatImpl_< \
            Self_, \
            ObjectLayout_, \
            Layout_, \
            float_, \
            int_, \
            uint_, \
            Bignum_  > :: \
        mp_name

template<
    class Self_,
    class ObjectLayout_,
    class Layout_,
    class float_,
    typename int_,
    typename uint_,
    typename Bignum_ >
class FloatImpl_ :
    public Record_<Self_, ObjectLayout_>
{
    public: typedef Layout_ Layout;

    // [A]
    public: static Val Add(float_ const a, float_ const b)
        { return Self_::Make(a + b); }

    // [C]
    public: static FpClass Classify(const Layout_*);

    // [D]
    public: static Val Div(float_ const a, float_ const b)
        { return Self_::Make(a / b); }

    // [F]
    public: static float_ FromBignum(const BignumImpl*, uint);

    // [G]
    // <summary>
    //   Returns floating-pointer marker for print.
    // </summary>
    public: static char16 GetMarker()
    {
        if (TLV(Aread_default_float_formatA) == Self_::Symbol_())
        {
            return 'e';
        }
        else
        {
            return Self_::Marker_();
        }
    } // GetMarker

    // [M]
    public: static Val Mul(float_ const a, float_ const b)
        { return Self_::Make(a * b); }

    // [R]
    public: static Val Rational(const Layout_*);
    public: static void Round(Val, Val, Val*, Val*);

    // [S]
    public: static Val Sub(float_ const a, float_ const b)
        { return Self_::Make(a - b); }

    public: static float_ Sub_1(float_ const a)
    {
        float_ b = a;
        Layout_* const p = reinterpret_cast<Layout_*>(&b);
        p->m_nSign ^= 1;
        return b;
    } // Sub_1

    // [T]
    protected: static Val toInt(const Layout_*);

    public: static void Truncate(
        float_  const fltA,
        float_  const fltB,
        Val*    const out_q,
        Val*    const out_r )
    {
        float_ const fltQ = Self_::Truncate_1(fltA / fltB);
        if (NULL != out_q) *out_q = Self_::Make(fltQ);
        if (NULL != out_r) *out_r = Self_::Make(fltA - fltB * fltQ);
    } // Truncate
}; // FloatImpl_

/// <summary>
///   Represents IEEE 32bit Floating Point Number.
/// </summary>
//  Exponent    Significand
//  -------------------------------------------------------
//  255         000 0000 ... 0000   Infinity
//  255         1uu uuuu ... uuuu   NaN        bit22 == 1
//  255         0uu uuuu ... uuuu   SNaN       bit22 != 1
//    0         000 0000 ... 0000   Zero
//    0         uuu uuuu ... uuuu   Subnormal (f x 2^-149)
//  others      uuu uuuu ... uuuu   Normal    (2^23+f)x2(be-150)
//
//  Note: 150 = ExponentBias(127) + SignificandBits(23)
//
class Float32Impl :
    public FloatImpl_<
        Float32Impl,
        Layout_float32,
        Float32Layout,
        float32,
        int32,
        uint32,
        BignumInt >
{
    public: enum
    {
        ExponentBias        = 127,
        ExponentMax         = 255,
        ExponentMin         = 0,
        ExponentSubnormal   = -149, // = -127 - 22

        NormalPrecision     = 24,
        SignificandBits     = 23,
        SubnormalPrecision  = 23,
    }; // enum

    public: static Val ClassD_()
        { return CLASSD_float32; }

    public: static char16 Marker_()
        { return 'f'; }

    public: static Val MostPositiveInBignum_()
        { return BIGNUM_most_positive_float32; }

    public: static Val Symbol_()
        { return Qsingle_float; }

    public: static const char16* TypeString_()
        { return L"Single-Float"; }

    // [G]
    public: float32 Get() const { return m_flt; }

    public: Layout* GetLayout() const
    {
        return const_cast<Layout*>(
                reinterpret_cast<const Layout*>(&m_flt) );
    } // GetLayout

    public: static uint32 GetSignificand(const Layout* const p)
        { return p->m_nSignificand; }

    // [M]
    public: static Val Make(float32);

    // [S]
    /// <summary>
    ///   Sets significand of this float32 number from the least 22 bit of
    ///   specified fixnum.
    ///   <p>We used this function for implementing FromBignum method.</p>
    /// </summary>
    public: static void SetSignificand(
        Layout* const p,
        Val     const a )
    {
        Int iA = Fixnum::Decode_(a);
        p->m_nSignificand = static_cast<uint>(iA);
    } // SetSignificand

    // [T]
    public: Val ToInt() const;
    public: static float32 Truncate_1(float32);
}; // Float32Impl

/// <summary>
///   Represents IEEE 64bit Floating Point Number.
/// </summary>
//  Exponent    Significand
//  -------------------------------------------------------
//  2047        0000 0000 ... 0000   Infinity
//  2047        1uu0 uuuu ... uuuu   NaN        bit51 == 1
//  2047        0uu0 uuuu ... uuuu   SNaN       bit51 != 1
//     0        0000 0000 ... 0000   Zero
//     0        uuuu uuuu ... uuuu   Subnormal (f x 2^-1074)
//  others      uuuu uuuu ... uuuu   Normal    (2^52+f) x 2(be-1075)
//
//  Note: 1075 = ExponentBias(1023) + SignificandBits(52)
//
class Float64Impl :
    public FloatImpl_<
        Float64Impl,
        Layout_float64,
        Float64Layout,
        float64,
        int64,
        uint64,
        BignumInt64 >
{
    public: enum
    {
        ExponentBias        = 1023,
        ExponentMax         = 2047,
        ExponentMin         = 0,
        ExponentSubnormal   = -1074, // = -1023 - 51

        NormalPrecision     = 53,
        SignificandBits     = 52,
        SubnormalPrecision  = 52,
    }; // enum

    public: static Val ClassD_()
        { return CLASSD_float64; }

    public: static char16 Marker_()
        { return 'd'; }

    public: static Val MostPositiveInBignum_()
        { return BIGNUM_most_positive_float64; }

    public: static Val Symbol_()
        { return Qdouble_float; }

    public: static const char16* TypeString_()
        { return L"Double-Float"; }

    // [G]
    public: float64 Get() const { return m_dbl; }

    public: Layout* GetLayout() const
    {
        return const_cast<Layout*>(
                reinterpret_cast<const Layout*>(&m_dbl) );
    } // GetLayout

    public: static uint64 GetSignificand(const Layout* const p)
    {
        uint64 u64 = p->m_nSignificandH;
        u64 <<= 32;
        u64 |= p->m_nSignificandL;
        return u64;
    } // GetSignificand

    // [M]
    public: static Val Make(float64);

    // [S]
    public: static void SetSignificand(Layout*, Val);

    // [T]
    public: Val ToInt() const;
    public: static float64 Truncate_1(float64);
}; // Float64Impl

/// <summary>
///   Represents float32 object on C-stack.
/// </summary>
class ALIGN_BINOBJ StackFloat32 : public Float32Impl
{
    public: StackFloat32(float32 flt)
    {
        m_classd = ClassD_();
        m_flt    = flt;
    } // StackFloat32
}; // StackFloat32

/// <summary>
///   Represents float64 object on C-stack.
/// </summary>
class ALIGN_BINOBJ StackFloat64 : public Float64Impl
{
    public: StackFloat64(float64 flt)
    {
        m_classd = ClassD_();
        m_dbl    = flt;
    } // StackFloat64
}; // StackFloat64

} // Internal

} // TinyCl

#endif //!defined(INCLUDE_tinycl_float_h)
