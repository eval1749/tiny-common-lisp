#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Number
// tinycl_rtl_number.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_number.cpp#4 $
//
#include "../tinycl.h"

#include "./tinycl_bignum.h"
#include "./tinycl_complex.h"
#include "./tinycl_float.h"
#include "./tinycl_ratio.h"

namespace TinyCl
{

using namespace Internal;

inline Int offsetOfClassD(Val classd)
{
    return reinterpret_cast<Int>(classd) - CLASSD_BASE - Arch::Tag_Record;
} // offsetOfClassD

#define CLASSD_OFFSET(mp_cname) \
    sizeof(Layout_classd) * ClassDIndex_ ## mp_cname

// <summary>
//   Type dispatcher for arithmetic operation.
// </summary>
template<typename Result_, class Operation_>
class Dispatcher_
{
    public: static Result_ DispatchA(Val const a, Val const b)
    {
        switch (offsetOfClassD(classd_of(a)))
        {
        case CLASSD_OFFSET(fixnum):
            return DispatchB(Fixnum::Decode_(a), b);

        case CLASSD_OFFSET(bignum):
            return DispatchB(a->StaticCast<BignumImpl>(), b);

        case CLASSD_OFFSET(float32):
            return DispatchB(a->StaticCast<Float32Impl>()->Get(), b);

        case CLASSD_OFFSET(float32_complex):
            return DispatchB(a->StaticCast<Float32ComplexImpl>(), b);

        case CLASSD_OFFSET(float64):
            return DispatchB(a->StaticCast<Float64Impl>()->Get(), b);

        case CLASSD_OFFSET(float64_complex):
            return DispatchB(a->StaticCast<Float64ComplexImpl>(), b);

        case CLASSD_OFFSET(ratio):
            return DispatchB(a->StaticCast<RatioImpl>(), b);

        case CLASSD_OFFSET(rational_complex):
            return DispatchB(a->StaticCast<RationalComplexImpl>(), b);

        default:
            SignalTypeError(a, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchA

    // Bignum
    public: static Result_ DispatchB(
        const BignumImpl*   const pA,
        Val                 const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            BignumInt oB(Fixnum::Decode_(b));
            return Operation_::Compute(pA, &oB);
        } // fixnum

        case CLASSD_OFFSET(bignum):
            return Operation_::Compute(pA, b->StaticCast<BignumImpl>());

        case CLASSD_OFFSET(float32):
            return Operation_::Compute(
                pA->ToFloat32(),
                b->StaticCast<Float32Impl>()->Get() );

        case CLASSD_OFFSET(float32_complex):
        {
            StackFloat32Complex oA(pA->ToFloat32());

            const Float32ComplexImpl* const pB =
                b->StaticCast<Float32ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float32_complex

        case CLASSD_OFFSET(float64):
            return Operation_::Compute(
                pA->ToFloat64(),
                b->StaticCast<Float64Impl>()->Get() );

        case CLASSD_OFFSET(float64_complex):
        {
            StackFloat64Complex oA(pA->ToFloat64());

            const Float64ComplexImpl* const pB =
                b->StaticCast<Float64ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float64_complex

        case CLASSD_OFFSET(ratio):
        {
            StackRatio oA(pA->Encode());
            return Operation_::Compute(&oA, b->StaticCast<RatioImpl>());
        } // ratio

        case CLASSD_OFFSET(rational_complex):
        {
            StackRationalComplex oA(pA->Encode());

            const RationalComplexImpl* const pB =
                b->StaticCast<RationalComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // rational_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // Bignum

    // Fixnum
    public: static Result_ DispatchB(
        Int const iA,
        Val const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
            return Operation_::Compute(iA, Fixnum::Decode_(b));

        case CLASSD_OFFSET(bignum):
        {
            BignumInt oA(iA);
            return Operation_::Compute(&oA, b->StaticCast<BignumImpl>());
        } // bignum

        case CLASSD_OFFSET(float32):
            return Operation_::Compute(
                static_cast<float32>(iA),
                b->StaticCast<Float32Impl>()->Get() );

        case CLASSD_OFFSET(float32_complex):
        {
            StackFloat32Complex oA(static_cast<float32>(iA));

            const Float32ComplexImpl* const pB =
                b->StaticCast<Float32ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float32_complex

        case CLASSD_OFFSET(float64):
            return Operation_::Compute(
                static_cast<float64>(iA),
                b->StaticCast<Float64Impl>()->Get() );

        case CLASSD_OFFSET(float64_complex):
        {
            StackFloat64Complex oA(static_cast<float64>(iA));

            const Float64ComplexImpl* const pB =
                b->StaticCast<Float64ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float32_complex

        case CLASSD_OFFSET(ratio):
        {
            StackRatio oA(Fixnum::Encode(iA));
            return Operation_::Compute(&oA, b->StaticCast<RatioImpl>());
        } // ratio

        case CLASSD_OFFSET(rational_complex):
        {
            StackRationalComplex oA(Fixnum::Encode(iA));

            const RationalComplexImpl* const pB =
                b->StaticCast<RationalComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float32_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB Fixnum

    // Float32
    public: static Result_ DispatchB(
        float32 const fltA,
        Val     const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
            return Operation_::Compute(
                fltA,
                static_cast<float32>(Fixnum::Decode_(b)) );

        case CLASSD_OFFSET(bignum):
            return Operation_::Compute(
                fltA,
                b->StaticCast<BignumImpl>()->ToFloat32() );

        case CLASSD_OFFSET(float32):
            return Operation_::Compute(
                fltA,
                b->StaticCast<Float32Impl>()->Get() );

        case CLASSD_OFFSET(float32_complex):
        {
            StackFloat32Complex oA(fltA);

            const Float32ComplexImpl* const pB =
                b->StaticCast<Float32ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float32_complex

        case CLASSD_OFFSET(float64):
            return Operation_::Compute(
                static_cast<float64>(fltA),
                b->StaticCast<Float64Impl>()->Get() );

        case CLASSD_OFFSET(float64_complex):
        {
            StackFloat64Complex oA(static_cast<float64>(fltA));

            const Float64ComplexImpl* const pB =
                b->StaticCast<Float64ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float64_complex

        case CLASSD_OFFSET(ratio):
            return Operation_::Compute(
                fltA,
                b->StaticCast<RatioImpl>()->ToFloat32() );

        case CLASSD_OFFSET(rational_complex):
        {
            const RationalComplexImpl*  const pB =
                b->StaticCast<RationalComplexImpl>();

            StackFloat32Complex oA(fltA);

            StackFloat32Complex oB(
                BignumImpl::ToFloat32(pB->m_real),
                BignumImpl::ToFloat32(pB->m_imag) );

            return Operation_::Compute(&oA, &oB);
        } // float32_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB Float32

    // Float32ComplexImpl
    public: static Result_ DispatchB(
        const Float32ComplexImpl*   const pA,
        Val                         const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            StackFloat32Complex oB(static_cast<float32>(Fixnum::Decode_(b)));
            return Operation_::Compute(pA, &oB);
        } // fixnum

        case CLASSD_OFFSET(bignum):
        {
            StackFloat32Complex oB(BignumImpl::ToFloat32(b));
            return Operation_::Compute(pA, &oB);
        } // bignum

        case CLASSD_OFFSET(float32):
        {
            StackFloat32Complex oB(b->StaticCast<Float32Impl>()->Get());
            return Operation_::Compute(pA, &oB);
        } // float32

        case CLASSD_OFFSET(float32_complex):
            return Operation_::Compute(
                pA,
                b->StaticCast<Float32ComplexImpl>() );

        case CLASSD_OFFSET(float64):
        {
            StackFloat64Complex oA(
                static_cast<float64>(pA->m_fltReal),
                static_cast<float64>(pA->m_fltImag) );

            StackFloat64Complex oB(b->StaticCast<Float64Impl>()->Get());

            return Operation_::Compute(&oA, &oB);
        } // float64

        case CLASSD_OFFSET(float64_complex):
        {
            StackFloat64Complex oA(
                static_cast<float64>(pA->m_fltReal),
                static_cast<float64>(pA->m_fltImag) );

            StackFloat64Complex oB(
                static_cast<float64>(pA->m_fltReal),
                static_cast<float64>(pA->m_fltImag) );

            return Operation_::Compute(&oA, &oB);
        } // float64_complex

        case CLASSD_OFFSET(ratio):
        {
            StackFloat32Complex oB(b->StaticCast<RatioImpl>()->ToFloat32());
            return Operation_::Compute(pA, &oB);
        } // ratio

        case CLASSD_OFFSET(rational_complex):
        {
            const RationalComplexImpl*  const pB =
                b->StaticCast<RationalComplexImpl>();

            StackFloat32Complex oB(
                BignumImpl::ToFloat32(pB->m_real),
                BignumImpl::ToFloat32(pB->m_imag) );

            return Operation_::Compute(pA, &oB);
        } // float32_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB Float32ComplexImpl

    // Float64
    public: static Result_ DispatchB(
        float64 const fltA,
        Val     const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
            return Operation_::Compute(
                fltA,
                static_cast<float64>(Fixnum::Decode_(b)) );

        case CLASSD_OFFSET(bignum):
            return Operation_::Compute(
                fltA,
                b->StaticCast<BignumImpl>()->ToFloat64() );

        case CLASSD_OFFSET(float32):
            return Operation_::Compute(
                fltA,
                static_cast<float64>(
                    b->StaticCast<Float32Impl>()->Get() ) );

        case CLASSD_OFFSET(float32_complex):
        {
            const Float32ComplexImpl* const pB =
                b->StaticCast<Float32ComplexImpl>();

            StackFloat64Complex oA(fltA);

            StackFloat64Complex oB(
                pB->m_fltReal,
                pB->m_fltImag );

            return Operation_::Compute(&oA, &oB);
        } // float32_complex

        case CLASSD_OFFSET(float64):
            return Operation_::Compute(
                fltA,
                b->StaticCast<Float64Impl>()->Get() );

        case CLASSD_OFFSET(ratio):
            return Operation_::Compute(
                fltA,
                b->StaticCast<RatioImpl>()->ToFloat64() );

        case CLASSD_OFFSET(rational_complex):
        {
            const RationalComplexImpl* const pB =
                b->StaticCast<RationalComplexImpl>();

            StackFloat64Complex oA(fltA);

            StackFloat64Complex oB(
                BignumImpl::ToFloat64(pB->m_real),
                BignumImpl::ToFloat64(pB->m_imag) );

            return Operation_::Compute(&oA, &oB);
        } // rational_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB Float64

    // Float64ComplexImpl
    public: static Result_ DispatchB(
        const Float64ComplexImpl*   const pA,
        Val                         const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            StackFloat64Complex oB(static_cast<float64>(Fixnum::Decode_(b)));
            return Operation_::Compute(pA, &oB);
        } // fixnum

        case CLASSD_OFFSET(bignum):
        {
            StackFloat64Complex oB(BignumImpl::ToFloat32(b));
            return Operation_::Compute(pA, &oB);
        } // bignum

        case CLASSD_OFFSET(float32):
        {
            StackFloat64Complex oB(b->StaticCast<Float32Impl>()->Get());
            return Operation_::Compute(pA, &oB);
        } // float32

        case CLASSD_OFFSET(float32_complex):
        {
            const Float32ComplexImpl* pB =
                b->StaticCast<Float32ComplexImpl>();

            StackFloat64Complex oB(
                static_cast<float64>(pB->m_fltReal),
                static_cast<float64>(pB->m_fltImag) );

            return Operation_::Compute(pA, &oB);
        } // float32_complex

        case CLASSD_OFFSET(float64):
        {
            StackFloat64Complex oB(b->StaticCast<Float64Impl>()->Get());
            return Operation_::Compute(pA, &oB);
        } // float64

        case CLASSD_OFFSET(float64_complex):
        {
            const Float64ComplexImpl* pB =
                b->StaticCast<Float64ComplexImpl>();

            return Operation_::Compute(pA, pB);
        } // float64_complex

        case CLASSD_OFFSET(ratio):
        {
            const RationalComplexImpl* pB =
                b->StaticCast<RationalComplexImpl>();

            StackFloat64Complex oB(
                BignumImpl::ToFloat64(pB->m_real),
                BignumImpl::ToFloat64(pB->m_imag) );

            return Operation_::Compute(pA, &oB);
        } // rational_complex

        case CLASSD_OFFSET(rational_complex):
        {
            const RationalComplexImpl*  const pB =
                b->StaticCast<RationalComplexImpl>();

            StackFloat64Complex oB(
                BignumImpl::ToFloat32(pB->m_real),
                BignumImpl::ToFloat32(pB->m_imag) );

            return Operation_::Compute(pA, &oB);
        } // float32_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB Float64ComplexImpl

    // Ratio
    public: static Result_ DispatchB(
        const RatioImpl*    const pA,
        Val                 const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            StackRatio oB(b);
            return Operation_::Compute(pA, &oB);
        } // fixnum

        case CLASSD_OFFSET(bignum):
        {
            StackRatio oB(b);
            return Operation_::Compute(pA, &oB);
        } // fixnum

        case CLASSD_OFFSET(float32):
            return Operation_::Compute(
                pA->ToFloat32(),
                b->StaticCast<Float32Impl>()->Get() );

        case CLASSD_OFFSET(float32_complex):
        {
            StackFloat32Complex oA(pA->ToFloat32());

            const Float32ComplexImpl* pB =
                b->StaticCast<Float32ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float32_complex

        case CLASSD_OFFSET(float64):
            return Operation_::Compute(
                pA->ToFloat64(),
                b->StaticCast<Float64Impl>()->Get() );

        case CLASSD_OFFSET(float64_complex):
        {
            StackFloat64Complex oA(pA->ToFloat64());

            const Float64ComplexImpl* pB =
                b->StaticCast<Float64ComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // float64_complex

        case CLASSD_OFFSET(ratio):
            return Operation_::Compute(
                pA,
                b->StaticCast<RatioImpl>() );

        case CLASSD_OFFSET(rational_complex):
        {
            StackRationalComplex oA(pA->Encode());

            const RationalComplexImpl* pB =
                b->StaticCast<RationalComplexImpl>();

            return Operation_::Compute(&oA, pB);
        } // rational_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB Ratio

    // RationalComplexImpl
    public: static Result_ DispatchB(
        const RationalComplexImpl*  const pA,
        Val                         const b )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            StackRationalComplex oB(b);
            return Operation_::Compute(pA, &oB);
        } // fixnum

        case CLASSD_OFFSET(bignum):
        {
            StackRationalComplex oB(b);
            return Operation_::Compute(pA, &oB);
        } // bignum

        case CLASSD_OFFSET(float32):
        {
            StackFloat32Complex oA(
                BignumImpl::ToFloat32(pA->m_real),
                BignumImpl::ToFloat32(pA->m_imag) );

            StackFloat32Complex oB(b->StaticCast<Float32Impl>()->Get());

            return Operation_::Compute(&oA, &oB);
        } // float32

        case CLASSD_OFFSET(float32_complex):
        {
            StackFloat32Complex oA(
                BignumImpl::ToFloat32(pA->m_real),
                BignumImpl::ToFloat32(pA->m_imag) );

            return Operation_::Compute(
                &oA,
                b->StaticCast<Float32ComplexImpl>() );
        } // float32_complex

        case CLASSD_OFFSET(float64):
        {
            StackFloat64Complex oA(
                BignumImpl::ToFloat64(pA->m_real),
                BignumImpl::ToFloat64(pA->m_imag) );

            StackFloat64Complex oB(b->StaticCast<Float64Impl>()->Get());

            return Operation_::Compute(&oA, &oB);
        } // float64

        case CLASSD_OFFSET(float64_complex):
        {
            StackFloat64Complex oA(
                BignumImpl::ToFloat64(pA->m_real),
                BignumImpl::ToFloat64(pA->m_imag) );

            return Operation_::Compute(
                &oA,
                b->StaticCast<Float64ComplexImpl>() );
        } // float64_complex

        case CLASSD_OFFSET(ratio):
        {
            StackRationalComplex oB(b);
            return Operation_::Compute(pA, &oB);
        } // rational_complex

        case CLASSD_OFFSET(rational_complex):
        {
            const RationalComplexImpl* const pB =
                b->StaticCast<RationalComplexImpl>();

            return Operation_::Compute(pA, pB);
        } // rational_complex

        default:
            SignalTypeError(b, Operation_::TypeSym_());
            // NOTREACHED
        }
    } // DispatchB RationalComplexImpl
}; // Dispatcher_

template<class Operation_>
class ArithOp_ : public Dispatcher_<Val, Operation_> {};

// <summary>
//   Type dispatcher for arithmetic operation.
// </summary>
template<class Operation_>
class DivOp
{
    public: static Val TypeSym_() { return Qreal; }

    public: static void DispatchA(Val a, Val b, Val* out_q, Val* out_r)
    {
        switch (offsetOfClassD(classd_of(a)))
        {
        case CLASSD_OFFSET(fixnum):
            DispatchB(Fixnum::Decode_(a), a, b, out_q, out_r);
            break;

        case CLASSD_OFFSET(bignum):
            DispatchB(a->StaticCast<BignumImpl>(), a, b, out_q, out_r);
            break;

        case CLASSD_OFFSET(float32):
            DispatchB(a->StaticCast<Float32Impl>()->Get(), a, b, out_q, out_r);
            break;

        case CLASSD_OFFSET(float64):
            DispatchB(a->StaticCast<Float64Impl>()->Get(), a, b, out_q, out_r);
            break;

        case CLASSD_OFFSET(ratio):
            DispatchB(a->StaticCast<RatioImpl>(), a, b, out_q, out_r);
            break;

        default:
            SignalTypeError(a, Qreal);
            // NOTREACHED
        } // switch classd
    } // DispatchA

    // Bignum
    public: static void DispatchB(
        const BignumImpl*   const pA,
        Val                 const,
        Val                 const b,
        Val*                const out_q,
        Val*                const out_r )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            BignumInt oB(Fixnum::Decode_(b));
            Operation_::Compute(pA, &oB, out_q, out_r);
            break;
        } // fixnum

        case CLASSD_OFFSET(bignum):
            Operation_::Compute(
                pA,
                b->StaticCast<BignumImpl>(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float32):
            Operation_::Compute(
                pA->ToFloat32(),
                b->StaticCast<Float32Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float64):
            Operation_::Compute(
                pA->ToFloat64(),
                b->StaticCast<Float64Impl>()->Get(),
                out_q,
                out_r );

        case CLASSD_OFFSET(ratio):
        {
            StackRatio oA(pA->Encode());
            Operation_::Compute(
                &oA,
                b->StaticCast<RatioImpl>(),
                out_q,
                out_r );
            break;
        } // ratio

        default:
            SignalTypeError(b, Qreal);
            // NOTREACHED
        } // switch classd
    } // DispatchB

    // Fixnum
    public: static void DispatchB(
        Int  const iA,
        Val  const a,
        Val  const b,
        Val* const out_q,
        Val* const out_r )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
            Operation_::Compute(
                iA,
                Fixnum::Decode_(b),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(bignum):
        {
            BignumInt oA(iA);
            Operation_::Compute(
                &oA,
                b->StaticCast<BignumImpl>(),
                out_q,
                out_r );
            break;
        } // bignum

        case CLASSD_OFFSET(float32):
            Operation_::Compute(
                static_cast<float32>(iA),
                b->StaticCast<Float32Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float64):
            Operation_::Compute(
                static_cast<float64>(iA),
                b->StaticCast<Float64Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(ratio):
        {
            StackRatio oA(a);
            Operation_::Compute(
                &oA,
                b->StaticCast<RatioImpl>(),
                out_q,
                out_r );
            break;
        } // ratio

        default:
            SignalTypeError(b, Qreal);
            // NOTREACHED
        } // switch classd
    } // DispatchB

    // Float32
    public: static void DispatchB(
        float32 const fltA,
        Val     const, 
        Val     const b,
        Val*    const out_q,
        Val*    const out_r )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
            Operation_::Compute(
                fltA,
                static_cast<float32>(Fixnum::Decode_(b)),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(bignum):
            Operation_::Compute(
                fltA,
                b->StaticCast<BignumImpl>()->ToFloat32(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float32):
            Operation_::Compute(
                fltA,
                b->StaticCast<Float32Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float64):
            Operation_::Compute(
                static_cast<float64>(fltA),
                b->StaticCast<Float64Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(ratio):
            Operation_::Compute(
                fltA,
                b->StaticCast<RatioImpl>()->ToFloat32(),
                out_q,
                out_r );
            break;

        default:
            SignalTypeError(b, Qreal);
            // NOTREACHED
        } // switch classd
    } // DispatchB

    // Float64
    public: static void DispatchB(
        float64 const fltA,
        Val     const,
        Val     const b,
        Val*    const out_q,
        Val*    const out_r )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
            Operation_::Compute(
                fltA,
                static_cast<float64>(Fixnum::Decode_(b)),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(bignum):
            Operation_::Compute(
                fltA,
                b->StaticCast<BignumImpl>()->ToFloat64(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float32):
            Operation_::Compute(
                fltA,
                static_cast<float64>(b->StaticCast<Float32Impl>()->Get()),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float64):
            Operation_::Compute(
                fltA,
                b->StaticCast<Float64Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(ratio):
            Operation_::Compute(
                fltA,
                b->StaticCast<RatioImpl>()->ToFloat64(),
                out_q,
                out_r );
            break;

        default:
            SignalTypeError(b, Qreal);
            // NOTREACHED
        } // switch classd
    } // DispatchB

    // Ratio
    public: static void DispatchB(
        const RatioImpl*    const pA,
        Val                 const,
        Val                 const b,
        Val*                const out_q,
        Val*                const out_r )
    {
        switch (offsetOfClassD(classd_of(b)))
        {
        case CLASSD_OFFSET(fixnum):
        {
            StackRatio oB(b);
            Operation_::Compute(pA, &oB, out_q, out_r);
            break;
        } // fixnum

        case CLASSD_OFFSET(bignum):
        {
            StackRatio oB(b);
            Operation_::Compute(pA, &oB, out_q, out_r);
            break;
        } // fixnum

        case CLASSD_OFFSET(float32):
            Operation_::Compute(
                pA->ToFloat32(),
                b->StaticCast<Float32Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(float64):
            Operation_::Compute(
                pA->ToFloat64(),
                b->StaticCast<Float64Impl>()->Get(),
                out_q,
                out_r );
            break;

        case CLASSD_OFFSET(ratio):
            Operation_::Compute(
                pA,
                b->StaticCast<RatioImpl>(),
                out_q,
                out_r );
            break;

        default:
            SignalTypeError(b, Qreal);
            // NOTREACHED
        } // switch classd
    } // DispatchB Ratio
}; // DivOp

class FixnumImpl
{
    #if 4 == SIZEOF_VAL
        public: static Val Add(Int const iA, Int const iB)
            {  return MakeInt64(static_cast<int64>(iA) + iB);  }
    #else
        public: static Val Add(Int const iA, Int const iB)
        {
            BignumInt oA(iA);
            BignumInt oB(iB);
            return BignumImpl::Add(&oA, &oB);
        } // Add
    #endif

    public: static Val Div(Int const iA, Int const iB)
    {
        if (0 == iB)
        {
            error(Qdivision_by_zero,
                Koperation, QS,
                Koperands, list(Fixnum::Encode(iA), Fixnum::Encode(iB)) );
        }

        Int iQ = iA / iB;
        Int iR = iA % iB;

        if (0 == iR)
        {
            return MakeInt(iQ);
        }

        return RatioImpl::Make(MakeInt(iQ * iB + iR), Fixnum::Encode(iB));
    } // Div

    #if 4 == SIZEOF_VAL
        public: static Val Mul(Int const iA, Int const iB)
            { return MakeInt64(static_cast<int64>(iA) * iB); }
    #else
        public: static Val Mul(Int const iA, Int const iB)
        {
            BignumInt oA(iA);
            BignumInt oB(iB);
            return BignumImpl::Mul(&oA, &oB);
        } // Mul
    #endif

    #if 4 == SIZEOF_VAL
        public: static Val Sub(Int const iA, Int const iB)
            { return MakeInt64(static_cast<int64>(iA) - iB); }
    #else
        public: static Val Sub(Int const iA, Int const iB)
        {
            BignumInt oA(iA);
            BignumInt oB(iB);
            return BignumImpl::Sub(&oA, &oB);
        } // Sub
    #endif

}; // FixnumImpl

#define defun_arith(mp_name, mp_Name) \
    class mp_Name \
    { \
        public: static Val TypeSym_() { return Qnumber; } \
\
        public: static Val Compute( \
                Int const iA, \
                Int const iB ) \
            { return FixnumImpl::mp_Name(iA, iB); } \
\
        public: static Val Compute( \
                const BignumImpl* const pA, \
                const BignumImpl* const pB ) \
            { return BignumImpl::mp_Name(pA, pB); } \
\
        public: static Val Compute( \
                float32 const fltA, \
                float32 const fltB ) \
            { return Float32Impl::mp_Name(fltA, fltB); } \
\
        public: static Val Compute( \
                const Float32ComplexImpl* const pA, \
                const Float32ComplexImpl* const pB ) \
            { return Float32ComplexImpl::mp_Name(pA, pB); } \
\
        public: static Val Compute( \
                float64 const fltA, \
                float64 const fltB ) \
            { return Float64Impl::mp_Name(fltA, fltB); } \
\
        public: static Val Compute( \
                const Float64ComplexImpl* const pA, \
                const Float64ComplexImpl* const pB ) \
            { return Float64ComplexImpl::mp_Name(pA, pB); } \
\
        public: static Val Compute( \
                const RatioImpl* const pA, \
                const RatioImpl* const pB ) \
            { return RatioImpl::mp_Name(pA, pB); } \
\
        public: static Val Compute( \
                const RationalComplexImpl* const pA, \
                const RationalComplexImpl* const pB ) \
            { return RationalComplexImpl::mp_Name(pA, pB); } \
    }; \
    defun(mp_name, (Val const a, Val const b)) \
        { return ArithOp_<mp_Name>::DispatchA(a, b); }

/// <summary>
///  Represents comparison operators for each numberic types.
///  <p>Note: There is no comparison defined for complex numbers.</p>
/// </summary>
class Compare
{
    public: static Val TypeSym_() { return Qreal; }

    // Note: We use notReal for MSVC linker to avoid getting
    // "unreachable code" warning.
    private: static int notReal(Val const a)
    {
        funcall(Qerror, Qtype_error, Kdatum, a, Kexpected_type, Qreal);
        return 0;
    } // notReal

    public: static int Compute(
        Int const iA,
        Int const iB )
    {
        if (iA == iB) return 0;
        return iA > iB ? 1 : -1;
    } // Compute

    public: static int Compute(
            const BignumImpl* const pA,
            const BignumImpl* const pB )
        { return BignumImpl::Cmp(pA, pB); }

    public: static int Compute(
        float32 const fltA,
        float32 const fltB )
    {
        if (fltA == fltB) return 0;
        return fltA > fltB ? 1 : -1;
    } // Compute

    public: static int Compute(
        const Float32ComplexImpl* const pA,
        const Float32ComplexImpl* const )
            { return notReal(pA->Encode()); }

    public: static int Compute(
        float64 const fltA,
        float64 const fltB )
    {
        if (fltA == fltB) return 0;
        return fltA > fltB ? 1 : -1;
    } // Compute

    public: static int Compute(
        const Float64ComplexImpl* const pA,
        const Float64ComplexImpl* const )
            { return notReal(pA->Encode()); }

    public: static int Compute(
        const RatioImpl* const pA,
        const RatioImpl* const pB )
            { return RatioImpl::Cmp(pA, pB); }

    public: static int Compute(
        const RationalComplexImpl* const pA,
        const RationalComplexImpl* const )
            { return notReal(pA->Encode()); }
}; // Compare

/// <summary>
///   Represents equality operator for each numberic types.
/// </summary>
class Eq
{
    public: static Val TypeSym_() { return Qnumber; }

    public: static bool Compute(
        Int const iA,
        Int const iB )
    {
        return iA == iB;
    } // Compute

    public: static bool Compute(
            const BignumImpl* const pA,
            const BignumImpl* const pB )
        { return 0 == BignumImpl::Cmp(pA, pB); }

    public: static bool Compute(
            float32 const fltA,
            float32 const fltB )
        { return fltA == fltB; }

    public: static bool Compute(
        const Float32ComplexImpl* const pA,
        const Float32ComplexImpl* const pB )
    {
        return
            pA->m_fltReal == pB->m_fltReal &&
            pA->m_fltImag == pB->m_fltImag;
    } // Compute

    public: static bool Compute(
            float64 const fltA,
            float64 const fltB )
        { return fltA == fltB; }

    public: static bool Compute(
        const Float64ComplexImpl* const pA,
        const Float64ComplexImpl* const pB )
    {
        return
            pA->m_dblReal == pB->m_dblReal &&
            pA->m_dblImag == pB->m_dblImag;
    } // Compute

    public: static bool Compute(
            const RatioImpl* const pA,
            const RatioImpl* const pB )
        { return 0 == RatioImpl::Cmp(pA, pB); }

    public: static bool Compute(
        const RationalComplexImpl* const pA,
        const RationalComplexImpl* const pB )
    {
        return
            eq(pA->m_real, pB->m_real) &&
            eq(pA->m_imag, pB->m_imag);
    } // Compute
}; // Eq

/// <summary>
///   Represents truncate operation for each real types.
/// </summary>
class Truncate
{
    public: static void Compute(
        Int     const iA,
        Int     const iB,
        Val*    const out_q,
        Val*    const out_r )
    {
        if (0 == iB)
        {
            error(Qdivision_by_zero,
                Koperation, Qtruncate,
                Koperands, list(Fixnum::Encode(iA), Fixnum::Encode(iB)) );
        }

        if (NULL != out_q) *out_q = Fixnum::Encode(iA / iB);
        if (NULL != out_r) *out_r = Fixnum::Encode(iA % iB);
    } // Compute

    public: static void Compute(
            const BignumImpl* const pA,
            const BignumImpl* const pB,
            Val*              const out_q,
            Val*              const out_r )
        { BignumImpl::Truncate(pA, pB, out_q, out_r); }

    public: static void Compute(
            float32 const fltA,
            float32 const fltB,
            Val*    const out_q,
            Val*    const out_r )
        { Float32Impl::Truncate(fltA, fltB, out_q, out_r); }

    public: static void Compute(
            float64 const fltA,
            float64 const fltB,
            Val*    const out_q,
            Val*    const out_r )
        { Float64Impl::Truncate(fltA, fltB, out_q, out_r); }

    public: static void Compute(
            const RatioImpl* const pA,
            const RatioImpl* const pB,
            Val*             const out_q,
            Val*             const out_r )
        { RatioImpl::Truncate(pA, pB, out_q, out_r); }
}; // Truncate

// [M]
Val MakeInt(Int iVal)
{
    if (iVal >= Fixnum::MostNegative && iVal <= Fixnum::MostPositive)
    {
        return Fixnum::Encode(iVal);
    }

    Val big = Thread::Get()->AllocBinVec(CLASSD_bignum, one);

    big->StaticCast<Bignum>()->GetStart()[0] = static_cast<Bigit>(iVal);

    return big;
} // MakeInt

Val MakeUInt(UInt uVal)
{
    if (uVal <= Fixnum::MostPositive)
    {
        return Fixnum::Encode(uVal);
    }

    Val big = Thread::Get()->AllocBinVec(
        CLASSD_bignum,
        static_cast<Int>(uVal) > 0 ? one : two );

    big->StaticCast<Bignum>()->GetStart()[0] = static_cast<Bigit>(uVal);

    return big;
} // MakeUInt

#if 4 == SIZEOF_VAL
Val MakeInt64(int64 iVal)
{
    if (iVal >= Fixnum::MostNegative && iVal <= Fixnum::MostPositive)
    {
        return Fixnum::Encode(static_cast<Int>(iVal));
    }

    BignumInt64 oA(iVal);
    return oA.Pin();
} // MakeInt64

Val MakeUInt64(UInt64 uVal)
{
    if (uVal <= Fixnum::MostPositive)
    {
        return Fixnum::Encode(static_cast<Int>(uVal));
    }

    if (uVal < (1ull << (Arch::BigitBits - 1)))
    {
        Val big = Thread::Get()->AllocBinVec(CLASSD_bignum, one);
        Bignum* p = big->StaticCast<Bignum>();
        p->GetStart()[0] = static_cast<Bigit>(uVal);
        return big;
    } // if

    if (static_cast<Int64>(uVal) > 0)
    {
        Val big = Thread::Get()->AllocBinVec(CLASSD_bignum, two);
        Bignum* p = big->StaticCast<Bignum>();
        p->GetStart()[0] = static_cast<Bigit>(uVal);
        p->GetStart()[1] = static_cast<Bigit>(uVal >> Arch::BigitBits);
        return big;
    } // if

    Val big = Thread::Get()->AllocBinVec(CLASSD_bignum, Fixnum::Encode(3));
    Bignum* p = big->StaticCast<Bignum>();
    p->GetStart()[0] = static_cast<Bigit>(uVal);
    p->GetStart()[1] = static_cast<Bigit>(uVal >> Arch::BigitBits);
    return big;
} // MakeInt64

#elif 8 == SIZEOF_VAL
Val MakeInt64(int64 i64)
    { return MakeInt(i64); }

Val MakeUInt64(uint64 u64)
    { return MakeUInt(u64); }
#endif


Val Float32ComplexImpl::Neg() const
{
    return Float32ComplexImpl::Make(
        Float32Impl::Sub_1(GetReal()),
        Float32Impl::Sub_1(GetImag()) );
} // Float32ComplexImpl::Neg

Val Float64ComplexImpl::Neg() const
{
    return Float64ComplexImpl::Make(
        Float64Impl::Sub_1(GetReal()),
        Float64Impl::Sub_1(GetImag()) );
} // Float64ComplexImpl::Neg

Val RationalComplexImpl::Neg() const
    { return RationalComplexImpl::Make(sub_1(GetReal()), sub_1(GetImag())); }

namespace CommonLisp
{

namespace
{

static int cmp(Val const a, Val const b)
    { return Dispatcher_<int, Compare>::DispatchA(a, b); }

} // namespace

using namespace Private;

#define CLASSD_min_number       CLASSD_bignum
#define CLASSD_min_real         CLASSD_bignum
#define CLASSD_min_rattional    CLASSD_bignum
#define CLASSD_max_rattional    CLASSD_ratio
#define CLASSD_min_float        CLASSD_double_float
#define CLASSD_max_float        CLASSD_double_float
#define CLASSD_max_real         CLASSD_double_float
#define CLASSD_min_complex      CLASSD_rational_complex
#define CLASSD_max_complex      CLASSD_double_float_complex
#define CLASSD_max_number       CLASSD_double_float_complex

// [A]
defun_arith(add, Add)

// [D]
defun_arith(div, Div)

// [E]
defpred(eq, (Val a, Val b))
    { return Dispatcher_<bool, Eq>::DispatchA(a, b); }

// [G]
defpred(ge, (Val a, Val b))
    { return cmp(a, b) >= 0; }

defpred(gt, (Val const a, Val const b))
    { return cmp(a, b) > 0; }

// [L]
defpred(le, (Val const a, Val const b))
    { return cmp(a, b) <= 0; }

defpred(lt, (Val const a, Val const b))
    { return cmp(a, b) < 0; }

// [M]
defun_arith(mul, Mul)

// [N]
defpred(ne, (Val const a, Val const b))
    { return ! eq(a, b); }

defun(neg, (Val const a))
    { return sub(zero, a); }

defpred(numberp, (Val x))
{
    if (x->Is<Fixnum>()) return true;
    if (Record* p = x->DynamicCast<Record>())
    {
        return
            p->m_classd >= CLASSD_min_number &&
            p->m_classd <= CLASSD_max_number;
    }
    return false;
} // numberp

// [R]
defun(rem, (Val const a, Val const b))
{
    Val r;
    truncate(a, b, NULL, &r);
    return r;
} // rem

// [S]
defun_arith(sub, Sub)

defun(sub_1, (Val a))
{
    switch (offsetOfClassD(classd_of(a)))
    {
    case CLASSD_OFFSET(fixnum):
        return MakeInt(-Fixnum::Decode_(a));

    case CLASSD_OFFSET(bignum):
        return a->StaticCast<BignumImpl>()->Neg();

    case CLASSD_OFFSET(float32):
        return Float32Impl::Make(
            Float32Impl::Sub_1(a->StaticCast<Float32Impl>()->Get()) );

    case CLASSD_OFFSET(float32_complex):
        return a->StaticCast<Float32ComplexImpl>()->Neg();

    case CLASSD_OFFSET(float64):
        return Float64Impl::Make(
            Float64Impl::Sub_1(a->StaticCast<Float64Impl>()->Get()) );

    case CLASSD_OFFSET(float64_complex):
        return a->StaticCast<Float64ComplexImpl>()->Neg();

    case CLASSD_OFFSET(ratio):
        return a->StaticCast<RatioImpl>()->Neg();

    case CLASSD_OFFSET(rational_complex):
        return a->StaticCast<RationalComplexImpl>()->Neg();

    default:
        SignalTypeError(a, Qnumber);
    } // switch classd
} // sub_1

// [T]
Val truncate(Val const a, Val const b)
{
    Val q, r;
    truncate(a, b, &q, &r);
    return values(q, r);
} // truncate

void truncate(Val a, Val b, Val* q, Val* r)
    { DivOp<Truncate>::DispatchA(a, b, q, r); }

} // CommonLisp
} // TinyCl
