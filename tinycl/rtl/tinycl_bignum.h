//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Bignum Implementation
// tinycl_bignum.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_bignum.h#2 $
//
#if !defined(INCLUDE_tinycl_bignum_h)
#define INCLUDE_tinycl_bignum_h

namespace TinyCl
{

namespace Internal
{

typedef Arch::SignedBigit SignedBigit;
enum { BigitBits = sizeof(Bigit) * 8 };
Bigit const BigitMask = static_cast<Bigit>(-1);
Bigit const BigitMinus1 = static_cast<Bigit>(-1);
Bigit const BaseDiv2 = static_cast<Bigit>(1) << (BigitBits-1);


/// <summary>
///   Represents Bignum implementation.
/// </summary>
//
//  Arbitrary  integer is stored as vector of "bigit" in two's complement.
//  A "bigit" is 32-bit unsigned integer.
//
//  m_p contains bigits in least significant bigit first (little endian):
//      m_p[0]      = least significant bigit
//      ...
//      m_p[n-1]    = most significant bigit
//
//  A most signficant bigit is 32-bit integer instead of unsigned integer.
//
//  A m_p[n-1] is 0 or -1 when
//       0      if m_p[n-2] >= 0x8000_0000
//      -1      if m_p[n-2] <  0x8000_0000
//
class BignumImpl : public Bignum
{
    // [A]
    public: static Val Add(const BignumImpl*, const BignumImpl*);

    // [C]
    public: static int Cmp(const BignumImpl*, const BignumImpl*);
    public: static int Cmp(Val, Val);
    public: uint ComputeIntegerLength() const;
    public: uint ComputeMinLen() const;

    // [D]
    public: static Val Div(const BignumImpl*, const BignumImpl*);

    // [E]
    private: class Enum
    {
        protected: Bigit*   m_pRunner;
        protected: Bigit*   m_pEnd;

        protected: Enum(Bigit* pStart, Bigit* pEnd) :
            m_pEnd(pEnd),
            m_pRunner(pStart) {}

        protected: Enum(BignumImpl* p) :
            m_pEnd(p->GetStart() + p->GetLength()),
            m_pRunner(p->GetStart()) {}

        protected: Enum(BignumImpl* p, int) :
            m_pEnd(p->GetStart() - 1),
            m_pRunner(p->GetStart() + p->GetLength() - 1) {}

        public: bool AtEnd() const
            { return m_pRunner == m_pEnd; }

        public: Bigit Get() const
            { ASSERT(! AtEnd()); return *m_pRunner; }

        public: Bigit Set(Bigit b)
            { ASSERT(! AtEnd()); return *m_pRunner = b; }
    }; // Enum

    public: class EnumLtoM : public Enum
    {
        public: EnumLtoM(const BignumImpl* p, Int nSkip = 0) :
            Enum(p->GetStart() + nSkip, p->GetStart() + p->GetLength()) {}

        public: EnumLtoM(Val x) :
            Enum(x->StaticCast<BignumImpl>()) {}

        public: EnumLtoM(const EnumLtoM* const p) :
            Enum(p->m_pRunner, p->m_pEnd) {}

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumLtoM

    public: class EnumMtoL : public Enum
    {
        public: EnumMtoL(const BignumImpl* p) :
            Enum(p->GetStart() + p->GetLength() -1, p->GetStart() - 1) {}

        public: EnumMtoL(Val x) :
            Enum(x->StaticCast<BignumImpl>(), 1) {}

        public: void Next()
            { ASSERT(! AtEnd()); --m_pRunner; }
    }; // EnumMtoL

    // [G]
    public: Bigit GetLsb() const
        { return GetStart()[0]; }

    public: SignedBigit GetMsb() const
        { return static_cast<SignedBigit>(GetStart()[GetLength() - 1]); }

    public: Bigit GetSign() const
        { return IsMinus() ? static_cast<Bigit>(-1) : 0; }

    // [I]
    public: bool IsMinus() const
        { return GetMsb() < 0; }

    /// <summary>
    ///   Returns true if this bignum is 0x8000 0x0000 ... 0x0000.
    /// </summary>
    public: bool IsMostNegative() const
    {
        EnumMtoL oEnum(this);

        if (oEnum.Get() != 1u << (sizeof(Bigit) * 8 - 1))
        {
            return false;
        }

        // Is all zero?
        for (;;)
        {
            if (oEnum.AtEnd())
            {
                return true;
            }

            oEnum.Next();

            if (0 != oEnum.Get())
            {
                return false;
            }
        } // for
    } // IsMostNegative

    public: bool IsOne() const
        { return 1 == GetLength() && 1 == GetStart()[0]; }

    public: bool IsPlus() const
        { return ! IsZero() && GetMsb() >= 0; }

    public: bool IsZero() const
        { return 1 == GetLength() && 0 == GetStart()[0]; }

    // [L]
    public: static Val Logand(const BignumImpl*, const BignumImpl*);
    public: static Val Logior(const BignumImpl*, const BignumImpl*);
    public: static Val Logeqv(const BignumImpl*, const BignumImpl*);
    public: static Val Logxor(const BignumImpl*, const BignumImpl*);

    // [M]
    public: static Val Mul(const BignumImpl*, const BignumImpl*);

    public: Val Neg() const;

    // [P]
    public: Val Pin() const;

    // [S]
    public: Val ShiftLeft(uint) const;
    public: Val ShiftRight(uint) const;
    public: static Val Sub(const BignumImpl*, const BignumImpl*);

    // [T]
    public: float32 ToFloat32() const;
    public: static float32 ToFloat32(Val);
    public: float64 ToFloat64() const;
    public: static float64 ToFloat64(Val);

    public: static void Truncate(
        const BignumImpl*,
        const BignumImpl*,
        Val*,
        Val* );
}; // BignumImpl

/// <summary>
///   Represents double word bigit.
/// </summary>
class DoubleBigit
{
    public: struct Div
    {
        Bigit q; Bigit r;
        Div(Bigit qq, Bigit rr) : q(qq), r(rr) {}
    }; // Div

    public: Bigit   low;
    public: Bigit   high;

    // ctor
    public: DoubleBigit(Bigit l, Bigit h) :
        low(l), high(h) {}

    #if 4 == SIZEOF_VAL
        public: DoubleBigit(uint64 u64) :
            low(static_cast<Bigit>(u64 & BigitMask)),
            high(static_cast<Bigit>((u64 >> 32) & BigitMask)) {}
    #endif

    // operator
    public: DoubleBigit& operator +=(Bigit n)
    {
        Bigit a = low;
        low += n;
        if (low < a) high += 1;
        return *this;
    } // operator +=

    public: DoubleBigit& operator -=(Bigit n)
    {
        Bigit a = low;
        low -= n;
        if (low > a) high -= 1;
        return *this;
    } // operator -=

    public: bool operator <=(const DoubleBigit& b) const
    {
        if (high == b.high) return low <= b.low;
        return high < b.high;
    } // operator <=

    // Add -- with carry
    public: static Bigit Add(Bigit a, Bigit b, bool* out_fCarry)
    {
        Bigit c = a + b;
        *out_fCarry = c < a;
        return c;
    } // Add

    // Mul -- a x b => double bigit
    public: static DoubleBigit Mul(Bigit a, Bigit b);

    // Rem -- remiander
    public: static Bigit Rem(Bigit y, Bigit x, Bigit z)
    {
        Div oDiv = Truncate(x, y, z);
        return oDiv.r;
    } // Rem

    // Shl -- arithmetic shift left
    public: static DoubleBigit Shl(Bigit a, uint k);

    // Truncate -- (x||y) / z ... returns quotient and remainder
    // Note: On overflow, x >= z, this function returns undefined values.
    public: static Div Truncate(Bigit y, Bigit x, Bigit z)
    {
        const int N = sizeof(Bigit) * 8;
        for (int i = 1; i <= N; i++)
        {
            // All 1's if MSB=1
            Bigit w = static_cast<SignedBigit>(x) >> (N-1);

            // (x||y) <<= 1
            x = (x << 1) | (y >> (N-1));
            y <<= 1;

            // 33bit comparision
            if ((x | w) >= z)
            {
                x -= z;
                y += 1;
            }
        } // for i
        return Div(y, x);
    } // Truncate
}; // DoubleBigit

class StackBignumImpl : public BignumImpl
{
    public: static Val ClassD_() { return CLASSD_bignum; }

    public: operator Val() const
        { return Encode(); }

    public: Val PinNeg() const;
}; // StackBignumImpl

class ALIGN_BINVEC
    BignumInt : public StackBignumImpl
{
    private: Bigit  m_rgBigit[1];

    public: BignumInt(Int iVal)
    {
        m_classd = ClassD_();
        m_length = one;
        m_rgBigit[0] = static_cast<Bigit>(iVal);
    } // BignumInt
}; // BignumInt

class ALIGN_BINVEC
    BignumUInt : public StackBignumImpl
{
    private: Bigit  m_rgBigit[2];

    public: BignumUInt(UInt iVal)
    {
        m_classd = ClassD_();
        m_length = one;
        m_rgBigit[0] = static_cast<Bigit>(iVal);
        if (static_cast<SignedBigit>(iVal) >= 0)
        {
            m_length = one;
        }
        else
        {
            m_length = two;
            m_rgBigit[1] = 0;
        }
    } // BignumUInt
}; // BignumUInt

#if 4 == SIZEOF_VAL

class ALIGN_BINVEC
    BignumInt64 : public StackBignumImpl
{
    private: Bigit  m_rgBigit[2];

    public: BignumInt64(int64 i64)
    {
        m_classd = ClassD_();
        m_length = two;
        m_rgBigit[0] = static_cast<Bigit>(i64 & BigitMask);
        m_rgBigit[1] = static_cast<Bigit>((i64 >> 32) & BigitMask);

        m_length = Fixnum::Encode(ComputeMinLen());
    } // BignumInt64
}; // BignumInt64

class ALIGN_BINVEC
    BignumUInt64 : public StackBignumImpl
{
    private: Bigit  m_rgBigit[3];

    public: BignumUInt64(uint64 u64)
    {
        m_classd = ClassD_();
        m_length = Fixnum::Encode(3);
        m_rgBigit[0] = static_cast<Bigit>(u64 & BigitMask);
        m_rgBigit[1] = static_cast<Bigit>((u64 >> 32) & BigitMask);
        m_rgBigit[2] = 0;

        m_length = Fixnum::Encode(ComputeMinLen());
    } // BignumUInt64
}; // BignumUInt64

#elif 8 == SIZEOF_VAL
    typedef class BignumInt  BignumInt64;
    typedef class BignumUInt BignumUInt64;
#else
    #error Unsupported "SIZEOF_VAL"
#endif

/// <summary>
///  Represents bignum on object stack.
/// </summary>
class StackBignum
{
    private: Val m_x;

    private: StackBignum(Val x) :
        m_x(x) {}

    public: static StackBignum AllocMinusOne(
        Int const cBigits )
    {
        Val b = Thread::Get()->AllocBinVec(
            BignumImpl::ClassD_(),
            Fixnum::Encode(cBigits));

        foreach (BignumImpl::EnumLtoM, oEnum, b)
        {
            oEnum.Set(static_cast<Bigit>(-1));
        } // for

        return StackBignum(b);
    } // AllocMinusOne

    public: static StackBignum AllocZero(
        Int const cBigits )
    {
        Val b = Thread::Get()->AllocBinVec(
            BignumImpl::ClassD_(),
            Fixnum::Encode(cBigits));

        foreach (BignumImpl::EnumLtoM, oEnum, b)
        {
            oEnum.Set(0);
        } // for

        return StackBignum(b);
    } // AllocZero

    public: static StackBignum Alloc(
        Int                 const cBigits,
        const BignumImpl*   const pA )
    {
        Val b = Thread::Get()->AllocBinVec(
            BignumImpl::ClassD_(),
            Fixnum::Encode(cBigits) );

        BignumImpl::EnumLtoM oEnumB(b->StaticCast<BignumImpl>());

        foreach (BignumImpl::EnumLtoM, oEnumA, pA)
        {
            oEnumB.Set(oEnumA.Get());
            oEnumB.Next();
        } // for A

        Bigit nSign = pA->GetSign();
        while (! oEnumB.AtEnd())
        {
            oEnumB.Set(nSign);
            oEnumB.Next();
        } // while

        return StackBignum(b);
    } // Alloc

    public: static StackBignum AllocNeg(
        const BignumImpl* const pA )
    {
        if (pA->IsMostNegative())
        {
            Val const b = Thread::Get()->AllocBinVec(
                BignumImpl::ClassD_(),
                Fixnum::Encode(pA->GetLength() + 1) );

            BignumImpl* const pB = b->StaticCast<BignumImpl>();

            ::CopyMemory(
                pB->GetStart(),
                pA->GetStart(),
                sizeof(Bigit) * pA->GetLength() );

            pB->GetStart()[pB->GetLength() - 1] = 0;

            return StackBignum(b);
        }
        else
        {
            Val const b = Thread::Get()->AllocBinVec(
                BignumImpl::ClassD_(),
                Fixnum::Encode(pA->GetLength()) );

            BignumImpl::EnumLtoM oEnumB(b->StaticCast<StackBignumImpl>());

            Bigit nCarry = 1;
            foreach (BignumImpl::EnumLtoM, oEnumA, pA)
            {
                DoubleBigit oAcc(~oEnumA.Get(), 0);
                oAcc += nCarry;
                oEnumB.Set(oAcc.low);
                nCarry = oAcc.high;
                oEnumB.Next();
            } // for A

            ASSERT(0 == nCarry);

            return StackBignum(b);
        }
    } // AllocNeg

    public: StackBignumImpl* Get() const
        { return m_x->StaticCast<StackBignumImpl>(); }
}; // StackBignum

} // Internal
} // TinyCl

#endif //!defined(INCLUDE_tinycl_bignum_h)
