//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions
// tinycl.h
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl.h#29 $
//
#if !defined(INCLUDE_tinycl_h)
#define INCLUDE_tinycl_h

#define USE_MODERN_MODE 1

#define BIGENDIAN       0
#define FLOT_BIGENDIAN  BIGENDIAN
#define SIZEOF_VAL      4
#define ALIGN_BINOBJ    __declspec(align(8))
#define ALIGN_BINVEC    __declspec(align(8))
#define ALIGN_CONS      __declspec(align(8))
#define ALIGN_NULL      __declspec(align(16))
#define ALIGN_RECORD    __declspec(align(8))

typedef __int64 Int64;
typedef unsigned __int64 UInt64;
typedef Int64 int64;
typedef UInt64 uint64;

//////////////////////////////////////////////////////////////////////
//
// Utility Classes and Functions
//
namespace TinyCl
{

namespace Private
{
    static const char16 Backquote   = 0x60;
    static const char16 Backslash   = 0x5C;
    static const char16 CloseParen  = 0x29;
    static const char16 DoubleQuote = 0x22;
    static const char16 Newline     = 0x0A;
    static const char16 OpenParen   = 0x28;
    static const char16 Page        = 0x0C;
    static const char16 Quote       = 0x27;
    static const char16 Return      = 0x0D;
    static const char16 SemiColon   = 0x3B;
    static const char16 Space       = 0x20;
}; // Private

class AsInt
{
    public: Int ToInt() const { return reinterpret_cast<Int>(this); }

    public: template<typename T> static T* FromInt(Int iVal)
        { return reinterpret_cast<T*>(iVal); }
}; // AsInt


//////////////////////////////////////////////////////////////////////
//
// CharSink
//
template<int t_cwch = 60>
class CharSink_
{
    private: char16*    m_pwch;
    private: char16*    m_pwchEnd;
    private: char16*    m_pwchStart;
    private: char16     m_rgwch[t_cwch];

    public: CharSink_() :
        m_pwch(m_rgwch),
        m_pwchEnd(m_rgwch + lengthof(m_rgwch)),
        m_pwchStart(m_rgwch) {}

    public: ~CharSink_()
    {
        if (m_pwchStart != m_rgwch) delete[] m_pwchStart;
    } // ~CharSink

    public: void Add(char16 ch)
    {
        if (m_pwch + 2 > m_pwchEnd)
        {
            grow();
        }

        *m_pwch++ = ch;
    } // Add

    public: int GetLength() const
        { return static_cast<int>(m_pwch - m_pwchStart); }

    public: char16* GetStart() const
        { return m_pwchStart; }

    private: void grow()
    {
        int cwch    = GetLength();
        int cwchNew = cwch * 130 / 100;

        char16* pwchNew = new char16[cwchNew];

        CopyMemory(pwchNew, m_pwchStart, sizeof(char16) * cwch);

        m_pwchStart = pwchNew;
        m_pwchEnd   = pwchNew + cwchNew;
        m_pwch      = pwchNew + cwch;
    } // grow

    public: void Shrink()
    {
        ASSERT(m_pwch > m_pwchStart);
        --m_pwch;
    } // Shrink
}; // CharSink

inline Int Ceiling(Int const x, Int const n)
    { return (x + n - 1) / n; }

inline uint RotateLeft(uint const x, uint const n)
{
    uint const nHigh = x >> (sizeof(uint) * 4 - n);
    uint y = x << n;
    y |= nHigh;
    return y;
} // RotateLeft

inline Int RoundUp(Int const x, Int const n)
    { return Ceiling(x, n) * n; }

} // TinyCl

#include "./z_util.h"

namespace TinyCl
{

using namespace Common;

//          Exe Start 0x00400000
const Int LISP_BASE = 0x0F000000;

#define defstatic_(mp_name, mp_datum) \
    Val const mp_name = reinterpret_cast<Val>(mp_datum)


class Datum;
typedef Datum* Val;

// Align_FunObj
//  We choose 16 for x86/x64. This value is based on
//    o cache line size
//    o Easy to map return address to function object
class Arch32
{
    public: typedef uint32 Bigit;
    public: typedef int32  SignedBigit;

    public: enum Constant
    {
        AllocUnit           = 1 << 16,

        Bits                = 32,
        TagBits             = 3,
        TagMask             = (1 << TagBits) - 1,

        Align_BinVec        = 8,
        Align_FunObj        = 16,
        Align_Instance      = 8,
        Align_Record        = 8,

        ArrayDimensionLimit = 1 << (Bits - TagBits - 1),
        ArrayRankLimit      = 7,
        ArrayTotalSizeLimit = ArrayDimensionLimit,

        BigitBits           = sizeof(Bigit) * 8,

        CharCodeLimit       = 1 << 16,

        MultipleValuesLimit = 128,

        TlvLimits           = 4096,
    }; // Constant

    public: enum Tag
    {
        Tag_Fixnum      = 0,
        Tag_Record      = 1,
        Tag_Null        = 2,
        Tag_Invalid3    = 3,

        Tag_Fixnum1     = 4,
        Tag_FunObj      = 5,
        Tag_Cons        = 6,
        Tag_Invalid7    = 7,
    }; // Tag

    public: union Float32
    {
        float   m_flt;
        uint32  m_i;
    }; // Float32

    public: union Float64
    {
        double  m_dbl;

        #if FLOAT_BIGENDIAN
            struct hl
            {
                uint32  h;
                uint32  l;
            } m_hl;
        #else
            struct hl
            {
                uint32  l;
                uint32  h;
            } m_hl;
        #endif
    }; // Float64

    public: typedef float  float32;
    public: typedef double float64;
}; // Arch32

typedef Arch32 Arch;
typedef Arch::float32 float32;
typedef Arch::float64 float64;
typedef Arch::Bigit   Bigit;

enum Limits
{
    MaxFormLength = 99999,
}; // Limits

} // TinyCl

#include "./tinycl_memory.h"
#include "./tinycl_layout.h"
#include "./tinycl_kernel.h"

#include "./rtl/tinycl_rtl.h"
#include "./rtl/tinycl_rtl2.h"

namespace TinyCl
{

class CString
{
    private: const char16*   m_pwch;
    private: Int             m_cwch;

    // ctor
    public: CString(const char16* pwch = 0, Int cwch = 0) :
        m_pwch(pwch), m_cwch(cwch) {}

    public: CString(Val s)
    {
        if (simple_string_p(s))
        {
            SimpleString* p = s->StaticCast<SimpleString>();
            m_pwch = p->GetStart();
            m_cwch = Fixnum::Decode_(p->m_length);
            return;
        }

        SignalTypeError(s, Qstring);
    } // CString

    const char16* GetEnd()    const { return m_pwch + m_cwch; }
    Int           GetLength() const { return m_cwch; }
    const char16* GetStart()  const { return m_pwch; }

    class Enum
    {
        const char16* m_pwch;
        const char16* m_pwchEnd;

        public: Enum(CString* p) :
            m_pwch(p->GetStart()), m_pwchEnd(p->GetEnd()) {}

        public: Enum(CString& r) :
            m_pwch(r.GetStart()), m_pwchEnd(r.GetEnd()) {}

        public: bool AtEnd() const { return m_pwch == m_pwchEnd; }
        public: char16 Get() const { ASSERT(!AtEnd()); return *m_pwch; }
        public: void Next() { ASSERT(!AtEnd()); m_pwch++; }

        public: const char16* GetPtr() const { return m_pwch; }
    }; // Enum
}; // CString

// Compute Adler32(RFC1950)
inline UInt ComputeHash(UInt nDatum, UInt nAcc = 0)
{
    const UInt k_nBase = 65521; // largest prime small than 65536
    UInt s1 = nAcc & 0xFFFF;
    UInt s2 = (nAcc >> 16) & 0xFFFF;
    s1 = (s1 + nDatum) % k_nBase;
    s2 = (s2 + s1) % k_nBase;
    return (s2 << 16) | s1;
} // ComputeHash

#define assert_latch_locked(mp_latch)
#define with_exclusive_latch(mp_latch)

// Application Program Interface

enum FunRet
{
    FunRet_Arbitrary    = -1,
    FunRet_Bool         = -2,
    FunRet_NoReturn     = -3,
}; // Values

#define TINYCLAPI __fastcall

Val TINYCLAPI CallFunction(Val, Val*, uint);

/// <summary>
///  Call lisp function and returns a condition if funcation signals
///  condition or nil if it doesn't.
/// </summary>
/// <param name="fn">A lisp function to call.</param>
inline Val TINYCLAPI CallFunction(Val fn)
    { return CallFunction(fn, static_cast<Val*>(NULL), 0); }

/// <summary>
///  Call lisp function and returns a condition if funcation signals
///  condition or nil if it doesn't.
/// </summary>
/// <param name="fn">A lisp function to call.</param>
inline Val TINYCLAPI CallFunction(Val fn, Val a)
    { return CallFunction(fn, &a, 1); }

/// <summary>
///  Call lisp function and returns a condition if funcation signals
///  condition or nil if it doesn't.
/// </summary>
/// <param name="fn">A lisp function to call.</param>
inline Val TINYCLAPI CallFunction(Val fn, Val a, Val b)
    { Val mv[2]= {a, b}; return CallFunction(fn, mv, lengthof(mv)); }

#if _DEBUG
    #define DEBUG_FORMAT(mp_fmt, ...) \
        Debugger::Printf("%u: %hs(%d): ", \
            ::GetTickCount(), __FUNCTION__, __LINE__); \
        DebugFormat(mp_fmt, __VA_ARGS__)

    extern void DebugFormat(const char*, ...);
#else
    #define DEBUG_FORMAT(mp_fmt, ...) __noop(__VA_ARGS__)
#endif

} // TinyCl

#endif //!defined(INCLUDE_tinycl_h)
