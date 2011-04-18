#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime I/O
// tinycl_rtl_io.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_io.cpp#8 $
//
#include "../tinycl.h"

#include "./tinycl_bignum.h"
#include "./tinycl_complex.h"
#include "./tinycl_float.h"
#include "./tinycl_ratio.h"

extern "C" double __cdecl ceil(double);

namespace TinyCl
{

using namespace Internal;
using namespace Private;

namespace Private
{

static Val ensureOutputStream(Val s)
{
    if (nil == s) return TLV(Astandard_outputA);
    if (t   == s) return TLV(Aterminal_ioA);
    check_type(s, stream);
    return s;
} // ensureOutputStream

// valuesToList
static Val valuesToList(int iStart)
{
    Thread* p = Thread::Get();
    int iEnd = static_cast<int>(Fixnum::Decode_(p->m_n));
    Val runner = nil;
    for (int i = iEnd - 1; i >= iStart; i -= 1)
    {
        runner = cons(p->mv_value[i], runner);
    } // for i
    return runner;
} // valuesToList

class FloatPrinter
{
    private: char16*    m_pwchDigit;
    private: char16     m_wszDigit[20];

    // ctor
    public: FloatPrinter() :
        m_pwchDigit(m_wszDigit) {}

    private: Val ashl(uint64 const u64, uint const k)
    {
        if (0 == k) return MakeUInt64(u64);
        BignumUInt64 oX(u64);
        return ash(oX, Fixnum::Encode(k));
    } // ashl

    private: static Val expt10(Int iPower)
    {
        ASSERT(iPower >= 0);

        Val result = one;
        Val base = Fixnum::Encode(10);

        Int k = iPower;
        while (k > 0)
        {
            if (k & 1)
            {
                result = mul(result, base);
                k -= 1;
            }
            else
            {
                base = mul(base, base);
                k >>= 1;
            }
        } // while

        return result;
    } // expt10

    // [P]
    public: void Print(
        Val     const stream,
        uint    const nSign,
        char16  const chMarker,
        int     const iExptIn )
    {
        if (nSign)
        {
            write_char('-', stream);
        }

        int iExpt = iExptIn;

        const char16* pwszDigit = m_wszDigit;

        if (iExpt >= -3 && iExpt <= 6)
        {
            // 10^3 <= |x| < 10^7 => free-format

            if (iExpt < 0)
            {
                write_string("0.", stream);
                for (int i = iExpt + 1; i < 0; i++)
                {
                    write_char('0', stream);
                } // for i
            } // if

            do
            {
                if (0 == *pwszDigit)
                {
                    write_char('0', stream);
                }
                else
                {
                    write_char(*pwszDigit, stream);
                    pwszDigit++;
                }

                if (0 == iExpt)
                {
                    write_char('.', stream);
                }

                iExpt -= 1;
            } while (0 != *pwszDigit || iExpt >= -1);

            if ('e' != chMarker)
            {
                write_char(chMarker, stream);
                write_char('0', stream);
            }
        }
        else
        {
            // Free-Format with Exponential
            write_char(*pwszDigit++, stream);
            write_char('.', stream);
            if (0 == *pwszDigit)
            {
                write_char('0', stream);
            }
            else
            {
                write_string(pwszDigit, stream);
            }

            format(stream, "~C~D",
                Character::FromCode(chMarker),
                Fixnum::Encode(iExpt) );
        } // if
    } // Print

    // [R]
    public: int RunNormal(
        uint64  const nSignificand,
        int     const iExpt,
        int     const iPrecision )
    {
        int iExpt10 = runNormal(nSignificand, iExpt, iPrecision);
        *m_pwchDigit = 0;
        return iExpt10;
    } // RunNormal

    int runNormal(uint64 ullSgfnd, int iExpt, int iPrec)
    {
        bool fRound = 0 == ullSgfnd % 2;
        const uint64 ullBase = static_cast<uint64>(1) << iPrec;
        if (iExpt >= 0)
        {
            if (ullBase != ullSgfnd)
            {
                Val be = ashl(1, iExpt);
                return scale(
                    ullSgfnd, iExpt,
                    mul(be, ashl(ullSgfnd, 1)), Fixnum::Encode(2),
                    be, be,
                    fRound, fRound );
            }
            else
            {
                Val be  = ashl(1, iExpt);
                Val be1 = ash(be, one);
                return scale(
                    ullSgfnd, iExpt,
                    mul(be1, ashl(ullSgfnd, 1)), Fixnum::Encode(4),
                    be1, be,
                    fRound, fRound );
            }
        }
        else if (ullBase != ullSgfnd)
        {
            return scale(
                ullSgfnd, iExpt,
                ashl(ullSgfnd, 1), ashl(2, -iExpt),
                one, one,
                fRound, fRound );
        }
        else
        {
            return scale(
                ullSgfnd, iExpt,
                ashl(ullSgfnd, 4), ashl(2, 1 - iExpt),
                Fixnum::Encode(2), one,
                fRound, fRound );
        }
    } // runNormal

    public: int RunSubnormal(
        uint64  const nSignificand,
        int     const iExpt )
    {
        ASSERT(iExpt < 0);

        bool const fRound = 0 == nSignificand % 2;

        int iExpt10 = scale(
            nSignificand,
            iExpt,
            ashl(nSignificand, 1),
            ashl(2, -iExpt),
            one,
            one,
            fRound,
            fRound );

        *m_pwchDigit = 0;
        return iExpt10;
    } // RunSubnormal

    // [S]
    int scale(
        uint64  ullSgfnd,
        int     iExpt,    // for estimation of k
        Val     r,
        Val     s,
        Val     mp,
        Val     mm,
        bool    fLow,
        bool    fHigh )
    {
        float64 dblEst = (iExpt + integer_length(ullSgfnd) - 1);
        dblEst *= 0.30102999566398114e0;    // (/ (log 2) (log 10))
        dblEst -= 1e-10;
        dblEst = ::ceil(dblEst);

        int iEst = static_cast<int>(dblEst);

        if (iEst >= 0)
        {
            return fixup(
                r,
                mul(s, expt10(iEst)),
                mp,
                mm,
                iEst,
                fLow,
                fHigh );
        }
        else
        {
            Val scale = expt10(-iEst);
            return fixup(
                mul(r, scale), s,
                mul(mp, scale), mul(mm, scale),
                iEst,
                fLow, fHigh );
        } // if
    } // scale

    private: int fixup(Val r, Val s, Val mp, Val mm, int k, bool fLow, bool fHigh)
    {
        if (fHigh ? ge(add(r, mp), s) : gt(add(r, mp), s))
        {
            generate(r, s, mp, mm, fLow, fHigh);
            k += 1;
        }
        else
        {
            generate(mul(r, 10), s, mul(mp, 10), mul(mm, 10), fLow, fHigh);
        }
        return k - 1;
    } // fixup

    private: void generate(Val r, Val s, Val mp, Val mm, bool fLow, bool fHigh)
    {

      loop:
        char16 wchDigit;
        {
            Val d;
            truncate(r, s, &d, &r);
            wchDigit = static_cast<char16>(Fixnum::Decode_(d) + '0');
                ASSERT(wchDigit >= '0' && wchDigit <= '9');
        } // wchDigit

        bool fTc1 = fLow  ? le(r, mm) : lt(r, mm);
        bool fTc2 = fHigh ? ge(add(r, mp), s) : gt(add(r, mp), s);

        if (! fTc1)
        {
            if (! fTc2)
            {
                *m_pwchDigit++ = wchDigit;
                r = mul(r, 10);
                mp = mul(mp, 10);
                mm = mul(mm, 10);
                goto loop;
            }
            else
            {
                *m_pwchDigit++ = static_cast<char>(wchDigit + 1);
            }
        }
        else if (! fTc2)
        {
            *m_pwchDigit++ = wchDigit;
        }
        else if (lt(ash(r, one), s))
        {
            *m_pwchDigit++ = wchDigit;
        }
        else
        {
            *m_pwchDigit++ = static_cast<char>(wchDigit + 1);
        }
    } // generate

    // integer_length
    private: static int integer_length(uint64 k)
    {
        int n = 0;
        while (0 != k)
        {
            k >>= 1;
            n += 1;
        } // while
        return n;
    } // integer_length
}; // FloatPrinter

template<class Float_, typename uint_>
class FloatPrinter_
{
    public: void static Run(
        const Float_*   const pA,
        Val                 const stream )
    {
        const Float_::Layout* const p = pA->GetLayout();

        switch (Float_::Classify(p))
        {
        case FpClass_Normal:
        {
            uint_ uSignificand = Float_::GetSignificand(p);
            uSignificand |= static_cast<uint_>(1) << Float_::SignificandBits;

            FloatPrinter oPrinter;

            int iExpt10 = oPrinter.RunNormal(
                uSignificand,
                p->m_nExponent -
                    Float_::ExponentBias -
                    Float_::NormalPrecision +
                    1,
                Float_::NormalPrecision );

            oPrinter.Print(
                stream,
                p->m_nSign,
                Float_::GetMarker(),
                iExpt10 );

            break;
        } // FpClass_Normal

        case FpClass_Subnormal:
        {
            uint_ uSignificand = Float_::GetSignificand(p);

            FloatPrinter oPrinter;

            int iExpt10 = oPrinter.RunSubnormal(
                uSignificand,
                Float_::ExponentSubnormal );

            oPrinter.Print(
                stream,
                p->m_nSign,
                Float_::GetMarker(),
                iExpt10 );

            break;
        } // FpClass_Subnormal

        case FpClass_Zero:
            if (p->m_nSign)
            {
                write_char('-', stream);
            }
            write_string("0.0", stream);
            write_char(Float_::Marker_(), stream);
            write_char('0', stream);
            break;

        case FpClass_Infinity:
            write_string("#<", stream);
            write_string(Float_::TypeString_(), stream);
            format(stream, " ~CInfinity>",
                p->m_nSign ?
                    Character::FromCode('-') :
                    Character::FromCode('+') );
            break;

        case FpClass_NaN:
            write_string("#<", stream);
            write_string(Float_::TypeString_(), stream);
            format(stream, " ~CNaN>",
                p->m_nSign ?
                    Character::FromCode('-') :
                    Character::FromCode('+') );
            break;

        case FpClass_SNaN:
            write_string("#<", stream);
            write_string(Float_::TypeString_(), stream);
            format(stream, " ~CSNaN>",
                p->m_nSign ?
                    Character::FromCode('-') :
                    Character::FromCode('+') );
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch fpclass
    } // Run
}; // FloatPrinter_

/// <summary>
///   Represents Formatter for format-control.
/// </summary>
class Formatter
{
    // Entry Point
    public: static Val Run()
    {
        Formatter oFormatter;
        return oFormatter.run();
    } // Run

    private: struct Param
    {
        int     m_cParams;
        bool    m_fAt;
        bool    m_fColon;
        Val     mv_value[5];

        Param() :
            m_cParams(0),
            m_fAt(false),
            m_fColon(false) {}

        Int GetInt(int iNth, Int iDefault) const
        {
            ASSERT(iNth < lengthof(mv_value));

            if (iNth >= m_cParams)
            {
                return iDefault;
            }

            Val n = mv_value[iNth];
            unless (fixnump(n)) SignalTypeError(n, Qinteger);
            return Fixnum::Decode_(n);
        } // GetInt
    }; // Param

    private: Val m_allargs;
    private: Val m_args;
    private: Val m_control;
    private: Val m_parens;
    private: Val m_stream;

    private: CString m_oControl;

    // ctor
    private: Formatter()
    {
        Thread* p = Thread::Get();
        m_stream  = p->mv_value[0];
        m_control = p->mv_value[1];
        m_allargs = valuesToList(2);
        m_args    = m_allargs;
        m_parens  = nil;
        m_oControl = CString(p->mv_value[1]);

        if (t == m_stream)
        {
            m_stream = TLV(Astandard_outputA);
        }
    } // Formatter

    // [F]
    private: void formatChars(const Param* p, char16 wch)
    {
        for (Int iCount = p->GetInt(0, 1); iCount > 0; iCount -= 1)
        {
            write_string(&wch, 1, m_stream);
        } // for
    } // formatChars

    // formatError
    private: void NoReturn formatError(const char* psz)
    {
        error(make_string(psz));
    } // formatError

    // format_A
    private: void format_A(const Param* p, Val val)
    {
        if (nil == val && p->m_fColon)
        {
            write_string("()", m_stream);
        }
        else
        {
            BindFrameScope_<2> oLet;
            oLet.Bind(TLV_Aprint_escapeA, nil);
            oLet.Bind(TLV_Aprint_readablyA, nil);

            print_object(val, m_stream);
        }
    } // format_A

    // format_C
    private: void format_C(const Param* p, Val val)
    {
        unless (characterp(val))
        {
            BindFrameScope_<2> oLet;
            oLet.Bind(TLV_Aprint_escapeA, nil);
            oLet.Bind(TLV_Aprint_readablyA, nil);

            print_object(val, m_stream);
            return;
        }

        char16 wch = val->StaticCast<Character>()->ToCode();
        if (p->m_fColon)
        {
            if (wch >= 0x21 && wch <= 0x7E)
            {
                write_string(&wch, 1, m_stream);
            }
            else
            {
                format(m_stream, "u~4,'0X", Fixnum::Encode(wch));
            }
        }
        else
        {
            write_string(&wch, 1, m_stream);
        }
    } // format_C

    // format_R
    private: void format_R(
        const Param* const p,
        Int          const iBase,
        Val          const val )
    {
        ASSERT(iBase >= 2 && iBase <= 36);

        Val const base = Fixnum::Encode(iBase);

        unless (fixnump(val))
        {
            BindFrameScope_<4> oLet;
            oLet.Bind(TLV_Aprint_escapeA,   nil);
            oLet.Bind(TLV_Aprint_radixA,    nil);
            oLet.Bind(TLV_Aprint_baseA,     base);
            oLet.Bind(TLV_Aprint_readablyA, nil);

            print_object(val, m_stream);
            return;
        } // unless

        char16 rgwch[100];
        char16* pwch = &rgwch[lengthof(rgwch)];
        char16  wchSign = p->m_fAt ? '+' : 0;
        if (zero == val)
        {
            *--pwch = '0';
        }
        else
        {
            Int iVal = Fixnum::Decode_(val);
            if (iVal < 0)
            {
                iVal = -iVal;
                wchSign = '-';
            }

            while (0 != iVal)
            {
                Int iMod = iVal % iBase;
                iVal /= iBase;
                *--pwch = digit_char(Fixnum::Encode(iMod), base)->
                    StaticCast<Character>()->ToCode();
            } // while
        } // if

        if (wchSign) *--pwch = wchSign;

        Val pad = Character::FromCode(' ');
        if (p->m_cParams >= 2)
        {
            pad = p->mv_value[1];
            if (MARKER_unbound == pad)
            {
                pad = Character::FromCode(' ');
            }
            else if (characterp(pad))
            {
                // good pad char
            }
            else
            {
                SignalTypeError(pad, Qcharacter);
            }
        } // if

        Int const cwch = &rgwch[lengthof(rgwch)] - pwch;

        if (p->m_cParams >= 1 && fixnump(p->mv_value[0]))
        {
            Int iWidth = Fixnum::Decode_(p->mv_value[0]);
            iWidth -= cwch;
            while (iWidth > 0)
            {
                write_char(pad, m_stream);
                iWidth -= 1;
            }
        } // if

        write_string(pwch, cwch, m_stream);
    } // format_R

    // format_S
    private: void format_S(const Param* const p, Val const val)
    {
        if (nil == val && p->m_fColon)
        {
            write_string("()", m_stream);
        }
        else
        {
            BindFrameScope_<1> oLet;
            oLet.Bind(TLV_Aprint_escapeA, t);

            print_object(val, m_stream);
        }
    } // formatS

    // [M]
    private: void mainLoop()
    {
        foreach (CString::Enum, oEnum, &m_oControl)
        {
            unless ('~' ==  oEnum.Get())
            {
                const char16* pwch = oEnum.GetPtr();
                do
                {
                    oEnum.Next();
                    if (oEnum.AtEnd()) break;
                } while ('~' != oEnum.Get());
                write_string(pwch, oEnum.GetPtr() - pwch, m_stream);
                if (oEnum.AtEnd()) break;
            } // if

            if (oEnum.AtEnd())
                { formatError("Cotnrol string ends with '~'."); }

            Param oParam;
            parseParams(&oEnum, &oParam);

            if (oEnum.AtEnd())
                { formatError("Missing directive character."); }

            switch (oEnum.Get())
            {
            case '&':
                // FIXME 2008-08-03 yosi@msn.com NYI ~&
                // FALLTHORUGH

            case '%':
                formatChars(&oParam, Newline);
                break;

            case '(':
                push(
                    cons(
                        Character::FromCode(OpenParen),
                        Fixnum::Encode(oEnum.GetPtr() - m_oControl.GetStart()) ),
                    m_parens );
                break;

            case ')':
                if (caar(m_parens) == Character::FromCode(OpenParen))
                {
                    m_parens = cdr(m_parens);
                }
                else
                {
                    formatError("Too many ~\x29");
                }
                break;

            case 'A': case 'a':
                format_A(&oParam, nextArg());
                break;

            case 'C': case 'c':
                format_C(&oParam, nextArg());
                break;

            case 'B': case 'b':
                format_R(&oParam, 2, nextArg());
                break;

            case 'D': case 'd':
                format_R(&oParam, 10, nextArg());
                break;

            case 'O': case 'o':
                format_R(&oParam, 8, nextArg());
                break;

            case 'S': case 's':
                format_S(&oParam, nextArg());
                break;

            case 'X': case 'x':
                format_R(&oParam, 16, nextArg());
                break;

            case '|':
                formatChars(&oParam, Page);
                break;

            case '~':
                formatChars(&oParam, '~');
                break;

            default:
                formatError("Unknown directive");
            } // swtich char
        } // for each char

        {
            Val const open_posn = m_parens;
            if (nil != open_posn)
            {
                error("~~%C starting at ~D isn't closed.",
                    car(open_posn), cdr(open_posn) );
            }
        }
    } // mainLoop

    // [N]
    private: Val nextArg()
    {
        if (nil == m_args) formatError("Insufficent arguments.");
        return pop(m_args);
    } // nextArg

    // [P]
    private: void parseParams(CString::Enum* pEnum, Param* p)
    {
        enum
        {
            State_None,

            State_Digit,
            State_Next,
            State_Quote,
        } eState = State_None;

        while (! pEnum->AtEnd())
        {
            pEnum->Next();
            char16 wch = pEnum->Get();

            switch (eState)
            {
            case State_None:
                if (wch >= '0' && wch <= '9')
                {
                    p->mv_value[p->m_cParams] = Fixnum::Encode(wch - '0');
                    eState = State_Digit;
                }
                else if ('\'' == wch)
                {
                    eState = State_Quote;
                }
                else if ('V' == wch || 'v' == wch)
                {
                    p->mv_value[p->m_cParams] = nextArg();
                    eState = State_Next;
                }
                else if (',' == wch)
                {
                    p->mv_value[p->m_cParams] = nil;
                    p->m_cParams += 1;
                }
                else
                {
                    goto done;
                }
                break;

            case State_Digit:
                if (wch >= '0' && wch <= '9')
                {
                    Val param = p->mv_value[p->m_cParams];
                    param = mul(param, 10);
                    param = add(param, wch - '0');
                    p->mv_value[p->m_cParams] = param;
                }
                else if (',' == wch)
                {
                    p->m_cParams += 1;
                    eState = State_None;
                }
                else
                {
                    p->m_cParams += 1;
                    eState = State_None;
                    goto done;
                }
                break;

            case State_Next:
                eState = State_None;
                unless (',' == wch) goto done;
                p->m_cParams += 1;
                goto done;

            case State_Quote:
                p->mv_value[p->m_cParams] = Character::FromCode(wch);
                p->m_cParams += 1;
                eState = State_Next;
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch
        } // while

      done:
        while (! pEnum->AtEnd())
        {
            char16 wch = pEnum->Get();
            if (':' == wch)
            {
                p->m_fColon = true;
            }
            else if ('@' == wch)
            {
                p->m_fAt = true;
            }
            else
            {
                return;
            }

            pEnum->Next();
        } // while
    } // parseParams

    // [R]
    private: Val run()
    {
        if (nil != m_stream)
        {
            mainLoop();
            return nil;
        }
        else
        {
            m_stream = make_string_output_stream();
            mainLoop();
            return get_output_stream_string(m_stream);
        }
    } // run
}; // Formatter

/// <summary>
///   The common-lisp reader.
/// </summary>
class Reader
{
    public: static Val Run(
        Val     stream,
        bool    fEofError,
        Val     eof_value,
        bool    fRecursive,
        bool    fPreserve )
    {
        Reader oReader(stream, fRecursive, fPreserve);
        return oReader.run(fEofError, eof_value);
    } // Run

    private: struct Token
    {
        enum Type
        {
            Type_None,

            Type_Datum,
            Type_Delimiter,
            Type_Dot,
            Type_Eof,
            Type_Macro,
        }; // Type

        Val     m_value;

        int     m_cwch;
        char16  m_rgwch[100];
        uint32  m_rgn[100];

        // ctor
        Token() :
            m_cwch(0),
            m_value(nil) {}

        // [A]
        void Add(Val ch, Int nAttr)
        {
            if (m_cwch >= lengthof(m_rgwch))
            {
                error(make_string("Too long token"));
            }
            m_rgwch[m_cwch] = ch->StaticCast<Character>()->ToCode();
            m_rgn[m_cwch] = static_cast<uint32>(nAttr);
            m_cwch += 1;
        } // Add

        // [E]
        class EnumAttr
        {
            private: uint32* m_pn;
            private: uint32* m_pnEnd;

            public: EnumAttr(Token* p) :
                m_pn(p->m_rgn),
                m_pnEnd(p->m_rgn + p->m_cwch) {}

            public: bool   AtEnd() const { return m_pn == m_pnEnd; }
            public: uint32 Get() const { ASSERT(!AtEnd()); return *m_pn; }
            public: void   Next() { ASSERT(!AtEnd()); m_pn++; }
        }; // EnumAttr

        class EnumChar
        {
            private: char16*        m_pwch;
            private: char16*        m_pwchEnd;
            private: const Token*   m_p;

            public: EnumChar(Token* p) :
                m_p(p),
                m_pwch(p->m_rgwch),
                m_pwchEnd(p->m_rgwch + p->m_cwch) {}

            public: bool   AtEnd() const { return m_pwch == m_pwchEnd; }
            public: char16 Get() const { ASSERT(!AtEnd()); return *m_pwch; }

            public: uint32 GetAttr() const
            {
                ASSERT(!AtEnd());
                return m_p->m_rgn[m_pwch - m_p->m_rgwch];
            } // GetAttr

            public: void   Next() { ASSERT(!AtEnd()); m_pwch++; }
        }; // EnumChar

        // [G]
        Val Get() const
        {
            return make_string(m_rgwch, m_cwch);
        } // Get
    }; // Token

    private: bool  m_fPreserve;
    private: bool  m_fRecursive;
    private: Token m_oToken;

    private: Val m_stream;

    // ctor
    private: Reader(
        Val     stream,
        bool    fRecursive,
        bool    fPreserve ) :
            m_fPreserve(fPreserve),
            m_fRecursive(fRecursive),
            m_stream(stream) {}

    // [C]
    private: static Val convertToFloat(
        Val const r,
        Val const type )
    {
        if (Qdouble_float == type)
        {
            return Float64Impl::Make(BignumImpl::ToFloat64(r));
        }

        if (Qsingle_float == type)
        {
            return Float32Impl::Make(BignumImpl::ToFloat32(r));
        }

        error(r, Qrational);
    } // chMarker

    // [E]
    private: bool evalFeatureExpr(Val const expr)
    {
        if (symbolp(expr))
        {
            return nil != memq(expr, TLV(AfeaturesA));
        }

        if (consp(expr))
        {
            if (car(expr) == Qand)
            {
                foreach (List::Enum, oEnum, cdr(expr))
                {
                    if (! evalFeatureExpr(oEnum.Get()))
                    {
                        return false;
                    }
                }
                return true;
            } // if and

            if (car(expr) == Qnot)
            {
                return ! evalFeatureExpr(cadr(expr));
            }

            if (car(expr) == Qor)
            {
                foreach (List::Enum, oEnum, cdr(expr))
                {
                    if (evalFeatureExpr(oEnum.Get()))
                    {
                        return true;
                    }
                }
                return false;
            } // if or
        }

        readerError("Invalid feature expression: ~S", expr);
    } // evalFeatureExpr

    private: static Val expt10(Val k)
    {
        ASSERT(ge(k, zero));

        Val result = one;
        Val base = Fixnum::Encode(10);

        while (gt(k, zero))
        {
            if (oddp(k))
            {
                result = mul(result, base);
                k = sub(k, one);
            }
            else
            {
                base = mul(base, base);
                k = ash(k, Fixnum::Encode(-1));
            }
        } // while

        return result;
    } // expt10

    private: void extraInfixArg(
        Val const dispch,
        Val const subch,
        Val const arg )
    {
        if (isSuppress())
        {
            // If we are in suppress mode, we don't signal error.
            return;
        }

        if (nil != arg)
        {
            readerError("~C~C doesn't accept infix argument: ~S",
                dispch, subch, arg );
        }
    } // extraInfixArg

    // [I]
    private: static bool isSuppress()
        { return nil != TLV(Aread_suppressA); }

    // [O]
    private: static bool oddp(Val const n)
    {
        if (fixnump(n))
        {
            return 0 != (Fixnum::Decode_(n) & 1);
        }

        if (BignumImpl* p = n->DynamicCast<BignumImpl>())
        {
            return 0 != (p->GetLsb() & 1);
        }

        SignalTypeError(n, Qinteger);
    } // oddp

    // [P]
    private: Token::Type process(
        Val     const delimiter,
        bool    const fEofError,
        Val     const eof_value )
    {
      try_again:
        Token::Type eType = readToken(delimiter);
        switch (eType)
        {
        case Token::Type_Delimiter:
            break;

        case Token::Type_Dot:
            unless (characterp(delimiter)) readerErrorDot();
            break;

        case Token::Type_Eof:
            if (fEofError)
            {
                readerErrorEof();
            }

            m_oToken.m_value = eof_value;
            eType = Token::Type_Datum;
            break;

        case Token::Type_Macro:
            unless (processMacroChar(m_oToken.m_value)) goto try_again;
            break;

        case Token::Type_None:
            break;

        case Token::Type_Datum:
            if (isSuppress())
            {
                return Token::Type_None;
            }

            eType = processToken();
            if (Token::Type_Dot == eType)
            {
                unless (characterp(delimiter)) readerErrorDot();
            }
            break;
        default:
            CAN_NOT_HAPPEN();
        } // switch eType
        return eType;
    } // process

    private: Val processDoubleQuote(Val const delimiter)
    {
        CharSink_<60> oSink;

        enum State
        {
            State_None,
            State_Escape,
        } eState = State_None;

        for (;;)
        {
            Val ch = read_char(m_stream);
            switch (eState)
            {
            case State_None:
                if (delimiter == ch)
                {
                    return make_string(oSink.GetStart(), oSink.GetLength());
                }

                if (Character::FromCode(Backslash) == ch)
                {
                    eState = State_Escape;
                }
                else
                {
                    oSink.Add(ch->StaticCast<Character>()->ToCode());
                }
                break;

            case State_Escape:
                oSink.Add(ch->StaticCast<Character>()->ToCode());
                eState = State_None;
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch state
        } // for
    } // processDoubleQuote

    private: bool processMacroChar(Val ch)
    {
        switch (ch->StaticCast<Character>()->ToCode())
        {
        case DoubleQuote:
            m_oToken.m_value = processDoubleQuote(ch);
            break;

        case '#':
            return processSharp(ch);

        case Quote:
            m_oToken.m_value = list(Qquote, read(m_stream, t, nil, t));
            break;

        case OpenParen:
            m_oToken.m_value = processOpenParen(
                Character::FromCode(CloseParen) );
            break;

        case CloseParen:
            // Eat all extra right parenthesis
            for (;;)
            {
                Val ch = read_char(m_stream, nil);
                if (Character::FromCode(CloseParen) == ch) break;
            }

            unless (isSuppress())
                { readerError("Too many close parenthesis."); }

            m_oToken.m_value = nil;
            break;

        case ',':
        {
            if (zero == TLV(Aread_backquoteA))
                { readerError("Comma(,) used out side backquote."); }
            Val ch = read_char(m_stream);
            Val quote = Qunquote;
            switch (ch->StaticCast<Character>()->ToCode())
            {
            case '@':
                quote = Qunquote_splicing;
                break;
            case '.':
                quote = Qunquote_nsplicing;
                break;
            default:
                unread_char(ch, m_stream);
                break;
            } // switch ch

            BindFrameScope_<1> oLet;
            oLet.Bind(
                TLV_Aread_backquoteA,
                sub(TLV(Aread_backquoteA), 1) );

            m_oToken.m_value = list(quote, read(m_stream, t, nil, t));

            break;
        } // comma

        case Backquote:
        {
            BindFrameScope_<1> oLet;
            oLet.Bind(
                TLV_Aread_backquoteA,
                add(TLV(Aread_backquoteA), 1) );

            m_oToken.m_value = list(Qbackquote, read(m_stream, t, nil, t));
            break;
        } // Backquote

        case SemiColon:
            for (;;)
            {
                Val ch = read_char(m_stream, nil, CHAR_Newline);
                if (CHAR_Newline == ch) break;
            } // for
            return false;

        default:
            readerError("Undefined macro character ~S", ch);
        } // switch ch

        return true;
    } // processMacroChar

    private: Val processOpenParen(Val delimiter)
    {
        BindFrameScope_<1> oLet;
        oLet.Bind(
            TLV_Aread_start_line_numberA,
            funcall(Qstream_line_number, m_stream) );

        Val list = processOpenParenAux(delimiter);

        if (nil != TLV(Aread_line_number_tableA) &&
            nil != TLV(Aread_start_line_numberA) )
        {
            setf_gethash(
                TLV(Aread_start_line_numberA),
                list,
                TLV(Aread_line_number_tableA) );
        }

        return list;
    } // processOpenParen

    private: Val processOpenParenAux(Val const delimiter)
    {
        Val const head = list(nil);
        Val tail = head;

        enum State
        {
            State_Car,
            State_List,
            State_Cdr,
            State_Last,
        } eState = State_Car;

        for (;;)
        {
            Token::Type const eType = process(delimiter, true, nil);
            switch (eState)
            {
            finish:
                return cdr(head);

            cons:
            {
                Val kons = list(m_oToken.m_value);
                tail = setf_cdr(kons, tail);
                break;
            } // cons

            case State_Car:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    goto finish;
                case Token::Type_Dot:
                    readerError("No object before dot.");
                default:
                    eState = State_List;
                    goto cons;
                } // switch token

            case State_List:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    goto finish;
                case Token::Type_Dot:
                    eState = State_Cdr;
                    break;
                default:
                    goto cons;
                } // switch token
                break;

            case State_Cdr:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    readerError("Missing object afer dot.");
                case Token::Type_Dot:
                    readerError("Dot after dot.");
                default:
                    setf_cdr(m_oToken.m_value, tail);
                    eState = State_Last;
                    break;
                } // switch token
                break;

            case State_Last:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    goto finish;
                case Token::Type_Dot:
                    readerError("Dot after object.");
                default:
                    readerError("Expect right parenthesis.");
                } // switch token

            default:
                CAN_NOT_HAPPEN();
            } // switch
        } // for
    } // processOpenParenAux

    /// <summary>
    ///   <list>
    ///     <item><term>1</term>
    ///       <description>
    ///         Collects digits as argument.
    ///       </description>
    ///     </item>
    ///     <item><term>2</term>
    ///       <description>
    ///         Dispatch by subchar after argument.
    ///       </description>
    ///     </item>
    ///     <item><term>2</term>
    ///       <description>
    ///         Returns true if read object, or false if read nothing.
    ///       </description>
    ///     </item>
    ///   </list>
    /// </summary>
    /// <param name="dispch">A dispatch character, e.g. #</param>
    private: bool processSharp(Val const dispch)
    {
        Val arg = zero;
        Val ch = read_char(m_stream);
        {
            int cDigits = 0;
            for (;;)
            {
                Val digit = digit_char_p(ch);
                if (nil == digit)
                {
                    if (0 == cDigits) arg = nil;
                    break;
                }

                arg = mul(arg, 10);
                arg = add(arg, digit);
            } // for
        }

        switch (ch->StaticCast<Character>()->ToCode())
        {
        case '(':
            m_oToken.m_value = processSharpParen(arg);
            return true;

        case '-': case '+':
            extraInfixArg(dispch, ch, arg);
            return processSharpPlus(ch);

        case '.':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpDot(ch);
            return true;

        case ':':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpColon();
            return true;

        case 'B': case 'b':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpR(ch, Fixnum::Encode(2));
            return true;

        case 'C': case 'c':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpC(ch);
            return true;

        case 'O': case 'o':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpR(ch, Fixnum::Encode(8));
            return true;

        case 'P': case 'p':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpP(ch);
            return true;

        case 'R': case 'r':
            m_oToken.m_value = processSharpR(ch, arg);
            return true;

        case 'X': case 'x':
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpR(ch, Fixnum::Encode(16));
            return true;

        case Backslash:
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpBackslash(ch);
            return true;

        case Quote:
            extraInfixArg(dispch, ch, arg);
            m_oToken.m_value = processSharpQuote(ch);
            return true;

        case '|':
            extraInfixArg(dispch, ch, arg);
            processSharpBar();
            return false;

        default:
            readerError("Unsupported sharp subchar ~S.", ch);
        } // switch char
    } // processSharp

    private: Val processSharpBackslash(Val const subchar)
    {
        unread_char(subchar, m_stream);

        if (isSuppress())
        {
            readToken(nil, false);
            return nil;
        }

        readStringToken();

        if (1 == m_oToken.m_cwch)
        {
            return Character::FromCode(m_oToken.m_rgwch[0]);
        }

        Val name = m_oToken.Get();
        Val ch = name_char(name);
        if (nil == ch)
        {
            readerError("No such character ~S.", name);
        }

        return ch;
    } // processSharpBackslash

    private: void processSharpBar()
    {
        enum
        {
            State_Bar,
            State_Normal,
            State_Sharp,
        } eState = State_Normal;

        uint nLevel = 1;

        for (;;)
        {
            Val const ch = read_char(m_stream);

           tryAgain:
            switch (eState)
            {
            case State_Bar:
                eState = State_Normal;
                if (Character::FromCode('#') == ch)
                {
                    nLevel -= 1;
                    if (0 == nLevel)
                    {
                        return;
                    }
                    break;
                }
                goto tryAgain;

            case State_Normal:
                if (Character::FromCode('#') == ch)
                {
                    eState = State_Sharp;
                }
                else if (Character::FromCode('|') == ch)
                {
                    eState = State_Bar;
                }
                break;

            case State_Sharp:
                eState = State_Normal;
                if (Character::FromCode('|') == ch)
                {
                    nLevel += 1;
                    break;
                }
                goto tryAgain;

            default:
                CAN_NOT_HAPPEN();
            } // switch state
        } // for
    } // processSharpBar

    private: Val processSharpColon()
    {
        if (isSuppress())
        {
            readToken(nil, false);
            return nil;
        }

        readStringToken();

        foreach (Token::EnumAttr, oEnum, &m_oToken)
        {
            if (oEnum.Get() & CharTrait_Package)
            {
                readerErrorColonInSymbol();
            }
        } // for attr

        return make_symbol(m_oToken.Get());
    } // processSharpColon

    private: Val processSharpDot(Val)
    {
        Val const form = read(m_stream, t, nil, t);
        return isSuppress() ? nil : eval(form);
    } // processSharpDot

    private: bool processSharpPlus(Val const subch)
    {
        if (isSuppress())
        {
            read(m_stream, t, nil, t);   // condition
            read(m_stream, t, nil, t);  // form
            return false;
        }

        Val cond;
        {
            BindFrameScope_<1> oLet;
            oLet.Bind(TLV_ApackageA, PKG_keyword);
            cond = read(m_stream, t, nil, t);
        }

        if (evalFeatureExpr(cond) == (Character::FromCode('+') == subch))
        {
            m_oToken.m_value = read(m_stream, t, nil, t);
            return true;
        }
        else
        {
            BindFrameScope_<1> oLet;
            oLet.Bind(TLV_Aread_suppressA, t);
            read(m_stream, t, nil, t);
            return false;
        }
    } // processSharpPlus

    private: Val processSharpQuote(Val)
    {
        Val const form = read(m_stream, t, nil, t);
        return isSuppress() ? nil : list(Qfunction, form);
    } // processSharpQuote


    private: Val processSharpC(Val const subch)
    {
        Val const args = read(m_stream, t, nil, t);
        if (isSuppress())
        {
            return nil;
        }

        if (! consp(args))
        {
            readerError("#~C requires list.", subch);
        }
        return apply(Qcomplex, args);
    } // processSharpC

    private: Val processSharpP(Val const subch)
    {
        if (isSuppress())
        {
            readToken(nil, false);
            return nil;
        }

        Val const str = read(m_stream, t, nil, t);
        if (! stringp(str))
        {
            readerError("#~C requires string.", subch);
        }
        return pathname(str);
    } // processSharpP

    private: Val processSharpParen(Val const arg)
    {
        Val const elts = processOpenParen(Character::FromCode(CloseParen));
        Val const nelts = length(elts);
        Val const len = nil == arg ? nelts : arg;
        Val const vec = make_vector(len);
        Val* const pxStart = vec->StaticCast<SimpleVector>()->GetStart();
        Val* px = pxStart;
        foreach (List::Enum, oEnum, elts)
        {
            *px++ = oEnum.Get();
        } // for

        Val const last = car(elts);
        Val* pxEnd = pxStart + Fixnum::Decode_(len);
        while (px < pxEnd)
        {
            *px++ = last;
        } // while

        return vec;
    } // processSharpParen

    private: Val processSharpR(Val subch, Val base)
    {
        if (isSuppress())
        {
            readToken(nil);
            return nil;
        }

        if (nil == base)
        {
            readerError("#~C requires base between # and ~C.",
                subch, base );
        }

        unless (base >= Fixnum::Encode(2) && base <= Fixnum::Encode(36))
        {
            readerError("#~C requires 2 to 36.", subch);
        }

        readToken(nil);

        Val num = processTokenAsNumber(base);
        if (nil == num)
        {
            readerError("Number syntax error: ~S", m_oToken.Get());
        }
        return num;
    } // processSharpR

    private: Token::Type processToken()
    {
        ASSERT(m_oToken.m_cwch > 0);

        if (processTokenAsDot())
        {
            return Token::Type_Dot;
        }

        m_oToken.m_value = processTokenAsNumber(TLV(Aread_baseA));
        if (nil == m_oToken.m_value)
        {
            m_oToken.m_value = processTokenAsSymbol();
        }
        return Token::Type_Datum;
    } // processToken

    private: bool processTokenAsDot()
    {
        int cDots = 0;
        foreach (Token::EnumAttr, oEnum, &m_oToken)
        {
            if (oEnum.Get() & CharTrait_Dot)
            {
                cDots += 1;
            }
            else
            {
                return false;
            }
        } // for each attr

        if (1 != cDots)
        {
            error("Can't use multiple dots");
        }

        return true;
    } // processTokenAsDot

    private: Val processTokenAsFloat()
    {
        enum State
        {
            State_AfterDot,
            State_Digit,
            State_Expt,
            State_ExptDigit,
            State_ExptDigitMust,
            State_FraDigit,
            State_Sign,
            State_Start,
        } eState = State_Start;

        Val const base = Fixnum::Encode(10);

        Val inum  = zero;
        Val expt  = zero;
        Val scale = zero;
        Val floty = TLV(Aread_default_float_formatA);
        bool fExptMinus = false;
        bool fMinus = false;
        foreach (Token::EnumChar, oEnum, &m_oToken)
        {
            char16 wch = oEnum.Get();
            Val ch = Character::FromCode(wch);

          tryAgain:
            switch (eState)
            {
            mustBeDigit:
            {
                inum = digit_char_p(ch, base);
                if (nil == inum) return nil;
                eState = State_Digit;
                break;
            } // switch char

            case State_AfterDot:
            {
                Val digit = digit_char_p(ch, base);
                if (nil == digit) return nil;
                inum = mul(inum, base);
                inum = add(inum, digit);
                scale = add(scale, one);
                eState = State_FraDigit;
                break;
            } // State_AfterDot

            case State_Digit:
            {
                switch (wch)
                {
                case '.':
                    eState = State_AfterDot;
                    break;

                case 'D':
                case 'd':
                case 'L':
                case 'l':
                    floty = Qdouble_float;
                    eState = State_Expt;
                    break;

                case 'E':
                case 'e':
                    eState = State_Expt;
                    break;

                case 'F':
                case 'f':
                case 'S':
                case 's':
                    floty = Qsingle_float;
                    eState = State_Expt;
                    break;

                default:
                {
                    Val digit = digit_char_p(ch, base);
                    if (nil == digit) return nil;
                    inum = mul(inum, base);
                    inum = add(inum, digit);
                    break;
                }
                } // switch wch
                break;
            } // State_Digit

            case State_Expt:
                switch (wch)
                {
                case '+':
                    eState = State_ExptDigitMust;
                    break;

                case '-':
                    fExptMinus = true;
                    eState = State_ExptDigitMust;
                    break;

                default:
                    eState = State_ExptDigit;
                    goto tryAgain;
                } // switch wch
                break;

            case State_ExptDigit:
            case State_ExptDigitMust:
            {
                Val digit = digit_char_p(ch, base);
                if (nil == digit) return nil;
                expt = mul(expt, base);
                expt = add(expt, digit);
                eState = State_ExptDigit;
                break;
            } // State_ExptDigit

            case State_FraDigit:
            {
                switch (wch)
                {
                case 'D':
                case 'd':
                case 'L':
                case 'l':
                    floty = Qdouble_float;
                    eState = State_Expt;
                    break;

                case 'E':
                case 'e':
                    eState = State_Expt;
                    break;

                case 'F':
                case 'f':
                case 'S':
                case 's':
                    floty = Qsingle_float;
                    eState = State_Expt;
                    break;

                default:
                {
                    Val digit = digit_char_p(ch, base);
                    if (nil == digit) return nil;
                    inum  = mul(inum, base);
                    inum  = add(inum, digit);
                    scale = add(scale, one);
                }
                } // switch wch
                break;
            } // State_FraDigit

            case State_Sign:
                goto mustBeDigit;

            case State_Start:
                switch (wch)
                {
                case '-':
                    fMinus = true;
                    // FALLTHROUGH
                case '+':
                    eState = State_Sign;
                    break;

                default:
                    goto mustBeDigit;
                } // switch wch
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch state
        } // for each char

        Val fnum = nil;

        switch (eState)
        {
        case State_AfterDot:
            fnum = inum;
            break;

        case State_ExptDigit:
        {
            Val const flo = convertToFloat(
                div(inum, expt10(scale)),
                floty );

            Val const e = expt10(expt);

            fnum = fExptMinus ? div(flo, e) : mul(flo, e);
            break;
        } // State_ExptDigit

        case State_FraDigit:
            fnum = convertToFloat(
                div(inum, expt10(scale)),
                floty );
            break;

        case State_Digit:
            fnum = inum;
            break;

        default:
            return nil;
        } // switch eState

        return fMinus ? sub_1(fnum) : fnum;
    } // processTokenAsFloat

    private: Val processTokenAsNumber(Val base)
    {
        Val inum = processTokenAsRational(base);
        if (nil != inum)
        {
            return inum;
        }

        return processTokenAsFloat();
    } // processTokenAsNumber

    private: Val processTokenAsRational(Val base)
    {
        enum State
        {
            State_DenDigit,
            State_Digit,
            State_Sign,
            State_Start,
        } eState = State_Start;

        Val inum = zero;
        Val iden = zero;
        bool fMinus = false;
        foreach (Token::EnumChar, oEnum, &m_oToken)
        {
            char16 wch = oEnum.Get();
            Val ch = Character::FromCode(wch);
            switch (eState)
            {
            mustBeDigit:
            {
                inum = digit_char_p(ch, base);
                if (nil == inum) return nil;
                eState = State_Digit;
                break;
            } // switch char

            case State_DenDigit:
            {
                Val digit = digit_char_p(ch, base);
                if (nil == digit) return nil;
                iden = mul(iden, base);
                iden = add(iden, digit);
                break;
            } // State_DenDigit

            case State_Digit:
            {
                if ('/' == wch)
                {
                    eState = State_DenDigit;
                    break;
                }

                Val digit = digit_char_p(ch, base);
                if (nil == digit) return nil;
                inum = mul(inum, base);
                inum = add(inum, digit);
                break;
            } // State_Digit

            case State_Sign:
                goto mustBeDigit;

            case State_Start:
                switch (wch)
                {
                case '-':
                    fMinus = true;
                    // FALLTHROUGH
                case '+':
                    eState = State_Sign;
                    break;

                default:
                    goto mustBeDigit;
                } // switch wch
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch state
        } // for each char

        if (fMinus)
        {
            inum = sub(zero, inum);
        }

        switch (eState)
        {
        case State_DenDigit:
            ASSERT(zero != iden);
            return div(inum, iden);

        case State_Digit:
            return inum;
        } // switch eState

        return nil;
    } // processTokenAsRational

    private: Val processTokenAsSymbol()
    {
        int cColons = 0;
        int cwchPackage = 0;
        enum State
        {
            State_Start,

            State_Colon,
            State_PackageOrSymbol,
            State_Symbol,
        } eState = State_Start;

        foreach (Token::EnumAttr, oEnum, &m_oToken)
        {
            switch (eState)
            {
            case State_Start:
                if (oEnum.Get() & CharTrait_Package)
                {
                    eState = State_Colon;
                    cColons = 1;
                }
                else
                {
                    cwchPackage = 1;
                    eState = State_PackageOrSymbol;
                }
                break;

            case State_Colon:
                if (oEnum.Get() & CharTrait_Package)
                {
                    cColons += 1;
                }
                else
                {
                    eState = State_Symbol;
                }
                break;

            case State_PackageOrSymbol:
                if (oEnum.Get() & CharTrait_Package)
                {
                    eState = State_Colon;
                    cColons = 1;
                }
                else
                {
                    cwchPackage += 1;
                }
                break;

            case State_Symbol:
                if (oEnum.Get() & CharTrait_Package)
                {
                    readerErrorColonInSymbol();
                }
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch state
        } // for each char

        if (cColons > 2) {
            readerError("Too many colons.");
        }

        if (0 == cColons) cwchPackage = 0;

        int cwchSkip = cwchPackage + cColons;
        StackString_<> oName(
            m_oToken.m_rgwch + cwchSkip,
            m_oToken.m_cwch - cwchSkip );

        // :keyword => ::keyword
        if (1 == cColons && 0 == cwchPackage)
        {
            cColons = 2;
        }

        Val pkg;
        switch (cColons)
        {
        case 0:
            return intern(oName);

        case 1:
        {
            StackString_<> oPkgName(m_oToken.m_rgwch, cwchPackage);
            pkg = find_package(oPkgName);

            if (nil == pkg)
            {
                readerError("No such pakage ~S", oPkgName);
            }

            Val status;
            Val sym = find_symbol(oName, pkg, &status);

            if (nil == status)
            {
                readerError("No such symbol ~S in ~S.", oName, pkg);
            }

            if (Kinherited == status || Kinternal == status)
            {
                readerError("Can't access internal symbol ~S in ~S.",sym, pkg);
            }
            return sym;
        } // 1

        case 2:
        {
            if (0 == cwchPackage)
            {
                return intern(oName, PKG_keyword);
            }
            else
            {
                StackString_<> oPkgName(m_oToken.m_rgwch, cwchPackage);
                pkg = find_package(oPkgName);

                if (nil == pkg)
                {
                    readerError("No such pakage ~S", oPkgName);
                }
                return intern(oName, pkg);
            }
        } // 2

        default:
            CAN_NOT_HAPPEN();
        } // switch cColons
    } // processTokenAsSymbol

    // [R]
    private: Val readCharWithAttr(Int* out_nAttr)
    {
        Val ch = read_char(m_stream, nil);
        if (nil == ch) return nil;

        Character* p = ch->StaticCast<Character>();
        char16 wch = p->ToCode();
        if (p->m_data->ToInt() & Character::Attr_Whitespace)
        {
            *out_nAttr = CharType_Space;
        }
        else if (Backquote   == wch ||
                 CloseParen  == wch ||
                 DoubleQuote == wch ||
                 OpenParen   == wch ||
                 Quote       == wch ||
                 ','         == wch ||
                 ';'         == wch )
        {
            *out_nAttr = CharType_Tmacro;
        }
        else if ('#' == wch)
        {
            *out_nAttr = CharType_Nmacro;
        }
        else if ('.' == wch)
        {
            *out_nAttr =
                CharType_Cons |
                CharTrait_Alphabetic |
                CharTrait_Decimal |
                CharTrait_Dot;
        }
        else if (':' == wch)
        {
            *out_nAttr = CharType_Cons | CharTrait_Package;
        }
        else if (Backslash == wch)
        {
            *out_nAttr = CharType_Sescape;
        }
        else if ('|' == wch)
        {
            *out_nAttr = CharType_Mescape;
        }
        else if (wch < 0x20)
        {
            *out_nAttr = CharType_Invalid;
        }
        else
        {
            *out_nAttr = CharType_Cons;
        }
        return ch;
    } // readCharWithAttr

    private: void NoReturn readerError(const char* psz)
    {
        error(Qsimple_reader_error,
            Kformat_control, make_string(psz),
            Kstream, m_stream );
    } // readerError

    private: void NoReturn readerError(const char* psz, Val a)
    {
        error(Qsimple_reader_error,
            Kformat_arguments, list(a),
            Kformat_control, make_string(psz),
            Kstream, m_stream );
    } // readerError

    private: void NoReturn readerError(const char* psz, Val a, Val b)
    {
        error(Qsimple_reader_error,
            Kformat_arguments, list(a, b),
            Kformat_control, make_string(psz),
            Kstream, m_stream );
    } // readerError

    private: void NoReturn readerError(
        const char* psz, Val a, Val b, Val c )
    {
        error(Qsimple_reader_error,
            Kformat_arguments, list(a, b, c),
            Kformat_control, make_string(psz),
            Kstream, m_stream );
    } // readerError

    private: void NoReturn readerErrorColonInSymbol()
        { readerError("Can't use package marker in symbol."); }

    private: void NoReturn readerErrorDot()
        { readerError("Use dot outside parenthesis."); }

    private: void NoReturn readerErrorEof()
    {
        error(Qreader_eof_error,
            Kstart,     TLV(Aread_start_line_numberA),
            Kstream,    m_stream );
    } // readerErrorEof

    private: void NoReturn readerErrorInvalidChar(Val)
        { readerError("Invalid character"); }

    private: void readStringToken()
    {
        switch (Token::Type eToken = readToken(nil))
        {
        case Token::Type_Datum:
            return;

        case Token::Type_Eof:
            readerErrorEof();

        case Token::Type_Macro:
            readerError("Can't use symbol ~S", m_oToken.Get());

        default:
            readerError("Internal error");
        } // switch eToken
    } // readStringToken

    private: Token::Type readToken(
        Val const   delimiter,
        bool        fDiscard = false )
    {
        m_oToken.m_cwch = 0;

        Val ch;
        Int nAttr;

      step1:
        {
            ch = readCharWithAttr(&nAttr);
            unless (characterp(ch)) return Token::Type_Eof;
            if (delimiter == ch)  return Token::Type_Delimiter;
            goto step2;
        } // step1

      step2:
        {
            switch (nAttr & CharType_Mask)
            {
            case CharType_Cons:
                goto step7;
            case CharType_Nmacro:
            case CharType_Tmacro:
                m_oToken.m_value = ch;
                return Token::Type_Macro;
            case CharType_Space:
                goto step1;
            case CharType_Sescape:
                ch = read_char(m_stream);
                m_oToken.Add(ch, CharType_Cons | CharTrait_Alphabetic);
                goto step8;
            case CharType_Mescape:
                goto step9;
            default:
                if (fDiscard)
                {
                    goto step7;
                }
                else
                {
                    readerError("Invalid character");
                }
            } // switch type
        } // step2

      step7:
        {
            Val rt = TLV(AreadtableA);
            Val read_case = Kupcase;
            if (nil != rt)
            {
                read_case = TLV(AreadtableA)->StaticCast<Readtable>()->m_case;
            }

            if (Kupcase == read_case)
            {
                ch = char_upcase(ch);
            }
            else if (Kdowncase == read_case)
            {
                ch = char_downcase(ch);
            }
            else if (Kpreserve == read_case)
            {
                // nothing to do
            }
            else if (! fDiscard)
            {
                readerError("Unsupported readtable-case");
            }

            m_oToken.Add(ch, nAttr);
            goto step8;
        } // step7

      step8:
        {
            ch = readCharWithAttr(&nAttr);
            unless (characterp(ch)) goto step10;

            switch (nAttr & CharType_Mask)
            {
            case CharType_Cons:
                goto step7;
            case CharType_Nmacro:
                goto step7;
            case CharType_Tmacro:
                unread_char(ch, m_stream);
                goto step10;
            case CharType_Space:
                goto step10;
            case CharType_Sescape:
                ch = read_char(m_stream);
                m_oToken.Add(ch, CharType_Cons | CharTrait_Alphabetic);
                goto step8;
            case CharType_Mescape:
                goto step9;
            default:
                if (fDiscard)
                {
                    goto step7;
                }
                else
                {
                    readerErrorInvalidChar(ch);
                }
            } // switch type
        } // step8

      step9:
        {
            ch = readCharWithAttr(&nAttr);
            unless (characterp(ch)) readerErrorEof();
            switch (nAttr & CharType_Mask)
            {
            case CharType_Mescape:
                goto step8;
            case CharType_Sescape:
                ch = read_char(m_stream);
                break;
            case CharType_Invalid:
                if (fDiscard)
                {
                    break;
                }
                else
                {
                    readerErrorInvalidChar(ch);
                }
            } // switch type
            m_oToken.Add(ch, CharType_Cons | CharTrait_Alphabetic);
            goto step9;
        } // step9

      step10:
        return Token::Type_Datum;
    } // readToken

    private: Val run(bool fEofError, Val eof_value)
    {
        process(nil, fEofError, eof_value);
        return m_oToken.m_value;
    } // run
}; // Reader

} // Private

// [E]
Val ensure_input_stream(Val s)
{
    if (nil == s)
    {
        s = TLV(Astandard_inputA);
    }
    else if (t == s)
    {
        s = TLV(Aterminal_ioA);
    }
    check_type(s, stream);
    return s;
} // ensure_input_stream

Val ensure_output_stream(Val s)
{
    if (nil == s)
    {
        s = TLV(Astandard_outputA);
    }
    else if (t == s)
    {
        s = TLV(Aterminal_ioA);
    }
    check_type(s, stream);
    return s;
} // ensure_output_stream

namespace CommonLisp
{

using namespace TinyCl;

// [F]
Val clear_input(Val stream)
{
    stream = ensure_input_stream(stream);
    return funcall(Qstream_clear_input, stream);
} // clear_input

Val clear_output(Val stream)
{
    stream = ensure_output_stream(stream);
    return funcall(Qstream_clear_output, stream);
} // clear_output

Val finish_output(Val stream)
{
    stream = ensure_output_stream(stream);
    return funcall(Qstream_finish_output, stream);
} // finish_output

Val force_output(Val stream)
{
    stream = ensure_output_stream(stream);
    return funcall(Qstream_force_output, stream);
} // force_output

Val format(Val s, const char* psz)
{
    StackString_<> oControl(psz);
    values(s, oControl);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz, Val a)
{
    StackString_<> oControl(psz);
    values(s, oControl, a);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz, Val a, Val b)
{
    StackString_<> oControl(psz);
    values(s, oControl, a, b);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz, Val a, Val b, Val c)
{
    StackString_<> oControl(psz);
    values(s, oControl, a, b, c);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz, Val a, Val b, Val c, Val d)
{
    StackString_<> oControl(psz);
    values(s, oControl, a, b, c, d);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz, Val a, Val b, Val c, Val d, Val e)
{
    StackString_<> oControl(psz);
    values(s, oControl, a, b, c, d, e);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz,
           Val a, Val b, Val c, Val d, Val e, Val f)
{
    StackString_<> oControl(psz);
    values(s, oControl, a, b, c, d, e, f);
    return formatV(Thread::Get());
} // format

Val format(Val s, const char* psz,
           Val a, Val b, Val c, Val d, Val e, Val f, Val g )
{
    StackString_<> oControl(psz);
    values(s, oControl, a, b, c, d, e, f, g);
    return formatV(Thread::Get());
} // format

// formatV - entry point for lisp
defun(formatV, (Thread*))
    { return Formatter::Run(); }

// [P]
Val print(Val obj, Val stream)
{
    stream = ensureOutputStream(stream);
    write_char(Newline, stream);
    print_object(obj, stream);
    write_char(Space, stream);
    return obj;
} // print

defmethod(print_object, basic_host, (Val x, Val s))
{
    format(s, "#<~S ~S>",
        class_name(class_of(x)),
        x->StaticCast<Record>()->To<Layout_basic_host>()->m_name );
    return x;
} // print_object basic_host

defmethod(print_object, bignum, (Val x, Val s))
{
    class BignumPrinter
    {
        private: static void printBignum(
            Val const x,
            Val const s,
            Val const baze )
        {
            static Val s_divisor[37];
            static int s_rgnDigits[37];

            if (0 == s_rgnDigits[2])
            {
                for (int iBase = 2; iBase <= 36; iBase += 1)
                {
                    int iPower1 = -1;
                    int64 iNewDiv = iBase;
                    int iDiv = 1;
                    while (iNewDiv < ((1 << 30) - 1))
                    {
                        s_divisor[iBase] = Fixnum::Encode(iDiv);
                        s_rgnDigits[iBase] = iPower1 + 1;

                        iPower1 += 1;
                        iDiv = static_cast<int>(iNewDiv);
                        iNewDiv = iNewDiv * iBase;
                    }
                } // for iBase
            } // if

            Val q, r;
            truncate(x, s_divisor[Fixnum::Decode_(baze)], &q, &r);

            if (fixnump(q))
            {
                printFixnum(q, s, baze);
            }
            else
            {
                printBignum(q, s, baze);
            }

            int cZeros = s_rgnDigits[Fixnum::Decode_(baze)] - 1;
            Val base_power = baze;
            while (le(base_power, r))
            {
                cZeros -= 1;
                base_power = mul(base_power, baze);
            } // while

            while (cZeros > 0)
            {
                write_char('0', s);
                cZeros -= 1;
            } // while

            printFixnum(r, s, baze);
        } // printBignum

        private: static void printFixnum(
            Val const x,
            Val const s,
            Val const baze )
        {
            Val q, r;
            truncate(x, baze, &q, &r);
            if (gt(q, zero)) printFixnum(q, s, baze);
            write_char(digit_char(r, baze), s);
        } // printFixnum

        public: static void Run(Val x, Val const s)
        {
            Val baze = TLV(Aprint_baseA);
            if (! fixnump(baze) ||
                lt(baze, two) ||
                gt(baze, Fixnum::Encode(36)) )
            {
                baze = Fixnum::Encode(10);
            }

            if (lt(x, zero))
            {
                write_char('-', s);
                x = sub(0, x);
            }

            if (fixnump(x))
            {
                printFixnum(x, s, baze);
            }
            else
            {
                printBignum(x, s, baze);
            }

            if (Fixnum::Encode(10) == baze &&
                nil != TLV(Aprint_radixA) )
            {
                write_char('.', s);
            }
        } // Run
    }; // BignumPrinter

    BignumPrinter::Run(x, s);
    return x;
} // print_object bignum

defmethod(print_object, character, (Val x, Val s))
{
    if (nil == TLV(Aprint_escapeA))
    {
        write_char(x, s);
        return x;
    }

    Val name = char_name(x);
    if (nil != name)
    {
        format(s, "#\\~A", name);
    }
    else
    {
        Character* p = x->DynamicCast<Character>();
        char16 wch = p->ToCode();
        if (wch >= 0x21 && wch <= 0x7E)
        {
            format(s, "#\\~C", x);
        }
        else
        {
            format(s, "#\\u~4,'0X", Fixnum::Encode(wch));
        }
    }
    return x;
} // print_object character

defmethod(print_object, charset, (Val x, Val s))
{
    Charset* p = x->StaticCast<Charset>();

    format(s, "#<Charset ~A ~D>",
        p->m_name,
        p->m_code_page );

    return x;
} // print_object charset

defmethod(print_object, class, (Val x, Val s))
{
    Layout_class* pClass = x->StaticCast<Instance>()->
        GetStorage<Layout_class>();

    if (nil == pClass->m_name)
    {
        format(s, "#<~S @~X~X>",
            type_of(x),
            Fixnum::Encode(x->ToInt() >> 4),
            Fixnum::Encode(x->ToInt() & 15) );
    }
    else
    {
        format(s, "#<~S ~S>", type_of(x), pClass->m_name);
    }
    return x;
} // print_object class

defmethod(print_object, class_description, (Val x, Val s))
{
    ClassD* p = x->DynamicCast<ClassD>();

    if (nil == p->m_class)
    {
        format(s, "#<Class-Description @~X~X>",
            Fixnum::Encode(x->ToInt() >> 4),
            Fixnum::Encode(x->ToInt() & 15) );
    }
    else
    {
        format(s, "#<Class-Description ~S>",
            class_name(p->m_class) );
    }
    return x;
} // print_object classd

defmethod(print_object, cons, (Val x, Val s))
{
    Val delimiter = Character::FromCode(Character::OpenParen);
    do
    {
        write_char(delimiter , s);
        print_object(car(x), s);
        delimiter = Character::FromCode(' ');
        x = cdr(x);
    } while (consp(x));
    if (nil != x)
    {
        write_string(" . ", s);
        print_object(x, s);
    }
    write_char(Character::FromCode(Character::CloseParen), s);
    return x;
} // print_object cons

defmethod(print_object, double_float, (Val x, Val s))
{
    FloatPrinter_<Float64Impl, uint64>::Run(
        x->StaticCast<Float64Impl>(),
        s );
    return x;
} // print_object double_float

defmethod(print_object, double_float_complex, (Val x, Val s))
{
    Float64ComplexImpl* const p = x->StaticCast<Float64ComplexImpl>();
    StackFloat64 oReal(p->m_dblReal);
    StackFloat64 oImag(p->m_dblImag);
    format(s, "#c(~S ~S)", oReal.Encode(), oImag.Encode());
    return x;
} // print_object double_float_complex

defmethod(print_object, external_format, (Val x, Val s))
{
    ExternalFormat* p = x->StaticCast<ExternalFormat>();

    if (Charset* q = p->m_charset->DynamicCast<Charset>())
    {
        format(s, "#<External-Format ~A ~A>",
            q->m_name,
            p->m_eol );
    }
    else
    {
        format(s, "#<External-Format @~X~X>",
            Fixnum::Encode(x->ToInt() >> 4),
            Fixnum::Encode(x->ToInt() & 15) );
    } // if

    return x;
} // print_object external_format

defmethod(print_object, fixnum, (Val x, Val s))
{
    class FixnumPrinter
    {
        public: static void Run(
            Val const x,
            Val const stream )
        {
            FixnumPrinter oPrinter(stream);
            oPrinter.run(x);
        } // Run

        private: Val m_base;
        private: Val m_stream;

        private: FixnumPrinter(Val stream) :
            m_base(TLV(Aprint_baseA)),
            m_stream(stream) {}

        private: void printUInt(Int const nVal)
        {
            Int const nBase = Fixnum::Decode_(m_base);

            if (nVal < nBase)
            {
                write_char(
                    digit_char(Fixnum::Encode(nVal), m_base),
                    m_stream );
            }
            else
            {
                printUInt(nVal / nBase);
                write_char(
                    digit_char(Fixnum::Encode(nVal % nBase), m_base),
                    m_stream );
            }
        } // printUInt

        private: void run(Val const x)
        {
            Int const iVal = Fixnum::Decode_(x);
            if (iVal < 0)
            {
                write_char(Character::FromCode('-'), m_stream);
                printUInt(-iVal);
            }
            else
            {
                printUInt(iVal);
            }
        } // Run

    }; // FixnumPriner

    FixnumPrinter::Run(x, s);
    return x;
} // print_object fixnum

defmethod(print_object, function, (Val x, Val s))
{
    format(s, "#<~S ~S @~X~X>",
        type_of(x),
        function_name(x),
        Fixnum::Encode(x->ToInt() >> 4),
        Fixnum::Encode(x->ToInt() & 15) );

    return x;
} // print_object function

defmethod(print_object, hash_table, (Val x, Val s))
{
    HashTable* p = x->StaticCast<HashTable>();
    format(s, "#<Hash-Table ~S ~D/~D @~X~X>",
        p->m_test,
        svref(p->m_vector, zero),
        ash(length(p->m_vector), minus_one),
        Fixnum::Encode(x->ToInt() >> 4),
        Fixnum::Encode(x->ToInt() & 15) );
    return x;
} // print_object hash_table

defmethod(print_object, package, (Val x, Val s))
{
    Package* p = x->DynamicCast<Package>();
    format(s, "#<Package ~A Ext:~D/~D Int:~D/~D>",
        car(p->m_names),
        svref(p->m_external_table, zero),
        length(p->m_external_table),
        svref(p->m_internal_table, zero),
        length(p->m_internal_table) );
    return x;
} // print_object package

defmethod(print_object, ratio, (Val x, Val s))
{
    RatioImpl* const p = x->StaticCast<RatioImpl>();
    format(s, "~S/~S", p->m_numerator, p->m_denominator);
    return x;
} // print_object ratio

defmethod(print_object, rational_complex, (Val x, Val s))
{
    RationalComplexImpl* const p = x->StaticCast<RationalComplexImpl>();
    format(s, "#c(~S ~S)", p->m_real, p->m_imag);
    return x;
} // print_object rational_complex

defmethod(print_object, setf_cell, (Val x, Val s))
{
    SetfCell* p = x->DynamicCast<SetfCell>();
    format(s, "#<Setf-Cell ~S>", p->m_name);
    return x;
} // print_object value_cell

defmethod(print_object, simple_string, (Val x, Val s))
{
    SimpleString* p = x->DynamicCast<SimpleString>();
    if (nil == TLV(Aprint_escapeA))
    {
        write_string(p->GetStart(), p->GetLength(), s);
    }
    else
    {
        write_char(DoubleQuote, s);
        CString oString(x);
        foreach (CString::Enum, oEnum, &oString)
        {
            char16 wch = oEnum.Get();
            if (DoubleQuote == wch || Backslash == wch)
            {
                write_char(Backslash, s);
            }
            write_char(wch, s);
        } // for each char
        write_char(DoubleQuote, s);
    }
    return x;
} // print_object simple_string

defmethod(print_object, simple_vector, (Val x, Val s))
{
    SimpleVector* p = x->StaticCast<SimpleVector>();

    if (nil == TLV(Aprint_arrayA))
    {
        format(s, "#<~S @~X~X>",
            type_of(x),
            Fixnum::Encode(x->ToInt() >> 4),
            Fixnum::Encode(x->ToInt() & 15) );
    }
    else
    {
        Int iCount = Fixnum::MostPositive;
        if (fixnump(TLV(Aprint_lengthA)))
        {
            iCount = Fixnum::Decode_(TLV(Aprint_lengthA));
        }
        format(s, "#(");
        const char16* pwsz = L"";
        foreach (SimpleVector::Enum, oEnum, p)
        {
            write_string(pwsz, s);

            if (iCount <= 0)
            {
                write_string("...", s);
                break;
            }

            iCount -= 1;

            print_object(oEnum.Get(), s);
            pwsz = L" ";
        } // for each elt
        write_string(")", s);
    } // if

    return x;
} // print_object simple_vector

defmethod(print_object, single_float, (Val x, Val s))
{
    FloatPrinter_<Float32Impl, uint32>::Run(
        x->StaticCast<Float32Impl>(),
        s );
    return x;
} // print_object single_float

defmethod(print_object, single_float_complex, (Val x, Val s))
{
    Float32ComplexImpl* const p = x->StaticCast<Float32ComplexImpl>();
    StackFloat32 oReal(p->m_fltReal);
    StackFloat32 oImag(p->m_fltImag);
    format(s, "#c(~S ~S)", oReal.Encode(), oImag.Encode());
    return x;
} // print_object single_float_complex

defmethod(print_object, symbol, (Val x, Val s))
{
    Symbol* p = x->DynamicCast<Symbol>();

    Val name = p->m_name;
    Val pkg  = p->m_package;
    if (nil == TLV(Aprint_escapeA))
    {
        // Don't print package name
    }
    else if (nil == pkg)
    {
        write_string("#:", s);
    }
    else if (PKG_keyword == pkg)
    {
        write_char(':', s);
    }
    else
    {
        Val status;
        find_symbol(name, TLV(ApackageA), &status);
        if (nil == status)
        {
            // Pick up the first package alias if it has.
            Val names = pkg->StaticCast<Package>()->m_names;
            if (nil != cadr(names))
            {
                names = cdr(names);
            }

            write_string(car(names), s);

            find_symbol(name, pkg, &status);
            if (Kinternal == status)
            {
                write_string("::", s);
            }
            else
            {
                write_char(':', s);
            }
        }
    } // if
    write_string(name, s);
    return x;
} // print_object symbol

defmethod(print_object, value_cell, (Val x, Val s))
{
    ValueCell* p = x->DynamicCast<ValueCell>();
    format(s, "#<Value-Cell ~S ~S>", p->m_kind, p->m_name);
    return x;
} // print_object value_cell

defmethod(print_object, t, (Val x, Val s))
{
    format(s, "#<~S @~X~X>",
        type_of(x),
        Fixnum::Encode(x->ToInt() >> 4),
        Fixnum::Encode(x->ToInt() & 15) );

    return x;
} // print_object

// [R]
Val read(Val stream, Val eof_error_p, Val eof_value, Val recursivep)
{
    stream = ensure_input_stream(stream);
    return Reader::Run(
        stream,
        nil != eof_error_p,
        eof_value,
        nil != recursivep,
        false );    // preserve_whitespace_p
} // read

Val read_char(Val stream, Val eof_error_p, Val eof_value)
{
    stream = ensure_input_stream(stream);
    Val ch = funcall(Qstream_read_char, stream);
    unless (characterp(ch))
    {
        if (nil != eof_error_p)
        {
            error(Qend_of_file, Kstream, stream);
        }
        ch = eof_value;
    }
    return ch;
} // read_char

// [U]
Val unread_char(Val ch, Val stream)
{
    check_type(ch, character);
    stream = ensure_input_stream(stream);
    return funcall(Qstream_unread_char, stream, ch);
} // unread_char

// [W]
Val write_char(Val ch, Val stream)
{
    check_type(ch, character);
    stream = ensure_output_stream(stream);
    return funcall(Qstream_write_char, stream, ch);
} // write_char

defun(write_stringV, (Thread* pth))
{
    Val string = pth->mv_value[0];
    Val stream = pth->mv_value[1];
    Val start  = zero;
    Val end    = nil;

    KeyArg rgoKey[] =
    {
        KEYARG(end),
        KEYARG(start),
    }; // rgoKey

    parseKeys(pth, 2, rgoKey, lengthof(rgoKey));

    stream = ensure_output_stream(stream);
    return funcall(Qstream_write_string, stream, string, start, end);
} // write_string

void write_string(Val string, Val stream)
{
    stream = ensure_output_stream(stream);
    funcall(Qstream_write_string, stream, string);
} // write_string

void write_string(const char* pch, Int cch, Val stream)
{
    StackString_<> oString(pch, cch);
    stream = ensure_output_stream(stream);
    funcall(Qstream_write_string, stream, oString);
} // write_string

void write_string(const char16* pwch, Int cwch, Val stream)
{
    StackString_<> oString(pwch, cwch);
    stream = ensure_output_stream(stream);
    funcall(Qstream_write_string, stream, oString);
} // write_string

} // CommonLisp
} // TinyCl

