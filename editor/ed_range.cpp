#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Range
// editor/ed_range.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_range.cpp#8 $
//
//
#define DEBUG_LIFE 0
#include "./ed_range.h"

#include "./ed_buffer.h"

namespace Editor
{

class EnumChar
{
    private: bool       m_fForward;
    private: Int        m_iK;
    private: Int        m_iN;
    private: Buffer*    m_pBuffer;
    private: Posn       m_limit;
    private: Posn       m_posn;

    public: struct Arg
    {
        Buffer* m_pBuffer;
        Val     m_n;
        Posn    m_posn;

        Arg(Buffer* pBuffer, Posn posn, Count n) :
            m_pBuffer(pBuffer),
            m_n(n),
            m_posn(posn) {}
    }; // Arg

    public: EnumChar(Arg oArg)
        { init(oArg.m_pBuffer, oArg.m_posn, oArg.m_n); }

    public: EnumChar(const Range* p, Posn posn, Count n)
        { init(p->GetBuffer(), posn, n); }

    private: void init(Buffer* pBuffer, Posn posn, Count n)
    {
        m_iK = 0;
        m_pBuffer = pBuffer;
        m_posn = posn;

        if (Kbackward == n)
        {
            m_iN = Fixnum::MostPositive;
            m_fForward = false;
        }
        else if (Kforward == n)
        {
            m_iN = Fixnum::MostPositive;
            m_fForward = true;
        }
        else if (fixnump(n))
        {
            m_fForward = xge(n, zero);
            if (m_fForward)
            {
                m_iN = Fixnum::Decode_(n);
            }
            else
            {
                m_iN = -Fixnum::Decode_(n);
            }
        }
        else
        {
            SignalTypeError(
                n,
                list(Qor,
                    Qfixnum,
                    list(Qmember, Kbackward, Kforward) ) );
        } // if

        m_limit = m_fForward ? pBuffer->GetEnd() : pBuffer->GetStart();
    } // EnumChar

    public: bool AtEnd() const
    {
        if (m_iK == m_iN) return true;
        return m_limit == m_posn;
    } // AtEnd

    public: bool AtStart() const
    {
        if (m_iK == 0) return true;

        if (m_fForward)
        {
            return zero == m_posn;
        }
        else
        {
            return m_pBuffer->GetEnd() == m_posn;
        }
    } // AtStart

    public: char16 Get() const
    {
        ASSERT(!AtEnd());
        if (m_fForward)
        {
            return m_pBuffer->GetCharAt(m_posn);
        }
        else
        {
            return m_pBuffer->GetCharAt(xsub(m_posn, 1));
        }
    } // Get

    public: Val GetCount() const
        { return Fixnum::Encode(m_iK); }

    public: Posn GetPosn() const
        { return m_posn; }

    public: void Next()
    {
        ASSERT(! AtEnd());
        m_iK += 1;
        if (m_fForward)
        {
            m_posn = xadd(m_posn, 1);
        }
        else
        {
            m_posn = xsub(m_posn, 1);
        }
    } // Next
    
    public: void Prev()
    {
        ASSERT(! AtStart());

        m_iK -= 1;
        if (m_fForward)
        {
            m_posn = xsub(m_posn, 1);
        }
        else
        {
            m_posn = xadd(m_posn, 1);
        }
    } // Prev
}; // EnumChar

Range::Range(Val buffer, Val start, Val end)
{
    Init(buffer, start, end);
} // Range

// [C]
void Range::Collapse(Val to)
{
    if (Kend == to)
    {
        m_start = m_end;
    }
    else if (Kstart == to)
    {
        m_end = m_start;
    }
    else
    {
        Si::SignalTypeError(to, list(Qmember, Kend, Kstart));
    }
} // Range::Collapse

Count Range::Copy() const
{

    Int iK = Fixnum::Decode_(xsub(m_end, m_start));
    if (0 == iK) return zero;
    Int cwch = iK;
    for (Posn posn = m_start; xlt(posn, m_end); posn = xadd(posn, 1))
    {
        char16 wch = GetBuffer()->GetCharAt(posn);
        if (0x0A == wch) cwch += 1;
    } // for posn

    size_t cb = sizeof(char16) * (cwch + 1);    // +1 for NUL

    Global<char16> oGlobal;
    if (! oGlobal.Alloc(cb)) PlatformError("GlobalAlloc");

    {
        char16* pwch = oGlobal.Lock();
        if (NULL == pwch) PlatformError("GlobalLock");
        for (Posn posn = m_start; xlt(posn, m_end); posn = xadd(posn, 1))
        {
            char16 wch = GetBuffer()->GetCharAt(posn);
            if (0x0A == wch) *pwch++ = 0x0D;
            *pwch++ = wch;
        } // for posn
        *pwch = 0;
        oGlobal.Unlock();
    }

    Clipboard oClipboard;
    if (! oClipboard.IsOpen())     PlatformError("OpenClipboard");
    if (! oClipboard.Empty())      PlatformError("EmptyClipboard");
    if (! oClipboard.Set(oGlobal)) PlatformError("SetClipboardData");

    oGlobal.Detach();

    return Fixnum::Encode(iK);
} // Range::Copy

Count Range::Cut()
{
    GetBuffer()->CheckWritable();
    Count k = Copy();
    if (xgt(k, zero)) GetBuffer()->Delete(m_start, m_end);
    return k;
} // Range::Cut

// [D]
Count Range::Delete(Unit unit, Count n)
{
    if (Kcharacter == unit)
    {
        Val m = m_start == m_end ? zero : one;
        Posn start = m_start;
        Posn end   = m_end;
        if (xgt(n, zero))
        {
            end = xmin(xsub(xadd(m_end, n), m), GetBuffer()->GetEnd());
        }
        else if (xlt(n, zero))
        {
            start = xmax(xadd(xadd(m_start, n), m), zero);
        }

        return GetBuffer()->Delete(start, end);
    } // if character

    if (Kword == unit)
    {
        if (xgt(n, zero))
        {
            GetBuffer()->ComputeMotion(Kword, n, &m_end);
        }
        else if (xlt(n, zero))
        {
            GetBuffer()->ComputeMotion(Kword, n, &m_start);
        }
        return GetBuffer()->Delete(m_start, m_end);
    } // if word

    SignalTypeError(unit, list(Qmember, Kcharacter, Kword));
} // Range::Delete

// [E]
Count Range::EndOf(Unit unit, Val extendp)
{
    Posn posn = GetBuffer()->ComputeEndOf(unit, m_end);
    Count k = xsub(posn, m_end);
    ASSERT(xge(k, zero));
    m_end = posn;
    if (nil == extendp) m_start = m_end;
    return k;
} // Range::EndOf

Posn Range::FindCloseParen(char16 wchOpen, char16 wchClose) const
{
    enum State
    {
        State_Normal,

        State_Backslash,
        State_Quote,
    } eState = State_Normal;

    uint cParens = 0;

    foreach (
        EnumChar,
        oEnum,
        EnumChar::Arg(GetBuffer(), m_end, Kforward) )
    {
        char16 wch = oEnum.Get();
        switch (eState)
        {
        case State_Normal:
            if (0 == wchOpen)
            {
                switch (wch)
                {
                case OpenBracket:
                case CloseBracket:
                    wchOpen  = OpenBracket;
                    wchClose = CloseBracket;
                    break;

                case OpenBrace:
                case CloseBrace:
                    wchOpen  = OpenBrace;
                    wchClose = CloseBrace;
                    break;

                case OpenParen:
                case CloseParen:
                    wchOpen  = OpenParen;
                    wchClose = CloseParen;
                    break;
                } // switch

                if (0 != wchOpen)
                {
                    if (oEnum.GetPosn() != m_end)
                    {
                        return oEnum.GetPosn();
                    }
                }
            } // if

            if (wch == wchOpen)
            {
                cParens += 1;
            }
            else if (wch == wchClose)
            {
                cParens -= 1;
                when (cParens <= 0)
                {
                    return xadd(oEnum.GetPosn(), one);
                }
            }
            else if (Backslash == wch)
            {
                eState = State_Backslash;
            }
            else if (DoubleQuote == wch)
            {
                eState = State_Quote;
            }
            break;

        case State_Backslash:
            eState = State_Normal;
            break;

        case State_Quote:
            if (DoubleQuote == wch)
            {
                eState = State_Normal;
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // for each char

    return nil;
} // Range::FindCloseParen

static bool checkNotEscaped(EnumChar* pEnum)
{
    if (pEnum->AtEnd()) return true;

    uint cEscapes = 0;
    for (;;)
    {
        pEnum->Next();

        if (pEnum->AtEnd())
        {
            break;
        }

        if (pEnum->Get() != '\\') 
        {
            pEnum->Prev();
            break;
        } // if
        cEscapes += 1;
    } // while
    
    return 0 == (cEscapes & 1);
} // checkNotEscape

Posn Range::FindOpenParen(char16 wchOpen, char16 wchClose) const
{
    enum State
    {
        State_Normal,

        State_Quote,
    } eState = State_Normal;

    uint cParens = 0;
    foreach (
        EnumChar,
        oEnum,
        EnumChar::Arg(GetBuffer(), m_start, Kbackward) )
    {
        char16 wch = oEnum.Get();
        switch (eState)
        {
        case State_Normal:
            if (0 == wchOpen)
            {
                switch (wch)
                {
                case OpenBracket:
                case CloseBracket:
                    wchOpen  = OpenBracket;
                    wchClose = CloseBracket;
                    break;

                case OpenBrace:
                case CloseBrace:
                    wchOpen  = OpenBrace;
                    wchClose = CloseBrace;
                    break;

                case OpenParen:
                case CloseParen:
                    wchOpen  = OpenParen;
                    wchClose = CloseParen;
                    break;
                } // switch
            } // if

            if (wch == wchOpen)
            {
                Posn open = oEnum.GetPosn();
                if (checkNotEscaped(&oEnum))
                {
                    if (cParens <= 1) return xsub(open, one);
                    cParens -= 1;
                }
            }
            else if (wch == wchClose)
            {
                if (checkNotEscaped(&oEnum))
                {
                    cParens += 1;
                }
                else if (0 == cParens)
                {
                    return m_start;
                }
            }
            else if (DoubleQuote == wch)
            {
                if (checkNotEscaped(&oEnum))
                {
                    eState = State_Quote;
                }
            }
            break;

        case State_Quote:
            if (DoubleQuote == wch)
            {
                if (checkNotEscaped(&oEnum))
                {
                    eState = State_Normal;
                }
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
        
        if (oEnum.AtEnd()) break;
    } // for each char

    return nil;
} // Range::FindOpenParen

// [G]
Buffer* Range::GetBuffer() const
    { return m_buffer->StaticCast<Buffer>(); }

Val Range::GetText() const
{
    Val n = xsub(m_end, m_start);
    Val s = make_string(n);
    SimpleString* p = s->StaticCast<SimpleString>();
    GetBuffer()->GetText(m_start, m_end, p->GetStart());
    return s;
} // Range::GetText

// [I]
void Range::Init(Val buffer, Val start, Val end)
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p %p [%d, %d]\n",
            this,
            buffer,
            Fixnum::Decode_(start),
            Fixnum::Decode_(end) );
    #endif

    m_buffer = buffer;
    m_start  = start;
    m_end    = end;

    Buffer* pBuffer = GetBuffer();
    pBuffer->CheckRange(start, end);
    
    // FIXME 2008-01-06 yosi@msn.com We must use weak-pointer
    // for linking ranges to buffer.
    pBuffer->To<Buffer::Ranges>()->Append(Encode());
} // Range::Init

// [M]
Count Range::Move(Unit unit, Count n)
{
    if (xgt(n, zero))
    {
        m_end = m_start;
    }
    else
    {
        m_start = m_end;
    }

    Count k = GetBuffer()->ComputeMotion(unit, n, &m_start);
    m_end = m_start;
    return k;
} // Range::Move

Count Range::MoveEndWhile(const char* psz, Count n)
{
    Si::StackString_<> oString(psz);
    EnumChar oEnum(this, m_end, n);
    while (! oEnum.AtEnd())
    {
        if (! char_in_set_p(oEnum.Get(), oString))
        {
            break;
        }
        oEnum.Next();
    } // while
    m_end = oEnum.GetPosn();
    if (xgt(m_start, m_end)) m_start = m_end;
    return oEnum.GetCount();
} // Range::MoveEndWhile

Count Range::MoveStartWhile(const char* psz, Count n)
{
    Si::StackString_<> oString(psz);
    EnumChar oEnum(this, m_start, n);
    while (! oEnum.AtEnd())
    {
        if (! char_in_set_p(oEnum.Get(), oString))
        {
            break;
        }
        oEnum.Next();
    } // while
    m_start = oEnum.GetPosn();
    if (xgt(m_start, m_end)) m_end = m_start;
    return oEnum.GetCount();
} // Range::MoveStartWhile

Count Range::MoveWhile(const char* psz, Count n)
{
    Count k;
    if (Kbackward == n || (fixnump(n) && xlt(n, zero)))
    {
        k = MoveStartWhile(psz, n);
        if (zero == k) m_start = m_end;
    }
    else
    {
        k = MoveEndWhile(psz, n);
        if (zero == k) m_end = m_start;
    } // if

    return k;
} // Range::MoveWhile

// [P]
Count Range::Paste()
{
    GetBuffer()->CheckWritable();

    Clipboard oClipboard;
    if (! oClipboard.IsOpen()) PlatformError("OpenClipboard");

    char16* pwsz = oClipboard.GetText();
    if (NULL == pwsz) return zero;
    if (0 == *pwsz) return zero;

    Delete(Kcharacter, zero);

    Posn posn = m_start;
    char16* pwchStart = pwsz;
    enum { State_Start, State_Normal, State_Cr } eState = State_Start;
    while (0 != *pwsz)
    {
        switch (eState)
        {
        case State_Cr:
            switch (*pwsz)
            {
            case 0x0A:
            {
                pwsz[-1] = 0x0A;
                Int cwch = pwsz - pwchStart;
                GetBuffer()->Insert(posn, pwchStart, cwch);
                posn = xadd(posn, cwch);
                eState = State_Start;
                pwchStart = pwsz + 1;
                break;
            } // 0x0A

            case 0x0D:
                break;

            default:
                eState = State_Normal;
                break;
            } // switch
            break;

        case State_Normal:
            if (0x0D == *pwsz)
            {
                eState = State_Cr;
            }
            break;

        case State_Start:
            eState = 0x0D == *pwsz ? State_Cr : State_Normal;
            break;
        } // switch
        pwsz++;
    } // while

    if (State_Start != eState)
    {
        Int cwch = pwsz - pwchStart;
        GetBuffer()->Insert(posn, pwchStart, cwch);
        posn = xadd(posn, cwch);
    } // if

    Count k = xsub(posn, m_start);
    m_start = m_end = posn;
    return k;
} // Range::Paste

// [S]
Posn Range::SetEnd(Posn posn)
    { SetRange(m_start, posn); return m_end; }

/// <summary>
///   Set start and end positions from range.
/// </summary>
void Range::SetRange(Val x)
{
    if (Range* pRange = x->DynamicCast<Range>())
    {
        if (pRange->GetBuffer() != GetBuffer())
        {
            error(Qwrong_buffer, Kbuffer, m_buffer, Krange, x);
        }

        m_end   = pRange->m_end;
        m_start = pRange->m_start;
    }
    else
    {
        Si::SignalTypeError(x, Qrange);
    }
} // Range::SetRange

void Range::SetRange(Posn start, Posn end)
{
    Buffer* pBuffer = GetBuffer();
    pBuffer->CheckPosn(start);
    pBuffer->CheckPosn(end);
    if (xle(start, end))
    {
        m_start = start;
        m_end   = end;
    }
    else
    {
        m_start = end;
        m_end   = start;
    }
} // Range::SetRange

Posn Range::SetStart(Posn posn)
    { SetRange(posn, m_end); return m_start; }

void Range::SetText(const char16* pwch, int cwch)
{
    Buffer* pBuffer = GetBuffer();

    pBuffer->CheckWritable();

    if (m_start == m_end)
    {
        pBuffer->Insert(m_start, pwch, cwch);
    }
    else
    {
        pBuffer->Delete(m_start, m_end);
        pBuffer->Insert(m_start, pwch, cwch);
    } // if

    m_end = min(xadd(m_start, cwch), pBuffer->GetEnd());
} // Range::SetText

void Range::SetText(Val x)
{
    if (Character* p = x->DynamicCast<Character>())
    {
        char16 wch = p->ToCode();
        SetText(&wch, 1);
    }
    else if (SimpleString* p = x->DynamicCast<SimpleString>())
    {
        SetText(p->GetStart(), p->GetLength());
    }
    else if (StringObject* p = x->DynamicCast<StringObject>())
    {
        SetText(p->GetStart(), p->GetLength());
    }
    else
    {
        SignalTypeError(x, list(Qor, Qcharacter, Qstring));
    }
} // Range::SetText

Count Range::StartOf(Unit unit, Val extendp)
{
    Posn posn = GetBuffer()->ComputeStartOf(unit, m_start);
    Count k = xsub(m_start, posn);
    ASSERT(xge(k, zero));
    m_start = posn;
    if (nil == extendp) m_end = m_start;
    return k;
} // Range::StartOf

StackRange::~StackRange()
{
    GetBuffer()->To<Buffer::Ranges>()->Delete(Encode());
} // StackRange::~StackRange

Buffer::UndoBlock::UndoBlock(Range* p, Val name, Val posn)
    { init(p->GetBuffer(), name, posn); }

} // Editor
