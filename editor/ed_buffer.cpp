#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer
// editor/ed_buffer.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer.cpp#11 $
//
//
#include "./ed_buffer.h"

#include "./cm_mode.h"

#include "./ed_selection.h"
#include "./ed_text_window.h"

namespace Editor
{

/// <summary>
///   Construct Buffer object of specified name.
/// </summary>
/// <param name="name">
///   A name of buffer. This name should be unique in all existing buffer
///   in *buffers*.
/// </param>
Buffer::Buffer(Val name) :
    BufferFile(name)
{
    m_lexer     = nil;
    m_mode      = VAR(text_mode);
    m_name      = name;
    m_read_only = nil;
    m_state     = nil;
    m_style_map = nil;
    m_windows   = nil;
} // Buffer::Buffer

//////////////////////////////////////////////////////////////////////
//
// getCharClass
//  Returns ANSIC C/POSIX(LC_TYPE)
//
// See WinNls.h for C1_xxx
//  C1_UPPER    0x001
//  C1_LOWER    0x002
//  C1_DIGIT    0x004
//  C1_SPACE    0x008
//  C1_PUNCT    0x010
//  C1_CTRL     0x020
//  C1_BLANK    0x040
//  C1_XDIGIT   0x080
//  C1_ALPHA    0x100
//  C1_DEFINED  0x200
//
//    Code    Name      Type
//  +-------+---------+-------------------------------
//  | 0x09  | TAB     | C1_SPACE + C1_CTRL + C1_BLANK
//  | 0x0A  | LF      | C1_SPACE + C1_CTRL
//  | 0x0D  | CR      | C1_SPACE + C1_CTRL
//  | 0x20  | SPACE   | C1_SPACE + C1_BLANK
//  +-------+---------+-------------------------------
//
static int getCharClass(char16 wch)
{
    WORD wType;
    if (! ::GetStringTypeW(CT_CTYPE1, &wch, 1, &wType)) return 0;
    int iMask = 0;
    //iMask |= C1_ALPHA;
    iMask |= C1_BLANK;
    iMask |= C1_CNTRL;
    //iMask |= C1_DEFINED;
    iMask |= C1_PUNCT;
    iMask |= C1_SPACE;
    return wType & iMask;
} // getCharClass

// [C]
Posn Buffer::ComputeEndOf(Unit unit, Posn posn) const
{
    CheckPosn(posn);

    if (Kbuffer == unit)
    {
        return GetEnd();
    } // buffer

    if (Kparagraph == unit)
    {
        while (xlt(posn, GetEnd()))
        {
            if (0x0A == GetCharAt(posn))
            {
                break;
            }
            posn = xadd(posn, 1);
        } // while
        return posn;
    } // paragraph

    if (Kword == unit)
    {
        int iClass1 = getCharClass(GetCharAt(posn));
        if (iClass1 & C1_BLANK)
        {
            // We are on whitespace.
            return posn;
        } // if

        // Move to end of word.
        while (xlt(posn, GetEnd()))
        {
            int iClass2 = getCharClass(GetCharAt(posn));
            if (iClass1 != iClass2) break;
            posn = xadd(posn, 1);
        } /// while
        return posn;
    } // word

    error(Qtype_error,
        Kdatum,         unit,
        Kexpected_type, list(Qmember, Kbuffer, Kparagraph, Kword) );
} // Buffer::ComputeEndOf

// Word Motion:
//                      forward             backward
//  th|is is a word.    this |is a word.    |this is a word.
//  this |is a word.    this is| a word.    |this is a word.
//
Count Buffer::ComputeMotion(Unit unit, Count n, Posn* inout_posn) const
{
    Posn posn = *inout_posn;
    CheckPosn(posn);

    check_type(n, fixnum);

    if (Kcharacter == unit)
    {
        if (xgt(n, zero))
        {
            *inout_posn = xmin(GetEnd(), xadd(posn, n));
            return xsub(posn, *inout_posn);
        }
        else if (xlt(n, zero))
        {
            *inout_posn = xmax(GetStart(), xadd(posn, n));
            return xsub(posn, *inout_posn);
        } // if
        return zero;
    } // if character

    if (Kparagraph == unit || Kline == unit)
    {
        Int iK = 0;
        if (xgt(n, zero))
        {
            Int iN = Fixnum::Decode_(n);
            for (iK = 0; iK < iN; iK++)
            {
                posn = ComputeEndOf(Kparagraph, posn);
                if (posn == GetEnd()) break;
                posn = xadd(posn, one);
            } // for iK
            *inout_posn = posn;
        }
        else if (xlt(n, zero))
        {
            Int iN = -Fixnum::Decode_(n);
            for (iK = 0; iK < iN; iK++)
            {
                posn = ComputeStartOf(Kparagraph, posn);
                if (posn == GetStart()) break;
                posn = xsub(posn, one);
            } // for iK
            *inout_posn = posn;
        } // if
        return Fixnum::Encode(iK);
    } // if paragraph

    if (Kword == unit)
    {
        Count k = zero;
        if (xgt(n, zero))
        {
            if (posn == GetEnd()) return zero;
            do
            {
                int iClass1 = getCharClass(GetCharAt(posn));
                // Skip current word
                for (;;)
                {
                    posn = xadd(posn, one);
                    if (posn == GetEnd())
                    {
                        k = xadd(k, one);
                        goto exit;
                    }

                    int iClass2 = getCharClass(GetCharAt(posn));
                    if (iClass1 == iClass2) continue;

                    while (iClass2 & C1_BLANK)
                    {
                        posn = xadd(posn, one);
                        if (posn == GetEnd()) break;
                        iClass2 = getCharClass(GetCharAt(posn));
                    } // while
                    break;
                } // for
                k = xadd(k, one);
            } while (xlt(k, n));
        }
        else if (xlt(n, zero))
        {
            if (posn == GetStart()) return zero;
            do
            {
                posn = xsub(posn, 1);
                int iClass1 = getCharClass(GetCharAt(posn));
                // Skip current work
                for (;;)
                {
                    if (posn == GetStart())
                    {
                        k = xadd(k, one);
                        goto exit;
                    }

                    posn = xsub(posn, one);
                    int iClass2 = getCharClass(GetCharAt(posn));
                    if (iClass1 == iClass2) continue;
                    if (iClass1 & C1_BLANK)
                    {
                        while (iClass2 & C1_BLANK)
                        {
                            if (posn == GetStart()) goto exit;
                            posn = xsub(posn, one);
                            iClass2 = getCharClass(GetCharAt(posn));
                        } // while
                    } // if
                    posn = xadd(posn, one);
                    break;
                } // for
                k = xadd(k, one);
            } while (xlt(k, n));
        } // if
      exit:
        *inout_posn = posn;
        return k;
    } // if word

    SignalTypeError(
        unit,
        list(Qmember, Kcharacter, Kparagraph, Kword) );
} // Buffer::ComputeMotion

Posn Buffer::ComputeStartOf(Unit unit, Posn posn) const
{
    CheckPosn(posn);

    if (Kbuffer == unit)
    {
        return GetStart();
    } // buffer

    if (Kparagraph == unit)
    {
        while (xgt(posn, 0))
        {
            posn = xsub(posn, 1);
            if (0x0A == GetCharAt(posn))
            {
                posn = xadd(posn, 1);
                break;
            }
        } // while
        return posn;
    } // paragraph

    if (Kword == unit)
    {
        if (posn == GetStart()) return posn;

        int iClass1 = getCharClass(GetCharAt(posn));
        if (iClass1 & C1_BLANK)
        {
            // We are on whitespace.
            return posn;
        }

        // Move to start of word
        while (xgt(posn, GetStart()))
        {
            posn = xsub(posn, 1);
            int iClass2 = getCharClass(GetCharAt(posn));
            if (iClass1 != iClass2)
            {
                // Back to previous character
                posn = xadd(posn, one);
                break;
            }
        } // while
        return posn;
    } // word

    error(Qtype_error,
        Kdatum,         unit,
        Kexpected_type, list(Qmember, Kbuffer, Kparagraph, Kword) );
} // Buffer::ComputeStartOf

Count Buffer::ComputeWhile(Val set, Count n, Posn* inout_posn) const
{
    Posn start = *inout_posn;
    if (xgt(n, zero))
    {
        Posn posn;
        for (posn = start; xlt(posn, GetEnd()); posn = xadd(posn, one))
        {
            char16 wch = GetCharAt(posn);
            if (! char_in_set_p(wch, set))
            {
                break;
            }

            n = xsub(n, 1);
            if (zero == n)
            {
                break;
            }
        } // for posn

        *inout_posn = posn;
        return xsub(posn, start);
    }
    else if (xlt(n, 0))
    {
        Posn posn = start;
        while (xgt(posn, zero))
        {
            posn = xsub(posn, 1);
            char16 wch = GetCharAt(posn);
            if (! char_in_set_p(wch, set))
            {
                posn = xadd(posn, one);
                break;
            } // if

            n = xadd(n, one);
            if (zero == n)
            {
                break;
            }
        } // while

        *inout_posn = posn;
        return xsub(start, posn);
    } // if

    return zero;
} // Buffer::ComputeWhile

// [D]
Count Buffer::Delete(Posn start, Posn end)
{
    CheckRange(start, end);
    CheckWritable();

    Val n = xsub(end, start);
    if (zero == n) return zero;

    if (checkPoint())
    {
        LogDelete(start, end);
    }

    InternalDelete(start, end);

    onChange();

    return xsub(end, start);
} // Buffer::Delete

// [I]
void Buffer::Insert(Posn posn, char16 wch, Val n)
{
    CheckPosn(posn);
    CheckWritable();

    check_type(n, fixnum);
    Int iN = Fixnum::Decode_(n);
    if (iN < 0)
    {
        error(Qtype_error, Kdatum, n, Kexpected_type, Qsequence_index);
    }

    InternalInsert(posn, wch, iN);

    onChange();

    if (checkPoint())
    {
        LogInsert(posn, xadd(posn, n));
    }
} // Buffer::Insert

void Buffer::Insert(Posn posn, const char16* pwch, size_t cwch)
{
    CheckPosn(posn);
    CheckWritable();

    InternalInsert(posn, pwch, cwch);

    onChange();

    if (checkPoint())
    {
        LogInsert(posn, xadd(posn, cwch));
    }
} // Buffer::Insert


// [M]
Val Buffer::mapStyle(Val keyvals)
{
    for (;;)
    {
        if (listp(keyvals))
        {
            return keyvals;
        }

        if (symbolp(keyvals))
        {
            Val name = keyvals;

            if (hash_table_p(m_style_map))
            {
                keyvals = gethash(name, m_style_map);
                if (nil != keyvals)
                {
                    return keyvals;
                }
            } // if

            if (Mode* p = m_mode->DynamicCast<Mode>())
            {
                if (hash_table_p(p->m_style_map))
                {
                    keyvals = gethash(name, p->m_style_map);
                    if (nil != keyvals)
                    {
                        return keyvals;
                    }
                }
            } // if mode

            Val style_map = VAR(Astyle_mapA);
            if (hash_table_p(style_map))
            {
                return gethash(name, style_map);
            }

            return nil;
        }
        else
        {
            return nil;
        }
    } // for
} // Buffer::mapStyle

// [O]
void Buffer::onChange()
{
    m_char_tick = xadd(m_char_tick, 1);
} // Buffer::onChange

bool Buffer::OnIdle(uint)
{
    if (nil == m_lexer)
    {
        m_lexer = funcall(Qmake_lexer, m_mode, Encode());
    }

    Val count = Fixnum::Encode(1000);
    return nil != funcall(Qanalyze_buffer, m_lexer, count);
} // Buffer::OnIdle

// [R]
void Buffer::Reload()
{
    foreach (EnumWindow, oEnum, this)
    {
        Val window = oEnum.Get();

        Editor::TextWindow* pWindow =
            window->StaticCast<Editor::TextWindow>();

        pWindow->GetSelection()->PrepareForReload();
    } // for each window

    Load();
} // Buffer::Reload

void Buffer::SetStyle(Posn start, Posn end, Val keyvals)
{
    ASSERT(start != end);

    if (nil != keyvals && symbolp(keyvals))
    {
        keyvals = mapStyle(keyvals);
    }

    setStyle(start, end, keyvals);
} // Buffer::SetStyle

} // Editor
