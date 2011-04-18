#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Selection
// editor/ed_selection.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_selection.cpp#8 $
//
//
#define DEBUG_MOTION 0
#include "./ed_selection.h"

#include "./ed_buffer.h"
#include "./ed_range.h"
#include "./ed_text_window.h"

#include "./peer/peer_text_window.h"

namespace Editor
{

Selection::Selection(
    Val window,
    Val buffer,
    Val start,
    Val end )
{
    To<Range>()->Init(buffer, start, end);
    m_goal_x  = minus_one;
    m_goal_y  = minus_one;
    m_active  = Kstart;
    m_window  = window;

    DEBUG_PRINTF("%p buffer=%p [%d, %d]\n",
        this,
        buffer,
        Fixnum::Decode_(start),
        Fixnum::Decode_(end) );
} // Selection

// [B]
#if 0
void Selection::Blink(Posn posn, Val wait)
{
    Val read_only = GetBuffer()->m_read_only;
    GetBuffer()->m_read_only = Kread_only;
    GetWindow()->Blink(posn, wait);
    GetBuffer()->m_read_only = read_only;
} // Selection::Blink
#endif

// [C]
void Selection::Collapse(Val x)
{
    forgetGoal();
    To<Range>()->Collapse(x);
} // Selection::Collapse

// [D]
Count Selection::Delete(Unit unit, Count n)
    { forgetGoal(); return To<Range>()->Delete(unit, n); }

// [E]
Count Selection::EndKey(Val unit, Val extendp)
{
    if (IsStartActive())
    {
        Collapse(Kend);
    }

    Count delta = EndOf(unit, extendp);

    SetActive(Kend);

    if (Kline != unit) return delta;

    Posn end = GetEnd();
    Count nspaces = To<Range>()->MoveEndWhile(" \t", Kbackward);

    // Selection.End was end of line before MoveEndWhile
    if (zero == delta) 
    {
        return nspaces;
    }

    // Selection.End was in middle of line.
    if (nspaces != delta)  
    {
        return delta;
    } // if

    // Selection.End was at end of non-whitespaces
    if (nil != extendp)
    {
        SetEnd(end);
    }
    else
    {
        SetRange(end, end);
    }

    return xsub(delta, nspaces);
} // Selection::EndKey

Count Selection::EndOf(Unit unit, Val extendp)
{
    Posn end = GetEnd();
    Posn posn;
    if (Kwindow == unit)
    {
        posn = GetWindow()->GetEnd();
    }
    else if (Kline == unit)
    {
        posn = GetWindow()->EndOfLine(GetActivePosn());
    }
    else
    {
        return To<Range>()->EndOf(unit, extendp);
    } // if

    if (nil == extendp)
    {
        SetRange(posn, posn);
    }
    else
    {
        if (! IsStartActive())
        {
            SetEnd(posn);
        }
        else if (isSingleLine())
        {
            SetEnd(posn);
            SetActive(Kend);
        }
        else
        {
            SetStart(posn);
        }
    } // if

    return xsub(posn, end);
} // Selection::EndOf

// [F]
void Selection::forgetGoal()
{
    #if DEBUG_MOTION
        DEBUG_FORAMT("goal=~D@~D", m_goal_x, m_goal_y);
    #endif
    m_goal_x = m_goal_y = minus_one;
} // Selection::forgetGoal

// [G]
Buffer* Selection::GetBuffer() const
    { return m_buffer->StaticCast<Buffer>(); }

Peer::TextWindow* Selection::GetWindow() const
{ 
    TextWindow* pWindow = m_window->StaticCast<TextWindow>();
    return pWindow->m_peer->To<Peer::TextWindow>();
} // Selection::GetWindow

// [H]
Count Selection::HomeKey(Unit unit, Val extendp)
{
    if (! IsStartActive())
    {
        Collapse(Kstart);
    }

    Count delta = StartOf(unit, extendp);

    if (Kline != unit)
    {
        return delta;
    }

    Posn line_start = GetStart();
    Count nspaces = To<Range>()->MoveStartWhile(" \t", Kforward);

    // Selection.Start was at end of line before MoveWhile
    if (zero == delta) return nspaces;

    // Selection.Start was in middle of line
    if (nspaces != delta) return delta;

    // Selection.Start was at start of none whitespaces.
    if (nil != extendp)
    {
        SetStart(line_start);
    }
    else
    {
        SetRange(line_start, line_start);
    }

    return xsub(delta, nspaces);
} // Selection::HomeKey

// [I]
bool Selection::isSingleLine() const
{
    Posn start = GetWindow()->StartOfLine(GetStart());
    Posn end   = GetWindow()->StartOfLine(GetEnd());
    return start == end;
} // Selection::isSingleLine

// [M]
Count Selection::moveAux(Val unit, Count n, Val extendp)
{
    Posn active;
    if (nil != extendp)
    {
        // Extend from active position
        active = GetActivePosn();
    }
    else if (GetEnd() == GetStart())
    {
        // Move from insertion position.
        active = GetStart();
    }
    else
    {
        // Move from specified directin end.
        active = xgt(n, zero) ? GetEnd() : GetStart();

        // Note: we make selection collaped insted of moveing end.
        if (Kcharacter == unit)
        {
            if (n == zero) return zero;
            MoveTo(active, nil);
            return one;
        }
    } // if

    Peer::Point pt(Fixnum::Decode_(m_goal_x), Fixnum::Decode_(m_goal_y));
    Count k = GetWindow()->ComputeMotion(unit, n, pt, &active);
    MoveTo(active, extendp);
    return k;
} // Selection::moveAux

//  Unit        | Key Combination
//  ------------+----------------
//  Line        | Down
//  Paragraph   | Ctrl+Down
/// Screen      | PageDown
//  Window      | Ctrl+PageDown
Count Selection::MoveDown(Val unit, Val n, Val extendp)
{
    if (Kparagraph == unit)
    {
        forgetGoal();
    }

    updateGoal();
    return moveAux(unit, n, extendp);
} // Selection::Move

//  Unit        | Key Combination
//  ------------+----------------
//  Char        | Left
/// Word        | Ctrl+Left
Count Selection::MoveLeft(Val unit, Val n, Val extendp)
    { forgetGoal(); return moveAux(unit, xsub(zero, n), extendp); }

//  Unit        | Key Combination
//  ------------+----------------
//  Char        | Right
/// Word        | Ctrl+Right
//
// Note: If selection isn't nondegenerate and fExtend is false, selection
// becomes degenerate.
Count Selection::MoveRight(Val unit, Val n, Val extendp)
{
    forgetGoal();
    return moveAux(unit, n, extendp);
} // Selection::MoveRight

void Selection::MoveTo(Posn posn, Val extendp)
{
    if (nil == extendp)
    {
        To<Range>()->SetRange(posn, posn);
    }
    else
    {
        To<Range>()->SetRange(posn, IsStartActive() ? GetEnd() : GetStart());
        SetActive(xle(posn, GetStart()) ? Kstart : Kend);
    } // if
} // Selection::MoveTo

Count Selection::MoveUp(Val unit, Val n, Val extendp)
{
    if (Kparagraph == unit)
    {
        forgetGoal();
    }

    updateGoal();
    return moveAux(unit, xsub(zero, n), extendp);
} // Selection::MoveUp

// [P]
void Selection::PrepareForReload()
{
    // FIXME 2008-01-20 yosi@msn.com We should use line number cache.

    Buffer* pBuffer = GetBuffer();
    int iLineNum = 0;
    for (
        Posn posn = zero;
        xlt(posn, m_start);
        posn = xxadd(posn, one) )
    {
        if (pBuffer->GetCharAt(posn) == 0x0A)
        {
            iLineNum += 1;
            break;
        } // if
    } // for posn

    m_line_number = Fixnum::Encode(iLineNum);
} // Selection::PrepareForReload

// [S]

/// <summary>
///   Set active end.
/// </summary>
/// <param name="x"><c>(member :end :start)</c></param>
void Selection::SetActive(Val x)
{
    if (Kend == x)
    {
        m_active = Kend;
    }
    else if (Kstart == x)
    {
        m_active = Kstart;
    }
    else
    {
        SignalTypeError(x, list(Qmember, Kend, Kstart));
    }
} // SetActive

/// <summary>
///  Set end position of selection object.
/// </summary>
/// <param name="x">A position</param>
Posn Selection::SetEnd(Posn x)
    { forgetGoal(); return To<Range>()->SetEnd(x); }

/// <summary>
///  Set start and end positions of selection from range.
/// </summary>
/// <param name="x">A range</param>
void Selection::SetRange(Val x)
    { forgetGoal(); To<Range>()->SetRange(x); }

/// <summary>
///  Set start and end positions of selection.
/// </summary>
/// <param name="e">A position</param>
/// <param name="s">A position</param>
void Selection::SetRange(Posn s, Posn e)
    { forgetGoal(); To<Range>()->SetRange(s, e); }

/// <summary>
///  Set start position of selection object.
/// </summary>
/// <param name="x">A position</param>
Posn Selection::SetStart(Posn x)
    { forgetGoal(); return To<Range>()->SetStart(x); }

/// <summary>
///  Replace contents of selection with specified text.
/// </summary>
/// <param name="cwch">A number of characters in C-String</param>
/// <param name="pwch">A C-String</param>
void Selection::SetText(const char16* pwch, int cwch)
{
    forgetGoal();
    To<Range>()->SetText(pwch, cwch);
} // SetText

/// <summary>
///  Move or extend start position of selection.
/// </summary>
/// <param name="extend">True if extend</param>
/// <param name="unit">An unit of move</param>
/// <returns>Number of character moved</returns>
Count Selection::StartOf(Unit unit, Val extendp)
{
    forgetGoal();

    Posn start = GetStart();

    Posn posn;
    if (Kline == unit)
    {
        posn = GetWindow()->StartOfLine(GetActivePosn());
    }
    else if (Kwindow == unit)
    {
        posn = GetWindow()->GetStart();
    }
    else
    {
        return To<Range>()->StartOf(unit, extendp);
    } // if

    if (nil == extendp)
    {
        SetRange(posn, posn);
    }
    else if (IsStartActive())
    {
        SetStart(posn);
    }
    else if (isSingleLine())
    {
        SetStart(posn);
        SetActive(Kstart);
    }
    else
    {
        SetEnd(posn);
    } // if

    return xsub(start, posn);
} // Selection::StartOf

// [T]
void Selection::TypeChar(Val ch, Val arg)
{
    check_type(ch, character);

    if (xle(arg, zero)) return;

    char16 wch = ch->StaticCast<Character>()->ToCode();

    Buffer* pBuffer = GetBuffer();
    if (GetStart() == GetEnd())
    {
        pBuffer->Insert(GetStart(), wch, arg);
    }
    else
    {
        To<Range>()->SetText(NULL, 0);
        pBuffer->Insert(GetStart(), wch, one);
    }

    MoveRight(Kcharacter, arg, nil);
} // Selection::TypeChar

// [U]
bool Selection::updateGoal()
{
    if (xge(m_goal_x, zero))
    {
        return false;
    }

    POINT pt;
    if (GetWindow()->MapPosnToPoint(GetActivePosn(), &pt) > 0)
    {
        m_goal_x = Fixnum::Encode(pt.x);
        m_goal_y = Fixnum::Encode(pt.y);
        #if DEBUG_MOTION
            DEBUG_FORMAT("new goal=~D@~D~%", m_goal_x, m_goal_y);
        #endif
    } // if

    return true;
} // Selection::updateGoal

StackRange::StackRange(Val x)
{
    if (Range* p = x->DynamicCast<Range>())
    {
        init(p->GetBuffer(), p->m_start, p->m_end);
    }
    else
    {
        Si::SignalTypeError(x, Qrange);
    }
} // StackRange::StackRange

StackRange::StackRange(Selection* p)
{ 
    init(p->GetBuffer(), p->m_start, p->m_end);
} // StackRange::StackRange

void StackRange::init(Buffer* pBuffer, Posn start, Posn end)
{
    m_classd = CLASSD_range;
    m_buffer = pBuffer->Encode();
    m_end    = end;
    m_start  = start;
    pBuffer->To<Buffer::Ranges>()->Append(Encode());
} // StackRange::init

Buffer::UndoBlock::UndoBlock(Selection* p, Val name, Val posn)
    { init(p->GetBuffer(), name, posn); }

} // Editor
