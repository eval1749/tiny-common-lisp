#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Lexer - Text Lexer
// editor/lexer/lexer_text.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_lexer.cpp#1 $
//
//
#define DEBUG_RESTART 1
#include "./lexer_defs.h"

#include "../cm_mode.h"

#include "../ed_buffer.h"
#include "../ed_interval.h"
#include "../ed_tracker.h"

namespace Editor
{

// [E]
Lexer::EnumCI::EnumCI(Buffer* pBuffer, Posn posn, int iCount) :
    m_intv(pBuffer->GetIntervalAt(posn)),
    m_posn(posn),
    m_pBuffer(pBuffer)
{
    m_end = xxadd(posn, Fixnum::Encode(iCount));
    m_end = xmin(m_end, pBuffer->GetEnd());
    fill();
} // Lexer::EnumCI::EnumCI

void Lexer::EnumCI::fill()
{
    uint cwch = m_pBuffer->GetText(m_posn, m_rgwch, lengthof(m_rgwch));
    m_cache_start = m_posn;
    m_cache_end   = xxadd(m_cache_start, cwch);
    m_cache_end   = xmin(m_cache_end, m_end);
    m_pwch = m_rgwch;
} // Lexer::EnumCI:fill

Val Lexer::EnumCI::Get() const
{
    ASSERT(! AtEnd());

    // We don't override style set by IME
    if (GetInterval()->m_state == Kime)
    {
        return Kime;
    }

    return Character::FromCode(*m_pwch);
} // Lexer::EnumCI:Get

Interval* Lexer::EnumCI::GetInterval() const
{
    ASSERT(! AtEnd());
    return m_intv->StaticCast<Interval>();
} // Lexer::EnumCI:GetInterval

bool Lexer::EnumCI::More() const
{
    return xlt(m_posn, m_pBuffer->GetEnd());
} // Lexer::EnumCI::More

void Lexer::EnumCI::Next()
{
    ASSERT(! AtEnd());
    m_posn = xadd(m_posn, one);
    if (m_posn == m_cache_end)
    {
        fill();
    }
    else
    {
        m_pwch++;
    }

    if (! m_intv->StaticCast<Interval>()->In(m_posn))
    {
        m_intv = m_pBuffer->GetIntervalAt(m_posn);
    }
} // Lexer::EnumCI::Next()

void Lexer::EnumCI::Prev()
{
    m_posn = xsub(m_posn, one);
    --m_pwch;

    if (! m_intv->StaticCast<Interval>()->In(m_posn))
    {
        m_intv = m_pBuffer->GetIntervalAt(m_posn);
    }
} // Lexer::EnumCI::Prev

void Lexer::endLexeme(Val state)
{
    ASSERT(symbolp(state));

    if (m_position != m_lexeme)
    {
        GetBuffer()->SetStyle(m_lexeme, m_position, state);
        m_lexeme = m_position;
    }

    m_state = nil;
} // Lexer::endLexeme

// [G]
Buffer* Lexer::GetBuffer() const
{
    return m_tracker->StaticCast<Tracker>()->GetBuffer();
} // Lexer::GetBuffer

// [I]
void Lexer::init(Val buffer)
{
    m_tracker      = (new Tracker(buffer))->Encode();
    m_lexeme       = zero;
    m_position     = zero;
    m_state        = nil;
} // Lexer::init

// Find restart position before posn.
// Note: The state of wew inserted characters are nil.
void Lexer::restart(Posn posn)
{
    if (posn != zero)
    {
        Val intv = GetBuffer()->GetIntervalAt(xxsub(posn, one));
        posn = zero;
        while (nil != intv)
        {
            Interval* pIntv = intv->StaticCast<Interval>();

            #if DEBUG_RESTART
                DEBUG_FORMAT("~S of [~D,~D]~%",
                    pIntv->m_state, pIntv->m_start, pIntv->m_end );
            #endif

            if (Kspace == pIntv->m_state || nil == pIntv->m_state)
            {
                //posn = xxsub(pIntv->m_end, one);
                posn = pIntv->m_start;
                break;
            }

            intv = pIntv->m_prev;
        } // while
    } // if

    #if DEBUG_RESTART
        DEBUG_FORMAT("Restart at ~D~%", posn);
    #endif

    m_lexeme   = posn;
    m_position = posn;
    m_state    = nil;
} // Lexer::restart

// [S]
void Lexer::startLexeme()
{
    m_lexeme = xsub(m_position, one);
} // Lexer::startLexeme

} // Editor
