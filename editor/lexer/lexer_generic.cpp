#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Lexer - Generic Program Language Lexer
// editor/lexer/lexer_generic.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_lexer.cpp#1 $
//
//
#define DEBUG_ANALYZE 0
#define DEBUG_RESTART 0
#define DEBUG_TAIL    0
#include "./lexer_generic.h"

#include "../cm_mode.h"

#include "../ed_buffer.h"
#include "../ed_interval.h"
#include "../ed_tracker.h"

#include "../rtl_defs.h"

namespace Editor
{

namespace
{

bool isCppWord(char16 wch)
{
    if (wch >= 'a' && wch <= 'z') return true;
    if ('_' == wch) return true;
    return false;
} // isCppWord

bool isCppWord2(char16 wch)
{
    if (isCppWord(wch)) return true;
    if (wch >= '0' && wch <= '9') return true;
    return false;
} // isCppWord

} // namespace

void GenericLexer::processLexeme(Val prefix)
{
    char16 rgwch[20];

    Val posn = xxsub(m_position, one);

    Val state = m_state;
    Int cwch = Fixnum::Decode_(xxsub(posn, m_lexeme));
    if (cwch > 0 && cwch < lengthof(rgwch))
    {
        char16* pwch = rgwch;
        if (Character* p = prefix->DynamicCast<Character>())
        {
            rgwch[0] = p->ToCode();
            cwch++;
            pwch++;
        } // if prefix

        GetBuffer()->GetText(m_lexeme, posn, pwch);

        Val symtab = GetBuffer()->m_mode->StaticCast<Mode>()->m_keywords;

        Si::StackString_<lengthof(rgwch)> oWord(rgwch, cwch);

        #if DEBUG_ANALYZE
            DEBUG_FORMAT("~S [~D,~D] ~S~%", 
                m_state, m_lexeme, posn, static_cast<Val>(oWord) );
        #endif

        state = gethash(oWord, symtab, Kword);
    } // if

    if (posn != m_lexeme)
    {
        GetBuffer()->SetStyle(m_lexeme, m_position, state);
        m_lexeme = m_position;
    }

    m_state = nil;
} // GenericLexer::processLexeme

Val GenericLexer::Run(Int iLimit)
{
    Tracker* pTracker = m_tracker->StaticCast<Tracker>();
    if (xge(m_position, pTracker->m_start))
    {
        restart(pTracker->m_start);
    }

    pTracker->Reset();

    if (zero == m_position)
    {
        m_state = Knewline;
    }

    EnumCI oEnumCI(pTracker->GetBuffer(), m_position, iLimit);

    if (oEnumCI.AtEnd())
    {
        return nil;
    }

    Val syntab = pTracker->GetBuffer()->m_mode->StaticCast<Mode>()->
                    m_syntax_table;

    bool fCpp = Knewline == char_syntax(
        Character::FromCode(LineFeed),
        syntab );

    while (! oEnumCI.AtEnd())
    {
        Val ch = oEnumCI.Get();
        oEnumCI.Next();

        // Ignore Backslash+LF
        if (Character::FromCode(Backslash) == ch)
        {
            if (oEnumCI.AtEnd())
            {
                oEnumCI.Prev();
                break;
            }

            ch = oEnumCI.Get();
            oEnumCI.Next();

            if (fCpp)
            {
                if (Character::FromCode(LineFeed) == ch)
                {
                    m_position = xxadd(m_position, two);
                    if (oEnumCI.AtEnd())
                    {
                        break;
                    }
                    continue;
                } // if
            } // if Cpp

            m_position = xxadd(m_position, one);
        } // if backslash

        if (Character* p = ch->DynamicCast<Character>())
        {
            m_position = xxadd(m_position, one);
            Val syntax = char_syntax(ch, syntab);
            transfer(ch, syntax);
        }
        else
        {
            // We may encounter IME.
            return nil;
        }
    } // while

    if (oEnumCI.More())
    {
        return Qcontinue;
    }

    #if DEBUG_TAIL
        DEBUG_FORMAT("Tail ~S~%",
            consp(m_state) ? car(m_state) : m_state );
    #endif

    if (m_position != m_lexeme)
    {
        Val state = m_state;
        if (consp(state))
        {
            state = car(state);
        }

        GetBuffer()->SetStyle(m_lexeme, m_position, state);
    }

    return nil;
} // GenericLexer::Run

void GenericLexer::transfer(Val ch, Val syntax)
{
   tryAgain:
    if (nil == m_state)
    {
        m_state = syntax;
        startLexeme();
        return;
    } // if nil

    if (Knewline == m_state)
    {
        char16 wch = ch->StaticCast<Character>()->ToCode();
        switch (wch)
        {
        case ' ':
        case LineFeed:
        case Tab:
        case Page:
            break;

        case '#':
            m_state = Kcpp;
            break;

        default:
            m_state = nil;
            goto tryAgain;
        } // switch ch
        return;
    } // if LineFeed

    // REVIEW 2008-02-17 yosi@msn.com Should we have independent CPP parser?
    if (Kcpp == m_state)
    {
        char16 wch = ch->StaticCast<Character>()->ToCode();
        switch (wch)
        {
        case ' ':
        case Tab:
        case Page:
        case LineFeed:
            break;

        default:
            if (isCppWord(wch))
            {
                startLexeme();
                m_state = Kcpp_word;
            }
            else
            {
                m_state = nil;
                goto tryAgain;
            }
            break;
        } // switch ch
        return;
    } // if cpp

    if (Kcpp_word == m_state)
    {
        char16 wch = ch->StaticCast<Character>()->ToCode();
        if (! isCppWord2(wch))
        {
            processLexeme(Character::FromCode('#'));
            m_state = nil;
            goto tryAgain;
        }
        return;
    } // if

    if (consp(m_state))
    {
        Val xfer = getf(cdr(m_state), ch);
        if (Kend == xfer)
        {
            #if DEBUG_ANALYZE
                DEBUG_FORMAT("End p=~D state=~S ch=~S~%", 
                    m_position, car(m_state), ch );
            #endif

            endLexeme(car(m_state));

            if (Character::FromCode(LineFeed) == ch)
            {
                goto tryAgain;
            }

            return;
        } // if

        if (nil != xfer)
        {
            #if DEBUG_ANALYZE
                DEBUG_FORMAT("Xfer p=~D state=~S ch=~S~%", 
                    m_position, car(m_state), ch );
            #endif

            m_state = xfer;
            return;
        } // if

        Val otherwise = getf(cdr(m_state), Qotherwise);
        if (nil != otherwise)
        {
            #if DEBUG_ANALYZE
                DEBUG_FORMAT("Otherwise p=~D state=~S ch=~S~%", 
                    m_position, car(m_state), ch );
            #endif

            if (symbolp(otherwise))
            {
                endLexeme(otherwise);
            }

            m_state = otherwise;
            return;
        } // if

        return;
    } // if cons

    if (m_state != syntax)
    {
        processLexeme(nil);
        m_state = nil;
        goto tryAgain;
    } // if
} // GenericLexer::transfer

} // Editor
