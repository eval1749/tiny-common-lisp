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
#define DEBUG_ANALYZE 0
#include "./lexer_defs.h"

#include "../ed_tracker.h"

/*
    RFC 2396 - Uniform Resource Identifiers (URI): Generic Syntax

    URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
    absoluteURI   = scheme ":" ( hier_part | opaque_part )
    relativeURI   = ( net_path | abs_path | rel_path ) [ "?" query ]

    hier_part     = ( net_path | abs_path ) [ "?" query ]
    opaque_part   = uric_no_slash *uric

    uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" |
                  "&" | "=" | "+" | "$" | ","

    net_path      = "//" authority [ abs_path ]
    abs_path      = "/"  path_segments
    rel_path      = rel_segment [ abs_path ]

    rel_segment   = 1*( unreserved | escaped |
                      ";" | "@" | "&" | "=" | "+" | "$" | "," )

    scheme        = alpha *( alpha | digit | "+" | "-" | "." )

    authority     = server | reg_name

    reg_name      = 1*( unreserved | escaped | "$" | "," |
                      ";" | ":" | "@" | "&" | "=" | "+" )

    server        = [ [ userinfo "@" ] hostport ]
    userinfo      = *( unreserved | escaped |
                     ";" | ":" | "&" | "=" | "+" | "$" | "," )

    hostport      = host [ ":" port ]
    host          = hostname | IPv4address
    hostname      = *( domainlabel "." ) toplabel [ "." ]
    domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
    toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
    IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit
    port          = *digit

    path          = [ abs_path | opaque_part ]
    path_segments = segment *( "/" segment )
    segment       = *pchar *( ";" param )
    param         = *pchar
    pchar         = unreserved | escaped |
                  ":" | "@" | "&" | "=" | "+" | "$" | ","

    query         = *uric

    fragment      = *uric

    uric          = reserved | unreserved | escaped
    reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
                  "$" | ","
    unreserved    = alphanum | mark
    mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
                  "(" | ")"

    escaped       = "%" hex hex
    hex           = digit | "A" | "B" | "C" | "D" | "E" | "F" |
                          "a" | "b" | "c" | "d" | "e" | "f"

    alphanum      = alpha | digit
    alpha         = lowalpha | upalpha

    lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
             "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
             "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
    upalpha  = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
             "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
             "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
    digit    = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
             "8" | "9"
*/

static bool isUriSafe(char16 wch)
{
    if (wch <= 0x20) return false;
    if (wch >= 0x7F) return false;
    return NULL == ::lstrchrW(L"\x22()<>[]{}", wch);
} // isUriSafe

namespace Editor
{

class TextLexer : public EditorObject_<TextLexer, Lexer>
{
    public: static Val ClassD_() { return CLASSD_text_lexer; }

    public: TextLexer(Val buffer)
        { init(buffer); }

    // [R]
    public: Val Run(Int iLimit)
    {
        Tracker* pTracker = m_tracker->StaticCast<Tracker>();
        if (xge(m_position, pTracker->m_start))
        {
            restart(pTracker->m_start);
        }

        pTracker->Reset();

        #if DEBUG_ANALYZE
            DEBUG_FORMAT("s=~S p=~D~%", m_state, m_position);
        #endif

        EnumCI oEnumCI(pTracker->GetBuffer(), m_position, iLimit);

        while (! oEnumCI.AtEnd())
        {
            Val ch = oEnumCI.Get();
            oEnumCI.Next();
            if (Character* p = ch->DynamicCast<Character>())
            {
                m_position = xxadd(m_position, one);
                transfer(p->ToCode());
            }
            else
            {
                // We may encounter IME.
                return nil;
            }
        } // while

        return oEnumCI.More() ? Qcontinue : nil;
    } // Run

    private: void transfer(char16 wch)
    {

     tryAgain:
        if (nil == m_state)
        {
            if (wch >= 'a' && wch <= 'z')
            {
                m_state = Kprotocol;
                startLexeme();
            }
            else
            {
                endLexeme(nil);
            }
            return;
        } // start

        if (Kprotocol == m_state)
        {
            if (wch >= 'a' && wch <= 'z')
            {
                // notthing to do
            }
            else if (wch >= '0' && wch <= '9')
            {
                // notthing to do
            }
            else if (':' == wch)
            {
                m_state = Character::FromCode(':');
            }
            else
            {
                m_state = nil;
                goto tryAgain;
            }
            return;
        } // if protocol

        if (Character::FromCode(':') == m_state)
        {
            if ('/' == wch)
            {
                m_state = Character::FromCode('/');
            }
            else
            {
                m_state = nil;
                goto tryAgain;
            }

            return;
        } // if ":"


        if (Character::FromCode('/') == m_state)
        {
            if ('/' == wch)
            {
                m_state = Kname;
            }
            else
            {
                m_state = nil;
                goto tryAgain;
            }

            return;
        } // if "/"

        if (Kname == m_state)
        {
            if (isUriSafe(wch))
            {
                return;
            }
            else
            {
                m_position = xxsub(m_position, one);
                endLexeme(Kuri);
                goto tryAgain;
            }
        } // if name
    } // transfer
}; // TextLexer

defmethod(analyze_buffer, text_lexer, (Val lexer, Val limit))
{
    return lexer->StaticCast<TextLexer>()->Run(Fixnum::Decode_(limit));
} // analyze_buffer

defmethod(make_lexer, text_mode, (Val, Val buffer))
{
    TextLexer* p = new TextLexer(buffer);
    Val lexer = p->Encode();
    return lexer;
} // make_lexer

} // Editor
