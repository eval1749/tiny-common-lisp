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
#include "./lexer_cxx.h"

#include "../cm_mode.h"

#include "../rtl_defs.h"

namespace
{

static const char*
k_rgpszCKeyword[] =
{
     "asm",
     "auto",
     "break",
     "case",
     "char",
     "const",
     "continue",
     "default",
     "do",
     "double",
     "else",
     "enum",
     "extern",
     "float",
     "for",
     "goto",
     "if",
     "int",
     "long",
     "nullptr",
     "register",
     "return",
     "short",
     "signed",
     "sizeof",
     "static",
     "struct",
     "switch",
     "typedef",
     "union",
     "unsigned",
     "void",
     "volatile",
     "while",
}; // k_rgpszCKeyword


//////////////////////////////////////////////////////////////////////
//
// C++ Keywords
//
static const char*
k_rgpszCxxKeyword[] =
{
    //  "and",
    //  "and_eq",
    //  "asm",      // C
    //  "auto",     // C
    //  "bitand",
    //  "bitor",
     "bool",
    //  "break",    // C
    //  "case",     // C
     "catch",
    //  "char",     // C
     "class",
    //  "compl",
    //  "const",    // C
     "const_cast",
    //  "continue", // C
    //  "default",  // C
     "delete",
    //  "do",       // C
    //  "double",   // C
     "dynamic_cast",
    //  "else",     // C
    //  "enum",     // C
     "explcit",
    //  "export",   // C
    //  "extern",   // C
     "false",
    //  "float",    // C
    //  "for",      // C
     "friend",
    //  "goto",     // C
    //  "if",       // C
     "inline",
    //  "int",      // C
     "namespace",
     "new",
    //  "not",      // C
    //  "not_eq",   // C
     "operator",
    //  "or",       // C
    //  "or_eq",    // C
     "private",
     "protected",
     "public",
    //  "register", // C
     "reinterpret_cast",
    //  "return",   // C
    //  "short",    // C
    //  "signed",   // C
    //  "sizeof",   // C
    //  "static",   // C
     "static_cast",
    //  "struct",   // C
    //  "switch",   // C
     "template",
     "this",
     "throw",
     "true",
     "try",
    //  "typedef",  // C
     "typeid",
     "typename",
    //  "union",    // C
    //  "unsigned", // C
     "using",
    //  "void",     // C
     "virtual",
    //  "volatile",
     "wchar_t",
    //  "while",
    //  "xor",
    //  "xor_eq",

    // Microsoft C++
     "__abstract",
     "__alignof",
     "__asm",
     "__assume",
     "__based",
     "__box",
     "__cdecl",
     "__declspec",
    //L"__delegate",
    //L"delegate",  context sensitive keyword
     "deprecated",
     "dllexport",
     "dllimport",
    //L"event",
    //L"__event",
     "__except",
     "__fastcall",
     "__finally",
     "__forceinline",
     "generic",
     "__inline",
     "__int8",
     "__int16",
     "__int32",
     "__int64",
     "__interface",
     "__Leave",
     "__m64",
     "__m128",
     "__m128d",
     "__m128i",
     "__raise",
     "__sealed",
     "sealed",
     "selectany",
     "__stdcall",
     "__super",
     "__unaligned",
     "__unhook",
     "__uuidof",
     "__value",
     "__w64",
     "wchar_t",

    // Vogue Extensions
     "foreach",
     "interface",
     "override",
     "unless",
     "when",
}; // k_rgpszCxxKeyword

} // namespace

namespace Editor
{

defmethod(analyze_buffer, cPP_lexer, (Val lexer, Val limit))
{
    return lexer->StaticCast<CxxLexer>()->Run(Fixnum::Decode_(limit));
} // analyze_buffer

defmethod(make_lexer, cPP_mode, (Val mode, Val buffer))
{
    Mode* pMode = mode->StaticCast<Mode>();
    if (nil == pMode->m_keywords)
    {
        Val keywords = make_hash_table(Ktest, Qequal);
        pMode->m_keywords = keywords;

        Val syntab = make_syntax_table(make_string("C++"));
        pMode->m_syntax_table = syntab;

        for (int ch = 0x20; ch <= 0x7E; ch++)
        {
            setf_char_syntax(
                Kpunctuation,
                Character::FromCode(static_cast<char16>(ch)),
                syntab );
        } // for ch

        for (int ch = '0'; ch <= '9'; ch++)
        {
            setf_char_syntax(
                Kword,
                Character::FromCode(static_cast<char16>(ch)),
                syntab );
        } // for ch

        for (int ch = 'A'; ch <= 'Z'; ch++)
        {
            setf_char_syntax(
                Kword,
                Character::FromCode(static_cast<char16>(ch)),
                syntab );
        } // for ch

        for (int ch = 'a'; ch <= 'z'; ch++)
        {
            setf_char_syntax(
                Kword,
                Character::FromCode(static_cast<char16>(ch)),
                syntab );
        } // for ch

        setf_char_syntax(
            Kspace,
            Character::FromCode(' '),
            syntab );

        setf_char_syntax(
            Kspace,
            Character::FromCode(Tab),
            syntab );

        setf_char_syntax(
            Kspace,
            Character::FromCode(LineFeed),
            syntab );

        setf_char_syntax(
            Kspace,
            Character::FromCode(Page),
            syntab );

        setf_char_syntax(
            Knewline,
            Character::FromCode(LineFeed),
            syntab );

        setf_char_syntax(
            Kword,
            Character::FromCode('_'),
            syntab );

        Val line_comment = list(Kline_comment);

        setf_cdr(
            list(Character::FromCode(LineFeed), Kend),
            line_comment );

        Val block_comment  = list(Kblock_comment);
        Val block_commentA = list(Kblock_comment);

        setf_cdr(
            list(Character::FromCode('*'), block_commentA),
            block_comment );

        setf_cdr(
            list(
                Character::FromCode('*'), block_commentA,
                Character::FromCode('/'), Kend,
                Qotherwise,               block_comment ),
            block_commentA );

        setf_char_syntax(
            list(Kpunctuation,
                    Character::FromCode('/'), line_comment,
                    Character::FromCode('*'), block_comment,
                    Qotherwise,               Kpunctuation ),
            Character::FromCode('/'),
            syntab );

        setf_char_syntax(
            list(Kstring,
                    Character::FromCode(DoubleQuote), Kend ),
            Character::FromCode(DoubleQuote),
            syntab );

        setf_char_syntax(
            list(Kstring,
                    Character::FromCode(SingleQuote), Kend ),
            Character::FromCode(SingleQuote),
            syntab );

        setf_gethash(Kkeyword, make_string("#define"), keywords);
        setf_gethash(Kkeyword, make_string("#endif"), keywords);
        setf_gethash(Kkeyword, make_string("#elif"), keywords);
        setf_gethash(Kkeyword, make_string("#else"), keywords);
        setf_gethash(Kkeyword, make_string("#error"), keywords);
        setf_gethash(Kkeyword, make_string("#if"), keywords);
        setf_gethash(Kkeyword, make_string("#ifdef"), keywords);
        setf_gethash(Kkeyword, make_string("#ifndef"), keywords);
        setf_gethash(Kkeyword, make_string("#include"), keywords);
        setf_gethash(Kkeyword, make_string("#pragma"), keywords);
        setf_gethash(Kkeyword, make_string("#undef"), keywords);

        // VC++ specific
        setf_gethash(Kkeyword, make_string("#import"),  keywords);
        setf_gethash(Kkeyword, make_string("#using"),   keywords);

        for (
            const char** p = k_rgpszCKeyword;
            p < k_rgpszCKeyword + lengthof(k_rgpszCKeyword);
            p++ )
        {
            setf_gethash(Kkeyword, make_string(*p), keywords);
        } // for p

        for (
            const char** p = k_rgpszCxxKeyword;
            p < k_rgpszCKeyword + lengthof(k_rgpszCxxKeyword);
            p++ )
        {
            setf_gethash(Kkeyword, make_string(*p), keywords);
        } // for p
    } // if

    CxxLexer* p = new CxxLexer(buffer);
    Val lexer = p->Encode();
    return lexer;
} // make_lexer

} // Editor
