//////////////////////////////////////////////////////////////////////////////
//
// Editor - Generic Program Language Lexer
// editor/lexer/lexer_generic.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer.h#7 $
//
#if !defined(INCLUDE_editor_lexer_generic_h)
#define INCLUDE_editor_lexer_generic_h

#include "./lexer_defs.h"

namespace Editor
{

class GenericLexer : public Lexer
{
    // [P]
    protected: void processLexeme(Val);

    // [R]
    public: Val   Run(Int);

    // [T]
    protected: void transfer(Val, Val);
}; // GenericLexer

} // Editor

#endif //!defined(INCLUDE_editor_lexer_generic_h)
