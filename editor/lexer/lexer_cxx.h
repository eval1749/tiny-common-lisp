//////////////////////////////////////////////////////////////////////////////
//
// Editor - C++ Program Language Lexer
// editor/lexer/lexer_cxx.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer.h#7 $
//
#if !defined(INCLUDE_editor_lexer_cxx_h)
#define INCLUDE_editor_lexer_cxx_h

#include "./lexer_generic.h"

namespace Editor
{

class CxxLexer : public EditorObject_<CxxLexer, GenericLexer>
{
    public: static Val ClassD_() { return CLASSD_cPP_lexer; }

    public: CxxLexer(Val buffer)
        { init(buffer); }
}; // CxxLexer


} // Editor

#endif //!defined(INCLUDE_editor_lexer_cxx_h)
