//////////////////////////////////////////////////////////////////////////////
//
// Editor - Lexical Analyzer for Coloring
// editor/ed_buffer.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_lexer.h#1 $
//
#if !defined(INCLUDE_editor_lexer_defs_h)
#define INCLUDE_editor_lexer_defs_h

#include "../ed_defs.h"

namespace Editor
{

class Lexer : public Layout_lexer
{
    // [E]
    protected: void endLexeme(Val);

    protected: class EnumCI
    {
        private: Posn       m_cache_end;
        private: Posn       m_cache_start;
        private: Posn       m_end;
        private: Val        m_intv;
        private: Posn       m_posn;
        private: Buffer*    m_pBuffer;
        private: char16*    m_pwch;
        private: char16     m_rgwch[80];

        // ctor
        public: EnumCI(Buffer*, Posn, Int);

        // [A]
        public: bool AtEnd() const
            { return m_cache_start == m_cache_end; }

        // [F]
        private: void fill();

        // [G]
        public: Interval* GetInterval() const;
        public: Val       Get()  const;

        // [M]
        public: bool More() const;

        // [N]
        public: void Next();

        // [P]
        public: void Prev();
    }; // EnumCI

    // [G]
    public:    Buffer* GetBuffer() const;

    // [I]
    protected: void init(Val);

    // [R]
    protected: void restart(Posn);

    // [S]
    protected: void startLexeme();
}; // Lexer

} // Editor

#endif //!defined(INCLUDE_editor_lexer_defs_h)
