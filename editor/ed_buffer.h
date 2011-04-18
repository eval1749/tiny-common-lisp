//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer
// editor/ed_buffer.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer.h#8 $
//
#if !defined(INCLUDE_editor_buffer_h)
#define INCLUDE_editor_buffer_h

#include "./ed_buffer_file.h"

namespace Editor
{

class Buffer : public BufferFile
{
    public: static Val ClassD_() { return CLASSD_buffer; }

    // ctor
    public: Buffer(Val);
    protected: Buffer() {}

    // [C]
    public: Posn  ComputeEndOf(Unit, Posn) const;
    public: Posn  ComputeMotion(Unit, Val, Posn*) const;
    public: Posn  ComputeStartOf(Unit, Posn) const;
    public: Count ComputeWhile(Val, Val, Posn*) const;

    // [D]
    public: Count Delete(Posn, Posn);

    // [E]
    /// <summary>
    ///   Enumerate windows displaying this buffer.
    /// </summary>
    public: class EnumWindow : public List::Enum
    {
        public: EnumWindow(Buffer* pBuffer) :
            List::Enum(pBuffer->m_windows) {}
    }; // EnumWindow

    // [I]
    public: void Insert(Posn, char16, Count);
    public: void Insert(Posn, const char16*, size_t);

    public: void Insert(Posn lPosn, const char16* pwch)
        { Insert(lPosn, pwch, ::lstrlenW(pwch)); }

    // [M]
    private: Val mapStyle(Val);

    // [O]
    private: void onChange();
    public: bool OnIdle(uint);

    // [R]
    public: void Reload();

    // [S]
    public: void SetStyle(Posn, Posn, Val);
    public: void xSetStyle(Posn, Posn, ...);
}; // Buffer

CASSERT(sizeof(Layout_buffer) == sizeof(Buffer));

} // Editor

#endif //!defined(INCLUDE_editor_buffer_h)
