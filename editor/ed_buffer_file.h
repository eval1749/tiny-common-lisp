//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer File Support
// editor/ed_buffer_filer.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_file.h#5 $
//
#if !defined(INCLUDE_editor_buffer_file_h)
#define INCLUDE_editor_buffer_file_h

#include "./ed_buffer_undo.h"

namespace Editor
{

class BufferFile : public BufferUndo
{
    // ctor
    protected: BufferFile(Val);
    protected: BufferFile() {}

    // [C]
    public: void CheckWritable() const;

    // [G]
    public: const char16* GetFileName() const;
    
    public: const char16* GetName() const
        { return m_name->StaticCast<SimpleString>()->GetStart(); }

    // [I]
    public: bool IsModified() const
        { return zero != m_char_tick; }

    public: bool IsNotReady() const
        { return nil != m_state; }

    // [L]
    public: bool Load(const char16* = NULL, Val = Kdefault);

    // [Q]
    public: Val QueryFileState(bool = false);

    // [S]
    public: bool Save(const char16*);
    public: void SetFileName(const char16*);
}; // BufferFile


CASSERT(sizeof(Layout_buffer) == sizeof(BufferFile));

} // Editor

#endif //!defined(INCLUDE_editor_buffer_file_h)
