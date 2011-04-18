//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime Functions
// editor/ed_text_window.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_text_window.h#3 $
//
#if !defined(INCLUDE_editor_text_window_h)
#define INCLUDE_editor_text_window_h

#include "./ed_window.h"

namespace Editor
{

class TextWindow :
    public Window_<
        TextWindow, 
        Layout_text_window,
        Peer::TextWindow >
{
    public: static Val ClassD_() { return CLASSD_text_window; }

    // ctor
    public: TextWindow(Val);

    // [G]
    public: Selection* GetSelection() const;
}; // TextWindow

CASSERT(sizeof(Layout_text_window) == sizeof(TextWindow));

} // Editor

#endif //!defined(INCLUDE_editor_text_window_h)
