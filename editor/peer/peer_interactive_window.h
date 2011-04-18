//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Window Peer
// eidtor/peer/peer_text_window.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_interactive_window.h#1 $
//
#if !defined(INCLUDE_editor_peer_interactive_window_h)
#define INCLUDE_editor_peer_interactive_window_h

#include "./peer_window.h"

namespace Editor
{

namespace Peer
{

/// <summary>
///   Base class for window classes that accept user input.
/// </summary>
class InteractiveWindow :
    public Window_<InteractiveWindow>,
    public WithObject_<Editor::Window>
{
    public: static const char* Kind_() { return "InteractiveWindow"; }

    // ctor
    protected: InteractiveWindow(Val);

    // [G]
    public: Val GetActiveTick() const;

    // [O]
    protected: int              onCreate(CREATESTRUCT*);
    private:   void             onDropFiles(HDROP, uint);
    protected: override LRESULT onMessage(uint, WPARAM, LPARAM);
    protected: virtual void     onSetFocus() {}
}; // InteractiveWindow

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_interactive_window_h)
