//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - Tab Container
// eidtor/peer/peer_root_window.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_root_window.h#1 $
//
#if !defined(INCLUDE_editor_peer_root_window_h)
#define INCLUDE_editor_peer_root_window_h

#include "./peer_container.h"

namespace Editor
{

namespace Peer
{

class RootWindow : public Window_<RootWindow, Container>
{
    public: static const char* Kind_()  { return "RootWindow"; }

    private: static RootWindow* sm_pRootWindow;

    // ctor
    public: RootWindow()
    {
        ASSERT(NULL == sm_pRootWindow);
        sm_pRootWindow = this;
    } // RootWindow

    // [C]
    public: virtual bool CanClose() const override
        { CAN_NOT_HAPPEN(); }

    // [E]
    public: void Enable(bool = true) const;

    // [G]
    public: static RootWindow* Get() { return sm_pRootWindow; }
}; // RootWindow

} // Peer
} // Editor

#endif // !defined(INCLUDE_editor_peer_root_window_h)
