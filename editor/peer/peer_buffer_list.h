//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Window Peer
// eidtor/peer/peer_buffer_list.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_buffer_list.h#1 $
//
#if !defined(INCLUDE_editor_visual_buffer_list_h)
#define INCLUDE_editor_visual_buffer_list_h

#include "./peer_window.h"

#include "../ed_defs.h"

namespace Editor
{

namespace Peer
{

class BufferListWindow :
    public Window_<BufferListWindow, Window>
{
    public: static const char* Kind_() { return "BufferListWindow"; }
    
    private: enum CtrlId
    {
        CtrlId_ListView = 1,
    }; // CtrlId

    private: HWND   m_hwndListView;

    // ctor
    public: BufferListWindow();

    // [G]
    public: static BufferListWindow* Get();

    // [O]
    protected: virtual LRESULT onMessage(uint, WPARAM, LPARAM);

    // [R]
    public: void Refresh();
}; // BufferListWindow

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_visual_buffer_list_h)
