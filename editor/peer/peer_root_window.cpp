#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Windows Peer - Root Window
// editor/peer/peer_root_window.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_root_window.cpp#1 $
//
#include "./peer_root_window.h"

namespace Editor
{

namespace Peer
{

RootWindow* RootWindow::sm_pRootWindow;

// [E]
void RootWindow::Enable(bool fEnable) const
{
    foreach (EnumChild, oEnum, this)
    {
        Window* pWindow = oEnum.Get();
        if (HWND hwnd = *pWindow)
        {
            ::EnableWindow(hwnd, fEnable);
        }
    } // for each frame
} // RootWindow::Enable

} // Peer
} // Editor
