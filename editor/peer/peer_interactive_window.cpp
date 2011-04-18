#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - Interactive Window
// editor/peer/peer_interactive_window.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_interactive_window.cpp#1 $
//
#include "./peer_interactive_window.h"

#include "../ed_window.h"

namespace Editor
{

namespace Peer
{

/// <summary>
///   Construct an InteractiveWindow
/// </summary>
InteractiveWindow::InteractiveWindow(Val window) :
    WithObject(window)
{
    // nothing to do
} // InteractiveWindow::InteractiveWindow

Val InteractiveWindow::GetActiveTick() const
    { return GetPeer()->m_active_tick; }

int InteractiveWindow::onCreate(CREATESTRUCT*)
{
    ::DragAcceptFiles(m_hwnd, true);
    return 0;
} // InteractiveWindow::onCreate

// <summary>
//   Handles WM_DROPFILES.
// </summary>
void InteractiveWindow::onDropFiles(HDROP hDrop, uint nIndex)
{
    ASSERT(NULL != hDrop);

    char16 wsz[MAX_PATH + 1];
    uint cwch = ::DragQueryFile(hDrop, nIndex, wsz, lengthof(wsz));
    if (0 == cwch)
    {
        return;
    }

    ::PostMessage(
        m_hwnd,
        PEER_WM_DROPFILES,
        reinterpret_cast<WPARAM>(hDrop),
        nIndex + 1 );

    Si::StackString oFilename(wsz);
    Val filename = funcall(Qpathname, oFilename);

    funcall(
        Qdispatch_event,
        VAR(Acommand_stateA),
        GetObject(),
        filename );
} // InteractiveWindow::onDropFiles

/// <summary>
///   Windows message handler.
/// </summary>
LRESULT InteractiveWindow::onMessage(
    uint    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case PEER_WM_DROPFILES:
        onDropFiles(
            reinterpret_cast<HDROP>(wParam),
            static_cast<uint>(lParam) );
        return 0;

    case WM_DROPFILES:
        ::PostMessage(m_hwnd, PEER_WM_DROPFILES, wParam, 0);
        return 0;

    case WM_SETFOCUS:
        GetPeer()->m_active_tick = xxadd(GetPeer()->m_active_tick, one);
        onSetFocus();
        return 0;
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // InteractiveWindow::onMessage

} // Peer
} // Editor
