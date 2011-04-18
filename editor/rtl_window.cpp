#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Misc
// editor/ed_rtl_misc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_window.cpp#5 $
//
//
#include "./rtl_defs.h"

#include "./ed_layout.h"
#include "./ed_objects.h"
#include "./ed_frame.h"
#include "./ed_window.h"

#include "./peer/peer_frame.h"
#include "./peer/peer_window.h"

namespace Editor
{

// [A]
defun(activate_window, (Val window))
{
    check_type(window, window);

    Window* pWindow = window->StaticCast<Window>();
    if (! pWindow->HasPeer())
    {
        error(Si::Qnot_realized, Kobject, window);
    }

    return pWindow->GetPeer()->Activate() ? window : nil;
} // activate_window

defun(add_window, (Val container, Val window))
{
    check_type(container, container);
    check_type(window, window);

    Window* pWindow = window->StaticCast<Window>();
    if (nil != pWindow->m_parent)
    {
        error("Can't move parent ~S", window);
    }

    Container* pContainer = container->StaticCast<Container>();

    pContainer->m_child_windows = nconc(
        pContainer->m_child_windows,
        list(window) );

    pWindow->m_parent = container;

    if (pContainer->HasPeer())
    {
        if (! pWindow->HasPeer())
        {
            realize_instance(window);
        }

        pContainer->GetPeer()->AppendChild(pWindow->GetPeer());
    } // if container is realized

    return container;
} // add_window

// [C]
defun(close_window, (Val window))
{
    check_type(window, window);

    HWND hwnd = window->StaticCast<Window>()->m_peer->To<Peer::Window>()->
        GetHwnd();

    ::DestroyWindow(hwnd);

    return window;
} // close_wndow


// [S]
defun(show_window, (Val window))
{
    check_type(window, window);
    Window* pWindow = window->StaticCast<Window>();
    if (Peer::Window* pPeerWindow = pWindow->m_peer->To<Peer::Window>())
    {
        ::ShowWindow(pPeerWindow->GetHwnd(), SW_SHOW);
        return window;
    }
    return nil;
} // show_window

} // Editor
