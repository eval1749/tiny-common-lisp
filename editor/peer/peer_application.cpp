#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Visual Application
// editor/vi_application.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_application.cpp#2 $
//
#include "./peer_application.h"

#include "../ed_frame.h"
#include "./peer_root_window.h"

namespace Editor
{

namespace Peer
{

Application* Application::sm_pApplication;

Application::Application()
{
    ASSERT(NULL == sm_pApplication);
    sm_pApplication = this;
    new RootWindow;
} // Application::Application

bool Application::OnIdle(uint nCount)
{
    bool fMore = false;
    foreach (RootWindow::EnumChild, oEnum, RootWindow::Get())
    {
        Window* pWindow = oEnum.Get();
        if (pWindow->OnIdle(nCount))
        {
            fMore = true;
        }
    } // for each frame
    return fMore;
} // Application::OnIdle

} // Peer
} // Editor
