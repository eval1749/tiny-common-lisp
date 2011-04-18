//////////////////////////////////////////////////////////////////////////////
//
// Editor - Windows Peer - Application
// peer/peer_application.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_application.h#2 $
//
#if !defined(INCLUDE_visual_application_h)
#define INCLUDE_visual_application_h

#include "./peer_defs.h"

namespace Editor
{

namespace Peer
{

class Frame;

class Application
{
    private: static Application* sm_pApplication;

    // ctor
    public: Application();

    // [G]
    public: static Application* Get() 
        { return sm_pApplication; }

    // [O]
    public: bool OnIdle(uint);
}; // Application

} // Peer
} // Editor

#endif //!defined(INCLUDE_visual_application_h)
