//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime Functions
// editor/ed_frame.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_frame.h#2 $
//
#if !defined(INCLUDE_editor_frame_h)
#define INCLUDE_editor_frame_h

#include "./ed_window.h"

namespace Editor
{

class Frame :
    public Container_<Frame, Layout_frame, Peer::Frame>
{
    public: static Val ClassD_() { return CLASSD_frame; }
}; // Frame

CASSERT(sizeof(Layout_frame) == sizeof(Frame));

} // Editor

#endif //!defined(INCLUDE_editor_frame_h)
