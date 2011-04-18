#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Frame
// editor/rtl_frame.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_frame.cpp#4 $
//
//
#include "./rtl_defs.h"

#include "./ed_frame.h"
#include "./ed_window.h"

#include "./peer/peer_frame.h"

namespace Editor
{

defmethod(realize_instance, frame, (Thread* pth))
{
    Val frame = pth->mv_value[0];
    Frame* pFrame = frame->StaticCast<Frame>();
    pFrame->AttachPeer();

    Peer::Frame* pPeer = pFrame->GetPeer();

    foreach (Frame::EnumChild, oEnum, pFrame)
    {
        Val child = oEnum.Get();
        realize_instance(child);
        pPeer->AppendChild(child->StaticCast<Window>()->GetPeer());
    } // for window

    pPeer->Realize(NULL);

    return frame;
} // realize_instance

defmethod(unrealize_instance, frame, (Val frame))
{
    Frame* pFrame = frame->StaticCast<Frame>();

    pFrame->DetachPeer();

    VAR(AframesA) = delq(frame, VAR(AframesA));

    if (nil == VAR(AframesA))
    {
        // FIXME 2007-12-10 yosi@msn.com We need to call frame-destory-hooks
        // REVIEW 2008-08-17 yosi@msn.com We should call Application::Quit
        // instead of PostQuitMessage.
        ::PostQuitMessage(0);
    }

    return frame;
} // unrealize_instance

} // Editor
