#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Misc
// editor/ed_rtl_misc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_text_window.cpp#2 $
//
//
#include "./rtl_defs.h"

#include "./ed_buffer.h"
#include "./ed_selection.h"
#include "./ed_text_window.h"
#include "./peer/peer_container.h"
#include "./peer/peer_text_window.h"

namespace Editor
{

defmethod(realize_instance, text_window, (Thread* pth))
{
    Val window = pth->mv_value[0];
    TextWindow* pWindow = window->StaticCast<TextWindow>();
    pWindow->AttachPeer();
    return window;
} // realize_instance

defmethod(unrealize_instance, text_window, (Val window))
{
    TextWindow* pWindow = window->StaticCast<TextWindow>();

    pWindow->DetachPeer();

    Buffer* pBuffer = pWindow->GetSelection()->GetBuffer();

    pBuffer->m_windows = delq(window, pBuffer->m_windows);

    if (Container* pParent = pWindow->m_parent->DynamicCast<Container>())
    {
        pParent->m_child_windows = delq(window, pParent->m_child_windows);
        pWindow->m_parent = nil;
    }

    unrealize_instance(pWindow->m_blink);
    unrealize_instance(pWindow->m_range);
    unrealize_instance(pWindow->m_selection);

    return window;
} // unrealize_instance

} // Editor
