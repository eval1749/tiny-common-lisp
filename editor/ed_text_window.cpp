#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Window Object
// editor/ed_text_window.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_text_window.cpp#3 $
//
//
#include "./ed_text_window.h"

#include "./ed_buffer.h"
#include "./ed_range.h"
#include "./ed_selection.h"

#include "./peer/peer_text_window.h"

namespace Editor
{

// [T]
TextWindow::TextWindow(Val buffer)
{
    m_blink     = (new Range(buffer))->Encode();
    m_range     = (new Range(buffer))->Encode();
    m_selection = (new Selection(Encode(), buffer))->Encode();
} // TextWindow::TextWindow

Selection* TextWindow::GetSelection() const
    { return m_selection->StaticCast<Selection>(); }

} // Editor
