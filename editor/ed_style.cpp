#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Style
// editor/ed_style.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_style.cpp#1 $
//
//
#include "./ed_style.h"

namespace Editor
{

Style::Style()
{
    m_color = nil;
    m_background  = nil;
    m_marker      = nil;
    m_font_family = nil;
    m_font_size   = nil;
    m_font_style  = nil;
    m_font_weight = nil;
    m_text_decoration = nil;
} // Style

} // Editor
