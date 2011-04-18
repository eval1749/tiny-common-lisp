//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer
// eidtor/vi_window.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_view.h#1 $
//
#if !defined(INCLUDE_editor_peer_rich_view_h)
#define INCLUDE_editor_peer_rich_view_h

#include "./peer_style.h"

namespace Editor
{

namespace Peer
{

namespace RichView
{

class RichStyle : public RenderStyle
{
    public: enum BorderStyle
    {
        BorderStyle_None,
        BorderStyle_Solid,
        BorderStyle_Dash,
    }; // BorderStyle

    public: COLORREF    m_crBorder;
    public: BorderStyle m_eBorder;
    public: Rect        m_rcBorder;
    public: Rect        m_rcMargin;
    public: Rect        m_rcPadding;

    public: RichStyle() :
        m_crBorder(RGB(0, 0, 0)),
        m_eBorder(BorderStyle_None) {}
}; // RichStyle

namespace Formatter
{

class Params
{
    public: RichStyle   m_oStyle;
    public: Size        m_sizeMin;
    public: Size        m_sizeMax;

    public: Params(Val);
}; // Params
} // Formatter

} // RichView
} // Peer
} // Editor

#include "./peer_rich_box.h"
#include "./peer_rich_node.h"

#endif //!defined(INCLUDE_editor_peer_rich_view_h)

