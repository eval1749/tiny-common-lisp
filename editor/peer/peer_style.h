//////////////////////////////////////////////////////////////////////////////
//
// Editor - Style
// editor/ed_style.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_style.h#2 $
//
#if !defined(INCLUDE_visual_style_h)
#define INCLUDE_visual_style_h

#include "./peer_defs.h"

namespace Editor
{

namespace Peer
{

class Font;
class FormatParams;
class RenderStyle;

class RenderStyle
{
    public: COLORREF    m_crColor;
    public: COLORREF    m_crBackground;
    public: COLORREF    m_crMarker;
    public: Font*       m_pFont;

    public: Val m_font_family;
    public: Val m_font_size;
    public: Val m_font_style;
    public: Val m_font_weight;
    public: Val m_text_decoration;

    public: RenderStyle() :
        m_crColor(0),
        m_crBackground(0),
        m_crMarker(0),
        m_pFont(NULL),
        m_text_decoration(nil) {}

    // [C]
    public: Font* ComputeFont(HDC, char16);

    // [E]
    public: bool Equal(const RenderStyle* p) const;

    // [F]
    public: void FromInterval(Val);
    public: void FromStyle(Val);

    // [H]
    public: UInt Hash() const;

    // [I]
    private: Font* internFont(HDC, Val, int);
}; // RenderStyle

class Font
{
    private: HFONT      m_hFont;
    private: LOGFONTW   m_oLogFont;
    private: TEXTMETRIC m_tm;

    public: Font(HDC, LOGFONTW*);

    public: ~Font()
    {
        if (NULL != m_hFont)
        {
            ::DeleteObject(m_hFont);
        }
    } // ~Font

    public: operator HFONT() const
        { return m_hFont; }

    // [G]
    public: int GetAscent() const
        { return m_tm.tmAscent; }

    public: int GetCharWidth(HDC, char16) const;

    public: int GetDescent() const
        { return m_tm.tmDescent; }

    public: HFONT GetHandle() const
        { return m_hFont; }

    public: int GetHeight() const
        { return m_tm.tmHeight + m_tm.tmExternalLeading; }

    public: const LOGFONTW* GetLogFont() const
        { return &m_oLogFont; }

    public: int GetTextWidth(HDC, const char16*, int) const;

    public: int GetWidth() const
        { return m_tm.tmAveCharWidth; }

    // [H]
    public: bool HasGlyph(HDC hdc, char16 wch) const
    {
        DcSelect oSelectFont(hdc, m_hFont);
        WORD wIndex;
        DWORD cwch = ::GetGlyphIndicesW(
            hdc,
            &wch,
            1,
            &wIndex,
            GGI_MARK_NONEXISTING_GLYPHS );
        if (1 != cwch) return false;
        if (0xFFFF == wIndex) return false;
        if (wIndex == m_tm.tmDefaultChar) return false;
        return true;
    } // HasGlyph
}; // Font

} // Peer
} // Editor

#endif //!defined(INCLUDE_visual_style_h)
