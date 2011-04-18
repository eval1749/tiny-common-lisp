#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Page
// editor/vi_text_page.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_style.cpp#3 $
//
#define DEBUG_FONT_CACHE 1
#include "./peer_style.h"

#include "../ed_interval.h"
#include "../ed_style.h"

namespace Editor
{

namespace Peer
{


static int CALLBACK enumFontFamilyProc(
    ENUMLOGFONTEX*  pLogFont,
    NEWTEXTMETRICEX*,
    DWORD,
    LPARAM )
{
    ASSERT(NULL != pLogFont);
    return 0;
} // enumFontFamilyProc

Font::Font(HDC hdc, LOGFONTW* pLogFont)
{
    #if DEBUG_FONT_CACHE
        DEBUG_PRINTF("'%ls' height=%d\n", 
            pLogFont->lfFaceName,
            pLogFont->lfHeight );
    #endif

    // Note: PlatformSDK still use FONTENUMPROCW in WinGDI.h
    ::EnumFontFamiliesEx(
        hdc,
        pLogFont,
        reinterpret_cast<FONTENUMPROCW>(enumFontFamilyProc),
        0,
        0 );

    m_hFont = ::CreateFontIndirectW(pLogFont);
    if (NULL == m_hFont)
    {
        PlatformError("CreateFontIndirect");
    }

    ::CopyMemory(&m_oLogFont, pLogFont, sizeof(m_oLogFont));

    DcSelect oSelectFont(hdc, m_hFont);
    if (! ::GetTextMetrics(hdc, &m_tm))
    {
        PlatformError("GetTextMetrics");
    }
} // Font::Font

// [G]
int Font::GetCharWidth(HDC hdc, char16 wch) const
{
    // Is fixed ptich font?
    if (0 == (m_tm.tmPitchAndFamily & TMPF_FIXED_PITCH))
    {
        if (wch <= 0x7E) return m_tm.tmAveCharWidth;
    }

    return GetTextWidth(hdc, &wch, 1);
} // Font::GetCharWidth

int Font::GetTextWidth(HDC hdc, const char16* pwch, int cwch) const
{
    if (0 == cwch)
    {
        return 0;
    }

    DcSelect oSelectFont(hdc, m_hFont);

    SIZE size;
    if (! ::GetTextExtentPoint32W(hdc, pwch, cwch, &size))
    {
        return m_tm.tmAveCharWidth;
    }

    // Note: Below line may not work if font includes Kanji character and
    // ASCII character.
    return size.cx <= 0 ? m_tm.tmAveCharWidth : size.cx;
} // Font::GetTextWidth

Font* RenderStyle::ComputeFont(HDC hdc, char16 wch)
{
    int nFontSize = -::MulDiv(
        ::GetDeviceCaps(hdc, LOGPIXELSY), 
        Fixnum::Decode_(m_font_size), 
        72 );

    if (! consp(m_font_family))
    {
        Font* pFont = internFont(hdc, m_font_family, nFontSize);
        if (pFont->HasGlyph(hdc, wch))
        {
            return m_pFont = pFont;
        }
    }
    else
    {
        foreach (List::Enum, oEnum, m_font_family)
        {
            Val font_family = oEnum.Get();
            Font* pFont = internFont(hdc, font_family, nFontSize);
            if (pFont->HasGlyph(hdc, wch))
            {
                return m_pFont = pFont;
            }
        } // for font_family
    } // if

    return NULL;
} // RenderStyle::ComputeFont

// [E]
bool RenderStyle::Equal(const RenderStyle* p) const
{
    if (m_crColor         != p->m_crColor)         return false;
    if (m_crBackground    != p->m_crBackground)    return false;
    if (m_pFont           != p->m_pFont)           return false;
    if (m_text_decoration != p->m_text_decoration) return false;
    return true;
} // RenderStyle::Equal

// [F]
void RenderStyle::FromInterval(Val intv)
{
    Interval* p = intv->StaticCast<Interval>();
    Style*    r = VAR(AstyleA)->StaticCast<Style>();

    #define Choose(mp_field) \
        ( nil != p->m_ ## mp_field ? p->m_ ## mp_field : r->m_ ## mp_field )

    m_crColor      = Fixnum::Decode_(Choose(color));
    m_crBackground = Fixnum::Decode_(Choose(background));
    m_crMarker     = Fixnum::Decode_(Choose(marker));

    m_font_family     = Choose(font_family);
    m_font_size       = Choose(font_size);
    m_font_style      = Choose(font_style);
    m_font_weight     = Choose(font_weight);
    m_text_decoration = Choose(text_decoration);

    #undef Choose
} // RenderStyle::FromInterval

// Initialize render style from interval. Inherit style properties from
// previous interval or default interval.
void RenderStyle::FromStyle(Val intv)
{
    Style* p = intv->StaticCast<Style>();
    Style* r = VAR(AstyleA)->StaticCast<Style>();

    #define Choose(mp_field) \
        ( nil != p->m_ ## mp_field ? p->m_ ## mp_field : r->m_ ## mp_field )

    m_crColor      = Fixnum::Decode_(Choose(color));
    m_crBackground = Fixnum::Decode_(Choose(background));
    m_crMarker     = Fixnum::Decode_(Choose(marker));

    m_font_family     = Choose(font_family);
    m_font_size       = Choose(font_size);
    m_font_style      = Choose(font_style);
    m_font_weight     = Choose(font_weight);
    m_text_decoration = Choose(text_decoration);

    #undef Choose
} // RenderStyle::FromStyle

// [H]
UInt RenderStyle::Hash() const
{
    uint nHash = 0;
    nHash = ComputeHash(nHash, m_crColor);
    nHash = ComputeHash(nHash, m_crBackground);
    nHash = ComputeHash(nHash, m_crMarker);
    nHash = ComputeHash(nHash, reinterpret_cast<UInt>(m_pFont));
    return nHash;
} // RenderStyle::Hash

// [I]
Font* RenderStyle::internFont(HDC hdc, Val font_family, int nFontSize)
{
    class FontCache
    {
        private: enum Constants
        {
            InitialSize     = 101,
            RehashSize      = 130,
            RehashThreshold = 65,
        }; // Constatns

        private: struct Slot
        {
            Font*   m_pFont;

            Slot() : m_pFont(NULL) {}
        }; // Slot

        private: uint   m_nCount;
        private: uint   m_nSize;
        private: Slot*  m_prgpSlot;

        public: FontCache() :
            m_nCount(0),
            m_nSize(InitialSize),
            m_prgpSlot(new Slot[InitialSize]) {}

        // [G]
        public: Font* Get(const LOGFONTW* pLogFont) const
        {
            uint nHash = hashKey(pLogFont);
            const Slot* const pTop   = m_prgpSlot;
            const Slot* const pBtm   = m_prgpSlot + m_nSize;
            const Slot* const pStart = m_prgpSlot + nHash % m_nSize;

            const Slot* pRunner = pStart;
            do
            {
                if (Font* pFont = pRunner->m_pFont)
                {
                    if (keyEqual(pFont->GetLogFont(), pLogFont))
                    {
                        return pFont;
                    }
                }
                else
                {
                    return NULL;
                }

                pRunner++;
                if (pRunner == pBtm) pRunner = pTop;
            } while (pRunner == pStart);
            // Hash-table is full!
            CAN_NOT_HAPPEN();
        } // Get

        // [H]
        private: static uint hashKey(const LOGFONTW* pLogFont)
        {
            const uint32* pStart = reinterpret_cast<const uint32*>(
                pLogFont );

            const uint32* pEnd = reinterpret_cast<const uint32*>(
                pLogFont + 1 );

            UInt nHash = 0;
            for (const uint32* p = pStart; p < pEnd; p++)
            {
                nHash = ComputeHash(nHash, *p);
            } // for p

            return nHash;
        } // hashKey

        // [K]
        private: static bool keyEqual(
            const LOGFONTW* p,
            const LOGFONTW* q )
        {
            return 0 == ::memcmp(p, q, sizeof(*p));
        } // keyEqual

        // [P]
        public: Font* Put(Font* pFont)
        {
            uint nHash = hashKey(pFont->GetLogFont());
            Slot* const pTop   = m_prgpSlot;
            Slot* const pBtm   = m_prgpSlot + m_nSize;
            Slot* const pStart = m_prgpSlot + nHash % m_nSize;

            Slot* pRunner = pStart;
            do
            {
                if (NULL == pRunner->m_pFont)
                {
                    pRunner->m_pFont = pFont;
                    m_nCount += 1;

                    if (m_nCount * 100 >= m_nSize * RehashThreshold)
                    {
                        rehash();
                    }
                    return pFont;
                }

                pRunner++;
                if (pRunner == pBtm) pRunner = pTop;
            } while (pRunner == pStart);
            // Hash-table is full!
            CAN_NOT_HAPPEN();
        } // Put

        // [R]
        private: void rehash()
        {
            const Slot* const pTop   = m_prgpSlot;
            const Slot* const pBtm   = m_prgpSlot + m_nSize;

            m_nSize = m_nSize * RehashSize / 100;

            m_prgpSlot = new Slot[m_nSize];

            uint nRest = m_nCount;
            m_nCount = 0;
            for (const Slot* pRunner = pTop; pRunner < pBtm; pRunner++)
            {
                if (Font* pFont = pRunner->m_pFont)
                {
                    Put(pFont);
                    nRest -= 1;
                    if (0 == nRest) break;
                }
            } // for pRunner

            delete[] m_prgpSlot;
        } // rehash
    }; // FontCache

    static FontCache s_oFontCache;

    Int nCharSet  = DEFAULT_CHARSET;

    if (consp(font_family))
    {
        nCharSet = Fixnum::Decode_(cadr(font_family));
        font_family = car(font_family);
    }

    const char16* pwszFamily = L"Courier New";

    if (SimpleString* p = font_family->DynamicCast<SimpleString>())
    {
        pwszFamily = p->GetStart();
    }

    LOGFONTW oLogFont;

    oLogFont.lfHeight = nFontSize;
    oLogFont.lfCharSet = static_cast<BYTE>(nCharSet);
    oLogFont.lfItalic  = m_font_style == Kitalic;
    oLogFont.lfWeight  = m_font_weight == Kbold ? FW_BOLD : FW_NORMAL;

    ::ZeroMemory(oLogFont.lfFaceName, sizeof(oLogFont.lfFaceName));
    ::lstrcpyW(oLogFont.lfFaceName, pwszFamily);

    oLogFont.lfWidth = 0;
    oLogFont.lfEscapement = 0;
    oLogFont.lfOrientation = 0;
    oLogFont.lfUnderline    = 0;
    oLogFont.lfStrikeOut    = 0;
    oLogFont.lfOutPrecision = OUT_DEFAULT_PRECIS;
    oLogFont.lfClipPrecision = CLIP_DEFAULT_PRECIS;
    oLogFont.lfQuality = DEFAULT_QUALITY;
    oLogFont.lfPitchAndFamily = DEFAULT_PITCH;

    if (Font* pFont = s_oFontCache.Get(&oLogFont))
    {
        return pFont;
    }

    return s_oFontCache.Put(new Font(hdc, &oLogFont));
} // RenderStyle::internFont

} // Peer
} // Editor
