#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Page
// editor/peer/peer_text_page.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_text_page.cpp#8 $
//
#define DEBUG_FORMATTER 0
#define DEBUG_MOTION    0
#define DEBUG_RENDER    0
#define DEBUG_SCROLL    0
#include "./peer_text_page.h"

#include "../ed_buffer.h"
#include "../ed_interval.h"
#include "../ed_selection.h"

#include "./peer_style.h"

namespace Editor
{

namespace Peer
{

namespace TextPageInternal
{

const int k_cyMinScroll = 50;

inline char16 toxdigit(int k)
{
    if (k <= 9) return static_cast<char16>(k + '0');
    return static_cast<char16>(k - 10 + 'A');
} // toxdigit

class EnumCI
{
    private: static const uint k_cwchCache = 1024;

    private: HANDLE         m_hHeap;
    private: const Buffer*  m_pBuffer;
    private: char16*        m_pwch;

    private: Posn       m_cache_end;
    private: Posn       m_cache_start;
    private: Val        m_intv;
    private: Posn       m_posn;

    // ctor
    public: EnumCI(HANDLE hHeap, const Buffer* pBuffer, Posn posn) :
        m_hHeap(hHeap),
        m_pBuffer(pBuffer),
        m_intv(pBuffer->GetIntervalAt(posn)),
        m_posn(posn)
    {
        fill();
    } // fill

    public: bool AtEnd() const
        { return m_cache_start == m_cache_end; }

    // [F]
    private: void fill()
    {
        m_pwch = reinterpret_cast<char16*>(
            ::HeapAlloc(m_hHeap, 0, sizeof(char16) * k_cwchCache) );

        if (NULL == m_pwch) PlatformError("HeapAlloc");

        uint cwch = m_pBuffer->GetText(
            m_posn,
            m_pwch,
            k_cwchCache );

        m_cache_start = m_posn;
        m_cache_end   = xadd(m_cache_start, cwch);
    } // fill

    // [G]
    public: Val GetInterval() const
        { return m_intv; }

    public: Posn GetPosn() const
        { return m_posn; }

    public: char16* GetPtr() const
        { ASSERT(! AtEnd()); return m_pwch; }

    public: void Next()
    {
        if (AtEnd())
        {
            return;
        }

        m_posn = xadd(m_posn, one);
        if (xge(m_posn, m_cache_end))
        {
            fill();
        }
        else
        {
            m_pwch++;
        }

        Interval* pIntv = m_intv->StaticCast<Interval>();
        if (! pIntv->In(m_posn))
        {
            ASSERT(nil != pIntv->m_next);
            m_intv = pIntv->m_next;
        }
    } // Next
}; // EnumCI

template<class T, class B = TextRun>
class TextRun_ : public WithCastable_<T, B>
{
    // ctor
    protected: TextRun_(Font* pFont, Posn start, int xLeft, int xRight)
    {
        ASSERT(xLeft < xRight);

        m_cy = pFont->GetHeight();
        m_cyAscent = pFont->GetAscent();
        m_cyDescent = pFont->GetDescent();
        m_xLeft = xLeft;
        m_xRight = xRight;
        m_end = start;
        m_start = start;
    } // TextRun_

    protected: TextRun_(Posn posn, int xLeft, int xRight)
    {
        ASSERT(xLeft < xRight);

        m_cy = 0;
        m_cyAscent = 0;
        m_cyDescent = 0;
        m_xLeft = xLeft;
        m_xRight = xRight;
        m_end = posn;
        m_start = posn;
    } // TextRun_
    
    protected: TextRun_() {}

    public: virtual const char* GetKind() const override
        { return T::Kind_(); }
}; // TextRun_

class Marker : public WithCastable_<Marker, TextRun>
{
    public: static const char* Kind_() { return "Marker"; }

    // ctor
    protected: Marker() {}

    protected: void init(
        HDC             hdc,
        RenderStyle*    pStyle,
        Posn            start,
        int             xLeft )
    {
        const char16 wchMarker = 'x';

        m_oStyle = *pStyle;

        Font* pFont = pStyle->ComputeFont(hdc, wchMarker);

        m_cy        = pFont->GetHeight();
        m_cyAscent  = pFont->GetAscent();
        m_cyDescent = pFont->GetDescent();

        m_xLeft  = xLeft;
        m_xRight = xLeft + pFont->GetCharWidth(hdc, wchMarker);

        m_start = start;
        m_end   = start;
    } // Marker
}; // Marker

template<class T, class B = Marker>
class Marker_ : public WithCastable_<T, B>
{
    protected: typedef Marker_<T, B> Base;

    protected: Marker_(
        HDC             hdc,
        RenderStyle*    pStyle,
        Posn            start,
        int             xLeft )
    {
        init(hdc, pStyle, start, xLeft);
    } // Marker_
}; // Marker_

class EofMarker : public Marker_<EofMarker>
{
    public: static const char* Kind_() { return "EofMarker"; }

    // ctor
    public: EofMarker(
        HDC             hdc,
        RenderStyle*    pStyle,
        Posn            start,
        int             xLeft ) :
            Base(hdc, pStyle, start, xLeft)
    {
        // Note: m_end is beyond end of buffer + 1 for displaying
        // caret at eof marker.
        m_end = xadd(m_start, 1);
    } // EofMarker

    #if 0
    // [M]
    public: int MapPosnToX(HDC, Posn posn, int* out_x) const
    {
        if (m_start != posn) return 0;
        *out_x = m_xLeft;
        return m_cy;
    } // MapPosnToX
    #endif

    // [R]
    public: virtual void Render(HDC hdc, const RECT* prc) const override
    {
        fillRect(hdc, prc, m_oStyle.m_crBackground);

        ::SetDCPenColor(hdc, m_oStyle.m_crMarker);
        //DcSelect oSelectPen(hdc, ::GetStockObject(DC_PEN));

        int yBottom = prc->bottom - m_cyDescent;
        //int yTop    = yBottom - m_cyAscent;
        int xLeft   = prc->left;
        int xRight  = prc->right;

        // Draw <-
        // FIXME 2007-06-13 We should get internal leading from font.
        int iInternalLeading = 3;
        int w = max(m_cyAscent / 6, 2);
        int y = yBottom - (m_cyAscent - iInternalLeading) / 2;
        drawHLine(hdc, xLeft, xRight, y);
        drawLine(hdc, xLeft + w, y - w, xLeft, y);
        drawLine(hdc, xLeft + w, y + w, xLeft, y);
    } // Render
}; // EofMarker

class EolMarker : public Marker_<EolMarker>
{
    public: static const char* Kind_() { return "EolMarker"; }

    // ctor
    public: EolMarker(
        HDC             hdc,
        RenderStyle*    pStyle,
        Posn            start,
        int             xLeft ) :
            Marker_<EolMarker>(hdc, pStyle, start, xLeft)
        {
            m_end = xadd(m_start, 1);
        } // EolMarker

    // [R]
    public: virtual void Render(HDC hdc, const RECT* prc) const override
    {
        fillRect(hdc, prc, m_oStyle.m_crBackground);

        ::SetDCPenColor(hdc, m_oStyle.m_crMarker);
        //DcSelect oSelectPen(hdc, ::GetStockObject(DC_PEN));

        int yBottom = prc->bottom - m_cyDescent;
        int yTop    = yBottom - m_cyAscent;
        int xLeft   = prc->left;
        int xRight  = prc->right;

        // Draw V
        yTop = yBottom - m_cyAscent * 3 / 5;
        int w = max((xRight - xLeft) / 6, 2);
        int x = xLeft + (xRight - xLeft) / 2;
        drawVLine(hdc, x, yTop, yBottom);
        drawLine(hdc, x - w, yBottom - w, x, yBottom);
        drawLine(hdc, x + w, yBottom - w, x, yBottom);
    } // Render
}; // EolMarker

class Filler : public TextRun_<Filler>
{
    public: static const char* Kind_() { return "Filler"; }

    public: Filler(
        COLORREF    crBackground,
        Posn        posn,
        int         xLeft,
        int         xRight ) :
            TextRun_<Filler>(posn, xLeft, xRight)
    {
        m_oStyle.m_crBackground = crBackground;
    } // Filler

    public: void Render(HDC hdc, const RECT* prc) const
    {
        fillRect(hdc, prc, m_oStyle.m_crBackground);
    } // Render
}; // Filler

class StringRun : public TextRun_<StringRun>
{
    public: static const char* Kind_() { return "StringRun"; }

    private: int            m_cwch;
    private: Font*          m_pFont;
    private: const char16*  m_pwch;

    // ctor
    public: StringRun(
        const RenderStyle*  pStyle,
        Font*               pFont,
        Val                 start,
        int                 xLeft,
        int                 cx,
        const char16*       pwch ) :
            m_cwch(1),
            m_pFont(pFont),
            m_pwch(pwch),
            TextRun_<StringRun>(pFont, start, xLeft, cx)
    {
        m_oStyle = *pStyle;
        m_end = xadd(m_start, 1);
    } // StringRun

    // [A]
    public: void AddChar(int cx)
    {
        m_cwch   += 1;
        m_xRight += cx;
        m_end = xadd(m_end, 1);
    } // AddChar

    // [C]
    public: bool CanMerge(
        const RenderStyle*  pStyle,
        const char16*       pwch ) const
    {
        if (m_pwch + m_cwch != pwch) return false;
        return m_oStyle.Equal(pStyle);
    } // CanMerge

    // [E]
    public: virtual bool Equal(const TextRun* p) const override
    {
        if (StringRun* q =  p->DynamicCast<StringRun>())
        {
            if (! TextRun::Equal(q)) return false;
            if (m_cwch != q->m_cwch) return false;
            return 0 == ::memcmp(m_pwch, q->m_pwch, sizeof(char16) * m_cwch);
        }
        return false;
    } // Equal

    // [H]
    public: UInt Hash() const
    {
        UInt nHash = TextRun::Hash();
        nHash = ComputeHash(nHash, m_cwch);
        for (int k = 0; k < m_cwch; k++)
        {
            nHash = ComputeHash(nHash, m_pwch[k]);
        } // for k
        return nHash;
    } // Hash

    // [M]
    public: Posn MapXToPosn(HDC hdc, int x) const
    {
        if (x < m_xLeft || x >= m_xRight) return nil;
        x -= m_xLeft;
        for (int k = 0; k < m_cwch; k++)
        {
            int cx = m_pFont->GetCharWidth(hdc, m_pwch[k]);
            x -= cx;
            if (x < 0) return xadd(m_start, k);
        } // for k
        return xsub(m_end, 1);
    } // MapXtoPosn

    public: int MapPosnToX(HDC hdc, Posn posn, int* out_x) const
    {
        if (xlt(posn, m_start)) return 0;
        if (xge(posn, m_end))   return 0;
        Int cwch = Fixnum::Decode_(xsub(posn, m_start));
        *out_x = m_xLeft + m_pFont->GetTextWidth(hdc, m_pwch, cwch);
        return m_cy;
    } // MapPosnToX

    // [R]
    public: virtual void Render(HDC hdc, const RECT* prc) const override
    {
        int y = prc->bottom - m_cyDescent;

#if 0
        if (m_oStyle.m_text_decoration != Knone)
        {
            y -= 1;
        }
#endif
        ::SetTextColor(hdc, m_oStyle.m_crColor);
        ::SetBkColor(hdc, m_oStyle.m_crBackground);
        DcSelect oSelect(hdc, *m_oStyle.m_pFont);

        // Note: We need to have ETO_CLIPPED. Some fonts need one more
        // pixel at left edge.
        ::ExtTextOutW(hdc, prc->left, y,
            ETO_OPAQUE | ETO_CLIPPED,
            prc, m_pwch, m_cwch, NULL );

        if (Kunderline == m_oStyle.m_text_decoration)
        {
            Pen oPen(PS_SOLID, 0, m_oStyle.m_crColor);
            DcSelect oSelect(hdc, oPen);
            drawHLine(hdc, prc->left, prc->right, y + 2);
        } // if underline

        #if SUPPORT_IME
        if (Kime_input == m_oStyle.m_text_decoration)
        {
            Pen oPen(PS_DOT, 0, m_oStyle.m_crColor);
            DcSelect oSelect(hdc, oPen);
            drawHLine(hdc, prc->left, prc->right - 4, y + 2);
        }
        else if (Kime_inactive0 == m_oStyle.m_text_decoration ||
                 Kime_inactive1 == m_oStyle.m_text_decoration )
        {
            Pen oPen(m_oStyle.m_crColor);
            DcSelect oSelect(hdc, oPen);
            drawHLine(hdc, prc->left, prc->right - 4, y + 2);
        }
        else if (Kime_active == m_oStyle.m_text_decoration)
        {
            Pen oPen(m_oStyle.m_crColor);
            DcSelect oSelect(hdc, oPen);
            drawHLine(hdc, prc->left, prc->right - 4, y + 1);
            drawHLine(hdc, prc->left, prc->right - 4, y + 2);
        }
        #endif // SUPPORT_IME
    } // Render
}; // StringRun

class TabMarker : public TextRun_<TabMarker>
{
    public: static const char* Kind_() { return "TabMarker"; }

    // ctor
    public: TabMarker(
        const RenderStyle*  pStyle,
        Font*               pFont,
        Posn                start,
        int                 xLeft,
        int                 xRight ) :
            TextRun_<TabMarker>(pFont, start, xLeft, xRight)
    {
        m_oStyle = *pStyle;
    } // TabMarker

    // [R]
    public: virtual void Render(HDC hdc, const RECT* prc) const override
    {
        fillRect(hdc, prc, m_oStyle.m_crBackground);

        ::SetDCPenColor(hdc, m_oStyle.m_crMarker);
        //DcSelect oSelectPen(hdc, ::GetStockObject(DC_PEN));

        int yBottom = prc->bottom - m_cyDescent;
        //int yTop    = yBottom - m_cyAscent;
        int xLeft   = prc->left;
        int xRight  = prc->right;

        // Draw |_|
        int w = max(m_cyAscent / 6, 2);
        drawHLine(hdc, xLeft + 2, xRight - 3, yBottom);
        drawVLine(hdc, xLeft + 2, yBottom, yBottom - w * 2);
        drawVLine(hdc, xRight - 3, yBottom, yBottom - w * 2);
    } // Render
}; // TabMarker

class UnprintableRun : public TextRun_<UnprintableRun>
{
    public: static const char* Kind_() { return "UnprintableRun"; }

    private: int    m_cwch;
    private: Font*  m_pFont;
    private: char16 m_rgwch[5];

    // ctor
    public: UnprintableRun(
        const RenderStyle*  pStyle,
        Font*               pFont,
        Posn                start,
        int                 xLeft,
        int                 xRight,
        const char16*       pwch,
        int                 cwch ) :
            m_pFont(pFont),
            m_cwch(cwch),
            TextRun_<UnprintableRun>(pFont, start, xLeft, xRight)
    {
        ASSERT(cwch >= 1 && cwch <= lengthof(m_rgwch));
        m_oStyle = *pStyle;
        ::CopyMemory(m_rgwch, pwch, sizeof(char16) * cwch);
    } // UnprintableRun

    // [E]
    public: virtual bool Equal(const TextRun* p) const override
    {
        ASSERT(m_cwch <= lengthof(m_rgwch));
        
        if (UnprintableRun* q = p->DynamicCast<UnprintableRun>())
        {
            if (! TextRun::Equal(q)) return false;
            if (m_cwch != q->m_cwch) return false;
            return 0 == ::memcmp(m_rgwch, q->m_rgwch, sizeof(char16) * m_cwch);
        }
        return false;
    } // Equal

    // [H]
    public: UInt Hash() const
    {
        UInt nHash = TextRun::Hash();
        nHash = ComputeHash(nHash, m_cwch);
        for (int k = 0; k < m_cwch; k++)
        {
            nHash = ComputeHash(nHash, m_rgwch[k]);
        } // for k
        return nHash;
    } // Hash

    // [R]
    public: virtual void Render(HDC hdc, const RECT* prc) const override
    {
        ::SetTextColor(hdc, m_oStyle.m_crMarker);
        ::SetBkColor(hdc, m_oStyle.m_crBackground);
        DcSelect oSelect(hdc, *m_oStyle.m_pFont);

        int y = prc->bottom - m_cyDescent;

        // Note: We need to have ETO_CLIPPED. Some fonts need one more
        // pixel at left edge.
        ::ExtTextOutW(hdc, prc->left, y,
            ETO_OPAQUE | ETO_CLIPPED,
            prc, m_rgwch, m_cwch, NULL );
    } // Render
}; // UnprintableRun

class WrapMarker : public Marker_<WrapMarker>
{
    public: static const char* Kind_() { return "WrapMarker"; }

    // ctor
    public: WrapMarker(
        HDC             hdc,
        RenderStyle*    pStyle,
        Posn            start,
        int             xLeft ) :
            Marker_<WrapMarker>(hdc, pStyle, start, xLeft) {}

    // [R]
    public: virtual void Render(HDC hdc, const RECT* prc) const override
    {
        fillRect(hdc, prc, m_oStyle.m_crBackground);

        int yBottom = prc->bottom - m_cyDescent;
        //int yTop    = yBottom - m_cyAscent;
        int xLeft   = prc->left;
        int xRight  = prc->right;

        //DcSelect oSelectPen(hdc, ::GetStockObject(DC_PEN));
        ::SetDCPenColor(hdc, m_oStyle.m_crMarker);

        // Draw ->
        xRight -= 1;
        int w = max(m_cyAscent / 6, 2);
        //int y = yTop + m_cyAscent / 2;
        int y = yBottom - w * 2;

        drawHLine(hdc, xLeft, xRight, y);
        drawLine(hdc, xRight - w, y - w, xRight, y);
        drawLine(hdc, xRight - w, y + w, xRight, y);
    } // Render
}; // WrapMarker

class Formatter
{
    private: HDC                    m_hdc;
    private: HANDLE                 m_hHeap;
    private: EnumCI                 m_oEnumCI;
    private: TextPage*              m_pPage;
    private: const FormatParams*    m_pParams;
    private: int                    m_x;

    public: Formatter(
        HDC                 hdc,
        TextPage*           pPage,
        const FormatParams* pParams,
        Posn                start ) :
            m_hdc(hdc),
            m_hHeap(pPage->m_hHeap),
            m_oEnumCI(pPage->m_hHeap, pParams->m_pBuffer, start),
            m_pPage(pPage),
            m_pParams(pParams) {}

    private: TextRun* formatChar(
        RenderStyle*    pStyle,
        TextRun*        pLastRun )
    {
        char16* pwch = m_oEnumCI.GetPtr();
        char16  wch  = *pwch;

        int xLeft = m_x;

        if (0x09 == wch)
        {
            Font* pFont = pStyle->ComputeFont(m_hdc, 'x');
            int cxTab = pFont->GetCharWidth(m_hdc, ' ');
            cxTab *= m_pParams->m_cwchTab;

            m_x = (m_x + cxTab - m_pParams->m_cxLeftMargin);
            m_x /= cxTab;
            m_x *= cxTab;

            return new(m_hHeap) TabMarker(
                pStyle,  
                pFont, 
                m_oEnumCI.GetPosn(),
                xLeft, 
                m_x );
        } // if tab

        if (Font* pFont = pStyle->ComputeFont(m_hdc, wch))
        {
            int cx = pFont->GetCharWidth(m_hdc, wch);
            int cxMarker = pFont->GetCharWidth(m_hdc, 'x');
            if (m_x + cx + cxMarker > m_pPage->GetRight())
            {
                return NULL;
            } // if

            m_x += cx;

            if (NULL != pLastRun)
            {
                if (StringRun* pRun = pLastRun->DynamicCast<StringRun>())
                {
                    if (pRun->CanMerge(pStyle, pwch))
                    {
                        pRun->AddChar(cx);
                        return pRun;
                    }
                } // if
            } // if

            return new(m_hHeap) StringRun(
                pStyle, 
                pFont,
                m_oEnumCI.GetPosn(),
                xLeft,
                m_x, 
                pwch );
        } // if printable

        // Unprintable character
        {
            char16 rgwch[5];
            int cwch;

            Font* pFont = pStyle->ComputeFont(m_hdc, 'x');
            if (wch < 0x20)
            {
                rgwch[0] = '^';
                rgwch[1] = static_cast<char16>(wch + 0x40);
                cwch = 2;
            }
            else
            {
                rgwch[0] = 'u';
                rgwch[1] = toxdigit((wch >> 12) & 15);
                rgwch[2] = toxdigit((wch >>  8) & 15);
                rgwch[3] = toxdigit((wch >>  4) & 15);
                rgwch[4] = toxdigit((wch >>  0) & 15);
                cwch = 5;
            }

            int cx = pFont->GetTextWidth(m_hdc, rgwch, cwch);
            cx += 6;

            int cxMarker = pFont->GetCharWidth(m_hdc, 'x');

            m_x += cx;

            if (m_x + cxMarker > m_pPage->GetRight())
            {
                return NULL;
            }

            return new(m_hHeap) UnprintableRun(
                pStyle,
                pFont,
                m_oEnumCI.GetPosn(),
                xLeft,
                m_x,
                rgwch,
                cwch );
        } // unprintable
    } // TextLine::formatChar

    public: TextLine* FormatLine()
    {
        TextLine* pLine = new(m_hHeap) TextLine;

        m_x = m_pPage->GetLeft();

        {
            int x = m_x;
            m_x += m_pParams->m_cxLeftMargin; 
            pLine->AddRun(new(m_hHeap) Filler(
                m_pParams->m_crBackground,
                m_oEnumCI.GetPosn(),
                x,
                m_x ) );
        }
            
        for (;;)
        {
            RenderStyle oStyle;

            if (m_pParams->IsInSelection(m_oEnumCI.GetPosn()))
            {
                oStyle = m_pParams->m_oSelectionStyle;
            }
            else
            {
                oStyle.FromInterval(m_oEnumCI.GetInterval());
            }

            if (m_oEnumCI.AtEnd())
            {
                pLine->AddRun(new(m_hHeap) EofMarker(
                    m_hdc, 
                    &oStyle,
                    m_oEnumCI.GetPosn(),
                    m_x ) );
                break;
            } // if

            if (0x0A == *m_oEnumCI.GetPtr())
            {
                pLine->AddRun(new(m_hHeap) EolMarker(
                    m_hdc, 
                    &oStyle,
                    m_oEnumCI.GetPosn(),
                    m_x ) );

                m_oEnumCI.Next();
                break;
            } // if

            TextRun* pTextRun = formatChar(&oStyle, pLine->GetLastRun());
            if (NULL == pTextRun)
            {
                pLine->AddRun(new(m_hHeap) WrapMarker(
                    m_hdc, 
                    &oStyle,
                    m_oEnumCI.GetPosn(),
                    m_x ) );
                break;
            } // if

            pLine->AddRun(pTextRun);
            m_oEnumCI.Next();
        } // for

        pLine->Finalize();
        return pLine;
    } // FormatLine

    public: bool FormatPage(Val target)
    {
        int yLine = m_pPage->GetTop();
        for (;;)
        {
            TextLine* pLine = FormatLine();
            m_pPage->AppendLine(pLine);
            yLine += pLine->GetHeight();

            if (pLine->GetLastRun()->Is<EofMarker>())
            {
                // We are reached at end of buffer.
                return true;
            }

            if (yLine >= m_pPage->GetBottom())
            {
                // We've filled a page.
                if (m_pPage->IsPosnFullyVisible(target))
                {
                    return false;
                }

                if (! m_pPage->scrollUp())
                {
                    return false;
                }
            } // if
        } // for
    } // FormatPage
}; // Formatter

} // TextPageInternal

using namespace TextPageInternal;

/// <summary>
///   Returns true if this page need to be redraw.
/// </summary>
bool FormatParams::IsDirty(
    const FormatParams* p ) const
{
    ASSERT(NULL != p);

    if (m_tick != p->m_tick)
    {
        #if DEBUG_FORMATTER
            DEBUG_PRINTF("buffer is updated.\n");
        #endif
        return true;
    }

    if (m_rc.left   != p->m_rc.left ||
        m_rc.top    != p->m_rc.top ||
        m_rc.right  != p->m_rc.right ||
        m_rc.bottom != p->m_rc.bottom )
    {
        #if DEBUG_FORMATTER
            DEBUG_PRINTF("page size is changed.\n");
        #endif
        return true;
    }

    if (m_crBackground != p->m_crBackground)
    {
        #if DEBUG_FORMATTER
            DEBUG_PRINTF("background is changed.\n");
        #endif
        return true;
    } // background

    return false;
} // FormatParams::IsDirty

// TextPage ctor
TextPage::TextPage(const FormatParams* pParams) :
    m_cy(0),
    m_hHeap(NULL),
    m_oParams(*pParams)
{
    m_hHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
    if (NULL == m_hHeap)
    {
        PlatformError("HeapCreate");
    }
} // TextPage::TextPage

TextPage::TextPage(HANDLE hHeap, const FormatParams* pParams) :
    m_cy(0),
    m_hHeap(hHeap),
    m_oParams(*pParams) {}

TextPage::~TextPage()
{
    if (NULL != m_hHeap)
    {
        ::HeapDestroy(m_hHeap);
    }
} // TextPage::TextPage

// [A]
void TextLine::AddRun(TextRun* pRun)
{
    m_cy = max(m_cy, pRun->GetHeight());
    m_cyDescent = max(m_cyDescent, pRun->GetDescent());

    if (GetLast() != pRun)
    {
        ASSERT(NULL == GetFirst() || GetFirst()->GetEnd() <= pRun->GetStart());
        Append(pRun);
    }
} // TextLine::AddRun

// [C]
TextPage* TextPage::Create(
    HDC                 hdc,
    const FormatParams* pParams,
    Posn                start,
    Posn                target )
{
    #if DEBUG_RENDER
        DEBUG_FORMAT("start=~D target=~D~%", start, target);
    #endif

    HANDLE hHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
    if (NULL == hHeap) PlatformError("HeapCreate");

    TextPage* pTextPage = new(hHeap) TextPage(hHeap, pParams);

    #if DEBUG_RENDER
        DEBUG_PRINTF("%p heap=%p\n", pTextPage, hHeap);
    #endif

    pTextPage->Format(hdc, start, target);
    return pTextPage;
} // TextPage::Create

// [E]
bool TextLine::Equal(const TextLine* p) const
{
    if (m_nHash != p->m_nHash) false;
    TextRuns::Enum oEnum1(this);
    TextRuns::Enum oEnum2(p);
    for (;;)
    {
        if (oEnum1.AtEnd()) return oEnum2.AtEnd();
        if (oEnum2.AtEnd()) return false;
        if (! oEnum1.Get()->Equal(oEnum2.Get())) return false;
        oEnum1.Next();
        oEnum2.Next();
    } // for
} // TextLine::Equal

bool TextRun::Equal(const TextRun* p) const
{
    if (m_cy        != p->m_cy)        return false;
    if (m_cyAscent  != p->m_cyAscent)  return false;
    if (m_cyDescent != p->m_cyDescent) return false;
    if (m_xLeft     != p->m_xLeft)     return false;
    if (m_xRight    != p->m_xRight)    return false;
    if (GetKind()   != p->GetKind())   return false;
    if (! m_oStyle.Equal(&p->m_oStyle)) return false;
    return true;
} // TextRun::Equal

// [F]
void TextPage::fillBottom(HDC hdc, int y) const
{
    #if DEBUG_RENDER
        DEBUG_PRINTF("fillBottom y=%d\n", y);
    #endif

    if (y < GetBottom())
    {
        Rect rc(GetLeft(), y, GetRight(), GetBottom());
        fillRect(hdc, &rc, m_oParams.m_crBackground);
    } // if
#if 0
    // FIXME 2007-08-05 yosi@msn.com We should expose show/hide
    // ruler settings to both script and UI.

    // Ruler
    Font* pFont = FontSet::Get(hdc, m_pBuffer->GetDefaultStyle())->
        FindFont(hdc, 'x');

    RECT rc;
    // FIXME 2007-08-05 yosi@msn.com We should expose rule position to
    // user.
    rc.left  = m_rc.left + pFont->GetWidth() * 80;
    rc.top   = m_rc.top;
    rc.right =  rc.left + 1;
    rc.bottom = m_rc.bottom;
    fillRect(hdc, &rc, Color(200, 200, 200));
#endif
} // TextPage::fillBottom

void TextPage::fillRight(HDC hdc, const TextLine* pLine, int y) const
{
    Rect rc(
        pLine->GetLastRun()->GetRight(),
        y,
        GetRight(),
        y + pLine->GetHeight() );

    fillRect(hdc, &rc, m_oParams.m_crBackground);
} // TextPage::fillRight

void TextLine::Finalize()
{
    ASSERT(0 == m_nHash);
    foreach (TextRuns::Enum, oEnum, this)
    {
        TextRun* pRun = oEnum.Get();
        pRun->m_cy        = m_cy;
        pRun->m_cyDescent = m_cyDescent;
        m_nHash = ComputeHash(m_nHash, pRun->Hash());
    } // for each run
} // TextLine::Finalize

TextLine* TextPage::FindLine(Posn posn) const
{
    if (xge(posn, GetStart()) && xlt(posn, GetEnd()))
    {
        foreach (EnumLine, oEnum, this)
        {
            TextLine* pLine = oEnum.Get();
            if (xlt(posn, pLine->GetEnd())) return pLine;
        } // for each line
    } // if posn in page

    foreach (Lines::Enum, oEnum, &m_oLineCache)
    {
        TextLine* pLine = oEnum.Get();
        if (xlt(posn, pLine->GetStart())) break;
        if (xlt(posn, pLine->GetEnd())) return pLine;
    } // for each line

    return NULL;
} // TextPage::FindLine

// Note: start should be start of paragraph or start of screen line.
void TextPage::Format(HDC hdc, Posn start, Posn target)
{
    m_cy = 0;
    DeleteAll();

    Formatter oFormatter(hdc, this, &m_oParams, start);
    if (oFormatter.FormatPage(target))
    {
        if (m_cy + GetTop() > GetBottom())
        {
            // This page displays end of buffer partially, so we scroll
            // the page up for displaying end of buffer fully.
            scrollUp();
        }
    }
} // TextPage::Format

TextLine* TextPage::FormatLine(HDC hdc, Posn start)
{
    if (TextLine* pLine = getCacheStartOf(start))
    {
        return pLine;
    }

    Formatter oFormatter(hdc, this, &m_oParams, start);
    return oFormatter.FormatLine();
} // TextPage::FormatLine

// [G]
TextLine* TextPage::getCacheEndOf(Posn end)
{
    foreach (Lines::Enum, oEnum, &m_oLineCache)
    {
        TextLine* pPresent = oEnum.Get();
        if (pPresent->GetEnd() == end)
        {
            #if DEBUG_SCROLL
                DEBUG_FORMAT("Hit [~D,~D]~%", pPresent->GetStart(), end);
            #endif

            m_oLineCache.Delete(pPresent);
            return pPresent;
        }

        if (pPresent->GetStart() > end)
        {
            break;
        }
    } // for each line
    return NULL;
} // TextPage::getCacheEndOf

TextLine* TextPage::getCacheStartOf(Posn start)
{
    foreach (Lines::Enum, oEnum, &m_oLineCache)
    {
        TextLine* pPresent = oEnum.Get();
        if (pPresent->GetStart() == start)
        {
            #if DEBUG_SCROLL
                DEBUG_FORMAT("Hit [~D,~D]~%", start, pPresent->GetEnd());
            #endif

            m_oLineCache.Delete(pPresent);
            return pPresent;
        }

        if (pPresent->GetStart() > start)
        {
            break;
        }
    } // for each line
    return NULL;
} // TextPage::getCacheStartOf

// [H]
UInt TextRun::Hash() const
{
    UInt nHash = m_oStyle.Hash();
    nHash = ComputeHash(nHash,  m_cyAscent);
    nHash = ComputeHash(nHash,  m_cyDescent);
    nHash = ComputeHash(nHash,  m_cy);
    nHash = ComputeHash(nHash,  m_xLeft);
    nHash = ComputeHash(nHash,  m_xRight);
    nHash = ComputeHash(nHash,  reinterpret_cast<UInt>(GetKind()));
    return nHash;
} // TextRun::Hash

// [I]
/// <summary>
///   Returns true if this page is not needed to redraw with specified
///   format parameters.
/// </summary>
bool TextPage::IsClean(const FormatParams* const pNew) const
{
    const FormatParams* const pCur = &m_oParams;

    if (pCur->IsDirty(pNew))
    {
        return false;
    }

    // Caret
    if (pCur->m_selection_start == pCur->m_selection_end)
    {
        if (pNew->m_selection_start == pNew->m_selection_end)
        {
            #if DEBUG_FORMATTER
                DEBUG_FORMAT("~D => ~D~%",
                    pCur->m_selection_start,
                    pNew->m_selection_start );
            #endif
            return true;
        }

        if (pNew->m_selection_start >= GetEnd() ||
            pNew->m_selection_end   <  GetStart() )
        {
            #if DEBUG_FORMATTER
                DEBUG_PRINTF("caret => outside\n");
            #endif
            return true;
        }

        #if DEBUG_FORMATTER
            DEBUG_FORMAT("~D => [~D,~D]~%",
                pCur->m_selection_start,
                pNew->m_selection_start, pNew->m_selection_end );
        #endif
        return false;
    } // if

    // Outside
    if (pCur->m_selection_start >= GetEnd() ||
        pCur->m_selection_end   <  GetStart() )
    {
        if (pNew->m_selection_start == pNew->m_selection_end)
        {
            #if DEBUG_FORMATTER
                DEBUG_PRINTF("outside => caret\n");
            #endif
            return true;
        }

        if (pNew->m_selection_start >= GetEnd() ||
            pNew->m_selection_end   <  GetStart() )
        {
            #if DEBUG_FORMATTER
                DEBUG_PRINTF("outside => outside\n");
            #endif
            return true;
        }

        #if DEBUG_FORMATTER
            DEBUG_PRINTF("outside => inside\n");
        #endif
        return false;
    } // if

    if (pNew->m_selection_start == pNew->m_selection_end)
    {
        #if DEBUG_FORMATTER
            DEBUG_FORMAT("[~D,~D] => ~D~%",
                pCur->m_selection_start, pCur->m_selection_end,
                pNew->m_selection_start );
        #endif
        return false;
    }

    if (max(pCur->m_selection_start, GetStart()) !=
        max(pNew->m_selection_start, GetStart()) ||
        min(pCur->m_selection_end,   GetEnd()) !=
        min(pNew->m_selection_end,   GetEnd()) )
    {
        #if DEBUG_FORMATTER
            DEBUG_FORMAT("[~D,~D] => [~D,~D]~%",
                pCur->m_selection_start, pCur->m_selection_end,
                pNew->m_selection_start, pNew->m_selection_end );
        #endif
        return false;
    }

    if (! pCur->m_oSelectionStyle.Equal(&pNew->m_oSelectionStyle))
    {
        #if DEBUG_FORMATTER
            DEBUG_PRINTF("selection style is changed.\n");
        #endif
        return false;
    }

    #if DEBUG_FORMATTER
        DEBUG_PRINTF("clean\n");
    #endif
    return true;
} // TextPage::IsClean

/// <summary>
///   Returns true if specified position is fully visible in this page.
/// </summary>
bool TextPage::IsPosnFullyVisible(Posn posn) const
{
    if (posn < GetStart())
    {
        return false;
    }

    if (posn >= GetEnd())
    {
        return false;
    }

    int y = m_oParams.m_rc.top;
    foreach (EnumLine, oEnum, this)
    {
        const TextLine* pLine = oEnum.Get();
        if (xlt(posn, pLine->GetEnd()))
        {
            return y + pLine->GetHeight() <= GetBottom();
        }

        y += pLine->GetHeight();
    } // for each line

    return false;
} // TextPage::IsPosnFullyVisible

// [M]
Posn TextPage::MapPointToPosn(HDC hdc, POINT pt) const
{
    if (pt.y >= GetBottom())
    {
        return xsub(GetEnd(), 1);
    }

    int yLine = m_oParams.m_rc.top;
    foreach (EnumLine, oEnum, this)
    {
        const TextLine* pLine = oEnum.Get();

        if (pt.y < yLine)
        {
            return pLine->GetStart();
        }

        int yNext = yLine + pLine->GetHeight();
        if (pt.y < yNext)
        {
            return pLine->MapXToPosn(hdc, pt.x);
        } // if

        yLine = yNext;
    } // for each line

    return xsub(GetEnd(), 1);
} // TextPage::MapPointToPosn

/// <summary>
///   Map position to point.
/// </summary>
int TextPage::MapPosnToPoint(HDC hdc, Posn posn, POINT* out_pt) const
{
    if (xlt(posn, GetStart()))
    {
        #if DEBUG_MOTION
            DEBUG_PRINTF("posn=%d start=%d\n",
                Fixnum::Decode_(posn),
                Fixnum::Decode_(GetStart()) );
        #endif
        return 0;
    }

    if (xge(posn, GetEnd())) 
    {
        #if DEBUG_MOTION
            DEBUG_PRINTF("posn=%d end=%d\n",
                Fixnum::Decode_(posn),
                Fixnum::Decode_(GetEnd()) );
        #endif

        #if 0
            if (m_oParams.m_pBuffer->GetEnd() != posn)
            {
                return 0;
            }

            #if DEBUG_MOTION
                DEBUG_PRINTF("Posn is at the end of buffer.\n");
            #endif
        #endif
        return 0;
    } // if

    int y = GetTop();
    const TextLine* pLine = NULL;
    foreach (EnumLine, oEnum, this)
    {
        pLine = oEnum.Get();

        if (xlt(posn, pLine->GetEnd()))
        {
            break;
        }
        
        if (pLine->GetFirstRun()->Is<EofMarker>())
        {
            break;
        }

        y += pLine->GetHeight();
    } // for each line

    if (NULL == pLine)
    {
        // Why we are here?
        return 0;
    }

    int x;
    int cy = pLine->MapPosnToX(hdc, posn, &x);
    if (cy > 0)
    {
        if (NULL != out_pt)
        {
            out_pt->x = x;
            out_pt->y = y;
        }
    }
    return cy;
} // TextPage::MapPosnToPoint

int TextLine::MapPosnToX(HDC hdc, Posn posn, int* out_x) const
{
    foreach (TextLine::EnumRun, oEnum, this)
    {
        const TextRun* pRun = oEnum.Get();
        int cy = pRun->MapPosnToX(hdc, posn, out_x);
        if (cy > 0)
        {
            return cy;
        }
    } // for each run

    // Why are we here?
    return 0;
} // TextRun::MapPosnToX

Posn TextLine::MapXToPosn(HDC hdc, int x) const
{
    foreach (TextLine::EnumRun, oEnum, this)
    {
        const TextRun* pRun = oEnum.Get();
        Posn posn = pRun->MapXToPosn(hdc, x);
        if (nil != posn)
        {
            return posn;
        }
    } // for each run
    return xsub(GetEnd(), 1);
} // TextLine::MapXToPosn

/// <summary>
///   Map y position to line.
/// </summary>
TextLine* TextPage::MapYToLine(int y) const
{
    if (y <= 0)
    {
        return GetFirstLine();
    }

    int yLine = 0;
    foreach (EnumLine, oEnum, this)
    {
        const TextLine* pLine = oEnum.Get();
        if (y <= yLine)
        {
            return const_cast<TextLine*>(pLine);
        }

        yLine += pLine->GetHeight();
    } // for line

    return GetLastLine();
} // TextPage::MapYToLine

// [P]
void TextPage::putCache(TextLine* pLine)
{
    foreach (Lines::Enum, oEnum, &m_oLineCache)
    {
        TextLine* pPresent = oEnum.Get();
        if (pLine->GetStart() < pPresent->GetStart())
        {
            m_oLineCache.InsertBefore(pLine, pPresent);
            return;
        }
    } // for each line
    m_oLineCache.Append(pLine);
} // Textpage::putCache

// [R]
void TextLine::Render(HDC hdc, int yLine) const
{
    foreach (TextLine::EnumRun, oEnum, this)
    {
        const TextRun* pRun = oEnum.Get();
        pRun->Render(hdc, yLine);
    } // for each run
} // TextLine::Render

void TextPage::Render(HDC hdc) const
{
    #if DEBUG_RENDER
        DEBUG_PRINTF("hdc=%p\n", hdc);
    #endif

    setupDc(hdc);

    int yLine = m_oParams.m_rc.top;
    foreach (EnumLine, oEnum, this)
    {
        const TextLine* pLine = oEnum.Get();
        renderLine(hdc, pLine, yLine);
        yLine += pLine->GetHeight();
    } // for each line

    fillBottom(hdc, yLine);
} // TextPage::Render

void TextPage::Render(HDC hdc, const RECT* prc) const
{
    #if DEBUG_RENDER
        DEBUG_PRINTF("%p (%d,%d)-(%d,%d)\n", this,
            prc->left, prc->top,  prc->right, prc->bottom );
    #endif

    setupDc(hdc);
    Render(hdc);

    RECT rc;
    rc.top = GetTop();
    foreach (EnumLine, oEnum, this)
    {
        if (rc.top > prc->bottom)
        {
            return;
        }

        const TextLine* pLine = oEnum.Get();

        rc.left   = GetLeft();
        rc.bottom = rc.top + pLine->GetHeight();

        #if DEBUG_RENDER
            DEBUG_PRINTF("%p line.pt=%d@%d\n", this, rc.left, rc.top);
        #endif

        if (rc.bottom > prc->top)
        {
            foreach (TextLine::EnumRun, oEnum, pLine)
            {
                if (rc.left > prc->right)
                {
                    break;
                }

                const TextRun* pRun = oEnum.Get();
                rc.right = rc.left + pRun->GetWidth();

                if (rc.right > prc->left)
                {
                    pRun->Render(hdc, rc.top);
                }
            } // for each run

            // Draw right margin
            {
                const TextRun* pRun = pLine->GetLastRun();
                rc.left  = max(prc->left,  pRun->GetRight());
                rc.right = max(prc->right, GetRight());
                if (rc.left < rc.right)
                {
                    fillRect(hdc, &rc, m_oParams.m_crBackground);
                }
            }
        } // if

        rc.top += pLine->GetHeight();
    } // for each line

    // Draw page bottom
    {
        rc.left   = max(prc->left,   GetLeft());
        rc.right  = max(prc->right,  GetRight());
        rc.top    = max(prc->top,    rc.top);
        rc.bottom = max(prc->bottom, GetBottom());

        if (rc.left < rc.right && rc.top < rc.bottom)
        {
            fillRect(hdc, &rc, m_oParams.m_crBackground);
        }
    }
} // TextPage::Render

bool TextPage::Render(HDC hdc, HWND hwnd, const TextPage* pPage) const
{
    #if DEBUG_RENDER
        DEBUG_PRINTF("hdc=%p hwnd=%p page=%p\n", hdc, hwnd, pPage);
    #endif

    ASSERT(NULL != pPage);

    int yNew = m_oParams.m_rc.top;

    // Compute common start -- pNewStart is end of common start.
    TextLine* pCurStart = pPage->GetFirstLine();
    TextLine* pNewStart = GetFirstLine();
    {
        while (NULL != pNewStart && NULL != pCurStart)
        {
            if (! pNewStart->Equal(pCurStart)) break;
            yNew += pNewStart->GetHeight();
            pNewStart = pNewStart->GetNext();
            pCurStart = pCurStart->GetNext();
        } // while

        if (NULL == pNewStart && NULL == pCurStart)
        {
            // This page and pPage are same.
            #if DEBUG_RENDER
                DEBUG_PRINTF("%p and %p are same.\n", this, pPage);
            #endif
            return false;
        } // if
    }

    int yCur = yNew;

    // Compute common end -- pNewEnd is start of common end.
    TextLine* pCurEnd = pPage->GetLastLine();
    TextLine* pNewEnd = GetLastLine();

    if (m_cy == pPage->m_cy)
    {
        for (;;)
        {
            if (NULL == pNewEnd) break;
            if (NULL == pCurEnd) break;
            if (! pNewEnd->Equal(pCurEnd)) break;

            pNewEnd = pNewEnd->GetPrev();
            pCurEnd = pCurEnd->GetPrev();
        } // for
    } // if

    if (NULL != pCurEnd) pCurEnd = pCurEnd->GetNext();
    if (NULL != pNewEnd) pNewEnd = pNewEnd->GetNext();

    // We need to redraw pNewStart(inclusive) to pNewEnd(exclusive).
    setupDc(hdc);

    TextLine* pScrollEnd = NULL;
    TextLine* pScrollStart = NULL;

    if (NULL != pCurStart)
    {
        TextLine* pCurBtm = pCurEnd;
        if (NULL == pCurBtm &&
            pPage->m_cy + pPage->GetTop() > pPage->GetBottom() )
        {
            pCurBtm = pPage->GetLastLine();
        }

        renderAux(
            hwnd,
            pNewStart, pNewEnd, yNew,
            pCurStart, pCurBtm, yCur,
            &pScrollStart, &pScrollEnd );
    } // if

    int cRedraws = 0;

    if (NULL != pScrollStart)
    {
        // Render lines outside pScrollStart ... pScrollEnd.
        bool fRedraw = true;
        for (
            TextLine* pNewLine = pNewStart;
            pNewLine != pNewEnd;
            pNewLine = pNewLine->GetNext() )
        {
            ASSERT(NULL != pNewLine);

            if (pNewLine == pScrollStart)
            {
                fRedraw = false;
            }
            else if (pNewLine == pScrollEnd)
            {
                fRedraw = true;
            }

            if (fRedraw)
            {
                cRedraws += 1;
                renderLine(hdc, pNewLine, yNew);
            } // if

            yNew += pNewLine->GetHeight();
        } // for each line
    }
    else
    {
        int yCur = m_oParams.m_rc.top;
        TextLine* pCurLine = pCurStart;

        for (
            TextLine* pNewLine = pNewStart;
            pNewLine != pNewEnd;
            pNewLine = pNewLine->GetNext() )
        {
            ASSERT(NULL != pNewLine);

            while (NULL != pCurLine)
            {
                if (yCur >= yNew) break;
                yCur += pCurLine->GetHeight();
                pCurLine = pCurLine->GetNext();
            } // while

            bool fRedraw;

            if (NULL == pCurLine)
            {
                fRedraw = true;
            }
            else
            {
                fRedraw = yNew != yCur || ! pNewLine->Equal(pCurLine);
                yCur += pCurLine->GetHeight();
                pCurLine = pCurLine->GetNext();
            } // if

            if (fRedraw)
            {
                cRedraws += 1;
                renderLine(hdc, pNewLine, yNew);
            }

            yNew += pNewLine->GetHeight();
        } // for each line
    } // if

    while (NULL != pNewEnd)
    {
        yNew += pNewEnd->GetHeight();
        pNewEnd = pNewEnd->GetNext();
    } // while

    fillBottom(hdc, yNew);

    #if DEBUG_RENDER
        if (0 == cRedraws)
        {
            DEBUG_PRINTF("%p no redraw\n", this);
        }
        else
        {
            DEBUG_PRINTF(
                " %p"
                " redraw=%d"
                " fillBottom y=%d"
                " w[%d %d] s[%d %d]"
                " rc=(%d,%d)-(%d,%d)\n",
                this,
                cRedraws,
                yNew,
                GetStart(), GetEnd(),
                m_oParams.m_selection_start, m_oParams.m_selection_end,
                m_oParams.m_rc.left,
                m_oParams.m_rc.top,
                m_oParams.m_rc.right,
                GetBottom() );
        } // if
    #endif

    return cRedraws > 0;
} // TextPage::Render

void TextPage::renderAux(
    HWND        hwnd,
    TextLine*   pNewStart,
    TextLine*   pNewEnd,
    int         yNewStart,
    TextLine*   pCurStart,
    TextLine*   pCurEnd,
    int         yCurStart,
    TextLine**  out_pScrollStart,
    TextLine**  out_pScrollEnd ) const
{
    int yCurScroll = 0;
    int yNewScroll = 0;
    int cyScroll   = 0;

    TextLine* pScrollStart = NULL;
    TextLine* pScrollEnd   = NULL;

    int yNew = yNewStart;
    for (
        TextLine* pNewLine = pNewStart;
        pNewLine != pNewEnd;
        pNewLine = pNewLine->GetNext() )
    {
        int yCur = yCurStart;
        for (
            TextLine* pCurLine = pCurStart;
            pCurLine != pCurEnd;
            pCurLine = pCurLine->GetNext() )
        {
            if (yCur != yNew)
            {
                int cy = 0;
                TextLine* pNewRunner = pNewLine;
                TextLine* pCurRunner = pCurLine;
                do
                {
                    if (! pNewRunner->Equal(pCurRunner)) break;
                    cy += pNewRunner->GetHeight();
                    pNewRunner = pNewRunner->GetNext();
                    pCurRunner = pCurRunner->GetNext();
                } while (pNewEnd != pNewRunner && pCurEnd != pCurRunner);

                if (cyScroll < cy)
                {
                    cyScroll     = cy;
                    yNewScroll   = yNew;
                    yCurScroll   = yCur;
                    pScrollStart = pNewLine;
                    pScrollEnd   = pNewRunner;
                }
            } // if

            yCur += pCurLine->GetHeight();
        } // for each cur line

        yNew += pNewLine->GetHeight();
    } // for each new line

    *out_pScrollStart = NULL;
    *out_pScrollEnd   = NULL;

    if (cyScroll < k_cyMinScroll)
    {
        return;
    }

    RECT rc;
    rc.left   = m_oParams.m_rc.left;
    rc.right  = m_oParams.m_rc.right;
    rc.top    = yCurScroll;
    rc.bottom = rc.top + cyScroll;

    // Note:
    //  SW_INVALIDATE is needed for when source area is not
    //  visible, e.g. out of screen, other windows hides
    //  this window.
    int iRgn = ::ScrollWindowEx(
        hwnd,
        0,
        yNewScroll - yCurScroll,
        &rc,                // prcScroll,
        &m_oParams.m_rc,    // prcClip
        NULL,               // prgnUpdate
        NULL,               // prcUpdate
        SW_INVALIDATE );

    #if DEBUG_RENDER
    {
        DEBUG_PRINTF("scroll %d->%d+%d %d\n",
            yCurScroll, yNewScroll, cyScroll, iRgn );
    }
    #endif // DEBUG_RENDER

    if (ERROR == iRgn)
    {
        return;
    }

    *out_pScrollStart = pScrollStart;
    *out_pScrollEnd   = pScrollEnd;
} // TextPage::renderAux

// [S]
bool TextPage::ScrollDown(HDC hdc)
{
    if (GetStart() == zero)
    {
        // This page shows start of buffer.
        return false;
    }

    TextLine* pRefLine = GetFirstLine();

    TextLine* pLine = getCacheEndOf(pRefLine->GetStart());

    if (NULL == pLine)
    {
        Posn goal  = xsub(GetStart(), 1);
        Posn start = m_oParams.m_pBuffer->ComputeStartOf(Kparagraph, goal);

        #if DEBUG_SCROLL
            DEBUG_FORMAT("~X start=~D goal=~D~%",
                Fixnum::Encode(this), start, goal );
        #endif

        Formatter oFormatter(hdc, this, &m_oParams, start);

        for (;;)
        {
            pLine = oFormatter.FormatLine();

            ASSERT(pLine->GetEnd() <= pRefLine->GetStart());

            if (pLine->GetEnd() == pRefLine->GetStart())
            {
                break;
            }
        } // for
    } // if

    InsertLine(pLine, pRefLine);

    int cyPage = m_oParams.m_rc.GetHeight();
    while (m_cy - GetLastLine()->GetHeight() > cyPage)
    {
        if (! scrollDown())
        {
            break;
        }
    } // while

    return true;
} // TextPage::ScrollDown

bool TextPage::scrollDown()
{
    TextLine* pLastLine = GetLastLine();
    if (GetFirstLine() == pLastLine) return false;
    m_cy -= pLastLine->GetHeight();
    Delete(pLastLine);
    putCache(pLastLine);
    return true;
} // TextPage::scrollDown

/// <summary>
///   Scroll this page to specified position.
/// </summary>
bool TextPage::ScrollToPosn(HDC hdc, Posn posn)
{
    if (IsPosnFullyVisible(posn))
    {
        return false;
    }

    #if DEBUG_SCROLL
        DEBUG_FORMAT("[~D,~D] ~D~%", GetStart(), GetEnd(), posn);
    #endif

    Posn start = posn;

    if (zero != posn)
    {
        int cLines = 0;
        foreach (EnumLine, oEnum, this)
        {
            cLines += 1;
        } // for each line

        int cLines2 = max(cLines / 2, 1);

        if (xgt(posn, GetStart()))
        {
            for (int k = 0; k < cLines2; k++)
            {
                if (! ScrollUp(hdc)) return 0 != k;
                if (IsPosnFullyVisible(posn)) return true;
            } // for k
        }
        else
        {
            for (int k = 0; k < cLines2; k++)
            {
                if (! ScrollDown(hdc)) return 0 != k;
                if (IsPosnFullyVisible(posn)) return true;
            } // for k
        } // if

        // Format page from cLines2 above.
        for (int k = 0; k < cLines2; k++)
        {
            if (start == zero) break;

            start = m_oParams.m_pBuffer->
                ComputeStartOf(Kparagraph, xsub(start, 1));
        } // for k
    } // if

    Format(hdc, start, posn);

    // If this page shows end of buffer, we shows lines as mush as
    // possible to fit in page.
    if (GetLastLine()->GetLastRun()->Is<EofMarker>())
    {
        int cyPage = m_oParams.m_rc.GetHeight();
        if (m_cy + GetLastLine()->GetHeight() < cyPage)
        {
            while (IsPosnFullyVisible(posn))
            {
                if (! ScrollDown(hdc)) return true;
            } // while
            ScrollUp(hdc);
        }
    } // if

    return true;
} // TextPage::ScrollToPosn

bool TextPage::ScrollUp(HDC hdc)
{
    if (GetLastLine()->GetLastRun()->Is<EofMarker>())
    {
        // This page shows end of buffer.
        if (m_cy < m_oParams.m_rc.GetHeight())
        {
            return false;
        }
    }

    if (! scrollUp())
    {
        return false;
    }
    
    if (GetLastLine()->GetLastRun()->Is<EofMarker>())
    {
        return true;
    }

    Formatter oFormatter(hdc, this, &m_oParams, GetEnd());

    int cyPage = m_oParams.m_rc.GetHeight();

    do
    {
        TextLine* pLine = oFormatter.FormatLine();
        AppendLine(pLine);
    } while (m_cy < cyPage);

    return true;
} // TextPage::ScrollUp

bool TextPage::scrollUp()
{
    TextLine* pFirstLine = GetFirstLine();
    if (GetLastLine() == pFirstLine) return false;
    m_cy -= pFirstLine->GetHeight();
    Delete(pFirstLine);
    putCache(pFirstLine);
    return true;
} // TextPage::scrollUp

} // Peer
} // Editor
