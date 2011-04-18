#include "precomp.h"
// Note: You must include GdiPlus.h in precomp.h if USE_GDIPLUS is true.
//////////////////////////////////////////////////////////////////////////////
//
// Evita - Peer - Tab Band Control
// editor/peer/ctrl_TabBand.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/ctrl_TabBand.cpp#4 $
//
#define DEBUG_DRAG      1
#define DEBUG_DRAW      1
#define DEBUG_HOVER     0
#define DEBUG_MESSAGE   0
#define DEBUG_SELECT    1
#define DEBUG_TOOLTIP   0
#include "./ctrl_tabBand.h"

#include "../../tinycl/z_defs.h"
#include "../../tinycl/z_util.h"


#pragma comment(lib, "msimg32.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "uxtheme.lib")

#if USE_GDIPLUS
    #pragma comment(lib, "gdiplus.lib")
#endif

using namespace Common;

namespace TabBandPrivate
{

//  Layout of non-Theme
//
//  0 backaground
//  1 background
//  light gray
//  0
//  1
//  2
//  3
//  4
//  icon 0
//  icon 1
//  icon 2
//  icon 3
//  icon 4
//  icon 5              xx    xx  0
//  icon 6              xxx  xxx  1
//  icon 7               xxxxxx   2
//  icon 8                xxxx    3
//  icon 9                xxxx    4
//  icon 10              xxxxxx   5
//  icon 11             xxx  xxx  5
//  icon 12             xx    xx  7
//  icon 13                     0123456 gray black
//  icon 14
//  icon 15
//  0
//  1
//  2
//  3
//  gray
//  black

template<typename T> T abs(T x)
{
    return x < 0 ? -x : x;
} // abs

class DcSelect
{
    private: HDC        m_hdc;
    private: HGDIOBJ    m_hObj;

    public: DcSelect(HDC hdc, HFONT hFont) :
        m_hdc(hdc)
    {
        m_hObj = ::SelectObject(hdc, hFont);
    } // DcSelect

    public: DcSelect(HDC hdc, HGDIOBJ hGdiObj) :
        m_hdc(hdc)
    {
        m_hObj = ::SelectObject(hdc, hGdiObj);
    } // DcSelect

    public: ~DcSelect()
    {
        ::SelectObject(m_hdc, m_hObj);
    } // ~DcSelect
}; // DcSelect

class Font
{
    private: HFONT  m_hFont;
    private: int    m_iDescent;
    private: int    m_iHeight;

    public: Font() :
        m_hFont(NULL),
        m_iDescent(0),
        m_iHeight(0) {}

    public: ~Font()
    {
        if (NULL != m_hFont)
        {
            ::DeleteObject(m_hFont);
        }
    } // ~Font

    // operator
    public: operator HFONT() const { return m_hFont; }

    // [G]
    public: int GetDescent() const { return m_iDescent; }
    public: int GetHeight()  const { return m_iHeight; }

    // [i]
    public: bool Init(HDC hdc, HFONT hFont)
    {
        if (NULL != m_hFont)
        {
            ::DeleteObject(m_hFont);
        }

        m_hFont = hFont;

        DcSelect oSelect(hdc, m_hFont);

        TEXTMETRIC tm;
        if (! ::GetTextMetrics(hdc, &tm))
        {
            return false;
        }

        m_iHeight  = tm.tmHeight + tm.tmInternalLeading;
        m_iDescent = tm.tmDescent;

        return true;
    } // Font
}; // Font

struct Point : POINT
{
    Point(int ix = 0, int iy = 0)
    {
        x = ix;
        y = iy;
    } // Point

    Point(LPARAM lParam)
    {
        x = GET_X_LPARAM(lParam);
        y = GET_Y_LPARAM(lParam);
    } // Point
    
    Point(const POINT& pt)
    {
        x = pt.x;
        y = pt.y;
    }
}; // Point

struct Rect : RECT
{
    Rect(int il = 0, int it = 0, int ir = 0, int ib = 0)
    {
        left = il;
        top  = it;
        right = ir;
        bottom = ib;
    } // Rect

    Rect(const RECT* prc)
    {
        left = prc->left;
        top = prc->top;
        right  = prc->right;
        bottom = prc->bottom;
    } // Rect

    Rect(const RECT& rc)
    {
        left = rc.left;
        top = rc.top;
        right  = rc.right;
        bottom = rc.bottom;
    } // Rect

    Rect(const Rect& rc)
    {
        left = rc.left;
        top = rc.top;
        right  = rc.right;
        bottom = rc.bottom;
    } // Rect

    // [G]
    int GetHeight() const { return bottom - top; }
    int GetWidth()  const { return right - left; }
}; // Rect

class WindowDc
{
    private: HDC m_hdc;
    private: HWND m_hwnd;

    public: WindowDc(HWND hwnd) :
        m_hwnd(hwnd)
    {
        m_hdc = ::GetDC(hwnd);
    } // WindowDc

    public: ~WindowDc()
    {
        ::ReleaseDC(m_hwnd, m_hdc);
    } // ~WindowDc

    public: operator HDC() const { return m_hdc; }
}; // WindowDc

inline void drawLine(HDC hdc, int sx, int sy, int ex, int ey)
    { ::MoveToEx(hdc, sx, sy, NULL); ::LineTo(hdc, ex, ey); }

inline void drawHLine(HDC hdc, int sx, int ex, int y)
    { drawLine(hdc, sx, y, ex + 1, y); }

inline void drawVLine(HDC hdc, int x, int sy, int ey)
    { drawLine(hdc, x, sy, x, ey + 1); }

static void fillGradientRect(
    HDC         hdc,
    const RECT* prc,
    COLORREF    cr0,
    COLORREF    cr1 )
{
    TRIVERTEX rgoVert[2];
    rgoVert[0].x = prc->left;
    rgoVert[0].y = prc->top;
    rgoVert[0].Red   = GetRValue(cr0) << 8;
    rgoVert[0].Green = GetGValue(cr0) << 8;
    rgoVert[0].Blue  = GetBValue(cr0) << 8;
    rgoVert[0].Alpha = 0;

    rgoVert[1].x = prc->right;
    rgoVert[1].y = prc->bottom;
    rgoVert[1].Red   = GetRValue(cr1) << 8;
    rgoVert[1].Green = GetGValue(cr1) << 8;
    rgoVert[1].Blue  = GetBValue(cr1) << 8;
    rgoVert[1].Alpha = 0;

    GRADIENT_RECT oRect;
    oRect.UpperLeft  = 0;
    oRect.LowerRight = 1;

    ::GradientFill(hdc, rgoVert, 2, &oRect, 1, GRADIENT_FILL_RECT_V);
} // fillGradientRect

static HINSTANCE g_hInstance;

#if USE_GDIPLUS
    static uint         s_cInstances;
    static ULONG_PTR    s_GdiplusToken;
#endif

class CloseButton;
class Drag;
class MouseTracker;
class TabBand;
class TabElement;
class TabItem;

struct LayoutContext
{
    bool    m_fTheme;
    HDC     m_hdc;
    Font*   m_pFont;
    Rect    m_rc;
}; // LayoutContext

typedef ChildList_<TabElement, TabElement> TabElements;

class TabElement :
    public Castable_<TabElement>,
    public ChildItem_<TabElement, TabElement>,
    public TabElements
{
    public: enum Flag
    {
        Flag_None,

        Flag_Selected = 1 << 0,
        Flag_Hover    = 1 << 1,
    }; // State

    protected: bool         m_fDisplay;
    protected: Rect         m_rc;
    private:   uint         m_rgfFlag;

    // ctor
    protected: TabElement() :
        m_fDisplay(false),
        m_rgfFlag(0) {}

    // [C]
    public: virtual const Rect* ComputeLayout(LayoutContext*) = 0;

    // [D]
    public: virtual void DrawNormal(HDC, Font*) const = 0;

    public: virtual void DrawTheme(
        Gdiplus::Graphics&,
        Gdiplus::Font* ) const = 0;

    // [G]
    public: TabElement* GetParent()   const { return m_pParent; }
    public: const RECT* GetRect()     const { return &m_rc; }

    // [H]
    public: virtual TabElement* HitTest(POINT pt) const
    {
        return ::PtInRect(&m_rc, pt) ? const_cast<TabElement*>(this) : NULL;
    } // TabElement*

    // [I]
    public: bool IsDisplay() const
        { return m_fDisplay; }

    public: bool IsHover() const
        { return 0 != (m_rgfFlag & Flag_Hover); }

    public: bool IsSelected() const
        { return 0 != (m_rgfFlag & Flag_Selected); }

    // [S]
    public: bool SetDisplay(bool f)
        { return m_fDisplay = f; }

    public: void SetFlag(uint rgfFlag)
        { m_rgfFlag |= rgfFlag; }

    // [U]
    public: void UnsetFlag(uint rgfFlag)
        { m_rgfFlag &= ~rgfFlag; }
}; // TabElement

class CloseButton :
    public WithCastable_<CloseButton, TabElement>
{
    public: static const char* Kind_() { return "CloseButton"; }

    public: enum Constant
    {
        cxBox = 8,
        cyBox = 8,

        cxBoxTheme = 16,
        cyBoxTheme = 17,

        cxMarginRight = 8,
        cyMarginTop   = 10,
    }; // Constant

    // [C]
    public: override const Rect* ComputeLayout(LayoutContext* pLayout)
    {
        m_rc = pLayout->m_rc;

        if (pLayout->m_fTheme)
        {
            m_rc.right -= cxMarginRight;
            m_rc.left   = m_rc.right - cxBoxTheme;
            m_rc.top   += cyMarginTop;
            m_rc.bottom = m_rc.top + cyBoxTheme;
        }
        else
        {
            m_rc.right -= cxMarginRight;
            m_rc.left   = m_rc.right - cxBox;
            m_rc.top   += cyMarginTop;
            m_rc.bottom = m_rc.top + cyBox;
        }

        return &m_rc;
    } // ComputeLayout

    // [D]
    public: override void DrawNormal(HDC hdc, Font*) const
    {
        DcSelect oSelectBrush(hdc, ::GetSysColorBrush(COLOR_BTNTEXT));

        int xl = m_rc.left;
        int xr = m_rc.right;
        int y  = m_rc.top;

        ::PatBlt(hdc, xl + 0, y + 0, 2, 1, PATCOPY);
        ::PatBlt(hdc, xr - 2, y + 0, 2, 1, PATCOPY);

        ::PatBlt(hdc, xl + 0, y + 1, 3, 1, PATCOPY);
        ::PatBlt(hdc, xr - 3, y + 1, 3, 1, PATCOPY);

        ::PatBlt(hdc, xl + 1, y + 2, 6, 1, PATCOPY);
        ::PatBlt(hdc, xl + 2, y + 3, 4, 1, PATCOPY);
        ::PatBlt(hdc, xl + 2, y + 4, 4, 1, PATCOPY);
        ::PatBlt(hdc, xl + 1, y + 5, 6, 1, PATCOPY);

        ::PatBlt(hdc, xl + 0, y + 6, 3, 1, PATCOPY);
        ::PatBlt(hdc, xr - 3, y + 6, 3, 1, PATCOPY);

        ::PatBlt(hdc, xl + 0, y + 7, 2, 1, PATCOPY);
        ::PatBlt(hdc, xr - 2, y + 7, 2, 1, PATCOPY);
    } // DrawNormal

    public: override void DrawTheme(Gdiplus::Graphics&, Gdiplus::Font*) const;
}; // CloseButton

class Drag
{
    private: static HCURSOR sm_hArrow;
    private: static HCURSOR sm_hDrag;
    private: static HCURSOR sm_hStop;

    private: bool       m_fDrag;
    private: HWND       m_hwnd;
    private: TabItem*   m_pTab;
    private: Point      m_pt;

    public: Drag() :
        m_fDrag(false),
        m_hwnd(NULL),
        m_pTab(NULL) {}

    // [C]
    public: TabItem* Continue(TabItem* pHover, POINT pt)
    {
        if (::GetCapture() == m_hwnd)
        {
            if (NULL == m_pTab)
            {
                ASSERT(! m_fDrag);
                return NULL;
            }

            HCURSOR hCursor;

            if (m_pTab == pHover)
            {
                if (! m_fDrag)
                {
                    if (abs(pt.x - m_pt.x) < 5)
                    {
                        return m_pTab;
                    }

                    m_fDrag = true;
                }

                hCursor = loadDragCursor();
            }
            else if (NULL != pHover)
            {
                hCursor = loadDragCursor();
            }
            else
            {
                hCursor = loadStopCursor();
            }

            ::SetCursor(hCursor);
            return m_pTab;
        } // if capture

        if (m_fDrag)
        {
            m_fDrag = false;
            ::SetCursor(loadArrowCursor());
        }

        m_hwnd  = NULL;
        m_pTab  = NULL;
        return NULL;
    } // Continue

    // [G]
    public: TabItem* GetTab() const
        { return m_pTab; }

    // [L]
    private: static HCURSOR loadArrowCursor()
    {
        if (NULL == sm_hArrow)
        {
            sm_hArrow = ::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
        }
        return sm_hArrow;
    } // loadArrowCursor

    private: static HCURSOR loadStopCursor()
    {
        if (NULL == sm_hStop)
        {
            sm_hStop = ::LoadCursor(NULL, MAKEINTRESOURCE(IDC_NO));
        }
        return sm_hStop;
    } // loadStopCursor

    private: static HCURSOR loadDragCursor()
    {
        if (NULL == sm_hDrag)
        {
            sm_hDrag = loadDragCursorFromIE();
            if (NULL == sm_hDrag)
            {
                sm_hDrag = ::LoadCursor(NULL, MAKEINTRESOURCE(IDC_HAND));
            }
        }
        return sm_hDrag;
    } // loadCursor

    private: static HCURSOR loadDragCursorFromIE()
    {
        HMODULE hDll = ::LoadLibraryEx(
            L"ieframe.dll",
            NULL,
            LOAD_LIBRARY_AS_DATAFILE );
        if (NULL == hDll)
        {
            return NULL;
        }

        if (HCURSOR hCursor = ::LoadCursor(hDll, MAKEINTRESOURCE(643)))
        {
            return sm_hDrag = CopyCursor(hCursor);
        } // if

        return NULL;
    } // loadDragCursorFromIE

    // [S]
    public: void Start(HWND hwnd, TabItem* pTab, POINT pt)
    {
        ASSERT(NULL == m_hwnd);
        ASSERT(NULL == m_pTab);

        m_hwnd = hwnd;
        m_pTab = pTab;
        m_pt   = pt;
        ::SetCapture(hwnd);
    } // Prepae

    public: TabItem* Stop()
    {
        if (m_fDrag)
        {
            m_fDrag = false;
            ::SetCursor(loadArrowCursor());
        }

        TabItem* pTab = m_pTab;
        ::ReleaseCapture();
        m_hwnd = NULL;
        m_pTab = NULL;
        return pTab;
    } // Stop
}; // Drag

HCURSOR Drag::sm_hArrow;
HCURSOR Drag::sm_hDrag;
HCURSOR Drag::sm_hStop;

class TabItem :
    public WithCastable_<TabItem, TabElement>
{
    public: static const char* Kind_() { return "TabItem"; }

    public: enum Constant
    {
        cxMarginLeft       = 0,
        cxMarginRight      = 1,
        cyMarginBottom     = 0,
        cyMarginTop        = 2,

        cxEdgeLeft         = 2,
        cxEdgeRight        = 2,
        cyEdgeTop          = 2,
        cyEdgeBottom       = 0,

        cyNormalTop        = 3,

        cxIconMarginLeft   = 2,
        cxIcon             = 16,
        cxIconMarginRight  = 3,
        cyIconMarginBottom = 0,

        cxTextMarginLeft   = 3,
        cxTextMarginRight  = 4,
        cyTextMarginTop    = 0,
        cyTextMarginBottom = 4,

        cxCloseMarginLeft  = 0,
        cxCloseMarginRight = 7,
    }; // Constant

    private: int            m_cwch;
    private: HANDLE         m_hHeap;
    private: int            m_iImage;
    private: LPARAM         m_lParam;
    private: CloseButton    m_oCloseButton;
    private: char16*        m_pwsz;

    public: TabItem(HANDLE hHeap) :
        m_cwch(0),
        m_hHeap(hHeap),
        m_iImage(-1),
        m_lParam(NULL),
        m_pwsz(NULL)
    {
        ASSERT(NULL != m_hHeap);
    } // TabItem

    public: ~TabItem()
    {
        ASSERT(NULL != m_hHeap);
        if (NULL != m_pwsz) ::HeapFree(m_hHeap, 0, m_pwsz);
    } // ~TabItem


    // [C]
    public: override const Rect* ComputeLayout(LayoutContext* pLayout)
    {
        m_rc = pLayout->m_rc;

        if (IsSelected())
        {
            m_oCloseButton.ComputeLayout(pLayout);
        }
        return &m_rc;
    } // ComputeLayout

    // [D]
    public: override void DrawNormal(HDC hdc, Font* pFont) const
    {
        ::SetBkColor(hdc, ::GetSysColor(COLOR_BTNFACE));
        ::SetTextColor(hdc, ::GetSysColor(COLOR_BTNTEXT));

        if (IsSelected())
        {
            COLORREF cr0 = ::GetSysColor(COLOR_BTNHIGHLIGHT);
            COLORREF cr1 = ::GetSysColor(COLOR_BTNFACE);

            Rect rc = m_rc;
            rc.left   += cxMarginLeft;
            rc.right  -= cxMarginRight;
            rc.top    += cyMarginTop;

            if (! IsSelected())
            {
                rc.top += cyNormalTop;
            }

            rc.bottom -= cyMarginBottom;

            fillGradientRect(hdc, &rc, cr0, cr1);
        } // selected

        // drawLabel
        {
            Rect rc = m_rc;

            rc.left += cxMarginLeft + cxEdgeLeft;

            if (m_iImage >= 0)
            {
                rc.left  += cxIconMarginLeft;
                rc.left  += cxIcon;
                rc.left  += cxIconMarginRight;
            } // if image

            rc.left += cxTextMarginLeft;

            rc.right  -= cxTextMarginRight;
            rc.right  -= cxEdgeRight + cxMarginRight;

            rc.bottom -= cyTextMarginBottom + cyMarginBottom;

            if (IsSelected())
            {
                rc.bottom -= 1;

                rc.right -= cxCloseMarginLeft;
                rc.right -= CloseButton::cxBox;
                rc.right -= cxCloseMarginRight;
            }

            ::ExtTextOut(
                hdc,
                rc.left,
                rc.bottom - pFont->GetDescent(),
                ETO_CLIPPED,
                &rc,
                m_pwsz,
                m_cwch,
                NULL );
        } // drawLabel

        // Close
        if (IsSelected())
        {
            m_oCloseButton.DrawNormal(hdc, pFont);
        } // if selected

        // Edge
        {
            Rect rc = m_rc;
            rc.left   += cxMarginLeft;
            rc.right  -= cxMarginRight;
            rc.top    += cyMarginTop;

            if (! IsSelected())
            {
                rc.top += cyEdgeTop;
            }

            //rc.bottom -= cyMarginBottom + cyEdgeBottom;
            rc.bottom -= cyMarginBottom;

            ::DrawEdge(hdc, &rc, EDGE_RAISED, BF_TOP | BF_LEFT | BF_RIGHT);
        }
    } // DrawNormal

    public: override void DrawTheme(Gdiplus::Graphics&, Gdiplus::Font*) const;

    // [G]
    public: void GetItem(TCITEM* pItem)
    {
        if (pItem->mask & TCIF_IMAGE)
        {
            pItem->iImage = m_iImage;
        } // if image

        if (pItem->mask & TCIF_PARAM)
        {
            pItem->lParam = m_lParam;
        } // if param

        if (pItem->mask & TCIF_STATE)
        {
            pItem->dwState = 0;
            if (IsHover())
            {
                pItem->dwState |= TCIS_HIGHLIGHTED;
            }
            
            if (IsSelected())
            {
                pItem->dwState = TCIS_BUTTONPRESSED;
            }
        } // if state

        if (pItem->mask & TCIF_TEXT)
        {
            int cwch = min(pItem->cchTextMax - 1, m_cwch);
            if (cwch > 0)
            {
                ::CopyMemory(pItem->pszText, m_pwsz, sizeof(char16) * cwch);
                pItem->pszText[cwch] = 0;
            }
        } // if text
    } // GetItem

    public: const char16* GetString() const
        { return m_pwsz; }

    // [H]
    public: override TabElement* HitTest(POINT pt) const
    {
        if (TabElement* pElt = Base::HitTest(pt))
        {
            if (IsSelected())
            {
                if (TabElement* pClose = m_oCloseButton.HitTest(pt))
                {
                    return pClose;
                }
            } // if selected
            return pElt;
        }
        return NULL;
    } // HitTest

    // [S]
    public: bool SetItem(const TCITEM* pItem)
    {
        if (pItem->mask & TCIF_IMAGE)
        {
            m_iImage = pItem->iImage;
        } // if image

        if (pItem->mask & TCIF_PARAM)
        {
            m_lParam = pItem->lParam;
        } // if param

        if (pItem->mask & TCIF_STATE)
        {
            if (pItem->dwStateMask & TCIS_BUTTONPRESSED)
            {
                if (pItem->dwState & TCIS_BUTTONPRESSED)
                {
                    SetFlag(Flag_Selected);
                }
                else
                {
                    UnsetFlag(Flag_Selected);
                }

            } // if

            if (pItem->dwStateMask & TCIS_HIGHLIGHTED)
            {
                if (pItem->dwState & TCIS_HIGHLIGHTED)
                {
                    SetFlag(Flag_Hover);
                }
                else
                {
                    UnsetFlag(Flag_Hover);
                }

            } // if
        } // if state

        if (pItem->mask & TCIF_TEXT)
        {
            if (NULL != m_pwsz)
            {
                ::HeapFree(m_hHeap, 0, m_pwsz);
                m_pwsz = NULL;
            }

            m_cwch = ::lstrlenW(pItem->pszText);

            const int cb = sizeof(char16) * (m_cwch + 1);

            m_pwsz = reinterpret_cast<char16*>(::HeapAlloc(m_hHeap, 0, cb));
            if (NULL == m_pwsz)
            {
                return false;
            }

            ::CopyMemory(m_pwsz, pItem->pszText, cb);
        } // if text

        return true;
    } // SetItem
}; // TabItem

class TabListButton :
    public WithCastable_<TabListButton, TabElement>
{
    public: static const char* Kind_() { return "TabListButton"; }

    private: enum Dimension
    {
        cxBox         = 16,
        cxBoxTheme    = 15,
        cxMarginLeft  = 0,
        cxMarginRight = 2,
        cyMarginTop   = 3,
    }; // Dimension

    private: HMENU  m_hMenu;

    // ctor
    public: TabListButton() :
        m_hMenu(NULL) {}

    public: ~TabListButton()
    {
        if (NULL != m_hMenu)
        {
            ::DestroyMenu(m_hMenu);
        }
    } // ~TabListButton

    // [C]
    public: override const Rect* ComputeLayout(LayoutContext* pLayout)
    {
        m_rc = pLayout->m_rc;
        if (pLayout->m_fTheme)
        {
            m_rc.right = m_rc.left + cxBoxTheme;
        }
        else
        {
            m_rc.right = m_rc.left + cxBox;
        }

        return &m_rc;
    } // ComputeLayout

    // [D]
    //      0123456789012345
    //    0 ---------------B
    //    1 -wwwwwwwwwwwwwdB
    //    3 -w------------dB
    //    4 -w------------dB
    //    5 -w------------dB
    //    6 -w------------dB
    //    7 -w------------dB
    //    8 -w------------dB
    //    9 -w------------dB
    //   10 -w------------dB
    //   11 -w---BBBBB----dB
    //   12 -w----BBB-----dB
    //   13 -w-----B------dB
    public: override void DrawNormal(HDC hdc, Font*) const
    {
        Rect rc = m_rc;
        rc.top += cyMarginTop;

        DcSelect oSelectBrush(hdc, ::GetSysColorBrush(COLOR_BTNTEXT));

        int x = (rc.GetWidth() - 5) / 2 + rc.left;
        int y = rc.GetHeight() / 2 + rc.top;

        ::PatBlt(hdc, x + 0, y + 0, 5, 1, PATCOPY);
        ::PatBlt(hdc, x + 1, y + 1, 3, 1, PATCOPY);
        ::PatBlt(hdc, x + 2, y + 2, 1, 1, PATCOPY);

        ::DrawEdge(hdc, &rc, EDGE_RAISED, BF_TOP | BF_LEFT | BF_RIGHT);
    } // DrawNormal

    public: override void DrawTheme(Gdiplus::Graphics&, Gdiplus::Font*) const;

    // [G]
    public: HMENU GetMenu()
    {
        if (NULL == m_hMenu)
        {
            m_hMenu = ::CreatePopupMenu();
        }
        else
        {
            while (::GetMenuItemCount(m_hMenu) > 0)
            {
                ::DeleteMenu(m_hMenu, 0, MF_BYPOSITION);
            } // while
        }

        return m_hMenu;
    } // GetMenu
}; // TabListButton

class MouseTracker
{
    private: bool           m_fTracking;
    private: TabElement*    m_pHover;

    // ctor
    public: MouseTracker() :
        m_fTracking(false),
        m_pHover(NULL) {}

    // [A]
    public: void Abort()
    {
        m_fTracking = false;
        m_pHover    = NULL;
    } // Abort

    // [C]
    public: void Continue(HWND hwnd, TabElement* pHover)
    {
        if (! m_fTracking)
        {
            TRACKMOUSEEVENT oTrack;
            oTrack.cbSize    = sizeof(oTrack);
            oTrack.dwFlags   = TME_LEAVE;
            oTrack.hwndTrack = hwnd;
            if (! ::TrackMouseEvent(&oTrack))
            {
                return;
            }

            m_fTracking = true;
        } // if ! m_fTracking

        if (m_pHover == pHover)
        {
            return;
        }
        
        if (NULL != m_pHover)
        {
            m_pHover->UnsetFlag(TabElement::Flag_Hover);
            ::InvalidateRect(hwnd, m_pHover->GetRect(), false);
        }

        m_pHover = pHover;

        if (NULL != m_pHover)
        {
            m_pHover->SetFlag(TabElement::Flag_Hover);
            ::InvalidateRect(hwnd, m_pHover->GetRect(), false);
        }
    } // Continue

    // [G]
    public: TabElement* GetElement() const
        { return m_pHover; }

    // [S]
    public: void Stop(HWND hwnd)
    {
        if (NULL != m_pHover)
        {
            m_pHover->UnsetFlag(TabItem::Flag_Hover);
            ::InvalidateRect(hwnd, m_pHover->GetRect(), false);
        }
        Abort();
    } // Stop
}; // MouseTracker

class TabBand :
    public WithCastable_<TabBand, TabElement>
{
    public: static const char* Kind_() { return "TabBand"; }

    private: enum Constant
    {
        cxMarginLeft  = 2,
        cxMarginRight = 2,
        cyEdgeBottom  = 2,
    }; // Constant

    private: bool           m_fDirty;
    private: bool           m_fTheme;
    private: int            m_cTabs;
    private: int            m_cxMinTab;
    private: int            m_cxTab;
    private: HANDLE         m_hHeap;
    private: HWND           m_hwnd;
    private: HWND           m_hwndToolTips;
    private: Drag           m_oDrag;
    private: Font           m_oFont;
    private: MouseTracker   m_oMouseTracker;
    private: TabListButton  m_oTabListButton;
    private: TabItem*       m_pFocus;
    private: TabItem*       m_pSelected;
    private: int            m_xTab;

    #if USE_GDIPLUS
        private: Gdiplus::Font* m_pGdiplusFont;
    #endif

    // ctor
    private: TabBand(HWND hwnd) :
        #if USE_GDIPLUS
            m_pGdiplusFont(NULL),
        #endif
        m_cTabs(0),
        m_cxTab(0),
        m_hHeap(NULL),
        m_fTheme(false),
        m_hwnd(hwnd),
        m_hwndToolTips(NULL),
        m_pFocus(NULL),
        m_pSelected(NULL),
        m_xTab(0)
    {
        changeTheme();
        TabElements::Append(&m_oTabListButton);
    } // TabBand

    public: ~TabBand()
    {
    } // ~TabBand

    // opertaor
    public: operator HWND() const { return m_hwnd; }

    // [C]
    private: bool changeFont(HDC hdc)
    {
        LOGFONT lf;

        if (! ::SystemParametersInfo(
                SPI_GETICONTITLELOGFONT,
                sizeof(lf),
                &lf, 0) )
        {
            return false;
        }

        HFONT hFont = ::CreateFontIndirect(&lf);
        if (NULL == hFont)
        {
            return false;
        }

        if (! m_oFont.Init(hdc, hFont))
        {
            return false;
        }

        #if USE_GDIPLUS
            delete m_pGdiplusFont;
            m_pGdiplusFont = new Gdiplus::Font(hdc, hFont);
        #endif

        return true;
    } // changeFont

    private: void changeTheme()
    {
        m_fTheme = 0 != ::IsThemeActive();
    } // changeTheme

    private: const Rect* ComputeLayout(LayoutContext* pLayout)
    {
        Rect rc = m_rc;
        rc.left   += cxMarginLeft;
        rc.bottom -= cyEdgeBottom;

        int cxMargin = m_fTheme ? 0 : 3;

        foreach (TabElements::Enum, oEnum, this)
        {
            TabElement* pElement = oEnum.Get();

            if (pElement->Is<TabListButton>())
            {
                pElement->SetDisplay(m_cTabs >= 2);
            }
            else
            {
                pElement->SetDisplay(true);
            }

            int cx = 0;

            if (pElement->IsDisplay())
            {
                pLayout->m_rc.left  = rc.left;
                pLayout->m_rc.right = rc.left + m_cxMinTab;

                const Rect* prc = pElement->ComputeLayout(pLayout);

                cx = prc->GetWidth();
            } // if IsDisplay

            rc.right = rc.left + cx;
            rc.left = rc.right + cxMargin;
        } // for each tab

        return &m_rc;
    } // ComputeLayout

    private: void computeLayout(HDC hdc)
    {
        if (! m_fDirty)
        {
            return;
        }

        if (0 == m_cTabs)
        {
            return;
        }

        DcSelect oSelect(hdc, m_oFont);

        LayoutContext oLayout;
        oLayout.m_fTheme = m_fTheme;
        oLayout.m_hdc    = hdc;
        oLayout.m_pFont  = &m_oFont;
        oLayout.m_rc     = m_rc;

        ComputeLayout(&oLayout);
    } // computeLayout

    // [D]
    private: override void DrawNormal(HDC hdc, Font* pFont) const
    {
        DcSelect oSelectBrush(hdc, ::GetStockObject(DC_BRUSH));
        DcSelect oSelectFont(hdc, m_oFont);
        DcSelect oSelectPen(hdc, ::GetStockObject(DC_PEN));

        ::SetBkMode(hdc, TRANSPARENT);
        ::SetTextAlign(hdc, TA_BASELINE);

        ::FillRect(hdc, &m_rc, ::GetSysColorBrush(COLOR_BTNFACE));

        foreach (TabElements::Enum, oEnum, this)
        {
            TabElement* pElement = oEnum.Get();
            if (pElement->IsDisplay())
            {
                pElement->DrawNormal(hdc, pFont);
            }
        } // for each tab

        ::DrawEdge(hdc, const_cast<Rect*>(&m_rc), EDGE_RAISED, BF_BOTTOM);
    } // DrawNormal

    #if USE_GDIPLUS
    private: override void DrawTheme(Gdiplus::Graphics&, Gdiplus::Font*) const;
    #endif

    // [H]
    private: TabElement* hitTest(POINT pt) const
    {
        foreach (TabElements::Enum, oEnum, this)
        {
            if (TabElement* pElt = oEnum.Get()->HitTest(pt))
            {
                return pElt;
            }
        }
        return NULL;
    } // hitTest

    // [I]
    public: static BOOL Init(HINSTANCE hInstance)
    {
        WNDCLASSEXW oWC;
            oWC.cbSize          = sizeof(oWC);
            oWC.style           = CS_DBLCLKS |
                                  CS_BYTEALIGNCLIENT;
            oWC.lpfnWndProc     = windowProc;
            oWC.cbClsExtra      = 0;
            oWC.cbWndExtra      = 0;
            oWC.hInstance       = hInstance;
            oWC.hIcon           = NULL;
            oWC.hCursor         =
                ::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
            oWC.hbrBackground   = NULL;
            oWC.lpszMenuName    = NULL;
            oWC.lpszClassName   = WC_TABBANDCLASS;
            oWC.hIconSm         = NULL;

        g_hInstance = hInstance;

        return ::RegisterClassExW(&oWC);
    } // Init

    public: int InsertItem(const TCITEM* pItem, int iIndex)
    {
        TabItem* pNew = new TabItem(m_hHeap);
        if (NULL == pNew) return -1;
        if (! pNew->SetItem(pItem)) return -1;

        TabItem* pRef = mapIndexToTab(iIndex);
        if (NULL == pRef)
        {
            TabElements::Append(pNew);
        }
        else
        {
            TabElements::InsertBefore(pNew, pRef);
        }

        m_cTabs += 1;

        markDirty();

        return iIndex;
    } // InsertItem

    // [M]
    private: TabItem* mapIndexToTab(WPARAM wParam) const
        { return mapIndexToTab(static_cast<int>(wParam)); }

    private: TabItem* mapIndexToTab(int iIndex) const
    {
        if (iIndex < 0 || iIndex >= m_cTabs)
        {
            return NULL;
        }

        foreach (TabElements::Enum, oEnum, this)
        {
            if (TabItem* pTab = oEnum.Get()->DynamicCast<TabItem>())
            {
                if (0 == iIndex) return pTab;
                iIndex -= 1;
            }
        } // for each tab

        return NULL;
    } // mapIndexToTab

    private: int mapTabToIndex(TabItem* pTab) const
    {
        if (NULL == pTab)
        {
            return -1;
        }

        int iItem = 0;
        foreach (TabElements::Enum, oEnum, this)
        {
            if (TabItem* pTab = oEnum.Get()->DynamicCast<TabItem>())
            {
                if (m_pSelected == pTab)
                {
                    return iItem;
                }

                iItem  += 1;
            } // if
        } // for each item
        return -1;
    } // mapTabToIndex

    private: void markDirty()
    {
        if (! m_fDirty)
        {
            m_fDirty = true;

            #if DEBUG_DRAW
                DEBUG_PRINTF("%p\n", this);
            #endif

            ::InvalidateRect(m_hwnd, NULL, false);
        }
    } // markDirty

    // [O]
    private: LRESULT onCreate(CREATESTRUCT* pcs)
    {
        ASSERT(NULL != pcs);

        #if USE_GDIPLUS
            if (0 == s_cInstances)
            {
                Gdiplus::GdiplusStartupInput oInput;
                Gdiplus::GdiplusStartup(&s_GdiplusToken, &oInput, NULL);
            } // if s_cInstances
            s_cInstances += 1;
        #endif

        m_hHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
        if (NULL == m_hHeap)
        {
            return false;
        }

        m_fDirty = true;

        WindowDc hdc(m_hwnd);

        m_cxMinTab = ::GetDeviceCaps(hdc, LOGPIXELSX) * 2;

        // Set tab label font
        changeFont(hdc);

        return true;
    } // onCreate

    private: void onLButtonDown(POINT pt)
    {
        TabElement* pElt = hitTest(pt);
        if (NULL == pElt) return;

        if (TabItem* pTab = pElt->DynamicCast<TabItem>())
        {
            if (! pTab->IsSelected())
            {
                selectTab(pTab);
                // Note: We should start tab dragging, otherwise if
                // mouse pointer is in close box, onButtonUp close
                // the tab.
            } // if selected

            m_oDrag.Start(m_hwnd, pTab, pt);
        } // if tab
    } // onLButtonDown

    private: void onLButtonUp(POINT pt)
    {
        TabElement* pElt = hitTest(pt);
        if (TabItem* pTab = m_oDrag.Stop())
        {
            if (TabItem* pRef = pElt->DynamicCast<TabItem>())
            {
                // Move pTab before pRef
                if (pRef != pTab)
                {
                    TabElements::Delete(pTab);
                    TabElements::InsertBefore(pTab, pElt);
                    markDirty();
                }
            } // if pRef
        }
        else if (pElt->Is<CloseButton>())
        {
            sendNotify(TABBAND_NOTIFY_CLOSE);
        }
        else if (pElt->Is<TabListButton>())
        {
            ASSERT(m_cTabs >= 2);

            Point ptMouse(m_rc.left, m_rc.bottom);
            ::ClientToScreen(m_hwnd, &ptMouse);

            HMENU hMenu = m_oTabListButton.GetMenu();

            // Add Tab name to menu.
            bool fDisplay = TabElements::GetFirst()->IsDisplay();
            int iItem = 0;
            foreach (TabElements::Enum, oEnum, this)
            {
                TabItem* pTab = oEnum.Get()->DynamicCast<TabItem>();
                if (NULL == pTab)
                {
                    continue;
                }

                uint rgfFlag = MF_STRING;

                if (pTab->IsSelected())
                {
                    rgfFlag |= MF_CHECKED;
                }

                if (pTab->IsDisplay() != fDisplay)
                {
                    ::AppendMenu(hMenu, MF_SEPARATOR, 0, NULL);
                }

                ::AppendMenu(
                    hMenu,
                    rgfFlag,
                    iItem,
                    pTab->GetString() );

                iItem += 1;
            } // for each element

            ::TrackPopupMenuEx(
                hMenu,
                TPM_LEFTALIGN | TPM_TOPALIGN,
                ptMouse.x, ptMouse.y,
                m_hwnd,
                NULL );
        } // if
    } // onLButtonUp

    private: LRESULT onMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
    {
        switch (uMsg)
        {
        case WM_COMMAND:
            if (TabItem* pTab = mapIndexToTab(LOWORD(wParam)))
            {
                selectTab(pTab);
            }
            return 0;

        case WM_CREATE:
            return onCreate(reinterpret_cast<CREATESTRUCT*>(lParam));

        case WM_DESTROY:
            #if USE_GDIPLUS
                s_cInstances -= 1;
                if (0 == s_cInstances)
                {
                    Gdiplus::GdiplusShutdown(s_GdiplusToken);
                }
            #endif
            return 0;

        case WM_LBUTTONDOWN:
            onLButtonDown(Point(lParam));
            return 0;

        case WM_LBUTTONUP:
            onLButtonUp(Point(lParam));
            return 0;

        case WM_MOUSELEAVE:
            m_oMouseTracker.Stop(m_hwnd);
            return 0;

        case WM_MOUSEMOVE:
        {
            Point pt(lParam);

            TabElement* pHover= hitTest(pt);

            if (m_oDrag.Continue(pHover->DynamicCast<TabItem>(), pt))
            {
                // nothing to do
            }
            else
            {
                m_oMouseTracker.Continue(m_hwnd, pHover);
            }
            return 0;
        } // WM_MOUSEMOVE

        case WM_NCDESTROY:
            delete this;
            break;

        case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = ::BeginPaint(m_hwnd, &ps);

            computeLayout(hdc);

            if (m_fTheme)
            {
                Gdiplus::Graphics g0(hdc);
                Gdiplus::Bitmap bitmap(m_rc.GetWidth(), m_rc.GetHeight(), &g0);
                Gdiplus::Graphics g(&bitmap);
                DrawTheme(g, m_pGdiplusFont);
                g0.DrawImage(&bitmap, 0, 0);
            }
            else
            {
                DrawNormal(hdc, &m_oFont);
            }

            ::EndPaint(m_hwnd, &ps);
            m_fDirty = false;
            return 0;
        } // WM_PAINT

        case WM_SETTINGCHANGE:
            switch (wParam)
            {
            case SPI_SETICONTITLELOGFONT:
            case SPI_SETNONCLIENTMETRICS:
            {
                WindowDc dc(m_hwnd);
                changeFont(dc);
                break;
            } // SPI_SETICONTITLELOGFONT
            } // switch wParam

        case WM_SIZE:
        {
            // Handle WM_SIZE at window creation. We won't receive WM_SIZE
            // since we handle WM_WINDOWPOSCHANGED.
            HWND hwndParent = ::GetParent(m_hwnd);
            if (NULL == hwndParent)
            {
                return 0;
            }

            const int cyFont  = m_oFont.GetHeight();
            const int cyIcon  = ::GetSystemMetrics(SM_CYSMICON);
            const int cyLabel = max(cyFont, cyIcon);

            ::GetClientRect(hwndParent, &m_rc);
            //m_rc.bottom = m_rc.top + 2 + 1 + 5 + cyLabel + 4 + 2;
            m_rc.bottom = m_rc.top + 2 + 7 + cyLabel + 5 + 2;

            ::SetWindowPos(
                m_hwnd,
                NULL,
                m_rc.left,
                m_rc.top,
                m_rc.right  - m_rc.left,
                m_rc.bottom - m_rc.top,
                SWP_NOZORDER );

            return 0;
        } // WM_SIZE

        case WM_THEMECHANGED:
            changeTheme();
            return 0;

        case WM_WINDOWPOSCHANGED:
        {
            WINDOWPOS* wp = reinterpret_cast<WINDOWPOS*>(lParam);
            if (wp->flags & SWP_NOSIZE)
            {
                return 0;
            }

            m_rc.left   = wp->x;
            m_rc.top    = wp->y;
            m_rc.right  = wp->x + wp->cx;
            m_rc.bottom = wp->y + wp->cy;

            markDirty();
            return 0;
        } // WM_WINDOWPOSCHANGED

        ////////////////////////////////////////////////////////////
        //
        // Tab Control messages (TCM_*)
        //
        case TCM_DELETEITEM:
            if (TabItem* pTab = mapIndexToTab(wParam))
            {
                TabItem* pSelected = NULL;
                if (m_pSelected == pTab)
                {
                    if (TabItem* pAnother =
                            pTab->GetNext()->DynamicCast<TabItem>() )
                    {
                        pSelected = pAnother;
                    }
                    else if (TabItem* pAnother =
                                pTab->GetPrev()->DynamicCast<TabItem>() )
                    {
                        pSelected = pAnother;
                    }
                } // if selected

                if (m_pFocus == pTab)
                {
                    m_pFocus = NULL;
                }

                if (m_oDrag.GetTab() == pTab)
                {
                    m_oDrag.Stop();
                }

                if (m_oMouseTracker.GetElement() == pTab)
                {
                    m_oMouseTracker.Abort();
                }

                TabElements::Delete(pTab);
                m_cTabs -= 1;

                markDirty();

                if (NULL != pSelected)
                {
                    selectTab(pSelected);
                }

                return true;
            } // if pTab
            return false;

        case TCM_GETCURFOCUS:
            return mapTabToIndex(m_pFocus);

        case TCM_GETCURSEL:
            return mapTabToIndex(m_pSelected);

        case TCM_GETITEM:
            if (TabItem* pTab= mapIndexToTab(wParam))
            {
                pTab->GetItem(reinterpret_cast<TCITEM*>(lParam));
                return 1;
            } // if item
            return 0;

        case TCM_GETITEMCOUNT:
            return m_cTabs;

        case TCM_GETTOOLTIPS:
            return reinterpret_cast<LRESULT>(m_hwndToolTips);

        case TCM_INSERTITEM:
            return InsertItem(
                reinterpret_cast<TCITEM*>(lParam),
                static_cast<int>(wParam) );

        case TCM_SETCURFOCUS:
            if (TabItem* pTab = mapIndexToTab(wParam))
            {
                if (m_pFocus != pTab)
                {
                    if (NULL != m_pFocus)
                    {
                        m_pFocus->UnsetFlag(TabItem::Flag_Hover);
                    }

                    m_pFocus = pTab;
                    m_pFocus->SetFlag(TabItem::Flag_Hover);

                    markDirty();
                }
            }
            return 0;

        case TCM_SETCURSEL:
            if (TabItem* pTab = mapIndexToTab(wParam))
            {
                selectTab(pTab);
            } // if tab
            return 0;

        case TCM_SETMINTABWIDTH:
        {
            int cxPrev = m_cxMinTab;
            m_cxTab = cxPrev;
            return cxPrev;
        } // TCM_SETMINTABWIDTH

        case TCM_SETTOOLTIPS:
            m_hwndToolTips = reinterpret_cast<HWND>(wParam);
            return 0;
        } // switch uMsg

        return ::DefWindowProc(m_hwnd, uMsg, wParam, lParam);
    } // onMessage

    // [S]
    private: void selectTab(TabItem* pTab)
    {
        if (m_pSelected == pTab)
        {
            return;
        }

        if (NULL != m_pSelected)
        {
            m_pSelected->UnsetFlag(TabItem::Flag_Selected);
        }

        m_pSelected = pTab;
        m_pSelected->SetFlag(TabItem::Flag_Selected);

        markDirty();

        #if DEBUG_SELECT
            DEBUG_PRINTF("%ls\n", pTab->GetString());
        #endif

        sendNotify(TCN_SELCHANGE);
    } // selectTab

    private: LRESULT sendNotify(uint nCode)
    {
        HWND hwndParent = ::GetParent(m_hwnd);
        if (NULL == hwndParent) return TRUE;

        NMHDR oNotify;
        oNotify.code     = nCode;
        oNotify.hwndFrom = m_hwnd;
        oNotify.idFrom   = ::GetDlgCtrlID(m_hwnd);

        return ::SendMessage(
            hwndParent,
            WM_NOTIFY,
            oNotify.idFrom,
            reinterpret_cast<LPARAM>(&oNotify) );
    } // sendNotify

    // [W]
    private: static LRESULT CALLBACK windowProc(
        HWND    hwnd,
        UINT    uMsg,
        WPARAM  wParam,
        LPARAM  lParam )
    {
        TabBand* pThis = reinterpret_cast<TabBand*>(
            ::GetWindowLongPtr(hwnd, GWLP_USERDATA) );

        if (NULL == pThis)
        {
            pThis = new TabBand(hwnd);

            ::SetWindowLongPtr(
                hwnd,
                GWLP_USERDATA,
                reinterpret_cast<LONG_PTR>(pThis) );
        } // if

        return pThis->onMessage(uMsg, wParam, lParam);
    } // windowProc
}; // TabBand


#if USE_GDIPLUS
void CloseButton::DrawTheme(Gdiplus::Graphics& g, Gdiplus::Font*) const
{
    g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);

    // Fill background
    if (IsHover())
    {
        Gdiplus::SolidBrush brush(Gdiplus::Color(237, 237, 239));

        RECT rc = m_rc;
        //rc.right  += 1;
        rc.bottom -= 1;

        g.FillRectangle(
            &brush,
            rc.left,
            rc.top,
            rc.right  - rc.left,
            rc.bottom - rc.top );
    } // IsHover

    // draw "X"
    {
        g.SetSmoothingMode(Gdiplus::SmoothingModeNone);

        Gdiplus::SolidBrush brush(Gdiplus::Color(150, 150, 150));
        if (IsHover())
        {
            brush.SetColor(Gdiplus::Color(184, 60, 61));
        }

        RECT rc = m_rc;
        rc.left += 4;
        rc.top  += 4;

        #define hline(x, y, cx, cy) \
            g.FillRectangle( \
                &brush, \
                m_rc.left + x, m_rc.top + y, \
                cx, cy );

        hline( 4,  4, 3, 1);
        hline(10,  4, 3, 1);

        hline( 5,  5, 3, 1);
        hline( 9,  5, 3, 1);

        hline( 6,  6, 5, 1);

        hline( 7,  7, 3, 2);  // center

        hline( 6,  9, 5, 1);

        hline( 5, 10, 3, 1);
        hline( 9, 10, 3, 1);

        hline( 4, 11, 3, 1);
        hline(10, 11, 3, 1);

        #undef hline
    } // draw "X"

    if (IsHover())
    {
        g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);

        // Draw edge
        {
            RECT rc = m_rc;
            //rc.right  += 1;
            rc.bottom -= 1;

            Gdiplus::Pen pen(Gdiplus::Color(150, 150, 150));
            Gdiplus::Point rgPoint[8];
            rgPoint[0] = Gdiplus::Point(rc.left  + 1, rc.top    + 0);
            rgPoint[1] = Gdiplus::Point(rc.right - 1, rc.top    + 0);
            rgPoint[2] = Gdiplus::Point(rc.right + 0, rc.top    + 1);
            rgPoint[3] = Gdiplus::Point(rc.right + 0, rc.bottom - 1);
            rgPoint[4] = Gdiplus::Point(rc.right - 1, rc.bottom + 0);
            rgPoint[5] = Gdiplus::Point(rc.left  + 1, rc.bottom + 0);
            rgPoint[6] = Gdiplus::Point(rc.left  + 0, rc.bottom - 1);
            rgPoint[7] = Gdiplus::Point(rc.left  + 0, rc.top    + 1);
            g.DrawLines(&pen, rgPoint, lengthof(rgPoint));
        }
    } // if hover
} // CloseButton::DrawTheme

void TabBand::DrawTheme(Gdiplus::Graphics& g, Gdiplus::Font* pFont) const
{
    // Fill background
    {
        Gdiplus::LinearGradientBrush fillBrush(
            Gdiplus::Point(0, m_rc.top + 14),
            Gdiplus::Point(0, m_rc.bottom - m_rc.top),
            Gdiplus::Color(207, 215, 235),
            Gdiplus::Color(242, 245, 250) );

        g.FillRectangle(
            &fillBrush,
            m_rc.left,
            m_rc.top,
            m_rc.right  - m_rc.left,
            m_rc.bottom - m_rc.top );
    }

    foreach (TabElements::Enum, oEnum, this)
    {
        TabElement* pElement = oEnum.Get();
        if (pElement->IsDisplay())
        {
            pElement->DrawTheme(g, pFont);
        }
    } // for each tab
} // TabBand::DrawTheme

void TabItem::DrawTheme(Gdiplus::Graphics& g, Gdiplus::Font* pFont) const
{
    RECT rc = m_rc;

    Gdiplus::GraphicsPath path;

    if (IsSelected())
    {
        rc.top += 2;
        rc.bottom -= 2;

        Gdiplus::Point rgPoint[10];
        rgPoint[0] = Gdiplus::Point(GetParent()->GetRect()->left, rc.bottom + 0);
        rgPoint[1] = Gdiplus::Point(rc.left  - 1, rc.bottom + 0);
        rgPoint[2] = Gdiplus::Point(rc.left  + 0, rc.bottom - 2);
        rgPoint[3] = Gdiplus::Point(rc.left  + 0, rc.top    + 2);
        rgPoint[4] = Gdiplus::Point(rc.left  + 2, rc.top    + 0);
        rgPoint[5] = Gdiplus::Point(rc.right - 2, rc.top    + 0);
        rgPoint[6] = Gdiplus::Point(rc.right + 0, rc.top    + 2);
        rgPoint[7] = Gdiplus::Point(rc.right + 0, rc.bottom - 2);
        rgPoint[8] = Gdiplus::Point(rc.right + 1, rc.bottom + 0);
        rgPoint[9] = Gdiplus::Point(GetParent()->GetRect()->right, rc.bottom + 0);

        path.AddLines(rgPoint, lengthof(rgPoint));
    }
    else
    {
        rc.top += 4;
        rc.bottom -= 2;
        Gdiplus::Point rgPoint[9];

        rgPoint[0] = Gdiplus::Point(rc.right, rc.bottom - 1);

        //TabElement* pNext = GetNextShow();
        TabElement* pNext = GetNext();
        if (NULL != pNext && pNext->IsSelected())
        {
            rgPoint[1] = Gdiplus::Point(rc.right - 1, rc.bottom);
        }
        else
        {
            rgPoint[1] = Gdiplus::Point(rc.right, rc.bottom);
        }

        //TabElement* pPrev = GetPrevShow();
        TabElement* pPrev = GetPrev();
        if (NULL != pPrev && pPrev->IsSelected())
        {
            rgPoint[2] = Gdiplus::Point(rc.left + 1, rc.bottom);
        }
        else
        {
            rgPoint[2] = Gdiplus::Point(rc.left, rc.bottom);
        }

        rgPoint[3] = Gdiplus::Point(rc.left,      rc.bottom - 1);
        rgPoint[4] = Gdiplus::Point(rc.left,      rc.top + 2);
        rgPoint[5] = Gdiplus::Point(rc.left  + 2, rc.top);
        rgPoint[6] = Gdiplus::Point(rc.right - 2, rc.top);
        rgPoint[7] = Gdiplus::Point(rc.right,     rc.top + 2);
        rgPoint[8] = Gdiplus::Point(rc.right,     rc.bottom - 1);

        path.AddLines(rgPoint, lengthof(rgPoint));
    } // if selected

    // Fill
    g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);

    if (IsSelected())
    {
        {
            RECT rc = m_rc;
            rc.top += 3;
            rc.bottom = rc.top + 10;
            Gdiplus::LinearGradientBrush fillBrush(
                Gdiplus::Point(0, rc.top),
                Gdiplus::Point(0, rc.bottom),
                Gdiplus::Color(252, 253, 253),
                Gdiplus::Color(231, 245, 251) );
            g.FillRectangle(
                &fillBrush,
                m_rc.left,
                rc.top,
                m_rc.right - m_rc.left,
                rc.bottom  - rc.top );
        }

        {
            RECT rc = m_rc;
            rc.top += 12;

            Gdiplus::LinearGradientBrush fillBrush(
                Gdiplus::Point(0, rc.top),
                Gdiplus::Point(0, rc.bottom),
                Gdiplus::Color(207, 231, 250),
                Gdiplus::Color(183, 200, 246) );

            g.FillRectangle(
                &fillBrush,
                m_rc.left,
                rc.top,
                m_rc.right - m_rc.left,
                rc.bottom - rc.top );
        }
    }
    else if (IsHover())
    {
        Gdiplus::LinearGradientBrush fillBrush(
            Gdiplus::Point(0, rc.top + 0),
            Gdiplus::Point(0, rc.top + 27),
            Gdiplus::Color(242, 245, 250),
            Gdiplus::Color(153, 198, 238) );

        fillBrush.SetBlendTriangularShape(0.5f, 1.0f);
        g.FillRectangle(
            &fillBrush,
            rc.left,
            rc.top,
            rc.right - rc.left,
            27 );
    }
    else
    {
        Gdiplus::LinearGradientBrush fillBrush(
            Gdiplus::Point(0, 13),
            Gdiplus::Point(0, 27 + 13 - 1),
            Gdiplus::Color(207, 215, 235),
            Gdiplus::Color(242, 245, 250) );

        g.FillRectangle(
            &fillBrush,
            rc.left,
            rc.top,
            rc.right - rc.left,
            27 );
    } // if

    // drawThemeContents
    {
        Rect rcLabel = m_rc;

        rcLabel.left += cxMarginLeft + cxEdgeLeft;

        if (m_iImage >= 0)
        {
            rcLabel.left  += cxIconMarginLeft;
            rcLabel.left  += cxIcon;
            rcLabel.left  += cxIconMarginRight;
        } // if image

        rcLabel.left += cxTextMarginLeft;

        rcLabel.right  -= cxTextMarginRight;
        rcLabel.right  -= cxEdgeRight + cxMarginRight;

        rcLabel.bottom -= cyTextMarginBottom + cyMarginBottom;

        if (IsSelected())
        {
            rcLabel.bottom -= 1;

            rcLabel.right -= cxCloseMarginLeft;
            rcLabel.right -= CloseButton::cxBox;
            rcLabel.right -= cxCloseMarginRight;
        }

        rcLabel.top += 10;

        // Draw label
        {
            g.SetSmoothingMode(Gdiplus::SmoothingModeNone);
            Gdiplus::SolidBrush brush(Gdiplus::Color(0, 0, 0));

            g.DrawString(
                m_pwsz,
                m_cwch,
                pFont,
                Gdiplus::PointF(
                    static_cast<Gdiplus::REAL>(rcLabel.left), 
                    static_cast<Gdiplus::REAL>(rcLabel.top) ),
                &brush );
        }
#if 0
        HIMAGELIST hImageList = GetImageList();
        if (NULL != hImageList && m_iImage >= 0)
        {
            #if 0
                HICON hIcon = ::ImageList_GetIcon(
                    hImageList,
                    m_iImage,
                    ILD_TRANSPARENT );

                Gdiplus::Bitmap icon(hIcon);

                g.DrawImage(&icon, rcLabel.left, rcLabel.top);
            #else
                HDC hdc = g.GetHDC();
                ::ImageList_Draw(
                    hImageList,
                    m_iImage,
                    hdc,
                    rcLabel.left - 20,
                    rcLabel.top,
                    ILD_TRANSPARENT );
                g.ReleaseHDC(hdc);
            #endif
        }
#endif
    } // drawThemeContents

    if (IsSelected())
    {
        m_oCloseButton.DrawTheme(g, pFont);
    } // if selected

    // Draw edge
    {
        Gdiplus::Pen linePen(Gdiplus::Color(143, 149, 161));
        g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
        g.DrawPath(&linePen, &path);
    }
} // TabItem::DrawTheme

//
//      [4]      [5]
//     --oooooooooo--
//     -o----------o-
// [3] o------------o [6]
//     o------------o
//     o------------o
//     o------------o
//     o------------o
//     o-----------o- [0]
//     oooooooooooo--
//    [2]        [1]
//
void TabListButton::DrawTheme(Gdiplus::Graphics& g, Gdiplus::Font*) const
{
    COLORREF cr3dFace = ::GetSysColor(COLOR_3DFACE);

    Gdiplus::Color color3dFace(
        GetRValue(cr3dFace),
        GetGValue(cr3dFace),
        GetBValue(cr3dFace) );

    // Fill background
    {
        Gdiplus::SolidBrush brush(color3dFace);

        g.FillRectangle(
            &brush,
            m_rc.left,
            m_rc.top,
            m_rc.right - m_rc.left,
            m_rc.bottom - m_rc.top );
    }


    RECT rc = m_rc;
    rc.top += 4;
    rc.bottom -= 2;

    Gdiplus::GraphicsPath path;
    {
        Gdiplus::Point rgPoint[8];

        rgPoint[0] = Gdiplus::Point(rc.right, rc.bottom - 1);

        //TabElement* pNext = GetNextShow();
        TabElement* pNext = GetNext();
        if (NULL != pNext && pNext->IsSelected())
        {
            rgPoint[1] = Gdiplus::Point(rc.right - 1, rc.bottom);
        }
        else
        {
            rgPoint[1] = Gdiplus::Point(rc.right, rc.bottom);
        }

        rgPoint[2] = Gdiplus::Point(rc.left,      rc.bottom);
        rgPoint[3] = Gdiplus::Point(rc.left,      rc.top + 2);
        rgPoint[4] = Gdiplus::Point(rc.left  + 2, rc.top);
        rgPoint[5] = Gdiplus::Point(rc.right - 2, rc.top);
        rgPoint[6] = Gdiplus::Point(rc.right,     rc.top + 2);
        rgPoint[7] = Gdiplus::Point(rc.right,     rc.bottom - 1);

        path.AddLines(rgPoint, lengthof(rgPoint));
        path.CloseAllFigures();
    }

    // Fill background
    {
        Gdiplus::LinearGradientBrush fillBrush(
            Gdiplus::Point(0, rc.top),
            Gdiplus::Point(0, m_rc.bottom),
            Gdiplus::Color(253, 253, 253),
            Gdiplus::Color(207, 231, 250) );
        g.FillPath(&fillBrush, &path);
    }

    // Fill highlight
    {
        Gdiplus::LinearGradientBrush fillBrush(
            Gdiplus::Point(0, 13),
            Gdiplus::Point(0, 27 + 13 - 1),
            IsHover() ?
                Gdiplus::Color(153, 198, 238) :
                Gdiplus::Color(207, 215, 235),
            IsHover() ?
                Gdiplus::Color(242, 245, 250) : 
                Gdiplus::Color(242, 245, 250) );

        g.FillRectangle(
            &fillBrush,
            rc.left,
            rc.top,
            rc.right - rc.left,
            27 );
    }

    // Draw down arrow
    {
        RECT rc = m_rc;

        // Draw down arrow
        g.SetSmoothingMode(Gdiplus::SmoothingModeNone);
        {
            int x = (rc.right - rc.left - 4) / 2 + rc.left;
            int y = (rc.bottom - rc.top) / 2 + rc.top;
            Gdiplus::SolidBrush arrowBrush(Gdiplus::Color(0, 0, 0));
            g.FillRectangle(&arrowBrush, x + 0, y + 0, 5, 1);
            g.FillRectangle(&arrowBrush, x + 1, y + 1, 3, 1);
            g.FillRectangle(&arrowBrush, x + 2, y + 2, 1, 1);
        }
    } // drawArrow

    // Draw edge
    {
        g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
        Gdiplus::Pen linePen(Gdiplus::Color(160, 160, 160));
        g.DrawPath(&linePen, &path);
    } // draw edge
} // TabListButton::DrawTheme

#endif // USE_GDIPLUS

} // TabBandPrivate

void TabBand__Init(HINSTANCE hInstance)
{
    TabBandPrivate::TabBand::Init(hInstance);
} // TabBandInit
