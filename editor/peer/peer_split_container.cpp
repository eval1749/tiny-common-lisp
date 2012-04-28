#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - Split Container
// editor/peer/peer_split_container.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_split_container.cpp#7 $
//
#define DEBUG_CURSOR    0
#define DEBUG_DRAW      0
#define DEBUG_DESTROY   0
#define DEBUG_FOCUS     0
#define DEBUG_GRIP      0
#define DEBUG_RESIZE    0
#define DEBUG_SCROLLBAR 0
#define DEBUG_SPLIT     0
#include "./peer_split_container.h"

#include "../resource.h"
#include "./peer_defs.h"

namespace Editor
{

namespace Peer
{

namespace SplitContainerPrivate
{

enum Constants
{
    k_cxSplitter    = 8,
    k_cySplitter    = 8,
    k_cyGrip        = 11,

    k_cxMinBox      = k_cxSplitter,
    k_cyMinBox      = k_cySplitter,
}; // Constants

typedef SplitContainer::Box Box;
typedef SplitContainer::Boxes Boxes;

static void drawSplitter(HDC hdc, RECT* prc, uint rgfEdge)
{
    ::FillRect(hdc, prc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));
    ::DrawEdge(hdc, prc, EDGE_RAISED, rgfEdge);
} // drawSplitter

class CompositeBox :
    public SplitContainer::ParentBox
{
    private: bool m_fBeingDestroyed;

    protected: CompositeBox() :
        m_fBeingDestroyed(false) {}

    // dtor
    public: ~CompositeBox()
    {
        if (NULL == GetParent())
        {
            ASSERT(NULL == GetFirst());
        }

        Boxes::Enum oEnum(this);
        while (! oEnum.AtEnd())
        {
            Box* pBox = oEnum.Get();
            oEnum.Next();
            delete pBox;
        } // for each box
    } // ~CompositeBox

    // [C]
    /// <summary>
    ///   Returns most recently activated window in child boxes.
    /// </summary>
    private: virtual Window* ComputeActiveWindow() const override
    {
        Window* pActive = NULL;
        foreach (Boxes::Enum, oEnum, this)
        {
            Window* const pWindow = oEnum.Get()->ComputeActiveWindow();
            if (NULL == pActive)
            {
                pActive = pWindow;
            }
            else if (NULL != pWindow)
            {
                if (pActive->GetActiveTick() < pWindow->GetActiveTick())
                {
                    pActive = pWindow;
                }
            }
        } // for each child
        return pActive;
    } // ComputeActiveWindow

    protected: int countBoxes() const
    {
        int cBoxes = 0;
        foreach (Boxes::Enum, oEnum, this)
        {
            cBoxes += 1;
        } // for each child
        return cBoxes;
    } // countBoxes

    // [D]
    private: virtual void Destroy() override
    {
        #if DEBUG_RESIZE || DEBUG_SPLIT
            DEBUG_PRINTF("%p beingDestroyed=%d\n",
                this, m_fBeingDestroyed );
        #endif

        m_fBeingDestroyed = true;

        while (Box* pBox = GetFirst())
        {
            pBox->Destroy();
        } // for each child
    } // Detroy

    // [F]
    private: virtual Box* FindBox(Window* const pWindow) const override
    {
        foreach (Boxes::Enum, oEnum, this)
        {
            if (Box* const pBox = oEnum.Get()->FindBox(pWindow))
            {
                return pBox;
            }
        } // for each child
        return NULL;
    } // FindBox

    // [O]
    private: virtual bool OnIdle(uint const nCount) override
    {
        bool fMore = false;
        foreach (Boxes::Enum, oEnum, this)
        {
            if (oEnum.Get()->OnIdle(nCount)) fMore = true;
        } // for each child
        return fMore;
    } // OnIdle

    // [R]
    protected: virtual void Remove(Box* const pBox) override
    {
        #if DEBUG_RESIZE || DEBUG_SPLIT
            DEBUG_PRINTF("%p pBox=%p\n", this, pBox);
        #endif

        Delete(pBox);

        if (! m_fBeingDestroyed)
        {
            Box* const pFirstBox = GetFirst();
            if (GetLast() == pFirstBox)
            {
                Delete(pFirstBox);
                GetParent()->Replace(pFirstBox, this);
                delete this;
            } // if
        } // if
    } // Remove

    private: virtual void Realize(Container* const pParent) override
    {
        foreach (Boxes::Enum, oEnum, this)
        {
            oEnum.Get()->Realize(pParent);
        } // for each child
    } // Realize

    private: virtual void Replace(
        Box* const pNew, 
        Box* const pOld) override
    {
        foreach (Boxes::Enum, oEnum, this)
        {
            if (oEnum.Get() == pOld)
            {
                InsertBefore(pNew, pOld);
                Delete(pOld);
                return;
            }
        } // for each child
        // pOld must be in child list.
        CAN_NOT_HAPPEN();
    } // Replace
}; // CompositeBox

class VSplitBox;

/// <summary>
///   Represents a composit box contains boxes layouting horizontally.
/// </summary>
class HSplitBox :
    public SplitContainer::Box_<HSplitBox, CompositeBox>
{
    public: static const char* Kind_() { return "HSplitBox"; }

    public: ~HSplitBox()
    {
        #if DEBUG_DESTROY
            DEBUG_PRINTF("%p\n", this);
        #endif
    } // ~HSplitBox

    // [D]
    private: void Draw(HDC const hdc) const
    {
        int const cyOffset = GetParent()->Is<VSplitBox>() ? 2 : 0;
        foreach (Boxes::Enum, oEnum, this)
        {
            Box* pBox = oEnum.Get();
            pBox->Draw(hdc);
            if (Box* pRight = pBox->GetNext())
            {
                Rect rc(
                    pBox->m_rc.right,
                    pBox->m_rc.top - cyOffset,
                    pRight->m_rc.left,
                    pRight->m_rc.bottom - cyOffset );

                drawSplitter(hdc, &rc, BF_LEFT | BF_RIGHT);
            } // if
        } // for each box
    } /// Draw

    // [H]
    private: virtual Hit HitTest(
        POINT const pt, 
        Box** const out_pBox ) const override
    {
        *out_pBox = NULL;

        if (! ::PtInRect(&m_rc, pt))
        {
            return SplitContainer::Hit_None;
        }

        foreach (Boxes::Enum, oEnum, this)
        {
            Box* const pBox = oEnum.Get();

            if (Hit const eHit = pBox->HitTest(pt, out_pBox))
            {
                return eHit;
            }

            if (Box* const pRight = pBox->GetNext())
            {
                Rect rc(
                    pBox->m_rc.right,
                    pBox->m_rc.top,
                    pRight->m_rc.left,
                    pRight->m_rc.bottom );
                if (::PtInRect(&rc, pt))
                {
                    *out_pBox = pRight;
                    return SplitContainer::Hit_HSplitter;
                }
            } // if
        } // for each box
        return SplitContainer::Hit_None;
    } // HitTest

    // [R]
    /// <summary>
    ///  Removes child box and enlarge left/right side box.
    /// </summary>
    private: void Remove(Box* const pBox)
    {
        ASSERT(pBox->GetParent() == this);

        Box* const pLeft = pBox->GetPrev();
        Box* const pRight = pBox->GetNext();
        Super::Remove(pBox);

        if (pLeft)
        {
            Rect rc(pLeft->m_rc);
            rc.right = pBox->m_rc.right;
            pLeft->Resize(&rc);
        }
        else if (pRight)
        {
            Rect rc(pRight->m_rc);
            rc.left = pBox->m_rc.left;
            pRight->Resize(&rc);
        } // if
    } // Remove

    private: void Resize(const Rect* const prc)
    {
        int cxParent = prc->GetWidth()  - m_rc.GetWidth();

        Rect rc(m_rc);
        m_rc = *prc;
        for (;;)
        {
            int cBoxes = countBoxes();
            if (0 == cBoxes)
            {
                break;
            }

            int const cxDelta     = cxParent / cBoxes;
            int cxRemainder = cxParent % cBoxes;

            if (cxParent < 0)
            {
                Box* pSmallest = NULL;
                foreach (Boxes::Enum, oEnum, this)
                {
                    Box* pBox = oEnum.Get();
                    if (pBox->GetWidth() + cxDelta < k_cxMinBox)
                    {
                        if (NULL == pSmallest)
                        {
                            pSmallest = pBox;
                        }
                        else if (pSmallest->GetWidth() > pBox->GetWidth())
                        {
                            pSmallest = pBox;
                        }
                    } // if
                } // for each box

                if (NULL != pSmallest)
                {
                    Delete(pSmallest);
                    pSmallest->Destroy();
                    continue;
                } // if
            } // if

            int x = m_rc.left;
            foreach (Boxes::Enum, oEnum, this)
            {
                Box* pBox = oEnum.Get();
                Rect rc(m_rc);
                rc.left  = x;
                rc.right = x + pBox->GetWidth() + cxDelta;
                if (cxRemainder > 0)
                {
                    rc.right += 1;
                    cxRemainder -= 1;
                }
                else if (cxRemainder < 0)
                {
                    rc.right -= 1;
                    cxRemainder += 1;
                }

                pBox->Resize(&rc);

                x = rc.right + k_cxSplitter;
            } // for each box
            break;
        } // for
    } // Resize
}; // HSplitBox

/// <summary>
///   Represents a leaf box.
/// </summary>
class LeafBox : public SplitContainer::Box_<LeafBox>
{
    public: static const char* Kind_() { return "LeafBox"; }

    private: HWND    m_hwndHScroll;
    private: HWND    m_hwndVScroll;
    private: Window* m_pWindow;

    // ctor
    public: LeafBox(Window* const p) :
        m_hwndHScroll(NULL),
        m_hwndVScroll(NULL),
        m_pWindow(p) {}

    public: ~LeafBox()
    {
        #if DEBUG_DESTROY || DEBUG_SPLIT
            DEBUG_PRINTF("%p\n", this);
        #endif

        if (NULL != m_hwndHScroll)
        {
            ::DestroyWindow(m_hwndHScroll);
            m_hwndHScroll = NULL;
        }

        if (NULL != m_hwndVScroll)
        {
            ::DestroyWindow(m_hwndVScroll);
            m_hwndVScroll = NULL;
        }
    } // ~LeafBox

    // [C]
    private: virtual Window* ComputeActiveWindow() const override
        { return m_pWindow; }

    // [D]
    private: virtual void Destroy() override
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("%p\n", this);
        #endif

        ::DestroyWindow(*m_pWindow);
        // At here, this object has been deleted.
    } // Destroy

    private: virtual void Draw(HDC const hdc) const override
    {
        if (hasVGrip())
        {
            Rect rc;
            getVGripRect(&rc);
            drawSplitter(hdc, &rc, BF_RECT);
        } // if
    } // Draw

    // [F]
    private: virtual Box* FindBox(Window* const pWindow) const override
    {
        if (m_pWindow != pWindow)
        {
            return NULL;
        }
        return const_cast<LeafBox*>(this);
    } // FindBox

    // [G]
    private: void getVGripRect(RECT* const out_rc) const
    {
        ASSERT(NULL != out_rc);
        *out_rc = m_rc;
        out_rc->left   = out_rc->right - ::GetSystemMetrics(SM_CXVSCROLL);
        out_rc->bottom = out_rc->top   + k_cyGrip;
    } // getVGripRect

    public: Window* GetWindow() const
        { return m_pWindow; }

    // [H]
    private: bool hasVGrip() const
    {
        if (NULL == m_hwndVScroll)
        {
            return false;
        }

        if (GetParent()->Is<VSplitBox>())
        {
            return NULL == GetPrev();
        }

        return true;
    } // hasVGrip

    private: virtual Hit HitTest(
        POINT const pt, 
        Box** const out_pBox) const override
    {
        if (hasVGrip())
        {
            Rect rc;
            getVGripRect(&rc);
            if (::PtInRect(&rc, pt))
            {
                *out_pBox = const_cast<LeafBox*>(this);
                return SplitContainer::Hit_VGrip;
            }
        } // if

        *out_pBox = NULL;
        return SplitContainer::Hit_None;
    } // HitTest

    // [O]
    private: virtual bool OnIdle(uint const nCount) override
        { return m_pWindow->OnIdle(nCount); }

    // [R]
    public: virtual void Realize(Container* const pParent) override
    {
        m_pWindow->Realize(pParent);

        uint dwStyle = ::GetWindowLong(*m_pWindow, GWL_STYLE);
        uint dwNewStyle = dwStyle;
        if (dwStyle & WS_VSCROLL)
        {
            dwNewStyle &= ~WS_VSCROLL;
            m_hwndVScroll = ::CreateWindowEx(
                0,
                L"SCROLLBAR",
                NULL,   // title
                WS_CHILD | WS_VISIBLE | SBS_VERT,
                0,  // x
                0,  // y
                0,  // width
                0,  // height
                ::GetParent(*m_pWindow), // parent
                NULL, // menu
                g_hInstance,
                NULL );

            ::SetWindowLongPtr(
                m_hwndVScroll, 
                GWLP_USERDATA,
                reinterpret_cast<LONG_PTR>(m_pWindow->GetHwnd()) );

            m_pWindow->SetScrollBar(m_hwndVScroll, SB_VERT);
        } // if

        if (dwStyle != dwNewStyle)
        {
            ::SetWindowLong(*m_pWindow, GWL_STYLE, dwNewStyle);
            Resize(&m_rc);
        }
    } // Realize

    public: virtual void Resize(const Rect* const prc) override
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("%p\n", this);
        #endif

        m_rc = *prc;

        int cxVScroll = 0;
        if (NULL != m_hwndVScroll)
        {
            cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);

            Rect rcGrip;
            if (hasVGrip())
            {
                getVGripRect(&rcGrip);
            }

            ::SetWindowPos(
                m_hwndVScroll,
                NULL,
                m_rc.right - cxVScroll,
                m_rc.top   + rcGrip.GetHeight(),
                cxVScroll,
                m_rc.GetHeight() - rcGrip.GetHeight(),
                SWP_NOZORDER );

            #if DEBUG_SCROLLBAR
                DEBUG_PRINTF("Resize scrollbar for %p\n", m_pWindow);
            #endif
        } // if

        ::SetWindowPos(
            m_pWindow->GetHwnd(),
            NULL,
            m_rc.left,
            m_rc.top,
            m_rc.GetWidth() - cxVScroll,
            m_rc.GetHeight(),
            SWP_NOZORDER );
    } // SetSize
}; // LeafBox

/// <summary>
///   Represents a composit box contains boxes layouting verticall.
/// </summary>
class VSplitBox :
    public SplitContainer::Box_<VSplitBox, CompositeBox>
{
    public: static const char* Kind_() { return "VSplitBox"; }

    public: ~VSplitBox()
    {
        #if DEBUG_DESTROY
            DEBUG_PRINTF("%p\n", this);
        #endif
    } // ~VSplitBox

    // [D]
    private: void Draw(HDC const hdc) const
    {
        int const cxOffset = GetParent()->Is<HSplitBox>() ? 2 : 0;

        foreach (Boxes::Enum, oEnum, this)
        {
            Box* const pBox = oEnum.Get();
            pBox->Draw(hdc);
            if (Box* pBelow = pBox->GetNext())
            {
                Rect rc(
                    pBox->m_rc.left - cxOffset,
                    pBox->m_rc.bottom,
                    pBox->m_rc.right + cxOffset,
                    pBelow->m_rc.top );
                drawSplitter(hdc, &rc, BF_TOP | BF_BOTTOM);
            } // if
        } // for each box
    } /// Draw

    // [H]
    private: virtual Hit HitTest(
        POINT const pt, 
        Box** const out_pBox ) const override
    {
        #if DEBUG_CURSOR
            DEBUG_PRINTF("%d@%d (%d,%d)-(%d,%d)\n",
                pt.x, pt.y,
                m_rc.left, m_rc.top, m_rc.right, m_rc.bottom );
        #endif

        *out_pBox = NULL;
        if (! ::PtInRect(&m_rc, pt))
        {
            return SplitContainer::Hit_None;
        }

        foreach (Boxes::Enum, oEnum, this)
        {
            Box* const pBox = oEnum.Get();

            if (Hit const eHit = pBox->HitTest(pt, out_pBox))
            {
                return eHit;
            }

            if (Box* const pBelow = pBox->GetNext())
            {
                Rect rc(
                    pBox->m_rc.left,
                    pBox->m_rc.bottom,
                    pBelow->m_rc.right,
                    pBelow->m_rc.top );
                if (::PtInRect(&rc, pt))
                {
                    *out_pBox = pBelow;
                    return SplitContainer::Hit_VSplitter;
                } // if
            } // if
        } // for each box
        return SplitContainer::Hit_None;
    } // HitTest

    // [R]
    private: virtual void Remove(Box* const pBox) override
    {
        ASSERT(pBox->GetParent() == this);

        Box* const pAbove = pBox->GetPrev();
        Box* const pBelow = pBox->GetNext();
        Super::Remove(pBox);

        if (pAbove)
        {
            Rect rc(pAbove->m_rc);
            rc.bottom = pBox->m_rc.bottom;
            pAbove->Resize(&rc);
        }
        else if (pBelow)
        {
            Rect rc(pBelow->m_rc);
            rc.top = pBox->m_rc.top;
            pBelow->Resize(&rc);
        }
    } // Remove

    private: virtual void Resize(const Rect* const prc) override
    {
        int const cyParent = prc->GetHeight()  - m_rc.GetHeight();

        Rect rc(m_rc);
        m_rc = *prc;
        for (;;)
        {
            int const cBoxes = countBoxes();
            if (0 == cBoxes)
            {
                break;
            }

            int const cyDelta     = cyParent / cBoxes;
            int cyRemainder = cyParent % cBoxes;

            if (cyParent < 0)
            {
                Box* pSmallest = NULL;
                foreach (Boxes::Enum, oEnum, this)
                {
                    Box* const pBox = oEnum.Get();
                    if (pBox->GetHeight() + cyDelta < k_cyMinBox)
                    {
                        if (NULL == pSmallest)
                        {
                            pSmallest = pBox;
                        }
                        else if (pSmallest->GetHeight() > pBox->GetHeight())
                        {
                            pSmallest = pBox;
                        }
                    } // if
                } // for each box

                if (NULL != pSmallest)
                {
                    Delete(pSmallest);
                    pSmallest->Destroy();
                    continue;
                } // if
            } // if

            int y = m_rc.top;
            foreach (Boxes::Enum, oEnum, this)
            {
                Box* const pBox = oEnum.Get();
                Rect rc(m_rc);
                rc.top    = y;
                rc.bottom = y + pBox->GetHeight() + cyDelta;
                if (cyRemainder > 0)
                {
                    rc.bottom += 1;
                    cyRemainder -= 1;
                }
                else if (cyRemainder < 0)
                {
                    rc.bottom -= 1;
                    cyRemainder += 1;
                }

                pBox->Resize(&rc);

                y = rc.bottom + k_cySplitter;
            } // for each box
            break;
        } // for
    } // Resize
}; // VSplitBox

} // namespace

using namespace SplitContainerPrivate;

// warning C4355: 'this' : used in base member initializer list
#pragma warning(disable : 4355)

// ctor
SplitContainer::SplitContainer() :
    m_oRootBox(this) {}

// dtor
SplitContainer::~SplitContainer()
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif
} // SplitContainer::~SplitContainer

// [A]
Window* SplitContainer::AppendChild(Window* pWindow)
{
    ASSERT(NULL == m_oRootBox.GetBox());
    Super::AppendChild(pWindow);
    m_oRootBox.SetBox(new LeafBox(pWindow));
    return pWindow;
} // SplitContainer::SetRootWindow

// [O]
int SplitContainer::onCreate(CREATESTRUCT* pCreate)
{
    m_rc.left = 0;
    m_rc.top  = 0;
    m_rc.right = pCreate->cx;
    m_rc.bottom = pCreate->cy;

    m_oRootBox.Realize(this);

    m_pActive = m_oRootBox.ComputeActiveWindow();
    return 0;
} // SplitContainer::onCreate

/// <summary>
///   Handles message from Windows.
/// </summary>
LRESULT SplitContainer::onMessage(
    uint    const uMsg,
    WPARAM  const wParam,
    LPARAM  const lParam )
{
    switch (uMsg)
    {
    case WM_LBUTTONDOWN:
    {
        Point pt(MAKEPOINTS(lParam));
        Box* pBox;
        Hit const eHit = m_oRootBox.HitTest(pt, &pBox);
        switch (eHit)
        {
        case Hit_HSplitter:
        case Hit_VGrip:
        case Hit_VSplitter:
            m_oDrag.Start(m_hwnd, eHit, pBox);
            break;
        } // switch hit
        return 0;
    } // WM_LBUTTONDOWN

    case WM_LBUTTONUP:
        if (Box* const pBox = m_oDrag.Stop(m_hwnd))
        {
            Point pt(MAKEPOINTS(lParam));
            m_oDrag.MoveSplitter(this, pt, pBox, false);
        } // if
        return 0;

    case WM_MOUSEMOVE:
        if (Box* const pBox = m_oDrag.IsActive(m_hwnd))
        {
            Point pt(MAKEPOINTS(lParam));
            m_oDrag.MoveSplitter(this, pt, pBox, true);
        } // if
        return 0;

    case WM_PAINT:
    {
        // For RedrawWindow with the RDW_INTERNALPAINT
        if (::GetUpdateRect(m_hwnd, NULL, false))
        {
            PAINTSTRUCT ps;
            HDC hdc = ::BeginPaint(m_hwnd, &ps);
            #if DEBUG_DRAW
                DEBUG_PRINTF("WM_PAINT (%d,%d)-(%d,%d)\n",
                    ps.rcPaint.left, ps.rcPaint.top,
                    ps.rcPaint.right, ps.rcPaint.bottom );
            #endif
            m_oRootBox.Draw(hdc);
            ::EndPaint(m_hwnd, &ps);
        } // if
        return 0;
    } // WM_PAINT

    case WM_PARENTNOTIFY:
        // Note: We don't receive WM_DESTROY for child window if split
        // container is being destroyed.
        switch (wParam)
        {
        case WM_DESTROY:
        {
            if (Window* const pWindow = Window::FromHwnd(lParam))
            {
                RemoveChild(pWindow);
            }
            break;
        } // WM_DESTORY
        } // switch wParam
        return 0;

    case WM_SETCURSOR:
    {
        POINT pt;
        if (! ::GetCursorPos(&pt))
        {
            return false;
        }

        if (! ::ScreenToClient(m_hwnd, &pt))
        {
            return false;
        }

        HCURSOR hCursor = NULL;
        switch (m_oRootBox.HitTest(pt))
        {
        case Hit_Box:
            return false;

        case Hit_HSplitter:
            static HCURSOR s_hHSplit;
            if (NULL == s_hHSplit)
            {
                s_hHSplit = ::LoadCursor(
                    g_hResource,
                    MAKEINTRESOURCE(IDC_HSPLIT) );
            }
            hCursor = s_hHSplit;
            break;

        case Hit_VGrip:
        case Hit_VSplitter:
            static HCURSOR s_hVSplit;
            if (NULL == s_hVSplit)
            {
                s_hVSplit = ::LoadCursor(
                    g_hResource,
                    MAKEINTRESOURCE(IDC_VSPLIT) );
            }
            hCursor = s_hVSplit;
            break;
        } // switch Hit

        if (NULL != hCursor)
        {
            ::SetCursor(hCursor);
            return true;
        }

        break;
    } // WM_SETCURSOR

    case WM_SETFOCUS:
        m_pActive = m_oRootBox.ComputeActiveWindow();
        if (NULL != m_pActive)
        {
            ::SetFocus(*m_pActive);
        }
        return 0;

    case WM_SIZE:
        goto resize;

    case WM_VSCROLL:
    {
        HWND const hwnd = reinterpret_cast<HWND>(
            ::GetWindowLongPtr(
                reinterpret_cast<HWND>(lParam),
                GWLP_USERDATA ) );

        if (NULL != hwnd)
        {
            ::SendMessage(hwnd, WM_VSCROLL, wParam, lParam);
        }

        return 0;
    } // WM_VSCROLL

    case WM_WINDOWPOSCHANGED:
    {
        const WINDOWPOS* const wp = reinterpret_cast<WINDOWPOS*>(lParam);

        #if DEBUG_REDRAW || DEBUG_RESIZE
            DEBUG_PRINTF("WM_WINDOWPOSCHANGED %p"
                " 0x%X %dx%d+%d+%d\n",
                this,
                wp->flags,
                wp->cx, wp->cy, wp->x, wp->y );
        #endif

        if (wp->flags & SWP_NOSIZE)
        {
            return 0;
        }

        goto resize;
    } // WM_WINDOWPOSCHANGED

    resize:
        ::GetClientRect(m_hwnd, &m_rc);

        m_oRootBox.Resize(&m_rc);

        // Draw splitters
        {
            Dc hdc(m_hwnd);
            m_oRootBox.Draw(hdc);
        }
        return 0;
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // SplitContainer::onMessage

// [R]
Window* SplitContainer::RemoveChild(Window* pWindow)
{
    Super::RemoveChild(pWindow);

    Box* pBox = m_oRootBox.FindBox(pWindow);
    if (NULL == pBox)
    {
        return pWindow;
    }

    ParentBox* pParent = pBox->GetParent();
    pParent->Remove(pBox);
    if (pParent == &m_oRootBox)
    {
        ::DestroyWindow(m_hwnd);
    }
    else
    {
        // Redraw splitters
        drawSplitters();
    }

    delete pBox;

    return pWindow;
} // SplitContainer::RemoveChild

// [S]
/// <summary>
///   Splits a window horizontally and inserts a new window at bottom.
/// </summary>
bool SplitContainer::SplitHorizontally(
    Window* const pNewWin,
    Window* const pRefWin,
    int     const iPercent,
    int     const cxRefWinIn )
{
    Box* const pRefBox = m_oRootBox.FindBox(pRefWin);
    if (NULL == pRefBox)
    {
        return false;
    }

    if (pRefBox->m_rc.GetWidth() < k_cxMinBox * 2 + k_cxSplitter)
    {
        // pRefBox is too small for splitting horizontally
        return false;
    }

    int cxRefWin = cxRefWinIn;
    if (0 == cxRefWin)
    {
        cxRefWin = pRefBox->m_rc.GetWidth() * iPercent / 100;
    }

    if (cxRefWin < k_cxMinBox)
    {
        return false;
    }

    ChildWindows::InsertAfter(pNewWin, pRefWin);

    ParentBox* pParent = pRefBox->GetParent();
    HSplitBox* pHBox = pParent->DynamicCast<HSplitBox>();
    if (NULL == pHBox)
    {
        pHBox = new HSplitBox;
        pHBox->m_rc = pRefBox->m_rc;
        pParent->Replace(pHBox, pRefBox);
        pHBox->Append(pRefBox);
    } // if

    Box* const pNewBox = new LeafBox(pNewWin);

    Rect rcNew(pRefBox->m_rc);
    Rect rcRef(pRefBox->m_rc);

    rcRef.right = rcNew.left + cxRefWin;
    rcNew.left  = rcRef.right + k_cxSplitter;

    pHBox->InsertAfter(pNewBox, pRefBox);

    pRefBox->Resize(&rcRef);

    pNewBox->Realize(this);
    pNewBox->Resize(&rcNew);

    drawSplitters();

    return true;
} // SplitContainer::SplitHorizontally

/// <summary>
///   Splits a window vertically and inserts a new window after reference
///   window.
/// </summary>
bool SplitContainer::SplitVertically(
    Window* const pNewWin,
    Window* const pRefWin,
    int     const iPercent,
    int     const cyRefWinIn )
{
    Box* const pRefBox = m_oRootBox.FindBox(pRefWin);
    if (NULL == pRefBox)
    {
        return false;
    }

    if (pRefBox->m_rc.GetHeight() < k_cyMinBox * 2 + k_cySplitter)
    {
        // pRefBox is too small for splitting vertically
        return false;
    }

    int cyRefWin = cyRefWinIn;
    if (0 == cyRefWin)
    {
        cyRefWin = pRefBox->m_rc.GetHeight() * iPercent / 100;
    }

    if (cyRefWin < k_cyMinBox)
    {
        return false;
    }

    ChildWindows::InsertAfter(pNewWin, pRefWin);

    ParentBox* const pParent = pRefBox->GetParent();
    VSplitBox* pVBox = pParent->DynamicCast<VSplitBox>();
    if (NULL == pVBox)
    {
        pVBox = new VSplitBox;
        pVBox->m_rc = pRefBox->m_rc;
        pParent->Replace(pVBox, pRefBox);
        pVBox->Append(pRefBox);
    } // if

    Box* const pNewBox = new LeafBox(pNewWin);

    Rect rcRef(pRefBox->m_rc);
    Rect rcNew(pRefBox->m_rc);

    rcRef.bottom = rcRef.top    + cyRefWin;
    rcNew.top    = rcRef.bottom + k_cySplitter;

    pVBox->InsertAfter(pNewBox, pRefBox);

    pRefBox->Resize(&rcRef);

    pNewBox->Realize(this);
    pNewBox->Resize(&rcNew);

    drawSplitters();

    return true;
} // SplitContainer::SplitVertically

void SplitContainer::Drag::moveHSplitter(
    SplitContainer* const pContainer,
    POINT           const pt,
    Box*            const pRight,
    bool            const fMoving )
{
    ASSERT(NULL != pContainer);
    ASSERT(NULL != pRight);

    Box* const pLeft  = pRight->GetPrev();
    if (NULL == pLeft)
    {
        return;
    }

    if (pt.x - pLeft->m_rc.left < k_cxMinBox)
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("Left box is too small\n");
        #endif

        if (fMoving)
        {
            return;
        }

        // Destroy left box
        Rect rc(pRight->m_rc);
        rc.left = pLeft->m_rc.left;
        pRight->Resize(&rc);

        pLeft->Destroy();
        pContainer->drawSplitters();
        return;
    } // if

    if (pRight->m_rc.right - pt.x - k_cxSplitter < k_cxMinBox)
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("Right box is too small\n");
        #endif

        if (fMoving)
        {
            return;
        }

        // Destroy right box
        Rect rc(pLeft->m_rc);
        rc.right = pRight->m_rc.right;
        pLeft->Resize(&rc);

        pRight->Destroy();
        pContainer->drawSplitters();
        return;
    } // if

    {
        Rect rc(pLeft->m_rc);
        rc.right = pt.x;
        pLeft->Resize(&rc);
    }

    {
        Rect rc(pRight->m_rc);
        rc.left = pt.x + k_cxSplitter;
        pRight->Resize(&rc);
    }

    pContainer->drawSplitters();
} // MoveSpillter

void SplitContainer::Drag::MoveSplitter(
    SplitContainer* const pContainer,
    POINT           const pt,
    Box*            const pBox,
    bool            const fMoving )
{
    ASSERT(NULL != pContainer);

    switch (m_eHit)
    {
    case Hit_HSplitter:
        moveHSplitter(pContainer, pt, pBox, fMoving);
        break;

    case Hit_VGrip:
        moveVGrip(pContainer, pt, pBox, fMoving);
        break;

    case Hit_VSplitter:
        moveVSplitter(pContainer, pt, pBox, fMoving);
        break;
    } // switch hit
} // SplitContainer::Drag::MoveSplitter

void SplitContainer::Drag::moveVGrip(
    SplitContainer* const pContainer,
    POINT           const pt,
    Box*            const pRefBox,
    bool            const fMoving )
{
    ASSERT(NULL != pContainer);
    ASSERT(NULL != pRefBox);

    ParentBox* const pParent = pRefBox->GetParent();
    VSplitBox* pVBox   = pParent->DynamicCast<VSplitBox>();
    if (NULL != pVBox && NULL != pRefBox->GetPrev())
    {
        moveVSplitter(pContainer, pt, pRefBox, fMoving);
        return;
    }

    int cyNewWin = pt.y - pParent->GetTop();
    if (cyNewWin < k_cyMinBox)
    {
        return;
    }

    int cyRefWin = pParent->GetBottom() - pt.y - k_cySplitter;
    if (cyRefWin < k_cyMinBox)
    {
        return;
    }

    Window* const pRefWin = pRefBox->ComputeActiveWindow();
    Window* const pNewWin = pRefWin->Clone();

    pContainer->InsertBefore(pNewWin, pRefWin);

    if (NULL == pVBox)
    {
        pVBox = new VSplitBox;
        pVBox->m_rc = pRefBox->m_rc;
        pParent->Replace(pVBox, pRefBox);
        pVBox->Append(pRefBox);
    }

    Box* pNewBox = new LeafBox(pNewWin);

    Rect rcRef(pRefBox->m_rc);
    Rect rcNew(pRefBox->m_rc);

    rcNew.bottom = rcNew.top    + cyNewWin;
    rcRef.top    = rcNew.bottom + k_cySplitter;

    pVBox->InsertBefore(pNewBox, pRefBox);

    pRefBox->Resize(&rcRef);

    pNewBox->Realize(pContainer);
    pNewBox->Resize(&rcNew);

    pContainer->drawSplitters();
} // SplitContainer::Drag::MoveVGrip

// [M]
/// <returns>Returns true if we need to redraw splitter.</returns>
void SplitContainer::Drag::moveVSplitter(
    SplitContainer* const pContainer,
    POINT           const pt,
    Box*            const pBelow,
    bool            const fMoving )
{
    ASSERT(NULL != pContainer);
    ASSERT(NULL != pBelow);

    Box* const pAbove = pBelow->GetPrev();
    if (NULL == pAbove)
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("No above box\n");
        #endif
        return;
    }

    ParentBox* const pParent = pAbove->GetParent();
    ASSERT(pBelow->GetParent() == pParent);

    if (pt.y - pAbove->m_rc.top < k_cyMinBox)
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("Above box is too small\n");
        #endif

        if (fMoving)
        {
            return;
        }

        // Destroy above box
        {
            Rect rc(pBelow->m_rc);
            rc.top = pAbove->m_rc.top;
            pBelow->Resize(&rc);
        }

        pAbove->Destroy();
        pContainer->drawSplitters();
        return;
    } // if

    if (pBelow->m_rc.bottom - pt.y - k_cySplitter < k_cyMinBox)
    {
        #if DEBUG_SPLIT
            DEBUG_PRINTF("Below box is too small\n");
        #endif

        if (fMoving)
        {
            return;
        }

        // Destroy below box
        {
            Rect rc(pAbove->m_rc);
            rc.bottom = pBelow->m_rc.bottom;
            pAbove->Resize(&rc);
        }

        pBelow->Destroy();
        pContainer->drawSplitters();
        return;
    } // if

    #if DEBUG_SPLIT
        DEBUG_PRINTF("Resize above and below\n");
    #endif

    {
        Rect rc(pAbove->m_rc);
        rc.bottom = pt.y;
        pAbove->Resize(&rc);
    }

    {
        Rect rc(pBelow->m_rc);
        rc.top = pt.y + k_cySplitter;
        pBelow->Resize(&rc);
    }

    pContainer->drawSplitters();
} // MoveSpillter

} // Peer
} // Editor
