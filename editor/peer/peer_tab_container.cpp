#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - Tab Container
// editor/peer/peer_tab_container.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_tab_container.cpp#5 $
//
#define DEBUG_IDLE      1
#define DEBUG_REDRAW    1
#define DEBUG_RESIZE    1
#define DEBUG_TAB       1
#include "./peer_tab_container.h"

#include "./ctrl_TabBand.h"

#include "./peer_defs.h"
#include "./peer_frame.h"

namespace Editor
{

namespace Peer
{

static void drawDecoration(HDC hdc, const RECT* prcContent)
{
    {
        RECT rc;
        rc.left   = prcContent->left  - 2;
        rc.top    = prcContent->top   - 4;
        rc.right  = prcContent->right + 2;
        rc.bottom = prcContent->top   - 2;
        fillRect(hdc, &rc, RGB(183, 200, 246));
    }

    {
        RECT rc = *prcContent;
        rc.left   -= 2;
        rc.top    -= 2;
        rc.right  += 2;
        rc.bottom += 2;
        ::DrawEdge(hdc, &rc, EDGE_SUNKEN, BF_RECT);
    }
} // drawDecoration

// ctor
TabContainer::TabContainer() :
    m_cyTabBand(0),
    m_hwndTabBand(NULL)
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif
} // TabContainer::TabContainer

TabContainer::~TabContainer()
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif
} // TabContainer::~TabContainer

// [A]
Window* TabContainer::AppendChild(Window* pWindow)
{
    Super::AppendChild(pWindow);
    addTab(pWindow);
    return pWindow;
} // TabContainer::AppendChild

Window* TabContainer::addTab(Window* pWindow)
{
    if (NULL != m_hwndTabBand)
    {
        int iTab = TabCtrl_GetItemCount(m_hwndTabBand);

        {
            Rect rc = computeContentRect();

            ::SetWindowPos(
                *pWindow,
                NULL,
                rc.left,
                rc.top,
                rc.GetWidth(),
                rc.GetHeight(),
                SWP_NOZORDER );
        }

        {
            char16 wsz[100];
            ::GetWindowText(*pWindow, wsz, lengthof(wsz));

            TCITEM oItem;
            oItem.mask = TCIF_PARAM | TCIF_TEXT;
            oItem.lParam  = reinterpret_cast<LPARAM>(pWindow);
            oItem.pszText = wsz;
            TabCtrl_InsertItem(m_hwndTabBand, iTab, &oItem);
            TabCtrl_SetCurSel(m_hwndTabBand, iTab);
        }
    } // if

    return pWindow;
} // TabContainer::addTab

// [C]
Rect TabContainer::computeContentRect() const
{
    Rect rc;

    rc.left   = m_rc.left + 2;
    rc.top    = m_rc.top  + m_cyTabBand + 4;
    rc.right  = m_rc.right - 2;
    rc.bottom = m_rc.bottom - 2;

    return rc;
} // TabContainer::computeContentRect


// [M]
Window* TabContainer::mapTabToWindow(int iTab) const
{
    if (iTab < 0)
    {
        return NULL;
    }

    TCITEM oItem;
    oItem.mask = TCIF_PARAM;
    LRESULT lResult = ::SendMessage(
        m_hwndTabBand,
        TCM_GETITEM,
        iTab,
        reinterpret_cast<LPARAM>(&oItem) );
    if (! lResult)
    {
        return NULL;
    }

    return reinterpret_cast<Window*>(oItem.lParam);
} // TabContainer::mapTabToWindow

int TabContainer::mapWindowToTab(Window* pWindow) const
{
    int iTab = 0;
    for (;;)
    {
        TCITEM oItem;
        oItem.mask = TCIF_PARAM;
        LRESULT lResult = ::SendMessage(
            m_hwndTabBand,
            TCM_GETITEM,
            iTab,
            reinterpret_cast<LPARAM>(&oItem) );
        if (! lResult)
        {
            break;
        }

        if (oItem.lParam == reinterpret_cast<LPARAM>(pWindow))
        {
            return iTab;
        }

        iTab += 1;
    } // for
    return -1;
} // TabContainer::mapWindowToTab

// [O]
int TabContainer::onCreate(CREATESTRUCT*)
{
    {
        m_hwndTabBand = ::CreateWindowEx(
            0,
            WC_TABBANDCLASS,
            NULL,
            WS_CHILD | WS_VISIBLE | TCS_TOOLTIPS,
            0,  // left
            0,  // top
            0,  // cx
            0,  // cy
            m_hwnd,
            reinterpret_cast<HMENU>(CtrlId_TabBand),
            g_hInstance,
            NULL );
            
        if (NULL == m_hwndTabBand)
        {
            PlatformError("CreateWindowEx");
        }

        RECT rc;
        ::GetWindowRect(m_hwndTabBand, &rc);
        m_cyTabBand = rc.bottom - rc.top;
    }

    foreach (EnumChild, oEnum, this)
    {
        Window* pWindow = oEnum.Get();
        pWindow->Realize(this);
        addTab(pWindow);
    } // for each window

    TabCtrl_SetCurSel(m_hwndTabBand, 0);

    return 0;
} // TabContainer::onCreate

bool TabContainer::OnIdle(uint nCount)
{
    if (Window* pActive = GetActiveWindow())
    {
        Container* pContainer = pActive->GetParent();
        if (pContainer != this)
        {
            return pContainer->OnIdle(nCount);
        }
        else
        {
            return pActive->OnIdle(nCount);
        }
    } // if active window
    return false;
} // TabContainer::OnIdle

LRESULT TabContainer::onMessage(
    uint    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case PEER_WM_ACTIVATE:
    {
        if (Window* pWindow = Window::FromHwnd(lParam))
        {
            int iTab = mapWindowToTab(pWindow);
            if (iTab >= 0)
            {
                TabCtrl_SetCurSel(m_hwndTabBand, iTab);
            }
        }
        break;
    } // PEER_WM_ACTIVATE

    case WM_NOTIFY:
    {
        NMHDR* pNotify = reinterpret_cast<NMHDR*>(lParam);

        switch (pNotify->idFrom)
        {
        case CtrlId_TabBand:
            switch (pNotify->code)
            {
            case TABBAND_NOTIFY_CLOSE:
            {
                int iCurSel = TabCtrl_GetCurSel(m_hwndTabBand);
                if (Window* pWindow = mapTabToWindow(iCurSel))
                {
                    if (! CanClose()) return false;

                    ::DestroyWindow(*pWindow);
                }
                return true;
            } // TABBAND_NOTIFY_CLOSE

            case TCN_SELCHANGE:
            {
                int iCurSel = TabCtrl_GetCurSel(m_hwndTabBand);
                if (Window* pWindow = mapTabToWindow(iCurSel))
                {
                    ASSERT(m_pActive != pWindow);

                    if (NULL != m_pActive)
                    {
                        ::ShowWindow(*m_pActive, SW_HIDE);
                    }

                    #if DEBUG_TAB
                        DEBUG_PRINTF("TCN_SELCHANGE: %p\n", pWindow);
                    #endif

                    m_pActive = pWindow;
                    ::ShowWindow(*m_pActive, SW_SHOW);
                    ::SetFocus(*pWindow);

                    updateTitle();
                } // if pWindow
            } // TCN_SELCHANGE
            } // switch code
            break;
        } // switch idFrom
        return 0;
    } // WM_NOTIFY

    case WM_PAINT:
    {
        PAINTSTRUCT ps;
        HDC hdc = ::BeginPaint(m_hwnd, &ps);

        Rect rc = computeContentRect();
        drawDecoration(hdc, &rc);

        ::EndPaint(m_hwnd, &ps);
        return 0;
    } // WM_PAINT

    case WM_PARENTNOTIFY:
        switch (wParam)
        {
        case WM_DESTROY:
            if (Window* pWindow = Window::FromHwnd(lParam))
            {
                RemoveChild(pWindow);
            } // if window
            break;
        } // switch wParam
        return 0;

    case WM_SETFOCUS:
        if (NULL != m_pActive)
        {
            ::SetFocus(*m_pActive);
        }
        return 0;

    case WM_WINDOWPOSCHANGED:
    {
        const WINDOWPOS* wp = reinterpret_cast<WINDOWPOS*>(lParam);

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

        ::GetClientRect(m_hwnd, &m_rc);

        ::SetWindowPos(
            m_hwndTabBand,
            NULL,
            m_rc.left,
            m_rc.top,
            m_rc.GetWidth(),
            m_cyTabBand,
            SWP_NOZORDER );

        Rect rc = computeContentRect();

        foreach (EnumChild, oEnum, this)
        {
            Window* pWindow = oEnum.Get();
            ::SetWindowPos(
                *pWindow,
                NULL,
                rc.left,
                rc.top,
                rc.GetWidth(),
                rc.GetHeight(),
                SWP_NOZORDER );
        } // for each winow

        {
            Dc hdc(*this);
            drawDecoration(hdc, &rc);
        }

        return 0;
    } // WM_WINDOWPOSCHANGED
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // TabContainer::onMessage

// [R]
Window* TabContainer::RemoveChild(Window* pWindow)
{
    if (m_pActive == pWindow)
    {
        m_pActive = NULL;
    }

    int iTab = mapWindowToTab(pWindow);
    TabCtrl_DeleteItem(m_hwndTabBand, iTab);

    Super::RemoveChild(pWindow);

    if (ChildWindows::IsEmpty())
    {
        ::DestroyWindow(*this);
    }

    return pWindow;
} // TabContainer::onRemove

Window* TabContainer::ReplaceChild(Window* pNew, Window* pOld)
{
    int iTab = mapWindowToTab(pOld);
    if (iTab < 0)
    {
        return pNew;
    }

    ChildWindows::Replace(pNew, pOld);
    
    char16 wsz[100];
    ::GetWindowText(*pNew, wsz, lengthof(wsz));

    if (m_pActive == pOld)
    {
        m_pActive = pNew;
    }

    TCITEM oItem;
    oItem.mask = TCIF_PARAM | TCIF_TEXT;
    oItem.lParam  = reinterpret_cast<LPARAM>(pNew);
    oItem.pszText = wsz;
    TabCtrl_SetItem(m_hwndTabBand, iTab, &oItem);

    return pNew;
} // TabContainer::ReplaceChild

} // Peer
} // Editor
