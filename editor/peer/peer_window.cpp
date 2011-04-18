#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer
// editor/vi_window.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_window.cpp#8 $
//
#define DEBUG_DESTROY _DEBUG
#include "./peer_window.h"

#include "../ed_window.h"

#include "./peer_container.h"
#include "./peer_frame.h"

namespace Editor
{

namespace Peer
{

ATOM Window::sm_atomWndClass;
int Window::sm_iActiveTick;
Window* Window::sm_pCreateWnd;

static const char16* k_pwszClassName = L"EvitaWindow";

// [A]
bool Window::Activate()
{
    if (Container* const pContainer = GetParent())
    {
        ::SendMessage(
            *pContainer,
            PEER_WM_ACTIVATE,
            0,
            reinterpret_cast<LPARAM>(m_hwnd) );
    }

    return 0 != ::SetForegroundWindow(m_hwnd);
} // Window::Activate

// [C]
bool Window::CreateWindowEx(
    DWORD   const dwExStyle,
    LPCWSTR const pwszText,
    DWORD   const dwStyle,
    HWND    const hwndParent,
    int     const x,
    int     const y,
    int     const cx,
    int     const cy )
{
    if (0 == sm_atomWndClass)
    {
        WNDCLASSEXW oWC;
            oWC.cbSize          = sizeof(oWC);
            oWC.style           = CS_DBLCLKS |
                                  CS_BYTEALIGNCLIENT;
            oWC.lpfnWndProc     = windowProc;
            oWC.cbClsExtra      = 0;
            oWC.cbWndExtra      = 0;
            oWC.hInstance       = g_hInstance;
            oWC.hIcon           =
                ::LoadIconW(g_hResource, MAKEINTRESOURCE(IDI_APPLICATION));
            oWC.hCursor         = NULL;

            // FIXME 2008-01-13 yosi@msn.com We need to set background brush
            // for filling background during resizing. Otherwise, we'll see
            // black region during resizing.
            oWC.hbrBackground   =
                reinterpret_cast<HBRUSH>(COLOR_WINDOW + 1);

            oWC.lpszMenuName    = NULL;
            oWC.lpszClassName   = k_pwszClassName;
            oWC.hIconSm         =
                ::LoadIconW(g_hResource, MAKEINTRESOURCE(IDI_APPLICATION));

        sm_atomWndClass = ::RegisterClassExW(&oWC);

        if (0 == sm_atomWndClass)
        {
            PlatformError("RegisterClassEx");
        }
    } // if

    sm_pCreateWnd = this;

    HWND const hwnd = ::CreateWindowEx(
        dwExStyle,
        MAKEINTATOM(sm_atomWndClass),
        pwszText,
        dwStyle,
        x, y, cx, cy,
        hwndParent,
        NULL,   // hMenu
        g_hInstance,
        0 );    // lParam

    if (NULL == hwnd)
    {
        PlatformError("CreateWindowEx");
    } // if

    return true;
} // Window::CreateWindowEx

// [F]
Window* Window::FromHwnd(HWND const hwnd)
{
    if (::GetClassLongPtrW(hwnd, GCW_ATOM) != sm_atomWndClass)
    {
        return NULL;
    }
    return reinterpret_cast<Window*>(
        ::GetWindowLongPtrW(hwnd, GWLP_USERDATA) );
} // Window::FromHwnd

// [G]
Window* Window::GetFollowing() const
{
    if (Container* const pContainer = DynamicCast<Container>())
    {
        if (Window* const pFirst = pContainer->GetFirstChild())
        {
            return pFirst;
        }
    }

    Window* pWindow = const_cast<Window*>(this);

    while (NULL != pWindow)
    {
        if (Window* const pNext = pWindow->GetFollowingSibling())
        {
            return pNext;
        }

        pWindow = pWindow->GetParent();
    } // while

    return NULL;
} // Window::GetFollowing

Frame* Window::GetFrame() const
{
    Window* pRunner = const_cast<Window*>(this);
    for (;;)
    {
        if (Frame* const pFrame = pRunner->DynamicCast<Frame>())
        {
            return pFrame;
        }

        pRunner = pRunner->GetParent();
    } // for
} // Window::GetFrame

Window* Window::GetPreceding() const
{
    if (Container* const pContainer = DynamicCast<Container>())
    {
        if (Window* const pLast = pContainer->GetLastChild())
        {
            return pLast;
        }
    }

    Window* pWindow = const_cast<Window*>(this);

    while (NULL != pWindow)
    {
        if (Window* const pNext = pWindow->GetPrecedingSibling())
        {
            return pNext;
        }

        pWindow = pWindow->GetParent();
    } // while

    return NULL;
} // Window::GetPreceding

// [O]
LRESULT Window::onMessage(
    uint    const uMsg,
    WPARAM  const wParam,
    LPARAM  const lParam )
{
    switch (uMsg)
    {
    case PEER_WM_ACTIVATE:
        if (HWND const hwndParent = ::GetParent(m_hwnd))
        {
            ::SendMessage(hwndParent, PEER_WM_ACTIVATE, wParam, lParam);
        }
        break;

    case WM_DESTROY:
        m_iActiveTick = 0;
        break;

    case WM_TIMER:
        if (TimerHandler* const p = reinterpret_cast<TimerHandler*>(wParam))
        {
            p->OnTimer(this);
        }
        break;
    } // switch uMsg

    return ::DefWindowProc(m_hwnd, uMsg, wParam, lParam);
} // onMessage

// [R]
void Window::Realize(Container* const pParent)
{
    ASSERT(! IsRealized());

    const DWORD dwExStyle = 0;

    DWORD dwStyle = WS_CHILD | WS_VISIBLE;
    dwStyle |= GetStyle();

    CreateWindowEx(
        dwExStyle,
        NULL,
        dwStyle,
        NULL == pParent ? NULL : pParent->GetHwnd(),
        0,
        0,
        0,
        0 );
} // Window::Realize

// [W]
LRESULT CALLBACK Window::windowProc(
    HWND   const hwnd,
    UINT   const uMsg,
    WPARAM const wParam,
    LPARAM const lParam )
{
    Window* pWnd = reinterpret_cast<Window*>(
        static_cast<LONG_PTR>(::GetWindowLongPtrW(hwnd, GWLP_USERDATA)) );

    if (NULL == pWnd)
    {
        pWnd = sm_pCreateWnd;
        pWnd->m_hwnd = hwnd;

        ::SetWindowLongPtrW(
            hwnd,
            GWLP_USERDATA,
            static_cast<LONG>(reinterpret_cast<LONG_PTR>(pWnd)) );

        sm_pCreateWnd = NULL;
    } // if

    switch (uMsg)
    {
    case WM_CREATE:
        return pWnd->onCreate(reinterpret_cast<CREATESTRUCT*>(lParam));

    case WM_DESTROY:
        pWnd->onDestroy();
        return 0;

    case WM_NCDESTROY:
        //ASSERT(NULL == pWnd->GetParent());
        pWnd->m_hwnd = NULL;
        pWnd->onNcDestroy();
        return 0;

    case WM_SETFOCUS:
        sm_iActiveTick += 1;
        pWnd->m_iActiveTick = sm_iActiveTick;
        break;
    } // switch uMsg

    return pWnd->onMessage(uMsg, wParam, lParam);
} // Window::windowProc

} // Peer
} // Editor
