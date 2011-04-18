#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Frame Peer
// editor/peer/peer_frame.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_frame.cpp#10 $
//
#define DEBUG_RESIZE 0
#include "./peer_frame.h"

#include "../ed_buffer.h"
#include "../ed_frame.h"
#include "../ed_selection.h"
#include "../ed_text_window.h"

#include "./peer_root_window.h"
#include "./peer_split_container.h"
#include "./peer_tab_container.h"
#include "./peer_text_window.h"

#include "../rtl_defs.h"

namespace Editor
{

namespace Peer
{

static const int k_cxMargin = 0;
static const int k_cyMargin = 0;

/// <summary>
///   Construct Peer::Frame from lisp frame object and window object. We
///   insert lisp frame object into *frames* list for removing frame object
///   in  destructor.
/// </summary>
/// <param name="frame">A lisp frame object</param>
/// <param name="window">A lisp window object</param>
Frame::Frame(Val frame) :
    m_fBeingDestroy(false),
    m_pContainer(new TabContainer),
    WithObject(frame)
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif

    Super::AppendChild(m_pContainer);
} // Frame::Frame

/// <summary>
///   Remove associated lisp frame object from *frames* list.
/// </summary>
Frame::~Frame()
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif
} // Frame::~Frame

// [A]
Window* Frame::AppendChild(Window* pWindow)
{
    if (pWindow->Is<TextWindow>())
    {
        SplitContainer* pContainer = new SplitContainer;
        pContainer->AppendChild(pWindow);
        pWindow = pContainer;
    } // if text window

    return m_pContainer->AppendChild(pWindow);
} // Frame::AppendChild

// [C]
/// <summary>
///   Returns true if this frame is safe to close.
/// </summary>
bool Frame::CanClose() const
{
    if (NULL != GetFollowingSibling() || NULL != GetPrecedingSibling())
    {
        // There are more than one frame
        return true;
    }

    foreach (List::Enum, oEnum, VAR(AbuffersA))
    {
        if (! can_close_p(oEnum.Get()))
        {
            return false;
        }
    } // for each buffer

    return true;
} // Frame::CanClose

/// <summary>
///   Retreive container.
/// </summary>
Container* Frame::GetContainer() const
{
    ASSERT(NULL != m_pContainer);
    return m_pContainer;
} // Frame::GetContainer

// [O]
int Frame::onCreate(CREATESTRUCT*)
{
    m_oStatusBar.Realize(m_hwnd, CtrlId_StatusBar);
    m_oTitleBar.Realize(m_hwnd);
    m_pContainer->Realize(this);

    ::GetClientRect(m_hwnd, &m_rc);

    RootWindow::Get()->AppendChild(this);

    push(GetObject(), VAR(AframesA));
    
    return 0;
} // Frame::onCreate

void Frame::onDestroy()
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("WM_DESTROY %p\n", this);
    #endif

    m_fBeingDestroy = true;
    RootWindow::Get()->RemoveChild(this);

    unrealize_instance(GetObject());
} // Frame::onDestroy

/// <summary>
///   Frame idle processing.
/// </summary>
bool Frame::OnIdle(uint nCount)
{
    bool fMore = m_pContainer->OnIdle(nCount);

    if (! fMore)
    {
        if (! m_pContainer->DrawStatusBar(&m_oStatusBar))
        {
            m_oStatusBar.SetSimpleText(L"None");
        }

        if (! m_pContainer->DrawTitleBar(&m_oTitleBar))
        {
            m_oTitleBar.SetText(L"Evita");
        }
    }

    return fMore;
} // Frame::OnIdle

/// <summary>
///   Windows message handler.
/// </summary>
LRESULT Frame::onMessage(
    uint    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case WM_CLOSE:
        if (! CanClose()) return 0;
        break;

    case WM_PARENTNOTIFY:
        if (m_fBeingDestroy)
        {
            return 0;
        }

        switch (wParam)
        {
        case WM_DESTROY:
        {
            Window* pWindow = Window::FromHwnd(
                reinterpret_cast<HWND>(lParam) );

            if (NULL == pWindow)
            {
                break;
            }

            if (pWindow == m_pContainer)
            {
                ::DestroyWindow(m_hwnd);
            }
            break;
        } // WM_DESTROY
        } // switch wParam
        return 0;

    case WM_SETFOCUS:
        GetPeer()->m_active_tick = xxadd(GetPeer()->m_active_tick, one);
        ::SetFocus(*m_pContainer);
        return 0;

    case WM_WINDOWPOSCHANGED:
    {
        const WINDOWPOS* wp = reinterpret_cast<WINDOWPOS*>(lParam);

        #if DEBUG_RESIZE
            DEBUG_PRINTF("WM_WINDOWPOSCHANGED:"
                " flags=0x%0x\r\n",
                wp->flags );
        #endif

        if (wp->flags & SWP_HIDEWINDOW)
        {
            // We don't take care hidden window.
            return 0;
        } // if

        if (0 == (wp->flags & 0x10000000))
        {
            if (wp->flags & SWP_NOSIZE)
            {
                return 0;
            } // if
        } // if

        if (::IsIconic(m_hwnd))
        {
            // We don't take care miminize window.
            return 0;
        } // if

        ::GetClientRect(m_hwnd, &m_rc);

        Rect rc = m_rc;
        rc.left   += k_cxMargin;
        rc.right  -= k_cyMargin;
        rc.top    += k_cxMargin;
        rc.bottom -= k_cyMargin;

        // StatusBar
        {
            ::SetWindowPos(
                m_oStatusBar,
                NULL,
                m_rc.left,
                m_rc.bottom - m_oStatusBar.GetHeight(),
                m_rc.right - m_rc.left,
                m_oStatusBar.GetHeight(),
                SWP_NOZORDER );

            char16 wsz[100];
            ::wsprintf(wsz, L"Resizing... %dx%d",
                m_rc.right - m_rc.left,
                m_rc.bottom - m_rc.top );

            m_oStatusBar.SetSimpleText(wsz);

            rc.bottom -= m_oStatusBar.GetHeight();
        }

        ::SetWindowPos(
            *m_pContainer,
            NULL,
            rc.left,
            rc.top,
            rc.GetWidth(),
            rc.GetHeight(),
            SWP_NOZORDER );

        return 0;
    } // WM_WINDOWPOSCHANGED
    } // switch uMsg
    return Super::onMessage(uMsg, wParam, lParam);
} // Frame::onMessage

// [R]
void Frame::Realize(Container*)
{
    ASSERT(! IsRealized());

    DWORD dwExStyle =
        WS_EX_APPWINDOW |
        WS_EX_WINDOWEDGE;

    DWORD dwStyle =
        WS_OVERLAPPEDWINDOW;

    CreateWindowEx(
        dwExStyle,
        L"Evita",
        dwStyle,
        NULL,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT );
} // Frame::Realize

} // Peer
} // Editor
