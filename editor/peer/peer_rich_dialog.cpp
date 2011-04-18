#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - TextWindow Peer
// editor/peer/peer_text_window.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_dialog.cpp#6 $
//
#define DEBUG_CONTROL   1
#define DEBUG_FOCUS     1
#include "./peer_rich_dialog.h"

#include "./peer_frame.h"
#include "./peer_rich_view.h"
#include "./peer_root_window.h"

namespace Editor
{

namespace Peer
{

void DispatchThreadMessage(MSG*);

uint RichDialog::sm_nModal;

/// <summary>
///   Construct RichDialog with specified Node.
/// </summary>
RichDialog::RichDialog(Node* pNode) :
    m_hwndDefaultButton(NULL),
    m_hwndFocus(NULL),
    m_pNode(pNode),
    m_pRootBox(NULL) {}

/// <summary>
///   Denstruct RichDialog.
/// </summary>
RichDialog::~RichDialog()
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif

    //delete m_pNode;
    if (NULL != m_pRootBox) m_pRootBox->Destroy();
} // RichDialog::~RichDialog

// [D]
/// <summary>
///   Enter modal dialog loop.
/// </summary>
/// <returns>TBD</returns>
int RichDialog::DoModal()
{
    {
        RichView::Formatter::Params oParams(VAR(Adialog_styleA));
        HDC const hdc = ::GetDC(NULL);
        m_pRootBox = m_pNode->Format(hdc, &oParams);
        ::ReleaseDC(NULL, hdc);
    }

    ASSERT(m_pRootBox->GetHeight() > 0);
    ASSERT(m_pRootBox->GetWidth() > 0);

    DWORD const dwExStyle =
        WS_EX_CONTROLPARENT |
        WS_EX_DLGMODALFRAME |
        WS_EX_TOOLWINDOW |
        WS_EX_WINDOWEDGE;

    DWORD const dwStyle =
        WS_CAPTION |
        WS_CLIPSIBLINGS |
        WS_POPUP |
        WS_SYSMENU |
        WS_VISIBLE |
        0;
        #if 0
        DS_3DLOOK |
        DS_SETFONT |
        DS_MODALFRAME;
        #endif

    HWND const hwndOwner = ::GetActiveWindow();

    HMONITOR const hMonitor = ::MonitorFromWindow(
        hwndOwner,
        MONITOR_DEFAULTTONEAREST );
    if (NULL == hMonitor)
    {
        PlatformError("MonitorFromWindow");
    }

    MONITORINFO oMonitor;
    oMonitor.cbSize = sizeof(oMonitor);
    if (! ::GetMonitorInfo(hMonitor, &oMonitor))
    {
        PlatformError("GetMonitorInfo");
    }

    int const cxDecoration = 10;
    int const cyDecoration = 50;

    Size const size(
        m_pRootBox->GetWidth() + cxDecoration,
        m_pRootBox->GetHeight() + cyDecoration );

    Point const pt(
        oMonitor.rcWork.left +
            (oMonitor.rcWork.right  - oMonitor.rcWork.left - size.cx) / 2,
        oMonitor.rcWork.top  +
            (oMonitor.rcWork.bottom - oMonitor.rcWork.top  - size.cy) / 2 );

    CreateWindowEx(
        dwExStyle,      // dwExStyle
        L"Rich Dialog", // pwszText
        dwStyle,        // dwStyle
        hwndOwner,     // hwndParent
        pt.x,           // x
        pt.y,           // y
        size.cx,        // cx
        size.cy );      // cy

    modalLoop(hwndOwner);

    return 0;
} // RichDialog::DoModal

// [G]
/// <summary>
///   Return Window handle of specified control, or NULL if there is no
///   such control in dialog.
/// </summary>
/// <param name="nCtrlId">A control identifier</param>
/// <returns>Window handle of control in this dialog.</returns>
HWND RichDialog::getControl(uint nCtrlId) const
{
    class WalkWindow
    {
        private: uint    const m_nCtrlId;
        private: HWND    m_hwnd;

        private: WalkWindow(uint nCtrlId) :
            m_nCtrlId(nCtrlId),
            m_hwnd(NULL) {}

        private: WalkWindow& operator=(const WalkWindow&)
            { return *this; }

        public: static HWND Run(HWND hwnd, uint nCtrlId)
        {
            WalkWindow oWalkWindow(nCtrlId);

            ::EnumChildWindows(
                hwnd,
                enumChildWindow,
                reinterpret_cast<LPARAM>(&oWalkWindow) );

            return oWalkWindow.m_hwnd;
        } // Run

        private: static BOOL CALLBACK enumChildWindow(
            HWND   const hwnd,
            LPARAM const lParam )
        {
            WalkWindow* pWalkWindow = reinterpret_cast<WalkWindow*>(lParam);
            uint nCtrlId = ::GetWindowLong(hwnd, GWL_ID);
            if (pWalkWindow->m_nCtrlId == nCtrlId)
            {
                pWalkWindow->m_hwnd = hwnd;
                return false;
            }
            return true;
        } // EnumChildWindow
    }; // WalkWindow

    return WalkWindow::Run(m_hwnd, nCtrlId);
} // RichDialog::getControl

/// <summary>
///  Retreive string from specified control. If this dialog doesn't
///  have specified control, this method returns empty string.
/// </summary>
/// <param name="nCtrlId">Control Identifier</param>
/// <returns>A string object</returns>
Val RichDialog::getControlText(uint const nCtrlId) const
{
    HWND hwnd = getControl(nCtrlId);
    if (NULL == hwnd)
    {
        return make_string(zero);
    }

    uint cwch = ::GetWindowTextLength(hwnd);
    Val str = make_string(Fixnum::Encode(cwch));
    ::GetWindowTextW(
        hwnd,
        str->StaticCast<SimpleString>()->GetStart(),
        cwch + 1 );

    return str;
} // RichDialog::getControlText

// [I]
HWND RichDialog::isButton(HWND const hwnd)
{
    if (NULL == hwnd)
    {
        return NULL;
    }

    if (! ::IsChild(GetHwnd(), hwnd))
    {
        return NULL;
    }

    uint const nCode = ::SendMessage(hwnd, WM_GETDLGCODE, 0, 0);

    #if DEBUG_FOCUS
        DEBUG_PRINTF("%p %x\n", hwnd, nCode);
    #endif

    if (nCode & (DLGC_DEFPUSHBUTTON | DLGC_UNDEFPUSHBUTTON))
    {
        return hwnd;
    }

    return NULL;
} // RichDialog::isButton

/// <summary>
///   Processes specified message. This method is called in message loop
///   for processing a message taken from GetMessage/PeekMessage.
///   <para>
///     Note: Some messages, e.g. WM_SETFOCUS, are not taken from GetMessage.
///   </para>
/// </summary>
bool RichDialog::IsDialogMessage(const MSG* const pMsg)
{
    ASSERT(NULL != pMsg);

    if (NULL == pMsg->hwnd)
    {
        return false;
    }

    if (! (pMsg->hwnd == GetHwnd() || ::IsChild(GetHwnd(), pMsg->hwnd)))
    {
        return false;
    }

    switch (pMsg->message)
    {
    case WM_CHAR:
    {
        if (pMsg->hwnd == GetHwnd())
        {
            // Ignore char for dialog box itself.
            return true;
        }

        uint nCode = static_cast<uint>(::SendMessage(
            pMsg->hwnd,
            WM_GETDLGCODE,
            pMsg->wParam,
            reinterpret_cast<LPARAM>(pMsg) ) );

        if (nCode & (DLGC_WANTCHARS | DLGC_WANTMESSAGE))
        {
            return false;
        }

        if (nCode & DLGC_WANTTAB)
        {
            if (pMsg->wParam == VK_TAB)
            {
                return false;
            }
        }

        // NYI: Check mnemonic in dialog box.

        return true;
    } // WM_CHAR

    case WM_KEYDOWN:
    {
        uint nCode = static_cast<uint>(::SendMessage(
            pMsg->hwnd,
            WM_GETDLGCODE,
            pMsg->wParam,
            reinterpret_cast<LPARAM>(pMsg) ) );

        if (nCode & (DLGC_WANTALLKEYS | DLGC_WANTMESSAGE))
        {
            return false;
        }

        switch (pMsg->wParam)
        {
        case VK_CANCEL:
        case VK_ESCAPE:
            ::PostMessage(GetHwnd(), WM_CLOSE, 0, 0);
            return true;

        case VK_DOWN:
        case VK_RIGHT:
            return motion(pMsg->hwnd, nCode, false);

        case VK_EXECUTE:
        case VK_RETURN:
        {
            // Send BN_CLICKED to IDOK
            if (HWND const hwndFocus = ::GetFocus())
            {
                uint const nCode = static_cast<uint>(::SendMessage(
                    hwndFocus,
                    WM_GETDLGCODE,
                    0,
                    0 ) );

                if (nCode & DLGC_DEFPUSHBUTTON)
                {
                    sendCommand(
                        ::GetDlgCtrlID(hwndFocus),
                        BN_CLICKED,
                        hwndFocus );
                    return true;
                }
            } // if

            if (NULL != m_hwndDefaultButton)
            {
                sendCommand(
                    ::GetDlgCtrlID(m_hwndDefaultButton),
                    BN_CLICKED,
                    m_hwndDefaultButton );
            }
            return true;
        } // VK_EXECUTE

        case VK_LEFT:
        case VK_UP:
            return motion(pMsg->hwnd, nCode, true);

        case VK_TAB:
        {
            if (nCode & DLGC_WANTTAB)
            {
                return false;
            }

            if (HWND const hwndNext = ::GetNextDlgTabItem(
                    GetHwnd(),
                    pMsg->hwnd,
                    ::GetKeyState(VK_SHIFT) & 0x8000 ) )
            {
                if (hwndNext == pMsg->hwnd)
                {
                    DEBUG_PRINTF("Only one dialog item\n");
                }
                else
                {
                    setFocus(hwndNext);
                }
            } // if

            return true;
        } // VK_TAB
        } // switch vkey

        return false;
    } // WM_KEYDOWN
    } // switch message

    return false;
} // RichDialog::IsDialogMessage

// [M]
/// <summary>
///   Modal dialog loop
/// </summary>
void RichDialog::modalLoop(HWND hwndOwner)
{
    RootWindow::Get()->Enable(false);

    if (HWND hwndCapture = ::GetCapture())
    {
        ::SendMessage(hwndCapture, WM_CANCELMODE, 0, 0);
    }

    bool fSentIdleMessage = false;
    sm_nModal += 1;
    while (sm_nModal > 0)
    {
        MSG oMsg;
        if (! ::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
        {
            if (hwndOwner)
            {
                if (! ::IsWindow(hwndOwner))
                {
                    hwndOwner = NULL;
                }
            }

            if (NULL != hwndOwner && ! fSentIdleMessage)
            {
                fSentIdleMessage = false;
                ::SendMessage(
                    hwndOwner,
                    WM_ENTERIDLE,
                    MSGF_DIALOGBOX,
                    reinterpret_cast<LPARAM>(GetHwnd()) );
                continue;
            } // if

            if (::GetMessage(&oMsg, NULL, 0, 0) < 0)
            {
                PlatformError("WaitMessage");
            }
        } // if

        fSentIdleMessage = false;

        if (WM_QUIT == oMsg.message)
        {
            ::PostQuitMessage(static_cast<int>(oMsg.wParam));
            break;
        }

        if (! IsDialogMessage(&oMsg))
        {
            if (NULL == oMsg.hwnd)
            {
                DispatchThreadMessage(&oMsg);
            }
            else
            {
                ::TranslateMessage(&oMsg);
                ::DispatchMessage(&oMsg);
            }
        }
    } // while

    RootWindow::Get()->Enable();

    if (NULL != hwndOwner)
    {
        #if _DEBUG
            Window* pWindow = Window::FromHwnd(hwndOwner);
            DEBUG_PRINTF("Active was %s.\n",
                NULL == pWindow ? "Unknown" : pWindow->GetKind() );
        #endif

        if (! ::SetForegroundWindow(hwndOwner))
        {
            DEBUG_PRINTF("SetForegroundWindow failed.\n");
        }
    }
} // RichDialog::modalLoop

bool RichDialog::motion(
    HWND const hwndCtrl,
    uint const nCode,
    bool const fPrevious  )
{
    if (nCode & (DLGC_WANTARROWS))
    {
        return false;
    }

    HWND const hwndNext = ::GetNextDlgGroupItem(GetHwnd(), hwndCtrl, fPrevious);
    if (NULL == hwndNext)
    {
        return true;
    }

    if (nCode & (DLGC_UNDEFPUSHBUTTON | DLGC_DEFPUSHBUTTON))
    {
        setFocus(hwndNext);
    }
    else if (nCode & DLGC_RADIOBUTTON)
    {
        // NYI: [RichDialog] BS_AUTORADIOBUTTON
        setFocus(hwndNext);
    }
    else if (! (nCode & DLGC_STATIC))
    {
        setFocus(hwndNext);
    }

    return true;
} // RichDialog::motion

// [O]
/// <summary>
///   Set focus to the first text control
/// </summary>
int RichDialog::onCreate(CREATESTRUCT*)
{
    class Walker : public RichView::Render::BoxWalker
    {
        private: typedef RichView::Render::ChildWindow ChildWindow;

        private: ChildWindow* m_pButton;
        private: ChildWindow* m_pTextBox;

        public: Walker() :
            m_pButton(NULL),
            m_pTextBox(NULL) {}

        public: HWND GetButton() const
        {
            if (NULL == m_pButton)
            {
                return NULL;
            }
            return m_pButton->GetHwnd();
        } // GetButton

        protected: override bool onVisit(RichView::Render::Box* pBox)
        {
            if (RichView::Render::TextBox* pTextBox =
                    pBox->DynamicCast<RichView::Render::TextBox>() )
            {
                if (NULL == m_pTextBox)
                {
                    m_pTextBox = pTextBox;
                    ::SetFocus(pTextBox->GetHwnd());

                    if (NULL != m_pButton)
                    {
                        return false;
                    }
                }
            }
            else if (RichView::Render::Button* pButton =
                        pBox->DynamicCast<RichView::Render::Button>() )
            {
                if (NULL == m_pButton)
                {
                    m_pButton = pButton;
                    pButton->SetDefault(true);

                    if (NULL != m_pTextBox)
                    {
                        return false;
                    }
                }
            }
            return true;
        } // onVisit
    }; // Walker

    m_pRootBox->Realize(GetHwnd());

    Walker oWalker;
    oWalker.Run(m_pRootBox);

    m_hwndDefaultButton = oWalker.GetButton();

    return 0;
} // RichDialog::onCreate

/// <summary>
///   A Window mesage handler.
/// </summary>
/// <param name="lParam">From GetMessag</param>
/// <param name="wParam">From GetMessag</param>
/// <param name="uMsg">From GetMessag</param>
/// <returns>LRESULT</returns>
LRESULT RichDialog::onMessage(
    uint    const uMsg,
    WPARAM  const wParam,
    LPARAM  const lParam )
{
    switch (uMsg)
    {
    case WM_ACTIVATE:
        DEBUG_PRINTF("WM_ACTIVATE: %p %p\n", wParam, lParam);
        if (WA_INACTIVE == wParam)
        {
            saveFocusItem();
        }
        else
        {
            restoreFocusItem();
        }
        return 0;

    case WM_COMMAND:
    {
        uint nCtrlId = LOWORD(wParam);
        uint nNotify = HIWORD(wParam);
        #if DEBUG_CONTROL
            DEBUG_PRINTF("WM_COMMAND notify=%d id=%d hwnd=%p\n",
                nNotify, nCtrlId, reinterpret_cast<HWND>(lParam) );
        #endif
        onCommand(nCtrlId, nNotify);
        break;
    } // WM_COMMAND

    case WM_KEYDOWN:
        DEBUG_PRINTF("WM_KEYDOWN: %x %x\n", wParam, lParam);
        return 0;

    case WM_PAINT:
    {
        // For RedrawWindow with the RDW_INTERNALPAINT
        if (::GetUpdateRect(m_hwnd, NULL, false))
        {
            PAINTSTRUCT ps;
            HDC hdc = ::BeginPaint(m_hwnd, &ps);
            RichView::Render::Params oParams(m_hwnd, hdc);
            oParams.m_rc = ps.rcPaint;
            m_pRootBox->Render(&oParams);
            ::EndPaint(m_hwnd, &ps);
        } // if
        return 0;
    } // WM_PAINT
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // RichDialog::onMessage

// [R]
void RichDialog::restoreFocusItem()
{
    if (NULL != m_hwndFocus && ::IsWindow(m_hwndFocus))
    {
        #if DEBUG_FOCUS
            DEBUG_PRINTF("%p\n", m_hwndFocus);
        #endif
        ::SetFocus(m_hwndFocus);
        m_hwndFocus = NULL;
    }
} // RichDialog::restoreFocusItem

// [S]
void RichDialog::saveFocusItem()
{
    if (NULL != m_hwndFocus)
    {
        return;
    }

    if (HWND const hwndFocus = ::GetFocus())
    {
        if (::IsChild(GetHwnd(), hwndFocus))
        {
            #if DEBUG_FOCUS
                DEBUG_PRINTF("%p\n", hwndFocus);
            #endif
            m_hwndFocus = hwndFocus;
        }
    }
} // RichDialog::saveFocusItem

void RichDialog::setFocus(HWND const hwnd)
{
    uint const nCode = ::SendMessage(hwnd, WM_GETDLGCODE, 0, 0);

    if (nCode & DLGC_HASSETSEL)
    {
        ::SendMessage(hwnd, EM_SETSEL, 0, MAXLONG);
    }

    if (nCode & (DLGC_UNDEFPUSHBUTTON | DLGC_DEFPUSHBUTTON))
    {
        if (NULL != m_hwndDefaultButton)
        {
            RichView::Render::Button::SetDefault(m_hwndDefaultButton, false);
            m_hwndDefaultButton = NULL;
        }

        RichView::Render::Button::SetDefault(hwnd, true);
        m_hwndDefaultButton = hwnd;
    }

    ::SetFocus(hwnd);
} // RichDialog::setFocus

} // Peer
} // Editor
