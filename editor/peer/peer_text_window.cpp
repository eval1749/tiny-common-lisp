#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - TextWindow Peer
// editor/peer/peer_text_window.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_text_window.cpp#20 $
//
#define DEBUG_AUTOSCROLL    0
#define DEBUG_BLINK         0
#define DEBUG_CARET         0
#define DEBUG_FOCUS         0
#define DEBUG_IDLE          0
#define DEBUG_IME           0
#define DEBUG_REDRAW        0
#define DEBUG_RESIZE        0
#define DEBUG_SCROLL        0
#define DEBUG_SCROLLBAR     0
#include "./peer_text_window.h"

#include "../cm_cmdl.h"

#include "./ctrl_StatusBar.h"
#include "./ctrl_TitleBar.h"

#include "../ed_buffer.h"
#include "../ed_interval.h"
#include "../ed_range.h"
#include "../ed_selection.h"
#include "../ed_style.h"
#include "../ed_text_window.h"
#include "../ed_window.h"

#include "./peer_text_page.h"

#define IDS_ASK_REFRESH "~A~%This file has been modified outside of the editor.~%Do you want to reload it?"
#define IDS_ASK_RELOAD "Do you really want to discard the changes you made to and revert to saved ~A?"

namespace Editor
{

namespace Peer
{

uint IsGraphicKey(uint nVKey)
{
    static uint s_rgnGraphicKey[0x100];
    ASSERT(nVKey < lengthof(s_rgnGraphicKey));

    if (0 == s_rgnGraphicKey['A'])
    {
        for (uint nVKey = 0; nVKey <= 255; nVKey++)
        {
            uint nChar = ::MapVirtualKey(nVKey, MAPVK_VK_TO_CHAR);

            if (nChar >= 0x20)
            {
                s_rgnGraphicKey[nVKey] = nChar;
            }
        } // for each nVKey
    }

    return s_rgnGraphicKey[nVKey];
} // IsGraphicKey

//////////////////////////////////////////////////////////////////////
//
// Caret
//
// Description:
//  Represents caret in per-thread queue. To blink caret, we must track
//  caret size. If we call CreateCaret, caret doesn't blink.
//
class Caret
{
    SIZE    m_size;
    POINT   m_pt;
    HWND    m_hwnd;
    bool    m_fShow;

    public: Caret() :
        m_hwnd(NULL),
        m_fShow(false) {}

    public: void Destroy()
    {
        m_hwnd = NULL;
        ::DestroyCaret();
        #if DEBUG_CARET
            DEBUG_PRINTF("%p\n", this);
        #endif // DEBUG_CARET
        m_size.cx = -1;
        m_pt.x = -1;
    } // Destroy

    // Hide - Hide caret
    public: void Hide()
    {
        if (NULL == m_hwnd) return;
        if (! m_fShow) return;
        #if DEBUG_CARET
            DEBUG_PRINTF("%p\n", this);
        #endif // DEBUG_CARET

        ::HideCaret(m_hwnd);
        m_fShow = false;
    } // Hide

    /// <summary>
    ///   Returns true if position or size of caret is changed since last
    ///   show.
    /// </summary>
    bool isDirty(HWND hwnd, SIZE size) const
    {
        if (m_hwnd != hwnd)
        {
            #if DEBUG_CARET
                DEBUG_PRINTF("m_hwnd\n");
            #endif
            return true;
        }

        if (m_size.cx != size.cx)
        {
            #if DEBUG_CARET
                DEBUG_PRINTF("m_size.cx %d %d\n", m_size.cx, size.cx);
            #endif
            return true;
        }

        if (m_size.cy != size.cy)
        {
            #if DEBUG_CARET
                DEBUG_PRINTF("m_size.cy\n");
            #endif
            return true;
        }

        return false;
    } // isDirty

    // Show - Show caret at pt with size.
    public: void Show(HWND hwnd, SIZE size, POINT pt)
    {
        if (isDirty(hwnd, size))
        {
            m_hwnd = hwnd;
            m_size = size;
            ::CreateCaret(m_hwnd, NULL, size.cx, size.cy);
            #if DEBUG_CARET
            {
                DEBUG_PRINTF("Create %dx%d\r\n",
                    size.cx, size.cy);
            }
            #endif // DEBUG_CARET
        } // if

        if (m_pt.x != pt.x || m_pt.y != pt.y)
        {
            m_pt = pt;
            ::SetCaretPos(m_pt.x, m_pt.y);
            #if DEBUG_CARET
            {
                DEBUG_PRINTF("SetCaretPos (%d, %d)\r\n",
                    pt.x, pt.y );
            }
            #endif // DEBUG_CARET
        } // if

        #if DEBUG_CARET
            DEBUG_PRINTF("(%d, %d) %dx%d\n",
                m_pt.x, m_pt.y, m_size.cx, m_size.cy );
        #endif // DEBUG_CARET
        ::ShowCaret(m_hwnd);
        m_fShow = true;
    } // Show
}; // Caret

Caret g_oCaret;

void TextWindow::AutoScroll::OnTimer(Window* p)
{
    int iDuration = ::GetTickCount() - m_nStartTick;
    int iCount = iDuration / 500;
    iCount = max(iCount, 1);
    iCount = min(iCount, 20);

    TextWindow* pWindow = p->StaticCast<TextWindow>();

    Val count = Fixnum::Encode(iCount);
    if (m_iDirection > 0)
    {
        pWindow->SmallScroll(0, iCount);
        count = pWindow->GetSelection()->MoveDown(Kline, count, Kextend);
    }
    else
    {
        pWindow->SmallScroll(0, -iCount);
        count = pWindow->GetSelection()->MoveUp(Kline, count, Kextend);
    }

    if (zero == count)
    {
        #if DEBUG_AUTOSCROLL
            DEBUG_PRINTF("Stop\n");
        #endif
        Stop();
    }
    else
    {
        #if DEBUG_AUTOSCROLL
            DEBUG_PRINTF("Continue\n");
        #endif
        Continue(50);
    }
} // TextWindow::AutoScroll::OnTimer

static HCURSOR s_hIBeam;

// TextWindow ctor
TextWindow::TextWindow(Val window) :
    Super(window),
    m_fHasFocus(false),
    #if SUPPORT_IME
        m_fImeTarget(false),
        m_ime_end(zero),
        m_ime_start(zero),
    #endif // SUPPORTIME
    m_pCurPage(NULL),
    m_pNewPage(NULL),
    m_caret_posn(zero)
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif
} // TextWindow::TextWindow

// TextWindow dtor
TextWindow::~TextWindow()
{
    #if DEBUG_LIFE
        DEBUG_PRINTF("%p\n", this);
    #endif

    // FIXME 2007-12-23 yosi@msn.com remove selection
    // FIXME 2007-12-23 yosi@msn.com remove range
    if (NULL != m_pCurPage) m_pCurPage->Destroy();
    if (NULL != m_pNewPage) m_pNewPage->Destroy();
} // TextWindow::~TextWindow

// [B]

/// <summary>
///   Start blinking caret at specified position during specified
///   seconds. Current caret position will be restored after blinking.
/// </summary>
/// <param name="posn">A position.</param>
/// <param name="wait">A number of second in integer.</param>
/// <seealso cref="TextWindow::Blink"/>
void TextWindow::Blink(Posn posn, Val wait)
{
    getPeer()->m_blink->StaticCast<Range>()->SetRange(posn, posn);
    m_oBlink.Start(m_hwnd, Fixnum::Decode_(wait));
    redraw();
} // TextWindow::Blink

// [C]
bool TextWindow::checkObsolete()
{
    Buffer* pBuffer = GetBuffer();
    if (pBuffer->QueryFileState() != Kobsolete)
    {
        return true;
    }

    Val msg = TinyCl::CommonLisp::format(
        nil, IDS_ASK_REFRESH, pBuffer->m_name );

    int iAnswer = ::MessageBoxW(
        NULL,
        msg->StaticCast<SimpleString>()->GetStart(),
        L"Evita",
        MB_YESNO | MB_ICONWARNING | MB_SETFOREGROUND | MB_TOPMOST );

    switch (iAnswer)
    {
    case IDNO:
        return true;

    case IDYES:
        pBuffer->Reload();
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch iAnswer

    return false;
} // TextWindow::checkObsolete

/// <summary>
///   Creates clone of this window.
/// </summary>
Window* TextWindow::Clone() const
{
    Editor::TextWindow* const pWinObj = new Editor::TextWindow(
        GetSelection()->m_buffer );

    realize_instance(pWinObj->Encode());
    return pWinObj->GetPeer();
} // TextWindow::Clone

/// <summary>
///   Compute position after moving specified unit.
/// </summary>
Count TextWindow::ComputeMotion(
    Unit    unit,
    Count   n,
    POINT   pt,
    Posn*   inout_posn )
{
    ASSERT(NULL != inout_posn);

    check_type(n, fixnum);

    if (Kline == unit)
    {
        Int iK = 0;
        Int iN = Fixnum::Decode_(n);
        if (xgt(n, zero))
        {
            const Posn buf_end = GetBuffer()->GetEnd();
            Posn goal = *inout_posn;
            for (iK = 0; iK < iN; iK++)
            {
                goal = EndOfLine(goal);
                if (xge(goal, buf_end)) break;
                goal = xadd(goal, one);
            } // for k

            *inout_posn = mapXToPosn(pt.x, xmin(goal, buf_end));
        }
        else if (xlt(n, zero))
        {
            iN = -iN;
            const Posn buf_start = GetBuffer()->GetStart();
            Posn start = *inout_posn;
            for (iK = 0; iK < iN; iK++)
            {
                start = StartOfLine(start);
                if (xle(start, buf_start)) break;
                start = xsub(start, one);
            } // for k

            *inout_posn = mapXToPosn(pt.x, xmax(start, buf_start));
        }

        return Fixnum::Encode(iK);
    } // line

    if (Kscreen == unit)
    {
        Dc hdc(m_hwnd);
        FormatParams oParams;
        setupFormat(&oParams);

        if (NULL == m_pNewPage)
        {
            Val start;
            if (m_pCurPage->IsClean(&oParams) &&
                m_pCurPage->IsPosnFullyVisible(*inout_posn) )
            {
                start = m_pCurPage->GetStart();
            }
            else
            {
                start = GetBuffer()->ComputeStartOf(
                    Kparagraph,
                    *inout_posn );
            }

            m_pNewPage = TextPage::Create(hdc, &oParams, start, start);
        }
        else if (! m_pNewPage->IsPosnFullyVisible(*inout_posn))
        {
            #if DEBUG_SCROLL
                DEBUG_FORMAT("New:[~D, ~D]  p=~D\n",
                    m_pNewPage->GetStart(), m_pNewPage->GetEnd(),
                    Fixnum::Encode(*inout_posn) );
            #endif

            m_pNewPage->ScrollToPosn(hdc, *inout_posn);
        } // if

        // Try position is at point by scrolling
        {
            Posn target = *inout_posn;

            for (;;)
            {
                TextLine* pLine = m_pNewPage->MapYToLine(pt.y);
                #if DEBUG_SCROLL
                    DEBUG_FORMAT("[~D, ~D] target=~D\n",
                        pLine->GetStart(), pLine->GetEnd(),
                        target );
                #endif

                if (target < pLine->GetStart())
                {
                    if (! m_pNewPage->ScrollDown(hdc))
                    {
                        break;
                    }
                }

                if (target >= pLine->GetEnd())
                {
                    if (! m_pNewPage->ScrollUp(hdc))
                    {
                        break;
                    }
                }

                break;
            } // for
        }

        Int iDy = Fixnum::Decode_(n);

        int iK = 0;
        if (iDy < 0)
        {
            // Scroll Down -- place top line out of window.
            iDy = -iDy;
            const Posn buf_start = GetBuffer()->GetStart();
            for (iK = 0; iK < iDy; iK++)
            {
                Posn start = m_pNewPage->GetStart();
                if (start == buf_start)
                {
                    break;
                }

                // Scroll down until page start goes out from page.
                do
                {
                    if (! m_pNewPage->ScrollDown(hdc))
                    {
                        break;
                    }

                    #if DEBUG_SCROLL
                        DEBUG_PRINTF("scroll down view.end=%d start=%d\n",
                            Fixnum::Decode_(m_pNewPage->GetEnd()),
                            Fixnum::Decode_(start) );
                    #endif

                } while (m_pNewPage->GetEnd() != start);
            } // for iK
        }
        else if (iDy > 0)
        {
            const Posn buf_end = GetBuffer()->GetEnd();
            for (iK = 0; iK < iDy; iK++)
            {
                Posn start = m_pNewPage->GetEnd();
                if (xge(start, buf_end))
                {
                    break;
                }

                #if DEBUG_SCROLL
                    DEBUG_PRINTF("scroll up start=%d\n",
                        Fixnum::Decode_(start) );
                #endif

                format(hdc, &oParams, start);
            } // for iK
        } // if

        #if DEBUG_SCROLL
            DEBUG_PRINTF("k=%ld pt=%d@%d\n", iK, pt.x, pt.y);
        #endif

        if (iK > 0)
        {
            Posn start = m_pNewPage->GetStart();
            rememberPageStart(start);

            Dc hdc(m_hwnd);
            *inout_posn = m_pNewPage->MapPointToPosn(hdc, pt);
        }
        else if (xgt(n, zero))
        {
            *inout_posn = GetEnd();
            iK = 1;
        }
        else if (xlt(n, zero))
        {
            pt.y = 0;
            *inout_posn = m_pNewPage->MapPointToPosn(hdc, pt);
            iK = 1;
        }
        return Fixnum::Encode(iK);
    } // screen

    if (Kwindow == unit)
    {
        if (xgt(n, zero))
        {
            *inout_posn = GetEnd();
            return one;
        }
        else if (xlt(n, zero))
        {
            *inout_posn = GetStart();
            return one;
        }
        return zero;
    } // window

    return GetBuffer()->ComputeMotion(unit, n, inout_posn);
} // TextWindow::ComputeMotion

// [D]
bool TextWindow::DrawStatusBar(StatusBar* const pStatusBar)
{
    char16 wsz[200];

    int const iMemUse = static_cast<int>(
        (Si::Mm::GetCommit()->ToInt() - Si::Mm::GetStart()->ToInt()) /
        1024 );

    int const iMemMax = static_cast<int>(
        (Si::Mm::GetEnd()->ToInt() - Si::Mm::GetStart()->ToInt()) /
        1024 );

    Selection* const pSelection = GetSelection();
    Buffer* const pBuffer = GetBuffer();

    if (pSelection->GetStart() != pSelection->GetEnd())
    {
        ::wsprintfW(wsz,
            L"Ready Sel:[%d, %d]/%d Mem:%d/%d KB",
            Fixnum::Decode_(pSelection->GetStart()),
            Fixnum::Decode_(pSelection->GetEnd()),
            Fixnum::Decode_(pBuffer->GetEnd()),
            iMemUse,
            iMemMax );
    }
    else
    {
        ::wsprintfW(wsz,
            L"Ready Sel:%d/%d Mem:%d/%d KB",
            Fixnum::Decode_(pSelection->GetStart()),
            Fixnum::Decode_(pBuffer->GetEnd()),
            iMemUse,
            iMemMax );
    }

    pStatusBar->SetSimpleText(wsz);

    return true;
} // TextWindow::DrawStatusBar

bool TextWindow::DrawTitleBar(TitleBar* const pTitleBar)
{
    char16 wsz[200];

    ::wsprintfW(wsz, L"%ls - Evita",
        GetBuffer()->m_name->StaticCast<SimpleString>()->GetStart() );

    pTitleBar->SetText(wsz);

    return true;
} // TextWindow::DrawTitleBar

// [E]
Posn TextWindow::EndOfLine(Posn posn)
{
    FormatParams oParams;
    setupFormat(&oParams);
    if (m_pCurPage->IsClean(&oParams))
    {
        TextLine* pLine = m_pCurPage->FindLine(posn);
        if (NULL != pLine)
        {
            return xsub(pLine->GetEnd(), 1);
        }
    }

    Buffer* pBuffer = GetBuffer();

    const Posn buf_end = pBuffer->GetEnd();
    if (xge(posn, buf_end))
    {
        return buf_end;
    }

    Dc hdc(m_hwnd);
    TextPage oPage(&oParams);
    Posn start = pBuffer->ComputeStartOf(Kparagraph, posn);
    for (;;)
    {
        TextLine* pLine = oPage.FormatLine(hdc, start);
        Posn end = pLine->GetEnd();
        if (xlt(posn, end))
        {
            return xsub(end, 1);
        }
        start = end;
    } // for
} // TextWindow::EndOfLine

// [F]
// Helper function for WM_CHAR and WM_KEY
// TODO yosi@msn.com 2008-06-19 We will call TextWindow::fireEvent with
// mouse events, e.g. mouse-down/up, wheel-up/down and file-drop.
void TextWindow::fireEvent(Val event)
{
    funcall(Qdispatch_event, VAR(Acommand_stateA), GetObject(), event);
} // TextWindow::fireEvent

void TextWindow::format(HDC hdc, const FormatParams* pParams, Posn start)
{
    format(hdc, pParams, start, start);
} // TextWindow::format

void TextWindow::format(
    HDC                 hdc,
    const FormatParams* pParams,
    Posn                start,
    Posn                target )
{
    if (NULL != m_pNewPage)
    {
        m_pNewPage->Destroy();
        m_pNewPage = NULL;
    }

    m_pNewPage = TextPage::Create(hdc, pParams, start, target);
} // TextWindow::format

// [G]
Buffer* TextWindow::GetBuffer() const
    { return GetSelection()->GetBuffer(); }

Posn TextWindow::GetEnd() const
    { return xmin(m_pCurPage->GetEnd(), GetBuffer()->GetEnd()); }

Selection* TextWindow::GetSelection() const
    { return getPeer()->GetSelection(); }

Posn TextWindow::GetStart() const
    { return m_pCurPage->GetStart(); }

// [I]
bool TextWindow::isSelectionActive() const
{
    // FIXME 2007-12-23 yosi@msn.com We need to check active modless dialog.
    return m_fHasFocus;
} // TextWindow::isSelectionActive

// [L]
/// <summary>
///  Equivalent of scroll bar click.
/// </summary>
/// <returns>Number of scrolled lines</returns>
int TextWindow::LargeScroll(int, int iDy)
{
    Dc hdc(m_hwnd);
    FormatParams oParams;
    setupFormat(&oParams);
    prepareScroll(hdc, &oParams);

    int k = 0;
    if (iDy < 0)
    {
        // Scroll Down -- place top line out of window.
        iDy = -iDy;
        const Posn buf_start = GetBuffer()->GetStart();
        for (k = 0; k < iDy; k++)
        {
            Posn start = m_pNewPage->GetStart();
            if (start == buf_start)
            {
                break;
            }

            // Scroll down until page start goes out from page.
            do
            {
                if (! m_pNewPage->ScrollDown(hdc))
                {
                    break;
                }

                #if DEBUG_SCROLL
                    DEBUG_PRINTF("scroll down view.end=%d start=%d\n",
                        Fixnum::Decode_(m_pNewPage->GetEnd()),
                        Fixnum::Decode_(start) );
                #endif

            } while (m_pNewPage->GetEnd() != start);
        } // for k
    }
    else if (iDy > 0)
    {
        const Posn buf_end = GetBuffer()->GetEnd();
        for (k = 0; k < iDy; k++)
        {
            Posn start = m_pNewPage->GetEnd();
            if (xge(start, buf_end))
            {
                break;
            }

            #if DEBUG_SCROLL
                DEBUG_PRINTF("scroll up start=%d\n",
                    Fixnum::Decode_(start) );
            #endif

            format(hdc, &oParams, start);
        } // for k
    } // if

    if (k > 0)
    {
        render(hdc);
    }

    return k;
} // TextWindow::LargeScroll

// [M]
Posn TextWindow::MapPointToPosn(POINT pt)
{
    Dc hdc(m_hwnd);
    return m_pCurPage->MapPointToPosn(hdc, pt);
} // TextWindow::MapPointToPosn

int TextWindow::MapPosnToPoint(Posn posn, POINT* out_pt)
{
    Dc hdc(m_hwnd);
    return m_pCurPage->MapPosnToPoint(hdc, posn, out_pt);
} // TextWindow::MapPosnToPoint

Posn TextWindow::mapXToPosn(int xGoal, Posn goal)
{
    if (xGoal < 0)
    {
        return goal;
    }

    FormatParams oParams;
    setupFormat(&oParams);
    Dc hdc(m_hwnd);

    if (m_pCurPage->IsClean(&oParams))
    {
        if (TextLine* pLine = m_pCurPage->FindLine(goal))
        {
            return pLine->MapXToPosn(hdc, xGoal);
        }
    } // if

    Posn start = GetBuffer()->ComputeStartOf(Kparagraph, goal);
    TextPage oPage(&oParams);
    for (;;)
    {
        TextLine* pLine = oPage.FormatLine(hdc, start);
        if (xlt(goal, pLine->GetEnd()))
        {
            return pLine->MapXToPosn(hdc, xGoal);

        }
        start = pLine->GetEnd();
    } // for
} // TextWindow::mapXToPosn

// [O]
/// <summary>
///   Intialize TextWindow object with HDC.
/// </summary>
int TextWindow::onCreate(CREATESTRUCT* pCreate)
{
    FormatParams oParams;
    setupFormat(&oParams, false);
    Dc hdc(m_hwnd);
    m_pCurPage = TextPage::Create(hdc, &oParams, zero, zero);

    if (pCreate->style & WS_VSCROLL)
    {
        m_oVertScrollBar.Set(m_hwnd, SB_VERT);
    }

    push(GetObject(), GetBuffer()->m_windows);
    return Super::onCreate(pCreate);
} //TextWindow::onCreate

/// <summary>
///   Unrealizes corresponding window object on destroy.
/// </summary>
void TextWindow::onDestroy()
{
    unrealize_instance(GetObject());
    Super::onDestroy();
} // TextWindow::onDestroy

/// <summary>
///   Called when editor is idle.
/// </summary>
bool TextWindow::OnIdle(uint nCount)
{
    #if DEBUG_IDLE
        DEBUG_PRINTF("%d\n", nCount);
    #endif

    bool fColor = true;
    bool fMore  = false;

    // Call buffer onIdle
    if (0 == nCount && m_fHasFocus)
    {
        // When associate file is obsolete, we don't color buffer.
        fColor = checkObsolete();
    }

    if (fColor)
    {
        fMore = GetBuffer()->OnIdle(nCount);
    }

    redraw();
    return fMore;
} // TextWindow::OnIdle

/// <summary>
///   Handles Windows window message.
/// </summary>
LRESULT TextWindow::onMessage(
    uint    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case WM_CHAR:
    {
        m_oBlink.Stop();

        // Note: Ctrl+<key> is handled by WM_KEYDOWN
        char16 wch = static_cast<char16>(wParam);
        if (wch >= 0x20)
        {
            Val command = Character::FromCode(wch);
            fireEvent(command);

            if (HIWORD(lParam) & KF_REPEAT)
            {
                fireEvent(command);
            }
        } // if graphic
        return 0;
    } // WM_CHAR

    case WM_GETTEXT:
    {
        SimpleString* pName = GetBuffer()->m_name->StaticCast<SimpleString>();
        int cwchName = pName->GetLength();
        if (GetBuffer()->IsModified())
        {
            cwchName += 1;
        }

        char16* pwsz = reinterpret_cast<char16*>(lParam);
        int cwch = static_cast<int>(wParam) - 1;

        cwch = min(cwchName, cwch);
        if (cwch <= 0)
        {
            return cwch;
        }

        ::CopyMemory(pwsz, pName->GetStart(), sizeof(char16) * cwch);

        if (GetBuffer()->IsModified())
        {
            if (cwchName < cwch)
            {
                pwsz[cwchName - 1] = '*';
            }
        } // if

        pwsz[cwchName] = 0;
        return cwchName;
    } // WM_GETTEXT

    case WM_GETTEXTLENGTH:
    {
        SimpleString* pName = GetBuffer()->m_name->StaticCast<SimpleString>();
        int cwchName = pName->GetLength();
        if (GetBuffer()->IsModified())
        {
            cwchName += 1;
        }
        return cwchName;
    } // WM_GETTEXTLENGTH

    case WM_KEYDOWN:
    {
        m_oBlink.Stop();

        uint nVKey = static_cast<uint>(wParam);
        if (VK_PROCESSKEY == nVKey)
        {
            // IME processed key down.
            break;
        }

        Val command = nil;

        if (uint nChar = IsGraphicKey(nVKey))
        {
            // Graph Key
            if (::GetKeyState(VK_CONTROL) >= 0)
            {
                break;
            }

            nChar |= Modifier_Control;
            if (::GetKeyState(VK_SHIFT) < 0)
            {
                nChar |= Modifier_Shift;
            }

            command = Fixnum::Encode(nChar);
        }
        else if (VK_CONTROL != nVKey &&
                 VK_SHIFT   != nVKey )
        {
            uint nKey = nVKey | 0x100;

            if (::GetKeyState(VK_CONTROL) < 0)
            {
                nKey |= Modifier_Control;
            }

            if (::GetKeyState(VK_SHIFT) < 0)
            {
                nKey |= Modifier_Shift;
            }

            command = Fixnum::Encode(nKey);
        } // if

        if (nil != command)
        {
            fireEvent(command);

            // FIXME yosi@msn.com 2008-06-19 We should pass numeric argument
            // to command instead of calling command twice.

            // IDEA yosi@msn.com 2008-06-19 Time based accelaration of
            // repeated key down. We should not call GetTickCount(?).
            if (HIWORD(lParam) & KF_REPEAT)
            {
                fireEvent(command);
            }

            return 0;
        } // if command

        break;
    } // WM_KEYDOWN

    case WM_KILLFOCUS:
        #if DEBUG_FOCUS
            DEBUG_FORMAT("WM_KILLFOCUS ~S~%", GetSelection()->m_buffer);
        #endif
        m_fHasFocus = false;
        g_oCaret.Destroy();
        break;

    case WM_LBUTTONDBLCLK:
    {
        Point pt(MAKEPOINTS(lParam));
        Posn posn = MapPointToPosn(pt);
        selectWord(posn);
        return 0;
    } // WM_LBUTTONDBLCLK

    case WM_LBUTTONDOWN:
    {
        Point pt(MAKEPOINTS(lParam));
        Posn posn = MapPointToPosn(pt);
        if (xlt(posn, 0))
        {
            // Click outside window. We do nothing.
            break;
        }

        Selection* pSelection = GetSelection();

        if (! m_fHasFocus)
        {
            ::SetFocus(m_hwnd);
            if (xge(posn, pSelection->m_start) &&
                xlt(posn, pSelection->m_end) )
            {
                // Click in selection
                return 0;
            }
        } // if not focus

        pSelection->MoveTo(posn, 0 != (wParam & MK_SHIFT) ? t : nil);

        if (wParam & MK_CONTROL)
        {
            selectWord(posn);
        }
        else
        {
            m_oDrag.Start(m_hwnd);
        }
        return 0;
    } // WM_LBUTTONDOWN

    case WM_LBUTTONUP:
        m_oAutoScroll.Stop();
        m_oDrag.Stop();
        return 0;

    case WM_MOUSEMOVE:
    {
        if (! m_oDrag.IsDragging(m_hwnd))
        {
            m_oAutoScroll.Stop();
            m_oDrag.Stop();
            return 0;
        } // if

        Point pt(MAKEPOINTS(lParam));
        Posn posn = MapPointToPosn(pt);
        if (xge(posn, zero))
        {
            GetSelection()->MoveTo(posn, t);
        }

        if (pt.y < m_rc.top)
        {
            #if DEBUG_AUTOSCROLL
                DEBUG_PRINTF("WM_MOUSEMOVE AutoScroll down %d\n",
                    m_oAutoScroll.IsActive() );
            #endif
            m_oAutoScroll.Start(m_hwnd, -1);
        }
        else if (pt.y >= m_rc.bottom)
        {
            #if DEBUG_AUTOSCROLL
                DEBUG_PRINTF("WM_MOUSEMOVE AutoScroll up %d\n",
                    m_oAutoScroll.IsActive() );
            #endif
            m_oAutoScroll.Start(m_hwnd, 1);
        }
        else
        {
            #if DEBUG_AUTOSCROLL
                DEBUG_PRINTF("WM_MOUSEMOVE AutoScroll stop\n");
            #endif
            m_oAutoScroll.Stop();
        }

        return 0;
    } // WM_MOUSEMOVE

    case WM_MOUSEWHEEL:
        if (GET_WHEEL_DELTA_WPARAM(wParam) > 0)
        {
            SmallScroll(0, -2);
        }
        else
        {
            SmallScroll(0, 2);
        }
        return 0;

    case WM_PAINT:
    {
        // For RedrawWindow with the RDW_INTERNALPAINT
        if (::GetUpdateRect(m_hwnd, NULL, false))
        {
            PAINTSTRUCT ps;
            HDC hdc = ::BeginPaint(m_hwnd, &ps);
            m_pCurPage->Render(hdc, &ps.rcPaint);
            ::EndPaint(m_hwnd, &ps);
        } // if
        return 0;
    } // WM_PAINT

    case WM_SETCURSOR:
        if (HTCLIENT == LOWORD(lParam))
        {
            if (NULL == s_hIBeam)
            {
                s_hIBeam = ::LoadCursor(NULL, MAKEINTRESOURCE(IDC_IBEAM));
            }
            ::SetCursor(s_hIBeam);
            return 1;
        }
        break;

    case WM_SIZE:
    {
        ::GetClientRect(*this, &m_rc);
        return 0;
    } // WM_SIZE

    case WM_VSCROLL:
    {
        switch (LOWORD(wParam))
        {
        case SB_ENDSCROLL:  // 8
            break;

        case SB_LINEDOWN:   // 1
            SmallScroll(0, 1);
            break;

        case SB_LINEUP:     // 0
            SmallScroll(0, -1);
            break;

        case SB_PAGEDOWN:   // 3
            LargeScroll(0, 1);
            break;

        case SB_PAGEUP:   // 2
            LargeScroll(0, -1);
            break;

        case SB_THUMBPOSITION:  // 4
            break;

        case SB_THUMBTRACK:     // 5
        {
            SCROLLINFO oInfo;
            oInfo.cbSize = sizeof(oInfo);
            oInfo.fMask  = SIF_ALL;
            if (m_oVertScrollBar.GetInfo(&oInfo))
            {
                DEBUG_PRINTF(
                    "si.nTrackPos=%d "
                    "si.nPage=%d "
                    "si.nMax=%d\n",
                    oInfo.nTrackPos, oInfo.nPage, oInfo.nMax );

                Dc hdc(m_hwnd);
                FormatParams oParams;
                setupFormat(&oParams);

                Posn start = GetBuffer()->ComputeStartOf(
                    Kparagraph,
                    Fixnum::Encode(oInfo.nTrackPos) );

                format(hdc, &oParams, start);
                render(hdc);
            }
            else
            {
                #if _DEBUG
                    __debugbreak();
                #endif
                DEBUG_PRINTF("GetScrollInfo failed\n");
            } // if
            break;
        } // SB_THUMBTRACK
        } // switch code
        return 0;
    } // WM_VSCROLL

    case WM_WINDOWPOSCHANGED:
    {
        // DefWindowProc sents WM_SIZE and WM_MOVE, so handling
        // WM_WINDPOSCHANGED is faster than DefWindowProc.
        const WINDOWPOS* wp = reinterpret_cast<WINDOWPOS*>(lParam);
        if (wp->flags & SWP_NOSIZE)
        {
            return 0;
        }

        #if DEBUG_REDRAW || DEBUG_RESIZE
            DEBUG_PRINTF("WM_WINDOWPOSCHANGED %p"
                " 0x%X %dx%d+%d+%d\n",
                this,
                wp->flags,
                wp->cx, wp->cy, wp->x, wp->y );
        #endif

        // Redraw text page for resizing
        ::GetClientRect(m_hwnd, &m_rc);

        redraw();
        return 0;
    } // WM_WINDOWPOSCHANGED

    #if SUPPORT_IME
    case WM_IME_COMPOSITION:
        onImeComposition(lParam);
        return 0;

    case WM_IME_ENDCOMPOSITION:
        m_fImeTarget = false;
        return 0;

    case WM_IME_REQUEST:
        if (IMR_RECONVERTSTRING == wParam)
        {
            uint cb = setReconvert(
                reinterpret_cast<RECONVERTSTRING*>(lParam),
                GetSelection()->GetStart(),
                GetSelection()->GetEnd() );
            return cb;
        }
        break;

    case WM_IME_SETCONTEXT:
        // We draw composition string instead of IME. So, we don't
        // need default composition window.
        lParam &= ~ISC_SHOWUICOMPOSITIONWINDOW;
        break;

    case WM_IME_STARTCOMPOSITION:
        if (! m_fImeTarget)
        {
            m_ime_start  = GetSelection()->GetStart();
            m_ime_end    = m_ime_start;
            m_fImeTarget = false;
        }
        return 0;
    #endif // SUPPORT_IME
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // TextWindow::onMessage

void TextWindow::onSetFocus()
{
    #if DEBUG_FOCUS
        DEBUG_FORMAT("WM_SETFOCUS ~S~%", GetSelection()->m_buffer);
    #endif
    m_fHasFocus = true;

    // Update file state
    GetBuffer()->QueryFileState(true);
} // TextWindow::onSetFocus

// [P]
void TextWindow::prepareScroll(HDC hdc, FormatParams* pParams)
{
    ASSERT(NULL != hdc);
    ASSERT(NULL != pParams);

    Posn page_start = m_pCurPage->GetStart();
    if (m_pCurPage->IsClean(pParams))
    {
        #if DEBUG_SCROLL
            DEBUG_PRINTF("cur page is clean.\n");
        #endif
        format(hdc, pParams, page_start);
    }
    else
    {
        Val line_start = GetBuffer()->ComputeStartOf(Kparagraph, page_start);
        #if DEBUG_SCROLL
            DEBUG_PRINTF("cur page is dirty. page=%d line=%d\n",
                Fixnum::Decode_(page_start),
                Fixnum::Decode_(line_start) );
        #endif
        format(hdc, pParams, line_start, page_start);
    } // if
} // TextWindow::prepareScroll

// [R]
void TextWindow::redraw()
{
    Dc hdc(m_hwnd);

    Posn caret_posn;
    Posn sel_start;
    Posn sel_end;

    bool fSelectionIsActive = false;

    if (m_oBlink.IsActive())
    {
        Range* pBlink = getPeer()->m_blink->StaticCast<Range>();
        sel_start  = pBlink->m_start;
        sel_end    = pBlink->m_end;
        caret_posn = sel_start;

        #if DEBUG_BLINK
            DEBUG_FORMAT("Blink [~D,~D]~%", sel_start, sel_end);
        #endif
    }
    else
    {
        Selection* pSelection = GetSelection();

        sel_start = pSelection->m_start;
        sel_end   = pSelection->m_end;

        fSelectionIsActive = isSelectionActive();

        if (fSelectionIsActive)
        {
            caret_posn = pSelection->GetActivePosn();
        }
        else
        {
            caret_posn = m_caret_posn;
            Posn end = GetBuffer()->m_length;
            if (sel_start == end && sel_end == end)
            {
                caret_posn = end;
            }
        }
    } // if

    FormatParams oParams;
    setupFormat(&oParams, fSelectionIsActive);

    Posn start = getPeer()->m_range->StaticCast<Range>()->m_start;

    if (NULL != m_pNewPage)
    {
        #if DEBUG_REDRAW || DEBUG_SCROLL
            DEBUG_FORMAT("New page [~D, ~D]. m_caret=~D caret=~D~%",
                m_pNewPage->GetStart(), m_pNewPage->GetEnd(),
                m_caret_posn, caret_posn );
        #endif
        m_caret_posn = caret_posn;
    }
    else if (! m_pCurPage->IsClean(&oParams))
    {
        #if DEBUG_REDRAW
            DEBUG_FORMAT("page is dirty. start=~D, m_caret=~D caret=~D~%",
                start, m_caret_posn, caret_posn );
        #endif

        start = GetBuffer()->ComputeStartOf(Kparagraph, start);
        format(hdc, &oParams, start, caret_posn);
        m_caret_posn = caret_posn;
    }
    else if (m_caret_posn != caret_posn)
    {
        m_caret_posn = caret_posn;

        if (m_pCurPage->IsPosnFullyVisible(caret_posn))
        {
            #if DEBUG_REDRAW
                DEBUG_FORMAT("Caret is in page. cur=[~D, ~D] caret=~D.~%",
                    m_pCurPage->GetStart(),
                    m_pCurPage->GetEnd(),
                    caret_posn );
            #endif

            updateScrollBar();
            updateCaret(hdc);
            return;
        } // if

        #if DEBUG_REDRAW
            DEBUG_FORMAT("Caret is moved. cur=[~D, ~D] caret=~D.~%",
                m_pCurPage->GetStart(),
                m_pCurPage->GetEnd(),
                caret_posn );
        #endif

        // Note: We can't use ScrollWindow. However, this is faster than
        // rendering long line.
        m_pCurPage->ScrollToPosn(hdc, caret_posn);
    }
    else if (m_pCurPage->GetStart() != start)
    {
        #if DEBUG_REDRAW
            DEBUG_FORMAT("Start is changed. cur.start=~D start=~D.~%",
                m_pCurPage->GetStart(), start );
        #endif

        format(hdc, &oParams, start);
    }
    else
    {
        updateScrollBar();
        updateCaret(hdc);
        return;
    } // if

    render(hdc);
} // TextWindow::redraw

/// <summary>
///  Remember page start position.
/// </summary>
void TextWindow::rememberPageStart(Posn start)
{
    getPeer()->m_range->StaticCast<Range>()->SetRange(start, start);
} // rememberPageStart

/// <summary>
///  Render window contents.
/// </summary>
/// <param name="hdc">HDC</param>
void TextWindow::render(HDC hdc)
{
    if (m_fHasFocus)
    {
        g_oCaret.Hide();
    }

    if (NULL == m_pNewPage)
    {
        // FIXME 2008-02-10 We should utilize current drawing.
        m_pCurPage->Render(hdc);
    }
    else
    {
        if (NULL == m_pCurPage)
        {
            m_pNewPage->Render(hdc);
        }
        else
        {
            m_pNewPage->Render(hdc, m_hwnd, m_pCurPage);
            m_pCurPage->Destroy();
        }

        m_pCurPage = m_pNewPage;
        m_pNewPage = NULL;
    } // if

    rememberPageStart(m_pCurPage->GetStart());

    updateScrollBar();
    updateCaret(hdc);
} // TextWindow::render

// [S]

// For left button double click and left button down with control key.
void TextWindow::selectWord(Posn posn)
{
    Selection* pSelection = GetSelection();
    pSelection->SetStart(posn);
    pSelection->StartOf(Kword);
    pSelection->EndOf(Kword, Kextend);
    pSelection->SetActive(Kend);
} // TextWindow::selectWord

void TextWindow::SetScrollBar(HWND hwnd, int iBar)
{
    switch (iBar)
    {
    case SB_VERT:
        m_oVertScrollBar.Set(hwnd, hwnd == *this ? SB_VERT : SB_CTL);
        break;
    } // switch iBar
} // TextWindow::SetScrollBar

/// <summary>
///   Initialize format paramter for page rendering.
/// </summary>
void TextWindow::setupFormat(FormatParams* p, bool fSelectionIsActive)
{
    Editor::TextWindow* pTextWindow = GetObject()->
        StaticCast<Editor::TextWindow>();

    Selection* pSelection = pTextWindow->m_selection->
        StaticCast<Selection>();

    Buffer* pBuffer = pSelection->m_buffer->StaticCast<Buffer>();

    {
        Val intv = pBuffer->GetIntervalAt(pBuffer->GetEnd());
        Interval* pIntv = intv->StaticCast<Interval>();

        if (fixnump(pIntv->m_background))
        {
            p->m_crBackground = Fixnum::Decode_(pIntv->m_background);
        }
        else
        {
            p->m_crBackground = Fixnum::Decode_(
                VAR(AstyleA)->StaticCast<Style>()->m_background );
        }
    }

    p->m_cwchTab      = 4;
    p->m_cxLeftMargin = 7;
    p->m_pBuffer      = pBuffer;
    p->m_rc           = m_rc;

    p->m_tick = pBuffer->m_tick;

    p->m_selection_start = pSelection->m_start;
    p->m_selection_end   = pSelection->m_end;

    p->m_oSelectionStyle.FromStyle(
        fSelectionIsActive ?
            VAR(Aactive_selection_styleA) :
            VAR(Ainactive_selection_styleA) );
} // TextWindow::setupFormat

void TextWindow::setupFormat(FormatParams* p)
    { setupFormat(p, isSelectionActive()); }

int TextWindow::SmallScroll(int, int iDy)
{
    Dc hdc(m_hwnd);
    FormatParams oParams;
    setupFormat(&oParams);
    prepareScroll(hdc, &oParams);

    int k = 0;

    if (iDy < 0)
    {
        iDy = -iDy;

        for (k = 0; k < iDy; k++)
        {
            if (! m_pNewPage->ScrollDown(hdc))
            {
                break;
            }
        } // for k
    }
    else
    {
        const Posn buf_end = GetBuffer()->GetEnd();
        for (k = 0; k < iDy; k++)
        {
            if (m_pNewPage->GetEnd() >= buf_end)
            {
                // Make sure whole line of buffer end is visible.
                m_pNewPage->ScrollToPosn(hdc, buf_end);
                k += 1;
                break;
            }

            if (! m_pNewPage->ScrollUp(hdc))
            {
                break;
            }
        } // for k
    } // if

    #if DEBUG_SCROLL
        DEBUG_PRINTF("k=%d page.start=%d\n",
            k, Fixnum::Decode_(m_pNewPage->GetStart()) );
    #endif

    if (k > 0)
    {
        render(hdc);
    }

    return k;
} // TextWindow::SmallScroll

Posn TextWindow::StartOfLine(Posn posn)
{
    if (xle(posn, zero)) return zero;

    FormatParams oParams;
    setupFormat(&oParams);

    if (m_pCurPage->IsClean(&oParams))
    {
        TextLine* pLine = m_pCurPage->FindLine(posn);
        if (NULL != pLine)
        {
            return pLine->GetStart();
        }
    } // if

    Posn start = GetBuffer()->ComputeStartOf(Kparagraph, posn);
    if (zero == posn)
    {
        return zero;
    }

    TextPage oPage(&oParams);
    Dc hdc(m_hwnd);
    for (;;)
    {
        TextLine* pLine = oPage.FormatLine(hdc, start);
        Posn end = pLine->GetEnd();
        if (xlt(posn, end))
        {
            return pLine->GetStart();
        }
        start = end;
    } // for
} // TextWindow::StartOfLine

// [U]

void TextWindow::updateCaret(HDC hdc)
{
    if (! m_fHasFocus)
    {
        return;
    }

    POINT pt;
    SIZE size;
    size.cy = m_pCurPage->MapPosnToPoint(hdc, m_caret_posn, &pt);

    #if DEBUG_CARET
        DEBUG_FORMAT("[~D, ~D] caret=~D caret.cy=~D\n",
            m_pCurPage->GetStart(), m_pCurPage->GetEnd(),
            m_caret_posn,
            Fixnum::Encode(size.cy) );
    #endif

    if (size.cy <= 0)
    {
        g_oCaret.Destroy();
    }
    else
    {
        size.cx = max(::GetSystemMetrics(SM_CXBORDER), 2);

        #if SUPPORT_IME
        {
            if (m_fImeTarget)
            {
                if (showImeCaret(size, pt)) return;
                DEBUG_PRINTF("showImeCaret failed.\r\n");
            } // if
        }
        #endif // SUPPORT_IME

        g_oCaret.Show(m_hwnd, size, pt);
    } // if
} // TextWindow::updateCaret

void TextWindow::updateScrollBar()
{
    Buffer* pBuffer = GetBuffer();
    Posn buf_end = xadd(pBuffer->m_length, 1);

    SCROLLINFO oInfo;
    oInfo.cbSize = sizeof(oInfo);
    oInfo.fMask = SIF_POS | SIF_RANGE | SIF_PAGE | SIF_DISABLENOSCROLL;

    oInfo.nPage = Fixnum::Decode_(
        xsub(m_pCurPage->GetEnd(), m_pCurPage->GetStart()) );

    oInfo.nMin  = 0;
    oInfo.nMax  = Fixnum::Decode_(buf_end);

    oInfo.nPos  = Fixnum::Decode_(m_pCurPage->GetStart());

    if (xge(Fixnum::Encode(oInfo.nPage), buf_end))
    {
        // Current screen shows entire buffer. We disable scroll bar.
        oInfo.nMax  = 0;

        #if DEBUG_SCROLLBAR
            DEBUG_PRINTF("Disable scrollbar for %p %ls\n",
                this, pBuffer->GetName() );
        #endif
    } // if

    m_oVertScrollBar.SetInfo(&oInfo, true);
} // TextWindow::updateScrollBar

#if SUPPORT_IME

// See Also Caret::Show for moving candidate window.

#include <imm.h>
#pragma comment(lib, "imm32.lib")

#define GCS_COMPSTRATTR (GCS_COMPSTR | GCS_COMPATTR | GCS_CURSORPOS)

class Imc
{
    private: HWND m_hwnd;
    private: HIMC m_himc;

    public: Imc(HWND hwnd) :
        m_hwnd(hwnd),
        m_himc(::ImmGetContext(hwnd)) {}

    public: ~Imc()
    {
        if (NULL != m_himc) ::ImmReleaseContext(m_hwnd, m_himc);
    } // ~Imc

    public: operator HIMC() const { return m_himc; }
}; // Imc

void TextWindow::onImeComposition(LPARAM lParam)
{
    Imc imc(m_hwnd);
    if (imc == NULL) return;

    Buffer::UndoBlock oUndo(GetSelection(), Qime);

    char16 rgwch[1024];
    // If IME has result string, we can insert it into buffer.
    if (lParam & GCS_RESULTSTR)
    {
        // Remove previous composition string. If user inputs "Space",
        // IME set GCS_RESULTSTR without composition.
        if (m_ime_start != m_ime_end)
        {
            GetSelection()->SetRange(m_ime_start, m_ime_end);
            GetSelection()->SetText();
        } // if

        // Get result string
        long cwch = ::ImmGetCompositionString(
            imc,
            GCS_RESULTSTR,
            rgwch,
            sizeof(rgwch) ) / sizeof(char16);

        // Insert result string into buffer
        if (cwch >= 1)
        {
            GetSelection()->SetText(rgwch, cwch);
            GetSelection()->Collapse(Kend);
            m_ime_end = GetSelection()->GetEnd();

            m_ime_start = m_ime_end;
        }
    } // if GC_RESULTSTR

    // IME has composition string
    if ((lParam & GCS_COMPSTRATTR) == GCS_COMPSTRATTR)
    {
        // Remove previous composition string
        if (m_ime_start != m_ime_end)
        {
            GetSelection()->SetRange(m_ime_start, m_ime_end);
            GetSelection()->SetText();
            m_ime_end = m_ime_start;
        } // if

        // Get composition string
        long cwch = ::ImmGetCompositionString(
            imc,
            GCS_COMPSTR,
            rgwch,
            sizeof(rgwch) ) / sizeof(char16);

        // Get composition attributes
        char rgbAttr[lengthof(rgwch)];
        long cbAttr = ::ImmGetCompositionString(
            imc,
            GCS_COMPATTR,
            rgbAttr,
            sizeof(rgbAttr) );
        if (cbAttr != cwch)
        {
            DEBUG_PRINTF("GCCS_COMPATTR\n");
            return;
        } // if

        long lCursor = ::ImmGetCompositionString(
            imc,
            GCS_CURSORPOS,
            NULL,
            0 );
        if (lCursor < 0)
        {
            DEBUG_PRINTF("GCCS_CURSORPOS\n");
            return;
        } // if

        uint32 rgnClause[100];
        ::ImmGetCompositionString(
            imc,
            GCS_COMPCLAUSE,
            rgnClause,
            sizeof(rgnClause) );

        GetSelection()->SetText(rgwch, cwch);
        GetSelection()->Collapse(Kend);
        m_ime_end = GetSelection()->GetEnd();

        GetSelection()->SetRange(
            xxadd(m_ime_start, lCursor),
            xxadd(m_ime_start, lCursor) );

        m_fImeTarget = false;
        Posn lEnd  = xxadd(m_ime_start, cwch);
        Posn posn = m_ime_start;
        int iClause = 0;
        int iConverted = 0;
        while (posn < lEnd)
        {
            iClause += 1;
            Posn next = xxadd(m_ime_start,  rgnClause[iClause]);

            Int iIndex = Fixnum::Decode_(xxsub(posn, m_ime_start));

            #if DEBUG_IME
                DEBUG_FORMAT("[~D, ~D] ~D\n",
                    posn, next,
                    Fixnum::Encode(rgbAttr[iIndex]) );
            #endif

            switch (rgbAttr[iIndex])
            {
            case ATTR_CONVERTED:    // 2
                // Note: We use ime_inactive0 and ime_inactive1 for separating
                // each sentence.
                GetBuffer()->SetStyle(
                    posn,
                    next,
                    iConverted ? Kime_inactive1 : Kime_inactive0 );

                iConverted = 1 - iConverted;
                break;

            case ATTR_INPUT:            // 0
                GetBuffer()->SetStyle(posn, next, Kime_input);
                iConverted = 0;
                break;

            case ATTR_TARGET_CONVERTED: // 1
                GetBuffer()->SetStyle(posn, next, Kime_active);
                m_fImeTarget = true;
                iConverted = 0;
                break;

            case ATTR_TARGET_NOTCONVERTED:  // 3
            {
                GetBuffer()->SetStyle(posn, next, Kime_not_converted);
                m_fImeTarget = true;
                iConverted = 0;
                break;
            } // ATTR_TARGET_NOTCONVERTED

            case ATTR_INPUT_ERROR:      // 4
            case ATTR_FIXEDCONVERTED:   // 5
            default:
                GetBuffer()->SetStyle(posn, next, Kime);
                break;
            } // switch attr

            posn = next;
        } // for posn
    } // if GCS_COMPSTRATTR

    ////////////////////////////////////////////////////////////
    //
    // We have already insert composed string. So, we don't
    // need WM_IME_CHAR and WM_CHAR messages to insert
    // composed string.
    if (lParam & GCS_RESULTSTR)
    {
        m_fImeTarget = false;
        return;
    } // if

    // Composition was canceled.
    if (0 == lParam)
    {
        m_fImeTarget = false;

        // Remove previous composition string
        GetSelection()->SetRange(m_ime_start, m_ime_end);
        GetSelection()->SetText();

        // if (m_fCancelButLeave)
        {
            long cwch = ::ImmGetCompositionString(
                imc,
                GCS_COMPSTR,
                rgwch,
                sizeof(rgwch) ) / sizeof(char16);
            if (cwch >= 1)
            {
                GetSelection()->SetText(rgwch, cwch);
            }
        }

        m_ime_end = m_ime_start;
    } // if
} // TextWindow::onImeComposition

//////////////////////////////////////////////////////////////////////
//
// TextWindow::Reconvert
//
// Note:
//  o IME2000 ignores string after newline.
//  o We should limit number of characters to be reconverted.
//
void TextWindow::Reconvert(Posn start, Posn end)
{
    BOOL fSucceeded;

    uint cb = setReconvert(NULL, start, end);
    if (0 == cb) return;

    char* pb = new char[cb];
    if (NULL == pb) return;

    RECONVERTSTRING* p = reinterpret_cast<RECONVERTSTRING*>(pb);

    setReconvert(p, start, end);

    Imc imc(m_hwnd);

    fSucceeded = ::ImmSetCompositionString(
        imc,
        SCS_QUERYRECONVERTSTRING,
        p,
        cb,
        NULL,
        0 );
    unless (fSucceeded)
    {
        DEBUG_PRINTF("SCS_QUERYRECONVERTSTRING\n");
        goto exit;
    }

    m_ime_start  = xxadd(start, p->dwCompStrOffset / 2);
    m_ime_end    = xxadd(m_ime_start, p->dwCompStrLen);
    m_fImeTarget = true;

    fSucceeded = ::ImmSetCompositionString(
        imc,
        SCS_SETRECONVERTSTRING,
        p,
        cb,
        NULL,
        0 );
    unless (fSucceeded)
    {
        DEBUG_PRINTF("SCS_SETRECONVERTSTRING\n");
        goto exit;
    }

  exit:
    delete[] pb;
} // TextWindow::Reconvert

uint TextWindow::setReconvert(RECONVERTSTRING* p, Posn start, Posn end)
{
    Int cwch = Fixnum::Decode_(xxsub(end, start));
    when (0 == cwch) return 0;

    uint cb = sizeof(RECONVERTSTRING) + sizeof(char16) * (cwch + 1);

    if (NULL == p) return cb;

    p->dwSize            = cb;
    p->dwVersion         = 0;
    p->dwStrLen          = cwch;
    p->dwStrOffset       = sizeof(RECONVERTSTRING);
    p->dwCompStrLen      = cwch;    // # of characters
    p->dwCompStrOffset   = 0;       // byte offset
    p->dwTargetStrLen    = p->dwCompStrLen;
    p->dwTargetStrOffset = p->dwCompStrOffset;

    char16* pwch = reinterpret_cast<char16*>(
        reinterpret_cast<char*>(p) + p->dwStrOffset );

    GetBuffer()->GetText(start, end, pwch);

    pwch[cwch] = 0;

    return cb;
} // TextWindow::Reconvert

//  Set left top coordinate of IME candiate window.
BOOL TextWindow::showImeCaret(SIZE sz, POINT pt)
{
    Imc imc(m_hwnd);
    if (imc == NULL) return FALSE;

    CANDIDATEFORM oCF;
    oCF.dwIndex         = 0;
    oCF.dwStyle         = CFS_EXCLUDE;
    oCF.ptCurrentPos.x  = pt.x;
    oCF.ptCurrentPos.y  = pt.y + sz.cy;

    oCF.rcArea.left     = pt.x;
    oCF.rcArea.top      = pt.y;
    oCF.rcArea.right    = pt.x;
    oCF.rcArea.bottom   = pt.y + sz.cy;

    return ::ImmSetCandidateWindow(imc, &oCF);
} // TextWindow::showImeCaret

#endif // SUPPORT_IME

} // Peer
} // Editor
