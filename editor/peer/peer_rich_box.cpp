#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Rich View
// editor/peer_rich_view.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_box.cpp#2 $
//
#define DEBUG_FORMATTER 1
#include "./peer_rich_view.h"

#include "./peer_rich_box.h"
#include "./peer_rich_node.h"

namespace Editor
{

namespace Peer
{

namespace RichView
{

namespace Render
{

// [B]
void Box::drawBorder() const
{
    // FIXME 2007-12-31 yosi@msn.com NYI Box::drawBorder
} // Box::drawBorder

RootBox* Box::getRoot() const
{
    Box* pRunner = const_cast<Box*>(this);
    for (;;)
    {
        pRunner = pRunner->GetParent();
        if (RootBox* pRoot = pRunner->DynamicCast<RootBox>())
        {
            return pRoot;
        }
    } // for
} // Box::getRoot

void Button::Realize(HWND const hwndParent)
{
    createWindow(hwndParent, L"BUTTON", m_dwStyle, m_pText->GetString());
} // Button::Realize

void Button::SetDefault(bool fDefault)
{
    SetDefault(m_hwnd, fDefault);
} // Button::SetDefault

void Button::SetDefault(HWND hwnd, bool fDefault)
{
    uint nStyle = ::GetWindowLong(hwnd, GWL_STYLE) & ~BS_DEFPUSHBUTTON;
    if (fDefault)
    {
        nStyle |= BS_DEFPUSHBUTTON;
    }

    ::SendMessage(hwnd, BM_SETSTYLE, nStyle, true);
} // Button::SetDefault

// [C]
void ChildWindow::createWindow(
    HWND            const hwndParent,
    const char16*   const pwszClass,
    DWORD           const dwStyle,
    const char16*   const pwszTitle )
{
    ASSERT(NULL == m_hwnd);

    RootBox* pRoot = getRoot();

    Rect rc(m_rc);
    offsetBox(&rc);

    uint const nId = m_pNode->StaticCast<Markup::Control>()->GetCtrlId();

    m_hwnd = ::CreateWindowExW(
        0,
        pwszClass,
        pwszTitle,
        dwStyle | WS_CHILD | WS_TABSTOP | WS_VISIBLE,
        rc.left,
        rc.top,
        rc.GetWidth(),
        rc.GetHeight(),
        hwndParent,
        reinterpret_cast<HMENU>(nId),
        g_hInstance,
        reinterpret_cast<void*>(const_cast<ChildWindow*>(this)) );

    if (NULL == m_hwnd)
    {
        PlatformError("CreateWindowEx");
    }

    pRoot->AddChildWindow(this);

    ::SendMessage(
        m_hwnd,
        WM_SETFONT, 
        reinterpret_cast<WPARAM>(m_pFont->GetHandle()),
        true );
} // ChildWindow::createWindow

void ContainerBox::Realize(HWND const hwndParent)
{
    foreach (Enum, oEnum, this)
    {
        oEnum.Get()->Realize(hwndParent);
    } // for
} // ContainerBox::Realize

// [H]
Box* HBox::AppendBox(Box* pBox)
{
    ASSERT(pBox->GetParent() == this);
    Boxes::Append(pBox);
    m_rc.left   = min(m_rc.left,   pBox->m_rc.left);
    m_rc.top    = min(m_rc.top,    pBox->m_rc.top);
    m_rc.right  = max(m_rc.right,  pBox->m_rc.right);
    m_rc.bottom = max(m_rc.bottom, pBox->m_rc.bottom);
    m_cyDescent = max(m_cyDescent, pBox->m_cyDescent);
    return pBox;
} // HBox::AppendBox

void HBox::Finalize()
{
    foreach (Boxes::Enum, oEnum, this)
    {
        Box* pBox = oEnum.Get();
        pBox->m_cyDescent = m_cyDescent;
    } // for each box

    #if DEBUG_FORMATTER
        DEBUG_PRINTF("%p (%d,%d)-(%d,%d)\n",
            this,
            m_rc.left, m_rc.top, m_rc.right, m_rc.bottom );
    #endif
} // HBox::Finalize

void HBox::Render(const Params* pParams) const
{
    drawBorder();

    Rect rc(m_rc);
    offsetBox(&rc);
    int xRight = rc.right;

    foreach (Boxes::Enum, oEnum, this)
    {
        Box* pBox = oEnum.Get();

        // Fill left margin
        {
            rc.right = pBox->m_rc.left + pBox->m_rcMargin.left;
            fillRect(pParams->m_hdc, &rc, m_crBackground);
        }

        // Fill top and bottom margins
        {
            Rect rcMargin(rc);
            rcMargin.right  = pBox->m_rc.right;
            rcMargin.bottom = pBox->m_rc.top + pBox->m_rcMargin.top;
            fillRect(pParams->m_hdc, &rcMargin, m_crBackground);

            rcMargin.top    = pBox->m_rc.bottom - pBox->m_rcMargin.bottom;
            rcMargin.bottom = rc.bottom;
            fillRect(pParams->m_hdc, &rcMargin, m_crBackground);
        }

        pBox->Render(pParams);

        rc.left = pBox->m_rc.right;
    } // for each box

    // Fill right margin
    rc.right = xRight;
    fillRect(pParams->m_hdc, &rc, m_crBackground);
} // HBox::Render

// [R]
void RootBox::Render(const Params* pParams) const
{
    ::SetTextAlign(pParams->m_hdc, TA_BASELINE | TA_NOUPDATECP);
    ::SelectObject(pParams->m_hdc, ::GetStockObject(DC_PEN));
    ::SelectObject(pParams->m_hdc, ::GetStockObject(DC_BRUSH));
    Base::Render(pParams);
} // RootBox::Render

// [T]
void Text::Render(const Params* pParams) const
{
    drawBorder();

    ::SetTextColor(pParams->m_hdc, m_crColor);
    ::SetBkColor(pParams->m_hdc, m_crBackground);
    DcSelect oSelect(pParams->m_hdc, *m_pFont);

    Rect rc(m_rc);
    offsetBox(&rc);

    // Note: We need to have ETO_CLIPPED. Some fonts need one more
    // pixel at left edge.
    ::ExtTextOutW(
        pParams->m_hdc,
        rc.left,
        rc.bottom - m_cyDescent,
        ETO_OPAQUE | ETO_CLIPPED,
        &rc,
        m_pNode->StaticCast<Markup::Text>()->GetString(),
        m_iEnd - m_iStart,
        NULL );
} // Text::Redner

void TextBox::Realize(HWND const hwndParent)
{
    createWindow(hwndParent,  L"EDIT", m_dwStyle, NULL);
} // TextBox::Realize

// [V]
Box* VBox::AppendBox(Box* pBox)
{
    ASSERT(pBox->GetParent() == this);

    if (0 == pBox->GetWidth() && 0 == pBox->GetHeight())
    {
        return pBox;
    }
    
    #if DEBUG_FORMATTER
        DEBUG_PRINTF("%p(%s) (%d,%d)-(%d,%d)\n",
            pBox, pBox->GetKind(),
            pBox->m_rc.left, pBox->m_rc.top, 
            pBox->m_rc.right, pBox->m_rc.bottom );
    #endif

    Boxes::Append(pBox);
    m_rc.left   = min(m_rc.left,   pBox->m_rc.left);
    m_rc.top    = min(m_rc.top,    pBox->m_rc.top);
    m_rc.right  = max(m_rc.right,  pBox->m_rc.right);
    m_rc.bottom = max(m_rc.bottom, pBox->m_rc.bottom);
    return pBox;
} // VBox::AppendBox

void VBox::Finalize()
{
    #if DEBUG_FORMAT
        DEBUG_PRINTF("%p (%d,%d)-(%d,%d)\n",
            this,
            m_rc.left, m_rc.top, m_rc.right, m_rc.bottom );
    #endif
} // VBox::Finalize

void VBox::Render(const Params* pParams) const
{
    drawBorder();

    Rect rc(m_rc);
    offsetBox(&rc);
    int yBottom = rc.bottom;

    foreach (Boxes::Enum, oEnum, this)
    {
        Box* pBox = oEnum.Get();

        // Fill top margin
        rc.bottom = pBox->m_rc.top;
        fillRect(pParams->m_hdc, &rc, m_crBackground);

        // Fill left and right margins
        {
            Rect rcMargin(rc);
            rcMargin.right  = pBox->m_rc.left + pBox->m_rcMargin.left;
            rcMargin.bottom = pBox->m_rc.bottom;
            fillRect(pParams->m_hdc, &rcMargin, m_crBackground);

            rcMargin.left  = pBox->m_rc.right - pBox->m_rcMargin.right;
            rcMargin.right = rc.right;
            fillRect(pParams->m_hdc, &rcMargin, m_crBackground);
        }

        pBox->Render(pParams);
        rc.top = pBox->m_rc.bottom;
    } // for each box

    // Fill bottom margin
    rc.bottom = yBottom;
    fillRect(pParams->m_hdc, &rc, m_crBackground);
} // VBox::Render

} // Render
} // RichView
} // Peer
} // Editor
