#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Rich View
// editor/peer_rich_view.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_node.cpp#2 $
//
#define DEBUG_FORMATTER 0
#define DEBUG_MOTION    0
#define DEBUG_RENDER    0
#include "./peer_rich_view.h"

#include "./peer_rich_box.h"
#include "./peer_rich_node.h"

namespace Editor
{

namespace Peer
{

namespace RichView
{

namespace Formatter
{

using namespace Render;

Params::Params(Val style) :
    m_sizeMax(500, 300),
    m_sizeMin(100, 100)
{
    m_oStyle.FromStyle(style);
} // Params::Params

class Context
{
    public: int                     m_cyDescent;
    public: HDC                     m_hdc;
    public: HANDLE                  m_hHeap;
    public: RenderStyle             m_oStyle;
    public: ContainerBox*           m_pBox;
    public: const Markup::Node*     m_pNode;
    public: const Params*           m_pParams;
    public: Point                   m_pt;
    public: Rect                    m_rcView;
    public: Rect                    m_rc;

    public: Context(
        HDC                     hdc,
        HANDLE                  hHeap,
        const Markup::Node*     pNode,
        ContainerBox*   pBox,
        const Params*           pParams ) :
        m_cyDescent(0),
        m_hdc(hdc),
        m_hHeap(hHeap),
        m_pBox(pBox),
        m_pNode(pNode),
        m_pParams(pParams)
    {
        m_oStyle = pParams->m_oStyle;
    } // Context

    // [C]
    public: void Clear(const Markup::Node*)
    {
        if (FinishHBox())
        {
            Font* pFont = m_pBox->ComputeFont(m_hdc, 'x');
            m_pBox->m_rc.bottom += pFont->GetHeight();
        }
    } // Clear

    // [E]
    public: HBox* EnsureHBox();
    public: VBox* EnsureVBox();
    
    // [F]
    public: bool FinishHBox()
    {
        if (HBox* pBox = m_pBox->DynamicCast<HBox>())
        {
            pBox->Finalize();
            m_pBox = pBox->GetParent();
            m_pBox->AppendBox(pBox);
            return pBox->IsEmpty();
        } // if hbox

        return false;
    } // FinishHBox

    // [N]
    public: HBox* NewHBox(const Markup::Node*);
    public: VBox* NewVBox(const Markup::Node*);

    // [S]
    public: void StartHBox(HBox* pHBox)
    {
        m_pBox = pHBox;
    } // PushBox
}; // Context

// [F]
HBox* Context::EnsureHBox()
{
    HBox* pHBox = m_pBox->DynamicCast<HBox>();
    if (NULL == pHBox)
    {
        pHBox = NewHBox(m_pNode);
        m_pBox = pHBox;
    } // if
    return pHBox;
} // Context::EnsureHBox

VBox* Context::EnsureVBox()
{
    VBox* pVBox = m_pBox->DynamicCast<VBox>();
    if (NULL == pVBox)
    {
        HBox* pHBox = m_pBox->StaticCast<HBox>();
        pHBox->Finalize();

        pVBox = m_pBox->GetParent()->StaticCast<VBox>();

        pVBox->AppendBox(pHBox);

        m_pBox = pVBox;
    } // if
    return pVBox;
} // Context::EnsureVBox

// [N]
HBox* Context::NewHBox(const Markup::Node* pNode)
{
    HBox* pHBox = new(m_hHeap) HBox(pNode, m_pBox);
    pNode->computeStyle(m_pBox, pHBox);
    if (VBox* pVBox = m_pBox->DynamicCast<VBox>())
    {
        pHBox->m_rc.left   = m_pBox->m_rc.left;
        pHBox->m_rc.top    = m_pBox->m_rc.bottom;
    }
    else
    {
        CAN_NOT_HAPPEN();
    }

    pHBox->m_rc.right  = pHBox->m_rc.left;
    pHBox->m_rc.bottom = pHBox->m_rc.top;
    return pHBox;
} // Context::NewHBox

VBox* Context::NewVBox(const Markup::Node* pNode)
{
    VBox* pVBox = new(m_hHeap) VBox(pNode, m_pBox);
    pNode->computeStyle(m_pBox, pVBox);
    if (Box* pBox = m_pBox->DynamicCast<HBox>())
    {
        pVBox->m_rc.left = pBox->m_rc.right;
        pVBox->m_rc.top  = pBox->m_rc.top;
    }
    else
    {
        CAN_NOT_HAPPEN();
    }

    pVBox->m_rc.right  = pVBox->m_rc.left;
    pVBox->m_rc.bottom = pVBox->m_rc.top;
    return pVBox;
} // Context::NewVBox

} // Formatter

namespace Markup
{

using namespace Formatter;

// [A]
Node* Node::AppendChild(Node* pNode)
{
    return Nodes::Append(pNode);
} // Node::AppendChild

// [C]
void Node::computeStyle(const RichStyle* pStyle, RichStyle* out_oStyle) const
{
    // FIXME 2007-12-31 yosi@msn.com Take style properties from node.
    *out_oStyle = *pStyle;
} // Node::computeStyle

// [F]
Render::RootBox* Node::Format(HDC hdc, const Params* pParams) const
{
    HANDLE hHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
    if (NULL == hHeap) PlatformError("HeapCreate");

    Render::RootBox* pRootBox = new(hHeap) Render::RootBox(this, hHeap);
    
    *static_cast<RichStyle*>(pRootBox) = pParams->m_oStyle;

    Context oContext(hdc, hHeap, this, pRootBox, pParams);

    Format(&oContext);
    return pRootBox;
} // Node::Format

// [B]
void Body::Format(Context* pContext) const
{
    Render::HBox* pHBox = pContext->NewHBox(this);

    pContext->StartHBox(pHBox);

    foreach (Nodes::Enum, oEnum, this)
    {
        oEnum.Get()->Format(pContext);
    } // for each node
    
    pContext->FinishHBox();
} // Body::Format

void Br::Format(Context* pContext) const
{
    pContext->Clear(this);
} // Br::Format

void Button::Format(Context* pContext) const
{
    Render::HBox* pHBox = pContext->EnsureHBox();

    RichStyle oStyle;
    computeStyle(pHBox, &oStyle);

    Font* pFont = oStyle.ComputeFont(pContext->m_hdc, 'x');

    Text* pText = GetFirst()->StaticCast<Text>();

    const char16* pwch = pText->GetString();
    uint cwch = pText->GetLength();

    int cx = pFont->GetTextWidth(pContext->m_hdc, pwch, cwch);

    Render::Button* pBox = new(pContext->m_hHeap) Render::Button(
        this,
        pContext->m_pBox,
        BS_PUSHBUTTON,
        pText );

    *static_cast<RichStyle*>(pBox) = oStyle;

    int cy = pFont->GetHeight();

    const int k_cxPadding = 5;
    const int k_cyPadding = 3;

    cx += k_cxPadding * 2;
    cy += k_cyPadding * 2;

    pBox->m_cyDescent = pFont->GetDescent();
    pBox->m_rc.left   = pHBox->m_rc.right;
    pBox->m_rc.right  = pBox->m_rc.left + cx;
    pBox->m_rc.top    = pHBox->m_rc.top;
    pBox->m_rc.bottom = pBox->m_rc.top + cy;

    pHBox->AppendBox(pBox);
} // Button::Format

// [T]
Text::Text(const char16* pwsz)
{
    m_cwch = ::lstrlenW(pwsz);
    m_pwch = new char16[m_cwch + 1];
    ::lstrcpyW(m_pwch, pwsz);
} // Text::Text

Text::Text(const char* psz)
{
    m_cwch = ::lstrlenA(psz);
    m_pwch = new char16[m_cwch + 1];
    char16* pwch = m_pwch;
    while (0 != *psz) { *pwch++ = *psz++; }
    *pwch = 0;
} // Text::Text

void Text::Format(Context* pContext) const
{
    // FIXME 2007-12-30 yosi@msn.com NYI multiline text format

    Render::HBox* pHBox = pContext->EnsureHBox();

    RichStyle oStyle;
    computeStyle(pHBox, &oStyle);

    Font* pFont = oStyle.ComputeFont(pContext->m_hdc, 'x');

    int cx = pFont->GetTextWidth(
        pContext->m_hdc,
        m_pwch,
        m_cwch );

    Render::Text* pBox = new(pContext->m_hHeap) Render::Text(
        this,
        pContext->m_pBox,
        cx,
        0,
        m_cwch );

    *static_cast<RichStyle*>(pBox) = oStyle;

    pBox->m_cyDescent = pFont->GetDescent();
    pBox->m_rc.left   = pHBox->m_rc.right;
    pBox->m_rc.right  = pBox->m_rc.left + cx;
    pBox->m_rc.top    = pHBox->m_rc.top;
    pBox->m_rc.bottom = pBox->m_rc.top + pFont->GetHeight();

    pHBox->AppendBox(pBox);
} // Text::Format


void TextBox::Format(Context* pContext) const
{
    Render::HBox* pHBox = pContext->EnsureHBox();

    RichStyle oStyle;
    computeStyle(pHBox, &oStyle);

    Font* pFont = oStyle.ComputeFont(pContext->m_hdc, 'm');

    int cx = pFont->GetWidth() * m_cwch;

    DWORD dwStyle = ES_AUTOHSCROLL | ES_NOHIDESEL;
    dwStyle |= WS_BORDER;

    Render::TextBox* pBox = new(pContext->m_hHeap) Render::TextBox(
        this,
        pContext->m_pBox,
        dwStyle );

    *static_cast<RichStyle*>(pBox) = oStyle;

    int cy = pFont->GetHeight();

    const int k_cxPadding = 5;
    const int k_cyPadding = 3;

    cx += k_cxPadding * 2;
    cy += k_cyPadding * 2;

    pBox->m_cyDescent = pFont->GetDescent();
    pBox->m_rc.left   = pHBox->m_rc.right;
    pBox->m_rc.right  = pBox->m_rc.left + cx;
    pBox->m_rc.top    = pHBox->m_rc.top;
    pBox->m_rc.bottom = pBox->m_rc.top + cy;

    pHBox->AppendBox(pBox);
} // TextBox::Format


} // Markup

} // RichView
} // Peer
} // Editor
