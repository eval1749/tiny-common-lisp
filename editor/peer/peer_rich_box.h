//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer
// eidtor/vi_window.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_box.h#2 $
//
#if !defined(INCLUDE_editor_peer_rich_box_h)
#define INCLUDE_editor_peer_rich_box_h


namespace Editor
{

namespace Peer
{

namespace RichView
{

namespace Markup
{
    class Node;
    class Text;
} // Markup

namespace Render
{

class ContainerBox;
class RootBox;

class Params
{
    public: HDC     m_hdc;
    public: HWND    m_hwnd;
    public: Rect    m_rc;

    public: Params(HWND hwnd, HDC hdc) :
        m_hdc(hdc),
        m_hwnd(hwnd) {}
}; // Params

/// <summary>
///   Represents rendering box.
/// </summary>
class Box :
    public Castable_<Box>,
    public DoubleLinkedItem_<Box, ContainerBox>,
    public ObjectInHeap,
    public RichStyle
{
    public: typedef DoubleLinkedList_<Box, ContainerBox> Boxes;

    public:    int                  m_cyDescent;
    protected: const Markup::Node*  m_pNode;
    private:   ContainerBox*        m_pParent;
    public:    Rect                 m_rc;

    protected: Box() :
        m_cyDescent(0),
        m_pNode(NULL),
        m_pParent(NULL) {}

    public: virtual ~Box()
    {
        #if DEBUG_LIFE
            DEBUG_PRINTF("%p\n", this);
        #endif
    } // ~Box

    // [D]
    protected: void drawBorder() const;

    // [G]
    public:    int           GetHeight() const { return m_rc.GetHeight(); }
    public:    ContainerBox* GetParent() const { return m_pParent; }
    protected: RootBox*      getRoot()   const;
    public:    int           GetWidth()  const { return m_rc.GetWidth(); }

    // [I]
    protected: void init(
        const Markup::Node* const pNode,
        ContainerBox*       const pParent )
    {
        m_pNode   = pNode;
        m_pParent = pParent;
    } // init

    // [O]
    protected: void offsetBox(Rect* prc) const
    {
        prc->left   += m_rcMargin.left   + m_rcBorder.left;
        prc->top    += m_rcMargin.top    + m_rcBorder.top;
        prc->right  -= m_rcMargin.right  + m_rcBorder.right;
        prc->left   -= m_rcMargin.bottom + m_rcBorder.bottom;
    } // offsetBox

    // [R]
    public: virtual void Realize(HWND) {}
    public: virtual void Render(const Params*) const = 0;
}; // Box

template<class T, class B = Box>
class Box_ :
    public WithCastable_<T, B>
{
    protected: typedef Box_<T, B> Base;

    protected: Box_() {}

    protected: Box_(
        const Markup::Node* const pNode,
        ContainerBox*       const pParent)
            { init(pNode, pParent); }
}; // Box_

class ChildWindow :
    public Box_<ChildWindow>,
    public DoubleLinkedItem_<ChildWindow, RootBox>
{
    public: static const char* Kind_() { return "ChildWindow"; }

    protected: HWND   m_hwnd;

    // ctor
    #if 0
    public: ChildWindow(const Markup::Node* pNode, ContainerBox* pParent) :
        m_hwnd(NULL),
        Base(pNode, pParent) {}
    #endif

    protected: ChildWindow() :
        m_hwnd(NULL) {}

    // [C]
    protected: void createWindow(
        HWND,
        const char16*,
        DWORD,
        const char16* );

    // [G]
    public: HWND GetHwnd() const { return m_hwnd; }

    // [R]
    protected: virtual void Render(const Params*) const override {}
}; // ChildWindow

class Button :
    public Box_<Button, ChildWindow>
{
    public: static const char* Kind_() { return "Button"; }

    private: DWORD m_dwStyle;
    private: const Markup::Text*    m_pText;

    // ctor
    public: Button(
        const Markup::Node* pNode,
        ContainerBox*       pParent,
        DWORD               dwStyle,
        const Markup::Text* pText ) :
        m_dwStyle(dwStyle),
        m_pText(pText),
        Base(pNode, pParent) {}

    // [R]
    private: virtual void Realize(HWND) override;

    // [S]
    public: void SetDefault(bool);
    public: static void SetDefault(HWND, bool);
}; // Button

class ContainerBox :
    public Box_<ContainerBox, Box>,
    public DoubleLinkedList_<Box, ContainerBox>
{
    public: static const char* Kind_() { return "ContainerBox"; }

    // ctor
    protected: ContainerBox(
        const Markup::Node* const pNode,
        ContainerBox*       const pParent) :
            Base(pNode, pParent) {}

    protected: ContainerBox() {}

    // [A]
    public: virtual Box* AppendBox(Box*) = 0;

    // [F]
    public: virtual void Finalize() = 0;

    // [R]
    public: virtual void Realize(HWND);
}; // ContainerBox

class HBox :
    public Box_<HBox, ContainerBox>
{
    public: static const char* Kind_() { return "HBox"; }

    // ctor
    public: HBox(const Markup::Node* pNode, ContainerBox* pParent) :
        Base(pNode, pParent) {}

    // [A]
    public: virtual Box* AppendBox(Box*) override;

    // [F]
    public: virtual void Finalize() override;

    // [R]
    public: virtual void Render(const Params*) const override;
}; // HBox

class VBox :
    public Box_<VBox, ContainerBox>
{
    public: static const char* Kind_() { return "VBox"; }

    // ctor
    public: VBox(const Markup::Node* pNode, ContainerBox* pParent) :
        Base(pNode, pParent) {}

    protected: VBox() {}

    // [A]
    public: virtual Box* AppendBox(Box*) override;

    // [F]
    public: virtual void Finalize() override;

    // [R]
    public: virtual void Render(const Params*) const override;
}; // VBox

class RootBox :
    public Box_<RootBox, VBox>
{
    public: static const char* Kind_() { return "RootBox"; }

    private: typedef DoubleLinkedList_<ChildWindow, RootBox>
        ChildWindows;

    public:  HANDLE m_hHeap;
    private: ChildWindows m_oChildWindows;

    // ctor
    public: RootBox(const Markup::Node* pNode, HANDLE hHeap) :
        m_hHeap(hHeap),
        Base(pNode, NULL) {}

    // [A]
    public: void AddChildWindow(ChildWindow* p)
        {  m_oChildWindows.Append(p); }

    // [D]
    public: void Destroy()
    {
        #if DEBUG_LIFE
            DEBUG_PRINTF("%p\n", this);
        #endif

        foreach (ChildWindows::Enum, oEnum, &m_oChildWindows)
        {
            ChildWindow* pChild = oEnum.Get();
            if (HWND hwnd = pChild->GetHwnd())
            {
                ::DestroyWindow(hwnd);
            }
        } // for each child winow

        if (NULL != m_hHeap) ::HeapDestroy(m_hHeap);
    } // Destroy

    // [F]
    public: virtual void Finalize() override {}

    // [R]
    public: virtual void Render(const Params*) const override;
}; // RootBox

class Text :
    public Box_<Text, Box>
{
    private: int    m_cx;
    private: int    m_iStart;
    private: int    m_iEnd;

    public: static const char* Kind_() { return "Text"; }

    public: Text(
        const Markup::Node* pNode,
        ContainerBox*       pParent,
        int                 cx,
        int                 iStart,
        int                 iEnd ) :
        m_cx(cx),
        m_iEnd(iEnd),
        m_iStart(iStart),
        Base(pNode, pParent) {}

    // [R]
    public: virtual void Render(const Params*) const override;
}; // Text

class TextBox :
    public Box_<TextBox, ChildWindow>
{
    public: static const char* Kind_() { return "TextBox"; }

    private: DWORD m_dwStyle;

    // ctor
    public: TextBox(
        const Markup::Node* pNode,
        ContainerBox*       pParent,
        DWORD               dwStyle ) :
        m_dwStyle(dwStyle),
        Base(pNode, pParent) {}

    // [R]
    private: virtual void Realize(HWND) override;
}; // TextBox

/// <summary>
///   Walk box tree.
/// </summary>
class BoxWalker
{
    public: void Run(Box* const pBox)
    {
        visit(pBox);
    } // Run

    protected: virtual bool onVisit(Box*) = 0;

    protected: bool visit(Box* pBox)
    {
        if (! onVisit(pBox))
        {
            return false;
        }

        if (ContainerBox* pContainer = pBox->DynamicCast<ContainerBox>())
        {
            foreach (ContainerBox::Enum, oEnum, pContainer)
            {
                if (! visit(oEnum.Get()))
                {
                    return false;
                }
            } // for child
        }

        return true;
    } // visit
}; // BoxWalker

} // Render

} // RichBox
} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_rich_box_h)
