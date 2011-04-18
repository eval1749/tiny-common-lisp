//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer
// eidtor/vi_window.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_node.h#1 $
//
#if !defined(INCLUDE_editor_peer_rich_node_h)
#define INCLUDE_editor_peer_rich_node_h

#include "./peer_style.h"

namespace Editor
{

namespace Peer
{

namespace RichView
{

namespace Formatter
{
    class Context;
} // Formatter

namespace Render
{
    class RootBox;
} // Rendering

namespace Markup
{

using Formatter::Params;

class Node :
    public Castable_<Node>,
    public ChildItem_<Node, Node>,
    protected ChildList_<Node, Node>
{
    public: typedef ChildList_<Node, Node> Nodes;

    friend class Formatter::Context;
    protected: typedef Formatter::Context Context;

    protected: RichStyle    m_oStyle;

    // ctor
    protected: Node() {}

    public: virtual ~Node()
    {
        #if DEBUG_LIFE
            DEBUG_PRINTF("%p\n", this);
        #endif
    } // ~Node

    // [A]
    public: Node* AppendChild(Node*);

    // [C]
    protected: void computeStyle(const RichStyle*, RichStyle*) const;

    // [F]
    public: Render::RootBox* Format(HDC, const Params*) const;
    public: virtual void     Format(Context*) const = 0;

    // [I]
    public: Node* InsertBefore(Node*, Node*);

    // [R]
    public: Node* RemoveChild(Node*);
}; // Node

template<class T, class B = Node>
class Node_ :
    public WithCastable_<T, B>
{
}; // Node_

class Control : public Node_<Control>
{
    public: static const char* Kind_() { return "Control"; }

    private: uint m_nId;
    public:  uint GetCtrlId() const { return m_nId; }
    protected: void init(uint nId) { m_nId = nId; }
}; // ChildWindow

// [B]
class Body :
    public Node_<Body>
{
    public: static const char* Kind_() { return "Body"; }

    private: override void Format(Context*) const;
}; // Body

class Br :
    public Node_<Br>
{
    public: static const char* Kind_() { return "Text"; }

    // [F]
    private: override void Format(Context*) const;
}; // Br

class Button :
    public Node_<Button, Control>
{
    public: static const char* Kind_() { return "Button"; }

    // ctor
    public: Button(uint nId) { Control::init(nId); }

    // [F]
    private: override void Format(Context*) const;
}; // Button

// [T]
#if 0
class Table :
    public Node_<Table>
{
    public: class Row : public Node_<Row>
    {
        public: class Cell : public Node_<Cell>
        {
        }; // Cell
    }; // Row
}; // Table
#endif

class Text :
    public Node_<Text>
{
    public: static const char* Kind_() { return "Text"; }

    private: uint       m_cwch;
    private: char16*    m_pwch;

    // ctor
    public: Text(const char16* pwsz);
    public: Text(const char* psz);
    public: ~Text() { delete[] m_pwch; }

    // [F]
    private: override void Format(Context*) const;

    // [G]
    public: uint          GetLength() const { return m_cwch; }
    public: const char16* GetString() const { return m_pwch; }
}; // Text

class TextBox :
    public Node_<TextBox, Control>
{
    public: static const char* Kind_() { return "Text"; }

    private: int m_cwch;
    private: uint m_nId;

    // ctor
    public: TextBox(uint nId, int cwch) :
        m_cwch(cwch)
    {
        Control::init(nId);
    } // TextBox

    // [F]
    private: override void Format(Context*) const;
}; // TextBox

} // Markup
} // RichView
} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_rich_node_h)
