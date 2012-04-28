//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer - Container
// eidtor/peer/peer_window.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_container.h#6 $
//
#if !defined(INCLUDE_editor_peer_container_h)
#define INCLUDE_editor_peer_container_h

#include "./peer_window.h"

namespace Editor
{

namespace Peer
{

class Container :
    public Window_<Container>,
    protected ChildList_<Window, Container>
{
    public: static const char* Kind_() { return "Container"; }

    protected: typedef ChildList_<Window, Container> ChildWindows;

    protected: Window* m_pActive;

    // ctor
    protected: Container() :
        m_pActive(NULL) {}

    // [A]
    public: virtual Window* AppendChild(Window*);

    // [C]
    public: virtual bool CanClose() const
        { return GetParent()->CanClose(); }

    // [D]
    public: virtual bool DrawStatusBar(StatusBar* const);
    public: virtual bool DrawTitleBar(TitleBar* const);

    // [E]
    public: class EnumChild : public ChildWindows::Enum
    {
        public: EnumChild(const Container* p) :
            ChildWindows::Enum(p) {}
    }; // EnumWindows

    public: class EnumFollowing
    {
        private: Window* m_pRunner;

        public: EnumFollowing(const Container* p) :
            m_pRunner(const_cast<Container*>(p)) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: Window* Get() const
            { return m_pRunner; }

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = m_pRunner->GetFollowing();
        } // Next
    }; // EnumFollowing

    // [G]
    public: Window* GetActiveWindow() const { return m_pActive; }
    public: Window* GetFirstChild()   const { return GetFirst(); }
    public: Window* GetLastChild()    const { return GetLast(); }

    // [O]
    protected: virtual int onCreate(CREATESTRUCT*) override;
    protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM) override;

    // [R]
    public: virtual Window* RemoveChild(Window*);

    public: virtual Window* ReplaceChild(Window*, Window*)
        { CAN_NOT_HAPPEN(); }

    // [U]
    protected: void updateTitle();
}; // Container

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_container_h)
