//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - Tab Container
// eidtor/peer/peer_tab_container.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_tab_container.h#3 $
//
#if !defined(INCLUDE_editor_peer_tab_container_h)
#define INCLUDE_editor_peer_tab_container_h

#include "./peer_container.h"

namespace Editor
{

namespace Peer
{

class TabContainer : public Window_<TabContainer, Container>
{
    public: static const char* Kind_()  { return "TabContainer"; }

    private: enum CtrlId
    {
        CtrlId_TabBand = 1,
    }; // CtrlId

    private: int        m_cyTabBand;
    private: HWND       m_hwndTabBand;
    private: Rect       m_rc;

    // cotr
    public: TabContainer();
    public: ~TabContainer();

    // [A]
    public:  virtual Window* AppendChild(Window*) override;
    private: Window* addTab(Window*);

    // [C]
    private: Rect computeContentRect() const;

    // [M]
    private: Window* mapTabToWindow(int) const;
    private: int     mapWindowToTab(Window*) const;

    // [O]
    private:   virtual int onCreate(CREATESTRUCT*) override;
    public:    virtual bool OnIdle(uint) override;
    protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM) override;

    // [R]
    public: virtual Window* RemoveChild(Window*) override;
    public: virtual Window* ReplaceChild(Window*, Window*) override;
}; // TabContainer

} // Peer
} // Editor

#endif // !defined(INCLUDE_editor_peer_tab_container_h)
