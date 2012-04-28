//////////////////////////////////////////////////////////////////////////////
//
// Editor - Frame Peer
// eidtor/peer/peer_frame.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_frame.h#7 $
//
#if !defined(INCLUDE_editor_peer_frame_h)
#define INCLUDE_editor_peer_frame_h

#include "./peer_container.h"

#include "./ctrl_StatusBar.h"
#include "./ctrl_TitleBar.h"

namespace Editor
{

namespace Peer
{

class TabContainer;

/// <summary>
///   Peer frame window. Peer frame window is displayed as toplevel window
///   and communicates with window manager.
/// </summary>
class Frame :
    public Window_<Frame, Container>,
    public WithObject_<Editor::Frame>
{
    public: static const char* Kind_() { return "Frame"; }

    private: enum CtrlId
    {
        CtrlId_StatusBar = 1,
    }; // CtrlId

    private: bool           m_fBeingDestroy;
    private: RECT           m_rc;
    private: StatusBar      m_oStatusBar;
    private: TitleBar       m_oTitleBar;
    private: TabContainer*  m_pContainer;

    // ctor
    public: Frame(Val);
    public: virtual ~Frame();

    // [A]
    public: virtual Window* AppendChild(Window*) override;

    // [C]
    public: virtual bool CanClose() const override;

    // [G]
    public: Container* GetContainer() const;

    // [O]
    private: virtual int onCreate(CREATESTRUCT*) override;
    private: virtual void onDestroy() override;
    public:  virtual bool OnIdle(uint) override;
    private: virtual LRESULT onMessage(uint, WPARAM, LPARAM) override;

    // [R]
    public: virtual void Realize(Container*) override;
}; // Frame

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_frame_h)
