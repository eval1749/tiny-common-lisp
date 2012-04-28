//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer
// eidtor/vi_window.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_rich_dialog.h#3 $
//
#if !defined(INCLUDE_editor_peer_rich_dialog_h)
#define INCLUDE_editor_peer_rich_dialog_h

#include "./peer_window.h"

namespace Editor
{

namespace Peer
{

namespace RichView
{
    namespace Markup
    {
        class Node;
    } // Markup

    namespace Render
    {
        class RootBox;
    } // Render
} // RichView

/// <summary>
///   Represents Rich Dialog Box.
/// </summary>
class RichDialog : public Window_<RichDialog>
{
    public: static const char* Kind_() { return "RichDialog"; }

    private: typedef RichView::Markup::Node    Node;
    private: typedef RichView::Render::RootBox RootBox;

    private: static uint sm_nModal;

    private: HWND       m_hwndDefaultButton;
    private: HWND       m_hwndFocus;
    private: Node*      m_pNode;
    private: RootBox*   m_pRootBox;

    // ctor
    public: RichDialog(Node*);
    public: ~RichDialog();

    // [D]
    public: int DoModal();

    // [G]
    protected: HWND getControl(uint) const;
    protected: Val  getControlText(uint) const;

    // [I]
    private: HWND isButton(HWND);
    public: bool IsDialogMessage(const MSG*);

    // [M]
    private: void modalLoop(HWND);
    private: bool motion(HWND, uint, bool);

    // [O]
    protected: virtual int onCreate(CREATESTRUCT*) override;
    protected: virtual void onCommand(uint, uint) {}
    protected: virtual LRESULT onMessage(uint, WPARAM, LPARAM) override;

    protected: virtual void onNcDestroy() override
    {
        ASSERT(sm_nModal > 0);
        sm_nModal -= 1;
        Super::onNcDestroy();
    } // onNcDestroy

    // [R]
    private: void restoreFocusItem();

    // [S]
    private: void saveFocusItem();

    private: LRESULT sendCommand(
        uint const nCtrlId,
        uint const nEvent )
    {
        return sendCommand(nCtrlId, nEvent, ::GetDlgItem(GetHwnd(), nCtrlId));
    } // sendCommand

    private: LRESULT sendCommand(
        uint const nCtrlId,
        uint const nEvent,
        HWND const hwndCtrl )
    {
        if (NULL == hwndCtrl)
        {
            return 0;
        }

        return SendMessage(
            WM_COMMAND, 
            MAKELONG(nCtrlId, nEvent), 
            reinterpret_cast<LPARAM>(hwndCtrl) );
    } // sendCommand

    private: void setFocus(HWND);
}; // RichDialog

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_rich_dialog_h)
