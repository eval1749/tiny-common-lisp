//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Window Peer
// eidtor/peer/peer_text_window.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_text_window.h#10 $
//
#if !defined(INCLUDE_editor_visual_text_window_h)
#define INCLUDE_editor_visual_text_window_h

#include "./peer_interactive_window.h"

#include "../ed_defs.h"

namespace Editor
{

namespace Peer
{

class FormatParams;
class TextPage;

/// <summary>
///   A TextWindow is a window for text editting.
/// </summary>
class TextWindow :
    public Window_<TextWindow, InteractiveWindow>
{
    public: static const char* Kind_() { return "TextWindow"; }

    protected: class AutoScroll : public TimerHandler
    {
        private: int    m_iDirection;
        private: uint   m_nStartTick;

        public: AutoScroll() :
            m_iDirection(0),
            m_nStartTick(0) {}

        public: virtual void OnTimer(Window*) override;

        public: void Start(HWND hwnd, int iDir)
        {
            m_iDirection = iDir;
            if (! IsActive())
            {
                m_nStartTick = ::GetTickCount();
                TimerHandler::Start(hwnd, 50);
            }
        } // Start
    }; // AutoScroll

    protected: class Blink : public TimerHandler
    {
        public: void OnTimer(Window*) { Stop(); }
    }; // Blink

    protected: class Drag
    {
        private: enum Mode
        {
            Mode_None,
            Mode_Selection,
        }; // Mode

        private: Mode   m_eMode;

        // ctor
        public: Drag() :
            m_eMode(Mode_None) {}

        // [I]
        public: bool IsDragging(HWND hwnd)
        {
            switch (m_eMode)
            {
            case Mode_None:
                return false;

            case Mode_Selection:
                return ::GetCapture() == hwnd;
                
            default:
                CAN_NOT_HAPPEN();
            } // switch mode
        } // IsDragging

        // [S]
        public: void Start(HWND hwnd)
        {
            ASSERT(Mode_None == m_eMode);
            m_eMode = Mode_Selection;
            ::SetCapture(hwnd);
        } // Start

        public: void Stop()
        {
            if (Mode_Selection == m_eMode)
            {
                ::ReleaseCapture();
            }
            m_eMode = Mode_None;
        } // Stop
    }; // Drag

    /// <summary>
    ///   Represents scroll bar.
    /// </summary>
    protected: struct ScrollBar
    {
        HWND    m_hwnd;
        int     m_nBar;

        ScrollBar() :
            m_hwnd(NULL),
            m_nBar(SB_CTL) {}

        bool GetInfo(SCROLLINFO* pInfo)
        {
            if (NULL == m_hwnd)
            {
                return false;
            }
            return 0 != ::GetScrollInfo(m_hwnd, m_nBar, pInfo);
        } // GetInfo

        HWND GetHwnd() const { return m_hwnd; }

        void Set(HWND hwnd, int nBar)
        {
            m_hwnd = hwnd;
            m_nBar = nBar;
        } // Set

        void SetInfo(SCROLLINFO* pInfo, bool fRedraw)
        {
            if (NULL != m_hwnd)
            {
                ::SetScrollInfo(m_hwnd, m_nBar, pInfo, fRedraw);
            }
        } // SetInfo
    }; // ScrollBar

    private: bool       m_fHasFocus;
    private: AutoScroll m_oAutoScroll;
    private: Blink      m_oBlink;
    private: Drag       m_oDrag;
    private: ScrollBar  m_oVertScrollBar;
    private: TextPage*  m_pCurPage;
    private: TextPage*  m_pNewPage;
    private: Rect       m_rc;

    private: Posn       m_caret_posn;

    // ctor
    public: TextWindow(Val);
    public: virtual ~TextWindow();

    // [B]
    public: void Blink(Posn, Val);

    // [C]
    private: bool  checkObsolete();
    public:  virtual Window* Clone() const override;
    public:  Count ComputeMotion(Unit, Val, POINT, Posn*);

    // [D]
    public: virtual bool DrawStatusBar(StatusBar* const);
    public: virtual bool DrawTitleBar(TitleBar* const)  override;

    // [E]
    public:  Posn EndOfLine(Posn);

    // [F]
    private: void fireEvent(Val);
    private: void format(HDC, const FormatParams*, Posn);
    private: void format(HDC, const FormatParams*, Posn, Posn);

    // [G]
    private: Buffer* GetBuffer() const;
    public:  Posn    GetEnd() const;

    private: Editor::TextWindow* getPeer() const
        { return GetObject()->StaticCast<Editor::TextWindow>(); }

    public: Selection* GetSelection() const;
    public:  Posn      GetStart() const;

    public: DWORD GetStyle() const
        { return WS_VSCROLL; }

    // [I]
    private: bool isSelectionActive() const;

    // [L]
    public: int LargeScroll(int, int);

    // [M]
    public: Posn  MapPointToPosn(POINT);
    public: int   MapPosnToPoint(Posn, POINT*);
    private: Posn mapXToPosn(int, Posn);

    // [O]
    private: virtual int     onCreate(CREATESTRUCT*) override;
    private: virtual void    onDestroy() override;
    private: virtual bool    OnIdle(uint) override;
    private: virtual LRESULT onMessage(uint, WPARAM, LPARAM) override;
    private: virtual void    onSetFocus() override;

    // [P]
    private: void prepareScroll(HDC, FormatParams*);

    // [R]
    private: void redraw();
    private: void rememberPageStart(Posn);
    private: void render(HDC);

    // [S]
    private: void          selectWord(Posn);
    public:  virtual void SetScrollBar(HWND, int) override;
    private: void          setupFormat(FormatParams*, bool);
    private: void          setupFormat(FormatParams*);
    public:  int           SmallScroll(int, int);
    public:  Posn          StartOfLine(Posn);

    // [U]
    private: void updateCaret(HDC);
    private: void updateScrollBar();

    #if SUPPORT_IME
    private: bool m_fImeTarget;
    private: Posn m_ime_start;
    private: Posn m_ime_end;
    private: void onImeComposition(LPARAM);
    public:  void Reconvert(Posn, Posn);
    private: uint setReconvert(RECONVERTSTRING*, Posn, Posn);
    private: BOOL showImeCaret(SIZE, POINT);
    #endif // SUPPORT_IME
}; // TextWindow

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_visual_text_window_h)
