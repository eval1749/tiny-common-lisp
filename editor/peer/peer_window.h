//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window Peer
// eidtor/peer/peer_window.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_window.h#8 $
//
#if !defined(INCLUDE_editor_peer_window_h)
#define INCLUDE_editor_peer_window_h

#include "./peer_defs.h"

class StatusBar;
class TitleBar;

namespace Editor
{

namespace Peer
{

class Container;
class Frame;
class Window;

class Window : 
    public Castable_<Window>,
    public ChildItem_<Window, Container>
{
    protected: class TimerHandler
    {
        private: HWND   m_hwnd;
        protected: TimerHandler() : m_hwnd(NULL) {}
        public: bool IsActive() const { return NULL != m_hwnd; }

        public: void Continue(uint nMillisecond)
        {
            ASSERT(NULL != m_hwnd);

            UINT_PTR nTimerId = reinterpret_cast<UINT_PTR>(this);
            if (! ::SetTimer(m_hwnd, nTimerId, nMillisecond, NULL))
            {
                m_hwnd = NULL;
            }
        } // Continue

        public: virtual void OnTimer(Window* p) = 0;

        public: void Start(HWND hwnd, uint nMillisecond)
        {
            if (NULL != m_hwnd) return;
            m_hwnd = hwnd;
            Continue(nMillisecond);
        } // Start

        public: void Stop()
        {
            if (NULL != m_hwnd)
            {
                ::KillTimer(m_hwnd, reinterpret_cast<UINT_PTR>(this));
                m_hwnd = NULL;
            }
        } // Stop
    }; // TimerHandler

    private: static ATOM    sm_atomWndClass;
    private: static int     sm_iActiveTick;
    private: static Window* sm_pCreateWnd;

    protected: HWND         m_hwnd;
    private:   int          m_iActiveTick;

    // ctor
    protected: Window() :
        m_hwnd(NULL),
        m_iActiveTick(0) {}

    protected: virtual ~Window()
        { ASSERT(NULL == m_hwnd); }

    public: operator HWND() const { return m_hwnd; }

    // [A]
    public: bool Activate();

    // [C]
    public: virtual Window* Clone() const
        { CAN_NOT_HAPPEN(); }

    public: bool CreateWindowEx(
        DWORD   dwExStyle,
        LPCWSTR pwszText,
        DWORD   dwStyle,
        HWND    hwndParent = NULL,
        int     x = CW_USEDEFAULT,
        int     y = CW_USEDEFAULT,
        int     w = CW_USEDEFAULT,
        int     h = CW_USEDEFAULT );

    // [D]
    public: void Destroy()
    {
        ASSERT(IsRealized());
        ::DestroyWindow(m_hwnd);
    } // Destroy

    public: virtual bool DrawStatusBar(StatusBar* const) { return false; }
    public: virtual bool DrawTitleBar(TitleBar* const)   { return false; }

    // [F]
    public: static Window* FromHwnd(HWND);

    public: static Window* FromHwnd(LPARAM lParam)
        { return FromHwnd(reinterpret_cast<HWND>(lParam)); }

    // [G]
    public: int GetActiveTick() const
        { return m_iActiveTick; }

    public: HWND GetHwnd() const
        { return m_hwnd; }

    public: virtual const char* GetKind() const = 0;

    public: Window* GetFollowingSibling() const
        { return GetNext(); }

    public: virtual Window* GetFollowing() const;

    public: Frame* GetFrame() const;

    public: Container* GetParent() const
        { return m_pParent; }

    public: Window* GetPrecedingSibling() const
        { return GetPrev(); }

    public: virtual Window* GetPreceding() const;

    public: virtual DWORD GetStyle() const
        { return 0; }

    // [I]
    public: bool IsRealized() const { return NULL != m_hwnd; }

    // [O]
    protected: virtual int      onCreate(CREATESTRUCT*) { return 0; }
    protected: virtual void     onDestroy() {}
    public:    virtual bool     OnIdle(uint) { return false; }
    protected: virtual void     onNcDestroy() { delete this; }
    protected: virtual LRESULT  onMessage(uint, WPARAM, LPARAM);

    // [R]
    public: virtual void Realize(Container*);

    // [S]
    public: LRESULT SendMessage(
        uint    uMsg,
        WPARAM  wParam = 0,
        LPARAM  lParam = 0 )
    {
        return ::SendMessage(m_hwnd, uMsg, wParam, lParam);
    } // SendMessage

    public: virtual void SetScrollBar(HWND, int)
        { return; }

    // [W]
    static LRESULT CALLBACK windowProc(
        HWND    hwnd,
        UINT    uMsg,
        WPARAM  wParam,
        LPARAM  lParam );
}; // Window

template<class T, class B = Window>
class Window_ : public B
{
    protected: typedef Window_<T, B> Super;

    protected: Window_() {}
    protected: Window_(Val x) : B(x) {}

    public: virtual const char* GetKind() const override
        { return T::Kind_(); }

    public: virtual bool Is_(const char* psz) const override
        { return T::Kind_() == psz || B::Is_(psz); }
}; // Window_

} // Peer
} // Editor

#endif //!defined(INCLUDE_editor_peer_window_h)
