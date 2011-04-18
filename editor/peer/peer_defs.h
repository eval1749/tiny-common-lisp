//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - Definitions
// eidtor/peer/peer_defs.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_defs.h#3 $
//
#if !defined(INCLUDE_editor_peer_defs_h)
#define INCLUDE_editor_peer_defs_h

#include "../ed_defs.h"

extern HINSTANCE g_hInstance;
extern HINSTANCE g_hResource;

#define DEBUG_LIFE  _DEBUG

namespace Editor
{

namespace Peer
{

using TinyCl::Castable_;
using TinyCl::ChildItem_;
using TinyCl::ChildList_;
using TinyCl::DoubleLinkedItem_;
using TinyCl::DoubleLinkedList_;

/// <summary>
///   Peer window message.
/// </summary>
enum PeerWm
{
    PEER_WM_FIRST = WM_USER,

    PEER_WM_ACTIVATE = PEER_WM_FIRST,
    PEER_WM_DROPFILES,

    PEER_WM_LAST,
}; // PeerWm

/// <summary>
///   Thread window message.
/// </summary>
enum ThreadWm
{
    THREAD_WM_ = WM_USER,

    THREAD_WM_GENESIS,
    THREAD_WM_STDOUT,
}; // ThreadWm

// Color
class Color
{
    COLORREF    m_cr;

    public: Color(COLORREF cr = 0) : m_cr(cr) {}
    public: Color(uint r, uint g, uint b) : m_cr(RGB(r, g, b)) {}

    public: operator COLORREF() const { return m_cr; }

    public: bool Equal(const Color& cr) const { return m_cr == cr.m_cr; }
    public: uint Hash() const { return m_cr; }
}; // Color

class Dc
{
    private: HDC    m_hdc;
    private: HWND   m_hwnd;

    public: Dc(HWND hwnd) :
        m_hdc(::GetDC(hwnd)),
        m_hwnd(hwnd)
    {
        if (NULL == m_hdc)
        {
            PlatformError("GetDC");
        }
    } // Dc

    public: ~Dc()
    {
        ASSERT(NULL != m_hdc);
        if (! ::ReleaseDC(m_hwnd, m_hdc))
        {
            PlatformError("ReleaseDC");
        }
    } // ~Dc

    public: operator HDC() const
        { ASSERT(NULL != m_hdc); return m_hdc; }
}; // Dc

class DcSelect
{
    private: HDC        m_hdc;
    private: HGDIOBJ    m_hObject;

    public: DcSelect(HDC hdc, HBRUSH hBrush) :
        m_hdc(hdc),
        m_hObject(::SelectObject(hdc, hBrush)) {}

    public: DcSelect(HDC hdc, HFONT hFont) :
        m_hdc(hdc),
        m_hObject(::SelectObject(hdc, hFont)) {}

    public: DcSelect(HDC hdc, HGDIOBJ hGdiobj) :
        m_hdc(hdc),
        m_hObject(::SelectObject(hdc, hGdiobj)) {}

#if 0
    public: DcSelect(HDC hdc, HPEN hPen) :
        m_hdc(hdc),
        m_hObject(::SelectObject(hdc, hPen)) {}
#endif

    public: ~DcSelect()
    {
        ::SelectObject(m_hdc, m_hObject);
    } // ~DcSelect
}; // DcSelect

class ObjectInHeap
{
    public: void* operator new(size_t cb, HANDLE hHeap)
    {
        void* pv = ::HeapAlloc(hHeap, 0, cb);
        if (NULL == pv)
        {
            PlatformError("HeapAlloc");
        }
        return pv;
    } // operator new
    
    public: void operator delete(void*)
    {
        CAN_NOT_HAPPEN();
    } // delete
}; // ObjectInHeap

// Pen
class Pen
{
    HPEN m_h;

    public: Pen(Color cr) :
        m_h(::CreatePen(PS_SOLID, 0, cr)) {}

    public: Pen(int iStyle, int iWidth, Color cr) :
        m_h(::CreatePen(iStyle, iWidth, cr)) {}

    public: ~Pen()
        { if (NULL != m_h) ::DeleteObject(m_h); }

    public: operator HPEN() const
        { return m_h; }

    public: operator HGDIOBJ() const
        { return reinterpret_cast<HGDIOBJ>(m_h); }
}; // Pen

struct Point : POINT
{
    Point(int xx = 0, int yy = 0)
        { x = xx; y  = yy; }

    Point(POINTS pt)
        { x = pt.x; y = pt.y; }
}; // Point

struct Rect : RECT
{
    Rect(int il = 0, int it = 0, int ir = 0, int ib = 0)
    {
        left = il;
        top  = it;
        right = ir;
        bottom = ib;
    } // Rect

    Rect(const RECT* prc)
    {
        left = prc->left;
        top = prc->top;
        right  = prc->right;
        bottom = prc->bottom;
    } // Rect

    Rect(const RECT& rc)
    {
        left = rc.left;
        top = rc.top;
        right  = rc.right;
        bottom = rc.bottom;
    } // Rect

    Rect(const Rect& rc)
    {
        left = rc.left;
        top = rc.top;
        right  = rc.right;
        bottom = rc.bottom;
    } // Rect

    // [G]
    int GetHeight() const { return bottom - top; }
    int GetWidth()  const { return right - left; }
}; // Rect

struct Size : SIZE
{
    Size(int ix = 0, int iy = 0)
    {
        cx = ix;
        cy = iy;
    } // Size
}; // Size

template<class T>
class WithObject_
{
    protected: typedef WithObject_<T> WithObject;

    private: TinyCl::Mm::GcAnchor* m_pGcAnchor;

    // ctor
    protected: WithObject_(Val object) :
        m_pGcAnchor(TinyCl::Mm::CreateGcAnchor(object)) {}

    public: ~WithObject_()
        { TinyCl::Mm::DestroyGcAnchor(m_pGcAnchor); }

    // [G]
    public: Val GetObject() const
        { return m_pGcAnchor->Get(); }

    public: T* GetPeer() const
        { return GetObject()->StaticCast<T>(); }
}; // WithObject_

class Application;
class Frame;
class Window;
class FormatParams;
class RenderStyle;

inline void drawLine(HDC hdc, int sx, int sy, int ex, int ey)
    { ::MoveToEx(hdc, sx, sy, NULL); ::LineTo(hdc, ex, ey); }

inline void drawHLine(HDC hdc, int sx, int ex, int y)
    { drawLine(hdc, sx, y, ex + 1, y); }

inline void drawVLine(HDC hdc, int x, int sy, int ey)
    { drawLine(hdc, x, sy, x, ey + 1); }

inline void fillRect(HDC hdc, const RECT* prc, COLORREF cr)
{
    ::SetBkColor(hdc, cr);
    ::ExtTextOutW(hdc, 0, 0, ETO_OPAQUE, prc, NULL, 0, NULL);
} // fillRect

} // Peer

} // Editor

#endif //!defined(INCLUDE_editor_peer_defs_h)
