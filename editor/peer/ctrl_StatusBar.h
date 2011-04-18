//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/ctrl_StatusBar.h#1 $
//
#if !defined(INCLUDE_visual_statusBar_h)
#define INCLUDE_visual_statusBar_h

//////////////////////////////////////////////////////////////////////
//
// StatusBar
//
class StatusBar
{
    private: int    m_cParts;
    private: HWND   m_hwnd;
    private: RECT   m_rc;
    private: int    m_rgiPart[50];

    public: StatusBar() :
        m_hwnd(NULL),
        m_cParts(0) {}

    public: operator HWND() const { return m_hwnd; }

    // [G]
    public: int GetHeight() const { return m_rc.bottom - m_rc.top; }

    // [I]
    public: bool IsEqual(const int* prgiPart, int cParts) const;

    // [R]
    public: void Realize(HWND hwndParent, int idCtrl);

    // [S]
    public: void SetParts(const int* prgiPart, int cParts);

    public: void SetSimple(bool fSimple)
        { ::SendMessage(m_hwnd, SB_SIMPLE, fSimple, 0); }

    public: void SetSimpleText(const char16* pwsz)
    {
        SetSimple(true);
        ::SendMessage(
            m_hwnd,
            SB_SETTEXT,
            SB_SIMPLEID | SBT_NOBORDERS,
            reinterpret_cast<LPARAM>(pwsz) );
    } // SetSimpleText
}; // StatusBar

#endif //!defined(INCLUDE_visual_statusBar_h)
