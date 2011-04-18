#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Status Bar
// listener/winapp/vi_statusBar.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/ctrl_StatusBar.cpp#1 $
//
#define DEBUG_STATUSBAR 0
#include "./ctrl_StatusBar.h"

#include "./peer_defs.h"

//////////////////////////////////////////////////////////////////////
//
// StatusBar::IsEqual
//
bool StatusBar::IsEqual(const int* prgiPart, int cParts) const
{
    if (m_cParts != cParts)
    {
        return false;
    }

    return 0 == ::memcmp(m_rgiPart, prgiPart, sizeof(int) * cParts);
} // StatusBar::IsEqual


//////////////////////////////////////////////////////////////////////
//
// StatusBar::Realize
//
void StatusBar::Realize(HWND hwndParent, int idCtrl)
{
    m_hwnd = ::CreateWindowEx(
        0,
        STATUSCLASSNAMEW,
        NULL,
        WS_CHILD | WS_VISIBLE,
        0, 0, 0, 0,
        hwndParent,
        reinterpret_cast<HMENU>(static_cast<INT_PTR>(idCtrl)),
        g_hInstance,
        NULL );

    ::GetWindowRect(m_hwnd, &m_rc);
} // StatusBar::Realize


//////////////////////////////////////////////////////////////////////
//
// StatusBar::SetParts
//
void StatusBar::SetParts(const int* prgiPart, int cParts)
{
    if (IsEqual(prgiPart, cParts))
    {
        return;
    }

    #if DEBUG_STATUSBAR
        DEBUG_PRINTF(L"%p %d cParts=%d\n", this, ::GetTickCount(), cParts);
    #endif // DEBUG_STATUSBAR

    m_cParts = min(cParts, lengthof(m_rgiPart));

    ::CopyMemory(m_rgiPart, prgiPart, sizeof(int) * m_cParts);

    ASSERT(NULL != m_hwnd);

    ::SendMessage(
        m_hwnd,
        SB_SETPARTS,
        m_cParts,
        reinterpret_cast<LPARAM>(m_rgiPart) );

    // Erase borders
    for (int i = 0; i < m_cParts; i++)
    {
        ::SendMessage(
            m_hwnd,
            SB_SETTEXT,
            i | SBT_NOBORDERS,
            0 );
    } // for i
} // StatusBar::SetParts
