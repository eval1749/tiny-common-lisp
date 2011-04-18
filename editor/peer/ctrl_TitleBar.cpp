#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Title Bar
// listener/winapp/vi_ctrl_titleBar.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/ctrl_TitleBar.cpp#1 $
//
#define DEBUG_STATUSBAR 0
#include "./ctrl_TitleBar.h"

//////////////////////////////////////////////////////////////////////
//
// TitleBar::Realize
//
int TitleBar::Realize(HWND hwnd)
{
    m_hwnd = hwnd;
    return 0;
} // TitleBar::Reailize


//////////////////////////////////////////////////////////////////////
//
// TitleBar::IsEqual
//
bool TitleBar::IsEqual(const char16* pwch, int cwch) const
{
    cwch = min(cwch, lengthof(m_wsz) - 1);
    if (m_cwch != cwch) return false;
    return 0 == ::memcmp(m_wsz, pwch, sizeof(char16) * m_cwch);
} // TitleBar::IsEqual


//////////////////////////////////////////////////////////////////////
//
// TitleBar::SetText
//
int TitleBar::SetText(const char16* pwch, int cwch)
{
    if (IsEqual(pwch, cwch))
    {
        return 0;
    }

    m_cwch = min(cwch, lengthof(m_wsz) - 1);

    ::CopyMemory(m_wsz, pwch, sizeof(char16) * m_cwch);
    m_wsz[cwch] = 0;

    if (! ::SetWindowText(m_hwnd, m_wsz))
    {
        DWORD dwError = ::GetLastError();
        return dwError;
    }

    return 0;
} // TitleBar::Reailize
