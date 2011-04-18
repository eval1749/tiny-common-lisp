//////////////////////////////////////////////////////////////////////////////
//
// Evita - Peer - Tab Band Control
// editor/peer/ctrl_TabBand.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/ctrl_TabBand.h#2 $
//
#if !defined(INCLUDE_control_tabBand_h)
#define INCLUDE_control_tabBand_h

#define TABBAND_NOTIFY_CLOSE        (TCN_LAST - 1)
//#define TABBAND_NOTIFY_QUERY_CLOSE  (TCN_LAST - 2)

static const char16 WC_TABBANDCLASS[] = L"TabBandClass";

void TabBand__Init(HINSTANCE);

#endif //!defined(INCLUDE_control_tabBand_h)
