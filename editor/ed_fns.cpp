#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Page
// editor/vi_text_page.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_fns.cpp#2 $
//
#include "./ed_defs.h"

namespace Editor
{

void __declspec(noreturn) PlatformError(const char* psz)
{
    DWORD dwError = ::GetLastError();
    error(Si::Qplatform_error,
        Kcode,  Si::MakeInt(dwError),
        Koperation, make_string(psz) );
} // PlatformError

} // Editor
