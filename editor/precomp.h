//////////////////////////////////////////////////////////////////////////////
//
// Editor - Pre-Compiled Header
// editor/precomp.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/precomp.h#8 $
//
#if !defined(INCLUDE_editor_precomp_h)
#define INCLUDE_editor_precomp_h

#pragma once

// warning C4481: nonstandard extension used: override specifier 'override'
#pragma warning(disable: 4481)

// Ignore new related warning since we manage memory ourselves. 
// C6211: Leaking memory 'pExt' due to an exception.
#pragma warning(disable: 6211)

// C6246: Local declaration of 'foo' hides declaration of the same name in outer scope
#pragma warning(disable: 6246)

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#define USE_GDIPLUS 1

#if USE_GDIPLUS
    #define INC_OLE2
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stddef.h> // offsetof

#include <ShellAPI.h>   // HDROP
#include <stddef.h>     // ptrdiff_t
#include <windowsx.h>   // GET_X_LPARAM


#if USE_GDIPLUS
    #include <gdiplus.h>
#endif

#undef GetFirstChild
#undef GetLastChild
#undef GetNextWindow
#undef GetPrevWindow

// undocumented SWP flags. See http://www.winehq.org.
#if !defined(SWP_NOCLIENTSIZE)
    #define SWP_NOCLIENTSIZE    0x0800
    #define SWP_NOCLIENTMOVE    0x1000
#endif // !defined(SWP_NOCLIENTSIZE)

// 0 = MAPVK_VK_TO_VSC
// 1 = MAPVK_VSC_TO_VK
// 2 = MAPVK_VK_TO_CHAR
// 3 = MAPVK_VSC_TO_VK_EX
// 4 = MAPVK_VK_TO_VSC_EX
#if !defined(MAPVK_VK_TO_CHAR)
    #define MAPVK_VK_TO_CHAR 2
#endif


// Theme (Visual Style )
#include <uxtheme.h>
#include <tmschema.h>

#include "../tinycl/z_defs.h"
#include "../tinycl/z_debug.h"
#include "../tinycl/vcsupport.h"
#include "../tinycl/tinycl.h"

namespace Editor
{
    using namespace TinyCl::CommonLisp;
    using TinyCl::Val;
} // Editor

#endif //!defined(INCLUDE_editor_precomp_h)
