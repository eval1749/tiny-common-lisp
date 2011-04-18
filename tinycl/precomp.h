//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions
// tinycl_compiler.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/precomp.h#2 $
//
#if !defined(INCLUDE_tinycl_precomp_h)
#define INCLUDE_tinycl_precomp_h

#pragma once

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stddef.h> // offsetof

#include "./z_defs.h"
#include "./z_debug.h"
#include "./vcsupport.h"

// C6246: Local declaration of 'foo' hides declaration of the same name in outer scope
#pragma warning(disable: 6246)

// Ignore new related warning since we manage memory ourselves. 
// C6211: Leaking memory 'pExt' due to an exception.
#pragma warning(disable: 6211)


// 'Type var' has different type in 'foo.cpp' and 'bar.cpp': '__declspec(align(8)) struct (40 bytes)' and 'struct (40 bytes)'
#pragma warning(disable: 4744)

// 'Type' : structure was padded due to __declspec(align())
#pragma warning(disable: 4324)


#endif //!defined(INCLUDE_tinycl_precomp_h)
