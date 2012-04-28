//////////////////////////////////////////////////////////////////////////////
//
// Regex - Pre-Compiled Header
// precomp.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/regex/precomp.h#1 $
//
#if !defined(INCLUDE_regex_precomp_h)
#define INCLUDE_regex_precomp_h

#pragma once

#pragma warning(disable: 4481)
#pragma warning(disable: 4627)
#pragma warning(disable: 4668)
#pragma warning(disable: 4820)

#define _WIN32_WINNT 0x0501
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "../regex/regex_defs.h"

#if NDEBUG

#undef CopyMemory
#define CopyMemory(d, s, c) myCopyMemory(d, s, c)

#undef FillMemory
#define FillMemory(d, c, k) myFillMemory(d, c, k)

//#undef ZeroMemory
//#define ZeroMemory(d, c) myZeroMemory(d, c)

extern "C"
{
    void* __fastcall myCopyMemory(void* dst, const void* src, size_t count);
    void* __fastcall myFillMemory(void* dst, size_t count, BYTE);
    void* __fastcall myZeroMemory(void* dst, size_t count);
} // extern "C"

#pragma function(memcmp)
#if _M_IX86
    #pragma function(memcpy)
    #pragma function(memset)
#endif // _M_IX86

#define my_memcmp     memcmp
#define my_memcpy     memcpy
#define my_memmove    memmove
#define my_memset     memset


#endif // NDEBUG

// C6246: Local declaration of 'foo' hides declaration of the same name in outer scope
#pragma warning(disable: 6246)

#endif // !defined(INCLUDE_regex_precomp_h)
