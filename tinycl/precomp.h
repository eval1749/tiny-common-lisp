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

// TODO(yosi) 2012-04-28 We should enable C4061.
// C4061: enumerator 'Type_Macro' in switch of enum 'TinyCl::Private::Reader::Token::Type' is not explicitly handled by a case label
#pragma warning(disable: 4061)

// TODO(yosi) 2012-04-28 We should enable C4062.
// C4062: enumerator 'ScanType_Thread' in switch of enum 'TinyCl::Mm::Area::ScanType' is not handled
#pragma warning(disable: 4062)

// C4324: 'Type' : structure was padded due to __declspec(align())
#pragma warning(disable: 4324)

// C4365: 'return' : conversion from 'const size_t' to 'int', signed/unsigned mismatch
#pragma warning(disable: 4365)

// warning C4710: 'static void __fastcall `anonymous namespace'::Initializer::Run(void)' : function not inlined
#pragma warning(disable: 4710)

// warning C4711: function 'wchar_t * __fastcall lstrrchrW(wchar_t const *,wchar_t)' selected for automatic inline expansion
#pragma warning(disable: 4711)

// C4744: 'Type var' has different type in 'foo.cpp' and 'bar.cpp': '__declspec(align(8)) struct (40 bytes)' and 'struct (40 bytes)'
#pragma warning(disable: 4744)

// C4481: nonstandard extension used: override specifier 'override'
#pragma warning(disable: 4481)

// C4668: '__midl' is not defined as a preprocessor macro, replacing with '0' for '#if/#elif'
#pragma warning(disable: 4668)

//  C4820: '_SECURITY_QUALITY_OF_SERVICE' : '2' bytes padding added after data member '_SECURITY_QUALITY_OF_SERVICE::EffectiveOnly'
#pragma warning(disable: 4820)

// TODO(yosi) 2012-04-28 We should enable C4946.
// C4946: reinterpret_cast used between related classes: 'TinyCl::Frame' and 'TinyCl::Generic::XferFrame'
#pragma warning(disable: 4946)

// C6246: Local declaration of 'foo' hides declaration of the same name in outer scope
#pragma warning(disable: 6246)

// Ignore new related warning since we manage memory ourselves. 
// C6211: Leaking memory 'pExt' due to an exception.
#pragma warning(disable: 6211)

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stddef.h> // offsetof

#include "./z_defs.h"
#include "./z_debug.h"
#include "./vcsupport.h"

#endif //!defined(INCLUDE_tinycl_precomp_h)
