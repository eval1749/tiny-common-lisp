#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - debugger
// kernel_debugger.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/regex/regex_debug.cpp#1 $
//
#include <stdlib.h> // EXIT_FAILURE

namespace Debugger
{

static HANDLE g_hConOut = INVALID_HANDLE_VALUE;
static BOOL   g_fConOut;

static BOOL
get_conout()
{
    if (! g_fConOut)
    {
        g_hConOut = ::CreateFileW(
            L"CONOUT$",
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_WRITE | FILE_SHARE_READ,
            NULL,
            OPEN_EXISTING,
            0,
            NULL );
        g_fConOut = TRUE;
    } // if

    return g_hConOut != INVALID_HANDLE_VALUE;
} // get_conout


#if defined(_DEBUG)
//////////////////////////////////////////////////////////////////////
//
// Assert
//
void
Assert(
    LPCSTR  pszFileName,
    int     iLineNum,
    LPCSTR  pszFunction,
    LPCSTR  pszExpr,
    bool    fExpr )
{
    if (fExpr)
    {
        return;
    }

    Fail(
        L"Assertion failed %hs\n"
        L"FileName: %hs\n"
        L"Line:     %d\n"
        L"Function: %hs\n",
        pszExpr,
        pszFileName,
        iLineNum,
        pszFunction );
} // Assert

// Can_Not_Happen
void
Can_Not_Happen(
    LPCSTR  pszFileName,
    int     iLineNum,
    LPCSTR  pszFunction )
{
    Fail(
        L"Can't Happen!!\n"
        L"FileName: %hs\n"
        L"Line:     %d\n"
        L"Function: %hs\n",
        pszFileName,
        iLineNum,
        pszFunction );
} // Can_Not_happen

#else
// Can_Not_Happen
void
Can_Not_Happen(
    LPCSTR,
    int,
    LPCSTR)
{
    __debugbreak();
} // Can_Not_happen
#endif // defined(_DEBUG)


//////////////////////////////////////////////////////////////////////
//
// Debugger - Fail
//
void
Fail(LPCWSTR pwszFormat, ...)
{
    va_list args;
    va_start(args, pwszFormat);

    WCHAR wsz[1024];
        ::wvsprintfW(wsz, pwszFormat, args);

    ::OutputDebugString(wsz);

    if (get_conout())
    {
        DWORD cchWritten;
        ::WriteConsoleW(
            g_hConOut,
            wsz,
            ::lstrlenW(wsz),
            &cchWritten,
            NULL );
    } // if

    if (::IsDebuggerPresent())
    {
        __debugbreak();
    }

    ::MessageBoxW(
        NULL,
        wsz, L"Evita Common Lisp",
        MB_ICONERROR | MB_TASKMODAL );

    ::ExitProcess(EXIT_FAILURE);
} // Fail


//////////////////////////////////////////////////////////////////////
//
// Debugger - Printf
//
void
Printf(LPCWSTR pwszFormat, ...)
{
    va_list args;
    va_start(args, pwszFormat);

    WCHAR wszString[1024];
    ::wvsprintfW(wszString, pwszFormat, args);

    ::OutputDebugString(wszString);

    #if 0
    {
        if (get_conout())
        {
            DWORD cchWritten;
            ::WriteConsoleW(
                g_hConOut,
                wszString,
                ::lstrlenW(wszString),
                &cchWritten,
                NULL );
        }
    }
    #endif
} // Printf


void
PrintHeader(const char* pszFname)
{
    char sz[1024];
    ::wsprintfA(sz, "%d %s: ", ::GetTickCount(), pszFname);
    ::OutputDebugStringA(sz);
} // PrintHeader

} // Debugger
