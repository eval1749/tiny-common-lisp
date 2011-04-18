//////////////////////////////////////////////////////////////////////////////
//
// Common Definitions
// z_defs.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/win/tinycl_win.h#2 $
//
#if !defined(INCLUDE_tinycl_win_h)
#define INCLUDE_tinycl_win_h

#include "../tinycl.h"

#if !defined(CP_UTF16LE)
    #define CP_UTF16LE  1200
    #define CP_UTF16BE  1201
#endif

class FileHandle
{
    public: HANDLE h;
    public: FileHandle(HANDLE hFile = INVALID_HANDLE_VALUE)
    {
        h = hFile;
    } // FileHandle

    public: ~FileHandle()
    {
        Release();
    } // ~FileHandle

    public: operator HANDLE() const { return h; }

    public: FileHandle& operator =(HANDLE hHandle)
    {
        Attach(hHandle);
        return *this;
    } // operator =

    public: void Attach(HANDLE hHandle)
    {
        Release();
        h  = hHandle;
    } // Attach

    public: HANDLE Detach()
        { HANDLE h1 = h; h = INVALID_HANDLE_VALUE; return h1; }

    public: void Release()
    {
        if (INVALID_HANDLE_VALUE != h)
        {
            ::CloseHandle(h);
            h = INVALID_HANDLE_VALUE;
        }
    } // Release
}; // FileHandle

namespace TinyCl
{

static const Int AllocUnit = 1 << 16;

} // TinyCl

#endif //!defined(INCLUDE_tinycl_win_h)
