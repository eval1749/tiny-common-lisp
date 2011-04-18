//////////////////////////////////////////////////////////////////////////////
//
// Common Definitions
// z_defs.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/z_defs.h#1 $
//
#if !defined(INCLUDE_z_defs_h)
#define INCLUDE_z_defs_h

// precomp.h must include <stddef.h> for offsetof

typedef wchar_t char16;

typedef char  int8;
typedef short int16;
typedef int   int32;

typedef unsigned char  uint8;
typedef unsigned short uint16;
typedef unsigned int   uint32;
typedef unsigned int   uint;

typedef __w64 int          Int;
typedef __w64 unsigned int UInt;

#define foreach(mp_enum, mp_var, mp_arg) \
    for (mp_enum mp_var(mp_arg); ! (mp_var).AtEnd(); (mp_var).Next())

#define lengthof(a) ( sizeof(a) / sizeof(*(a)) )
#define override virtual
#define unless(mp_expr) if (! (mp_expr) )
#define when(mp_expr) if ((mp_expr))

#define NoReturn __declspec(noreturn)

template<typename T>
void swap(T& rx, T& ry)
{
    T temp = rx;
    rx = ry;
    ry = temp;
} // swap

#endif //!defined(INCLUDE_z_defs_h)
