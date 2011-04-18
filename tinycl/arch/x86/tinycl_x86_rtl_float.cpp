#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - x86 - Runtime - Float
// tinycl_x86_rtl_float.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_rtl_float.cpp#1 $
//
extern "C"
{

#define declaim(mp_name) \
    double __cdecl mp_name(double); \
    float  __cdecl mp_name##f(float); \

declaim(ceil)
declaim(floor)
declaim(rint)
declaim(truncate)

#pragma function(floor)

#define x86_div(mp_size, mp_mode) \
{ \
     __asm fld      mp_size ptr [esp+4] \
     __asm fnstcw   [esp-2] \
     __asm mov      ax, [esp-2] \
     __asm and      ax, 0f3ffh \
     __asm or       ax, mp_mode * 0x100\
     __asm mov      [esp-4], ax \
     __asm fldcw    [esp-4] \
     __asm frndint \
     __asm fldcw    [esp-2] \
     __asm ret \
}


float __declspec(naked)  __cdecl rintf(float)
{
    __asm
    {
        fld dword ptr [esp+4]
        frndint
        ret
    }
} // rintf


double __declspec(naked) __cdecl rint(double)
{
    __asm
    {
        fld qword ptr [esp+4]
        frndint
        ret
    }
} // rintf

float __declspec(naked) __cdecl ceilf(float)
    { x86_div(dword, 8); }

double __declspec(naked) __cdecl ceil(double)
    { x86_div(qword, 8); }

float __declspec(naked) __cdecl floorf(float)
    { x86_div(dword, 4); }

double __declspec(naked) __cdecl floor(double)
    { x86_div(qword, 4); }

float __declspec(naked) __cdecl truncatef(float)
    { x86_div(dword, 12); }

double __declspec(naked) __cdecl truncate(double)
    { x86_div(qword, 12); }

} // extern "C"
