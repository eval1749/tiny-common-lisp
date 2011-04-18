#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Windows x86
// arch/x86/tinycl_win_x86.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_win.cpp#10 $
//
//
// See evcl3/arch/x86/x86_ke_mach.cpp
//
#include "./tinycl_x86.h"

#include "../../tinycl_dll_link.h"
#include "../../rtl/tinycl_bignum.h"

#if 0
//  warning C6540: The use of attribute annotations on this function will invalidate all of its existing __declspec annotations
#pragma warning(disable: 6540)
#include <intrin.h>
#pragma warning(default: 6540)
#endif

extern "C" unsigned __int64 __emulu(unsigned int,unsigned int);
#pragma intrinsic(__emulu)

namespace TinyCl
{

using namespace X86;

// To reduce size of function, we should keep following invariatns:
CASSERT(offsetof(Thread, m_fn) < 128);
CASSERT(offsetof(Thread, m_fp) < 128);
CASSERT(offsetof(Thread, m_n) < 128);
CASSERT(offsetof(Thread, mv_value) + sizeof(Val) * 0 < 128);
CASSERT(offsetof(Thread, mv_value) + sizeof(Val) * 1 < 128);
CASSERT(offsetof(Thread, mv_value) + sizeof(Val) * 2 < 128);
CASSERT(offsetof(Thread, mv_value) + sizeof(Val) * 3 < 128);
CASSERT(offsetof(Thread, mv_value) + sizeof(Val) * 4 < 128);

// For __asm
enum Constant
{
    Tag_Fixnum      = Arch::Tag_Fixnum,
    Tag_Fixnum1     = Arch::Tag_Fixnum1,
    Tag_Function    = Arch::Tag_FunObj,

    FrameType_FromForeign   = Frame::Type_FromForeign,
}; // Constant

typedef Thread Context;

namespace Internal
{

// Mul -- a x b => double bigit
DoubleBigit DoubleBigit::Mul(Bigit a, Bigit b)
{
    uint64 const u64 = __emulu(a, b);
    return DoubleBigit(u64);
} // DoubleBigit::Mul

// Shl -- arithmetic shift left
DoubleBigit DoubleBigit::Shl(Bigit a, uint k)
{
    uint64 const u64 = static_cast<uint64>(a) << k;
    return DoubleBigit(u64);
} // DoubleBigit::Shl

} // Internal


//////////////////////////////////////////////////////////////////////
//
//  CallLisp
//      ecx = Context
//      Context->m_n           = number of arguments
//      Context->m_fn          = callee (function)
//      Context->mv_value[0]    = arg_0
//      Context->mv_value[1]    = arg_1
//      ...
//      Context->mv_value[n-1]  = arg_n-1
//
Val __declspec(naked) __fastcall
CallLisp(Thread*)
{
    struct LocalFrame
    {
        int32   m_oFromForeign_m_pOuter;
        int32   m_oFromForeign_m_eFrame;
        int32   m_ebx;
        int32   m_ebp;
        int32   m_esi;
        int32   m_edi;
    }; // LocalFrame
    
    CASSERT(sizeof(LocalFrame) == 16 + sizeof(FromForeignFrame));

    __asm
    {
        // Allocate save area
        sub     esp, SIZE LocalFrame

        // Save caller save registers for C
        mov     [esp] LocalFrame.m_ebx,  ebx
        mov     [esp] LocalFrame.m_ebp, ebp
        mov     [esp] LocalFrame.m_esi, esi
        mov     [esp] LocalFrame.m_edi, edi

        // ebp <- pContext
        mov     ebp, ecx

        // Make FromForeign Frame
        mov     eax, [ebp] Context.m_fp
        mov     [ebp] Context.m_fp, esp
        mov     [esp] LocalFrame.m_oFromForeign_m_pOuter, eax
        mov     dword ptr [esp] LocalFrame.m_oFromForeign_m_eFrame, FrameType_FromForeign

        // Set callee entry point
        mov     eax, [ebp] Context.m_fn
        add     eax, SIZE FunObj - Tag_Function
        mov     [esp-4], eax

        // Set arguments
        mov     ecx, [ebp] Context.m_n
        mov     eax, [ebp] Context.mv_value[0*4]
        mov     edx, [ebp] Context.mv_value[1*4]
        mov     ebx, [ebp] Context.mv_value[2*4]
        mov     esi, [ebp] Context.mv_value[3*4]
        mov     edi, [ebp] Context.mv_value[4*4]

        // Call lisp function
        call    dword ptr [esp-4]

        // Save primary value
        mov     [ebp] Context.mv_value[0*4], eax

        // Set number of values
        mov     eax, Tag_Fixnum1
        cmovnc  ecx, eax
        mov     [ebp] Context.m_n, ecx

        // Set values to thread
        mov     [ebp] Context.mv_value[1*4], edx
        mov     [ebp] Context.mv_value[2*4], ebx
        mov     [ebp] Context.mv_value[3*4], esi
        mov     [ebp] Context.mv_value[4*4], edi

        // Restore m_fp
        mov     eax, [esp] LocalFrame.m_oFromForeign_m_pOuter
        mov     [ebp] Context.m_fp, eax

        // Restore primary value
        mov     eax, [ebp] Context.mv_value[0*4]

        // Restore caller save registers
        mov     ebx, [esp] LocalFrame.m_ebx
        mov     ebp, [esp] LocalFrame.m_ebp
        mov     esi, [esp] LocalFrame.m_esi
        mov     edi, [esp] LocalFrame.m_edi

        add     esp, SIZE LocalFrame
        ret
    } // __asm
} // CallLisp

//////////////////////////////////////////////////////////////////////
//
// DllLinkStab
//
//  [1] Save argments into thread.mv_value
//  [2] Compute address of Link Vector entry
//  [3] Call C_resolver(thread, entry)
//  [4] Restore arguments
//  [5] Transfer control
//
//          +--------------+
//   esp+0  |     ecx      |
//          +--------------+
//   esp+4  |     edx      |
//          +--------------+
//   esp+8  |    callee    |    esp-4
//          +--------------+
//   esp+12 | RA of caller |    esp+0
//          +--------------+
extern "C" Val __declspec(naked) __fastcall
DllLinkStab()
{
    __asm
    {
        // [1] Save C/C++ arguments
        sub esp, 12
        mov [esp],    ecx   // the first argument
        mov [esp+4],  edx   // the second argument

        // [2] Compute address of Link Vector entry
        mov edx, [esp+12]   // eax <- return address of caller
        mov edx, [edx-4]    // eax <- ea of CALL [disp32] instruction

        // [3] Call DllResolve
        mov ecx, ebp
        call DllResolve
        mov [esp+8], eax    // save function entry point

        // [4] Restore arguments
        mov     edx, [esp+4]    // the first argument
        mov     ecx, [esp]      // the second argument
        add     esp, 12

        // [5] Transfer control
        jmp dword ptr [esp-4]   // transfer control to function
    } // __asm
} // DllLinkStab

void __declspec(noreturn) __declspec(naked) __fastcall
Generic::XferFrame::Transfer(Thread*)
{
    __asm
    {
        // ecx = frame
        // edx = thread
        mov ebp, edx    // ebp <- pThread
        mov esp, [ecx] ExitPointFrame.m_sp

        // Compute target address
        mov eax, [ecx] XferFrame.m_nIp
        add eax, [ecx] ExitPointFrame.m_fn
        add eax, SIZE FunObj - Tag_Function;
        mov [esp-4], eax

        // Load values
        mov ecx, [ebp] Thread.m_n
        mov eax, [ebp] Thread.mv_value[0*4]
        mov edx, [ebp] Thread.mv_value[1*4]
        mov ebx, [ebp] Thread.mv_value[2*4]
        mov esi, [ebp] Thread.mv_value[3*4]
        mov edi, [ebp] Thread.mv_value[4*4]
        stc

        // Transfer control
        jmp dword ptr [esp-4]
    } // _asm
} // Generic::XferFrame::Transfer

void __declspec(noreturn) __declspec(naked) __fastcall
Generic::TagsFrame::Transfer(Thread*, Tag*)
{
    __asm
    {
        //  ecx = frame
        //  edx = thread
        mov eax, [esp+4]    // eax <- pTag
        mov ebp, edx        // ebp <- pThread
        mov esp, [ecx] ExitPointFrame.m_sp

        // Compute target address
        mov eax, [eax] Tag.m_nIp
        add eax, [ecx] ExitPointFrame.m_fn
        add eax, SIZE FunObj - Tag_Function
        jmp eax
    }
} // Generic::TagsFrame::Transfer

void __declspec(noreturn) __declspec(naked) __fastcall
Generic::TryCatchFrame::Transfer(Thread*, Catch*, Val)
{
    __asm
    {
        //  ecx = frame
        //  edx = thread
        mov ebx, [esp+4]    // eax <- pCatch
        mov ebp, edx        // ebp <- pThread
        mov eax, [esp+8]    // eax <- cond
        mov esp, [ecx] ExitPointFrame.m_sp

        // Compute target address
        mov ebx, [ebx] Catch.m_nIp
        add ebx, [ecx] ExitPointFrame.m_fn
        add ebx, SIZE FunObj - Tag_Function
        mov ecx, one
        jmp ebx
    }
} // Generic::TryCatchFrame::Transfer

} // TinyCl
