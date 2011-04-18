#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Code Generator
// tinycl_c_cg.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg.cpp#4 $
//
#include "./tinycl_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

bool CopyInstruction::IsUseless() const
{
    if (GetOutput() == GetSx())
    {
        return true;
    }

    if (Instruction* pPrevI = GetPrev())
    {
        if (pPrevI->Is<CopyInstruction>())
        {
            // Copy %r1 <= %r2
            // Copy %r2 <= %r1
            if (GetOutput() == pPrevI->GetSx() &&
                GetSx()     == pPrevI->GetOutput() )
            {
                return true;
            }

            // Copy %r1 <= %r2
            // Copy %r1 <= %r2
            if (GetOutput() == pPrevI->GetOutput() &&
                GetSx()     == pPrevI->GetSx() )
            {
                return true;
            }
        }
    } // if prev

    return false;
} // CopyInstruction::IsUseless

void FunLit::HtmlPrint(Val stream, bool) const
{
    GetFun()->HtmlPrint(stream);
} // FunLit::HtmlPrint

void Physical::HtmlPrint(Val stream, bool) const
{
    write_string(GetDesc()->m_pszName, stream);
} // Physical::HtmlPrint

void StackSlot::HtmlPrint(Val stream, bool) const
{
    format(stream, "[$sp+~D]", Fixnum::Encode(GetLocation()));
} // StackSlot::HtmlPrint

void ThreadSlot::HtmlPrint(Val stream, bool) const
{
    format(stream, "[$tcb+~D]", Fixnum::Encode(GetLocation()));
} // ThreadSlot::HtmlPrint

void TlvOffset::HtmlPrint(Val stream, bool) const
{
    TlvRecord* p = GetTlvRec()->StaticCast<TlvRecord>();
    format(stream, "(TlvOfs ~S)", p->m_name);
} // TlvOffset::HtmlPrint

void VarHome::HtmlPrint(Val stream, bool) const
{
    format(stream, "(VarHome ~S)", GetVar()->GetName());
} // VarHome::HtmlPrint

} // Compiler

} // TinyCl
