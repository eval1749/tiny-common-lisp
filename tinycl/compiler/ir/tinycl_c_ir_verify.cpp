#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - IR Verifier
// tinycl_c_ir_verify.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_verify.cpp#4 $
//
#include "../tinycl_c_defs.h"
#include "../cg/tinycl_c_cg.h"
#include "./tinycl_c_ir_loop.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

// [B]
bool BBlock::Verify() const
{
    bool fValid = true;
    foreach (BBlock::EnumI, oEnum, this)
    {
        if (! oEnum.Get()->Verify())
        {
            fValid = false;
        }
    } // for each BBlock
    return fValid;
} // BBlock::Verify

// [E]
bool EntryI::Verify() const
{
    if (NULL != GetPrev())
    {
        CLOG(1, "<li>~S must be the first instruction.</li>", this);
        return false;
    }

    return true;
} // EntryI::Verify

// [F]
bool Function::Verify() const
{
    CLOG(1, "<li>Verify ~S<ol>", this);

    bool fValid = true;
    foreach (Function::EnumBBlock, oEnum, this)
    {
        if (! oEnum.Get()->Verify())
        {
            fValid = false;
        }
    } // for each function

    CLOG(1, "</ol></li>~%");
    return fValid;
} // Function::Verify

// [I]
bool Instruction::Verify() const
{
    if (NULL == GetNext())
    {
        CLOG(1, "<li>~S can't be the last instruction.</li>", this);
        return false;
    }
    return true;
} // Instruction::Verify

// [L]
bool LastInstruction::Verify() const
{
    if (NULL != GetNext())
    {
        CLOG(1, "<li>~S must be the last instruction.</li>", this);
        return false;
    }

    return true;
} // LastInstruction::Verify

// [M]
bool Module::Verify() const
{
    CLOG(1, "<hr/><h2 id='M.Verify'>IR Verification ~S</h2><ol>", this);

    bool fValid = true;
    foreach (Module::EnumFunction, oEnum, this)
    {
        if (! oEnum.Get()->Verify())
        {
            fValid = false;
        }
    } // for each function

    CLOG(1, "</ol>");
    return fValid;
} // Module::Verify

bool PhiI::Verify() const
{
    if (! Super::Verify())
    {
        return false;
    }

    if (Instruction* const pPrevI = GetPrev())
    {
        if (! pPrevI->Is<PhiI>()) {
            CLOG(1, "<li>~S must be start of bblock.</li>", this);
            return false;
        }
    }

    uint cPreds = 0;
    foreach (BBlock::EnumPred, oEnum, GetBBlock())
    {
        BBlock* const pPredBB = oEnum.Get();
        if (NULL == FindOperandBox(pPredBB))
        {
            CLOG(1, "<li>~S has no oeprand for ~S.</li>", this, pPredBB);
            return false;
        }

        cPreds += 1;
    } // for each pred

    if (CountOperands() != cPreds)
    {
        CLOG(1, "<li>~S has extra operand.</li>", this);
        return false;
    }

    return true;
} // PhiI::Verify

} // Compiler
} // TinyCl
