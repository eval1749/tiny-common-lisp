#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Optimize - Typep
// tinycl_c_opt_typep.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_typep.cpp#2 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

/// <summary>
///   Optimization pass for simplifies TYPEP instruction.
/// </summary>
class PassTypep :
    public Pass_<PassTypep, FunctionPass>
{
    public: static const char* GetName_() { return "Typep"; }

    private: WorkList_<Instruction> m_oTypeps;

    /// <summary>
    ///  Pass entry point.
    /// </summary>
    protected: override void processFunction(Function* pFun)
    {
        CLOG(1, "<h3>Process ~S</h3><ol>~%", pFun);
        foreach (Function::EnumBBlock, oEnum, pFun)
        {

            BBlock* pBB = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* pI = oEnum.Get();
                if (pI->Is<TypepI>())
                {
                    m_oTypeps.Push(pI);
                }
            } // for insn
        } // for bb

        while (! m_oTypeps.IsEmpty())
        {
            expandTypep(static_cast<TypepI*>(m_oTypeps.Pop()));
        } // while

        CLOG(1, "</ol>~%");
    } // processFunction

    private: void expandTypep(TypepI* pTypepI)
    {
        ASSERT(NULL != pTypepI);
    } // expandTypep
}; // PassTypep

DEFPASS(Typep)

} // Compiler
} // TinyCl
