#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Expand Values
// tinycl_x86_c_values.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_arity.cpp#2 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

namespace Compiler
{

class PassArity :
    public   Pass_<PassArity, ModulePass>
{
    public: static const char* GetName_() { return "Arity"; }

    // Entry point
    protected: override void processModule(Module* pM)
    {
        CLOG(1, "<ol>~%");
        foreach (Module::EnumFunction, oEnum, pM)
        {
            Function* pFun = oEnum.Get();

            CLOG(2, "<li>~S - ", pFun);

            if (isFixedArity(pFun))
            {
                pFun->GetPrologueI()->GetOperandBox(0)->
                    Replace(Literal::New(nil));
            }

            CLOG(2, "</li>~%");
        } // for each fun
        CLOG(1, "</ol>~%");
    } // Run

    // [I]
    private: static bool isFixedArity(Function* pCallee)
    {
        if (pCallee->HasUseSite())
        {
            // pCallee is called with unknown arguments.
            CLOG(2, "called globally");
            return false;
        }

        int iMin = pCallee->GetArity()->m_iMin;
        int iMax = pCallee->GetArity()->m_iMax;
        if (iMin != iMax)
        {
            // pCallee has optional parameters.
            CLOG(2, "optional parameters");
            return false;
        }

        if (nil != pCallee->GetPrologueI()->GetLy()) 
        {
            // pCallee has rest parameter.
            CLOG(2, "rest parameter");
            return false;
        }

        foreach (Function::EnumCall, oEnum, pCallee)
        {
            Instruction* pCallI = oEnum.GetI();
            Values* pVy = pCallI->GetVy();
            if (! pVy->GetDefI()->Is<ValuesI>())
            {
                // FIXME 2007-12-02 yosi@msn.com NYI: we should check
                // CALL instruction with known callee.

                // pCallee is called with unknown number of arguments.
                CLOG(2, "unknown number of arguments");
                return false;
            }
        } // for each call

        CLOG(2, "fixed arguments");
        return true;
    } // isFixedArity
}; /// PassArity

DEFPASS(Arity)

} // Compiler

} // TinyCl
