#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Self Tail Call optimization
// tinycl_c_opt_call.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_tail_call.cpp#3 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

namespace Compiler
{

class PassTailCall :
    public   Pass_<PassTailCall, FunctionPass>
{
    public: static const char* GetName_() { return "TailCall"; }

    /// <summary>Entry point</summary>
    protected: void processFunction(Function* pFun)
    {
        CLOG(1, "<h2>Process ~S</h2>~%", pFun);

        if (! isFixedArity(pFun))
        {
            CLOG(1, "Variable arity~%");
            return;
        }

        WorkList_<Instruction> oTailCalls;
        foreach (BBlock::EnumPred, oEnum, pFun->GetExitBB())
        {
            BBlock* const pPredBB = oEnum.Get();
            if (CallI* const pCallI = getSelfTailCall(pPredBB))
            {
                if (pCallI->IsNotinline())
                {
                    CLOG(1, "<li>~S is marked <b>notinline</b></li>",
                        pCallI );
                }
                else
                {
                    oTailCalls.Push(pCallI);
                }
            }
        } // for pred

        if (oTailCalls.IsEmpty())
        {
            CLOG(1, "<li>No self tail calls</li>~%");
            return;
        }

        CLOG(1, "<ol>");
        BBlock* const pHeadBB = makeHeadBB(pFun);
        while (! oTailCalls.IsEmpty())
        {
            Instruction* pCallI = oTailCalls.Pop();
            CLOG(1, "<li>Self-Tail Call ~S<ol>~%", pCallI);
            expandCall(pCallI, pHeadBB);
            CLOG(1, "</ol>~%");
        } // wihle
        CLOG(1, "</ol>");

        pFun->Clean();
    } // processFunction

    // [E]
    public: static void Expand(
        Instruction* const pCallI )
    {
        CLOG(1, "<li>Self-Tail Call ~S<ol>~%", pCallI);
        expandCall(pCallI, makeHeadBB(pCallI->GetBB()->GetFunction()));
        CLOG(1, "</ol>~%");
    } // Expand

    private: static void expandCall(
        Instruction* const pCallI,
        BBlock*      const pHeadBB )
    {
        if (ValuesI* const pValuesI =
                pCallI->GetVy()->GetDefI()->DynamicCast<ValuesI>() )
        {
            Instruction::EnumOperand oEnumO(pValuesI);

            foreach (BBlock::EnumI, oEnumI, pHeadBB)
            {
                PhiI* const pPhiI = oEnumI.Get()->DynamicCast<PhiI>();
                if (NULL == pPhiI)
                {
                    break;
                }

                if (oEnumO.AtEnd())
                {
                    COMPILER_INTERNAL_ERROR();
                    return;
                }

                pPhiI->AddOperand(pCallI->GetBB(), oEnumO.Get());

                oEnumO.Next();
            } // for phi
        }
        else
        {
            uint nNth = 0;
            foreach (BBlock::EnumI, oEnumI, pHeadBB)
            {
                PhiI* const pPhiI = oEnumI.Get()->DynamicCast<PhiI>();
                if (NULL == pPhiI)
                {
                    break;
                }

                Register* pRx = new Register;

                pCallI->GetBB()->InsertBeforeI(
                    new SelectI(tyT, pRx, pCallI->GetVy(), nNth),
                    pCallI );

                pPhiI->AddOperand(pCallI->GetBB(), pRx);

                nNth += 1;
            } // for phi
        } // if

        pCallI->GetBB()->RemoveI(pCallI->GetNext());
        pCallI->GetBB()->ReplaceI(new JumpI(pHeadBB), pCallI);
    } // expandCall

    // [G]
    private: static CallI* getSelfTailCall(BBlock* const pPredBB)
    {
        Instruction* const pRetI = pPredBB->GetLastI()->DynamicCast<RetI>();
        if (NULL == pRetI)
        {
            return NULL;
        }

        Instruction* pPrevI = pPredBB->GetLastI()->GetPrev();
        if (NULL == pPrevI)
        {
            return NULL;
        }

        CallI* pCallI = pPrevI->DynamicCast<CallI>();
        if (NULL == pCallI)
        {
            return NULL;
        }

        if (pCallI->GetOutput() != pRetI->GetSx())
        {
            return NULL;
        }

        if (pCallI->GetSx() != pPredBB->GetFunction())
        {
            return NULL;
        }

        if (NULL == pCallI->GetVy())
        {
            return NULL;
        }

        return pCallI;
    } // getSelfTailCall

    // [I]
    private: static bool isFixedArity(Function* const pFun)
    {
        const Function::Arity* const pArity = pFun->GetArity();
        return pArity->m_iMin == pArity->m_iMax && ! pArity->m_fRest;
    } // isFixedArity

    // [M]
    /// <summary>
    ///   <example>
    ///     BB1:
    ///     PROLOGUE ty %v1 &lt;=
    ///     SELECT ty %r2 &lt;= %v1 0
    ///     SELECT ty %r3 &lt;= %v1 0
    ///     JUMP BB2
    ///
    ///     BB2:
    ///     PHI ty %r4 &lt;= BB1 %r2
    ///     PHI ty %r5 &lt;= BB1 %r3
    ///   </example>
    /// </summary>
    private: static BBlock* makeHeadBB(Function* const pFun)
    {
        CLOG(1, "<li>Make Loop Header<ol>~%");

        Instruction* pRunnerI = pFun->GetPrologueI()->GetNext();
        while (pRunnerI->Is<SelectI>())
        {
            pRunnerI = pRunnerI->GetNext();
        }

        BBlock* const pStartBB = pRunnerI->GetBB();

        BBlock* const pHeadBB = pStartBB->SplitBefore(pRunnerI);

        Instruction* pRefI = pHeadBB->GetFirstI();

        BBlock::EnumI oEnumI(pStartBB);
        oEnumI.Next();
        while (! oEnumI.AtEnd())
        {
            Instruction* const pSelectI = oEnumI.Get()->DynamicCast<SelectI>();
            if (NULL == pSelectI)
            {
                break;
            }

            PhiI* const pPhiI =
                new PhiI(pSelectI->GetTy(), pSelectI->GetRd());

            pHeadBB->InsertBeforeI(pPhiI, pRefI);

            Register* const pRx = new Register(pSelectI->GetRd()->GetVar());

            pSelectI->SetOutput(pRx);
            pPhiI->AddOperand(pStartBB, pRx);

            oEnumI.Next();
        } // for

        CLOG(1, "</ol></li>~%");

        return pHeadBB;
    } // makeHeadBB
}; // PassTailCall

void ExpandSelfTailCall(Instruction* pCallI)
    { return PassTailCall::Expand(pCallI); }

DEFPASS(TailCall)

} // Compiler
} // TinyCl
