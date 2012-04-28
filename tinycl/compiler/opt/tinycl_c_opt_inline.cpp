#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Inline Expansion
// tinycl_c_opt_inline.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_inline.cpp#4 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

namespace Compiler
{

void ExpandSelfTailCall(Instruction*);

/// <summary>
///   Function integrator
/// </summary>
class FunctionIntegrator
{
    private: CallI* m_pCallI;

    private: Function* m_pCallee;
    private: Function* m_pCaller;

    private: BBlock* m_pCallBB;
    private: BBlock* m_pEntryBB;
    private: BBlock* m_pExitBB;
    private: BBlock* m_pJoinBB;

    private: PhiI* m_pPhiI;

    public: static void Run(CallI* const pCallI)
    {
        CLOG_SECTION(1, "<h2>Expand ~S with ~S</h2>",
            pCallI, pCallI->GetFrameReg() );

        FunctionIntegrator oPass(pCallI);
        oPass.run();
    } // Run

    private: FunctionIntegrator(
        CallI* const pCallI ) :
            m_pCallI(pCallI)
    {
        m_pCallBB  = pCallI->GetBB();
        m_pJoinBB  = m_pCallBB->SplitBefore(pCallI);
        m_pCaller  = m_pCallBB->GetFunction();

        m_pCallee  = pCallI->GetSx()->StaticCast<Function>();
        m_pEntryBB = m_pCallee->GetEntryBB();
        m_pExitBB  = m_pCallee->GetExitBB();
    } // FunctionIntegrator

    // [A]
    private: static Operand* adjustSx(
        Output*      const pOutput,
        Operand*     const pSx,
        Instruction* const pRefI )
    {
        if (pOutput->Is<Values>())
        {
            if (! pSx->Is<Values>())
            {
                Values* const pVx = new Values;

                pRefI->GetBB()->InsertBeforeI(
                    new ValuesI(pOutput->GetTy(), pVx, pSx),
                    pRefI );

                return pVx;
            }
        }
        else
        {
            if (Values* const pVx = pSx->DynamicCast<Values>())
            {
                Register* const pRx = new Register;

                pRefI->GetBB()->InsertBeforeI(
                    new SelectI(pOutput->GetTy(), pRx, pVx, 0),
                    pRefI );

                return pRx;
            }
        } // if

        return pSx;
    } // adjustSx

    // [G]
    private: static Variable* getUpVar(Register* const pRx)
    {
        Instruction* const pLoadI =
            pRx->GetDefI()->StaticCast<LoadI>();

        Instruction* const pSlotI =
            pLoadI->GetRx()->GetDefI()->StaticCast<SlotI>();

        Instruction* const pUpVarDefI =
            pSlotI->GetQz()->GetDefI()->StaticCast<UpVarDefI>();

        Variable* const pVar =
            pUpVarDefI->GetSx()->StaticCast<Variable>();

        return pVar;
    } // getUpVar

    // [I]
    private: static void insertClose(
        FrameReg*    const pFr,
        Instruction* const pRefI )
    {
        CLOG_SECTION(4, "insert Close for ~S", pFr);

        Instruction* const pOpenI = pFr->GetDefI();
        if (NULL == pOpenI)
        {
            CLOG(4, "<li>~S is removed</li>", pFr);
            return;
        }

        pRefI->GetBB()->InsertBeforeI(new CloseI(pFr), pRefI);

        if (pOpenI->Is<OpenFinallyI>())
        {
            Values* const pVx = new Values;
            pRefI->GetBB()->InsertBeforeI(new ValuesI(pVx), pRefI);

            pRefI->GetBB()->InsertBeforeI(
                new CallI(tyVoid, Void, pOpenI->GetSx(), pVx),
                pRefI );
        }  // if OpenFinallyI
    } // insertClose

    // [R]

    private: void rewriteBody()
    {
        CLOG_SECTION(2, "Rewrite Body");
        BBlock* pRunnerBB = m_pEntryBB->GetNext();

        while (pRunnerBB != m_pExitBB)
        {
            BBlock* const pNextBB = pRunnerBB->GetNext();

            static_cast<Function::LayoutList*>(m_pCaller)->InsertBefore(
                static_cast<Function::LayoutList*>(m_pCallee)->Delete(
                    pRunnerBB ),
                m_pJoinBB );

            Instruction* const pLastI = pRunnerBB->GetLastI();

            if (RetI* const pRetI = pLastI->DynamicCast<RetI>())
            {
                rewriteRet(pRetI);
            }
            else if (UnreachableI* const pUnreachableI =
                            pLastI->DynamicCast<UnreachableI>() )
            {
                rewriteUnreachable(pUnreachableI);
            }

            pRunnerBB = pNextBB;
        } // while
    } // rewriteBody

    private: void rewritePrologue(Instruction* const pCallI)
    {
        CLOG_SECTION(3, "Rewrite Prologue");

        Instruction* const pPrologueI = m_pCallee->GetPrologueI();
        Values* const pVx = pPrologueI->GetVd();
        if (NULL == pVx)
        {
            CLOG(3, "<li>No parameter</li>");
            return;
        }

        pVx->ReplaceAll(pCallI->GetVy());
        pPrologueI->GetBB()->RemoveI(pPrologueI);
    } // rewritePrologue

    // OpenBB:
    //  OpenTags [Tagbody] <=
    //  TagDef %r1 <= [Tagbody] TargetBB exit
    //  VarDef %r2 <= [Var] %r1
    //
    // NonlocalBB:
    //  Close [Tagbody]
    //
    private: void rewriteGo(
        Instruction* const pGoI,
        Instruction* const pUnreachableI )
    {
        CLOG_SECTION(3, "rewrite ~S", pGoI);

        Variable* const pVar = getUpVar(pGoI->GetRx());
        if (pVar->GetOwner() == m_pCaller)
        {
            Instruction* const pVarDefI =
                pVar->GetDefI()->StaticCast<VarDefI>();

            Instruction* const pTagDefI =
                pVarDefI->GetRy()->GetDefI();

            BBlock* const pTargetBB =
                pTagDefI->GetSy()->StaticCast<Label>()->GetBB();

            unwindCalleeFrames(pGoI);

            unwindCallerFrames(
                pGoI,
                pTagDefI->GetOutput()->StaticCast<FrameReg>() );

            pGoI->GetBB()->RemoveI(pUnreachableI);
            pGoI->GetBB()->ReplaceI(new JumpI(pTargetBB), pGoI);
        } // if
    } // rewriteGo

    private: void rewriteRet(Instruction* const pRetI)
    {
        CLOG_SECTION(3, "rewrite ~S", pRetI);

        if (Instruction* const pPrevI = pRetI->GetPrev())
        {
            if (CallI* pCallI = pPrevI->DynamicCast<CallI>())
            {
                if (pRetI->GetSx() == pCallI->GetOutput() &&
                    pCallI->GetSx() == m_pCaller )
                {
                    ExpandSelfTailCall(pCallI);
                    return;
                }
            }
        }

        if (NULL != m_pPhiI)
        {
            m_pPhiI->AddOperand(
                pRetI->GetBB(),
                adjustSx(m_pPhiI->GetOutput(), pRetI->GetSx(), pRetI) );
        }

        pRetI->GetBB()->ReplaceI(new JumpI(m_pJoinBB), pRetI);
    } // rewriteRet

    //  OpenBlock [Block] <= NonlocalBB
    //  Frame %f1 <= [Block]
    //  VarDef %r2 <= [Var] %f1
    // NonlocalBB:
    //  Nonlocal %v2 <=
    //  Jump JoinBB
    // JoinBB:
    //  Phi %v3 <=
    //
    //  Callee:
    //  UpVarDef %q1 <= [Var]
    //  Slot %q2 <= closed-cell value %q1
    //  Load %r3 <= %q2
    //  ReturnFrom %r3 %v4
    private: void rewriteReturnFrom(
        Instruction* const pReturnI,
        Instruction* const pUnreachableI )
    {
        CLOG_SECTION(3, "rewrite ~S", pReturnI);

        Variable* const pVar = getUpVar(pReturnI->GetRx());
        if (pVar->GetOwner() == m_pCaller)
        {
            Instruction* const pVarDefI =
                pVar->GetDefI()->StaticCast<VarDefI>();

            Instruction* const pFrameI =
                pVarDefI->GetRy()->GetDefI()->StaticCast<FrameI>();

            FrameReg* const pFr =
                pFrameI->GetSx()->StaticCast<FrameReg>();

            Instruction* const pOpenI =
                pFr->GetDefI()->StaticCast<OpenBlockI>();

            BBlock* const pNonlocalBB =
                pOpenI->GetSy()->StaticCast<Label>()->GetBB();

            BBlock* const pTargetBB =
                pNonlocalBB->GetLastI()->GetSx()->StaticCast<Label>()->GetBB();

            unwindCalleeFrames(pReturnI);
            unwindCallerFrames(pReturnI, pFr->GetOuter());

            if (Instruction* const pNonlocalI =
                    pNonlocalBB->GetFirstI()->DynamicCast<NonlocalI>() )
            {
                if (PhiI* const pPhiI =
                        pTargetBB->GetFirstI()->DynamicCast<PhiI>() )
                {
                    pPhiI->AddOperand(pReturnI->GetBB(), pReturnI->GetSy());
                }
            }
            else
            {
                // Nonlocal exit point is merged or return-from doesn't
                // return usefull value.
            } // if

            pReturnI->GetBB()->RemoveI(pUnreachableI);

            pReturnI->GetBB()->ReplaceI(
                new JumpI(pTargetBB),
                pReturnI );
        } // if
    } // rewriteReturnFrom

    private: void rewriteUnreachable(Instruction* pUnreachableI)
    {
        CLOG_SECTION(3, "rewrite ~S", pUnreachableI);

        Instruction* pPrevI = pUnreachableI->GetPrev();
        while (NULL != pPrevI)
        {
            if (! pPrevI->Is<UseI>())
            {
                break;
            }

            pPrevI = pPrevI->GetPrev();
        } // while

        if (NULL == pPrevI)
        {
            return;
        }

        if (pPrevI->Is<GoI>())
        {
            rewriteGo(pPrevI, pUnreachableI);
        }
        else if (pPrevI->Is<ReturnFromI>())
        {
            rewriteReturnFrom(pPrevI, pUnreachableI);
        }
    } // rewriteUnreachable

    // Move UpVarDef
    // Note: We need UpVarDef for GO and RETURNFROM.
    private: void rewriteUpVarDefs()
    {
        CLOG_SECTION(2, "Rewrite UpVarDefs");

        BBlock::EnumI oEnum(m_pEntryBB);
        while (! oEnum.AtEnd())
        {
            Instruction* const pI = oEnum.Get();
            oEnum.Next();

            if (UpVarDefI* pUpVarDefI = pI->DynamicCast<UpVarDefI>())
            {
                Variable* const pVar =
                    pUpVarDefI->GetSx()->StaticCast<Variable>();

                CLOG_SECTION(2, "rewrite ~S", pUpVarDefI);

                if (pVar->GetOwner() == m_pCaller)
                {
                    pUpVarDefI->GetOutput()->StaticCast<SsaOutput>()->
                        ReplaceAll(pVar->GetDefI()->GetOutput());

                    m_pEntryBB->RemoveI(pUpVarDefI);
                }
                else
                {
                    pUpVarDefI->GetBB()->RemoveI(pUpVarDefI);

                    m_pCaller->GetEntryBB()->InsertBeforeI(
                        pUpVarDefI,
                        m_pCaller->GetEntryBB()->GetLastI() );
                }
            } // if UpVarDef
        } // while
        ASSERT(m_pCallee->m_oUpVarDefs.IsEmpty());
    } // rewriteUpVarDefs

    private: void run()
    {
        setupPhi(m_pCallI);

        m_pCallBB->ReplaceI(
            new JumpI(m_pCallee->GetStartBB()),
            m_pCallBB->GetLastI() );

        ASSERT(m_pCallee->m_oUsers.IsEmpty());

        if (! m_pCallee->m_oFrameRegs.IsEmpty())
        {
            CLOG_SECTION(2, "Move frame registers");

            m_pCallee->m_oFrameRegs.GetFirst()->SetOuter(
                m_pCallI->GetFrameReg() );

            while (FrameReg* const p = m_pCallee->m_oFrameRegs.Pop())
            {
                CLOG(2, "<li>~S</li>", p);
                m_pCaller->m_oFrameRegs.Append(p);
            } // while
        }

        {
            CLOG_SECTION(2, "Move VarDefs");
            while (OperandBox* const pBox = m_pCallee->m_oVarDefs.Pop())
            {
                CLOG(2, "<li>~S</li>", pBox->GetI());
                m_pCaller->m_oVarDefs.Append(pBox);
            } // while
        }

        {
            CLOG_SECTION(2, "update Cg edges");
            while (CgEdge* const p = m_pCallee->m_oOutEdges.Pop())
            {
                if (CgEdge* const pPresent =
                        p->GetTo()->FindEdgeFrom(m_pCaller) )
                {
                    pPresent->m_cUsers += 1;
                    CLOG(2, "<li>one more ~S</li>", pPresent);
                }
                else
                {
                    p->SetFrom(m_pCaller);
                    m_pCaller->m_oOutEdges.Append(p);
                    CLOG(2, "<li>add edge ~S</li>", p);
                }
            } // while
        }

        {
            CLOG_SECTION(2, "update Cfg edges");

            BBlock::EnumInEdge oEnum(m_pExitBB);
            while (! oEnum.AtEnd())
            {
                CfgEdge* const pEdge = oEnum.Get();
                oEnum.Next();

                if (pEdge->GetFrom() != m_pEntryBB)
                {
                    pEdge->GetFrom()->RedirectEdgeTo(
                        m_pCaller->GetExitBB(),
                        m_pExitBB );
                }
            } // while
        }

        rewriteBody();
        rewriteUpVarDefs();
        rewritePrologue(m_pCallI);

        m_pCallee->RemoveBBlock(m_pEntryBB);

        m_pCallI->GetBB()->RemoveI(m_pCallI);
        ASSERT(m_pCallee->m_oCalls.IsEmpty());

        //m_pCallee->GetModule()->Delete(m_pCallee);
    } // run

    // [S]
    private: void setupPhi(Instruction* const pCallI)
    {
        m_pPhiI = NULL;

        Output* pOutput = NULL;
        if (Register* pRd = pCallI->GetRd())
        {
            pOutput = new Register(pRd->GetVar());
            pRd->ReplaceAll(pOutput);
        }
        else if (Values* pVd = pCallI->GetVd())
        {
            pOutput = new Values;
            pVd->ReplaceAll(pOutput);
        }
        else if (pCallI->GetOutput() == Void)
        {
            return;
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
            return;
        }

        m_pPhiI = new PhiI(pCallI->GetTy(), pOutput);
        m_pJoinBB->InsertBeforeI(m_pPhiI, m_pJoinBB->GetFirstI());
    } // setupPhi

    // [U]
    private: void unwindCalleeFrames(Instruction* const pRefI)
    {
        CLOG_SECTION(3, "<li>unwind callee frames");
        for (;;)
        {
            UseI* pUseI = pRefI->GetNext()->DynamicCast<UseI>();
            if (NULL == pUseI)
            {
                break;
            }

            if (FrameReg* pFr = pUseI->GetSx()->DynamicCast<FrameReg>())
            {
                insertClose(pFr, pRefI);
            }
            
            pRefI->GetBB()->RemoveI(pUseI);
        } // for
    } // unwindCalleeFrames

    private: void unwindCallerFrames(
        Instruction* const pRefI,
        FrameReg*    const pFrEnd )
    {
        CLOG_SECTION(3, "unwind caller frames to ~S", pFrEnd);

        for (
            FrameReg* pRunner = m_pCallI->GetFrameReg();
            pRunner != pFrEnd;
            pRunner = pRunner->GetOuter() )
        {
            if (pRunner->GetDefI()->GetBB()->GetFunction() == m_pCaller)
            {
                insertClose(pRunner, pRefI);
            }
        } // for
    } // unwindCallerFrames
}; // FunctionIntegrator

/// <summary>
///   Represents "Inline" pass.
/// </summary>
class PassInline :
    public   Pass_<PassInline, ModulePass>
{
    public: static const char* GetName_() { return "Inline"; }

    private: WorkList_<Instruction> m_oCalls;

    /// <summary>Entry point</summary>
    protected: virtual void processModule(Module* pM) override
    {
        uint nCount = 0;
        for (;;)
        {
            nCount += 1;

            {
                CLOG(1, "<h2>Planning[~D]</h2>", nCount);
                foreach (Module::EnumFunction, oEnum, pM)
                {
                    Function* const pFun = oEnum.Get();
                    planInline(pFun);
                } // for each fun
            }

            if (m_oCalls.IsEmpty())
            {
                break;
            }

            while (! m_oCalls.IsEmpty())
            {
                CallI* pCallI = m_oCalls.Pop()->StaticCast<CallI>();
                FunctionIntegrator::Run(pCallI);
            } // while
        } // for
    } // Run

    // [C]
    private: static bool canInline(Function* const pCallee)
    {
        if (pCallee->GetFlavor() == Function::Flavor_Toplevel)
        {
            CLOG(2, "<li>~S is toplevel</li>", pCallee);
            return false;
        } // switch flavor

        // Note: Function::HasUseSite doesn't consider OpenFinally.
        #if 0
        if (pCallee->HasUseSite())
        {
            CLOG(2, "<li>~S is used as value</li>", pCallee);
            return false;
        }
        #else
        {
            Function::EnumUser oEnum(pCallee);
            if (! oEnum.AtEnd())
            {
                CLOG(2, "<li>~S is used as value</li>", pCallee);
                return false;
            }
        }
        #endif

        {
            Function::EnumCall oEnum(pCallee);
            if (oEnum.AtEnd())
            {
                if (pCallee->GetFlavor() == Function::Flavor_Finally)
                {
                    CLOG(2, "<li>~S is finally.</li>", pCallee);
                }
                else
                {
                    CLOG(2, "<li>~S isn't called.</li>", pCallee);
                }
                return false;
            }

            oEnum.Next();

            if (! oEnum.AtEnd())
            {
                CLOG(2, "<li>~S is called more than once.</li>", pCallee);
                return false;
            }
        }

        return true;
    } // canInline

    // [P]
    private: void planInline(Function* const pCallee)
    {
        CLOG_SECTION(2, "<h3>Plan for callee ~S</h3>", pCallee);

        if (! canInline(pCallee))
        {
            return;
        }

        foreach (Function::EnumCall, oEnum, pCallee)
        {
            CallI* pCallI = oEnum.GetI()->DynamicCast<CallI>();
            if (! pCallI->IsNotinline())
            {
                CLOG(2, "inline ~S", pCallI);
                m_oCalls.Push(pCallI);
            }
        } // for
    } // planInline
}; /// PassInline

DEFPASS(Inline)

} // Compiler
} // TinyCl
