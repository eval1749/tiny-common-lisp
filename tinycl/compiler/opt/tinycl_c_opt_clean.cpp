#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Optimize - Clean
// tinycl_c_opt_clean.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_clean.cpp#13 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

/// <summary>
///   Clean pass. Remove redundnat basic block in CFG.
///   <list>
///     <item>
///      <term>Coalesce basic blocks</term>
///      <description>
///        Coalesce predecessor and successor if sucessor has only one
///        predecessor.
///      </description>
///     </item>
///     <item>
///       <term>Fold static branch</term>
///       <description>
///         A branch conditiion is always true or false.
///       </description>
///     </item>
///     <item>
///       <term>Fold useless branch</term>
///       <description>
///         Targets of true and false are same.
///       </description>
///     </item>
///     <item>
///       <term>Remove empty block</term>
///       <description>
///         Remove block contains only JUMP instruction.
///       </description>
///     </item>
///     <item>
///       <term>Replace Jump+Phi+Ret to Ret</term>
///     </item>
///   </list>
/// </summary>
class PassCleanBase
{
    protected: bool m_fFinal;

    protected: PassCleanBase(bool fFinal) :
        m_fFinal(fFinal) {}

    /// <summary>Entry point</summary>
    protected: void processFunction(Function* pFun)
    {
        uint nCount = 0;
        for (;;)
        {
            bool fMore = false;

            nCount += 1;

            CLOG(1, "<h2> [~D] Clean ~S</h2>~%", nCount, pFun);

            foreach (Function::EnumBBlockPostorder, oEnum, pFun)
            {
                BBlock* pBB = oEnum.Get();
                if (pFun->GetEntryBB() != pBB)
                {
                    if (processBBlock(pBB))
                    {
                        fMore = true;
                    }
                }
            } // for each bblock

            if (! fMore) break;
        } // for

        if (nCount >= 2)
        {
            pFun->Clean();
        }
    } // processFunction

    private: struct TailInfo
    {
        uint                m_nCount;
        WorkList_<BBlock>   m_oTails;

        TailInfo() :
            m_nCount(0) {}
    }; // TialInfo

    // [C]
    private: static bool computeTailInfo(
        BBlock*     const pCurrBB,
        TailInfo*   const pInfo )
    {
        class Local
        {
            public: static uint CountMatch(
                BBlock* const pBB1,
                BBlock* const pBB2 )
            {
                CLOG_SECTION(2, "compare ~S and ~S", pBB1, pBB2);

                uint nCount = 0;
                BBlock::EnumIReverse oEnum2(pBB2);
                foreach (BBlock::EnumIReverse, oEnum1, pBB1)
                {
                    if (oEnum2.AtEnd())
                    {
                        break;
                    }

                    CLOG(2, "<li>compare ~S and ~S</li>~%",
                        oEnum1.Get(), oEnum2.Get() );

                    if (! oEnum1.Get()->Equal(oEnum2.Get()))
                    {
                        break;
                    }

                    oEnum2.Next();
                    nCount += 1;
                } // for

                CLOG(2, "<li>n=~D</li>", nCount);
                return nCount;
            } // CountMatch
        }; // Local

        if (! pCurrBB->HasMoreThanOnePred())
        {
            return false;
        }

        CLOG_SECTION(2, "Compute common tail for ~S", pCurrBB);

        pInfo->m_nCount = 1;
        BBlock* pLeaderBB = NULL;

        foreach (BBlock::EnumInEdge, oEnum, pCurrBB)
        {
            CfgEdge* pRunner= oEnum.Get();

            switch (pRunner->GetKind())
            {
            case CfgEdge::Kind_Pseudo:
            case CfgEdge::Kind_Nonlocal:
                continue;
            } // switch kind

            BBlock* const pBB1 = pRunner->GetFrom();

            CLOG_SECTION(2, "try ~S", pBB1);

            for (;;)
            {
                pRunner = static_cast<BBlock::InEdge*>(pRunner)->GetNext();
                if (NULL == pRunner)
                {
                    break;
                }

                BBlock* const pBB2 = pRunner->GetFrom();

                if (pBB2->IsInList())
                {
                    continue;
                }

                if (pBB1 == pBB2)
                {
                    continue;
                }

                uint const nCount = Local::CountMatch(pBB1, pBB2);
                if (pInfo->m_nCount < nCount)
                {
                    CLOG(2, "<li><b>New tail ~D</b></li>~%", nCount);

                    pInfo->m_nCount = nCount;

                    pInfo->m_oTails.MakeEmpty();
                    pInfo->m_oTails.Push(pBB1);
                    pInfo->m_oTails.Push(pBB2);
                    pLeaderBB = pBB1;
                }
                else if (pInfo->m_nCount == nCount)
                {
                    if (pLeaderBB == pBB1)
                    {
                        ASSERT(! pInfo->m_oTails.IsEmpty());
                        ASSERT(nCount > 1);
                        CLOG(2, "<li><b>add tail</b></li>~%");
                        pInfo->m_oTails.Push(pBB2);
                    }
                }
            } // for
        } // for each edge

        return ! pInfo->m_oTails.IsEmpty();
    } // computeTailInfo

    // [F]
    private: static void foldBranch(
        BranchI* const pBranchI,
        BBlock*  const pTargetBB )
    {
        CLOG_SECTION(2, "Fold ~S => ~S", pBranchI, pTargetBB);

        JumpI* const pJumpI = new JumpI(pTargetBB);
        pBranchI->GetBBlock()->ReplaceI(pJumpI, pBranchI);
        processJump(pJumpI);
    } // foldBranch

    // [M]
    private: static void mergeTail(
        BBlock*   const pCurrBB,
        TailInfo* const pInfo )
    {
        class Local
        {
            public: static void MoveLast(
                BBlock* const pToBB,
                BBlock* const pFromBB,
                uint    const nCount )
            {
                Instruction* pRefI = pToBB->GetLastI();

                for (uint k = 0; k < nCount; k++)
                {
                    pRefI = pToBB->MoveBeforeI(
                        pFromBB->GetLastI(),
                        pRefI );
                } // for k
            } // MoveLast

            public: static void RemoveLast(
                BBlock* const pFromBB,
                uint    const nCount )
            {
                for (uint k = 0; k < nCount; k++)
                {
                    pFromBB->RemoveI(pFromBB->GetLastI());
                } // for k
            } // RemoveLast
        }; // Local

        CLOG_SECTION(1, "<h2>Merge tails of preds of ~S</h2>", pCurrBB);

        BBlock* const pCommonBB = new BBlock;
        pCurrBB->GetFunction()->InsertBefore(pCommonBB, pCurrBB);

        // Move tails
        BBlock* const pFirstBB = pInfo->m_oTails.Pop();
        Local::MoveLast(pCommonBB, pFirstBB, pInfo->m_nCount);
        pFirstBB->AppendI(new JumpI(pCommonBB));

        // Remove tails
        while (! pInfo->m_oTails.IsEmpty())
        {
            BBlock* const pBB = pInfo->m_oTails.Pop();
            Local::RemoveLast(pBB, pInfo->m_nCount);
            pBB->AppendI(new JumpI(pCommonBB));
        } // for
    } // mergeTail

    // [P]
    private: bool processBBlock(BBlock* pBB)
    {
        CLOG_SECTION(1, "<h3>Clean ~S</h3>", pBB);

        bool fChanged = false;

        if (m_fFinal)
        {
            TailInfo oInfo;
            if (computeTailInfo(pBB, &oInfo))
            {
                mergeTail(pBB, &oInfo);
                fChanged = true;
            } // if
        } // if final

        Instruction* pLastI = pBB->GetLastI();
        if (BranchI* pBranchI = pLastI->DynamicCast<BranchI>())
        {
            if (processBranch(pBranchI)) fChanged = true;
        }
        else if (JumpI* pJumpI = pLastI->DynamicCast<JumpI>())
        {
            if (processJump(pJumpI)) fChanged = true;
        }

        return fChanged;
    } // processBBlock

    private: static bool processBranch(BranchI* const pBranchI)
    {
        if (pBranchI->GetSx()->IsTrue())
        {
            #if 0
                updatePhis(
                    pBranchI->GetBBlock(),
                    pBranchI->GetBBlock(),
                    pBranchI->GetTrueBB() );
            #endif

            removePhis(pBranchI->GetBB(), pBranchI->GetFalseBB());

            foldBranch(pBranchI, pBranchI->GetTrueBB());
            return true;
        }

        if (pBranchI->GetSx()->IsFalse())
        {
            #if 0
                updatePhis(
                    pBranchI->GetBBlock(),
                    pBranchI->GetBBlock(),
                    pBranchI->GetFalseBB() );
            #endif

            removePhis(pBranchI->GetBB(), pBranchI->GetTrueBB());

            foldBranch(pBranchI, pBranchI->GetFalseBB());
            return true;
        }

        if (pBranchI->GetTrueBB() == pBranchI->GetFalseBB())
        {
            foldBranch(pBranchI, pBranchI->GetFalseBB());
            return true;
        }

        bool fChanged = false;

#if 0
        if (processNeIfNeBranch(pBranchI))
        {
            fChanged = true;
        }
#endif
        if (processBranchPhi(pBranchI))
        {
            return fChanged;
        }

        if (processBranchPhiRet(pBranchI))
        {
            return fChanged;
        }

        if (processBranchRet(pBranchI))
        {
            return fChanged;
        }

        return fChanged;
    } // processBranch

    // Branch+Phi => If
    // See (defun foo (n old-p) (<= 1 n (if old-p 4999 3999)))
    private: static bool processBranchPhi(BranchI* const pBranchI)
    {
        BBlock* const pTrueBB = pBranchI->GetTrueBB();
        JumpI* const pTrueJumpI = pTrueBB->GetFirstI()->DynamicCast<JumpI>();
        if (NULL == pTrueJumpI) return false;
        if (pTrueBB->HasMoreThanOnePred()) return false;

        BBlock* const pFalseBB = pBranchI->GetFalseBB();
        JumpI* const pFalseJumpI = pFalseBB->GetFirstI()->DynamicCast<JumpI>();
        if (NULL == pFalseJumpI) return false;
        if (pFalseBB->HasMoreThanOnePred()) return false;

        BBlock* const pJoinBB = pTrueJumpI->GetTargetBB();
        if (pFalseJumpI->GetTargetBB() != pJoinBB) return false;

        if (pJoinBB->CountInEdge() != 2) return false;

        if (! pJoinBB->GetFirstI()->Is<PhiI>()) return false;

        CLOG_SECTION(2, "Fold Branch+Phi ~S", pBranchI);

        BBlock* const pCurr = pBranchI->GetBBlock();
        Bool* const pBx = pBranchI->GetBx();
        for (;;)
        {
            PhiI* const pPhiI = pJoinBB->GetFirstI()->DynamicCast<PhiI>();
            if (NULL == pPhiI) break;

            pCurr->InsertBeforeI(
                new IfI(
                    pPhiI->GetOutput(),
                    pBx,
                    pPhiI->GetOperand(pTrueBB),
                    pPhiI->GetOperand(pFalseBB) ),
                pBranchI );

            pJoinBB->RemoveI(pPhiI);
        } // for

        JumpI* const pJumpI = new JumpI(pJoinBB);
        pCurr->ReplaceI(pJumpI, pBranchI);

        pTrueBB->GetFunction()->RemoveBBlock(pTrueBB);
        pFalseBB->GetFunction()->RemoveBBlock(pFalseBB);

        processJump(pJumpI);
        return true;
    } // processBranchPhi

    // Branch+Phi+Ret => If+Ret
    //  See (defun foo (x) (typep x 'symbol))
    private: static bool processBranchPhiRet(BranchI* const pBranchI)
    {
        class Local
        {
            public: static bool TryPhiRet(
                BranchI*    const pBranchI,
                PhiI*       const pPhiI )
            {
                RetI* const pRetI = pPhiI->GetNext()->DynamicCast<RetI>();
                if (NULL == pRetI)
                {
                    return false;
                }

                BBlock* pRetBB = pPhiI->GetBB()->GetFunction()->InsertBefore(
                    new BBlock,
                    pPhiI->GetBB() );

                OperandBox* const pPhiBox = pPhiI->GetOperandBox(
                    pBranchI->GetBB() );

                pRetBB->AppendI(new RetI(pPhiBox->GetOperand()));

                pBranchI->GetBB()->RedirectEdgeTo(
                    pRetBB,
                    pPhiI->GetBB() );

                return true;
            } // TryPhiRet
        }; // Local

        ASSERT(pBranchI->GetTrueBB() != pBranchI->GetFalseBB());

        bool fChanged = false;

        if (PhiI* const pPhiI =
                pBranchI->GetTrueBB()->GetFirstI()->DynamicCast<PhiI>() )
        {
            if (Local::TryPhiRet(pBranchI, pPhiI))
            {
                fChanged = true;
            }
        }

        if (PhiI* const pPhiI =
                pBranchI->GetFalseBB()->GetFirstI()->DynamicCast<PhiI>() )
        {
            if (Local::TryPhiRet(pBranchI, pPhiI))
            {
                fChanged = true;
            }
        }

        return fChanged;
    } // processBranchPhiRet

    // Branch+Ret => If+Ret
    // See
    //  (defun foo (x) (declare (type character x))
    //    (if (char< x #\a) 10 20) )
    private: static bool processBranchRet(BranchI* const pBranchI)
    {
        RetI* const pTrueRetI = pBranchI->GetTrueBB()->
            GetFirstI()->DynamicCast<RetI>();
        if (NULL == pTrueRetI)
        {
            return false;
        }

        RetI* const pFalseRetI = pBranchI->GetFalseBB()->
            GetFirstI()->DynamicCast<RetI>();
        if (NULL == pFalseRetI)
        {
            return false;
        }

        Register* const pR1 = new Register;
        pBranchI->GetBB()->InsertBeforeI(
            new IfI(tyT, pR1, pBranchI->GetBx(),
                    pTrueRetI->GetSx(),
                    pFalseRetI->GetSx() ),
            pBranchI );

        pBranchI->GetBB()->ReplaceI(new RetI(pR1), pBranchI);

        return true;
    } // processBranchRet

    private: static bool processJump(JumpI* const pJumpI)
    {
        if (pJumpI->GetBBlock()->HasMoreThanOneSucc())
        {
            CLOG(3, "<li>processJump ~S out=~D</li>~%",
                pJumpI, pJumpI->GetBBlock()->CountOutEdge() );
            // Maybe OPENBLOCK, OPENCATCH, OPENTAGS
            return false;
        }

        if (processJumpEmpty(pJumpI))    return true;
        if (processJumpCoalesce(pJumpI)) return true;
        if (processJumpPhiNeBranch(pJumpI)) return true;
        if (processJumpPhiRet(pJumpI))   return true;
        return false;
    } // processJump

    private: static bool processJumpCoalesce(JumpI* const pJumpI)
    {
        ASSERT(NULL != pJumpI);

        BBlock* const pCurr = pJumpI->GetBBlock();
        ASSERT(! pCurr->HasMoreThanOneSucc());

        BBlock* const pSucc = pJumpI->GetTargetBB();

        if (pSucc->HasMoreThanOnePred())
        {
            CLOG(3, "<li>Coalesce ~S pred=~D</li>~%",
                pSucc, pSucc->CountInEdge() );
            return false;
        }

        CLOG_SECTION(2, "Coalesce ~S => ~S", pSucc, pCurr);

        // Fold PHI in succ
        for (;;)
        {
            PhiI* const pPhiI = pSucc->GetFirstI()->DynamicCast<PhiI>();
            if (NULL == pPhiI)
            {
                break;
            }

            SsaOutput* const pSd = pPhiI->GetSsaOutput();
            if (NULL != pSd)
            {
                pSd->ReplaceAll(pPhiI->GetOperand(pCurr));
            }
            pSucc->RemoveI(pPhiI);
        } // for

        // Move instructions from succ to curr
        {
            Instruction* const pRefI = pCurr->GetLastI();
            while (Instruction* pI = pSucc->GetFirstI())
            {
                pCurr->MoveBeforeI(pI, pRefI);
            } // while

            pCurr->RemoveI(pRefI);
        }

        // Move all out edge from succ to curr
        {
            BBlock::EnumSucc oEnum(pSucc);
            while (! oEnum.AtEnd())
            {
                BBlock* const pSuccSucc = oEnum.Get();
                oEnum.Next();
                pSuccSucc->RedirectEdgeFrom(pCurr, pSucc);
            } // for each succ
        }

        pSucc->MoveLabelsTo(pCurr);
        pSucc->GetFunction()->RemoveBBlock(pSucc);

        return true;
    } // processJumpCoalesce

    private: static bool processJumpEmpty(JumpI* const pJumpI)
    {
        ASSERT(NULL != pJumpI);

        BBlock* const pCurr = pJumpI->GetBBlock();
        ASSERT(! pCurr->HasMoreThanOneSucc());

        BBlock* const pSucc = pJumpI->GetTargetBB();

        if (pCurr->GetFirstI() != pJumpI) return false;
        if (pSucc->GetFirstI()->Is<PhiI>()) return false;
        if (pCurr == pSucc) return false;   // for (defun foo () (foo))

        CLOG_SECTION(2, "Remove empty ~S => ~S", pCurr, pSucc);

        // Redirect pred=>curr to pred=>succ
        {
            BBlock::EnumPred oEnum(pCurr);
            while (! oEnum.AtEnd())
            {
                BBlock* const pPred = oEnum.Get();
                oEnum.Next();
                pPred->RedirectEdgeTo(pSucc, pCurr);
            } // for each inedge
        }

        pCurr->MoveLabelsTo(pSucc);
        pCurr->GetFunction()->RemoveBBlock(pCurr);

        return true;
    } // processJumpEmpty

    // Redirect Jump+Phi+Ne+Branch
    private: static bool processJumpPhiNeBranch(JumpI* const pJumpI)
    {
        PhiI* const pPhiI = pJumpI->GetTargetBB()->
            GetFirstI()->DynamicCast<PhiI>();
        if (NULL == pPhiI) return false;

        NeI* const pNeI = pPhiI->GetNext()->DynamicCast<NeI>();
        if (NULL == pNeI) return false;
        if (pNeI->GetSx() != pPhiI->GetRd()) return false;
        if (! pNeI->GetSy()->IsFalse()) return false;

        if (NULL == pPhiI->GetRd()->GetSingleUser())
        {
            // Multiple users
            // See macroexpand-1
            return false;
        }

        BranchI* const pBranchI = pNeI->GetNext()->DynamicCast<BranchI>();
        if (NULL == pBranchI) return false;
        if (pBranchI->GetBx() != pNeI->GetBd()) return false;

        BBlock* pTargetBB;
        if (pPhiI->GetOperand(pJumpI->GetBB())->IsFalse())
        {
            pTargetBB = pBranchI->GetFalseBB();
        }
        else if (pPhiI->GetOperand(pJumpI->GetBB())->IsTrue())
        {
            pTargetBB = pBranchI->GetTrueBB();
        }
        else
        {
            return false;
        }

        CLOG_SECTION(1, "Jump+Phi+Ne+Branch ~S", pJumpI);

        updatePhis(
            pJumpI->GetBB(),
            pBranchI->GetBB(),
            pTargetBB );

        pJumpI->GetBB()->ReplaceI(
            new JumpI(pTargetBB),
            pJumpI );

        return true;
    } // processJumpPhiNeBranch

    /// <summary>
    ///   Rewrite Jump+Phi+Ret to Ret
    /// </summary>
    private: static bool processJumpPhiRet(JumpI* const pJumpI)
    {
        BBlock* const pCurr = pJumpI->GetBBlock();
        ASSERT(! pCurr->HasMoreThanOneSucc());

        BBlock* const pSucc = pJumpI->GetTargetBB();

        RetI* const pRetI = pSucc->GetLastI()->DynamicCast<RetI>();
        if (NULL == pRetI) return false;

        PhiI* const pPhiI = pSucc->GetFirstI()->DynamicCast<PhiI>();
        if (NULL == pPhiI) return false;

        if (NULL == pPhiI->GetSsaOutput()) return false;

        if (pPhiI->GetNext() != pRetI) return false;

        CLOG_SECTION(2, "Fold Jump+Phi+Ret: ~S", pPhiI);

        OperandBox* const pBox = pPhiI->GetOperandBox(pCurr);
        Operand* const pSx = pBox->GetOperand();

        //pPhiI->RemoveOperand(pBox);

        pCurr->ReplaceI(new RetI(pSx), pJumpI);

        return true;
    } // processJumpPhiRet

#if 0
    // ; (and x y)
    // BB10
    //  Ne BOOL %b11 <= %r3 NIL
    //  If T    %r12 <= %b11 %r4 NIL
    //  Ne BOOL %b15 <= %r12 NIL
    //  Branch  %b15 BB13 BB14
    //  =>
    // BB10
    //  Ne BOOL %b11 <= %r3 NIL
    //  Branch  %bb1 BB15 BB14
    // BB15
    //  Ne BOOL %b15 <= %r4 NIL
    //  Branch  %b15 BB13 BB14
    private: static bool processNeIfNeBranch(BranchI* const pBranchI)
    {
        NeI* const pNe1I = pBranchI->GetBx()->GetDefI()->DynamicCast<NeI>();
        if (NULL == pNe1I) return false;
        if (! pNe1I->GetSy()->IsFalse()) return false;

        IfI* const pIfI = pNe1I->GetRx()->GetDefI()->DynamicCast<IfI>();
        if (NULL == pIfI) return false;
        if (! pIfI->GetSz()->IsFalse()) return false;

        NeI* const pNe2I = pIfI->GetBx()->GetDefI()->DynamicCast<NeI>();
        if (NULL == pNe2I) return false;
        if (! pNe2I->GetSy()->IsFalse()) return false;

        if (pBranchI->GetPrev() != pNe1I) return false;
        if (pBranchI->GetPrev()->GetPrev() != pIfI) return false;
        if (pBranchI->GetPrev()->GetPrev()->GetPrev() != pNe2I) return false;

        CLOG_SECTION(1, "Optimize (and x y)");

        BBlock* const pBB = pNe2I->GetBB();

        pNe1I->GetOperandBox(0)->Replace(pIfI->GetSy());

        pBB->RemoveI(pIfI);

        BBlock* const pAfterBB = pBB->SplitBefore(pNe1I);

        pBB->ReplaceI(
            new BranchI(
                pNe2I->GetBd(),
                pAfterBB,
                pBranchI->GetFalseBB() ),
            pBB->GetLastI() );

        updatePhis(pBB, pAfterBB, pBranchI->GetFalseBB());

        return true;
    } // processNeIfNeBranch
#endif

    private: static void removePhis(
        BBlock* const pBB,
        BBlock* const pTargetBB )
    {
        foreach (BBlock::EnumI, oEnum, pTargetBB)
        {
            PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI)
            {
                break;
            }

            OperandBox* const pBox = pPhiI->GetOperandBox(pBB);
            pPhiI->RemoveOperand(pBox);
        } // for
    } // removePhis

    /// <summary>
    ///   Updates PHI instructions.
    /// </summary>
    /// <param name="pNewBB">A new basic block</param>
    /// <param name="pOldBB">A old basic block</param>
    /// <param name="pBB">
    ///   A basic block contains PHI instructions to replace.
    /// </param>
    private: static void updatePhis(
        BBlock* const pNewBB,
        BBlock* const pOldBB,
        BBlock* const pBB )
    {
        CLOG(2, "<li>Update Phi new=~S, old=~S at ~S</li>",
            pNewBB, pOldBB, pBB );

        foreach (BBlock::EnumI, oEnum, pBB)
        {
            PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI)
            {
                break;
            }

            pPhiI->AddOperand(pNewBB, pPhiI->GetOperand(pOldBB));
        } // for each phi
    } // updatePhis
}; /// PassClean

class PassClean :
    public Pass_<PassClean, FunctionPass>,
    public PassCleanBase
{
    public: static const char* GetName_() { return "Clean"; }

    public: PassClean() :
        PassCleanBase(false) {}

    protected: override void processFunction(Function* pFun)
        { PassCleanBase::processFunction(pFun); }
}; // PassClean

class PassFinalClean :
    public Pass_<PassFinalClean, FunctionPass>,
    public PassCleanBase
{
    public: static const char* GetName_() { return "FinalClean"; }

    public: PassFinalClean() :
        PassCleanBase(true) {}

    protected: override void processFunction(Function* pFun)
        { PassCleanBase::processFunction(pFun); }
}; // PassFinalClean

DEFPASS(Clean)
DEFPASS(FinalClean)

} // Compiler

} // TinyCl
