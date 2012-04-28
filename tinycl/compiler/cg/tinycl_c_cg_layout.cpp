#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Layout
// tinycl_c_cg_layout.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_layout.cpp#6 $
//
#include "./tinycl_c_cg.h"

#include "../ir/tinycl_c_ir_loop.h"

namespace TinyCl
{

namespace Compiler
{

namespace CgLayout
{

/// <summary>
///   Represents basic block executing frequency predict pass.
/// </summary>
class SubPassPredict :
    public Pass_<SubPassPredict, SubPass>
{
    public: static const char* GetName_() { return "Predict"; }

    private: static const uint CountUnit = 1000;

    // ctor
    private: SubPassPredict() {}

    // Entry Point
    public: static void Run(Function* pFun)
    {
        SubPassPredict oSubPass;
        oSubPass.run(pFun);
    } // Run

    // [C]
    private: static uint computeTrueBranch(
        const BranchI*  const pBranchI,
        int             const nCount )
    {
        const BBlock* const pTrueBB = pBranchI->GetTrueBB();
        if (pTrueBB->GetLastI()->Is<UnreachableI>())
        {
            return 1;
        }

        if (pTrueBB->GetLastI()->Is<TerminateInstruction>())
        {
            return 2;
        }

        if (pTrueBB->GetLastI()->Is<BranchI>())
        {
            return max(nCount - 10, 1);
        }

        const BBlock* const pFalseBB = pBranchI->GetFalseBB();
        if (pFalseBB->GetLastI()->Is<UnreachableI>())
        {
            return max(nCount - 1, 1);
        }

        if (pFalseBB->GetLastI()->Is<TerminateInstruction>())
        {
            return max(nCount - 2, 1);
        }

        if (pFalseBB->GetLastI()->Is<BranchI>())
        {
            return 10;
        }

        int const iDiff =
            pTrueBB->GetLoopNode()->m_nDepth -
            pFalseBB->GetLoopNode()->m_nDepth;

        if (iDiff > 0)
        {
            return max(nCount - 1, 1);
        }
        else if (iDiff < 0)
        {
            return 1;
        }
        else
        {
            return static_cast<uint>(Ceiling(nCount, 2));
        }
    } // computeTrueBranch

    // [P]
    private: void processBBlock(
        BBlock* const pBB,
        int     const nBBDepth )
    {
        int nCount = 0;
        foreach (BBlock::EnumInEdge, oEnum, pBB)
        {
            CfgEdge* const pEdge = oEnum.Get();
            nCount += pEdge->GetCount();
        } // for in edge

        CLOG_SECTION(1, "~S depth=~D count=~D", pBB, nBBDepth, nCount);

        for (uint k = nBBDepth; k < pBB->GetLoopNode()->m_nDepth; k++)
        {
            nCount *= 10;
        } // for k

        uint const nDepth = pBB->GetLoopNode()->m_nDepth;

        if (BranchI* pBranchI = pBB->GetLastI()->DynamicCast<BranchI>())
        {
            CfgEdge* const pTrueEdge  = pBB->GetEdge(pBranchI->GetTrueBB());
            CfgEdge* const pFalseEdge = pBB->GetEdge(pBranchI->GetFalseBB());

            int const nTrue  = pTrueEdge->GetCount();
            int const nFalse = pFalseEdge->GetCount();

            if (0 == nTrue && 0 == nFalse)
            {
                int const nTrueCount = computeTrueBranch(pBranchI, nCount);
                pTrueEdge->SetCount(nTrueCount);
                pFalseEdge->SetCount(nCount - nTrueCount);
            }
            else if (0 == nTrue)
            {
                pTrueEdge->SetCount(nCount - nFalse);
                pFalseEdge->SetCount(nFalse);
            }
            else
            {
                pTrueEdge->SetCount(nFalse);
                pFalseEdge->SetCount(nCount - nFalse);
            }
        }
        else if (JumpI* const pJumpI = pBB->GetLastI()->DynamicCast<JumpI>())
        {
            CfgEdge* const pEdge = pBB->GetEdge(pJumpI->GetTargetBB());
            if (0 == pEdge->GetCount())
            {
                pEdge->SetCount(nCount);
            }
        } // if

        foreach (BBlock::EnumChild, oEnum, pBB)
        {
            BBlock* const pChild = oEnum.Get();
            processBBlock(pChild, nDepth);
        } // for child
    } // processBBlock

    private: static void propagateUnreachable(BBlock* const pBB)
    {
        pBB->SetFlag(1);
        foreach (BBlock::EnumInEdge, oEnum, pBB)
        {
            CfgEdge* const pEdge = oEnum.Get();
            pEdge->SetCount(1);
            BBlock* pBB2 = pEdge->GetFrom();
            if (pBB2->GetLastI()->Is<JumpI>())
            {
                propagateUnreachable(pBB2);
            }
        } // for
    } // propagateUnreachable

    // [R]
    private: void run(Function* pFun)
    {
        CLOG(1, "<h2>Static Edge Frequency Prediction ~S</h2>~%", pFun);

        pFun->ComputeLoop();

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();
            pBB->SetFlag(0);
            foreach (BBlock::EnumOutEdge, oEnum, pBB)
            {
                CfgEdge* const pEdge = oEnum.Get();
                pEdge->SetCount(0);
            }
        } // for bblock

        foreach (BBlock::EnumPred, oEnum, pFun->GetExitBB())
        {
            BBlock* const pPredBB = oEnum.Get();
            if (pPredBB->GetLastI()->Is<UnreachableI>())
            {
                propagateUnreachable(pPredBB);
            }
        } // for pred

        foreach (BBlock::EnumOutEdge, oEnum, pFun->GetEntryBB())
        {
            CfgEdge* const pEdge = oEnum.Get();
            pEdge->SetCount(
                pEdge->GetKind() == CfgEdge::Kind_Normal ?
                    CountUnit :
                    0 );
        } // for out edge

        foreach (BBlock::EnumChild, oEnum, pFun->GetEntryBB())
        {
            processBBlock(oEnum.Get(), 1);
        } // for child
    } // run
}; // SubPassPredict

/// <summary>
///   Represents basic block reorder pass.
/// </summary>
class SubPassReorder :
    public Pass_<SubPassReorder, SubPass>,
    protected Mm
{
    public: static const char* GetName_() { return "Reorder"; }

    private: class Chain;
    private: typedef DoubleLinkedList_<Chain> Chains;

    private: class Chain :
        public LocalObject,
        public DoubleLinkedItem_<Chain>,
        public Object,
        public WorkListItem_<Chain>
    {
        private: bool   m_fUnreachable;
        public: uint    m_nPriority;
        public: Chains  m_oChains;
        public: BBlock* m_pBB;

        // ctor
        public: Chain(BBlock* const pBB) :
            m_fUnreachable(pBB->GetFlag() != 0),
            m_nPriority(1),
            m_pBB(pBB)
            { m_oChains.Append(this); }

        // [G]
        public: BBlock* GetHeadBB() const
            { return m_oChains.GetFirst()->m_pBB; }

        public: BBlock* GetTailBB() const
            { return m_oChains.GetLast()->m_pBB; }

        // [H]
        public: virtual void HtmlPrint(Val stream, bool) const override
        {
            cformat(stream, "[Chain ~D ~S...~S]",
                m_nPriority,
                m_oChains.GetFirst()->m_pBB,
                m_oChains.GetLast()->m_pBB );
        } // HtmlPrint

        // [I]
        public: bool IsLess(const Chain* const that) const
        {
            if (this->IsUnreachable())
            {
                if (! that->IsUnreachable())
                {
                    return true;
                }
            }
            else if (that->IsUnreachable())
            {
                return false;
            }

            if (int const iDiff = this->m_nPriority - that->m_nPriority)
            {
                return iDiff < 0;
            }

            return this->GetHeadBB()->GetPostorder() >
                   that->GetHeadBB()->GetPostorder();
        } // IsLess

        public: bool IsUnreachable() const
            { return m_fUnreachable; }
    }; // Chain

    private: typedef WorkList_<CfgEdge> EdgeList;

    private: class EdgeOrder
    {
        public: static bool GreaterThan(const CfgEdge* p, const CfgEdge* q)
            { return p->GetCount() >  q->GetCount(); }
    }; // EdgeOrder

    // ctor
    private: SubPassReorder() {}

    // Entry Point
    public: static void Run(Function* pFun)
    {
        SubPassReorder oSubPass;
        oSubPass.run(pFun);
    } // Run

    // [B]
    private: void buildChain(Function* const pFun)
    {
        CLOG_SECTION(3, "<h3>build chain ~S</h3>", pFun);

        // [1] Make each block a degenerate chain & set its priority to
        // # blocks
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBB = oEnum.Get();
            pBB->SetWork(new(this) Chain(pBB));
        } // for bblock

        // [2] P <= 1
        uint nP = 1;

        // [3] for all edge E = (x, y) in the CFG, in order by decreasing
        // frequency
        EdgeList oEdgeList;
        foreach (Function::EnumBBlock, oEnumBB, pFun)
        {
            BBlock* pBB = oEnumBB.Get();
            foreach (BBlock::EnumOutEdge, oEnum, pBB)
            {
                CfgEdge* pEdge = oEnum.Get();
                if (pEdge->GetKind() == CfgEdge::Kind_Normal)
                {
                    CLOG(4, "<li>insert ~S</li>~%", pEdge);
                    oEdgeList.Insert<EdgeOrder>(pEdge);
                }
            } // for out edge
        } // for bblock

        while (! oEdgeList.IsEmpty())
        {
            CfgEdge* pEdge = oEdgeList.Pop();

            CLOG(4, "<li>pop ~D ~S</li>~%", nP, pEdge);

            BBlock* const pTailB  = pEdge->GetFrom();
            Chain*  const pChainX = pTailB->GetWork<Chain>();

            BBlock* const pHeadB  = pEdge->GetTo();
            Chain*  const pChainY = pHeadB->GetWork<Chain>();

            if (! pEdge->IsBackward() &&
                pChainX != pChainY &&
                pChainX->GetTailBB() == pTailB &&
                pChainY->GetHeadBB() == pHeadB )
            {
                // merge chain a and b
                CLOG(4, "<li>merge ~S and ~S</li>~%", pChainX, pChainY);
                while (Chain* pRunner = pChainY->m_oChains.GetFirst())
                {
                    BBlock* const pBB = pRunner->m_pBB;
                    pChainY->m_oChains.Delete(pRunner);
                    pChainX->m_oChains.Append(pRunner);
                    pBB->SetWork(pChainX);
                    pChainX->m_nPriority += 1;
                } // while
            }
            else if (! pChainY->IsUnreachable())
            {
                // Place targets after their sources, to make forward
                // branches
                nP += 1;
                pChainY->m_nPriority = min(pChainY->m_nPriority, nP);
            }
        } // for edge
    } // buildChain

    // [P]
    private: static Chain* pick(WorkList_<Chain>* const pWorkList)
    {
        Chain* pCandidate = pWorkList->Get();
        foreach (WorkList_<Chain>::Enum, oEnum, pWorkList)
        {
            Chain* const pRunner = oEnum.Get();
            if (pCandidate->IsLess(pRunner))
            {
                pCandidate = pRunner;
            }
        } // for

        pWorkList->Delete(pCandidate);
        return pCandidate;
    } // pick

    // [R]
    private: static void reorder(Function* const pFun)
    {
        CLOG_SECTION(2, "<h3>reorder ~S</h3>", pFun);

        WorkList_<Chain> oWorkList;
        oWorkList.Push(pFun->GetEntryBB()->GetWork<Chain>());

        WorkList_<Chain> oDoneList;

        while (! oWorkList.IsEmpty())
        {
            Chain* const pChain = pick(&oWorkList);
            oDoneList.Push(pChain);

            CLOG_SECTION(3, "pick ~S priority=~D",
                pChain, pChain->m_nPriority );

            foreach (Chains::Enum, oEnumChain, &pChain->m_oChains)
            {
                BBlock* const pBB = oEnumChain.Get()->m_pBB;
                if (pBB == pFun->GetExitBB())
                {
                    continue;
                }

                pFun->MoveBBlock(pBB, pFun->GetExitBB());

                foreach (BBlock::EnumSucc, oEnumSucc, pBB)
                {
                    BBlock* const pSucc  = oEnumSucc.Get();
                    Chain*  const pChain = pSucc->GetWork<Chain>();
                    if (! pChain->IsInList())
                    {
                        oWorkList.Push(pChain);
                    }
                } // for succ
            } // for chain
        } // while
    } // reorder

    private: void run(Function* const pFun)
    {
        buildChain(pFun);
        reorder(pFun);
    } // run
}; // SubPassReorder

} // CgLayout

using namespace CgLayout;

/// <summary>
///   Represents Layout pass.
/// </summary>
class PassLayout :
    public Pass_<PassLayout, FunctionPass>
{
    public: static const char* GetName_() { return "Layout"; }

    private: virtual void processFunction(Function* pFun) override
    {
        SubPassPredict::Run(pFun);
        SubPassReorder::Run(pFun);
    } // processFunction
}; // PassLayout

DEFPASS(Layout)

} // Compiler

} // TinyCl
