#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse
// tinycl_compiler_parser.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_dom.cpp#2 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// DomTreeBuilder class
//
template<class DomCFG, class DomBBlock, class DomInfo>
class DomTreeBuilder
{
    private: DomCFG*     m_pCfg;
    private: DomBBlock*  m_pEntry;
    private: DomBBlock*  m_pDfsPrev;
    private: uint        m_nDfsName;

    public: DomTreeBuilder() {}

    // Build
    public: void Build(DomCFG* pCFG)
    {
        ASSERT(NULL != pCFG);

        m_pCfg = pCFG;

        m_pEntry = pCFG->GetEntry();

        m_nDfsName = 0;
        m_pDfsPrev = NULL;

        init();

        CLOG(2, "<li>Build DFS Tree<ol>");
        buildDfsTree(m_pEntry);
        CLOG(2, "</ol></li>~%");

        removeUnrechable();
        computeParent();
        m_pEntry->SetParent(NULL);
        computeChildren();
        computeFrontiers();
        uninit();
    } // Build

    // addFrontier
    private: void addFrontier(DomBBlock* pFrontier, DomBBlock* pBB)
    {
        ASSERT(NULL != pFrontier);
        ASSERT(NULL != pBB);

        if (pBB->GetFrontiers()->Position(pFrontier) < 0)
        {
            pBB->GetFrontiers()->SetFirst(
                new DomInfo::FrontierList::Cons(
                    pFrontier,
                    pBB->GetFrontiers()->GetFirst() ) );
        }
    } // addFrontier

    // buildDfsTree
    private: void buildDfsTree(DomBBlock* pBB)
    {
        DomBBlock* pCurr = pBB->Extend<DomBBlock>();
        pCurr->SetDfsName(1);

        foreach (DomBBlock::EnumOutEdge, oEnum, pCurr)
        {
            DomBBlock* pSucc = oEnum.GetNode();
            if (0 == pSucc->GetDfsName())
            {
                buildDfsTree(pSucc);
            }
        } // for each succ

        m_nDfsName += 1;
        pCurr->SetDfsName(m_nDfsName);
        pCurr->SetDfsNext(m_pDfsPrev);
        m_pDfsPrev = pCurr;

        CLOG(2, "<li>visit ~S dfs=~D</li>", pBB, m_nDfsName);
    } // buildDfsTree

    // compute-children
    //  Make children list in reverse postorder.
    private: void computeChildren()
    {
        for (
            DomBBlock* pBB = m_pEntry->GetDfsNext();
            NULL != pBB;
            pBB = pBB->GetDfsNext() )
        {
            DomBBlock* pParent = pBB->GetParent();

            pParent->GetChildren()->Append(pBB->GetDomInfo());
        } // for each bblock
    } // computeChildren

    // computeFrontiers
    //  Loop over bblock which has more than one predecessors.
    private: void computeFrontiers()
    {
        foreach (DomCFG::EnumBBlock, oEnum, m_pCfg)
        {
            DomBBlock* pBB = oEnum.Get()->Extend<DomBBlock>();

            if (hasMoreThanOnePred(pBB))
            {
                foreach (DomBBlock::EnumInEdge, oEnum, pBB)
                {
                    DomBBlock* pParent = pBB->GetParent();

                    for (
                        DomBBlock* pRunner = oEnum.GetNode();
                        pRunner != pParent;
                        pRunner = pRunner->GetParent() )
                    {
                        addFrontier(pBB, pRunner);
                    } // for each ancestor
                } // for each pred edge
            } // if
        } // for each bblock
    } // computeFrontiers

    // computeParent
    //  Computes parent (immediate dominator) for each bblock.
    private: void computeParent()
    {
        m_pEntry->SetParent(m_pEntry);
        m_pEntry->SetDepth(1);

        bool fChanged = true;
        while (fChanged)
        {
            fChanged = false;

            for (
                DomBBlock* pBB = m_pEntry->GetDfsNext();
                NULL != pBB;
                pBB = pBB->GetDfsNext() )
            {
                if (computeParentAux(pBB))
                {
                    fChanged = true;
                }
            } // for each bblock
        } // while
    } // computeParent

    // computeParentAux
    //  Computes new parent by processed predecessor.
    private: bool computeParentAux(DomBBlock* pCurr)
    {
        ASSERT(NULL != pCurr);

        foreach (DomBBlock::EnumInEdge, oEnum, pCurr)
        {
            DomBBlock* pParent = oEnum.GetNode();
                if (NULL == pParent->GetParent()) continue;

            foreach (DomBBlock::EnumInEdge, oEnum, pCurr)
            {
                DomBBlock* pPred = oEnum.GetNode();

                if (pParent != pPred && NULL != pPred->GetParent())
                {
                    pParent = intersect(pPred, pParent);
                }
            } // for each pred

            if (pCurr->GetParent() != pParent)
            {
                pCurr->SetParent(pParent);
                pCurr->SetDepth(pParent->GetDepth() + 1);
                return true;
            }
        } // for each parent

        return false;
    } // computeParentAux

    // hasMoreThanOnePred
    private: static bool hasMoreThanOnePred(DomBBlock* pBBlock)
    {
        DomBBlock::EnumInEdge oEnum(pBBlock);
        if (oEnum.AtEnd()) return false;
        oEnum.Next();
        return ! oEnum.AtEnd();
    } // hasMoreThanOnePred

    // init
    //  Allocates dominfo for all bblocks
    private: void init()
    {
        foreach (DomCFG::EnumBBlock, oEnum, m_pCfg)
        {
            DomBBlock* pBB = oEnum.Get()->Extend<DomBBlock>();
                pBB->SetDfsName(0);
                pBB->SetDfsNext(NULL);

            DomInfo* pDomInfo = pBB->GetDomInfo();
            if (NULL != pDomInfo)
            {
                pDomInfo->Reset();
            }
            else
            {
                pDomInfo = new DomInfo(pBB);
                pBB->SetDomInfo(pDomInfo);
            }
        } // for each bblock
    } // init

    // intersect
    private: DomBBlock* intersect(DomBBlock* pFinger1, DomBBlock* pFinger2)
    {
        ASSERT(NULL != pFinger1);
        ASSERT(NULL != pFinger2);

        while (pFinger1 != pFinger2)
        {
            while (pFinger1->GetDfsName() < pFinger2->GetDfsName())
            {
                pFinger1 = pFinger1->GetParent();
            } // while

            while (pFinger2->GetDfsName() < pFinger1->GetDfsName())
            {
                pFinger2 = pFinger2->GetParent();
            } // while
        } // while

        return pFinger1;
    } // intersect

    // remove unreachable bblocks
    private: void removeUnrechable()
    {
        DomCFG::EnumBBlock oEnum(m_pCfg);
        while (! oEnum.AtEnd())
        {
            BBlock* pBB = oEnum.Get();
            oEnum.Next();

            if (0 == pBB->Extend<DomBBlock>()->GetDfsName())
            {
                pBB->GetFunction()->RemoveBBlock(pBB);
            }
        } // for each bblock
    } // removeUnrechable

    // uninit
    private: void uninit()
    {
        foreach (DomCFG::EnumBBlock, oEnum, m_pCfg)
        {
            BBlock* pBB = oEnum.Get();
            pBB->Reset();
        } // for each bblock
    } // uninit
}; // DomTreeBuilder


template<class DomCFG, class DomBBlock, class DomInfo = BBlock::DomInfo>
class DomTreeDumper
{
    public: static void Run(DomCFG* pFun, const char* psz)
    {
        CLOG(3, "<h3>~ADominance Tree of ~S</h3>~%",
            psz,
            pFun );

        CLOG(3, "<table border='1'>~%");
        CLOG(3, "<thead><tr>~%");
        CLOG(3,   "<th>Depth</th>");
        CLOG(3,   "<th>BBlock</th>");
        CLOG(3,   "<th>Parent</th>");
        CLOG(3,   "<th>Children</th>");
        CLOG(3,   "<th>Frontiers</th>");
        CLOG(3, "</tr></thead>~%");

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DomBBlock* pBBlock = oEnum.Get()->Extend<DomBBlock>();

            CLOG(3, "<tr><td>~S</td>", pBBlock);

            BBlock::DomInfo* pDomInfo = pBBlock->GetDomInfo();

            CLOG(3, "<td>~D</td>~%", pDomInfo->GetDepth());

            if (NULL == pDomInfo->GetParent())
            {
                CLOG(3, "<td>-</td>~%");
            }
            else
            {
                CLOG(3, "<td>~S</td>~%",
                    pDomInfo->GetParent() );
            } // if

            // Children
            {
                CLOG(3, "<td>{");

                const char* psz = "";
                foreach (BBlock::DomInfo::EnumChild, oEnum, pDomInfo)
                {
                    BBlock* pChild = oEnum.Get();

                    CLOG(3, psz);
                    psz = " ";

                    CLOG(3, "~S", pChild);
                } // for each child

                CLOG(3, "}</td>~%");
            } // children

            // Frontiers
            {
                CLOG(3, "<td>{");

                const char* psz = "";
                foreach (BBlock::DomInfo::EnumFrontier, oEnum, pDomInfo)
                {
                    BBlock* pFrontier = oEnum.Get();

                    CLOG(3, psz);
                    psz = " ";

                    CLOG(3, "~S", pFrontier);
                } // for each Frontier

                CLOG(3, "}</td>~%");
            } // frontiers

            CLOG(3, "</tr>~%");
        } // for each bblock

        CLOG(3, "</table>~%");
    } // Run
}; // DomTreeDumper


//////////////////////////////////////////////////////////////////////
//
// DomBBlock_
//
template<class Base_, class EnumInEdge_, class EnumOutEdge_>
class DomBBlock_ : public Base_
{
    private: typedef BBlock::DomInfo DomInfo;
    private: typedef DomBBlock_<Base_, EnumInEdge_, EnumOutEdge_> BBlock_;

    public: uint GetDfsName() const
        { return static_cast<uint>(GetIndex()); }

    public: BBlock_* GetDfsNext() const
        { return GetWork<BBlock_>(); }

    public: void SetDfsName(uint nName)
        { SetIndex(nName); }

    public: void SetDfsNext(BBlock_* pNext)
        { SetWork(pNext); }

    public: class EnumInEdge : public EnumInEdge_
    {
        public: EnumInEdge(BBlock* pBB) : EnumInEdge_(pBB) {}

        public: BBlock_* GetNode() const
            { return EnumInEdge_::GetNode()->Extend<BBlock_>(); }
    }; // EnumInEdge

    public: class EnumOutEdge : public EnumOutEdge_
    {
        public: EnumOutEdge(BBlock* pBB) :
            EnumOutEdge_(pBB) {}

        public: BBlock_* GetNode() const
            { return EnumOutEdge_::GetNode()->Extend<BBlock_>(); }
    }; // EnumOutEdge

    ////////////////////////////////////////////////////////////
    //
    // DomInfo accessors shortcut
    //
    public: DomInfo::ChildList* GetChildren() const
        { return GetDomInfo()->GetChildren(); }

    public: uint GetDepth() const { return GetDomInfo()->GetDepth(); }
    public: uint SetDepth(uint n) { return GetDomInfo()->SetDepth(n); }

    public: DomInfo::FrontierList* GetFrontiers() const
        { return GetDomInfo()->GetFrontiers(); }

    public: BBlock_* GetParent() const
        { return GetDomInfo()->GetParent()->Extend<BBlock_>(); }

    public: void SetParent(BBlock_* pBB)
        { GetDomInfo()->SetParent(pBB); }
    
    DISALLOW_COPY_AND_ASSIGN(DomBBlock_);
}; // DomBBlock_

//////////////////////////////////////////////////////////////////////
//
// DomBBlock
//
class DomBBlockBase : public BBlock
{
    private: DomBBlockBase() {}

    public: DomInfo* GetDomInfo() const
        { return m_pDomInfo; }

    public: void SetDomInfo(DomInfo* pDomInfo)
        { m_pDomInfo = pDomInfo; }
}; // DomBBlockBase

//////////////////////////////////////////////////////////////////////
//
// PostDomBBlock
//
class PostDomBBlockBase : public BBlock
{
    private: PostDomBBlockBase() {}

    public: DomInfo* GetDomInfo() const
        { return m_pPostDomInfo; }

    public: void SetDomInfo(DomInfo* pDomInfo)
        { m_pPostDomInfo = pDomInfo; }
}; // PostDomBBlockBase

typedef DomBBlock_<
        DomBBlockBase, 
        BBlock::EnumInEdge, 
        BBlock::EnumOutEdge >
    DomBBlock;

typedef DomBBlock_<
        PostDomBBlockBase,
        BBlock::EnumOutEdge,
        BBlock::EnumInEdge > 
    PostDomBBlock;

//////////////////////////////////////////////////////////////////////
//
// DomCFG
//
class DomCFG : public Function
{
    private: DomCFG() {}

    public: DomBBlock* GetEntry() const
        { return GetEntryBB()->Extend<DomBBlock>(); }
}; // DomCFG

//////////////////////////////////////////////////////////////////////
//
// PostDomCFG
//
class PostDomCFG : public Function
{
    private: PostDomCFG() {}

    public: PostDomBBlock* GetEntry() const
        { return GetExitBB()->Extend<PostDomBBlock>(); }
}; // PostDomCFG

/// <summary>
///   Computes dominance.
/// </summary>
/// <returns>
///   Returns false if dominance tree has been computed.
/// </returns>
bool Function::ComputeDominance()
{
    CLOG(1, "<h2>Compute Dominance ~S</h2><ol>", this);

    if (Has(Cache_Dom))
    {
        CLOG(1, "<li>Use cached dominance.</li>");
        CLOG(1, "</ol>~%");
        return false;
    }

    DomTreeBuilder<DomCFG, DomBBlock, BBlock::DomInfo> oBuilder;
    oBuilder.Build(Extend<DomCFG>());
    m_rgfCache |= Cache_Dom;

    #if _DEBUG
        DomTreeDumper<DomCFG, DomBBlock, BBlock::DomInfo>::Run(
            Extend<DomCFG>(), "" );
    #endif

    CLOG(1, "</ol>~%");
    return true;
} // Function::ComputeDominance

/// <summary>
///   Computes post-dominance.
/// </summary>
/// <returns>
///   Returns false if dominance tree has been computed.
/// </returns>
bool Function::ComputePostDominance()
{
    CLOG(1, "<h2>Compute Post Dominance ~S</h2><ol>", this);

    if (Has(Cache_PostDom))
    {
        CLOG(1, "<li>Use cached dominance.</li>");
        CLOG(1, "</ol>~%");
        return false;
    }

    EliminateInfiniteLoop();

    DomTreeBuilder<PostDomCFG, PostDomBBlock, BBlock::DomInfo> oBuilder;
    oBuilder.Build(Extend<PostDomCFG>());
    m_rgfCache |= Cache_PostDom;

    #if _DEBUG
        DomTreeDumper<PostDomCFG, PostDomBBlock, BBlock::DomInfo>::Run(
            Extend<PostDomCFG>(), "Post-" );

    #endif

    CLOG(1, "</ol>~%");
    return true;
} // Function::ComputePostDominance

} // Compiler

} // TinyCl
