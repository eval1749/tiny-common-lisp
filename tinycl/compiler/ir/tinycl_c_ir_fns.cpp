#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Intermediate Representation (IR)
// tinycl_c_ir_fns.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_fns.cpp#13 $
//
#include "../tinycl_c_defs.h"
#include "../cg/tinycl_c_cg.h"
#include "./tinycl_c_ir_loop.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

// ctor
BBlock::BBlock() :
    m_iName(++Context::Get()->m_cBBlocks),
    m_pDfData(NULL),
    m_pLoopNode(NULL)
{
    m_oLabels.Append(new Label(this));
} // BBlock::BBlock

// [A]
Instruction* BBlock::AppendI(Instruction* pI)
{
    //static_cast<Instructions*>(this)->Append(pI);
    Instructions::Append(pI);
    pI->Realize();
    return pI;
} // BBlock::AppendI

// [D]
/// <summary>
///   Returns true if this block dominates specified block.
/// </summary>
bool BBlock::DoesDominate(const BBlock* const that) const
{
    const BBlock* pRunner = that;
    for (;;)
    {
        if (this == pRunner)
        {
            return true;
        }

        pRunner = pRunner->m_pDomInfo->GetParent();
        if (NULL == pRunner)
        {
            return false;
        }
    } // for
} // BBlock::DoesDominate

// [G]
Instruction* BBlock::GetFirstI() const
    { return static_cast<const Instructions*>(this)->GetFirst(); }

Instruction* BBlock::GetLastI() const
    { return static_cast<const Instructions*>(this)->GetLast(); }

static void htmlPrintDomInfo(
    Val             stream,
    const BBlock::DomInfo*  p,
    const char*     psz )
{
    if (NULL == p) return;

    cformat(stream, "<h3>~A</h3>~%", psz);
    cformat(stream, "<table border='1' cellpadding='3'>~%");
    cformat(stream, "<tr><th>Parent</th><td>~S</td></tr>", p->GetParent());

    cformat(stream, "<tr><th>Children</th><td>");
    foreach (BBlock::DomInfo::EnumChild, oEnum, p)
    {
        cformat(stream, " ~S", oEnum.Get());
    } // for each child
    cformat(stream, "</td></tr>~%");

    cformat(stream, "<tr><th>Frontiers</th><td>");
    foreach (BBlock::DomInfo::EnumFrontier, oEnum, p)
    {
        cformat(stream, " ~S", oEnum.Get());
    } // for each child
    cformat(stream, "</td></tr>~%");

    cformat(stream, "</table>~%");
} // htmlPrintDomInfo

void BBlock::HtmlPrint(Val stream, bool fDef) const
{
    if (! fDef)
    {
        html_format(stream, "<a class='b' href='#b~D'>BB~D</a>",
            Fixnum::Encode(GetName()),
            Fixnum::Encode(GetName()) );
        return;
    }

    html_format(stream,
        "<h4 class='b' id='b~D'>BB~D preorder=~D postorder=~D</h4>",
        Fixnum::Encode(GetName()),
        Fixnum::Encode(GetName()),
        Fixnum::Encode(GetPreorder()),
        Fixnum::Encode(GetPostorder()) );

    // Edge
    format(stream, "<h5>Edges</h5>~%");
    format(stream, "<table border='1'>");
    {
        format(stream, "<tr><th>In</th><td>");
        const char* pszDelimiter = "";
        foreach (BBlock::EnumInEdge, oEnum, this)
        {
            write_string(pszDelimiter, stream);
            pszDelimiter = " ";
            oEnum.Get()->HtmlPrint(stream, true);
        } // for each edge
        when (0 == *pszDelimiter) cformat(stream, "<i>none</i>");
        format(stream, "</td></tr>");
    }

    {
        format(stream, "<tr><th>Out</th><td>");
        const char* pszDelimiter = "";
        foreach (BBlock::EnumOutEdge, oEnum, this)
        {
            write_string(pszDelimiter, stream);
            pszDelimiter = " ";
            oEnum.Get()->HtmlPrint(stream, true);
        } // for each edge
        when (0 == *pszDelimiter) cformat(stream, "<i>none</i>");
        format(stream, "</td></tr>");
    }
    format(stream, "</table>");

    if (GetFunction()->Has(Function::Cache_Dom))
    {
        htmlPrintDomInfo(stream, m_pDomInfo,     "DomInfo");
    }

    if (GetFunction()->Has(Function::Cache_PostDom))
    {
        htmlPrintDomInfo(stream, m_pPostDomInfo, "PostDomInfo");
    }

    if (NULL != m_pLoopNode)
    {
        cformat(stream, "<table border='1' cellpadding='3'>~%");
        cformat(stream, "<caption>Loop Information</caption>~%");

        if (NULL != m_pLoopNode->m_pParent)
        {
            cformat(stream, "<tr><td>Parent</td><td>~S</td></tr>",
                m_pLoopNode->m_pParent->m_pBBlock );
        }
        else
        {
            cformat(stream, "<tr><td>Parent</td><td><i>none</i></td></tr>");
        }

        cformat(stream, "<tr><td>Depth</td><td>~D</td></tr>",
            m_pLoopNode->m_nDepth );

        cformat(stream, "<tr><td>Kind</td><td>~A</td></tr>",
            m_pLoopNode->m_eKind == LoopNode::Kind_Leaf     ? "leaf" :
            m_pLoopNode->m_eKind == LoopNode::Kind_Multiple ? "multiple" :
            m_pLoopNode->m_eKind == LoopNode::Kind_Root     ? "root" :
            m_pLoopNode->m_eKind == LoopNode::Kind_Single   ? "single" :
                                                    "unknown" );

        cformat(stream, "<tr><td>Children</td><td>");
        const char* psz = "";
        foreach(LoopNode::EnumChild, oEnum, m_pLoopNode)
        {
            write_string(psz, stream);
            psz = " ";
            oEnum.Get()->m_pBBlock->HtmlPrint(stream);
        } // for each child
        cformat(stream, "</td></tr>~%");

        cformat(stream, "</table>");
    }

    // Instructions
    format(stream, "<h5>Instructions</h5>~%");
    format(stream, "<table>~%");
    foreach (EnumI, oEnum, this)
    {
        Instruction* pI = oEnum.Get();
        format(stream, "<tr>");
        pI->HtmlPrint(stream, true);
        format(stream, "</tr>~%");
    } // for each instruction
    format(stream, "</table>~%");
} // BBlock::HtmlPrint

Instruction* BBlock::InsertAfterI(Instruction* pI, Instruction* pRef)
{
    static_cast<Instructions*>(this)->InsertAfter(pI, pRef);
    pI->Realize();
    CLOG(3, "<li>insert ~S after ~S</li>~%", pI, pRef);
    return pI;
} // BBlock::InsertAfterI

Instruction* BBlock::InsertBeforeI(Instruction* pI, Instruction* pRefI)
{
    if (NULL == pRefI)
    {
        return AppendI(pI);
    }

    CLOG(3, "<li>insert ~S before ~S</li>~%", pI, pRefI);
    static_cast<Instructions*>(this)->InsertBefore(pI, pRefI);
    pI->Realize();
    return pI;
} // BBlock::InsertBeforeI

bool BBlock::IsExitBB() const
{
    Instruction* pI = GetLastI();
    when (NULL == pI) return false;
    return NULL != pI->DynamicCast<ExitI>();
} // BBlock::IsExitBB

Instruction* BBlock::MoveBeforeI(Instruction* pI, Instruction* pRefI)
{
    CLOG(3, "<li>move ~S before ~S</li>~%", pI, pRefI);

    pI->OnMove(this);

    pI->GetBB()->Instructions::Delete(pI);
    if (NULL == pRefI)

    {
        return Instructions::Append(pI);
    }
    else
    {
        ASSERT(pRefI->GetBBlock() == this);
        return Instructions::InsertBefore(pI, pRefI);
    }
} // BBlock::MoveBeforeI

void BBlock::MoveLabelsTo(BBlock* pNewBB)
{
    while (Label* pLabel = m_oLabels.GetFirst())
    {
        pLabel->SetBB(pNewBB);
        m_oLabels.Delete(pLabel);
        pNewBB->m_oLabels.Append(pLabel);
    } // while
} // MoveLabelsTo

/// <summary>
///   Redirect one edge from pOldBB to this bblock to pNewBB.
/// </summary>
/// <param name="pNewBB">A new start block.</param>
/// <param name="pOldBB">An old start block.</param>
void BBlock::RedirectEdgeFrom(BBlock* pNewBB, BBlock* pOldBB)
{
    CLOG_SECTION(2, "redirect/from ~S=>~S to ~S=>~S",
        pOldBB, this, pNewBB, this );

    redirectEdgeFrom(pNewBB, pOldBB);
    replacePhiOperands(pNewBB, pOldBB);
} // BBlock::RedirectEdgeFrom

/// <summary>
///   Redirect one edge to pOldBB to this bblock to pNewBB.
/// </summary>
/// <param name="pNewBB">A new destination block.</param>
/// <param name="pOldBB">An old destination block.</param>
void BBlock::RedirectEdgeTo(BBlock* pNewBB, BBlock* pOldBB)
{
    CLOG_SECTION(2, "redirect/to ~S=>~S to ~S=>~S",
        this, pOldBB, this, pNewBB );

    redirectEdgeTo(pNewBB, pOldBB);

    foreach (EnumI, oEnum, pOldBB)
    {
        PhiI* pPhiI = oEnum.Get()->DynamicCast<PhiI>();
        if (NULL == pPhiI)
        {
            break;
        }

        pPhiI->RemoveOperand(pPhiI->GetOperandBox(this));
    } // for insn

    replaceLabels(pNewBB, pOldBB);
} // BBlock::RedirectEdgeTo

/// <summary>
///   Remove critical edget between this block and pSucc.
/// </summary>
/// <param name="pSucc">A successor</param>
/// <returns>BBlock* or null</returns>
BBlock* BBlock::RemoveCriticalEdge(BBlock* const pSucc)
{
    BBlock* const pPred = this;

    if (! pPred->HasMoreThanOneSucc())
    {
        return NULL;
    }

    if (! pSucc->HasMoreThanOnePred())
    {
        return NULL;
    }

    CLOG_SECTION(2, "remove critical edge ~S=>~S", pPred, pSucc);

    BBlock* const pCurr = new BBlock;
    pPred->GetFunction()->InsertBefore(pCurr, pSucc);

    pPred->redirectEdgeTo(pCurr, pSucc);
    pPred->replaceLabels(pCurr, pSucc);

    pSucc->replacePhiOperands(pCurr, pPred);

    pCurr->AppendI(new JumpI(pSucc));

    return pCurr;
} // BBlock::RemoveCriticalEdge

void BBlock::RemoveI(Instruction* pI)
{
    ASSERT(pI->GetBB() == this);
    CLOG(3, "<li><b class='r'>remove</b> ~S</li>~%", pI);
    pI->Unrealize();
    Instructions::Delete(pI);
} // BBlock::RemoveI

Instruction* BBlock::ReplaceI(Instruction* pNewI, Instruction* pOldI)
{
    ASSERT(pOldI->GetBB() == this);
    InsertBeforeI(pNewI, pOldI);
    RemoveI(pOldI);
    return pNewI;
} // BBlock::ReplaceI

void BBlock::replaceLabels(BBlock* const pNewBB, BBlock* const pOldBB)
{
    foreach (Instruction::EnumOperand, oEnum, GetLastI())
    {
        OperandBox* const pBox = oEnum.GetBox();
        if (Label* const pLabel = pBox->GetOperand()->DynamicCast<Label>())
        {
            if (pLabel->GetBB() == pOldBB)
            {
                CLOG(2, "<li>update ~S => ~S</li>~%",
                    pBox->GetI(), pNewBB );

                pBox->SetOperand(pNewBB->GetLabel());
            }
        }
    } // for each box
} // BBlock::replaceLabels

void BBlock::replacePhiOperands(
    BBlock* const pNewBB, 
    BBlock* const pOldBB )
{
    foreach (BBlock::EnumI, oEnum, this)
    {
        PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
        if (NULL == pPhiI) 
        {
            break;
        }

        PhiOperandBox* const pBox = pPhiI->GetOperandBox(pOldBB);
        pBox->SetBB(pNewBB);
    } // for each phi
} // BBlock::replacePhiOperands

/// <summary>
///   Split this bblock before specified instruction.
/// </summary>
/// <returns>A newly created block contains pRefI</returns>
/// <param name="pRefI">This block is split after this instruction.</param>
BBlock* BBlock::SplitBefore(Instruction* pRefI)
{
    ASSERT(NULL != pRefI);
    ASSERT(pRefI->GetBBlock() == this);

    CLOG(2, "<li>split ~S before ~S<ol>~%", this, pRefI);

    BBlock* pAfterBB = new BBlock();

    getParent()->InsertAfter(pAfterBB, this);

    // Move instructions to new bblock
    {
        Instruction* pRunnerI = pRefI;
        while (NULL != pRunnerI)
        {
            Instruction* pNextI = pRunnerI->GetNext();
            pAfterBB->MoveBeforeI(pRunnerI, NULL);
            pRunnerI = pNextI;
        } // while
    }

    for (;;)
    {
        EnumSucc oEnum(this);
        if (oEnum.AtEnd())
        {
            break;
        }
        oEnum.Get()->RedirectEdgeFrom(pAfterBB, this);
    } // while

    AppendI(new JumpI(pAfterBB));

    CLOG(2, "</ol></li>~%");

    return pAfterBB;
} // BBlock::SplitBefore

// [C]
void CfgEdge::HtmlPrint(Val stream, bool fDef) const
{
    static const char* const k_rgpszEdge[] =
    {
        "",            // normal
        "(exit)",
        "(nonlocal)",
        "(pseudo)",
    }; // k_rgpwszEdge

    if (fDef)
    {
        cformat(stream, "<a id='e~X'>~S=>~S~A<a ~A>x~D~A</a></a>",
            reinterpret_cast<Int>(this),
            GetFrom(),
            GetTo(),
            k_rgpszEdge[GetKind()],
            IsBackward() ? " class='eb'" : "",
            GetCount(),
            IsBackward() ? "!" : "" );
    }
    else
    {
        cformat(stream, "<a href='#e~X'>~S=>~S~A<a ~A>x~D~A</a></a>",
            reinterpret_cast<Int>(this),
            GetFrom(),
            GetTo(),
            k_rgpszEdge[GetKind()],
            IsBackward() ? " class='eb'" : "",
            GetCount(),
            IsBackward() ? "!" : "" );
    }
} // CfgEdge::HtmlPrint

void CgEdge::HtmlPrint(Val stream, bool) const
{
    cformat(stream, "~S=>~S", GetFrom(), GetTo());
} // CgEdge::HtmlPrint

// [M]
void Module::Clean()
{
    uint nCount = 0;
    for (;;)
    {
        nCount += 1;

        CLOG(1, "<h2>Module Clean[~D]</h2>", nCount);

        WorkList_<Function> oRemoves;

        foreach (Module::EnumFunction, oEnum, this)
        {
            Function* const pFun = oEnum.Get();
            switch (pFun->GetFlavor())
            {
            case Function::Flavor_Finally:
            case Function::Flavor_Toplevel:
                break;

            default:
                if (pFun->HasUseSite())
                {
                    // used
                }
                else if (pFun->HasCallSite())
                {
                    // called
                }
                else
                {
                    oRemoves.Push(pFun);
                }
                break;
            } // switch
        } // for

        if (oRemoves.IsEmpty())
        {
            break;
        }

        while (! oRemoves.IsEmpty())
        {
            Function* const pFun = oRemoves.Pop();
            CLOG_SECTION(2, "Remove ~S", pFun);
            RemoveFunction(pFun);
        }
    } // for
} // Module::Clean

void Module::HtmlPrint(Val const stream, bool const fDef) const
{
    if (! fDef)
    {
        cformat(stream, "[module @ ~X]", this);
    }
    else
    {
        format(stream, "<hr/><h2 id='fall'>Functions</h2>~%");
        format(stream, "<ol>~%");

        foreach (Module::EnumFunction, oEnum, this)
        {
            Function* const pFun = oEnum.Get();
            cformat(stream, "<li>~S ~S</li>~%", pFun, pFun->GetFunty());
        } // for each fun

        format(stream, "</ol>~%");

        foreach (EnumFunction, oEnum, this)
        {
            oEnum.Get()->HtmlPrint(stream, true);

            format(stream, "<a href='#fall'>Functions</a><br/><br/>~%");
        } // for each function
    }
} // Module::HtmlPrint

void Module::RemoveFunction(Function* const pFun)
{
    pFun->MakeVoid();
    static_cast<LayoutList*>(this)->Delete(pFun);
} // Module::RemoveFunction

} // Compiler
} // TinyCl
