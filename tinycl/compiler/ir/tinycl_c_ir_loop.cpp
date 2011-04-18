#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Loop Tree
// ir/ir_loop.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_loop.cpp#2 $
//
//
// Description:
//  This file contains implementation of computing loop tree of [1].
//
// References:
//  [1] 
#include "../tinycl_c_defs.h"
#include "./tinycl_c_ir_loop.h"


namespace TinyCl
{

namespace Compiler
{

static void findBody(LoopNode* pHead, LoopNode::Kind eKind)
{
    pHead->m_eKind = eKind;

    WorkList_<LoopNode> oQueue;
    foreach (LoopNode::EnumGenerator, oEnum, pHead)
    {
        LoopNode* pNode = oEnum.Get();
        LoopNode* pAncestor = pNode->GetAncestor();
        if (! pAncestor->IsInList())
        {
            pHead->AddBody(pAncestor);
            oQueue.Push(pAncestor);
        }
    } // for each generator

    while (! oQueue.IsEmpty())
    {
        LoopNode* pNode = oQueue.Pop();
        foreach (BBlock::EnumInEdge, oEnum, pNode->m_pBBlock)
        {
            LoopNode* pPred = oEnum.Get()->GetFrom()->GetLoopNode();
            if (pPred != pHead)
            {
                LoopNode* pAncestor = pPred->GetAncestor();
                if (pAncestor != pHead)
                {
                    pHead->AddBody(pAncestor);
                    oQueue.Push(pAncestor);
                }
            }
        } // for each pred
    } // for each elt
} // findBody

static void findLoop(LoopNode* pNode)
{
    LoopNode* pEnd = pNode;
    bool fBackEdge = false;
    WorkList_<LoopNode> oLoop;
    foreach (BBlock::EnumInEdge, oEnum, pNode->m_pBBlock)
    {
        CfgEdge* pEdge = oEnum.Get();
        if (pEdge->IsBackward())
        {
            fBackEdge = true;

            LoopNode* pPred = pEdge->GetFrom()->GetLoopNode();

            pEnd = pEnd->m_pBBlock->GetCommonDominator(pPred->m_pBBlock)->
                GetLoopNode();

            if (! pPred->IsInList() && pPred != pNode)
            {
                oLoop.Push(pPred);
            }
        } // if backedge
    } // for each pred

    if (oLoop.IsEmpty())
    {
        if (fBackEdge)
        {
            pNode->m_eKind = LoopNode::Kind_Single;
            pNode->m_nDepth += 1;
        }
        return;
    }

    while (! oLoop.IsEmpty())
    {
        LoopNode* pLoopNode = oLoop.Pop();
        pLoopNode->m_pGenerator = pEnd->m_pGenerator;
        pEnd->m_pGenerator = pLoopNode;
    } // while

    if (pEnd == pNode)
    {
        findBody(pNode, LoopNode::Kind_Single);
    }
} // findLoop

#if 0
static void dumpLoopNode(const LoopNode* pHead)
{
    const char* psz;

    switch (pHead->m_eKind)
    {
    case LoopNode::Kind_Leaf:
        CLOG(2, "<li>~S/~D</li>~%",
            pHead->m_pBBlock, pHead->m_nDepth );
        return;
    case LoopNode::Kind_Root:     psz = "Root"; break;
    case LoopNode::Kind_Single:   psz = "Single"; break;
    case LoopNode::Kind_Multiple: psz = "Multiple"; break;
    default:
        CAN_NOT_HAPPEN();
    } // switch kind

    CLOG(2, "<li>~A: ~S/~D",
        psz, pHead->m_pBBlock, pHead->m_nDepth );

    CLOG(2, "<ul>~%");
    foreach (LoopNode::EnumChild, oEnum, pHead)
    {
        dumpLoopNode(oEnum.Get());
    } // for each member
    CLOG(2, "</ul></li>~%");
} // dumpLoopNode

void ir_dump_loop_tree(Function* pFun)
{
    CLOG(2, "<h2>Loop Tree of ~S</h2>~%", pFun);
    dumpLoopNode(pFun->GetEntryBB()->GetLoopNode());
} // ir_dump_loop_tree
#endif

LoopNode* Function::ComputeLoop()
{
    NumberInstructions();

    foreach (Function::EnumBBlock, oEnum, this)
    {
        BBlock* pBBlock = oEnum.Get();
        if (NULL == pBBlock->GetLoopNode())
        {
            pBBlock->SetLoopNode(new LoopNode(pBBlock));
        }
    } // for each bblock

    foreach (Function::EnumBBlockPostorder, oEnum, this)
    {
        BBlock* pBBlock = oEnum.Get();
        LoopNode* pNode = pBBlock->GetLoopNode();
        if (NULL != pNode->m_pGenerator)
        {
            findBody(pNode, LoopNode::Kind_Multiple);
        }
        findLoop(pNode);
    } // for each bblock

    LoopNode* pEntry = GetEntryBB()->GetLoopNode();
    pEntry->m_pGenerator = GetExitBB()->GetLoopNode();

    findBody(pEntry, LoopNode::Kind_Root);

    GetEntryBB()->GetLoopNode()->m_nDepth = 1;
#if 0
    if (nil != Session::Get()->GetPass()->GetLogStream(1))
    {
        ir_dump_loop_tree(this);
    }
#endif
    return pEntry;
} // Function::ComputeLoop

} // Compiler

} // TinyCl
