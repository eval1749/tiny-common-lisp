#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Graph
// tinycl_c_graph.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_graph.cpp#2 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

namespace Compiler
{

namespace
{

template<class Node_, class Parent_>
class DfsWalker_
{
    private: typedef Graph_<Node_, Parent_> Graph;

    private: typedef  ChildList_<Node_, Parent_>          LayoutList;
    private: typedef  DoubleLinkedList_<Node_, Preorder>  PreorderList;
    private: typedef  DoubleLinkedList_<Node_, Postorder> PostorderList;

    public: static void Run(Graph* pGraph)
    {
        DfsWalker_<Node_, Parent_> oWalker(pGraph);
        oWalker.run();
    } // Run

    private: uint   m_nPreorder;
    private: uint   m_nPostorder;
    private: Graph* m_pGraph;

    // ctor
    private: DfsWalker_(Graph* pGraph) :
        m_nPreorder(0),
        m_nPostorder(0),
        m_pGraph(pGraph)
    {
        foreach (LayoutList::Enum, oEnum, m_pGraph)
        {
            oEnum.Get()->SetPreorder(0);
            oEnum.Get()->SetPostorder(0);
        } // for each node

        static_cast<PreorderList*>(m_pGraph)->DeleteAll();
        static_cast<PostorderList*>(m_pGraph)->DeleteAll();
    } // DfsWalker_

    // [I]
    private: static bool isVisited(Node_* pNode)
        { return 0 != pNode->GetPreorder(); }

    // [R]
    private: void run()
    {
        walk(static_cast<LayoutList*>(m_pGraph)->GetFirst());
    } // run

    // [W]
    private: void walk(Node_* pCurr)
    {
        ASSERT(! isVisited(pCurr));

        m_nPreorder += 1;
        pCurr->SetPreorder(m_nPreorder);

        static_cast<PreorderList*>(m_pGraph)->Append(pCurr);

        foreach (Node_::EnumOutEdge, oEnum, pCurr)
        {
            // ! visited    tree
            // 0 == post    back
            // pre < s,pre  forward
            // otherwise    cross
            Node_* pSucc = oEnum.GetNode();
            if (! isVisited(pSucc))
            {
                oEnum.Get()->SetBackward(false);
                walk(pSucc);
            }
            else
            {
                oEnum.Get()->SetBackward(0 == pSucc->GetPostorder());
            }
        } // for each succ

        m_nPostorder += 1;
        pCurr->SetPostorder(m_nPostorder);

        static_cast<PostorderList*>(m_pGraph)->Append(pCurr);
    } // walk
}; // DfsWalker_

template<class Node_, class Parent_>
class SccWalker_
{
    private: typedef Graph_<Node_, Parent_>     Graph;
    private: typedef ChildList_<Node_, Parent_> LayoutList;

    private: uint    m_nSccNum;
    private: Graph*  m_pGraph;
    private: Node_*  m_pStackTop;

    // SccWalker
    private: SccWalker_(Graph* pGraph) :
        m_nSccNum(0),
        m_pGraph(pGraph),
        m_pStackTop(NULL) {}
        
    public: static void Run(Graph* pGraph)
    {
        SccWalker_ oWalker(pGraph);

        foreach (LayoutList::Enum, oEnum, pGraph)
        {
            oEnum.Get()->ResetScc();
        } // for each node

        oWalker.dfs(static_cast<LayoutList*>(pGraph)->GetFirst());
    } // SccWalker

    // dfs
    private: uint dfs(Node_* pCurr)
    {
        ASSERT(0 == pCurr->m_nSccNum);

        m_nSccNum += 1;
        pCurr->m_nSccNum  = m_nSccNum;
        push(pCurr);

        uint nMin = m_nSccNum;
        foreach (Node_::EnumOutEdge, oEnum, pCurr)
        {
            Node_* pSucc = oEnum.GetNode();

            uint nNum = pSucc->m_nSccNum;
            if (0 == nNum)
            {
                nNum = dfs(pSucc);
            }

            nMin = min(nMin, nNum);
        } // for each succ

        if (pCurr->m_nSccNum == nMin)
        {
            static_cast<Parent_::SccList*>(m_pGraph)->Append(pCurr);

            Node_* pSccNext = NULL;
            Node_* pSccId = pCurr;
            for (;;)
            {
                Node_* pTop = pop();

                pTop->m_nSccNum  = static_cast<uint>(-1);
                pTop->m_pSccId   = pSccId;
                pTop->m_pSccNext = pSccNext;

                pSccNext = pTop;

                if (pTop == pCurr) break;
            } // for
        } // if

        return nMin;
    } // dfs

    // push
    void push(Node_* p)
    { 
        p->m_pSccNext = m_pStackTop; 
        m_pStackTop = p;
    } // push

    // pop
    Node_* pop()
    {
        Node_* p = m_pStackTop;
        m_pStackTop = m_pStackTop->m_pSccNext;
        return p;
    } // pop
}; // SccWalker_

} // namespace

template<class Node_, class Parent_> void
Graph_<Node_, Parent_>::ComputeScc()
{
    if (Has(Cache_Scc)) return;
    CLOG(1, "<li>SCC walk on ~S</li>~%", static_cast<Parent_*>(this));
    SccWalker_<Node_, Parent_>::Run(this);
    m_rgfCache |= Cache_Scc;
} // Graph_<Node_, Parent_>::ComputeScc

template<class Node_, class Parent_> void
Graph_<Node_, Parent_>::prepareDfsTraversal()
{
    if (Has(Cache_Dfs)) return;

    CLOG(1, "<li>DFS walk on ~S</li>~%", static_cast<Parent_*>(this));
    DfsWalker_<Node_, Parent_>::Run(this);
    m_rgfCache |= Cache_Dfs;
} // Graph_<Node_, Parent_>::prepareDfsTraversal

template void Graph_<Function, Module>::ComputeScc();
template void Graph_<BBlock, Function>::prepareDfsTraversal();
template void Graph_<Function, Module>::prepareDfsTraversal();

} // Compiler

} // TinyCl
