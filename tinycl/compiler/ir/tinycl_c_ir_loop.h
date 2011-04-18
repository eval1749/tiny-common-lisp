// -*- Mode: C++ -*-
//
// TinyCl - TinyCl Compiler
// compiler/tinycl_c_defs.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_loop.h#2 $
//
#if !defined(INCLUDE_tinycl_c_loop_h)
#define INCLUDE_tinycl_c_loop_h

//#include "../../tinycl.h"

namespace TinyCl
{

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Loop Tree Node
//
class LoopNode :
    public LocalObject,
    public WorkListItem_<LoopNode>
{
    public: enum Kind
    {
        Kind_Leaf,
        Kind_Multiple,
        Kind_Root,
        Kind_Single,
    }; // Kind

    public: LoopNode*   m_pParent;
    public: LoopNode*   m_pChild;
    public: LoopNode*   m_pSibling;
    public: LoopNode*   m_pGenerator;
    public: BBlock*     m_pBBlock;
    public: uint        m_nDepth;
    public: Kind        m_eKind;

    // ctor
    public: LoopNode(BBlock* pBBlock) :
        m_nDepth(0),
        m_eKind(Kind_Leaf),
        m_pBBlock(pBBlock),
        m_pChild(NULL),
        m_pParent(NULL),
        m_pGenerator(NULL),
        m_pSibling(NULL) {}

    // [A]
    public: void AddBody(LoopNode* pNode)
    {
        ASSERT(NULL == pNode->m_pParent);

        uint nDepth = m_nDepth + 1;

        pNode->m_nDepth   = nDepth;
        pNode->m_pParent  = this;
        pNode->m_pSibling = m_pChild;

        m_pChild = pNode;

        if (Kind_Leaf == pNode->m_eKind) return;

        nDepth += 1;
        pNode->m_nDepth = nDepth;
        foreach (EnumChild, oEnum, pNode)
        {
            oEnum.Get()->m_nDepth = nDepth;
        } // for each body
    } // AddBody

    // [E]
    public: class EnumGenerator
    {
        private: LoopNode* m_pRunner;

        public: EnumGenerator(LoopNode* pNode) :
            m_pRunner(pNode->m_pGenerator) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: LoopNode* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pGenerator; }
    }; // EnumGenerator

    public: class EnumChild
    {
        private: LoopNode*   m_pRunner;

        public: EnumChild(LoopNode* pNode) :
            m_pRunner(pNode->m_pChild) {}

        public: EnumChild(const LoopNode* pNode) :
            m_pRunner(const_cast<LoopNode*>(pNode->m_pChild)) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: LoopNode* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pSibling; }
    }; // EnumChild

    // [G]
    public: LoopNode* GetAncestor() const
    {
        LoopNode* pNode = const_cast<LoopNode*>(this);
        while (NULL != pNode->m_pParent) { pNode = pNode->m_pParent; }
        return pNode;
    } // GetAncestor
}; // LoopNode

} // Compiler

} // TinyCl

#endif //!defined(INCLUDE_tinycl_c_loop_h)
