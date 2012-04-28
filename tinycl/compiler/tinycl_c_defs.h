// -*- Mode: C++ -*-
//
// TinyCl - TinyCl Compiler
// compiler/tinycl_c_defs.h
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/tinycl_c_defs.h#78 $
//
#if !defined(INCLUDE_tinycl_c_defs_h)
#define INCLUDE_tinycl_c_defs_h

#include "../tinycl.h"

namespace TinyCl
{

namespace Compiler
{

#define html_format format
void cformat(Val, const char*, ...);
void cformatv(Val, const char*, va_list);

#if !defined(interface)
    #define interface struct
#endif

interface IMm;

class Context;
class Mm;
class Pass;
class SubPass;
class Target;   // See tinycl_c_cg.h

class BBlock;
class CfgEdge;
class CgEdge;
class Function;
class Instruction;
class Module;
class Type;

class LoopNode;

class Operand;
    class Immediate;
        class FunLit;
        class FunName;
        class Integer;
        class Label;
    class Output;
        class CgOutput;
            class Physical;
            class StackSlot;
            class ThreadSlot;
        class SsaOutput;
            class Bool;
            class DynConstant;
            class Pseudo;
            class Register;
        class TrueOperand;
        class UnreachableOutput;
        class Values;
        class VoidOutput;
    class Variable;

class OperandBox;
    class FunctionOperandBox;
    class VariableOperandBox;

class BitVec;
class DataFlowData;

class Type;
    class TyClass;
    class TyFunction;
        class TyUnknownFunction;
    class TyForeign;
    class TyValues;
        class TyValuesRestT;

#define definstruction(mp_name, mp_format) class mp_name ## I;
#define defstatic(mp_cty, mp_cname) extern mp_cty* mp_cname;
#define defstatic2(mp_cty, mp_cname, mp_a) extern mp_cty* mp_cname;
#include "./tinycl_compiler.inc"
#undef defstatic
#undef defstatic2

template<class Base_>
class Extendable_
{
    public: template<class T> T* Extend()
        { return reinterpret_cast<T*>(static_cast<Base_*>(this)); }

    public: template<class T> T* Extend() const
    {
        Base_* pThis = const_cast<Base_*>(
            static_cast<const Base_*>(this) );

        return reinterpret_cast<T*>(pThis);
    } // Extend
}; // Extendable_

class WithIndex
{
    private: int m_i;

    // ctor
    public: WithIndex() : m_i(0) {}

    // [G]
    public: int GetIndex() const { return m_i; }

    // [S]
    public: int SetIndex(int n) { return m_i = n; }
}; // WithIndex

class WithWorkArea
{
    private: int    m_i;
    private: void*  m_pv;

    // ctor
    public: WithWorkArea() : m_i(0), m_pv(0) {}

    // [G]
    public: int GetFlag() const
        { return m_i; }

    public: template<typename T> T* GetWork() const
        { return reinterpret_cast<T*>(m_pv); }

    // [S]
    public: int   SetFlag(int i)    { return m_i = i; }
    public: void* SetWork(void* pv) { return m_pv = pv; }
}; // WithWorkArea

class LocalObject
{
    public: void operator delete(void*) { CAN_NOT_HAPPEN(); }
    public: void operator delete(void*, IMm*) {}

    public: void* operator new(size_t);
    public: void* operator new(size_t, IMm*);
}; // LocalObject

class Object
{
    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const = 0;
}; // Object


class None {};
class Backward {};
class Forward {};
class Layout {};
class Preorder {};
class Postorder {};
class Scc {};


template<class Item_, class Node_>
class Edge_ :
    public DoubleLinkedItem_<Item_, Backward>,
    public DoubleLinkedItem_<Item_, Forward>
{
    public: enum Kind
    {
        Kind_Normal,

        Kind_Exit,
        Kind_Nonlocal,
        Kind_Pseudo,
    }; // Kind

    private: Kind   m_eKind;
    private: bool   m_fBackward;
    private: uint   m_nCount;
    private: Node_* m_pFrom;
    private: Node_* m_pTo;

    // ctor
    public: Edge_(Node_* pIn, Node_* pOut, Kind eKind) :
        m_eKind(eKind),
        m_fBackward(false),
        m_nCount(0),
        m_pFrom(pIn),
        m_pTo(pOut) {}

    // [G]
    public: uint   GetCount() const { return m_nCount; }
    public: Node_* GetFrom()  const { return m_pFrom; }
    public: Kind   GetKind()  const { return m_eKind; }
    public: Node_* GetTo()    const { return m_pTo; }

    // [I]
    public: bool IsBackward() const { return m_fBackward; }

    // [S]
    public: bool   SetBackward(bool f) { return m_fBackward = f; }
    public: uint   SetCount(uint n)    { return m_nCount = n; }
    public: Node_* SetFrom(Node_* p)   { return m_pFrom = p; }
    public: Kind   SetKind(Kind e)     { return m_eKind = e; }
    public: Node_* SetTo(Node_* p)     { return m_pTo = p; }
}; // Edge_


template<class Node_, class Parent_>
class SccNode_ :
    public DoubleLinkedItem_<Node_, Scc>
{
    public: uint    m_nSccNum;
    public: Node_*  m_pSccId;
    public: Node_*  m_pSccNext;

    public: SccNode_()
        { ResetScc(); }

    // [E]
    public: class EnumSccMember
    {
        private: Node_* m_pRunner;

        public: EnumSccMember(Node_* p) :
            m_pRunner(p) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: Node_* Get() const
            { ASSERT(!AtEnd()); return m_pRunner; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_pRunner = m_pRunner->GetSccNode()->m_pSccNext;
        } // Next
    }; // EnumSccMember

    // [I]
    private: bool IsSccLeader() const
        { return m_pSccId == this; }

    // [R]
    public: void ResetScc(Node_* pSccId = NULL)
    {
        m_nSccNum  = 0;
        m_pSccId   = pSccId;
        m_pSccNext = NULL;
    } // ResetScc
}; // SccNode_

template<class Node_>
class SccList_ :
    public DoubleLinkedList_<Node_, Scc> {};

template<class Node_, class Parent_>
class Graph_ :
    public ChildList_<Node_, Parent_>,
    public DoubleLinkedList_<Node_, Preorder>,
    public DoubleLinkedList_<Node_, Postorder>,
    public SccList_<Node_>
{
    protected: typedef Graph_<Node_, Parent_>              Graph;
    protected: typedef ChildList_<Node_, Parent_>          LayoutList;
    protected: typedef DoubleLinkedList_<Node_, Preorder>  PreorderList;
    protected: typedef DoubleLinkedList_<Node_, Postorder> PostorderList;
    public:    typedef SccList_<Node_>                     SccList;

    public: enum Cache
    {
        Cache_Dfs       = 1 << 0,   // 1
        Cache_Scc       = 1 << 1,   // 2
        Cache_Dom       = 1 << 2,   // 4
        Cache_PostDom   = 1 << 3,   // 8
    }; // Cache

    protected: uint m_rgfCache;

    // ctor
    protected: Graph_() : m_rgfCache(0) {}

    // [C]
    public: void ComputeScc();

    // [E]
    public: class EnumScc : public SccList::Enum
    {
        public: EnumScc(const Graph* p) :
            SccList::Enum(static_cast<const SccList*>(p)) {}
    }; // EnumScc

    public: class EnumSccReverse : public SccList::EnumReverse
    {
        public: EnumSccReverse(const Graph* p) :
            SccList::EnumReverse(static_cast<const SccList*>(p)) {}
    }; // EnumSccReverse

    // [H]
    public: bool Has(Cache e) const
        { return 0 != (m_rgfCache & e); }

    // [P]
    private: void prepareDfsTraversal();

    protected: LayoutList* prepareTraversal(LayoutList* p)
        { return p; }

    protected: PreorderList* prepareTraversal(PreorderList* p)
        { prepareDfsTraversal(); return p; }

    protected: PostorderList* prepareTraversal(PostorderList* p)
        { prepareDfsTraversal(); return p; }

    // [R]
    public: void RemoveNode(Node_* pNode)
    {
        static_cast<LayoutList*>(this)->Delete(pNode);
        static_cast<PreorderList*>(this)->Delete(pNode);
        static_cast<PostorderList*>(this)->Delete(pNode);
    } // RemoveNode

    // [S]
    public: void SetChanged() { m_rgfCache = 0; }
}; // Graph_

template<class Node_, class Graph_>
class DomInfo_ :
    public DoubleLinkedItem_<DomInfo_<Node_, Graph_>, Node_>,
    public LocalObject
{
    public: typedef DomInfo_<Node_, Graph_>  DomInfo;
    public: typedef DoubleLinkedList_<DomInfo, Node_> ChildList;
    public: typedef SingleLinkedList_<Node_, LocalObject> FrontierList;

    protected: Node_*       m_pNode;
    protected: Node_*       m_pParent;
    protected: ChildList    m_oChildren;
    protected: FrontierList m_oFrontiers;
    protected: uint         m_nDepth;

    public: DomInfo_(Node_* p) :
        m_pNode(p), m_pParent(NULL), m_nDepth(0) {}

    public: ChildList* GetChildren() const
        { return const_cast<ChildList*>(&m_oChildren); }

    public: FrontierList* GetFrontiers() const
        { return const_cast<FrontierList*>(&m_oFrontiers); }

    public: Node_* GetParent() const
        { return m_pParent; }

    public: uint GetDepth()      const { return m_nDepth; }
    public: uint SetDepth(uint nDepth) { return m_nDepth = nDepth; }

    public: void Reset()
    {
        m_pParent = NULL;
        m_oChildren.DeleteAll();
        m_oFrontiers.DeleteAll();
    } // Reset

    public: void SetParent(Node_* pParent)
        { m_pParent = pParent; }

    public: class EnumChild : public ChildList::Enum
    {
        public: EnumChild(const DomInfo* p) :
            ChildList::Enum(&p->m_oChildren) {}

        public: Node_* Get() const
            { return ChildList::Enum::Get()->m_pNode; }
    }; // EnumChild

    public: class EnumFrontier : public FrontierList::Enum
    {
        public: EnumFrontier(const DomInfo* p) :
            FrontierList::Enum(&p->m_oFrontiers) {}
    }; // EnumFrontier
}; // DomInfo_

template<class Parent_, class Node_, class Edge_>
class GraphNode_ :
    public ChildItem_<Node_, Parent_>,
    public DoubleLinkedItem_<Node_, Preorder>,
    public DoubleLinkedItem_<Node_, Postorder>,
    public SccNode_<Node_, Parent_>
{
    public: typedef DoubleLinkedItem_<Edge_, Backward> InEdge;
    public: typedef DoubleLinkedItem_<Edge_, Forward>  OutEdge;

    protected: typedef DoubleLinkedList_<Edge_, Backward> InEdges;
    protected: typedef DoubleLinkedList_<Edge_, Forward>  OutEdges;

    protected: typedef ChildItem_<Node_, Parent_>          LayoutNode;
    protected: typedef DoubleLinkedItem_<Node_, Preorder>  PreorderNode;
    protected: typedef DoubleLinkedItem_<Node_, Postorder> PostorderNode;

    private: typedef GraphNode_<Parent_, Node_, Edge_> GraphNode;

    public: typedef DomInfo_<Node_, Parent_> DomInfo;
    public: typedef SccNode_<Node_, Parent_> SccNode;

    private:   uint         m_nPreorder;
    private:   uint         m_nPostorder;
    protected: InEdges      m_oInEdges;
    protected: OutEdges     m_oOutEdges;
    protected: DomInfo*     m_pDomInfo;
    protected: DomInfo*     m_pPostDomInfo;

    // ctor
    public: GraphNode_() :
        m_nPreorder(0),
        m_nPostorder(0),
        m_pDomInfo(NULL),
        m_pPostDomInfo(NULL) {}

    // [A]
    public: Edge_* AddEdge(Node_* pTo)
    {
        Edge_* pEdge = new Edge_(static_cast<Node_*>(this), pTo);
        m_oOutEdges.Append(pEdge);
        pTo->m_oInEdges.Append(pEdge);
        return pEdge;
    } // AddEdge

    // [C]
    public: uint CountInEdge() const
    {
        uint nCount = 0;
        foreach (EnumInEdge, oEnum, this)
        {
            nCount += 1;
        } // for
        return nCount;
    } // CountInEdge

    public: uint CountOutEdge() const
    {
        uint nCount = 0;
        foreach (EnumOutEdge, oEnum, this)
        {
            nCount += 1;
        } // for
        return nCount;
    } // CountOutEdge

    // [E]
    public: class EnumChild : public DomInfo::EnumChild
    {
        public: EnumChild(const Node_* p) :
            DomInfo::EnumChild(p->m_pDomInfo) {}
    }; // EnumChild

    public: class EnumFrontier : public DomInfo::EnumFrontier
    {
        public: EnumFrontier(const Node_* p) :
            DomInfo::EnumFrontier(p->m_pDomInfo) {}
    }; // EnumFrontier

    public: class EnumInEdge : public InEdges::Enum
    {
        public: EnumInEdge(const GraphNode_* pNode) :
            EnumInEdge::Enum(&pNode->m_oInEdges) {}

        public: Node_* GetNode() const
            { return Get()->GetFrom(); }
    }; // EnumInEdge

    public: class EnumOutEdge : public OutEdges::Enum
    {
        public: EnumOutEdge(const GraphNode_* pNode) :
            EnumOutEdge::Enum(&pNode->m_oOutEdges) {}

        public: Node_* GetNode() const
            { return Get()->GetTo(); }
    }; // EnumOutEdge

    public: class EnumPred : public EnumInEdge
    {
        public: EnumPred(const GraphNode_* pNode) :
            EnumInEdge(pNode) {}

        public: Node_* Get() { return GetNode(); }
    }; // EnumPred

    public: class EnumSucc : public EnumOutEdge
    {
        public: EnumSucc(const GraphNode_* pNode) :
            EnumOutEdge(pNode) {}

        public: Node_* Get() { return GetNode(); }
    }; // EnumSucc

    // [F]
    public: Edge_* FindEdgeFrom(Node_* const pFrom) const
    {
        foreach (EnumInEdge, oEnum, this)
        {
            Edge_* const pEdge = oEnum.Get();
            if (pEdge->GetFrom() == pFrom)
            {
                return pEdge;
            }
        } // for each in edge
        return NULL;
    } // FindEdgeFrom

    public: Edge_* FindEdgeTo(Node_* const pTo) const
    {
        foreach (EnumOutEdge, oEnum, this)
        {
            Edge_* const pEdge = oEnum.Get();
            if (pEdge->GetTo() == pTo)
            {
                return pEdge;
            }
        } // for each out edge
        return NULL;
    } // FindEdgeTo

    // [G]
    public: Node_* GetCommonDominator(Node_* b) const
    {
        Node_* a = static_cast<Node_*>(const_cast<GraphNode*>(this));

        while (a->m_pDomInfo->GetDepth() > b->m_pDomInfo->GetDepth())
            { a = a->m_pDomInfo->GetParent(); }

        while (b->m_pDomInfo->GetDepth() > a->m_pDomInfo->GetDepth())
            { b = b->m_pDomInfo->GetParent(); }

        while (a != b)
        {
            a = a->m_pDomInfo->GetParent();
            b = b->m_pDomInfo->GetParent();
        }

        return a;
    } // GetCommonDominator

    public: Edge_* GetEdge(Node_* pTo) const
    {
        Edge_* pEdge = FindEdgeTo(pTo);
        ASSERT(NULL != pEdge);
        return pEdge;
    } // GetEdge

    public: uint GetPreorder()  const { return m_nPreorder; }
    public: uint GetPostorder() const { return m_nPostorder; }

    protected: Parent_* getParent() const
        { return m_pParent; }

    public: SccNode* GetSccNode() const
        { return const_cast<SccNode*>(static_cast<const SccNode*>(this)); }

    // [H]
    public: bool HasInEdge() const
        { return NULL != m_oInEdges.GetFirst(); }

    public: bool HasMoreThanOnePred() const
    {
        EnumPred oEnum(this);
        if (oEnum.AtEnd()) return false;
        oEnum.Next();
        return ! oEnum.AtEnd();
    } // HasMoreThanOnePred

    public: bool HasMoreThanOneSucc() const
    {
        EnumSucc oEnum(this);
        if (oEnum.AtEnd()) return false;
        oEnum.Next();
        return ! oEnum.AtEnd();
    } // HasMoreThanOneSucc

    public: bool HasOnlyOnePred() const
    {
        EnumPred oEnum(this);
        if (oEnum.AtEnd()) return false;
        oEnum.Next();
        return oEnum.AtEnd();
    } // HasOnlyOnePred

    public: bool HasOutEdge() const
        { return NULL != m_oOutEdges.GetFirst(); }

    // [R]
    protected: void redirectEdgeFrom(Node_* pNew, Node_* pOld)
    {
        Edge_* pEdge = pOld->GetEdge(static_cast<Node_*>(this));
        pOld->m_oOutEdges.Delete(pEdge);
        pEdge->SetFrom(pNew);
        pNew->m_oOutEdges.Append(pEdge);

        getParent()->SetChanged();
    } // redirectEdgeFrom

    protected: void redirectEdgeTo(Node_* pNew, Node_* pOld)
    {
        Edge_* pEdge = GetEdge(pOld);

        pOld->m_oInEdges.Delete(pEdge);
        pEdge->SetTo(pNew);
        pNew->m_oInEdges.Append(pEdge);

        getParent()->SetChanged();
    } // redirectEdgeTo

    public: void RemoveEdge(Edge_* pEdge)
    {
        Node_* pTo = pEdge->GetTo();

        pTo->m_oInEdges.Delete(pEdge);
        m_oOutEdges.Delete(pEdge);

        getParent()->SetChanged();
    } // RemoveEdge

    public: void RemoveOutEdge(Node_* pTo)
        { RemoveEdge(GetEdge(pTo)); }

    // [S]
    protected: Parent_* setParent(Parent_* p)
        { return m_pParent = p; }

    public: uint SetPreorder(uint n)  { return m_nPreorder  = n; }
    public: uint SetPostorder(uint n) { return m_nPostorder = n; }
}; // GraphNode_

#define class_Enum_(mp_Type, mp_unit, mp_List) \
    public: class Enum ## mp_unit : public mp_List::Enum \
    { \
        public: Enum ## mp_unit(const mp_Type* p) : \
            mp_List::Enum( \
                const_cast<mp_Type*>(p)->prepareTraversal( \
                    static_cast<mp_List*>(const_cast<mp_Type*>(p)) ) ) {} \
    }; \
    public: class Enum ## mp_unit ## Reverse : public mp_List::EnumReverse \
    { \
        public: Enum ## mp_unit ## Reverse(const mp_Type* p) : \
            mp_List::EnumReverse( \
                const_cast<mp_Type*>(p)->prepareTraversal( \
                    static_cast<mp_List*>(const_cast<mp_Type*>(p)) )) {} \
    }; // class_Enum_

class CfgEdge :
    public Edge_<CfgEdge, BBlock>,
    public LocalObject,
    public Object,
    public WorkListItem_<CfgEdge>
{
    private: typedef Edge_<CfgEdge, BBlock> Edge;

    // ctor
    public: CfgEdge(
        BBlock* pFrom = NULL,
        BBlock* pTo   = NULL,
        Kind    eKind = Kind_Normal ) :
            Edge(pFrom, pTo, eKind) {}

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;
}; // CfgEdge

class CgEdge :
    public LocalObject,
    public Object,
    public Edge_<CgEdge, Function>
{
    private: typedef Edge_<CgEdge, Function> Edge;

    public: uint m_cUsers;

    public: CgEdge(
        Function*   pIn,
        Function*   pOut,
        Kind        eKind = Kind_Normal) :
            m_cUsers(0),
            Edge(pIn, pOut, eKind) {}

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;
}; // CgEdge

class Operand :
    public Object,
    public Castable_<Operand>,
    public LocalObject
{
    public: typedef DoubleLinkedList_<OperandBox, Operand> Users;

    // [C]
    public: virtual Operand* Compute() const
        { return NULL; }

    // [E]
    public: virtual bool Equal(const Operand* const that) const
        { return this == that; }

    // [G]
    public: virtual const Type* GetTy() const { CAN_NOT_HAPPEN(); }

    // [I]
    public: virtual bool IsFalse()     const { return false; }
    public: virtual bool IsTrue()      const { return true; }

    // [R]
    public: virtual void Realize(OperandBox*) {}

    // [U]
    public: virtual void Unrealize(OperandBox*) {}
}; // Operand

class OperandBox :
    public Castable_<OperandBox>,
    public ChildItem_<OperandBox, Instruction>,
    public DoubleLinkedItem_<OperandBox, Operand>,
    public LocalObject,
    public WorkListItem_<OperandBox>
{
    public: static const char* Kind_() { return "OperandBox"; }

    private: Operand*   m_pOperand;
    public:  Physical*  m_pPhysical;

    // ctor
    public: OperandBox(Operand* p) { init(p); }

    protected: OperandBox() : m_pOperand(NULL), m_pPhysical(NULL) {}

    // [G]
    public: Instruction*        GetI()       const { return m_pParent; }
    public: const char*         GetKind()    const { return Kind_(); }
    public: Operand*            GetOperand() const { return m_pOperand; }

    public: Register* GetRx() const
        { return GetOperand()->DynamicCast<Register>(); }

    // [H]
    public: virtual void HtmlPrint(Val s, bool = false) const
        { GetOperand()->HtmlPrint(s, false); }

    // [I]
    protected: void init(Operand* p)
    {
        m_pOperand = p;
        m_pPhysical = NULL;
    } // init

    // [R]
    public: Operand* Replace(Operand*);

    // [S]
    public: Instruction* SetFromstruction(Instruction* p)
        { return m_pParent = p; }

    public: Operand* SetOperand(Operand* p)
        { return m_pOperand = p; }
}; // OperandBox

template<class Self_, class Parent_ = Operand>
class Operand_ :
    public WithCastable_<Self_, Parent_>
{
}; // Operand_

class Immediate : public Operand_<Immediate>
{
    public: static const char* Kind_() { return "Immediate"; }
}; // Immediate

class Subtype
{
    public: enum Answer
    {
        No,
        Yes,
        Unknown,
    }; // Answer
}; // Subtype

class Type :
    public Operand_<Type, Immediate>,
    public WorkListItem_<Type>
{
    public: static const char* Kind_() { return "Type"; }

    protected: Val m_name;
    protected: Val m_typespec;

    protected: Type() :
        m_name(nil),
        m_typespec(nil) {}

    // [A]
    public: static const Type* And(const Type*, const Type*);

    // [C]
    public: virtual const Type* ComputeAnd(const Type*) const
        { return this; }

    public: virtual const Type* ComputeDiff(const Type*) const
        { return const_cast<Type*>(this); }

    public: virtual const Type* ComputeOr(const Type*) const
        { return NULL; }

    // [D]
    public: static const Type* Diff(const Type*, const Type*);

    // [E]
    public: virtual bool Equal(const Operand* const that) const override
        { return this == that; }

    public: virtual bool Equal(const Type* that) const
        { return this == that; }

    // [G]
    public: virtual const char* GetKind() const override { return Kind_(); }

    public: virtual const Type* GetPrimaryTy() const
        { return const_cast<Type*>(this); }

    // [H]
    public: virtual void HtmlPrint(Val stream, bool) const override
        { cformat(stream, "<i class='t'>~W</i>", Unparse()); }

    // [I]
    public: virtual bool IsForeign() const { return false; }

    public: static Subtype::Answer IsSubtype(const Type*, const Type*);

    public: virtual Subtype::Answer IsSubtypeOf(const Type* that) const
        { return this == that ? Subtype::Yes : Subtype::Unknown; }

    public: virtual Subtype::Answer IsType(Val) const
        { return Subtype::Unknown; }

    // [O]
    public: static const Type* Or(const Type*, const Type*);

    // [P]
    public: static const Type* Parse(Val typepsec)
        { return Parse(typepsec, typepsec); }

    public: static const Type* Parse(Val, Val);

    // [U]
    public: virtual Val Unparse() const
        { return nil != m_name ? m_name : m_typespec; }
}; // Type

class Types;

class TypeItem :
    public DoubleLinkedItem_<TypeItem, Types>,
    public LocalObject
{
    private: const Type* m_pty;

    public: TypeItem(const Type* pty) :
        m_pty(pty) {}

    public: const Type* GetTy() const
        { return m_pty; }
}; // TypeItem

class Types :
    public DoubleLinkedList_<TypeItem, Types>
{
    private: typedef DoubleLinkedList_<TypeItem, Types> Base;

    // [A]
    public: const Type* Append(const Type* pty)
        { Base::Append(new TypeItem(pty)); return pty; }

    // [E]
    public: class Enum : public Base::Enum
    {
        public: Enum(const Types* pTypes) :
            Base::Enum(pTypes) {}

        public: const Type* Get() const
            { return Base::Enum::Get()->GetTy(); }
    }; // Enum
}; // Types

/// <summary>
///   Internal representation of class object.
/// </summary>
class TyClass : public WithCastable_<TyClass, Type>
{
    public: static const char* Kind_() { return "TyClass"; }

    private: class Map : public LocalObject
    {
        private: struct Slot
        {
            Val             m_name;
            const TyClass*  m_pTyClass;

            Slot() :
                m_name(nil), m_pTyClass(NULL) {}
        }; // Slot

        private: Slot   m_prgoSlot[1003];

        public: const TyClass* Get(Val) const;
        public: void Put(Val, const TyClass*);
    }; // Map

    private: static bool sm_fStaticInit;
    private: static Map* sm_pMap;

    // ctor
    public: TyClass(Val);

    // [C]
    public: virtual const Type* ComputeAnd(const Type*) const override;
    public: virtual const Type* ComputeDiff(const Type*) const override;

    // [B]
    public: static void BeginStaticInit(Mm*);

    // [E]
    public: static void EndStaticInit(Mm*);

    // [G]
    public: Val GetClass() const
        { return m_typespec; }

    public: static const TyClass* Get(Val);

    // [I]
    public: virtual Subtype::Answer IsSubtypeOf(const Type*) const override;

    public: virtual Subtype::Answer IsType(Val x) const override
        { return typep(x, m_typespec) ? Subtype::Yes : Subtype::No; }

    // [U]
    public: virtual Val Unparse() const override
        { return class_name(m_typespec); }
}; // TyClass

// See below for TyFunction.

/// <summary>
///   (integer min max) type
/// </summary>
class TyInteger : public WithCastable_<TyInteger, Type>
{
    public: static const char* Kind_() { return "TyInteger"; }

    private: Val m_min;
    private: Val m_max;

    // ctor
    private: TyInteger(Val typespec, Val name, Val min, Val max) :
        m_min(min),
        m_max(max)
    {
        m_name     = name;
        m_typespec = typespec;
    } // TyInteger

    private: TyInteger(Val typespec, Val name, Int iMin, Int iMax)
    {
        m_name     = name;
        m_typespec = typespec;

        m_min = MakeInt(iMin);
        m_max = MakeInt(iMax);
    } // TyInteger

    // [C]
    public: bool Contain(const TyInteger*) const;

    // [I]
    public: virtual Subtype::Answer IsSubtypeOf(const Type* that) const override;
    public: virtual Subtype::Answer IsType(Val) const override;

    // [P]
    public: static const TyInteger* Parse(Val, Val);
}; // TyInteger

class TyOr : public WithCastable_<TyOr, Type>
{
    public: static const char* Kind_() { return "TyOr"; }

    private: Types  m_oElts;

    // ctor
    private: TyOr(Val const typespec, Val const name)
    {
        m_name     = name;
        m_typespec = typespec;
    } // TyOr

    public: TyOr(
        const Type* const ptya,
        const Type* const ptyb )
    {
        m_name = nil;
        m_typespec = list(Qor, ptya->Unparse(), ptyb->Unparse());
        Append(ptya);
        Append(ptyb);
    } // TyOr

    // [A]
    public: const Type* Append(const Type* const pty)
        { return m_oElts.Append(pty); }

    // [C]
    public: virtual const Type* ComputeDiff(const Type*) const override;

    // [E]
    public: class Enum : public Types::Enum
    {
        public: Enum(const TyOr* const p) :
            Types::Enum(&p->m_oElts) {}
    }; // Enum

    public: virtual bool Equal(const Operand* const that) const
        { return this == that; }

    public: virtual bool Equal(const Type*) const override;

    // [I]
    public: virtual Subtype::Answer IsSubtypeOf(const Type*) const override;
    public: virtual Subtype::Answer IsType(Val) const override;

    // [P]
    public: static const Type* Parse(Val, Val);
}; // TyOr

// for native types
class TyForeign : public WithCastable_<TyForeign, Type>
{
    public: static const char* Kind_() { return "TyForeign"; }

    // ctor
    public: TyForeign(Val name)
    {
        m_name     = name;
        m_typespec = name;
    } // TyForeign

    // [I]
    public: virtual bool IsForeign() const override { return true; }

    public: virtual Subtype::Answer IsSubtypeOf(const Type* that) const override
        { return this == that ? Subtype::Yes : Subtype::Unknown; }

    public: virtual Subtype::Answer IsType(Val) const override
        { return Subtype::No; }
}; // TyForeign

class TyUndef : public WithCastable_<TyUndef, Type>
{
    public: static const char* Kind_() { return "TyUndef"; }

    // ctor
    public: TyUndef(Val typespec)
        { m_typespec = typespec; }
}; // TyUndef

/// <summary>
///   Compiler representation of type VALUES.
/// </summary>
class TyValues : public WithCastable_<TyValues, Type>
{
    public: static const char* Kind_() { return "TyValues"; }

    public: struct Arity
    {
        bool    m_fRest;
        int     m_iMin;
        int     m_iMax;

        Arity() :
            m_fRest(false), m_iMin(0), m_iMax(0) {}

        public: bool IsFixed() const
            { return m_iMin == m_iMax && ! m_fRest; }
    }; // Arity

    //= <FIXME date="2009-01-01" by="yosi@msn.com">
    //=     &amp;allow-other-keys
    //= </FIXME>

    public: class KeyEntry :
        public DoubleLinkedItem_<KeyEntry>,
        public LocalObject
    {
        public: Val         m_keyword;
        public: const Type* m_pty;
    }; // KeyEntry

    public: typedef DoubleLinkedList_<KeyEntry> KeyEntries;

    private:   Types        m_oReqs;
    private:   Types        m_oOpts;
    protected: const Type*  m_pResty;
    private:   KeyEntries   m_oKeys;

    // ctor
    public: TyValues() :
        m_pResty(NULL) {}

    public: TyValues(Val const typespec) :
            m_pResty(NULL)
        { m_typespec = typespec; }

    // [C]
    public: Arity ComputeArity() const;
    private: Val computeTypespec() const;

    // [E]
    public: class Enum
    {
        private: enum State
        {
            State_End,
            State_Keyword,
            State_KeywordValue,
            State_Optional,
            State_Required,
            State_Rest,
            State_Start,
        }; // State

        private: State              m_eState;
        private: TypeItem*          m_pRunner;
        private: const Type*        m_pty;
        private: const TyValues*    m_ptyValues;

        public: Enum(const TyValues* ptyValues) :
            m_eState(State_Start),
            m_pRunner(NULL),
            m_pty(NULL),
            m_ptyValues(ptyValues)
        {
            Next();
        } // Enum

        public: bool AtEnd() const
            { return State_End == m_eState; }

        public: const Type* Get() const
        {
            ASSERT(! AtEnd());
            ASSERT(NULL != m_pty);
            return m_pty;
        } // Get

        public: bool IsOptional() const
            { return State_Optional == m_eState; }

        public: bool IsRequired() const
            { return m_eState == State_Required; }

        public: bool IsRest() const
        {
            switch (m_eState)
            {
            case State_Keyword:
            case State_KeywordValue:
            case State_Rest:
                return true;
            } // switch state
            return false;
        } // IsRest

        public: void Next();
    }; // Enum

    public: virtual bool Equal(const Operand* const that) const
        { return this == that; }

    public: virtual bool Equal(const Type* that) const override;

    // [G]
    public: const Type* GetNthTy(Int) const;

    public: virtual const Type* GetPrimaryTy() const override
        { return GetNthTy(0); }

    // [I]
    public: const Type* IsScalar() const;

    public: virtual Subtype::Answer IsType(Val) const override
        { CAN_NOT_HAPPEN(); }

    // [O]
    public: static const Type* Or(const TyValues*, const TyValues*);

    // [P]
    public: static TyValues* Parse(Val);

    // [S]
    public: bool sameArity(const TyValues* const) const;
}; // TyValues

/// <summary>
///   Used for initializing tyValuesRestT as (values &amp;rest t).
/// </summary>
class TyValuesRestT :
    public TyValues
{
    public: TyValuesRestT(const Type* pResty) :
        TyValues(list(Qvalues, QArest, t))
    {
        m_pResty = pResty;
    } // resty
}; // TyValuesRestT

/// <summary>
///   Internal representation function type
/// </summary>
class TyFunction : public WithCastable_<TyFunction, Type>
{
    public: static const char* Kind_() { return "TyInteger"; }

    private: const TyValues* m_pParamTy;
    private: const Type*     m_pValueTy;

    // ctor
    protected: TyFunction(
        Val             const name,
        Val             const typespec,
        const TyValues* const pParamTy,
        const Type*     const pValueTy ) :
            m_pParamTy(pParamTy),
            m_pValueTy(pValueTy)
    {
        m_name     = name;
        m_typespec = typespec;
    } // TyFunction

    public: TyFunction(
        const TyValues* const pParamTy,
        const Type*     const pValueTy ) :
            m_pParamTy(pParamTy),
            m_pValueTy(pValueTy)
    {
        m_name     = nil;

        m_typespec = list(
            Qfunction,
            cdr(pParamTy->Unparse()),   // skip first symbol VALUES.
            pValueTy->Unparse() );
    } // TyFunction

    // [E]
    public: class EnumParam : public TyValues::Enum
    {
        public: EnumParam(const TyFunction* const p) :
            TyValues::Enum(p->m_pParamTy) {}
    }; // EnumParam

    // [G]
    public: const TyValues* GetParamTy() const
        { return m_pParamTy; }

    public: const Type* GetValueTy() const
        { return m_pValueTy; }

    // [P]
    public: static const TyFunction* Parse(Val, Val = nil);
}; // TyFunction

class TyUnknownFunction : public TyFunction
{
    public: TyUnknownFunction();
}; // TyUnknownFunction

/// <summary>
///   An integer operand.
/// </summary>
class Integer :
    public Operand_<Integer, Immediate>
{
    public: static const char* Kind_() { return "Integer"; }

    private: Int m_iVal;

    // ctor
    public: Integer(Int iVal) :
        m_iVal(iVal) {}

    // [G]
    public: Int   GetDatum() const { return m_iVal; }
    public: const Type* GetTy()    const { return tyInt; }

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [N]
    public: static Integer* New(Int iVal)
        { return new Integer(iVal); }
}; // Integer

class Label :
    public Operand_<Label, Immediate>,
    public DoubleLinkedItem_<Label>
{
    public: static const char* Kind_() { return "Label"; }

    private: BBlock* m_pBBlock;

    // ctor
    public: Label(BBlock* p) :
        m_pBBlock(p) {}

    // [E]
    public: virtual bool Equal(const Operand* const) const override;

    // [G]
    public: BBlock* GetBB() const { return m_pBBlock; }

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [S]
    public: BBlock* SetBB(BBlock* p) { return m_pBBlock = p; }
}; // Label

/// <summary>
///   Internal representation Literal.
/// </summary>
class Literal :
    public Operand_<Literal, Immediate>,
    public DoubleLinkedItem_<Literal>
{
    public: static const char* Kind_() { return "Literal"; }

    private: Val m_value;

    // ctor
    public: Literal(Val val) :
        m_value(val) {}

    // [E]
    public: virtual bool Equal(const Operand* const p) const override
    {
        if (Literal* that = p->DynamicCast<Literal>())
        {
            return this->GetDatum() == that->GetDatum();
        }
        return false;
    } // Equal

    // [G]
    public: Val   GetDatum() const { return m_value; }
    public: virtual const Type* GetTy() const override;

    // [I]
    public: virtual bool IsFalse() const override { return nil == GetDatum(); }
    public: virtual bool IsTrue()  const override { return nil != GetDatum(); }

    // [N]
    public: static Literal* New(Val obj);

    // [S]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [T]
    public: static Operand* True(const Operand*);
}; // Literal

/// <summary>
///   Internal representation Output.
/// </summary>
class Output :
    public WithCastable_<Output, Operand>,
    public WithIndex,
    public WithWorkArea
{
    public: static const char* Kind_() { return "Output"; }

    private: int            m_iName;
    private: Instruction*   m_pDefI;

    // ctor
    protected: Output();

    // [G]
    public: Instruction*         GetDefI() const { return m_pDefI; }
    public: int                  GetName() const { return m_iName; }
    public: virtual const Type* GetTy() const override { return tyT; }

    protected: virtual char16 getPrefix() const { return 'r'; }

    // [I]
    public: virtual bool IsFalse()  const { return false; }
    public: virtual bool IsTrue()   const { return false; }

    // [S]
    public: Instruction* SetDefI(Instruction* p) { return m_pDefI = p; }
    public: virtual void HtmlPrint(Val, bool = false) const override;
}; // Output

/// <summary>
///   Internal representation SSA Output.
/// </summary>
class SsaOutput :
    public WithCastable_<SsaOutput, Output>,
    public DoubleLinkedList_<OperandBox, Operand>
{
    public: static const char* Kind_() { return "SsaOutput"; }

    // [C]
    public: virtual Operand* Compute() const override;

    // [E]
    public: class EnumUser : public Users::Enum
    {
        public: EnumUser(const SsaOutput* p) :
            Users::Enum(static_cast<const Users*>(p)) {}

        public: Instruction* GetI() const
            { ASSERT(! AtEnd()); return Get()->GetI(); }
    }; // EnumUser

    // [G]
    public: OperandBox* GetSingleUser() const
    {
        EnumUser oEnum(this);
        if (oEnum.AtEnd()) return NULL;
        OperandBox* pBox = oEnum.Get();
        oEnum.Next();
        if (! oEnum.AtEnd()) return NULL;
        return pBox;
    } // GetSingleUser

    public: virtual const Type* GetTy() const override;

    // [H]
    public: bool HasUser() const
        { return ! static_cast<const Users*>(this)->IsEmpty(); }

    // [R]
    public: virtual void Realize(OperandBox*) override;
    public: void          ReplaceAll(Operand*);

    // [U]
    public: virtual void Unrealize(OperandBox*) override;
}; // SsaOutput

/// <summary>
///   Internal representation Boolean output
/// </summary>
class Bool :
    public Operand_<Bool, SsaOutput>
{
    public: static const char* Kind_() { return "Bool"; }

    // [G]
    public: virtual const Type* GetTy() const { return tyBool; }

    // [I]
    public: virtual bool IsFalse() const { return this == Bool_False; }
    public: virtual bool IsTrue()  const { return this == Bool_True; }

    // [S]
    public: virtual void HtmlPrint(Val, bool = false) const override;
}; // Bool

/// <summary>
///   Internal representation Dynamic Constant
///   (for load-value-time (?))
/// </summary>
class DynConstant :
    public Operand_<DynConstant, SsaOutput>
{
    public: static const char* Kind_() { return "DynConstant"; }

    // [G]
    public: virtual const Type* GeTy() const { return tyT; }

    // [S]
    public: virtual void HtmlPrint(Val, bool = false) const override;
}; // DynConstant

//  m_nCount
//      Number of TAGDEF instructions for computing size of TagsFrame.
class FrameReg :
    public DoubleLinkedItem_<FrameReg, Function>,
    public Operand_<FrameReg, SsaOutput>,
    public WorkListItem_<FrameReg>
{
    public: static const char* Kind_() { return "FrameReg"; }

    public:  uint       m_nCount;
    private: int        m_ofs;
    private: FrameReg*  m_pOuter;
    private: Val        const m_kind;
    private: Val        const m_name;

    // ctor
    public: FrameReg(FrameReg* pOuter, Val kind, Val name = nil) :
        m_nCount(0),
        m_ofs(0),
        m_pOuter(pOuter),
        m_kind(kind),
        m_name(name) {}

    // Prevent default assignment operator
    private: FrameReg& operator=(FrameReg&) { CAN_NOT_HAPPEN(); }

    // [G]
    public: Val                  GetFrameKind() const { return m_kind; }
    public: int                  GetLocation()  const { return m_ofs; }
    public: FrameReg*            GetOuter()     const { return m_pOuter; }
    public: virtual const Type* GetTy() const override { return tyPtrT; }

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [S]
    public: int SetLocation(int ofs)        { return m_ofs = ofs; }
    public: FrameReg* SetOuter(FrameReg* p) { return m_pOuter = p; }
}; // FrameReg

typedef DoubleLinkedList_<FrameReg, Function> FrameRegs;

class Pseudo :
    public Operand_<Pseudo, SsaOutput>
{
    public: static const char* Kind_() { return "Pseudo"; }

    protected: virtual char16 getPrefix() const override { return 'q'; }
}; // Pseudo

enum RegClass
{
    RegClass_Fpr,
    RegClass_Gpr,

    RegClass_Limit,
}; // RegClass

/// <summary>
///   Symbolic register of intermediate representation of program.
/// </summary>
class Register :
    public Operand_<Register, SsaOutput>,
    public Extendable_<Register>,
    public DoubleLinkedItem_<Register>,
    public WorkListItem_<Register>
{
    public: static const char* Kind_() { return "Register"; }

    private: RegClass   m_eRegClass;
    public:  Physical*  m_pPhysical;
    public:  StackSlot* m_pSpill;
    private: Variable*  m_pVar;

    // ctor
    public: Register(
                RegClass e = RegClass_Gpr,
                Variable* pVar = NULL ) :
        m_eRegClass(e), m_pPhysical(NULL), m_pSpill(NULL), m_pVar(pVar) {}

    public: Register(Variable* pVar) :
        m_eRegClass(RegClass_Gpr),
        m_pPhysical(NULL),
        m_pSpill(NULL),
        m_pVar(pVar) {}

    // [G]
    public: RegClass  GetRegClass() const { return m_eRegClass; }
    public: Variable* GetVar()      const { return m_pVar; }
    
    // [I]
    protected: void init(RegClass eRegClass, Variable* pVar = NULL)
    {
        m_eRegClass = eRegClass;
        m_pVar = pVar;
    } // init

    // [S]
    public: Variable* SetVar(Variable* p) { return m_pVar = p; }
}; // Register

/// <summary>
///   Symbolic register of intermediate representation of program.
/// </summary>
class Float :
    public Operand_<Float, Register>
{
    public: static const char* Kind_() { return "Float"; }

    // ctor
    public: Float() { init(RegClass_Fpr); }
    public: Float(Variable* pVar) { init(RegClass_Fpr, pVar); }

    protected: virtual char16 getPrefix() const override { return 'f'; }
}; // Float

typedef DoubleLinkedList_<Register> RegList;
typedef WorkList_<Register> RegWorkList;

/// <summary>
//    Internal representation of generaized true value.
/// </summary>
class TrueOperand :
    public Operand_<TrueOperand, Operand>
{
    public: static const char* Kind_() { return "TrueOperand"; }

    // [G]
    public: virtual const Type* GetTy() const override { return tyT; }

    // [H]
    public: virtual void HtmlPrint(Val s, bool = false) const override
        { format(s, "True"); }
}; // TrueOperand

/// <summary>
///   Values operand
/// </summary>
class Values : public Operand_<Values, SsaOutput>
{
    public: static const char* Kind_() { return "Values"; }

    protected: virtual char16 getPrefix() const override { return 'v'; }
}; // Values

/// <summary>
//    Internal representation of lexical variable.
/// </summary>
class Variable :
    public Operand_<Variable, Operand>,
    public Extendable_<Variable>,
    public WithIndex,
    public WithWorkArea,
    public WorkListItem_<Variable>
{
    public: static const char* Kind_() { return "Variable"; }

    public: enum Storage
    {
        Storage_Closed,
        Storage_Literal,
        Storage_Register,
        Storage_Stack,
    }; // Storage

    public:  uint           m_cUpRefs;
    private: Storage        m_eStorage;
    private: int            m_iLocation;
    private: Instruction*   m_pDefI;
    private: const Type*    m_pty;

    private: Val m_name;

    // ctor
    public: Variable(Val name) :
        m_cUpRefs(0),
        m_eStorage(Storage_Closed),
        m_iLocation(-1),
        m_pDefI(NULL),
        m_pty(tyT),
        m_name(name) {}

    protected: Variable() { CAN_NOT_HAPPEN(); }

    // [G]
    public: static Variable* Get(Operand*);

    public: Instruction* GetDefI()     const { return m_pDefI; }
    public: int          GetLocation() const { return m_iLocation; }
    public: Val          GetName()     const { return m_name; }
    public: Function*    GetOwner()    const;
    public: Register*    GetRd()       const;
    public: Storage      GetStorage()  const { return m_eStorage; }
    public: const Type*  GetTy()       const { return m_pty; }

    // [H]
    public: bool HasLocation() const
        { return -1 != m_iLocation; }

    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [R]
    public: virtual void Realize(OperandBox*) override;

    // [S]
    public: Instruction* SetDefI(Instruction* pI) { return m_pDefI = pI; }
    public: int          SetLocation(int i)       { return m_iLocation = i; }
    public: Storage      SetStorage(Storage e)    { return m_eStorage = e; }
    public: const Type*  SetTy(const Type* p)     { return m_pty = p; }
    // [U]
    public: virtual void Unrealize(OperandBox*) override;
}; // Variable

class VariableOperandBox :
    public OperandBox,
    public ChildItem_<VariableOperandBox, Function>
{
    public: VariableOperandBox(Variable* p) :
        OperandBox(p) {}
}; // VariableOperandBox

/// <summary>
///   Unreachable output.
/// </summary>
class UnreachableOutput :
    public Operand_<UnreachableOutput, Output>
{
    public: static const char* Kind_() { return "Unreachable"; }

    public: virtual void HtmlPrint(Val stream, bool = false) const override
        { html_format(stream, "%unreachable"); }
}; // UnreachableOutput

/// <summary>
///   Void output.
/// </summary>
class VoidOutput :
    public Operand_<VoidOutput, Output>
{
    public: static const char* Kind_() { return "Void"; }

    public: virtual void HtmlPrint(Val stream, bool = false) const override
        { html_format(stream, "%void"); }
}; // VoidOutput

enum Op
{
    #define definstruction(mp_name, mp_format) Op_ ## mp_name,
    Op_None = 0,
    #include "./tinycl_compiler.inc"
    Op_Limit,
}; // Op

/// <summary>
///   Internal represenation of Instruction
/// </summary>
class Instruction :
    public    Object,
    public    Castable_<Instruction>,
    protected ChildList_<OperandBox, Instruction>,
    public    ChildItem_<Instruction, BBlock>,
    public    LocalObject,
    public    WithIndex,
    public    WithWorkArea,
    public    WorkListItem_<Instruction>
{
    private: typedef ChildList_<OperandBox, Instruction> OperandBoxes;

    protected: static const char* sm_rgpszMnemonic[Op_Limit];

    protected: Op           m_eOp;
    protected: Output*      m_pOutput;
    protected: const Type*  m_pty;

    // ctor
    protected: Instruction() {}

    // [A]
    public: Operand* AppendOperand(Operand* pSx)
    {
        AppendOperand(new OperandBox(pSx));
        return pSx;
    } // AppendOperand

    public: OperandBox* AppendOperand(OperandBox* pBox)
    {
        static_cast<OperandBoxes*>(this)->Append(pBox);
        if (IsRealized())
        {
            pBox->GetOperand()->Realize(pBox);
        }
        return pBox;
    } // AppendOperand

    // [C]
    public: virtual Operand* Compute() const
        { return NULL; }

    public: uint CountOperands() const
    {
        uint cOperands = 0;
        foreach (EnumOperand, oEnum, this)
        {
            cOperands += 1;
        } // for each operand
        return cOperands;
    } // CountOperands

    // [D]
    public: bool DoesDominate(const Instruction*) const;

    // [E]
    public: class EnumOperand : public OperandBoxes::Enum
    {
        private: typedef OperandBoxes::Enum Enum;

        public: EnumOperand(const Instruction* p) :
            Enum(static_cast<const OperandBoxes*>(p)) {}

        public: Operand* Get() const
            { return GetBox()->GetOperand(); }

        public: Register* GetRx() const
            { return Get()->DynamicCast<Register>(); }

        public: OperandBox* GetBox() const
            { return Enum::Get(); }
    }; // EnumOperand

    public: bool Equal(const Instruction*) const;

    // [G]
    public: BBlock* GetBBlock() const
        { return m_pParent; }

    public: BBlock* GetBB() const
        { return m_pParent; }

    public: Bool* GetBd() const
        { return GetOutput()->DynamicCast<Bool>(); }

    public: Bool* GetBx() const
        { return GetSx()->DynamicCast<Bool>(); }

    public: Int GetIy() const
        { return GetSy()->StaticCast<Integer>()->GetDatum(); }

    public: Val GetLx() const
        { return GetSx()->StaticCast<Literal>()->GetDatum(); }

    public: Val GetLy() const
        { return GetSy()->StaticCast<Literal>()->GetDatum(); }

    public: Val GetLz() const
        { return GetSz()->StaticCast<Literal>()->GetDatum(); }

    public: const char*  GetMnemonic() const
        { return sm_rgpszMnemonic[m_eOp]; }

    public: Op GetOp() const
        { return m_eOp; }

    public: OperandBox* GetOperandBox(int) const;
    public: Operand*    GetOperand(int) const;

    public: Output* GetOutput() const
        { ASSERT(NULL != m_pOutput); return m_pOutput; }

    public: Pseudo* GetQd() const
        { return GetOutput()->DynamicCast<Pseudo>(); }

    public: Pseudo* GetQx() const
        { return GetSx()->DynamicCast<Pseudo>(); }

    public: Pseudo* GetQy() const
        { return GetSy()->DynamicCast<Pseudo>(); }

    public: Pseudo* GetQz() const
        { return GetSz()->DynamicCast<Pseudo>(); }

    public: Register* GetRd() const
        { return GetOutput()->DynamicCast<Register>(); }

    public: Register* GetRx() const
        { return GetSx()->DynamicCast<Register>(); }

    public: Register* GetRy() const
        { return GetSy()->DynamicCast<Register>(); }

    public: Register* GetRz() const
        { return GetSz()->DynamicCast<Register>(); }

    public: SsaOutput* GetSsaOutput() const
        { return GetOutput()->DynamicCast<SsaOutput>(); }

    public: Operand* GetSx() const { return GetOperand(0); }
    public: Operand* GetSy() const { return GetOperand(1); }
    public: Operand* GetSz() const { return GetOperand(2); }

    public: const Type* GetTy() const { return m_pty; }

    public: Values* GetVd() const
        { return GetOutput()->DynamicCast<Values>(); }

    public: Values* GetVx() const
        { return GetSx()->DynamicCast<Values>(); }

    public: Values* GetVy() const
        { return GetSy()->DynamicCast<Values>(); }

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [I]
    public: Operand* InsertOperandBefore(Operand* pSx, OperandBox* pRef)
    {
        InsertOperandBefore(new OperandBox(pSx), pRef);
        return pSx;
    } // InsertOperandBefore

    public: OperandBox* InsertOperandBefore(
        OperandBox* pBox,
        OperandBox* pRef )
    {
        if (NULL == pRef)
        {
            return AppendOperand(pBox);
        }

        static_cast<OperandBoxes*>(this)->InsertBefore(pBox, pRef);
        if (IsRealized())
        {
            pBox->GetOperand()->Realize(pBox);
        }
        return pBox;
    } // InsertOperandBefore

    public:         bool IsRealized() const { return NULL != GetBBlock(); }
    public: virtual bool IsUseless()  const;

    // [O]
    public: virtual void OnMove(BBlock* const) {}
    public: virtual bool Optimize() { return false; }

    // [R]
    public: virtual void Realize();

    public: void RemoveOperand(OperandBox*);

    // [S]
    public: Output* SetOutput(Output*);

    public: const Type* SetTy(const Type* pty) 
        { return m_pty = pty; }

    // [U]
    public: virtual void Unrealize();

    // [V]
    public: virtual bool Verify() const;
}; // Instruction

template<class T, Op Op_, class Parent_ = Instruction>
class Instruction_ : public WithCastable_<T, Parent_>
{
    protected: typedef Instruction_<T, Op_, Parent_> Base;
    protected: typedef Parent_ Super;

    public: static Op GetOp_()
        { return Op_; }

    public: static const char* Kind_()
        { return sm_rgpszMnemonic[Op_]; }

    protected: Instruction_(
        Op          const eOp,
        const Type* const pType,
        Output*     const pOutput )
    {
        m_eOp     = eOp;
        m_pOutput = pOutput;
        m_pty     = pType;
    } // Instruction_

    protected: Instruction_()
    {
        m_eOp     = Op_None;
        m_pOutput = NULL;
        m_pty     = NULL;
    } // Instruction_
}; // Instruction_

/// <summary>
///  Internal representation of basic block.
/// </summary>
class BBlock :
    public    LocalObject,
    protected ChildList_<Instruction, BBlock>,
    public    Extendable_<BBlock>,
    public    GraphNode_<Function, BBlock, CfgEdge>,
    public    Object,
    public    WithIndex,
    public    WorkListItem_<BBlock>,
    public    WithWorkArea
{
    private: typedef ChildList_<Instruction, BBlock>    Instructions;
    private: typedef DoubleLinkedList_<Label>           Labels;

    private:   int              m_iName;
    private:   Labels           m_oLabels;
    protected: DataFlowData*    m_pDfData;
    private:   LoopNode*        m_pLoopNode;

    // ctor
    public: BBlock();

    // [A]
    public: Instruction* AppendI(Instruction*);

    // [D]
    public: bool DoesDominate(const BBlock*) const;

    // [E]
    class_Enum_(BBlock, I, Instructions)

    private: Instructions* prepareTraversal(Instructions* p)
        { return p; }

    // [G]
    public: Instruction* GetFirstI()   const;
    public: Function*    GetFunction() const { return getParent(); }
    public: Label*       GetLabel()    const { return m_oLabels.GetFirst(); }
    public: Instruction* GetLastI()    const;

    public: LoopNode* GetLoopNode() const
        { return m_pLoopNode; }

    public: int GetName() const
        { return m_iName; }

    public: BBlock* GetNext() const
        { return static_cast<const LayoutNode*>(this)->GetNext(); }

    public: BBlock* GetPrev() const
        { return static_cast<const LayoutNode*>(this)->GetPrev(); }

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [I]
    public: Instruction* InsertAfterI(Instruction*, Instruction*);
    public: Instruction* InsertBeforeI(Instruction*, Instruction*);
    public: bool IsExitBB() const;
    public: bool IsLiveIn(int) const;
    public: bool IsLiveOut(int) const;

    // [M]
    public: Instruction* MoveBeforeI(Instruction*, Instruction*);
    public: void         MoveLabelsTo(BBlock*);

    // [R]
    public: void         RedirectEdgeFrom(BBlock*, BBlock*);
    public: void         RedirectEdgeTo(BBlock*, BBlock*);
    public: BBlock*      RemoveCriticalEdge(BBlock*);
    public: void         RemoveI(Instruction*);
    public: Instruction* ReplaceI(Instruction*, Instruction*);
    private:   void      replaceLabels(BBlock*, BBlock*);
    protected: void      replacePhiOperands(BBlock*, BBlock*);

    public: void Reset()
    {
        SetFlag(0);
        SetIndex(0);
        SetWork(NULL);
    } // Reset

    // [S]
    public: Function* SetFunction(Function* p) { return setParent(p); }
    public: void      SetLiveIn(int);
    public: void      SetLiveOut(int);
    public: LoopNode* SetLoopNode(LoopNode* p) { return m_pLoopNode = p; }
    public: BBlock*   SplitBefore(Instruction*);

    // [V]
    public: bool Verify() const;
}; // BBlock

/// <summary>
///  Internal representation of funciton object.
/// </summary>
class Function :
    public Operand_<Function>,
    public Extendable_<Function>,
    public Graph_<BBlock, Function>,
    public GraphNode_<Module, Function, CgEdge>,
    public WithIndex,
    public WithWorkArea,
    public WorkListItem_<Function>
{
    public: static const char* Kind_() { return "Function"; }

    // For moving variables
    friend class FunctionIntegrator;

    // For retrieving owner of variable
    friend class Variable;

    public: enum Flag
    {
        Flag_DynamicExtent  = 1 << 0,
    }; // Flag

    public: enum Flavor
    {
        Flavor_Anonymous,
        Flavor_Finally,
        Flavor_Named,
        Flavor_Template,
        Flavor_Toplevel,
    }; // Flavor

    public: struct Arity
    {
        bool    m_fRest;
        int     m_iMin;
        int     m_iMax;

        Arity() :
            m_fRest(false), m_iMin(0), m_iMax(0) {}

        public: bool IsFixed() const
            { return m_iMin == m_iMax && ! m_fRest; }
    }; // Arity

    // Asm and Stack passes use m_cbFrame.
    public: uint m_cbFrame;

    // SplitClosure needs to access m_oArity.
    protected: Arity m_oArity;

    private: Flavor             m_eFlavor;
    private: uint               m_rgfFlag;
    private: Users              m_oCalls;
    private: FrameRegs          m_oFrameRegs;
    private: Users              m_oUsers;
    private: Users              m_oUpVarDefs;
    private: Users              m_oVarDefs;
    private: const TyFunction*  m_pFunty;

    public: Val m_fn;
    public: Val m_name;

    // ctor
    public: Function(Flavor eFlavor, Val = nil);
    protected: Function() {}

    // [A]
    public: FrameReg* AddFrameReg(FrameReg* p)
        { return m_oFrameRegs.Append(p); }

    // [C]
    public: bool        Clean();
    public: bool        ComputeDominance();
    public: bool        ComputeLiveness();
    public: LoopNode*   ComputeLoop();
    public: bool        ComputePostDominance();
    public: bool        ComputeVarLiveness();

    // [E]
    public: bool EliminateInfiniteLoop();

    class_Enum_(Function, BBlock,          LayoutList);
    class_Enum_(Function, BBlockPreorder,  PreorderList)
    class_Enum_(Function, BBlockPostorder, PostorderList)

    public: class EnumCall : public Users::Enum
    {
        public: EnumCall(const Function* p) :
            Users::Enum(&p->m_oCalls) {}

        public: Instruction* GetI() const
            { ASSERT(! AtEnd()); return Users::Enum::Get()->GetI(); }
    }; // EnumCall

    public: class EnumFrameReg : public FrameRegs::Enum
    {
        public: EnumFrameReg(const Function* p) :
            FrameRegs::Enum(&p->m_oFrameRegs) {}
    }; // EnumFrameReg

    public: class EnumI
    {
        private: BBlock*        m_pBBlock;
        private: Instruction*   m_pI;

        public: EnumI(const Function* p) :
            m_pBBlock(p->GetEntryBB()),
            m_pI(p->GetEntryBB()->GetFirstI()) {}

        public: bool AtEnd() const
            { return NULL == m_pI; }

        public: Instruction* Get() const
            { ASSERT(!AtEnd()); return m_pI; }

        public: void Next()
        {
            m_pI = m_pI->GetNext();

            for (;;)
            {
                if (NULL != m_pI) return;
                m_pBBlock = m_pBBlock->GetNext();
                if (NULL == m_pBBlock) return;
                m_pI = m_pBBlock->GetFirstI();
            } // for
        } // Next
    }; // EnumI

    public: class EnumOutput : protected EnumI
    {
        public: EnumOutput(const Function* p) :
            EnumI(p) { next(); }

        public: bool AtEnd() const
            { return EnumI::AtEnd(); }

        public: Output* Get() const
            { return EnumI::Get()->GetOutput(); }

        public: void Next()
        {
            ASSERT(!AtEnd());
            EnumI::Next();
            next();
         } // Next

        private: void next()
        {
            while (! EnumI::AtEnd())
            {
                unless (EnumI::Get()->GetOutput()->Is<VoidOutput>()) break;
                EnumI::Next();
            } // while
        } // next
    }; // EnumOutput

    public: class EnumReg : protected EnumOutput
    {
        public: EnumReg(const Function* p) :
            EnumOutput(p) { next(); }

        public: Register* Get() const
        {
            ASSERT(!AtEnd());
            return EnumOutput::Get()->StaticCast<Register>();
        } // Get

        public: bool AtEnd() const
            { return EnumOutput::AtEnd(); }

        public: void Next()
            { ASSERT(!AtEnd()); EnumOutput::Next(); next(); }

        private: void next()
        {
            while (! AtEnd())
            {
                if (EnumOutput::Get()->Is<Register>()) break;
                EnumOutput::Next();
            } // while
        } // next
    }; // EnumReg

    public: class EnumUser : public Users::Enum
    {
        public: EnumUser(const Function* p) :
            Users::Enum(&p->m_oUsers) {}

        public: Instruction* GetI() const
            { ASSERT(! AtEnd()); return Users::Enum::Get()->GetI(); }
    }; // EnumUser

    public: class EnumUpVar : public Users::Enum
    {
        public: EnumUpVar(const Function* p) :
            Users::Enum(&p->m_oUpVarDefs) {}

        public: Variable* Get() const
        {
            ASSERT(! AtEnd());
            return GetI()->GetSx()->StaticCast<Variable>();
        } // Get

        public: Instruction* GetI() const
            { ASSERT(! AtEnd()); return Users::Enum::Get()->GetI(); }
    }; // EnumUpVar

    public: class EnumVar : public Users::Enum
    {
        public: EnumVar(const Function* p) :
            Users::Enum(&p->m_oVarDefs) {}

        public: Variable* Get() const
        {
            ASSERT(! AtEnd());
            return GetI()->GetSx()->StaticCast<Variable>();
        } // Get

        public: Instruction* GetI() const
        {
            ASSERT(! AtEnd());
            return Users::Enum::Get()->GetI();
        } // GetI
    }; // EnumVar

    // [F]
    public: Pseudo* FindUpVar(Variable*) const;

    // [G]
    public: const Arity*         GetArity()   const { return &m_oArity; }
    public: BBlock*              GetEntryBB() const;
    public: BBlock*              GetExitBB()  const;
    public: Flavor               GetFlavor()  const { return m_eFlavor; }
    public: const TyFunction*    GetFunty()   const { return m_pFunty; }
    public: Module*              GetModule()  const { return getParent(); }
    public: Val                  GetName()    const { return m_name; }
    public: PrologueI*           GetPrologueI() const;
    public: Function*            GetSingleCaller() const;
    public: BBlock*              GetStartBB() const;
    public: virtual const Type* GetTy() const override { return m_pFunty; }

    // [H]
    public: bool HasCallSite() const
        { return ! m_oCalls.IsEmpty(); }

    public: bool HasNonlocalExitPoint() const;
    public: bool HasUpVar() const;
    public: bool HasUseSite() const;
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [I]
    /// <summary>
    ///   Insert block after reference block.
    /// </summary>
    /// <param name="pBB">A block to be inserted</param>
    /// <param name="pRefBB">A reference block</param>
    /// <see cref="InsertBefore"/>
    public: BBlock* InsertAfter(BBlock* pBB, BBlock* pRefBB)
    {
        return static_cast<LayoutList*>(this)->InsertAfter(pBB, pRefBB);
    } // InsertAfter

    /// <summary>
    ///   Insert block before reference block.
    /// </summary>
    /// <param name="pBB">A block to be inserted</param>
    /// <param name="pRefBB">A reference block</param>
    /// <see cref="InsertAfter"/>
    public: BBlock* InsertBefore(BBlock* pBB, BBlock* pRefBB)
    {
        return static_cast<LayoutList*>(this)->InsertBefore(pBB, pRefBB);
    } // InsertBefore

    public: Pseudo* InternUpVar(Variable*);
    public: bool    IsClosure() const;

    public: bool IsDynamicExtent() const
        { return 0 != (m_rgfFlag & Flag_DynamicExtent); }

    public: bool IsNotClosure() const;

    public: bool IsStackRestify() const;

    // [M]
    public: void    MakeVoid();
    public: uint    MarkFlag(uint n) { return m_rgfFlag |= n; }
    public: BBlock* MoveBBlock(BBlock*, BBlock*);

    // [N]
    public: bool NeedArity() const;
    public: void NumberInstructions();

    // [R]
    public: virtual void Realize(OperandBox*) override;
    public: void          RemoveBBlock(BBlock*);

    public: void RemoveFrameReg(FrameReg* p)
        { m_oFrameRegs.Delete(p); }

    // [S]
    public: void SetArity(int a, int b, bool fRest)
    {
        m_oArity.m_fRest = fRest;
        m_oArity.m_iMin  = a;
        m_oArity.m_iMax  = b;
    } // SetArity

    public: Flavor  SetFlavor(Flavor e)  { return m_eFlavor = e; }
    public: Module* SetModule(Module* p) { return setParent(p); }

    public: const TyFunction* SetFunty(const TyFunction* p)
        { return m_pFunty = p; }

    // [U]
    public: uint          UnmarkFlag(uint n) { return m_rgfFlag &= ~n; }
    public: virtual void Unrealize(OperandBox*) override;
    public: bool          UpdateValueTy();

    // [V]
    public: bool Verify() const;
}; // Function

/// <summary>
///   Function operand box used for tracking user(?)
//    Note: Do we really ned this?
/// </summary>
class FunctionOperandBox :
    public OperandBox,
    public ChildItem_<FunctionOperandBox, Function>
{
    public: FunctionOperandBox(Function* p) :
        OperandBox(p) {}
}; // FunctionOperandBox

/// <summary>
///   Internal representation of function name that have type information.
/// </summary>
class FunName :
    public Operand_<FunName>
{
    public: static const char* Kind_() { return "FunName"; }

    private: Val               const m_alist;
    private: Val               const m_name;
    private: const TyFunction* const m_pFunty;

    // ctor
    public: FunName(
            Val               const name,
            Val               const alist,
            const TyFunction* const pFunty ) :
        m_alist(alist),
        m_name(name),
        m_pFunty(pFunty) {}

    private: FunName() :
        m_alist(nil),
        m_name(nil),
        m_pFunty(NULL) {}

    private: FunName& operator =(const FunName&)
        { CAN_NOT_HAPPEN(); }

    // [G]
    public: Val                GetAlist() const { return m_alist; }
    public: Val                GetName()  const { return m_name; }
    public: const TyFunction * GetFunty() const { return m_pFunty; }
    public: const Type*        GetTy()    const { return GetFunty(); }

    // [H]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [I]
    public: static FunName* Intern(Val);
}; // FunName

/// <summary>
///   Compilation module contains functions. This is a unit of compilation.
/// </summary>
class Module :
    public Graph_<Function, Module>,
    public Object
{
    // [A]
    public: Function* Append(Function* p)
        { return static_cast<LayoutList*>(this)->Append(p); }

    // [C]
    public: void Clean();

    // [E]
    class_Enum_(Module, Function,          LayoutList);
    class_Enum_(Module, FunctionPreorder,  PostorderList)
    class_Enum_(Module, FunctionPostorder, PostorderList)

    // [P]
    public: virtual void HtmlPrint(Val, bool = false) const override;

    // [R]
    public: void RemoveFunction(Function*);

    // [V]
    public: bool Verify() const;
}; // Module

interface IMm
{
    public: virtual void* Alloc(size_t) = 0;
}; // IMm

class Mm : public IMm
{
    private: HANDLE m_hHeap;

    public: Mm() : m_hHeap(::HeapCreate(HEAP_NO_SERIALIZE, 0, 0)) {}

    public: Mm(HANDLE hHeap) : m_hHeap(hHeap) {}

    public: virtual ~Mm()
        { if (NULL != m_hHeap) ::HeapDestroy(m_hHeap); }

    // [A]
    public: virtual void* Alloc(size_t cb)
        { return ::HeapAlloc(m_hHeap, HEAP_ZERO_MEMORY, cb); }

    // [D]
    protected: void detach() { m_hHeap = NULL; }
}; // Mm

class Context : public Mm
{
    public:  Val    m_errors;
    public:  Val    m_form;
    private: Val    m_previous;
    private: Val    m_truename;
    public:  Val    m_warnings;

    public:  int        m_cBBlocks;
    public:  int        m_cFunctions;
    public:  int        m_cOutputs;
    public:  int        m_iDepth;
    public:  int        m_iPass;
    public:  int        m_iSubPass;
    private: Pass*      m_pPass;
    private: Target*    m_pTarget;
    private: Module     m_oModule;

    // ctor
    public: Context(Val);
    public: ~Context();

    // [C]
    public: bool CanContinue() const { return nil == m_errors; }

    // [E]
    public: void Error(const char*);
    public: void Error(const char*, Val);
    public: void Error(const char*, Val, Val);
    public: void Error(const char*, Val, Val, Val);
    public: void ErrorV(const char*, Val);

    // [G]
    public: static Context* Get()
        { return TLV(c_AcontextA)->To<Context>(); }

    public: Val      GetTruename() const { return m_truename; }
    public: Module*  GetModule()         { return &m_oModule; }
    public: Pass*    GetPass()     const { return m_pPass; }
    public: Target*  GetTarget()   const { return m_pTarget; }

    // [I]
    public: void InternalError(const char*, int, const char*);

    // [L]
    public: static void Log(int, const char*, ...);

    // [N]
    public: static void Note(Instruction* pI, const char* psz, Val a)
        { NoteV(pI, psz, list(a)); }

    public: static void Note(
        Instruction* pI,
        const char* psz,
        Val a, Val b)
        { NoteV(pI, psz, list(a, b)); }

    public: static void Note(
        Instruction* pI,
        const char* psz,
        Val a, Val b, Val c)
        { NoteV(pI, psz, list(a, b, c)); }

    public: static void NoteV(Instruction*, const char*, Val);

    // [R]
    public: Val Run();

    // [S]
    public: Pass*   SetPass(Pass* p)     { return m_pPass = p; }
    public: Target* SetTarget(Target* p) { return m_pTarget = p; }
}; // Context

#define COMPILER_INTERNAL_ERROR() \
    Context::Get()->InternalError(__FILE__, __LINE__, "")

#define C_INTERNAL_ERROR(mp_msg) \
    Context::Get()->InternalError(__FILE__, __LINE__, mp_msg)

#define CLOG Context::Log

class LogSection
{
    private: int            m_iLevel;
    private: const char*    m_psz;

    public: LogSection(int const iLevel, const char* const psz) :
        m_iLevel(iLevel),
        m_psz(psz) {}

    public: ~LogSection()
        { CLOG(m_iLevel, m_psz); }
}; // LogSection

#define CLOG_SECTION(mp_i, mp_fmt, ...) \
    LogSection macro_oLogSection ## __COUNTER__ (mp_i, "</ol></li>~%"); \
    CLOG(mp_i, "<li>"); \
    CLOG(mp_i, mp_fmt, __VA_ARGS__); \
    CLOG(mp_i, "<ol>~%");

class Pass :
    public Castable_<Pass>
{
    private: uint           m_nStartAt;
    private: Pass*          m_pOuter;
    private: const char*    m_pszName;
    protected: Val          m_stream;

    // ctor/dtor
    public: Pass();
    public: virtual ~Pass();

    // [G]
    public: virtual const char* GetName()   const = 0;
    public: Pass*               GetOuter()  const { return m_pOuter; }
    public: Val                 GetStream() const { return m_stream; }

    // [P]
    public: virtual void Prepare();

    // [U]
    protected: void unexpected(Instruction*)
        { COMPILER_INTERNAL_ERROR(); }
}; // Pass

class FunctionPass :
    public Pass
{
    public: static const char* Kind_() { return "FunctionPass"; }

    public: void Run();
    protected: virtual void processFunction(Function*) = 0;
}; // FunctionPass

class ModulePass :
    public Pass
{
    public: static const char* Kind_() { return "Module"; }

    public: void Run();
    protected: virtual void processModule(Module*) = 0;
}; // ModulePass

template<class Self_, class Parent_ = Pass>
class Pass_ : public WithCastable_<Self_, Parent_>
{
    public: virtual const char* GetName() const override
        { return Self_::GetName_(); }

    public: static const char* Kind_()
        { return Self_::GetName_(); }
}; // Pass_

class SubPass : public Pass
{
}; // SubPass

#define PROCESS_I_NAME_(mp_name) process_ ## mp_name

#define DefProcI(mp_name) \
    protected: virtual void PROCESS_I_NAME_(mp_name)(Instruction* pI) override

#define DefProcI_(mp_name) \
    void THIS_PASS :: PROCESS_I_NAME_(mp_name)(Instruction* pI)

//////////////////////////////////////////////////////////////////////
//
// InstructionDispatch
//
// Note: To avoid fat member function pointer, we use static function
// wrapper.
class InstructionDispatcher
{
    #define definstruction(mp_name, mp_format) \
        private: static void \
            PROCESS_I_NAME_(_ ## mp_name)( \
                InstructionDispatcher* p, \
                Instruction* pI) \
            { p->PROCESS_I_NAME_(mp_name)(pI); } \
        protected: virtual void \
            PROCESS_I_NAME_(mp_name)(Instruction* pI) \
                { processDefault(pI); }

    #include "./tinycl_compiler.inc"

    protected: typedef void (*ProcessIFun)(
        InstructionDispatcher*,
        Instruction* );

    protected: static const ProcessIFun k_rgpfnProcessIFun[Op_Limit];

    protected: void dispatch(Instruction* pI)
        { k_rgpfnProcessIFun[pI->GetOp()](this, pI); }

    protected: virtual void processDefault(Instruction*) {}
}; // InstructionDispatcher

#define defstatic(mp_ctype, mp_cname) \
    extern mp_ctype* mp_cname;

#define defstatic2(mp_ctype, mp_cname, mp_a) \
    extern mp_ctype* mp_cname;

#include "./tinycl_compiler.inc"

#define PASS_ENTRY_(mp_name) runPass ## mp_name
#define DECLPASS(mp_name) void PASS_ENTRY_(mp_name)()

#define DEFPASS(mp_name) \
    DECLPASS(mp_name) \
    { \
        Pass ## mp_name oPass; \
        oPass.Prepare(); \
        oPass.Run(); \
    } // DEFPASS

//- <summary>
//-   Function database.
//- </summary>
template<class Self_, class Entry_, uint cTableSize_>
class FunDb_
{
    private: struct Slot
    {
        const Entry_*  m_pEntry;
    }; // Slot

    private: static Self_* sm_pFunDb;
    private: Slot m_rgoSlot[cTableSize_];

    protected: FunDb_()
    {
        ::ZeroMemory(m_rgoSlot, sizeof(m_rgoSlot));
    } // FunDb_

    public: static const Entry_* Find(Val const name)
    {
        if (NULL == sm_pFunDb)
        {
            sm_pFunDb = Self_::Create();
        }

        return sm_pFunDb->find(name);
    } // Find

    private: const Entry_* find(Val const name) const
    {
        const Slot* const pTop = m_rgoSlot;
        const Slot* const pBtm = m_rgoSlot + lengthof(m_rgoSlot);
        const Slot* const pStart = pTop + hash(name) % (pBtm - pTop);
        const Slot* pRunner = pStart;
        for (;;)
        {
            const Entry_* pEntry = pRunner->m_pEntry;
            if (NULL == pEntry)
            {
                return NULL;
            }

            if (pEntry->m_name == name)
            {
                return pEntry;
            }

            pRunner++;
            if (pRunner == pBtm)
            {
                pRunner = pTop;
            }

            if (pRunner == pStart)
            {
                COMPILER_INTERNAL_ERROR();
                return NULL;
            }
        } // for
    } // find

    private: static Int hash(Val const name)
    {
        if (Symbol* const p = name->DynamicCast<Symbol>())
        {
            return Fixnum::Decode_(p->m_hash_code);
        }

        if (SetfCell * const p = name->DynamicCast<SetfCell>())
        {
            if (Symbol* const q = p->m_name->DynamicCast<Symbol>())
            {
                return Fixnum::Decode_(q->m_hash_code) << 1;
            }
        }

        COMPILER_INTERNAL_ERROR();
        return 0;
    } // hash

    protected: void load(
        const Entry_* const prgoEntry,
        uint          const cEntries )
    {
        for (
            const Entry_* p = prgoEntry;
            p < prgoEntry + cEntries;
            p++ )
        {
            put(p);
        } // for p
    } // load

    private: void put(const Entry_* pEntry)
    {
        Val const name = pEntry->m_name;
        Slot* const pTop = m_rgoSlot;
        Slot* const pBtm = m_rgoSlot + lengthof(m_rgoSlot);
        Slot* const pStart = pTop + hash(name) % (pBtm - pTop);
        Slot* pRunner = pStart;
        for (;;)
        {
            if (NULL == pRunner->m_pEntry)
            {
                pRunner->m_pEntry = pEntry;
                return;
            }

            ASSERT(pRunner->m_pEntry->m_name != name);

            pRunner++;
            if (pRunner == pBtm)
            {
                pRunner = pTop;
            }

            if (pRunner == pStart)
            {
                // FunDb is overflow.
                CAN_NOT_HAPPEN();
            }
        } // for
    } // put
}; // FunDb_

} // Compiler

} // TinyCl

#include "./ir/tinycl_c_ir_insn.h"

#endif //!defined(INCLUDE_tinycl_c_defs_h)
