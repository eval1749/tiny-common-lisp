//////////////////////////////////////////////////////////////////////////////
//
// Regex - Parse Tree Node
// regex_node.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/regex/regex_node.h#6 $
//
#if !defined(INCLUDE_regex_node_h)
#define INCLUDE_regex_node_h

#include "./regex_bytecode.h"
#include "./regex_util.h"

namespace Regex
{

namespace RegexPrivate
{

class Compiler;

/// <remark>
///   Regex length information
/// </remark>
struct LengthInfo
{
    bool    m_fFixed;
    int     m_iMax;
    int     m_iMin;

    /// <summary>
    ///   Returns fixed match information.
    /// </summary>
    /// <returns>Value of m_fFixed</returns>
    bool IsFixed() const { return m_fFixed; }

    /// <summary>
    ///  Constructs LengthInfo
    /// </summary>
    /// <param name="fFixed">
    ///   True if regex matches fixed length of source
    /// </param>
    /// <param name="iMax">
    ///  Maximum number of characters to be matched.
    /// </param>
    /// <param name="iMin">
    ///  Minimum number of characters to be matched.
    /// </param>
    LengthInfo(bool fFixed, int iMin = 0, int iMax = 0) :
        m_fFixed(fFixed),
        m_iMax(iMax),
        m_iMin(iMin) {}
}; // LengthInfo

/// <remark>
///   For specifiying min-max parameter.
/// </remark>
struct MinMax
{
    int m_iMin;
    int m_iMax;

    MinMax(int iMin = -1, int iMax = -1) :
        m_iMin(iMin), m_iMax(iMax) {}

    bool IsOne() const
        { return 1 == m_iMin && 1 == m_iMax; }
}; // MinMax

/// <remark>
///   Base class of parse tree node
/// </remark>
class Node :
    public Castable_<Node>,
    public DoubleLinkedItem_<Node>,
    public LocalObject
{
    public: enum Case
    {
        CaseSensitive   = 0,
        CaseInsensitive = 1,
    }; // Case

    public: enum Direction
    {
        Forward  = 0,
        Backward = 1,
    }; // Direction

    // [C]
    public: virtual void Compile(Compiler*, int) = 0;

    public: virtual void CompileNot(Compiler*, int)
        { CAN_NOT_HAPPEN(); }

    public: virtual LengthInfo ComputeLength() const
        { return LengthInfo(false); }

    public: virtual int ComputeMinLength() const { return 0; }

    public: virtual bool IsMember(IEnvironment*, char16) const
        { return false; }

    // [N]
    public: virtual bool NeedStack() const { return false; }

    public: virtual Node* Not()
        { CAN_NOT_HAPPEN(); }

    // [R]
    public: virtual Node* Reverse() { return this; }

    // [S]
    public: virtual Node* Simplify(IEnvironment*, LocalHeap*) { return this; }

    #if _DEBUG
        public: virtual void Print() const
        {
            printf("%s", GetKind());
        } // Print
    #endif
}; // Node

typedef DoubleLinkedList_<Node> Nodes;

//////////////////////////////////////////////////////////////////////
//
/// WithCase
//
class WithCase
{
    protected: Node::Case m_eCase;

    // [G]
    public: Node::Case GetCase() const 
        { return m_eCase; }

    // [I]
    public: bool IsIgnoreCase() const 
        { return Node::CaseInsensitive == m_eCase; }
}; // WithCase

//////////////////////////////////////////////////////////////////////
//
/// WithDirection
//
class WithDirection
{
    protected: Node::Direction m_eDirection;

    // [G]
    public: Node::Direction GetDirection() const 
        { return m_eDirection; }

    // [I]
    public: bool IsBackward() const 
        { return Node::Backward == m_eDirection; }
}; // WithDirection

//////////////////////////////////////////////////////////////////////
//
// NodeEqBase
//
class NodeEqBase :
    public WithCastable_<NodeEqBase, Node>,
    public WithDirection
{
    public: static const char* Kind_() { return "NodeEqBase"; }

    protected: bool m_fNot;

    // ctor
    protected: void init(Direction eDir, bool fNot)
    {
        m_eDirection = eDir;
        m_fNot       = fNot;
    } // init

    // [I]
    public: bool IsNot() const
        { return m_fNot; }

    // [N]
    public: Node* Not()
    {
        m_fNot = ! m_fNot;
        return this;
    } // Not
}; // NodeEqBase

/// <remark>
///   Base class for capture reference.
/// </remark>
class NodeCaptureBase :
    public WithCastable_<NodeCaptureBase, Node>,
    public WithDirection
{
    public: static const char* Kind_() { return "NodeCaptureBase"; }

    protected: int m_iNth;

    protected: void init(Direction eDir, int iNth)
    {
        m_eDirection = eDir;
        m_iNth       = iNth;
    } // init

    // [G]
    public: int GetNth() const { return m_iNth; }

    // [S]
    public: void SetNth(int iNth) { ASSERT(m_iNth <= 0); m_iNth = iNth; }
}; // NodeCaptureBase

//////////////////////////////////////////////////////////////////////
//
// NodeCsBase
//
class NodeCsBase :
    public WithCastable_<NodeCsBase, NodeEqBase>,
    public WithCase
{
    public: static const char* Kind_() { return "NodeCsBase"; }

    // [I]
    protected: void init (Direction eDir, Case eCase, bool fNot)
    {
        m_eDirection = eDir;
        m_fNot       = fNot;
        m_eCase      = eCase;
    } // init
}; // NodeCsBase

//////////////////////////////////////////////////////////////////////
//
// NodeOpBase
//
class NodeOpBase :
    public WithCastable_<NodeOpBase, Node>
{
    public: static const char* Kind_() { return "NodeOpBase"; }

    private: Op m_eOp;

    // [C]
    public: override void Compile(Compiler*, int);

    // [G]
    public: Op GetOp() const { return m_eOp; }

    // [I]
    protected: void init(Op const eOp)
    {
        m_eOp = eOp;
    } // init

    // [S]
    protected: void setOp(Op const eOp) {
        m_eOp = eOp;
    } // setOp
}; // NodeOpBase

//////////////////////////////////////////////////////////////////////
//
// NodeSubNodeBase
//
class NodeSubNodeBase :
    public WithCastable_<NodeSubNodeBase, Node>
{
    public: static const char* Kind_() { return "NodeSubNodeBase"; }

    protected: Node* m_pNode;

    // [C]
    public: override int ComputeMinLength() const
        { return m_pNode->ComputeMinLength(); }

    // [G]
    public: Node* GetNode() const
        { return m_pNode; }

    // [I]
    protected: void init(Node* pNode)
    {
        m_pNode = pNode;
    } // init

    // [N]
    public: override bool NeedStack() const
        { return m_pNode->NeedStack(); }

    // [R]
    public: override Node* Reverse()
    {
        m_pNode = m_pNode->Reverse();
        return this;
    } // Reverse

    #if _DEBUG
        public: override void Print() const
        {
            printf("(%s ", GetKind());
            m_pNode->Print();
            printf(")");
        } // Print
    #endif
}; // NodeSubNodeBase

//////////////////////////////////////////////////////////////////////
//
// NodeSubNodesBase
//
class NodeSubNodesBase :
    public WithCastable_<NodeSubNodesBase, Node>
{
    public: static const char* Kind_() { return "NodeSubNodesBase"; }

    protected: Nodes m_oNodes;

    // [A]
    public: void Append(Node* pNode)
        { m_oNodes.Append(pNode); }

    // [D]
    public: void Delete(Node* pNode)
        { m_oNodes.Delete(pNode); }

    // [G]
    public: Node* GetFirst() const
        { return m_oNodes.GetFirst(); }

    // [R]
    public: override Node* Reverse();

    #if _DEBUG
        public: override void Print() const
        {
            printf("(%s", GetKind());
            foreach (Nodes::Enum, oEnum, &m_oNodes)
            {
                printf(" ");
                oEnum.Get()->Print();
            } // for each node
            printf(")");
        } // Print
    #endif
}; // NodeSubNodesBase

/// <remark>
///   Parse tree node "And".
/// </remark>
class NodeAnd :
    public WithCastable_<NodeAnd, NodeSubNodesBase>
{
    public: static const char* Kind_() { return "And"; }

    /// <remark>
    ///   Constructs empty And node.
    /// </remark>
    public: NodeAnd() {}

    /// <remark>
    ///   Constructs And node with one sub node.
    /// </remark>
    public: NodeAnd(Node* pNode1)
        { Append(pNode1); }

    /// <remark>
    ///   Constructs And node with two sub nodes.
    /// </remark>
    public: NodeAnd(Node* pNode1, Node* pNode2)
    {
        Append(pNode1);
        Append(pNode2);
    } // NodeAnd

    /// <remark>
    ///   Constructs And node with three sub nodes.
    /// </remark>
    public: NodeAnd(Node* pNode1, Node* pNode2, Node* pNode3)
    {
        Append(pNode1);
        Append(pNode2);
        Append(pNode3);
    } // NodeAnd

    // [C]
    /// <summary>
    ///   Sum of value of all subnodes.
    /// </summary>
    public: override int ComputeMinLength() const
    {
        int nMinLen = 0;
        foreach (Nodes::Enum, oEnum, &m_oNodes)
        {
            Node* pNode = oEnum.Get();
            nMinLen += pNode->ComputeMinLength();
            if (nMinLen >= Infinity) return Infinity;
        } // for each node
        return nMinLen;
    } // ComputeMinLength

    public: override void Compile(Compiler*, int);

    // [S]
    public: override Node* Simplify(IEnvironment*, LocalHeap*);
}; // NodeAnd

/// <remark>
///   Parse tree node for dot(.)
/// </remark>
class NodeAny :
    public WithCastable_<NodeAny, Node>,
    public WithDirection
{
    public: static const char* Kind_() { return "Any"; }

    // ctor
    public: NodeAny(Direction eDir)
    {
        m_eDirection = eDir;
    } // NodeAny

    // [C]
    public: override void Compile(Compiler*, int);
    public: override int  ComputeMinLength() const { return 1; }

    // [G]
    public: Op GetOp() const
        { return IsBackward() ? Op_Any_B : Op_Any_F; }
}; // NodeAny

/// <remark>
///   Parse tree node for atomic group <c>(?>...)</c>
/// </remark>
class NodeAtom :
    public WithCastable_<NodeAtom, NodeSubNodeBase>
{
    public: static const char* Kind_() { return "Atom"; }

    public: NodeAtom(Node* pNode)
    {
        m_pNode = pNode;
    } // NodeAtom

    // [C]
    public: override void Compile(Compiler*, int);
}; // NodeAtom

/// <remark>
///   Parse tree node capture. Regex syntax is <c>(...)</c>.
/// </remark>
class NodeCapture :
    public WithCastable_<NodeCapture, NodeSubNodeBase>,
    public WithDirection
{
    public: static const char* Kind_() { return "Capture"; }

    /// <summary>
    ///  An index number of this capture.
    /// </summary>
    private: int m_iNth;

    /// <summary>
    ///   Construct NodeCapture object.
    /// </summary>
    /// <param name="eDir">A direction of capturing</param>
    /// <param name="iNth">Capture index number</param>
    /// <param name="pNode">A node will be captured</param>
    public: NodeCapture(Direction eDir, Node* pNode, int iNth)
    {
        ASSERT(iNth >= 1);
        m_pNode      = pNode;
        m_eDirection = eDir;
        m_iNth       = iNth;
    } // NodeCpature

    // [C]
    public: override void Compile(Compiler*, int);

    // [G]
    /// <summary>
    ///   Opcode of this node.
    /// </summary>
    /// <returns>
    ///   Op_Capture_B for backward capturing, Op_Capture_F for
    ///   forward capturing.
    /// </returns>
    public: Op GetOp() const
        { return IsBackward() ? Op_Capture_B : Op_Capture_F; }

    // [N]
    /// <summary>
    ///   Alwasy true for NodeCapture.
    /// </summary>
    public: override bool NeedStack() const
        { return true; }

    #if _DEBUG
        public: virtual void Print() const
        {
            printf("(%s[%d] ", 
                IsBackward() ? "CaptureB" : "CaptureF",
                m_iNth );
            m_pNode->Print();
            printf(")");
        } // Print
    #endif
}; // NodeCapture

//////////////////////////////////////////////////////////////////////
//
// NodeCaptureEq -- \<n> \k<name>
//
class NodeCaptureEq :
    public WithCastable_<NodeCaptureEq, NodeCaptureBase>,
    public WithCase
{
    public: static const char* Kind_() { return "CaptureEq"; }

    public: NodeCaptureEq(Direction eDir, int iNth, Case eCase)
    {
        m_eCase      = eCase;
        m_eDirection = eDir;
        m_iNth       = iNth;
    } // NodeCaptureEq

    // [C]
    public: override void Compile(Compiler*, int);

    // [G]
    public: Op GetOp() const
    {
        return IsBackward() ?
            IsIgnoreCase() ? Op_CaptureEq_Ci_B : Op_CaptureEq_Cs_B :
            IsIgnoreCase() ? Op_CaptureEq_Ci_F : Op_CaptureEq_Cs_F;
    } // GetOp
}; // NodeCaptureEq

//////////////////////////////////////////////////////////////////////
//
// NodeCaptureIfNot
//
class NodeCaptureIfNot :
    public WithCastable_<NodeCaptureIfNot, NodeCaptureBase>
{
    public: static const char* Kind_() { return "CaptureIfNot"; }

    public: NodeCaptureIfNot(Direction eDir, int iNth)
    {
        m_eDirection = eDir;
        m_iNth       = iNth;
    } // NodeCaptureIfNot

    // [C]
    public: override void Compile(Compiler*, int)
        { CAN_NOT_HAPPEN(); }
}; // NodeCpautreIfNot

/// <remark>
///   Parse tree node for character comparison.
/// </remark>
class NodeChar :
    public WithCastable_<NodeChar, NodeCsBase>
{
    public: static const char* Kind_() { return "Char"; }

    private: char16 m_wch;

    /// <summary>
    ///  Construct NodeChar.
    /// </summary>
    public: NodeChar(
        Direction   eDir,
        char16      wch,
        Case        eCase = CaseSensitive,
        bool        fNot  = false )
    {
        m_eCase      = eCase;
        m_eDirection = eDir;
        m_fNot       = fNot;
        m_wch        = wch;
    } // NodeChar

    // [C]
    public: override void Compile(Compiler*, int);
    public: override void CompileNot(Compiler*, int);
    public: override int  ComputeMinLength() const { return 1; }

    // [G]
    public: char16 GetChar() const { return m_wch; }

    public: Op GetOp(bool fNot) const
    {
        static const Op k_rgeOp[] =
        {
            Op_CharEq_Cs_F,
            Op_CharEq_Cs_B,

            Op_CharNe_Cs_F,
            Op_CharNe_Cs_B,

            Op_CharEq_Ci_F,
            Op_CharEq_Ci_B,

            Op_CharNe_Ci_F,
            Op_CharNe_Ci_B,
        }; // k_rgeOp

        int k = 0;
        if (IsBackward()) k |= 1;
        if (fNot) k |= 2;
        if (IsIgnoreCase()) k |= 4;
        return k_rgeOp[k];
    } // GetOp

    // [I]
    public: override bool IsMember(IEnvironment*, char16) const;

    // [S]
    public: override Node* Simplify(IEnvironment*, LocalHeap*);

    #if _DEBUG
        public: virtual void Print() const
        {
            printf("(Char%s%s%s '%c')",
                IsNot()        ? "Ne" : "Eq",
                IsIgnoreCase() ? "Ci" : "Cs",
                IsBackward()   ? "B"  : "F",
                GetChar() );
        } // Print
    #endif
}; // NodeChar

//////////////////////////////////////////////////////////////////////
//
// NodeCharClass
//
class NodeCharClass :
    public WithCastable_<NodeCharClass, NodeSubNodesBase>,
    public WithDirection
{
    public: static const char* Kind_() { return "CharClass"; }

    private: bool m_fNot;

    public: NodeCharClass(Direction eDir, bool fNot)
    {
        m_eDirection = eDir;
        m_fNot = fNot;
    } // NodeCharClass

    // [C]
    public: override void Compile(Compiler*, int);
    public: override int  ComputeMinLength() const { return 1; }

    // [I]
    public: bool IsMember(IEnvironment* pIEnv, char16 wch) const
    {
        foreach (Nodes::Enum, oEnum, &m_oNodes)
        {
            if (oEnum.Get()->IsMember(pIEnv, wch))
            {
                return true;
            }
        } // for each node
        return false;
    } // IsMember

    public: bool IsNot() const { return m_fNot; }

    // [N]
    public: override bool NeedStack() const { return true; }

    // [S]
    public: override Node* Simplify(IEnvironment*, LocalHeap*);

    #if _DEBUG
        public: override void Print() const
        {
            printf("(CharClass%s", IsNot() ? "Not" : "" );
            foreach (Nodes::Enum, oEnum, &m_oNodes)
            {
                printf(" ");
                oEnum.Get()->Print();
            } // for each node
            printf(")");
        } // Print
    #endif
}; // NodeCharClass

//////////////////////////////////////////////////////////////////////
//
// NodeCharSet
//
class NodeCharSet :
    public WithCastable_<NodeCharSet, NodeEqBase>
{
    public: static const char* Kind_() { return "CharSet"; }

    private: int        m_cwch;
    private: char16*    m_pwch;

    // ctor
    public: NodeCharSet(Direction eDir, char16* pwch, int cwch, bool fNot) :
        m_cwch(cwch),
        m_pwch(pwch)
    {
        m_eDirection = eDir;
        m_fNot       = fNot;
    } // NodeCharSet

    // [C]
    public: override void Compile(Compiler*, int);
    public: override int  ComputeMinLength() const { return 1; }

    // [G]
    public: Op GetOp() const
    {
        return IsBackward() ?
            IsNot() ? Op_CharSetNe_B : Op_CharSetEq_B :
            IsNot() ? Op_CharSetNe_F : Op_CharSetEq_F;
    } // GetOp

    public: int           GetLength() const { return m_cwch; }
    public: const char16* GetString() const { return m_pwch; }

    // [I]
    public: override bool IsMember(IEnvironment*, char16 wch) const
    {
        const char16* pwchEnd = m_pwch + m_cwch;
        for (const char16* pwch = m_pwch; pwch < pwchEnd; pwch++)
        {
            if (*pwch == wch) return ! m_fNot;
        } // for each char
        return m_fNot;
    } // IsMember

    #if _DEBUG
        public: override void Print() const
        {
            printf("(%s '%ls')",
                IsNot() ? "CharSetNe" : "CharSetEq",
                m_pwch );
        } // Print
    #endif
}; // NodeCharSet

//////////////////////////////////////////////////////////////////////
//
// NodeIf
//
class NodeIf :
    public WithCastable_<NodeIf, Node>
{
    public: static const char* Kind_() { return "If"; }
 
    private: Node* m_pCond;
    private: Node* m_pElse;
    private: Node* m_pThen;

    // ctor
    public: NodeIf(Node* pCond, Node* pThen, Node* pElse) :
        m_pCond(pCond),
        m_pThen(pThen),
        m_pElse(pElse) {}

    // [C]
    public: override void Compile(Compiler*, int);
    public: override int  ComputeMinLength() const;

    // [N]
    public: override bool NeedStack() const { return true; }

    // [R]
    public: override Node* Reverse()
    {
        m_pCond = m_pCond->Reverse();
        m_pThen = m_pThen->Reverse();
        m_pElse = m_pElse->Reverse();

        return this;
    } // Reverse

    // [S]
    public: override Node* Simplify(IEnvironment* pIEnv, LocalHeap* pHeap)
    {
        m_pCond = m_pCond->Simplify(pIEnv, pHeap);
        m_pThen = m_pThen->Simplify(pIEnv, pHeap);
        m_pElse = m_pElse->Simplify(pIEnv, pHeap);

        return this;
    } // Simplify

    #if _DEBUG
        public: override void Print() const
        {
            printf("(if ");
            m_pCond->Print();
            printf(" ");
            m_pThen->Print();
            printf(" ");
            m_pElse->Print();
            printf(")");
        } // Print
    #endif
}; // NodeIf

//////////////////////////////////////////////////////////////////////
//
// NodeLookaround
//
class NodeLookaround :
    public WithCastable_<NodeLookaround, NodeSubNodeBase>
{
    public: static const char* Kind_() { return "Lookaround"; }

    private: bool m_fPositive;

    public: NodeLookaround(Node* pNode, bool fPositive) :
        m_fPositive(fPositive)
    {
        m_pNode = pNode;
    } // NodeLookaround

    // [C]
    public: override void Compile(Compiler*, int);
    public: override int  ComputeMinLength() const { return 0; }

    // [I]
    public: bool IsPositive() const { return m_fPositive; }

    // [N]
    public: override bool NeedStack() const { return true; }
}; // NodeLookaround

//////////////////////////////////////////////////////////////////////
//
// NodeMinMax
//  Base class for NodeMax and NodeMin.
//
class NodeMinMax :
    public WithCastable_<NodeMinMax, NodeSubNodeBase>,
    public WithDirection
{
    public: static const char* Kind_() { return "MinMax"; }

    protected: int m_iMax;
    protected: int m_iMin;

    // [C]
    public: override int ComputeMinLength() const
        { return m_pNode->ComputeMinLength() * m_iMin; }

    // [G]
    public: int GetMax() const { return m_iMax; }
    public: int GetMin() const { return m_iMin; }

    // [I]
    protected: void init(Direction eDir, Node* pNode, MinMax oMinMax)
    {
        m_eDirection = eDir;
        m_pNode = pNode;
        m_iMax = oMinMax.m_iMax;
        m_iMin = oMinMax.m_iMin;
    } // init

    // [N]
    public: override bool NeedStack() const { return true; }

    #if _DEBUG
        public: virtual void Print() const
        {
            printf("(%s min=%d max=%d ", GetKind(), GetMin(), GetMax());
            GetNode()->Print();
            printf(")");
        } // Print
    #endif
}; // NodeMinMax

//////////////////////////////////////////////////////////////////////
//
// NodeMax
//
class NodeMax :
    public WithCastable_<NodeMax, NodeMinMax>
{
    public: static const char* Kind_() { return "Max"; }

    public: NodeMax(Direction eDir, Node* pNode, MinMax oMinMax)
    {
        init(eDir, pNode, oMinMax);
    } // NodeMax

    // [C]
    public: override void Compile(Compiler*, int);
}; // NodeMax

//////////////////////////////////////////////////////////////////////
//
// NodeMin
//
class NodeMin :
    public WithCastable_<NodeMin, NodeMinMax>
{
    public: static const char* Kind_() { return "Min"; }

    public: NodeMin(Direction eDir, Node* pNode, MinMax oMinMax)
    {
        init(eDir, pNode, oMinMax);
    } // NodeMin

    // [C]
    public: override void Compile(Compiler*, int);
}; // NodeMin

//////////////////////////////////////////////////////////////////////
//
// NodeOneWidth
//
class NodeOneWidth :
    public WithCastable_<NodeOneWidth, NodeOpBase>
{
    public: static const char* Kind_() { return "OneWidth"; }

    public: NodeOneWidth(Op eOp)
        { Base::init(eOp); }

    // [C]
    public: override void CompileNot(Compiler*, int);
    public: override int  ComputeMinLength() const { return 1; }

    // [G]
    public: Op GetNotOp() const
        { return static_cast<Op>(GetOp() + 1); }

    // [I]
    public: override bool IsMember(IEnvironment*, char16 wch) const;

    // [N]
    public: override Node* Not();

}; // NodeOneWidth

//////////////////////////////////////////////////////////////////////
//
// NodeOr
//
class NodeOr :
    public WithCastable_<NodeOr, NodeSubNodesBase>
{
    public: static const char* Kind_() { return "Or"; }

    // ctor
    public: NodeOr() {}

    public: NodeOr(Node* pNode1)
        { m_oNodes.Append(pNode1); }

    public: NodeOr(Node* pNode1, Node* pNode2)
    {
        m_oNodes.Append(pNode1);
        m_oNodes.Append(pNode2);
    } // NodeOr

    // [C]
    
    // ComputeMinLength - minimum value of all subnodes.
    public: override int ComputeMinLength() const
    {
        int nMinLen = Infinity;
        foreach (Nodes::Enum, oEnum, &m_oNodes)
        {
            Node* pNode = oEnum.Get();
            nMinLen = min(nMinLen, pNode->ComputeMinLength());
        } // for each node
        return nMinLen;
    } // ComputeMinLength

    public: override void Compile(Compiler*, int);

    // [G]
    public: Nodes* GetNodes() { return &m_oNodes; }

    // [N]
    public: override bool NeedStack() const { return true; }
}; // NodeOr

//////////////////////////////////////////////////////////////////////
//
// NodeRange
//  Represent simple character class, e.g. [A-Z].
//
class NodeRange :
    public WithCastable_<NodeRange, NodeCsBase>
{
    public: static const char* Kind_() { return "Range"; }

    private: char16 m_wchMin;
    private: char16 m_wchMax;

    public: NodeRange(
        Direction   eDir,
        char16      wchMin,
        char16      wchMax,
        Case        eCase,
        bool        fNot = false ) :
            m_wchMin(wchMin),
            m_wchMax(wchMax)
    {
        init(eDir, eCase, fNot);
    } // NodeRange

    // [C]
    public: override void Compile(Compiler*, int);
    public: override void CompileNot(Compiler*, int);
    public: override int  ComputeMinLength() const { return 1; }

    // [G]
    public: char16 GetMaxChar() const { return m_wchMax; }
    public: char16 GetMinChar() const { return m_wchMin; }

    public: Op GetOp(bool fNot) const
    {
        static const Op k_rgeOp[] =
        {
            Op_RangeEq_Cs_F,
            Op_RangeEq_Cs_B,

            Op_RangeNe_Cs_F,
            Op_RangeNe_Cs_B,

            Op_RangeEq_Ci_F,
            Op_RangeEq_Ci_B,

            Op_RangeNe_Ci_F,
            Op_RangeNe_Ci_B,
        }; // k_rgeOp

        int k = 0;
        if (IsBackward()) k |= 1;
        if (fNot) k |= 2;
        if (IsIgnoreCase()) k |= 4;
        return k_rgeOp[k];
    } // GetOp

    // [I]
    public: override bool IsMember(IEnvironment*, char16) const;

    // [S]
    public: override Node* Simplify(IEnvironment*, LocalHeap*);

    #if _DEBUG
        public: override void Print() const
        {
            printf("(Range%s%s %lc-%lc)",
                IsNot() ? "Not" : "",
                IsIgnoreCase() ? "Ci" : "Cs",
                GetMinChar(),
                GetMaxChar() );
        } // Print
    #endif
}; // NodeRange

/// <remark>
///   Parse tree node for string comparison.
/// </remark>
class NodeString :
    public WithCastable_<NodeString, NodeCsBase>
{
    public: static const char* Kind_() { return "StringEq"; }

    private: int            m_cwch;
    private: const char16*  m_pwch;

    /// <summary>
    ///  Construct NodeString.
    /// </summary>
    public: NodeString(
        Direction       eDir,
        const char16*   pwch,
        int             cwch,
        Case            eCase = CaseSensitive,
        bool            fNot = false ) :
            m_cwch(cwch),
            m_pwch(pwch)
    {
        Base::init(eDir, eCase, fNot);
    } // NodeString

    // [C]
    public: override int  ComputeMinLength() const { return m_cwch; }
    public: override void Compile(Compiler*, int);

    // [G]
    public: int GetLength() const { return m_cwch; }

    public: Op GetOp(bool fNot) const
    {
        static const Op k_rgeOp[] =
        {
            Op_StringEq_Cs_F,
            Op_StringEq_Cs_B,

            Op_StringNe_Cs_F,
            Op_StringNe_Cs_B,

            Op_StringEq_Ci_F,
            Op_StringEq_Ci_B,

            Op_StringNe_Ci_F,
            Op_StringNe_Ci_B,
        }; // k_rgeOp

        int k = 0;
        if (IsBackward()) k |= 1;
        if (fNot) k |= 2;
        if (IsIgnoreCase()) k |= 4;
        return k_rgeOp[k];
    } // GetOp

    public: const char16* GetStart() const { return m_pwch; }

    // [P]
    #if _DEBUG
        public: override void Print() const
        {
            printf("(String%s%s%s '%ls')",
                IsNot()        ? "Ne" : "Eq",
                IsIgnoreCase() ? "Ci" : "Cs",
                IsBackward()   ? "B"  : "F",
                m_pwch );
        } // Print
    #endif
}; // NodeString

/// <summary>
///  Parse tree node for void. This is used for empty capture.
/// </summary>
class NodeVoid :
    public WithCastable_<NodeVoid, Node>
{
    public: static const char* Kind_() { return "Void"; }

    // [C]
    public: override void Compile(Compiler*, int) {}
}; // NodeVoid

//////////////////////////////////////////////////////////////////////
//
// NodeZeroWidth
//  Op_AsciiBoundary        "(?a:\b)"
//  Op_AfterNewline         "^"m                scanner
//  Op_AsciiNotBoundary     "(?a:\B)"
//  Op_BeforeNewLine        "(?m:$)"            scanner
//  Op_EndOfLine            "$" "\Z"            scanner
//  Op_EndOfString          "(?s:$)" "\z"       scanner
//  Op_Posn                 "\G"                scanner
//  Op_StartOfString        "^" "\A"            scanner
//  Op_UnicodeBoundary      "(?:u\b)"
//  Op_UnicodeNotBoundary   "(?:u\B)"
//
// See Also:
//      k_rgoZeroWidthMap           in regex_parse.cpp
//      Compiler::compileScanner    in regex_compile.cpp
//      Engine::Execute             in regex_exec.cpp
//
class NodeZeroWidth :
    public WithCastable_<NodeZeroWidth, NodeOpBase>
{
    public: static const char* Kind_() { return "ZeroWidth"; }

    public: NodeZeroWidth(Op eOp)
        { Base::init(eOp); }
}; // NodeOpBase

//////////////////////////////////////////////////////////////////////
//
// CaptureDef
//
struct CaptureDef :
    LocalObject,
    DoubleLinkedItem_<CaptureDef>
{
    int     m_iNth;
    char16* m_pwszName;

    CaptureDef(char16* pwszName, int iNth) :
        m_iNth(iNth), m_pwszName(pwszName) {}
}; // CaptureDef

typedef DoubleLinkedList_<CaptureDef> CaptureDefs;

//////////////////////////////////////////////////////////////////////
//
// Tree
//
class Tree :
    public LocalObject
{
    public: int         m_cCaptures;
    public: int         m_iErrorCode;
    public: long        m_lErrorPosn;
    public: CaptureDefs m_oCaptures;
    public: Node*       m_pNode;
    public: int         m_rgfOption;
    public: char16*     m_prgpwszCaptureName;

    public: Tree(int rgfOption) :
        m_cCaptures(0),
        m_iErrorCode(0),
        m_lErrorPosn(0),
        m_pNode(NULL),
        m_rgfOption(rgfOption),
        m_prgpwszCaptureName(NULL) {}
}; // Tree

} // RegexPrivate
} // Regex

#endif //!defined(INCLUDE_regex_node_h)
