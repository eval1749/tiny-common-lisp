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

namespace Regex {

namespace RegexPrivate {

class Compiler;

/// <remark>
/// Regex length information
/// </remark>
struct LengthInfo {
  bool m_fFixed;
  int m_iMax;
  int m_iMin;

  /// <summary>
  /// Returns fixed match information.
  /// </summary>
  /// <returns>Value of m_fFixed</returns>
  bool IsFixed() const { return m_fFixed; }

  /// <summary>
  /// Constructs LengthInfo
  /// </summary>
  /// <param name="fFixed">
  /// True if regex matches fixed length of source
  /// </param>
  /// <param name="iMax">
  /// Maximum number of characters to be matched.
  /// </param>
  /// <param name="iMin">
  /// Minimum number of characters to be matched.
  /// </param>
  LengthInfo(bool fFixed, int iMin = 0, int iMax = 0) :
      m_fFixed(fFixed),
      m_iMax(iMax),
      m_iMin(iMin) {}
};

/// <remark>
/// For specifiying min-max parameter.
/// </remark>
struct MinMax {
  int m_iMin;
  int m_iMax;

  MinMax(int iMin = -1, int iMax = -1)
      : m_iMin(iMin), m_iMax(iMax) {}

  bool IsOne() const { return 1 == m_iMin && 1 == m_iMax; }
};

/// <remark>
/// Base class of parse tree node
/// </remark>
class Node
    : public Castable_<Node>,
      public DoubleLinkedItem_<Node>,
      public LocalObject {
  public: enum Case {
      CaseSensitive = 0,
      CaseInsensitive = 1,
  };

  public: enum Direction {
      Forward = 0,
      Backward = 1,
  };

  // [C]
  public: virtual void Compile(Compiler*, int) = 0;

  public: virtual void CompileNot(Compiler*, int) { CAN_NOT_HAPPEN(); }

  public: virtual LengthInfo ComputeLength() const {
    return LengthInfo(false);
  }

  public: virtual int ComputeMinLength() const { return 0; }

  public: virtual bool IsMember(IEnvironment*, char16) const {
    return false;
  }

  // [N]
  public: virtual bool NeedStack() const { return false; }

  public: virtual Node* Not() { CAN_NOT_HAPPEN(); }

  // [R]
  public: virtual Node* Reverse() { return this; }

  // [S]
  public: virtual Node* Simplify(IEnvironment*, LocalHeap*) { return this; }

  #if _DEBUG
      public: virtual void Print() const {
        printf("%s", GetKind());
      }
  #endif
};

typedef DoubleLinkedList_<Node> Nodes;

//////////////////////////////////////////////////////////////////////
//
/// WithCase
//
class WithCase {
  protected: Node::Case m_eCase;

  // [G]
  public: Node::Case GetCase() const { return m_eCase; }

  // [I]
  public: bool IsIgnoreCase() const {
    return Node::CaseInsensitive == m_eCase;
  }
};

//////////////////////////////////////////////////////////////////////
//
/// WithDirection
//
class WithDirection {
  protected: Node::Direction m_eDirection;

  // [G]
  public: Node::Direction GetDirection() const { return m_eDirection; }

  // [I]
  public: bool IsBackward() const { return Node::Backward == m_eDirection; }
};

//////////////////////////////////////////////////////////////////////
//
// NodeEqBase
//
class NodeEqBase
    : public WithCastable_<NodeEqBase, Node>,
      public WithDirection {
  public: static const char* Kind_() { return "NodeEqBase"; }

  protected: bool m_fNot;

  // ctor
  protected: void init(Direction eDir, bool fNot) {
    m_eDirection = eDir;
    m_fNot = fNot;
  }

  // [I]
  public: bool IsNot() const { return m_fNot; }

  // [N]
  public: Node* Not() {
    m_fNot = ! m_fNot;
    return this;
  }
};

/// <remark>
/// Base class for capture reference.
/// </remark>
class NodeCaptureBase
    : public WithCastable_<NodeCaptureBase, Node>,
      public WithDirection {
  public: static const char* Kind_() { return "NodeCaptureBase"; }

  protected: int m_iNth;

  protected: void init(Direction eDir, int iNth) {
    m_eDirection = eDir;
    m_iNth = iNth;
  }

  // [G]
  public: int GetNth() const { return m_iNth; }

  // [S]
  public: void SetNth(int iNth) { ASSERT(m_iNth <= 0); m_iNth = iNth; }
};

//////////////////////////////////////////////////////////////////////
//
// NodeCsBase
//
class NodeCsBase
    : public WithCastable_<NodeCsBase, NodeEqBase>,
      public WithCase {
  public: static const char* Kind_() { return "NodeCsBase"; }

  // [I]
  protected: void init (Direction eDir, Case eCase, bool fNot) {
    m_eDirection = eDir;
    m_fNot = fNot;
    m_eCase = eCase;
  }
};

//////////////////////////////////////////////////////////////////////
//
// NodeOpBase
//
class NodeOpBase : public WithCastable_<NodeOpBase, Node> {
  public: static const char* Kind_() { return "NodeOpBase"; }

  private: Op m_eOp;

  // [C]
  public: void Compile(Compiler*, int) override;

  // [G]
  public: Op GetOp() const { return m_eOp; }

  // [I]
  protected: void init(Op const eOp) {
    m_eOp = eOp;
  }

  // [S]
  protected: void setOp(Op const eOp) {
    m_eOp = eOp;
  }
};

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
  public: int ComputeMinLength() const override {
    return m_pNode->ComputeMinLength();
  }

  // [G]
  public: Node* GetNode() const { return m_pNode; }

  // [I]
  protected: void init(Node* pNode) { m_pNode = pNode; }

  // [N]
  public: bool NeedStack() const override { return m_pNode->NeedStack(); }

  // [R]
  public: Node* Reverse() override {
    m_pNode = m_pNode->Reverse();
    return this;
  }

  #if _DEBUG
    public: void Print() const override {
      printf("(%s ", GetKind());
      m_pNode->Print();
      printf(")");
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeSubNodesBase
//
class NodeSubNodesBase : public WithCastable_<NodeSubNodesBase, Node> {
  public: static const char* Kind_() { return "NodeSubNodesBase"; }

  protected: Nodes m_oNodes;

  // [A]
  public: void Append(Node* pNode) { m_oNodes.Append(pNode); }

  // [D]
  public: void Delete(Node* pNode) { m_oNodes.Delete(pNode); }

  // [G]
  public: Node* GetFirst() const { return m_oNodes.GetFirst(); }

  // [R]
  public: Node* Reverse() override;

  #if _DEBUG
    public: void Print() const override {
      printf("(%s", GetKind());
      foreach (Nodes::Enum, oEnum, &m_oNodes) {
        printf(" ");
        oEnum.Get()->Print();
      }
      printf(")");
    }
  #endif
};

/// <remark>
/// Parse tree node "And".
/// </remark>
class NodeAnd : public WithCastable_<NodeAnd, NodeSubNodesBase>
{
  public: static const char* Kind_() { return "And"; }

  /// <remark>
  /// Constructs empty And node.
  /// </remark>
  public: NodeAnd() {}

  /// <remark>
  /// Constructs And node with one sub node.
  /// </remark>
  public: NodeAnd(Node* pNode1) { Append(pNode1); }

  /// <remark>
  /// Constructs And node with two sub nodes.
  /// </remark>
  public: NodeAnd(Node* pNode1, Node* pNode2) {
    Append(pNode1);
    Append(pNode2);
  }

  /// <remark>
  /// Constructs And node with three sub nodes.
  /// </remark>
  public: NodeAnd(Node* pNode1, Node* pNode2, Node* pNode3) {
    Append(pNode1);
    Append(pNode2);
    Append(pNode3);
  }

  // [C]
  /// <summary>
  /// Sum of value of all subnodes.
  /// </summary>
  public: int ComputeMinLength() const override {
    int nMinLen = 0;
    foreach (Nodes::Enum, oEnum, &m_oNodes) {
      auto const pNode = oEnum.Get();
      nMinLen += pNode->ComputeMinLength();
      if (nMinLen >= Infinity) return Infinity;
    }
    return nMinLen;
  }

  public: void Compile(Compiler*, int) override;

  // [S]
  public: Node* Simplify(IEnvironment*, LocalHeap*) override;
};

/// <remark>
/// Parse tree node for dot(.)
/// </remark>
class NodeAny
    : public WithCastable_<NodeAny, Node>,
      public WithDirection {
  public: static const char* Kind_() { return "Any"; }

  // ctor
  public: NodeAny(Direction eDir) {
    m_eDirection = eDir;
  }

  // [C]
  public: void Compile(Compiler*, int) override;
  public: int ComputeMinLength() const override { return 1; }

  // [G]
  public: Op GetOp() const { return IsBackward() ? Op_Any_B : Op_Any_F; }
};

/// <remark>
/// Parse tree node for atomic group <c>(?>...)</c>
/// </remark>
class NodeAtom : public WithCastable_<NodeAtom, NodeSubNodeBase> {
  public: static const char* Kind_() { return "Atom"; }

  public: NodeAtom(Node* pNode) {
    m_pNode = pNode;
  }

  // [C]
  public: void Compile(Compiler*, int) override;
};

/// <remark>
/// Parse tree node capture. Regex syntax is <c>(...)</c>.
/// </remark>
class NodeCapture
    : public WithCastable_<NodeCapture, NodeSubNodeBase>,
      public WithDirection {
  public: static const char* Kind_() { return "Capture"; }

  /// <summary>
  /// An index number of this capture.
  /// </summary>
  private: int m_iNth;

  /// <summary>
  /// Construct NodeCapture object.
  /// </summary>
  /// <param name="eDir">A direction of capturing</param>
  /// <param name="iNth">Capture index number</param>
  /// <param name="pNode">A node will be captured</param>
  public: NodeCapture(Direction eDir, Node* pNode, int iNth) {
    ASSERT(iNth >= 1);
    m_pNode = pNode;
    m_eDirection = eDir;
    m_iNth = iNth;
  }

  // [C]
  public: void Compile(Compiler*, int) override;

  // [G]
  /// <summary>
  /// Opcode of this node.
  /// </summary>
  /// <returns>
  /// Op_Capture_B for backward capturing, Op_Capture_F for
  /// forward capturing.
  /// </returns>
  public: Op GetOp() const {
    return IsBackward() ? Op_Capture_B : Op_Capture_F;
  }

  // [N]
  /// <summary>
  /// Alwasy true for NodeCapture.
  /// </summary>
  public: bool NeedStack() const override { return true; }

  #if _DEBUG
    public: virtual void Print() const override {
      printf("(%s[%d] ",
          IsBackward() ? "CaptureB" : "CaptureF",
          m_iNth);
      m_pNode->Print();
      printf(")");
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeCaptureEq -- \<n> \k<name>
//
class NodeCaptureEq
    : public WithCastable_<NodeCaptureEq, NodeCaptureBase>,
      public WithCase {
  public: static const char* Kind_() { return "CaptureEq"; }

  public: NodeCaptureEq(Direction eDir, int iNth, Case eCase) {
    m_eCase = eCase;
    m_eDirection = eDir;
    m_iNth = iNth;
  }

  // [C]
  public: void Compile(Compiler*, int) override;

  // [G]
  public: Op GetOp() const {
    return IsBackward() ?
        IsIgnoreCase() ? Op_CaptureEq_Ci_B : Op_CaptureEq_Cs_B :
        IsIgnoreCase() ? Op_CaptureEq_Ci_F : Op_CaptureEq_Cs_F;
  }
};

//////////////////////////////////////////////////////////////////////
//
// NodeCaptureIfNot
//
class NodeCaptureIfNot
    : public WithCastable_<NodeCaptureIfNot, NodeCaptureBase> {
  public: static const char* Kind_() { return "CaptureIfNot"; }

  public: NodeCaptureIfNot(Direction eDir, int iNth) {
    m_eDirection = eDir;
    m_iNth = iNth;
  }

  // [C]
  public: void Compile(Compiler*, int) override { CAN_NOT_HAPPEN(); }
};

/// <remark>
/// Parse tree node for character comparison.
/// </remark>
class NodeChar : public WithCastable_<NodeChar, NodeCsBase> {
  public: static const char* Kind_() { return "Char"; }

  private: char16 m_wch;

  /// <summary>
  /// Construct NodeChar.
  /// </summary>
  public: NodeChar(
      Direction eDir,
      char16 wch,
      Case eCase = CaseSensitive,
      bool fNot = false) {
    m_eCase = eCase;
    m_eDirection = eDir;
    m_fNot = fNot;
    m_wch = wch;
  }

  // [C]
  public: void Compile(Compiler*, int) override;
  public: void CompileNot(Compiler*, int) override;
  public: int ComputeMinLength() const override { return 1; }

  // [G]
  public: char16 GetChar() const { return m_wch; }

  public: Op GetOp(bool fNot) const {
    static const Op k_rgeOp[] = {
      Op_CharEq_Cs_F,
      Op_CharEq_Cs_B,

      Op_CharNe_Cs_F,
      Op_CharNe_Cs_B,

      Op_CharEq_Ci_F,
      Op_CharEq_Ci_B,

      Op_CharNe_Ci_F,
      Op_CharNe_Ci_B,
    };

    auto k = 0;
    if (IsBackward()) k |= 1;
    if (fNot) k |= 2;
    if (IsIgnoreCase()) k |= 4;
    return k_rgeOp[k];
  }

  // [I]
  public: bool IsMember(IEnvironment*, char16) const override;

  // [S]
  public: Node* Simplify(IEnvironment*, LocalHeap*) override;

  #if _DEBUG
    public: virtual void Print() const override {
      printf("(Char%s%s%s '%c')",
          IsNot() ? "Ne" : "Eq",
          IsIgnoreCase() ? "Ci" : "Cs",
          IsBackward() ? "B" : "F",
          GetChar());
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeCharClass
//
class NodeCharClass
    : public WithCastable_<NodeCharClass, NodeSubNodesBase>,
      public WithDirection {
  public: static const char* Kind_() { return "CharClass"; }

  private: bool m_fNot;

  public: NodeCharClass(Direction eDir, bool fNot) {
    m_eDirection = eDir;
    m_fNot = fNot;
  }

  // [C]
  public: void Compile(Compiler*, int) override;
  public: int ComputeMinLength() const override { return 1; }

  // [I]
  public: bool IsMember(IEnvironment* pIEnv, char16 wch) const {
    foreach (Nodes::Enum, oEnum, &m_oNodes) {
      if (oEnum.Get()->IsMember(pIEnv, wch)) {
        return true;
      }
    }
    return false;
  }

  public: bool IsNot() const { return m_fNot; }

  // [N]
  public: bool NeedStack() const override { return true; }

  // [S]
  public: Node* Simplify(IEnvironment*, LocalHeap*) override;

  #if _DEBUG
    public: void Print() const override {
      printf("(CharClass%s", IsNot() ? "Not" : "");
      foreach (Nodes::Enum, oEnum, &m_oNodes) {
        printf(" ");
        oEnum.Get()->Print();
      }
      printf(")");
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeCharSet
//
class NodeCharSet : public WithCastable_<NodeCharSet, NodeEqBase> {
  public: static const char* Kind_() { return "CharSet"; }

  private: int m_cwch;
  private: char16* m_pwch;

  // ctor
  public: NodeCharSet(Direction eDir, char16* pwch, int cwch, bool fNot)
    : m_cwch(cwch),
      m_pwch(pwch) {
    m_eDirection = eDir;
    m_fNot = fNot;
  }

  // [C]
  public: void Compile(Compiler*, int) override;
  public: int ComputeMinLength() const override { return 1; }

  // [G]
  public: Op GetOp() const {
    return IsBackward() ?
        IsNot() ? Op_CharSetNe_B : Op_CharSetEq_B :
        IsNot() ? Op_CharSetNe_F : Op_CharSetEq_F;
  }

  public: int GetLength() const { return m_cwch; }
  public: const char16* GetString() const { return m_pwch; }

  // [I]
  public: bool IsMember(IEnvironment*, char16 wch) const override {
    const char16* pwchEnd = m_pwch + m_cwch;
    for (const char16* pwch = m_pwch; pwch < pwchEnd; pwch++) {
      if (*pwch == wch) return ! m_fNot;
    }
    return m_fNot;
  }

  #if _DEBUG
    public: void Print() const override {
      printf("(%s '%ls')",
          IsNot() ? "CharSetNe" : "CharSetEq",
          m_pwch);
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeIf
//
class NodeIf : public WithCastable_<NodeIf, Node> {
  public: static const char* Kind_() { return "If"; }

  private: Node* m_pCond;
  private: Node* m_pElse;
  private: Node* m_pThen;

  // ctor
  public: NodeIf(Node* pCond, Node* pThen, Node* pElse)
    : m_pCond(pCond),
      m_pThen(pThen),
      m_pElse(pElse) {}

  // [C]
  public: void Compile(Compiler*, int) override;
  public: int ComputeMinLength() const override;

  // [N]
  public: bool NeedStack() const override { return true; }

  // [R]
  public: Node* Reverse() override {
    m_pCond = m_pCond->Reverse();
    m_pThen = m_pThen->Reverse();
    m_pElse = m_pElse->Reverse();
    return this;
  }

  // [S]
  public: virtual Node* Simplify(
      IEnvironment* pIEnv,
      LocalHeap* pHeap) override {
    m_pCond = m_pCond->Simplify(pIEnv, pHeap);
    m_pThen = m_pThen->Simplify(pIEnv, pHeap);
    m_pElse = m_pElse->Simplify(pIEnv, pHeap);
    return this;
  }

  #if _DEBUG
    public: virtual void Print() const override {
      printf("(if ");
      m_pCond->Print();
      printf(" ");
      m_pThen->Print();
      printf(" ");
      m_pElse->Print();
      printf(")");
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeLookaround
//
class NodeLookaround
    : public WithCastable_<NodeLookaround, NodeSubNodeBase> {
  public: static const char* Kind_() { return "Lookaround"; }

  private: bool m_fPositive;

  public: NodeLookaround(Node* pNode, bool fPositive)
      : m_fPositive(fPositive) {
    m_pNode = pNode;
  }

  // [C]
  public: virtual void Compile(Compiler*, int) override;
  public: virtual int ComputeMinLength() const override { return 0; }

  // [I]
  public: bool IsPositive() const { return m_fPositive; }

  // [N]
  public: virtual bool NeedStack() const override { return true; }
};

//////////////////////////////////////////////////////////////////////
//
// NodeMinMax
// Base class for NodeMax and NodeMin.
//
class NodeMinMax
    : public WithCastable_<NodeMinMax, NodeSubNodeBase>,
      public WithDirection {
  public: static const char* Kind_() { return "MinMax"; }

  protected: int m_iMax;
  protected: int m_iMin;

  // [C]
  public: virtual int ComputeMinLength() const override {
    return m_pNode->ComputeMinLength() * m_iMin;
  }

  // [G]
  public: int GetMax() const { return m_iMax; }
  public: int GetMin() const { return m_iMin; }

  // [I]
  protected: void init(Direction eDir, Node* pNode, MinMax oMinMax) {
    m_eDirection = eDir;
    m_pNode = pNode;
    m_iMax = oMinMax.m_iMax;
    m_iMin = oMinMax.m_iMin;
  }

  // [N]
  public: virtual bool NeedStack() const override { return true; }

  #if _DEBUG
    public: virtual void Print() const override {
      printf("(%s min=%d max=%d ", GetKind(), GetMin(), GetMax());
          GetNode()->Print();
          printf(")");
    }
  #endif
};

//////////////////////////////////////////////////////////////////////
//
// NodeMax
//
class NodeMax : public WithCastable_<NodeMax, NodeMinMax> {
  public: static const char* Kind_() { return "Max"; }

  public: NodeMax(Direction eDir, Node* pNode, MinMax oMinMax) {
    init(eDir, pNode, oMinMax);
  }

  // [C]
  public: virtual void Compile(Compiler*, int) override;
};

//////////////////////////////////////////////////////////////////////
//
// NodeMin
//
class NodeMin : public WithCastable_<NodeMin, NodeMinMax> {
  public: static const char* Kind_() { return "Min"; }

  public: NodeMin(Direction eDir, Node* pNode, MinMax oMinMax) {
    init(eDir, pNode, oMinMax);
  }

  // [C]
  public: virtual void Compile(Compiler*, int) override;
};

//////////////////////////////////////////////////////////////////////
//
// NodeOneWidth
//
class NodeOneWidth : public WithCastable_<NodeOneWidth, NodeOpBase> {
  public: static const char* Kind_() { return "OneWidth"; }

  public: NodeOneWidth(Op eOp) { Base::init(eOp); }

  // [C]
  public: virtual void CompileNot(Compiler*, int) override;
  public: virtual int ComputeMinLength() const override { return 1; }

  // [G]
  public: Op GetNotOp() const { return static_cast<Op>(GetOp() + 1); }

  // [I]
  public: virtual bool IsMember(IEnvironment*, char16 wch) const override;

  // [N]
  public: virtual Node* Not() override;

};

//////////////////////////////////////////////////////////////////////
//
// NodeOr
//
class NodeOr : public WithCastable_<NodeOr, NodeSubNodesBase> {
  public: static const char* Kind_() { return "Or"; }

  // ctor
  public: NodeOr() {}

  public: NodeOr(Node* pNode1) { m_oNodes.Append(pNode1); }

  public: NodeOr(Node* pNode1, Node* pNode2) {
      m_oNodes.Append(pNode1);
      m_oNodes.Append(pNode2);
  }

  // [C]

  // ComputeMinLength - minimum value of all subnodes.
  public: virtual int ComputeMinLength() const override {
    auto nMinLen = int(Infinity);
    foreach (Nodes::Enum, oEnum, &m_oNodes) {
        auto const pNode = oEnum.Get();
        nMinLen = min(nMinLen, pNode->ComputeMinLength());
    }
    return nMinLen;
  }

  public: virtual void Compile(Compiler*, int) override;

  // [G]
  public: Nodes* GetNodes() { return &m_oNodes; }

  // [N]
  public: virtual bool NeedStack() const override { return true; }
};

//////////////////////////////////////////////////////////////////////
//
// NodeRange
// Represent simple character class, e.g. [A-Z].
//
class NodeRange : public WithCastable_<NodeRange, NodeCsBase> {
  public: static const char* Kind_() { return "Range"; }

  private: char16 m_wchMin;
  private: char16 m_wchMax;

  public: NodeRange(
      Direction eDir,
      char16 wchMin,
      char16 wchMax,
      Case eCase,
      bool fNot = false)
      : m_wchMin(wchMin),
        m_wchMax(wchMax) {
    init(eDir, eCase, fNot);
  }

  // [C]
  public: virtual void Compile(Compiler*, int) override;
  public: virtual void CompileNot(Compiler*, int) override;
  public: virtual int ComputeMinLength() const override { return 1; }

  // [G]
  public: char16 GetMaxChar() const { return m_wchMax; }
  public: char16 GetMinChar() const { return m_wchMin; }

  public: Op GetOp(bool fNot) const {
    static const Op k_rgeOp[] = {
      Op_RangeEq_Cs_F,
      Op_RangeEq_Cs_B,

      Op_RangeNe_Cs_F,
      Op_RangeNe_Cs_B,

      Op_RangeEq_Ci_F,
      Op_RangeEq_Ci_B,

      Op_RangeNe_Ci_F,
      Op_RangeNe_Ci_B,
    };

    auto k = 0;
    if (IsBackward()) k |= 1;
    if (fNot) k |= 2;
    if (IsIgnoreCase()) k |= 4;
    return k_rgeOp[k];
  }

  // [I]
  public: virtual bool IsMember(IEnvironment*, char16) const override;

  // [S]
  public: virtual Node* Simplify(IEnvironment*, LocalHeap*) override;

  #if _DEBUG
    public: virtual void Print() const override {
      printf("(Range%s%s %lc-%lc)",
          IsNot() ? "Not" : "",
          IsIgnoreCase() ? "Ci" : "Cs",
          GetMinChar(),
          GetMaxChar());
    }
  #endif
};

/// <remark>
/// Parse tree node for string comparison.
/// </remark>
class NodeString : public WithCastable_<NodeString, NodeCsBase> {
  public: static const char* Kind_() { return "StringEq"; }

  private: int m_cwch;
  private: const char16* m_pwch;

  /// <summary>
  /// Construct NodeString.
  /// </summary>
  public: NodeString(
      Direction eDir,
      const char16* pwch,
      int cwch,
      Case eCase = CaseSensitive,
      bool fNot = false)
      : m_cwch(cwch),
        m_pwch(pwch) {
    Base::init(eDir, eCase, fNot);
  }

  // [C]
  public: virtual int ComputeMinLength() const override { return m_cwch; }
  public: virtual void Compile(Compiler*, int) override;

  // [G]
  public: int GetLength() const { return m_cwch; }

  public: Op GetOp(bool fNot) const {
    static const Op k_rgeOp[] = {
      Op_StringEq_Cs_F,
      Op_StringEq_Cs_B,

      Op_StringNe_Cs_F,
      Op_StringNe_Cs_B,

      Op_StringEq_Ci_F,
      Op_StringEq_Ci_B,

      Op_StringNe_Ci_F,
      Op_StringNe_Ci_B,
    };

    auto k = 0;
    if (IsBackward()) k |= 1;
    if (fNot) k |= 2;
    if (IsIgnoreCase()) k |= 4;
    return k_rgeOp[k];
  }

  public: const char16* GetStart() const { return m_pwch; }

  // [P]
  #if _DEBUG
    public: virtual void Print() const override {
      printf("(String%s%s%s '%ls')",
          IsNot() ? "Ne" : "Eq",
          IsIgnoreCase() ? "Ci" : "Cs",
          IsBackward() ? "B" : "F",
          m_pwch);
      }
  #endif
};

/// <summary>
/// Parse tree node for void. This is used for empty capture.
/// </summary>
class NodeVoid : public WithCastable_<NodeVoid, Node> {
  public: static const char* Kind_() { return "Void"; }

  // [C]
  public: virtual void Compile(Compiler*, int) override {}
};

//////////////////////////////////////////////////////////////////////
//
// NodeZeroWidth
// Op_AsciiBoundary "(?a:\b)"
// Op_AfterNewline "^"m scanner
// Op_AsciiNotBoundary "(?a:\B)"
// Op_BeforeNewLine "(?m:$)" scanner
// Op_EndOfLine "$" "\Z" scanner
// Op_EndOfString "(?s:$)" "\z" scanner
// Op_Posn "\G" scanner
// Op_StartOfString "^" "\A" scanner
// Op_UnicodeBoundary "(?:u\b)"
// Op_UnicodeNotBoundary "(?:u\B)"
//
// See Also:
// k_rgoZeroWidthMap in regex_parse.cpp
// Compiler::compileScanner in regex_compile.cpp
// Engine::Execute in regex_exec.cpp
//
class NodeZeroWidth : public WithCastable_<NodeZeroWidth, NodeOpBase> {
  public: static const char* Kind_() { return "ZeroWidth"; }
  public: NodeZeroWidth(Op eOp) { Base::init(eOp); }
};

//////////////////////////////////////////////////////////////////////
//
// CaptureDef
//
struct CaptureDef
    : LocalObject,
      DoubleLinkedItem_<CaptureDef> {
  int m_iNth;
  char16* m_pwszName;

  CaptureDef(char16* pwszName, int iNth)
    : m_iNth(iNth), m_pwszName(pwszName) {}
};

typedef DoubleLinkedList_<CaptureDef> CaptureDefs;

//////////////////////////////////////////////////////////////////////
//
// Tree
//
class Tree : public LocalObject {
  public: int m_cCaptures;
  public: int m_iErrorCode;
  public: long m_lErrorPosn;
  public: CaptureDefs m_oCaptures;
  public: Node* m_pNode;
  public: int m_rgfOption;
  public: char16* m_prgpwszCaptureName;

  public: Tree(int rgfOption)
    : m_cCaptures(0),
      m_iErrorCode(0),
      m_lErrorPosn(0),
      m_pNode(NULL),
      m_rgfOption(rgfOption),
      m_prgpwszCaptureName(NULL) {}
};

} // RegexPrivate
} // Regex

#endif //!defined(INCLUDE_regex_node_h)
