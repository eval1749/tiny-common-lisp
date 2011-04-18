//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Compiler Code Generator
// tinycl_c_cg.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg.h#10 $
//
#if !defined(INCLUDE_tinycl_c_cg_h)
#define INCLUDE_tinycl_c_cg_h

#include "../tinycl_c_defs.h"

namespace TinyCl
{

namespace Compiler
{

struct RegDesc;
struct RegSet;
struct RegGroup;

class Physical;
class StackSlot;
class ThreadSlot;

struct RegDesc
{
    RegClass    m_eRegClass;
    uint        m_nId;
    uint        m_nSize;
    uint        m_nIndex;
    const char* m_pszName;
}; // RegDesc

struct RegSet
{
    RegClass                m_eRegClass;
    int                     m_c;
    const RegDesc* const*   m_prgpReg;

    // [E]
    class Enum
    {
        private: int           m_i;
        private: const RegSet* m_p;

        public: Enum(const RegSet* p) :
            m_p(p), m_i(0) {}

        public: bool AtEnd() const
            { return m_i >= m_p->m_c; }

        public: const RegDesc* Get() const
        {
            ASSERT(! AtEnd());
            return m_p->m_prgpReg[m_i];
        } // Get

        public: int GetIndex() const
        {
            ASSERT(! AtEnd());
            return m_i;
        } // GetIndex

        public: void Next()
            { ASSERT(! AtEnd()); m_i += 1; }
    }; // Enum
}; // RegSet

struct RegGroup
{
    const char*     m_pszName;
    RegClass        m_eRegClass;
    uint            m_cbWidth;
    const RegDesc*  m_pRn;
    const RegDesc*  m_pRsp;
    const RegDesc*  m_pRtcb;
    const RegSet*   m_pAll;
    const RegSet*   m_pAllocable;
    const RegSet*   m_pFree;
    const RegSet*   m_pArgs;
    const RegSet*   m_pCalleeSave;
    const RegSet*   m_pCallerSave;
}; // RegGroup

/// <summary>
///   Represents closed variable in IR.
/// </summary>
class ClosedMarker :
    public Operand_<ClosedMarker, Pseudo>
{
    public: static const char* Kind_() { return "ClosedMarker"; }

    private: uint m_nNth;

    public: ClosedMarker(uint nNth) :
        m_nNth(nNth) {}

    // [G]
    public: uint GetNth() const { return m_nNth; }
    protected: override char16 getPrefix() const { return 'c'; }

    public: override const Type* GetTy() const
        { return tyT; }
}; // ClosedMarker

/// <summary>
///   Represents function literal for function being compiled in IR.
/// </summary>
class FunLit :
    public Operand_<FunLit, Immediate>
{
    public: static const char* Kind_() { return "FunLit"; }

    private: Function* m_pFun;

    // ctor
    public: FunLit(Function* pFun) :
        m_pFun(pFun) {}

    // [G]
    public: Function* GetFun() const { return m_pFun; }


    public: override const Type* GetTy() const
        { return GetFun()->GetFunty(); }

    // [H]
    public: override void HtmlPrint(Val, bool = false) const;
}; // FunLit

/// <summary>
///   Represents memory location in IR.
/// </summary>
class TlvOffset :
    public Operand_<TlvOffset, Immediate>
{
    public: static const char* Kind_() { return "TlvOffset"; }

    private: Val m_tlvrec;

    // ctor
    public: TlvOffset(Val tlvrec) :
        m_tlvrec(tlvrec) { ASSERT(tlv_record_p(tlvrec)); }

    // [G]
    public: Val GetTlvRec() const { return m_tlvrec; }

    // [H]
    public: override void HtmlPrint(Val, bool = false) const;
}; // TlvOffset

/// <summary>
///   Represents base class for output operand used during CG.
/// </summary>
class CgOutput : public WithCastable_<CgOutput, Output>
{
    public: static const char* Kind_() { return "CgOutput"; }
}; // CgOutput

/// <summary>
///   Represents memory location in IR.
/// </summary>
class MemSlot : 
    public WithCastable_<MemSlot, CgOutput>
{
    public: static const char* Kind_() { return "MemSlot"; }

    protected: RegClass m_eRegClass;
    private:   int      m_iLocation;

    // [G]
    public: RegClass GetClass()    const { return m_eRegClass; }
    public: int      GetLocation() const { return m_iLocation; }

    // [S]
    public: int  SetLocation(int i) { return m_iLocation = i; }
}; // MemSlot

/// <summary>
///   Represents physical register in IR.
/// </summary>
class Physical :
    public Operand_<Physical, CgOutput>
{
    public: static const char* Kind_() { return "Physical"; }

    public:  int            m_cUses;
    private: const RegDesc* m_pRegDesc;

    // ctor
    public: Physical(const RegDesc* p) :
        m_cUses(0), m_pRegDesc(p) {}

    // [G]
    public: const RegDesc* GetDesc() const { return m_pRegDesc; }

    // [S]
    public: override void HtmlPrint(Val, bool = false) const;
}; // Physical

class StackSlot :
    public Operand_<StackSlot, MemSlot>,
    public WorkListItem_<StackSlot>
{
    public: static const char* Kind_() { return "StackSlot"; }

    private: Variable*  m_pVar;

    // ctor
    public: StackSlot(
        RegClass    e = RegClass_Gpr,
        Int         i = 0,
        Variable*   pVar = NULL ) :
        m_pVar(pVar)
    {
        m_eRegClass    = e;
        SetLocation(static_cast<int>(i));
    } // StackSlot

    // [G]
    public: Variable* GetVar() const { return m_pVar; }

    // [H]
    public: override void HtmlPrint(Val, bool = false) const;
}; // StackSlot

class ThreadSlot :
    public Operand_<ThreadSlot, MemSlot>
{
    public: static const char* Kind_() { return "ThreadSlot"; }

    // ctor
    public: ThreadSlot(RegClass e, Int i)
    {
        m_eRegClass    = e;
        SetLocation(static_cast<int>(i));
    } // ThreadSlot

    // [H]
    public: override void HtmlPrint(Val, bool = false) const;
}; // ThreadSlot

class Target
{
    private: const RegGroup*    m_pFprGroup;
    private: const RegGroup*    m_pGprGroup;

    // ctor
    public: Target(const RegGroup* pFprGroup, const RegGroup* pGprGroup) :
        m_pFprGroup(pFprGroup),
        m_pGprGroup(pGprGroup) {}

    // [C]
    public: virtual int ComputeFrameSize(FrameReg*) const = 0;

    // [G]
    public: const RegGroup* GetFprGroup() const
        { return m_pFprGroup; }

    public: const RegGroup* GetGprGroup() const
        { return m_pGprGroup; }

    public: virtual CgOutput* GetArgReg(uint) = 0;
    public: virtual Physical* GetPhysical(const RegDesc*) = 0;
}; // Target

// Used for using spilled memory slot as variable slot.
// See VarDefI processing in SubPassAssign.
class VarHome :
    public Operand_<VarHome, MemSlot>,
    public WorkListItem_<VarHome>
{
    public: static const char* Kind_() { return "VarHome"; }

    private: Variable*  m_pVar;

    // ctor
    public: VarHome(
        Variable*   pVar,
        RegClass    e = RegClass_Gpr,
        Int         i = 0 ) :
        m_pVar(pVar)
    {
        m_eRegClass = e;
        SetLocation(static_cast<int>(i));
    } // VarHome

    // [G]
    public: Variable* GetVar() const { return m_pVar; }

    // [H]
    public: override void HtmlPrint(Val, bool = false) const;
}; // VarHome

class CopyInstruction :
    public WithCastable_<CopyInstruction, Instruction>
{
    public: static const char* Kind_() { return "CopyInstruction"; }

    // ctor - Preven to make instances
    protected:; CopyInstruction() {}

    // [I]
    public: override bool IsUseless() const;
}; // CopyInstruction

// AssignI
class AssignI : public Instruction_<AssignI, Op_Assign, CopyInstruction>
{
    public: AssignI(Register* pRd, Operand* pSx) :
        Base(GetOp_(), tyT, pRd)
        { AppendOperand(pSx); }
}; // AssignI

// CopyI
class CopyI : public Instruction_<CopyI, Op_Copy, CopyInstruction>
{
    public: CopyI(Output* pRd, Operand* pSx) :
        Base(GetOp_(), tyT, pRd)
        { AppendOperand(pSx); }

    public: CopyI(const Type* pty, Output* pRd, Operand* pSx) :
        Base(GetOp_(), pty, pRd)
        { AppendOperand(pSx); }
}; // CopyI

// ReloadI ty %pd <= %mx
class ReloadI : public Instruction_<ReloadI, Op_Reload, CopyInstruction>
{
    public: ReloadI(const Type* pty, Physical* pRd, StackSlot* pMx) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pMx);
    } // ReloadI
}; // ReloadI

// PhiCopyI ty %rd <= %rx
class PhiCopyI : public Instruction_<PhiCopyI, Op_PhiCopy, CopyInstruction>
{
    public: PhiCopyI(CgOutput* pRd, Operand* pSx) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pSx);
    } // PhiCopyI
}; // PhiCopyI

// SpillI ty %md <= %rx
class SpillI : public Instruction_<SpillI, Op_Spill, CopyInstruction>
{
    public: SpillI(const Type* pty, StackSlot* pMd, Physical* pRx) :
        Base(GetOp_(), pty, pMd)
    {
        AppendOperand(pRx);
    } // SpillI
}; // SpillI

/// <summary>
///   SWAP instruction. Register allocator pass intorduces this instruction
///   for breaking cyclin during serializing parallel copy.
///   <para>
///     SWAP %pd &lt;= %px
///   </para>
/// </summary>
class SwapI : public Instruction_<SwapI, Op_Swap>
{
    public: SwapI(Physical* const pPd, Physical* const pPx) :
        Base(GetOp_(), tyT, pPd)
    {
        AppendOperand(pPx);
    } // SwapI
}; // SwapI

/// <summary>
///     VARREF instruction. Generates reference of owned variable. X86 Ensure
///     pass introduces this instruction for passing reference of owned
///     variable to inner function.
///   <para>
///     VARREF ty %rd &lt;= %qx
///   </para>
/// </summary>
class VarRefI : public Instruction_<VarRefI, Op_VarRef>
{
    public: VarRefI(
        Register* const pRd, 
        Pseudo* const pQx ) :
            Base(GetOp_(), tyPtrT, pRd)
    {
        AppendOperand(pQx);
    } // VarRefI
}; // VarRefI

/// <summary>
///     UPVARREF instruction. Reference of up level variable.
///   <para>
///     UPVARREF ty %rd &lt;= %rx %qy
///   </para>
/// </summary>
class UpVarRefI : public Instruction_<UpVarRefI, Op_UpVarRef>
{
    public: UpVarRefI(
        Register* const pRd, 
        Output*   const pRx, 
        Pseudo*   const pQy ) :
            Base(GetOp_(), tyPtrT, pRd)
    {
        AppendOperand(pRx);
        AppendOperand(pQy);
    } // UpVarRefI
}; // UpVarRefI

// VarAnnexI ty %md <= %sx %qy
class VarAnnexI : public Instruction_<VarAnnexI, Op_VarAnnex>
{
    public: VarAnnexI(StackSlot* pMd, ClosedMarker* pCx) :
        Base(GetOp_(), tyT, pMd)
    {
        AppendOperand(pCx);
    } // VarAnnexI
}; // VarAnnexI

// VarHomeI ty %md <= %qx %ry
class VarHomeI : public Instruction_<VarHomeI, Op_VarHome>
{
    public: VarHomeI(const Type* pty, Output* pRd, Output* pRx, Operand* pSy) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pRx);
        AppendOperand(pSy);
    } // VarHomeI
}; // VarHomeI

} // Compiler

} // TinyCl

#endif //!defined(INCLUDE_tinycl_c_cg_h)
