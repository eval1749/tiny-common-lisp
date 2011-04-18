//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Compiler - IR Instruction
// tinycl_c_insn.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_insn.h#28 $
//
#if !defined(INCLUDE_tinycl_c_insn_h)
#define INCLUDE_tinycl_c_insn_h

namespace TinyCl
{

namespace Compiler
{

class ArithmeticInstruction : public Instruction
{
}; // ArithmeticInstruction

class CastInstruction :
    public WithCastable_<CastInstruction, Instruction>
{
    public: static const char* Kind_() { return "CastInstruction"; }

    // [C]
    public: override Operand* Compute() const;

    // [O]
    public: override bool Optimize();
}; // CastInstruction

/// <summary>
///   Family members of Last instruction:
///   <list>
///     <item>Branch</item>
///     <item>Exit</item>
///     <item>Jump</item>
///   </list>
/// </summary>
class LastInstruction : public Instruction
{
    protected: override bool Verify() const;

    // [O]
    public: override void OnMove(BBlock* const)
        { CAN_NOT_HAPPEN(); }
}; // LastInstruction

/// <summary>
///   Family members of Terminate instruction:
///   <list>
///     <item>Ret</item>
///     <item>Unreachable</item>
///   </list>
/// </summary>
class TerminateInstruction :
    public WithCastable_<TerminateInstruction, LastInstruction>
{
    public: static const char* Kind_() { return "TerminateInstruction"; }

    // ctor - Preven to make instances
    protected: TerminateInstruction() {}

    // [O]
    public: override void OnMove(BBlock* const);

    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // TerminateInstruction

/// <summary>
///   Notinline instruction attribute
/// </summary>
class WithAttrNotinline
{
    private: bool m_fNotinline;

    protected: WithAttrNotinline() :
        m_fNotinline(false) {}

    // [I]
    public: bool IsNotinline() const { return m_fNotinline; }

    // [S]
    public: bool SetNotinline(bool f) { return m_fNotinline = f; }
}; // WithAttrNotinline


// [A]
/// <summary>
///   ADD instruction.
///   <para>
///     ADD ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of ADD operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class AddI :
    public Instruction_<AddI, Op_Add, ArithmeticInstruction>
{
    public: AddI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // AddI
}; // AddI

// [B]
/// <summary>
///   Pseudo instruction BOX.
///   <example>
///     BOX ty %rd &lt;= %rx
///   </example>
/// </summary>
class BoxI :
    public Instruction_<BoxI, Op_Box>
{
    public: BoxI(
        const Type* const pty,
        Output*     const pRd,
        Register*   const pRx ) :
            Base(GetOp_(), pty, pRd)
        { AppendOperand(pRx); }

    // [C]
    public: override Operand* Compute() const;
}; // BoxI

/// <summary>
///   Branch instruction. Transfers controls to BBy if %bx is true, otherwise
///   transfers to BBz.
///   <example>
///     BRANCH %bx BBy BBz
///   </example>
///   <para>
///     We prefer positive branch over negative branch and call positive
///     branch as normal form. The method Optimize transform negative
///     branch to positive branch.
///   </para>
///   <para>
///     Postive branch
///     <example>
///       NE %b1 &lt;= %r2 nil
///       BRANCH %b1 BB3 BB4        ; branch to BB3 if %r2 is true
///     </example>
///   </para>
///   <para>
///     Postive branch
///     <example>
///       EQ %b1 &lt;= %r2 nil
///       BRANCH %b1 BB3 BB4        ; branch to BB3 if %r2 is false
///     </example>
///   </para>
/// </summary>
class BranchI :
    public Instruction_<BranchI, Op_Branch, LastInstruction>
{
    public: BranchI(Bool* pBx, BBlock* pBBy, BBlock* pBBz) :
        Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(pBx);
        AppendOperand(pBBy->GetLabel());
        AppendOperand(pBBz->GetLabel());
    } // BranchI

    // [G]
    public: BBlock* GetFalseBB() const
        { return GetSz()->StaticCast<Label>()->GetBB(); }

    public: BBlock* GetTrueBB() const
        { return GetSy()->StaticCast<Label>()->GetBB(); }

    // [O]
    public: override void OnMove(BBlock* const);
    public: override bool Optimize();

    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // BranchI

// [C]

/// <summary>
///   Call instruction.
///   <para>
///     CALL ty %sd &lt;= %sx %vy
///   </para>
/// </summary>
class CallI :
    public Instruction_<CallI, Op_Call>,
    public WithAttrNotinline
{
    private: FrameReg*  m_pFrameReg;

    public: CallI(
        const Type* const pty,
        Output*     const pOut,
        Operand*    const pSx,
        Values*     const pVy ) :
            m_pFrameReg(NULL),
            Base(GetOp_(), pty, pOut)
    {
        if (Function* const pFun = pSx->DynamicCast<Function>())
        {
            AppendOperand(new FunctionOperandBox(pFun));
        }
        else
        {
            AppendOperand(pSx);
        }

        AppendOperand(pVy);
    } // CallI

    public: CallI(
        const Type* const pty,
        Output*     const pOut,
        Val         const fn,
        Values*     const pVy ) :
            m_pFrameReg(NULL),
            Base(GetOp_(), pty, pOut)
    {
        AppendOperand(Literal::New(fn));
        AppendOperand(pVy);
    } // CallI

    public: CallI(
        const Type* const pty,
        Output*     const pOut,
        Val         const fn ) :
            m_pFrameReg(NULL),
            Base(GetOp_(), pty, pOut)
    {
        AppendOperand(Literal::New(fn));
        AppendOperand(Void);
    } // CallI

    // [C]
    public: override Operand* Compute() const;

    // [G]
    public: FrameReg* GetFrameReg() const { return m_pFrameReg; }

    // [I]
    public: override bool IsUseless() const;

    // [O]
    public: override bool Optimize();

    // [S]
    public: FrameReg* SetFrameReg(FrameReg* p) { return m_pFrameReg = p; }
}; // CallI

// CatchI
class CatchI : public Instruction_<CatchI, Op_Catch>
{
    // ctor
    public: CatchI(FrameReg* pRx, BBlock* pBB, Val typespec) :
        Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(pRx);
        AppendOperand(pBB->GetLabel());
        AppendOperand(Literal::New(typespec));
    } // CatchI

    // [I]
    public: override bool IsUseless() const;

    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // CatchI

// CloseI
class CloseI : public Instruction_<CloseI, Op_Close>
{
    // ctor
    public: CloseI(FrameReg* pRx) :
        Base(GetOp_(), tyVoid, Void)
        { AppendOperand(pRx); }

    // [I]
    public: override bool IsUseless() const
        { return NULL == GetSx()->StaticCast<FrameReg>()->GetDefI(); }
}; // CloseI

// ClosureI
class ClosureI :
    public Instruction_<ClosureI, Op_Closure>,
    public WithAttrNotinline
{
    public: ClosureI(
        const TyFunction*   const pFunty,
        Register*           const pRd,
        Function*           const pFun,
        Values*             const pVy ) :
            Base(GetOp_(), pFunty, pRd)
    {
        AppendOperand(new FunctionOperandBox(pFun));
        AppendOperand(pVy);
    } // ClosureI

    // Note: Since we need to propagate function type and notinline attribute
    // to CALL instruciton, we don't have Compute method for CLOSURE
    // instruction.
}; // ClosureI

/// <summary>
///   CONVERT instruction used for loading floating-point number.
///   <example>
///     CONVERT ty %rd &lt;= %rx
///   </example>
/// </summary>
class ConvertI : public Instruction_<ConvertI, Op_Convert>
{
    public: ConvertI(
        const Type* const pty,
        Output*     const pRd,
        Output*     const pRx ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pRx);
    } // ConvertI

    // [C]
    public: override Operand* Compute() const;
}; // ConvertI

// CountI
class CountI : public Instruction_<CountI, Op_Count>
{
    public: CountI(Register* const pRd, Values* const pVx) :
        Base(GetOp_(), tyFixnum, pRd)
        { AppendOperand(pVx); }

    // [C]
    public: override Operand* Compute() const;
}; // CountI

// [D]
/// <summary>
///   DIV instruction.
///   <para>
///     DIV ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of DIV operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class DivI :
    public Instruction_<DivI, Op_Div, ArithmeticInstruction>
{
    public: DivI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // DivI
}; // DivI


// [E]

// EntryI
class EntryI : public Instruction_<EntryI, Op_Entry>
{
    public: EntryI() :
        Base(GetOp_(), tyVoid, Void) {}

    private: override bool Verify() const;
}; // EntryI

/// <summary>
///   EQ instruction
/// </summary>
/// <example>
///     EQ bool %bd &lt;= %sx %sy
/// </example>
class EqI : public Instruction_<EqI, Op_Eq>
{
    public: EqI(Bool* pBd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // EqI
}; // EqI

// ExitI <none>
class ExitI : public Instruction_<ExitI, Op_Exit, LastInstruction>
{
    public: ExitI() :
        Base(GetOp_(), tyVoid, Void) {}
}; // ExitI

// [F]

// FrameI
class FrameI : public Instruction_<FrameI, Op_Frame>
{
    public: FrameI(Register* pRd, FrameReg* pRx) :
        Base(GetOp_(), tyPtrT, pRd)
        { AppendOperand(pRx); }
}; // FrameI

// [G]

/// <summary>
///   GE instruction
/// </summary>
/// <example>
///     GE bool %bd &lt;= %sx %sy
/// </example>
class GeI : public Instruction_<GeI, Op_Ge>
{
    // ctor
    public: GeI(Bool* pBd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // GeI
}; // GeI

/// <summary>
///   GO instruction
/// </summary>
/// <example>
///     GO %rx
/// </example>
class GoI : public Instruction_<GoI, Op_Go>
{
    public: GoI(Register* pRx) :
        Base(GetOp_(), tyVoid, Void)
        { AppendOperand(pRx); }
}; // GoI

/// <summary>
///   GT instruction
/// </summary>
/// <example>
///     GT bool %bd &lt;= %sx %sy
/// </example>
class GtI : public Instruction_<GtI, Op_Gt>
{
    // ctor
    public: GtI(Bool* pBd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // GtI

    // [C]
    public: override Operand* Compute() const;
}; // GtI

// [I]
/// <summary>
///   If instruction
///   <para>
///     IF ty %d &lt;= %bx %sy %sz
///   </para>
/// </summary>
class IfI : public Instruction_<IfI, Op_If>
{
    // ctor
    public: IfI(Output* pRd, Bool* pBx, Operand* pSy, Operand* pSz) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pBx);
        AppendOperand(pSy);
        AppendOperand(pSz);
    } // IfI

    public: IfI(
        const Type* const pty,
        Output*     const pRd,
        Bool*       const pBx,
        Operand*    const pSy,
        Operand*    const pSz ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pBx);
        AppendOperand(pSy);
        AppendOperand(pSz);
    } // IfI

    // [C]
    public: override Operand* Compute() const;

    // [I]
    public: bool IsTrueFalse() const;

    // [O]
    public: override bool Optimize();
}; // IfI

// [J]
// JumpI label
class JumpI : public Instruction_<JumpI, Op_Jump, LastInstruction>
{
    public: JumpI(BBlock* pBBlock) :
        Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(new Label(pBBlock));
    } // JumpI

    // [G]
    public: Label* GetLabel() const
        { return GetSx()->StaticCast<Label>(); }

    public: BBlock* GetTargetBB() const
        { return GetLabel()->GetBB(); }

    // [O]
    public: override void OnMove(BBlock* const);

    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // JumpI

// [K]
class KeySuppliedI :
    public Instruction_<KeySuppliedI, Op_KeySupplied>
{
    // ctor
    public: KeySuppliedI(Bool* pBd, Register* pRx, Val key) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pRx);
        AppendOperand(Literal::New(key));
    } // KeySuppliedI
}; // KeySuppliedI

class KeyValI : public Instruction_<KeyValI, Op_KeyVal>
{
    // ctor
    public: KeyValI(Register* pRd, Values* pVx, Val key) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pVx);
        AppendOperand(Literal::New(key));
    } // KeyValI
}; // KeyValI

// [L]
/// <summary>
///   LE instruction
/// </summary>
/// <example>
///     LE bool %bd &lt;= %sx %sy
/// </example>
class LeI : public Instruction_<LeI, Op_Le>
{
    // ctor
    public: LeI(Bool* pBd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // LeI
}; // LeI

// LoadI ty %rd <= %rx
class LoadI : public Instruction_<LoadI, Op_Load>
{
    public: LoadI(const Type* pty, Output* pRd, Output* pRx) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pRx);
    } // LoadI

    public: LoadI(Output* pRd, Output* pRx) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pRx);
    } // LoadI
}; // LoadI

// LoadFunI ty %rd <= cell
//  cell = symbol | value-cell | tlv-record
class LoadFunI :
    public Instruction_<LoadFunI, Op_LoadFun>,
    public WithAttrNotinline
{
    public: LoadFunI(const TyFunction* pFunty, Register* pRd, Operand* pSx) :
        Base(GetOp_(), pFunty, pRd)
    {
        AppendOperand(pSx);
    } // LoadFunI
}; // LoadFunI

/// <summary>
///   LOGAND instruction.
///   <para>
///     LOGAND ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of AND operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class LogAndI :
    public Instruction_<LogAndI, Op_LogAnd, ArithmeticInstruction>
{
    public: LogAndI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // LogAndI
}; // LogAndI

/// <summary>
///   LOGEQV instruction.
///   <para>
///     LOGEQV ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of EQV operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class LogEqvI :
    public Instruction_<LogEqvI, Op_LogEqv, ArithmeticInstruction>
{
    public: LogEqvI(
        const Type* const pty,
        Output*     const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // LogEqvI
}; // LogEqvI

/// <summary>
///   LOGIOR instruction.
///   <para>
///     LOGIOR ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of IOR operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class LogIorI :
    public Instruction_<LogIorI, Op_LogIor, ArithmeticInstruction>
{
    public: LogIorI(
        const Type* const pty,
        Output*     const pRd,
        Operand*    const pSx,
        Operand*    const pSy) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // LogIorI
}; // LogIorI

/// <summary>
///   LOGXOR instruction.
///   <para>
///     LOGXOR ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of XOR operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class LogXorI :
    public Instruction_<LogXorI, Op_LogXor, ArithmeticInstruction>
{
    public: LogXorI(
        const Type* const pty,
        Output*     const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // LogXorI
}; // LogXorI

/// <summary>
///   LT instruction
/// </summary>
/// <example>
///     LT bool %bd &lt;= %sx %sy
/// </example>
class LtI : public Instruction_<LtI, Op_Lt>
{
    // ctor
    public: LtI(Bool* pBd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // LtI
}; // LtI

// [M]

/// <summary>
///   MUL instruction.
///   <para>
///     MUL ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of MUL operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class MulI :
    public Instruction_<MulI, Op_Mul, ArithmeticInstruction>
{
    public: MulI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // MulI
}; // MulI

// MvRestoreI ty %vd <= %rx
class MvRestoreI :
    public Instruction_<MvRestoreI, Op_MvRestore>
{
    public: MvRestoreI(Values* pVd, Register* pRx) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        AppendOperand(pRx);
    } // MvRestoreI

    public: MvRestoreI(Values* pVd, Pseudo* pRx) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        AppendOperand(pRx);
    } // MvRestoreI

    // [C]
    public: override Operand* Compute() const;
}; // MvRestoreI

// MvSaveI ty %rd <= %vx
class MvSaveI :
    public Instruction_<MvSaveI, Op_MvSave>
{
    public: MvSaveI(Register* pRd, Values* pVx) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pVx);
    } // MvSaveI

    public: MvSaveI(Pseudo* pRd, Values* pVx) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pVx);
    } // MvSaveI
}; // MvSaveI

// [N]

/// <summary>
///   NE instruction
/// </summary>
/// <example>
///     NE bool %bd &lt;= %sx %sy
/// </example>
class NeI : public Instruction_<NeI, Op_Ne>
{
    // ctor
    public: NeI(Bool* pBd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // NeI

    // [C]
    public: override Operand* Compute() const;
}; // NeI

// NonlocalI ty %rd <=
class NonlocalI :
    public Instruction_<NonlocalI, Op_Nonlocal>
{
    public: NonlocalI(const Type* pty, Output* pOd) :
        Base(GetOp_(), pty, pOd) {}
}; // NonlocalI

// NthValue ty %rd <= %sx %vy
class NthValueI :
    public Instruction_<NthValueI, Op_NthValue>
{
    public: NthValueI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx,
        Values*     const pVy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pVy);
    } // NthValueI
}; // NthValueI

// [O]

class OpenInstruction :
    public WithCastable_<OpenInstruction, Instruction>
{
    public: static const char* Kind_() { return "OpenInstruction"; }

    // [I]
    public: override bool IsUseless() const;

    protected: bool isNextClose() const
    {
        if (CloseI* pCloseI = GetNext()->DynamicCast<CloseI>())
        {
            return pCloseI->GetSx() == GetOutput();
        }
        return false;
    } // isNextClose

    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // OpenInstruction

class OpenExitPointInstruction :
    public WithCastable_<OpenExitPointInstruction, OpenInstruction>
{
    public: static const char* Kind_() { return "OpenExitPointInstruction"; }
}; // OpenExitPointInstruction

class OpenBlockInstruction :
    public WithCastable_<OpenBlockInstruction, OpenExitPointInstruction>
{
    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // OpenBlockInstruction

class BindOperandBox :
    public WithCastable_<BindOperandBox, OperandBox>
{
    public: static const char* Kind_() { return "BindOperandBox"; }

    private: Val m_cell;

    public: BindOperandBox(Val cell, Operand* pSx) :
        m_cell(cell)
        { init(pSx); }

    // [G]
    public: Val GetVarCell() const { return m_cell; }

    // [H]
    public: override void HtmlPrint(Val, bool = false) const;
}; // BindOperandBox

// OpenBind ty %fd <= name label
class OpenBindI :
    public Instruction_<OpenBindI, Op_OpenBind, OpenInstruction>
{
    // ctor
    public: OpenBindI(FrameReg* pFd) :
        Base(GetOp_(), tyT, pFd) {}

    // [A]
    public: BindOperandBox* AddBind(Val cell, Operand* pSx)
    {
        BindOperandBox* pBox = new BindOperandBox(cell, pSx);
        AppendOperand(pBox);
        return pBox;
    } // AddBind

    // [I]
    public: override bool IsUseless() const
        { return isNextClose(); }
}; // OpenBindI

// OpenBlockI ty %fd <= name label
class OpenBlockI :
    public Instruction_<OpenBlockI, Op_OpenBlock, OpenBlockInstruction>
{
    public: OpenBlockI(FrameReg* pFd, Val name, BBlock* pNonlocalXpBB) :
        Base(GetOp_(), tyT, pFd)
    {
        AppendOperand(new Literal(name));
        AppendOperand(pNonlocalXpBB->GetLabel());
    } // OpenBlockI
}; // OpenBlockI

// OpenCatchI ty %fd <= name label
class OpenCatchI :
    public Instruction_<OpenCatchI, Op_OpenCatch, OpenBlockInstruction>
{
    public: OpenCatchI(FrameReg* pFd, Operand* pSx, BBlock* pNonlocalXpBB) :
        Base(GetOp_(), tyT, pFd)
    {
        AppendOperand(pSx);
        AppendOperand(pNonlocalXpBB->GetLabel());
    } // OpenCatchI

    // [I]
    public: override bool IsUseless() const
        { return isNextClose(); }
}; // OpenCatchI

// OpenFinallyI ty %fd <= fn %vy
class OpenFinallyI :
    public Instruction_<OpenFinallyI, Op_OpenFinally, OpenInstruction>
{
    public: OpenFinallyI(FrameReg* pFd, Function* pFn, Values* pVy) :
        Base(GetOp_(), tyT, pFd)
    {
        AppendOperand(pFn);
        AppendOperand(pVy);
    } // OpenFinallyI

    // [I]
    public: override bool IsUseless() const
        { return isNextClose(); }
}; // OpenFinallyI

// OpenHandlerI ty %fd <= typespec %fn...
class OpenHandlerI :
    public Instruction_<OpenHandlerI, Op_OpenHandler, OpenInstruction>
{
    // ctor
    public: OpenHandlerI(FrameReg* pFd) :
        Base(GetOp_(), tyT, pFd) {}

    // [I]
    public: override bool IsUseless() const
        { return isNextClose(); }
}; // OpenHandlerI

// OpenTagsI ty %fd <=
class OpenTagsI :
    public Instruction_<OpenTagsI, Op_OpenTags, OpenExitPointInstruction>
{
    // ctor
    public: OpenTagsI(FrameReg* pFd) :
        Base(GetOp_(), tyT, pFd) {}

    // No TagDef for this OpenTags
    public: override bool IsUseless() const
    {
        if (isNextClose()) return true;
        return 0 == GetOutput()->StaticCast<FrameReg>()->m_nCount;
    } // IsUseless
}; // OpenTagsI

// OpenTryI ty %fd <=
class OpenTryI :
    public Instruction_<OpenTryI, Op_OpenTry, OpenExitPointInstruction>
{
    // ctor
    public: OpenTryI(FrameReg* pFd) :
        Base(GetOp_(), tyT, pFd) {}

    // No Catch for this OpenTry
    public: override bool IsUseless() const
    {
        if (isNextClose()) return true;
        return 0 == GetOutput()->StaticCast<FrameReg>()->m_nCount;
    } // IsUseless
}; // OpenTryI

// [P]
class ParseKeysI :
    public Instruction_<ParseKeysI, Op_ParseKeys>
{
    // ctor
    public: ParseKeysI(Values* pVd, Register* pRx, Val key) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        AppendOperand(pRx);
        AppendOperand(Literal::New(key));
    } // ParseKeys
}; // ParseKeysI

class PhiOperandBox :
    public WithCastable_<PhiOperandBox, OperandBox>
{
    public: static const char* Kind_() { return "PhiOperandBox"; }

    private: BBlock* m_pBB;

    // ctor
    public: PhiOperandBox(BBlock* pBB, Operand* pSx) :
        m_pBB(pBB) { init(pSx); }

    // [G]
    public: BBlock* GetBB()    const { return m_pBB; }
    public: BBlock* SetBB(BBlock* p) { return m_pBB = p; }

    // [H]
    public: override void HtmlPrint(Val s, bool = false) const
    {
        cformat(s, "(~S ~S)", GetBB(), GetOperand());
    } // HtmlPrint
}; // PhiOperandBox

// PhiI ty %rd <= (BBk %sx)+
class PhiI :
    public Instruction_<PhiI, Op_Phi>
{
    // ctor
    public: PhiI(const Type* pty, Output* pOut) :
        Base(GetOp_(), pty, pOut) {}

    // [A]
    public: Operand* AddOperand(
        BBlock*  const pBB,
        Operand* const pSx )
    {
        PhiOperandBox* const pBox = new PhiOperandBox(pBB, pSx);
        AppendOperand(pBox);
        return pSx;
    } // AddInput

    // [C]
    public: override Operand* Compute() const;

    // [F]
    public: PhiOperandBox* FindOperandBox(BBlock* const pBB) const
    {
        foreach (EnumOperand, oEnum, this)
        {
            PhiOperandBox* const pBox = oEnum.GetBox()->
                StaticCast<PhiOperandBox>();
            if (pBox->GetBB() == pBB)
            {
                return pBox;
            }
        } // for each box
        return NULL;
    } // GetOperandBox

    // [G]
    public: PhiOperandBox* GetOperandBox(BBlock* pBB) const
    {
        PhiOperandBox* const pBox = FindOperandBox(pBB);
        ASSERT(NULL != pBox);
        return pBox;
    } // GetOperandBox

    public: Operand* GetOperand(BBlock* pBB) const
        { return GetOperandBox(pBB)->GetOperand(); }

    public: Register* GetRx(BBlock* pBB) const
        { return GetOperand(pBB)->DynamicCast<Register>(); }

    // [O]
    public: override bool Optimize();

    // [V]
    private: override bool Verify() const;
}; // PhiI

// PrologueI label
class PrologueI :
    public Instruction_<PrologueI, Op_Prologue>
{
    // ctor
    public: PrologueI(Values* pVd) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        AppendOperand(Literal::New(t));
        AppendOperand(Literal::New(nil));
    } // PrologueI

    // [I]
    public: override bool IsUseless() const { return false; }
}; // PrologueI

// [R]

/// <summary>
///   RET instruction.
///   <para>
///     RET %sx
///   </para>
///   <list>
///     <item>
///       <term>%sx</term>
///       <description>
///         A value of containing function.
///       </description>
///     </item>
///   </list>
/// </summary>
class RetI : public Instruction_<RetI, Op_Ret, TerminateInstruction>
{
    // ctor
    public: RetI(Operand* pSx) :
        Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(pSx);
    } // RetI

    protected: RetI() {}

    // [O]
    public: override bool Optimize();
}; // RetI

// ReturnFromI %rx %vy
class ReturnFromI :
    public Instruction_<ReturnFromI, Op_ReturnFrom>
{
    // ctor
    public: ReturnFromI(Register* pRx, Values* pVy) :
        Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(pRx);
        AppendOperand(pVy);
    } // ReturnFromI
}; // ReturnFromI

/// <summary>
///   RUNTIMECAST instruction.
///   <para>
///     RUNTIMECAST ty %rd &lt;= %rx
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of RUNTIMECAST operation.
///       </description>
///     </item>
///     <item>
///       <term>%rx</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class RuntimeCastI :
    public Instruction_<RuntimeCastI, Op_RuntimeCast, CastInstruction>
{
    // ctor
    public: RuntimeCastI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
    } // RuntimeCastI
}; // RuntimeCast

// [S]

/// <summary>
///   <example>
///     SELECT ty %rd &lt;= var %sy
///   </example>
/// </summary>
class SelectI :
    public Instruction_<SelectI, Op_Select>
{
    public: SelectI(const Type* pty, Register* pRd, Values* pVx, Int iNth) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pVx);
        AppendOperand(Integer::New(iNth));
    } // SelectI

    public: override Operand* Compute() const;
    public: override bool Optimize();
}; // SelectI

/// <summary>
///   <example>
///     SHL ty %rd &lt;= %sx %sy
///   </example>
/// </summary>
class ShlI :
    public Instruction_<ShlI, Op_Shl>
{
    public: ShlI(const Type* pty, Register* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // ShlI
}; // ShlI

/// <summary>
///     Represents SHOULDBE instruction for describing function argument
///     type in IR.
///   <example>
///     SHOULDBE ty %rd &lt;= %sx
///   </example>
/// </summary>
class ShouldBeI :
    public Instruction_<ShouldBeI, Op_ShouldBe, CastInstruction>
{
    public: ShouldBeI(const Type* pty, Register* pRd, Operand* pSx) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
    } // ShouldBeI
}; // ShouldBeI

/// <summary>
///   <example>
///     SHR ty %rd &lt;= %sx %sy
///   </example>
/// </summary>
class ShrI :
    public Instruction_<ShrI, Op_Shr>
{
    public: ShrI(const Type* pty, Register* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // ShrI
}; // ShrI

// SLOT ty %rd <= class slot %sz
class SlotI :
    public Instruction_<SlotI, Op_Slot>
{
    // ctor
    public: SlotI(
        Type*       pty,
        Output*     pRd,
        Val         klass,
        Val         slot,
        Operand*    pSz) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(Literal::New(klass));
        AppendOperand(Literal::New(slot));
        AppendOperand(pSz);
    } // SlotI
}; // SlotI

/// <summary>
///   STATICCAST instruction.
///   <para>
///     STATICCAST ty %rd &lt;= %rx
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of STATICCAST operation.
///       </description>
///     </item>
///     <item>
///       <term>%rx</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class StaticCastI :
    public Instruction_<StaticCastI, Op_StaticCast, CastInstruction>
{
    public: StaticCastI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
    } // StaticCastI
}; // StaticCast

// STORE %rx %sy
class StoreI :
    public Instruction_<StoreI, Op_Store>
{
    public: StoreI(
        Output*     pRx,
        Operand*    pSy ) :
            Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(pRx);
        AppendOperand(pSy);
    } // StoreI
}; // StoreI

/// <summary>
///   SUB instruction.
///   <para>
///     SUB ty %rd &lt;= %sx %sy
///   </para>
///   <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A GPR contains result of SUB operation.
///       </description>
///     </item>
///     <item>
///       <term>%sx</term>
///       <description>An operand</description>
///     </item>
///     <item>
///       <term>%sy</term>
///       <description>An operand</description>
///     </item>
///  </list>
/// </summary>
class SubI :
    public Instruction_<SubI, Op_Sub, ArithmeticInstruction>
{
    public: SubI(Output* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyT, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // SubI

    public: SubI(
        const Type* const pty,
        Register*   const pRd,
        Operand*    const pSx,
        Operand*    const pSy ) :
            Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // AndI
}; // SubI

// [T]
/// <summary>
///   TAGDEF instruction. Make tag object for nonlocal go.
///   <example>
///     TAGDEF %rd &lt;= %fx $label tag
///   </example>
/// </summary>
/// <seealso cref="OpenTagsI" />
class TagDefI :
    public Instruction_<TagDefI, Op_TagDef>
{
    public: TagDefI(
        Register*   const pRd,
        FrameReg*   const pFx,
        BBlock*     const pBB,
        Val         const tag ) :
            Base(GetOp_(), tyPtrT, pRd)
    {
        AppendOperand(pFx);
        AppendOperand(pBB->GetLabel());
        AppendOperand(Literal::New(tag));
    } // TagDefI

    // [R]
    public: override void Realize();

    // [U]
    public: override void Unrealize();
}; // TagDefI

/// <summary>
///  THROW instruction. Parse emits this instruction for special-form
///  (throw tag-form values-form)
///  <para>
///     THROW %rx %vy
///  </para>
///  <list>
///     <item>
///       <term>%rx</term>
///       <description>A register contains catch tag.</description>
///     </item>
///     <item>
///       <term>%vy</term>
///       <description>A values to throw.</description>
///     </item>
///  </list>
/// </summary>
class ThrowI :
    public Instruction_<ThrowI, Op_Throw>
{
    // ctor
    public: ThrowI(Operand* pSx, Values* pVy) :
        Base(GetOp_(), tyVoid, Void)
    {
        AppendOperand(pSx);
        AppendOperand(pVy);
    } // ThrowI
}; // ThrowI

/// <summary>
///  TLV instruction
///  <para>
///     TLV (ptr ty) %rd &lt;= tlvrec
///  </para>
///  <list>
///     <item>
///       <term>%rd</term>
///       <description>
///         A register contains point to TLV value slot.
///       </description>
///     </item>
///     <item>
///       <term>tlvrec</term>
///       <description>A tlv-record object</description>
///     </item>
///  </list>
/// </summary>
class TlvI :
    public Instruction_<TlvI, Op_Tlv>
{
    public: TlvI(Output* pRd, Val tlvrec) :
        Base(GetOp_(), tyPtrT, pRd)
    {
        AppendOperand(Literal::New(tlvrec));
    } // TlvI
}; // TlvI

/// <summary>
///   TYPEP instruction. Parser emits this instruction for function typep.
///   <para>
///     TYPEP bool %bd &lt;= %sx typespec
///   </para>
///   <list>
///     <item>
///       <term>%bd</term>
///       <description>
///         A bool register contains result of type predicate.
///       </description>
///     </item>
///     <item>
///       <term>typespec</term>
///       <description>A type-specifier</description>
///     </item>
///  </list>
/// </summary>
class TypepI :
    public Instruction_<TypepI, Op_Typep>
{
    // ctor
    public: TypepI(Bool* pBd, Register* pSx, const Type* pty) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(const_cast<Type*>(pty));
    } // TypepI

    // [C]
    public: override Operand* Compute() const;
}; // TypepI

// [U]

/// <summary>
///   Pseudo instruction UNBOX.
///   <example>
///     UNBOX %rx
///   </example>
/// </summary>
class UnBoxI :
    public Instruction_<UnBoxI, Op_UnBox>
{
    public: UnBoxI(const Type* pty, Output* pRd, Operand* pSx) :
        Base(GetOp_(), pty, pRd)
        { AppendOperand(pSx); }
}; // UnBoxI

/// <summary>
///   Pseudo instruction UNREACHABLE.
///   <example>
///     UNREACHABLE %rx
///   </example>
/// </summary>
class UnreachableI :
    public Instruction_<UnreachableI, Op_Unreachable, TerminateInstruction>
{
    public: UnreachableI() :
        Base(GetOp_(), tyVoid, Void) {}
}; // UnreachableI

class UpVarBaseI :
    public Instruction_<UpVarBaseI, Op_UpVarBase>
{
    public: UpVarBaseI(Register* pRd, Function* pOwner) :
            Base(GetOp_(), tyPtrT, pRd)
    {
        AppendOperand(pOwner);
    } // UpVarBaseI
}; // UpVarBaseI

class UpVarDefI :
    public Instruction_<UpVarDefI, Op_UpVarDef>
{
    public: UpVarDefI(Pseudo* pQd, Variable* pVar) :
            Base(GetOp_(), tyClosedCell, pQd)
    {
        AppendOperand(pVar);
    } // UpVarDefI

    public: override bool IsUseless() const;
}; // UpVarDefI

/// <summary>
///   Pseudo instruction USE.
///   <example>
///     USE %rx
///   </example>
/// </summary>
class UseI :
    public Instruction_<UseI, Op_Use>
{
    public: UseI(Output* pRx) :
        Base(GetOp_(), tyVoid, Void)
        { AppendOperand(pRx); }

    public: override bool IsUseless() const;
}; // UseI

// [V]

// VALUES ty %vd <= %sx*
class ValuesI :
    public Instruction_<ValuesI, Op_Values>
{
    // ctor
    public: ValuesI(Values* pVd) :
        Base(GetOp_(), tyValuesRestT, pVd) {}

    public: ValuesI(const Type* pty, Values* pVd) :
        Base(GetOp_(), pty, pVd) {}

    public: ValuesI(Values* pVd, Operand* pSx) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        ASSERT(! pSx->Is<Values>());
        AppendOperand(pSx);
    } // ValuesI

    public: ValuesI(const Type* pty, Values* pVd, Operand* pSx) :
        Base(GetOp_(), pty, pVd)
    {
        ASSERT(! pSx->Is<Values>());
        AppendOperand(pSx);
    } // ValuesI

    public: ValuesI(Values* pVd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        ASSERT(! pSx->Is<Values>());
        ASSERT(! pSy->Is<Values>());
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // ValuesI

    public: override bool Optimize();

    private: bool UpdateTy();
}; // ValuesI

// VALUES* ty %vd <= %sx*
class ValuesAI :
    public Instruction_<ValuesAI, Op_ValuesA>
{
    // ctor
    public: ValuesAI(Values* pVd) :
        Base(GetOp_(), tyValuesRestT, pVd) {}

    public: ValuesAI(Values* pVd, Operand* pSx) :
        Base(GetOp_(), tyValuesRestT, pVd)
    {
        AppendOperand(pSx);
    } // ValuesAI

    // [O]
    public: override bool Optimize();

    private: bool UpdateTy();
}; // ValuesAI

// VARDEF ty %rd <= var %sy
class VarDefI :
    public Instruction_<VarDefI, Op_VarDef>
{
    public: VarDefI(Register* pRd, Variable* pVar, Operand* pSy) :
        Base(GetOp_(), tyClosedCell, pRd)
    {
        pRd->SetVar(pVar);
        pVar->SetDefI(this);
        AppendOperand(pVar);
        AppendOperand(pSy);
    } // VarDefI

    // [I]
    public: override bool IsUseless() const;
}; // VarDefI

} // Compiler

} // TinyCl

#endif //!defined(INCLUDE_tinycl_c_insn_h)
