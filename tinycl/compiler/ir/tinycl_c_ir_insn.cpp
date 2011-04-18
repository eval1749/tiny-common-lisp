#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler
// tinycl_c_ir_insn.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_insn.cpp#28 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

class Static
{
    public: static ValuesI* GetCallArgs(
        Instruction* const pI,
        Val          const fname )
    {
        CallI* const pCallI = pI->DynamicCast<CallI>();
        if (NULL == pCallI)
        {
            return NULL;
        }

        FunName* pFunName = pCallI->GetSx()->DynamicCast<FunName>();
        if (NULL == pFunName)
        {
            return NULL;
        }

        if (pFunName->GetName() != fname)
        {
            return NULL;
        }

        Values* pVy = pCallI->GetVy();
        if (NULL == pVy)
        {
            return NULL;
        }

        return pVy->GetDefI()->DynamicCast<ValuesI>();
    } // GetCallArg
}; // Static

static bool isEqNil(Instruction* const pI)
{
    if (NULL == pI)
    {
        return false;
    }

    if (! pI->Is<EqI>())
    {
        return false;
    }

    if (Literal* const pLy = pI->GetSy()->DynamicCast<Literal>())
    {
        return pLy->GetDatum() == nil;
    }
    return false;
} // isEqNil

static bool isEqNil(Bool* const pBx)
{
    ASSERT(NULL != pBx);
    return isEqNil(pBx->GetDefI());
} // isEqNil

static bool isIfTrueFalse(Instruction* const pI)
{
    if (NULL == pI)
    {
        return false;
    }

    if (! pI->Is<IfI>())
    {
        return false;
    }

    Operand* const pSy = pI->GetSy();
    Operand* const pSz = pI->GetSz();
    return pSy->IsTrue() && ! pSy->IsFalse() &&
           ! pSz->IsTrue() && pSz->IsFalse();
} // isIfTrueFalse

static bool isIfTrueFalse(Register* const pRx)
{
    if (NULL == pRx)
    {
        return false;
    }

    return isIfTrueFalse(pRx->GetDefI());
} // isIfTrueFalse

static bool isNeNil(Instruction* const pI)
{
    if (NULL == pI)
    {
        return false;
    }

    if (! pI->Is<NeI>())
    {
        return false;
    }

    if (Literal* const pLy = pI->GetSy()->DynamicCast<Literal>())
    {
        return pLy->GetDatum() == nil;
    }
    return false;
} // isNeNil

#if 0
static bool isNeNil(Bool* const pBx)
{
    ASSERT(NULL != pBx);
    return isNeNil(pBx->GetDefI());
} // isNeNil
#endif

// FIXME 2008-08-17 yosi@msn.com How do we prevent multiple
// error of CastInstruction?
static void reportIncompatibleType(
    const Instruction*  const pI,
    Operand*            const pSx,
    const Type*         const ptyExpected )
{
    CLOG(1, "<li><b class='r'>Type error</b> ~S operand=~S expected=~S",
        pI, pSx, ptyExpected );

    if (Literal* const pLx = pSx->DynamicCast<Literal>())
    {
        Context::Get()->Error("~S isn't type of ~S.",
            pLx->GetDatum(),
            ptyExpected->Unparse() );
    }
    else
    {
        Context::Get()->Error("Type ~S isn't subtype of ~S.",
            pSx->GetTy()->Unparse(),
            ptyExpected->Unparse() );
    }
} // reportIncompatibleType

// [B]
Operand* BoxI::Compute() const
{
    if (Register* const pRx = GetRx())
    {
        const Instruction* const pDefI = pRx->GetDefI();
        if (pDefI->Is<UnBoxI>())
        {
            return pDefI->GetSx();
        }
    } // if rx

    return NULL;
} // BoxI::Compute

void BranchI::OnMove(BBlock* const pNewBB)
{
    CLOG(3, "<ol>~%");
    GetTrueBB()->RedirectEdgeFrom(pNewBB, GetBB());
    GetFalseBB()->RedirectEdgeFrom(pNewBB, GetBB());
    CLOG(3, "</ol>~%");
} // BranchI::OnMove

/// <summary>
///   Normalize Not-Branch to Branch. This normalization leads another
///   optimization for IF+EQ.
///   <example>
///         EQ %b1 &lt;= %r2 NIL
///         BRANCH %b1 BB3 BB4
///       =>
///         NE %b4 &lt;= %r2 NIL
///         BRANCH %b4 BB4 BB3
///   </example>
/// </summary>
bool BranchI::Optimize()
{
    Instruction* const pDefI = GetBx()->GetDefI();
    if (NULL == pDefI)
    {
        // Constant branch
        return false;
    }

    if (EqI* const pEqI = pDefI->DynamicCast<EqI>())
    {
        if (Literal* const pLy = pEqI->GetSy()->DynamicCast<Literal>())
        {
            if (pLy->GetDatum() == nil)
            {
                CLOG(2, "<li>Normalize Branch<ol>");
                CLOG(2, "<li>~S</li>", this);
                CLOG(2, "<li>~S</li>", pEqI);

                Bool* const pBx = new Bool;

                Instruction* const pNeI = new NeI(pBx, pEqI->GetSx(), pLy);
                GetBB()->InsertBeforeI(pNeI, this);

                // Swap true/false labels
                Operand* const pSy = GetSy();
                Operand* const pSz = GetSz();
                GetOperandBox(1)->SetOperand(pSz);
                GetOperandBox(2)->SetOperand(pSy);

                GetOperandBox(0)->Replace(pBx);

                CLOG(2, "</ol></li>");
                return true;
            }
        } // if nil
    } // if eq

    return false;
} // BranchI::Optimize

void BranchI::Realize()
{
    Instruction::Realize();
    GetBB()->AddEdge(GetTrueBB());
    GetBB()->AddEdge(GetFalseBB());
} // BranchI::Realize

void BranchI::Unrealize()
{
    GetBB()->RemoveOutEdge(GetTrueBB());
    GetBB()->RemoveOutEdge(GetFalseBB());
    Instruction::Unrealize();
} // BranchI::Unrealize

// [C]
// See tinycl_c_ir_fundb.cpp for CallI::Optimize

/// <summary>
///   Compute output of cast instruction familiy, RuntimeCast and StaticCast.
///   We detect following cases:
///   <list>
///     <item>
///       <term>Source operand is already expected type</term>
///       <description>
///         In this case, we can use source operand as output.
///       </description>
///     </item>
///     <item>
///       <term>Source operand can't be expected type</term>
///       <description>
///         We signal compilation error.
///       </description>
///     </item>
///   </list>
/// </summary>
Operand* CastInstruction::Compute() const
{
    if (Literal* pLx = GetSx()->DynamicCast<Literal>())
    {
        switch (GetTy()->IsType(pLx->GetDatum()))
        {
        case Subtype::No:
            reportIncompatibleType(this, pLx, GetTy());
            return NULL;

        case Subtype::Yes:
            return pLx;
        } // switch
    }
    else
    {
        switch (Type::IsSubtype(GetSx()->GetTy(), GetTy()))
        {
        case Subtype::No:
            if (Subtype::No == Type::IsSubtype(GetTy(), GetSx()->GetTy()))
            {
                reportIncompatibleType(this, GetSx(), GetTy());
            }
            return NULL;

        case Subtype::Yes:
            return GetSx();
        } // switch
    } // if

    // FIXME 2008-08-17 yosi@msn.com We should support local function
    // reference in CastInstruction::Compute.
    return NULL;
} // CastInstruction::Compute

bool CastInstruction::Optimize()
{
    const Type* const pty = Type::And(GetTy(), GetSx()->GetTy());
    if (tyNil == pty)
    {
        reportIncompatibleType(this, GetSx(), GetTy());
        return false;
    }

    if (GetTy()->Equal(pty))
    {
        return false;
    }

    CLOG(1, "<li>update ~S to ~S</li>", this, pty);
    SetTy(pty);
    return true;
} // CastInstruction::Optimize

/// <summary>
///   Returns true if this CATCH instruction is uselss. CATCH instruction is
///   useless if there is no instruction between CATCH and CLOSE instruction.
/// </summary>
bool CatchI::IsUseless() const
{
    if (JumpI* pJumpI = GetNext()->DynamicCast<JumpI>())
    {
        BBlock* pBB = pJumpI->GetTargetBB();
        return pBB->GetFirstI()->Is<CloseI>();
    } // if jump
    return false;
} // CatchI::IsUseless

void CatchI::Realize()
{
    Instruction::Realize();
    FrameReg* pFd = GetSx()->StaticCast<FrameReg>();
    pFd->m_nCount += 1;

    BBlock* pCatchBB = GetSy()->StaticCast<Label>()->GetBB();
    GetBB()->AddEdge(pCatchBB)->SetKind(CfgEdge::Kind_Nonlocal);
} // CatchI::Realize

void CatchI::Unrealize()
{
    FrameReg* pFd = GetSx()->StaticCast<FrameReg>();
    ASSERT(pFd->m_nCount >= 1);
    pFd->m_nCount -= 1;

    BBlock* pCatchBB = GetSy()->StaticCast<Label>()->GetBB();
    GetBB()->RemoveOutEdge(pCatchBB);

    Instruction::Unrealize();
} // CatchI::Unrealize

Operand* CountI::Compute() const
{
    if (ValuesI* const pValuesI = GetVx()->GetDefI()->DynamicCast<ValuesI>())
    {
        return Literal::New(Fixnum::Encode(pValuesI->CountOperands()));
    }

    if (TyValues* const pty = GetVx()->GetTy()->DynamicCast<TyValues>())
    {
        TyValues::Arity const oArity = pty->ComputeArity();
        if (oArity.IsFixed())
        {
            return Literal::New(Fixnum::Encode(oArity.m_iMin));
        }
    }

    return NULL;
} // CountI::Compute

Operand* ConvertI::Compute() const
{
    Operand*    const pSx = GetSx();
    const Type* const pTx = pSx->GetTy();

    if (GetTy() == pTx)
    {
        return pSx;
    }

    return NULL;
} // ConvertI::Compute

// [G]
Operand* GtI::Compute() const
{
    if (Literal* const pLx = GetSx()->DynamicCast<Literal>())
    {
        if (Literal* const pLy = GetSy()->DynamicCast<Literal>())
        {
            Val const rd = funcall(QGS2, pLx->GetDatum(), pLy->GetDatum());
            return nil != rd ? Bool_True : Bool_False;
        }
    } // if

    return NULL;
} // GtI::Compute

// [I]
Operand* IfI::Compute() const
{
    if (GetBx() == Bool_False)
    {
        return GetSz();
    }

    if (GetBx() == Bool_True)
    {
        return GetSy();
    }

    return NULL;
} // IfI::Compute

/// <summary>
///   Returns true if this If instruction is true/false form.
/// </summary>
bool IfI::IsTrueFalse() const
{
    if (! (! GetSz()->IsTrue() && GetSz()->IsFalse()) )
    {
        return false;
    }

    if (GetSy()->IsTrue() && ! GetSy()->IsFalse())
    {
        return true;
    }

    // 0007  Typep BOOL %b9 <= %r3 CONS
    // 0008  If T %r10 <= %b9 %r3 'NIL
    // 0009  Ne BOOL %b11 <= %r10 'NIL
    if (TypepI* const pTypepI = GetBx()->GetDefI()->DynamicCast<TypepI>())
    {
        if (GetSy() == pTypepI->GetSx() &&
            Subtype::No == Type::IsSubtype(
                    pTypepI->GetSy()->StaticCast<Type>(),
                    tyNull ) )
        {
            return true;
        }
    } // if TypepI

    return false;
} // IfI::IsTrueFalse

/// <summary>
///   Optimize IF instruction.
/// </summary>
bool IfI::Optimize()
{
    Bool* const pBx = GetBx();
    Instruction* const pDefI = pBx->GetDefI();
    if (NULL == pDefI)
    {
        return false;
    }

    // (defun foo (x) (not (not x)))
    //  Eq BOOL %b9 <= %r3 'NIL
    //  If T %r10 <= %b9 'T 'NIL
    //  Eq BOOL %b11 <= %r10 'NIL
    //  If T %r12 <= %b11 'T 'NIL
    // ==>
    //  Ne BOOL %b100 <= %r3 'NIL
    // If T %r12 <= %b100 'T 'NIL
    if (isIfTrueFalse(this) &&
        isEqNil(pBx) &&
        isIfTrueFalse(pDefI->GetRx()) &&
        isEqNil(pDefI->GetRx()->GetDefI()->GetBx()) )
    {
        Instruction* const pEqI = pDefI->GetRx()->GetDefI()->
            GetBx()->GetDefI();

        CLOG(2, "<li>Optimize not-not sequence: ~S</li>~%", this);

        Bool* const pB2 = new Bool;
        GetBBlock()->InsertBeforeI(
            new NeI(pB2, pEqI->GetSx(), pEqI->GetSy()),
            this );
        GetOperandBox(0)->Replace(pB2);
        return true;
    } // if

    // (defun foo (x) (typep x 'symbol))
    //  Phi T %r17 <= (BB6 'T) (BB11 'NIL) (BB12 'NIL) (BB13 True)
    //  Ne BOOL %b9 <= %r17 'NIL
    //  If T %r10 <= %b9 'T 'NIL
    if (isNeNil(pDefI) &&
        pDefI->GetRx()->GetDefI()->Is<PhiI>() )
    {
        PhiI* const pPhiI = pDefI->GetRx()->
            GetDefI()->StaticCast<PhiI>();

        bool fAllLits = true;

        foreach (PhiI::EnumOperand, oEnum, pPhiI)
        {
            if (oEnum.Get()->Is<Literal>())
            {
                // ok
            }
            else if (oEnum.Get() == True)
            {
                // ok
            }
            else
            {
                fAllLits = false;
                break;
            }
        } // for

        if (fAllLits)
        {
            PhiI* const pNewPhiI = new PhiI(GetTy(), GetRd());

            GetBBlock()->InsertBeforeI(pNewPhiI, GetBBlock()->GetFirstI());
            SetOutput(new Register);

            foreach (PhiI::EnumOperand, oEnum, pPhiI)
            {
                PhiOperandBox* const pBox = oEnum.GetBox()->
                    StaticCast<PhiOperandBox>();

                Operand* pSx = GetSz();

                if (pBox->GetOperand() == True)
                {
                    pSx = GetSy();
                }
                else if (pBox->GetOperand()->StaticCast<Literal>()->
                                GetDatum() != nil )
                {
                    pSx = GetSy();
                }

                pNewPhiI->AddOperand(pBox->GetBB(), pSx);
            } // for

            return true;
        } // if AllLits
    } // if

    // Ne BOOL %b138 <= %r21 NIL
    // If T %r139 <= %b138 %r21 %r129
    if (isNeNil(GetBx()->GetDefI()))
    {
        const Type* const ptyY = Type::Diff(GetSy()->GetTy(), tyNull);
        const Type* const pty  = Type::Or(ptyY, GetSz()->GetTy());
        if (! GetTy()->Equal(pty))
        {
            SetTy(pty);
            return true;
        }
    }

    return false;
} // IfI::Optimize

/// <summary>
///   Returns true if this instruction dominates specified instruction.
///   <para>
///     You must call Function::NumberInstruction before calling this method.
///   </para>
/// </summary>
bool Instruction::DoesDominate(const Instruction* const that) const
{
    if (this->GetIndex() >= that->GetIndex())
    {
        return false;
    }

    if (! this->GetBB()->DoesDominate(that->GetBB()))
    {
        return false;
    }

    return true;
} // Instruction::DoesDoeminate

bool Instruction::Equal(const Instruction* const that) const
{
    if (this->GetOp() != that->GetOp())
    {
        return false;
    }

    if (! this->GetOutput()->Equal(that->GetOutput()))
    {
        return false;
    }

    Instruction::EnumOperand oEnumThat(that);
    foreach (Instruction::EnumOperand, oEnumThis, this)
    {
        if (oEnumThat.AtEnd())
        {
            return false;
        }

        if (! oEnumThis.Get()->Equal(oEnumThat.Get()))
        {
            return false;
        }

        oEnumThat.Next();
    } // for

    return true;
} // Instruction::Equal

OperandBox* Instruction::GetOperandBox(int iNth) const
{
    foreach (EnumOperand, oEnum, this)
    {
        if (0 == iNth)
        {
            return oEnum.GetBox();
        }

        iNth -= 1;
    } // for each operand

    return NULL;
} // Instruction::GetOperandBox

Operand* Instruction::GetOperand(int iNth) const
    { return GetOperandBox(iNth)->GetOperand(); }

void Instruction::HtmlPrint(Val const stream, bool const fDef) const
{
    class Local
    {
        public: static void WriteAttr(
            const Instruction* const pI,
            Val                const stream )
        {
            if (const CallI* const p = pI->DynamicCast<CallI>())
            {
                if (p->IsNotinline()) write_string(".n", stream);
            }
            else if (const ClosureI* const p = pI->DynamicCast<ClosureI>())
            {
                if (p->IsNotinline()) write_string(".n", stream);
            }
            else if (const LoadFunI* const p = pI->DynamicCast<LoadFunI>())
            {
                if (p->IsNotinline()) write_string(".n", stream);
            }
        } // WriteAttr
    }; // Local

    if (fDef)
    {
        format(stream, "<td>~4,'0D</td><td width='10'/>",
            Fixnum::Encode(GetIndex()) );

        write_string("<td>", stream);
        write_string("<a class='m'>", stream);
        write_string(GetMnemonic(), stream);
        Local::WriteAttr(this, stream);
        write_string("</a>", stream);
        write_string("</td>", stream);

        if (m_pOutput == Void)
        {
            write_string("<td/><td/><td/>", stream);
        }
        else
        {
            write_string("<td>", stream);
                m_pty->HtmlPrint(stream, false);
            write_string("</td>", stream);

            write_string("<td>", stream);
                m_pOutput->HtmlPrint(stream, true);
            write_string("</td>", stream);

            write_string("<td>&lt;=</td>", stream);
        }

        // Source operands
        write_string("<td>", stream);

        foreach (EnumOperand, oEnum, this)
        {
            write_char(Space, stream);
            oEnum.GetBox()->HtmlPrint(stream, true);
        } // for each operand

        write_string("</td>", stream);

        if (Register* const pRd = GetRd())
        {
            if (Variable* const pVar = pRd->GetVar())
            {
                write_string("<td>; <b>", stream);
                    pVar->HtmlPrint(stream, false);
                write_string("</b></td>", stream);
            }
        } // if
    }
    else
    {
        write_char('(', stream);

        if (NULL != GetBB())
        {
            GetBB()->HtmlPrint(stream);
            write_char(' ', stream);
        }

        write_string("<a class='m'>", stream);
        write_string(GetMnemonic(), stream);
        Local::WriteAttr(this, stream);
        write_string("</a>", stream);

        if (m_pOutput != Void)
        {
            write_string(" <i class='t'>", stream);
            m_pty->HtmlPrint(stream, false);
            write_string("</i> ", stream);

            m_pOutput->HtmlPrint(stream, fDef);
            write_string(L" &lt;=", stream);
        }

        foreach (EnumOperand, oEnum, this)
        {
            write_char(Space, stream);
            oEnum.GetBox()->HtmlPrint(stream);
        } // for each operand

        write_char(')', stream);
    }
} // Instruction::HtmlPrint

bool Instruction::IsUseless() const
{
    if (GetOutput() == Void)
    {
        return false;
    }

    if (SsaOutput* pRd = GetSsaOutput())
    {
        return pRd->IsEmpty();
    }

    // For non-SSA output, We don't know whether this instruction is
    // useless or not.
    return false;
} // Instruction::IsUseless

void Instruction::Realize()
{
    m_pOutput->SetDefI(this);

    foreach (EnumOperand, oEnum, this)
    {
        OperandBox* const pBox = oEnum.GetBox();
        pBox->GetOperand()->Realize(pBox);
    } // for each box
} // Instruction::Realize

void Instruction::RemoveOperand(OperandBox* pBox)
{
    ASSERT(NULL != pBox);
    pBox->GetOperand()->Unrealize(pBox);
    static_cast<OperandBoxes*>(this)->Delete(pBox);
} // Instruction::RemoveOperand

Output* Instruction::SetOutput(Output* pOd)
{
    ASSERT(NULL != pOd);

    m_pOutput = pOd;
    if (IsRealized())
    {
        pOd->SetDefI(this);
    }
    return pOd;
} // Instruction::SetOutput

void Instruction::Unrealize()
{
    foreach (EnumOperand, oEnum, this)
    {
        OperandBox* pBox = oEnum.GetBox();
        pBox->GetOperand()->Unrealize(pBox);
    } // for each box

    if (m_pOutput->GetDefI() == this)
    {
        m_pOutput->SetDefI(NULL);
    }
} // Instruction::Unrealize

// [J]

void JumpI::OnMove(BBlock* const pNewBB)
{
    GetTargetBB()->RedirectEdgeFrom(pNewBB, GetBB());
} // JumpI::OnMove

void JumpI::Realize()
{
    Instruction::Realize();
    GetBB()->AddEdge(GetLabel()->GetBB());
} // JumpI::Realize

void JumpI::Unrealize()
{
    BBlock* const pTargetBB = GetLabel()->GetBB();
    GetBB()->RemoveOutEdge(pTargetBB);

    foreach (BBlock::EnumI, oEnum, pTargetBB)
    {
        PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
        if (NULL == pPhiI)
        {
            break;
        }

        if (OperandBox* const pBox =
                    pPhiI->FindOperandBox(GetBB()) )
        {
            pPhiI->RemoveOperand(pBox);
        }
    } // for

    Instruction::Unrealize();
} // JumpI::Unrealize

// [M]
Operand* MvRestoreI::Compute() const
{
    Register* const pRx = GetRx();
    if (NULL == pRx)
    {
        return NULL;
    }

    MvSaveI* const pMvSaveI = GetRx()->GetDefI()->DynamicCast<MvSaveI>();
    if (NULL == pMvSaveI)
    {
        // Invalid pairing of MvSave/MvRestore.
        return NULL;
    }

    if (pMvSaveI == GetPrev())
    {
        // There is nothing between MvSave and MvRestore
        return pMvSaveI->GetVx();
    }

    if (ValuesI* pValuesI =
            pMvSaveI->GetVx()->GetDefI()->DynamicCast<ValuesI>() )
    {
        return pValuesI->GetVd();
    }

    return NULL;
} // MvRestoreI::Compute

// [N]
/// <summary>
///  Compute output of NE instruction.
/// </summary>
Operand* NeI::Compute() const
{
    // (defun foo (x) (not x))
    // If %r1 <= %b2 't 'nil
    // Ne %b3 <= %r1 'nil
    Literal* const pLy = GetSy()->DynamicCast<Literal>();
    if (NULL == pLy)
    {
        return NULL;
    }

    if (Literal* const pLx = GetSx()->DynamicCast<Literal>())
    {
        CLOG(2, "<li>Fold constant: ~S</li>", this);
        return pLx->GetDatum() != pLy->GetDatum() ? Bool_True : Bool_False;
    }

    if (nil != pLy->GetDatum())
    {
        return NULL;
    }

    Register* const pRx = GetSx()->DynamicCast<Register>();
    if (NULL == pRx)
    {
        return NULL;
    }

    IfI* const pIfI = pRx->GetDefI()->DynamicCast<IfI>();
    if (NULL == pIfI)
    {
        return NULL;
    }

    if (pIfI->IsTrueFalse())
    {
        CLOG(2, "<li>Simplify ~S+~S => ~S</li>",
            this, pIfI, pIfI->GetBx() );

        return pIfI->GetBx();
    }

    return NULL;
} // NeI::Compute

// [O]
void BindOperandBox::HtmlPrint(Val s, bool) const
{
    if (value_cell_p(m_cell))
    {
        cformat(s, "(#&lt;value-cell ~W> ~S)",
            m_cell->StaticCast<ValueCell>()->m_name,
            GetOperand() );
    }
    else if (tlv_record_p(m_cell))
    {
        cformat(s, "(#&lt;tlv ~W> ~S)",
            m_cell->StaticCast<TlvRecord>()->m_name,
            GetOperand() );
    }
    else
    {
        cformat(s, "<b color='style:red'>Bad Operand</b>");
    }
} // BindOperandBox::HtmlPrint

void OpenBlockInstruction::Realize()
{
    OpenInstruction::Realize();

    BBlock* pNonlocalXpBB = GetSy()->StaticCast<Label>()->GetBB();
    GetBB()->AddEdge(pNonlocalXpBB)->SetKind(CfgEdge::Kind_Nonlocal);
} // OpenBlockInstruction::Realize

void OpenBlockInstruction::Unrealize()
{
    BBlock* pNonlocalXpBB = GetSy()->StaticCast<Label>()->GetBB();
    GetBB()->RemoveOutEdge(pNonlocalXpBB);

    OpenInstruction::Unrealize();
} // OpenBlockInstruction::Unrealize

bool OpenInstruction::IsUseless() const
{
    if (isNextClose())
    {
        // No instruction between Open and Close.
        return true;
    }

    foreach (FrameReg::EnumUser, oEnum, GetOutput()->StaticCast<FrameReg>())
    {
        Instruction* const pI = oEnum.GetI();
        if (pI->Is<CloseI>())
        {
            continue;
        }

        if (pI->Is<UseI>())
        {
            continue;
        }

        return false;
    } // for each use
    return true;
} // OpenInstruction::IsUseless

void OpenInstruction::Realize()
{
    Instruction::Realize();

    GetBB()->GetFunction()->AddFrameReg(
        GetOutput()->StaticCast<FrameReg>() );
} // OpenInstruction::Realize

void OpenInstruction::Unrealize()
{
    GetBB()->GetFunction()->RemoveFrameReg(
        GetOutput()->StaticCast<FrameReg>() );

    Instruction::Unrealize();
} // OpenInstruction::Unrealize

// [P]
/// <summary>
///   Compute value of Phi instruction.
/// </summary>
Operand* PhiI::Compute() const
{
    Operand* pSx = NULL;
    foreach (EnumOperand, oEnum, this)
    {
        if (NULL == pSx)
        {
            pSx = oEnum.Get();
        }
        else if (! pSx->Equal(oEnum.Get()))
        {
            return NULL;
        }
    } // for

    return pSx;
} // PhiI::Compute

/// <summary>
///   Optimize Phi instruction.
///   <list>
///     <item><term>Update type</term></item>
///   </list>
/// </summary>
bool PhiI::Optimize()
{
    class Local
    {
        public: static bool Decompose(PhiI* const pPhiI)
        {
            Values* const pVd = pPhiI->GetVd();
            if (NULL == pVd)
            {
                return false;
            }

            int iCount = -1;
            foreach (EnumOperand, oEnum, pPhiI)
            {
                if (Values* pVx = oEnum.Get()->DynamicCast<Values>())
                {
                    if (ValuesI* pValuesI =
                            pVx->GetDefI()->DynamicCast<ValuesI>() )
                    {
                        if (iCount < 0)
                        {
                            iCount = pValuesI->CountOperands();
                        }
                        else if (static_cast<uint>(iCount) !=
                                 pValuesI->CountOperands() )
                        {
                            return false;
                        }
                    }
                    else
                    {
                        return false;
                    }
                }
                else
                {
                    // Operands of Phi %vd must be %vx
                    COMPILER_INTERNAL_ERROR();
                    return false;
                }
            } // for

            if (iCount < 0)
            {
                // No Phi operands - unreachable block
                return false;
            }

            CLOG_SECTION(2, "Decompose ~S", pPhiI);

            Values* pV2 = new Values;
            ValuesI* pValuesI = new ValuesI(pPhiI->GetTy(), pV2);

            {
                Instruction* pRefI = pPhiI;
                while (pRefI->Is<PhiI>())
                {
                    pRefI = pRefI->GetNext();
                }
                pRefI->GetBB()->InsertBeforeI(pValuesI, pRefI);
            }

            for (int i = 0; i < iCount; i++)
            {
                Register* const pRd = new Register;
                PhiI* const pPhiI2 = new PhiI(tyT, pRd);
                pPhiI->GetBB()->InsertBeforeI(pPhiI2, pPhiI);
                pValuesI->AppendOperand(pRd);

                const Type* pty = tyNil;
                foreach (EnumOperand, oEnum, pPhiI)
                {
                    PhiOperandBox* const pBox = oEnum.GetBox()->
                        StaticCast<PhiOperandBox>();

                    Instruction* const pI = pBox->GetOperand()->
                        StaticCast<Values>()->GetDefI()->StaticCast<ValuesI>();

                    CLOG(2, "<li>take ~D of ~S</li>", i, pI);

                    Operand* const pSx = pI->GetOperand(i);

                    pPhiI2->AddOperand(pBox->GetBB(), pSx);

                    pty = Type::Or(pty, pSx->GetTy());
                } // for

                pPhiI2->SetTy(pty);
                CLOG(2, "<li>update ~S</li>", pPhiI2);
            } // for i

            pVd->ReplaceAll(pV2);

            return true;
        } // Decompose

        public: static bool UpdateTy(PhiI* const pPhiI)
        {
            const Type* pty = tyNil;
            foreach (EnumOperand, oEnum, pPhiI)
            {
                pty = Type::Or(pty, oEnum.Get()->GetTy());
                ASSERT(tyNil != pty);
            } // for

            ASSERT(tyNil != pty);

            if (pPhiI->GetTy()->Equal(pty))
            {
                return false;
            }

            pPhiI->SetTy(pty);
            return true;
        } // UpdateTy
    }; // Local

    if (Local::Decompose(this))
    {
        return true;
    }

    if (Local::UpdateTy(this))
    {
        return true;
    }

    return false;
} // PhiI::Optimize

// [R]
/// <summary>
///   Optimize Ret instruction.
///   <list>
///     <item><term>Values to Single</term>
///       <description>
///        Remove VALUES instruction if it has single value.
///       </description>
///     </item>
///   </list>
/// </summary>
bool RetI::Optimize()
{
    if (Values* const pVx = GetSx()->DynamicCast<Values>())
    {
        if (ValuesI* const pValuesI = pVx->GetDefI()->DynamicCast<ValuesI>())
        {
            if (1 == pValuesI->CountOperands())
            {
                GetOperandBox(0)->Replace(pValuesI->GetSx());
                return true;
            }
        }
    } // if value

    return false;
} // RetI::Optimize

// [S]
Operand* SelectI::Compute() const
{
    Instruction* const pDefI = GetVx()->GetDefI();
    if (NULL == pDefI)
    {
        return NULL;
    }

    if (ValuesAI* const pValuesAI = pDefI->DynamicCast<ValuesAI>())
    {
        Int const iY = GetIy();
        if (static_cast<uint>(iY) >= pValuesAI->CountOperands())
        {
            return NULL;
        }
        return pValuesAI->GetOperandBox(iY)->GetOperand();
    } // if ValuesAI

    if (ValuesI* const pValuesI = pDefI->DynamicCast<ValuesI>())
    {
        Int const iY = GetIy();
        if (static_cast<uint>(iY) >= pValuesI->CountOperands())
        {
            return Literal::New(nil);
        }
        return pValuesI->GetOperandBox(iY)->GetOperand();
    } // if ValuesI

    return NULL;
} // SelectI::Compute

bool SelectI::Optimize()
{
    if (const TyValues* const ptyValues =
                    GetSx()->GetTy()->DynamicCast<TyValues>() )
    {
        const Type* const pty = ptyValues->GetNthTy(GetIy());
        if (! GetTy()->Equal(pty))
        {
            CLOG(3, "<li>update ~S => ~S</li>", this, pty);
            SetTy(pty);
            return true;
        }
    }

    return false;
} // SelectI::Optimize

// [T]
void TagDefI::Realize()
{
    Instruction::Realize();
    FrameReg* const pFd = GetSx()->StaticCast<FrameReg>();
    pFd->m_nCount += 1;

    BBlock* const pNonlocalXpBB = GetSy()->StaticCast<Label>()->GetBB();
    GetBB()->AddEdge(pNonlocalXpBB)->SetKind(CfgEdge::Kind_Nonlocal);
} // TagDefI::Realize

void TagDefI::Unrealize()
{
    FrameReg* const pFd = GetSx()->StaticCast<FrameReg>();
    ASSERT(pFd->m_nCount >= 1);
    pFd->m_nCount -= 1;

    BBlock* const pNonlocalXpBB = GetSy()->StaticCast<Label>()->GetBB();
    GetBB()->RemoveOutEdge(pNonlocalXpBB);

    Instruction::Unrealize();
} // TagDefI::Unrealize

void TerminateInstruction::OnMove(BBlock* const pNewBB)
{
    GetBB()->GetFunction()->GetExitBB()->RedirectEdgeFrom(pNewBB, GetBB());
} // TerminateInstruction::OnMove

void TerminateInstruction::Realize()
{
    Instruction::Realize();

    GetBB()->AddEdge(GetBB()->GetFunction()->GetExitBB())->
        SetKind(CfgEdge::Kind_Exit);
} // TerminateInstruction::Realize

void TerminateInstruction::Unrealize()
{
    GetBB()->RemoveOutEdge(GetBB()->GetFunction()->GetExitBB());
    Instruction::Unrealize();
} // TerminateInstruction::Unrealize

Operand* TypepI::Compute() const
{
    if (Register* const pRx = GetRx())
    {
        switch (Type::IsSubtype(pRx->GetTy(), GetTy()))
        {
        case Subtype::No:
            return Bool_False;

        case Subtype::Yes:
            return Bool_True;
        }
        return NULL;
    } // if register

    if (Literal* const pLx = GetSx()->DynamicCast<Literal>())
    {
        switch (GetTy()->IsType(pLx->GetDatum()))
        {
        case Subtype::No:
            return Bool_False;

        case Subtype::Yes:
            return Bool_True;
        } // switch
        return NULL;
    } // if literal

    // FIXME 2008-08-17 yosi@msn.com We should support local function
    // reference in TypepI::Compute.
    return NULL;
} // TypepI::Compute

// [U]
bool UpVarDefI::IsUseless() const
{
    if (GetOutput() == Void)
    {
        return true;
    }

    if (GetSx()->StaticCast<Variable>()->m_cUpRefs)
    {
        return false;
    }

    return Instruction::IsUseless();
} // UpVarDefI::IsUseless

bool UseI::IsUseless() const
{
    if (SsaOutput* const pRx = GetSx()->DynamicCast<SsaOutput>())
    {
        if (NULL == pRx->GetDefI())
        {
            CLOG(2, "<li>No open for ~S</li>", pRx);
            return true;
        }
        return pRx->GetDefI()->IsUseless();
    }

    return false;
} // UseI::IsUseless

// [V]
bool VarDefI::IsUseless() const
{
    if (GetSx()->StaticCast<Variable>()->m_cUpRefs)
    {
        return false;
    }

    //if (GetOutput()->Is<Pseudo>()) return true;
    return Instruction::IsUseless();
} // VarDefI::IsUseless

bool ValuesI::Optimize()
{
    return UpdateTy();
} /// ValuesI::Optimize

bool ValuesI::UpdateTy()
{
    Val ty = list(Qvalues);
    Val tail = ty;

    foreach (EnumOperand, oEnum, this)
    {
        tail = setf_cdr(list(oEnum.Get()->GetTy()->Unparse()), tail);
    } // for

    if (nil != cdr(ty) && nil == cddr(ty))
    {
        ty = cadr(ty);
    }

    if (equal(GetTy()->Unparse(), ty))
    {
        return false;
    }

    SetTy(Type::Parse(ty));

    return true;
} // ValuesI::UpdateTy

bool ValuesAI::Optimize()
{
    class Local
    {
        public: static ValuesI* MakeValuesI(
            ValuesAI* const pValuesAI,
            uint      const cOperands )
        {
            ASSERT(cOperands > 0);

            ValuesI* pValuesI = new ValuesI(
                pValuesAI->GetVd() );

            uint nRest = cOperands;
            foreach (EnumOperand, oEnum, pValuesAI)
            {
                nRest -= 1;
                if (0 == nRest)
                {
                    break;
                }
                pValuesI->AppendOperand(oEnum.Get());
            } // for

            return pValuesI;
        } // MakeValuesI
    }; // Local

    uint const cOperands = CountOperands();

    Operand* const pSx = GetOperandBox(cOperands - 1)->GetOperand();

    if (Literal* pLx = pSx->DynamicCast<Literal>())
    {
        ValuesI* pValuesI = Local::MakeValuesI(this, cOperands);

        {
            Val runner = pLx->GetDatum();
            while (consp(runner))
            {
                pValuesI->AppendOperand(Literal::New(car(runner)));
                runner = cdr(runner);
            } // while

            if (nil != runner)
            {
                Context::Get()->Error("Expected proper list but ~S",
                    pLx->GetDatum() );
                return false;
            }
        }

        GetBB()->ReplaceI(pValuesI, this);
        return true;
    } // if literal

    if (Register* pRx = pSx->DynamicCast<Register>())
    {
        if (ValuesI* pArgsI = Static::GetCallArgs(pRx->GetDefI(), Qlist))
        {
            ValuesI* pValuesI = Local::MakeValuesI(this, cOperands);
            foreach (EnumOperand, oEnum, pArgsI)
            {
                pValuesI->AppendOperand(oEnum.Get());
            } // for

            GetBB()->ReplaceI(pValuesI, this);
            return true;
        } // if
    } // if register

    return UpdateTy();
} // ValuesAI::Optimize

bool ValuesAI::UpdateTy()
{
    Val ty = list(Qvalues);
    Val tail = ty;

    {
        EnumOperand oEnum(this);
        while (! oEnum.AtEnd())
        {
            const Type* pty = oEnum.Get()->GetTy();
            oEnum.Next();

            if (oEnum.AtEnd())
            {
                tail = setf_cdr(list(QArest), tail);
            }

            tail = setf_cdr(list(pty->Unparse()), tail);
        } // while
    }

    if (nil != cdr(ty) && nil == cddr(ty))
    {
        ty = cadr(ty);
    }

    if (equal(GetTy()->Unparse(), ty))
    {
        return false;
    }

    SetTy(Type::Parse(ty));
    return true;
} // ValuesAI::UpdateTy

} // Compiler
} // TinyCl
