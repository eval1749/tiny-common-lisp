#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse
// tinycl_c_cl_01_emit.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_01_emit.cpp#11 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

/// <summary>
///   Emits variable binding for specified variable definition.
/// </summary>
/// <param name="pVarDef">A variable to bound</param>
/// <param name="pSx">A value for binding</param>
/// <param name="pOpenBindI">The last merge-able OpenBind instruction</param>
OpenBindI* PassParse::emitBind(
    VarDef*     const pVarDef,
    Operand*    const pSx,
    OpenBindI*  pOpenBindI )
{
    Val const cell = pVarDef->GetCell();
    if (nil == cell)
    {
        Variable* const pVar = pVarDef->GetVar();
        emitI(new VarDefI(new Register(pVar), pVar, pSx));
    }
    else
    {
        if (NULL == pOpenBindI)
        {
            FrameReg* const pRd = new FrameReg(
                NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
                Qlet );

            BindFrame* const pFrame = new(this) BindFrame(pRd);
            pushFrame(pFrame);
            pRd->SetWork(pFrame);
            pOpenBindI = new OpenBindI(pRd);
        }

        pOpenBindI->AddBind(cell, pSx);
    }

    return pOpenBindI;
} // PassParse::emitBind

/// <summary>
///   Emit generaized boolean value for boolean register. This method
///   is used for inititialize supplied variable of optional parameter
///   and keyword parameter of lambda-list.
/// </summary>
Register* PassParse::emitBool(Bool* const pBx)
{
    Register* const pRd = new Register;
    emitI(new IfI(pRd, pBx, True, Literal::New(nil)));
    return pRd;
} // PassParse::emitBool

void PassParse::emitCall(
    const Type*     const pty,
    Output*         const pSd,
    const Callee*   const pCallee,
    Values*         const pVy )
{
    CallI* pCallI = new CallI(pty, pSd, pCallee->m_pOperand, pVy);
    pCallI->SetNotinline(pCallee->m_fNotinline);

    if (NULL != m_pFrame)
    {
        pCallI->SetFrameReg(m_pFrame->GetRd());
    }

    emitI(pCallI);
} // PassParse::emitCall

Operand* PassParse::emitLinkage(
    const Expect* const pExpect,
    Register*     const pRx )
{
    if (NULL == m_pCurr)
    {
        return Unreachable;
    }

    Operand* const pSx = emitCast(
        pExpect->m_pty->GetPrimaryTy(),
        pRx,
        pExpect->m_eKind );

    return emitLinkage(pSx);
} // PassParse::emitLinkage

Operand* PassParse::emitLinkage(
    Operand* const pSx )
{
    if (Unreachable == pSx)
    {
        return setUnreachable();
    }

    switch (getLinkage())
    {
    case Linkage_Jump:
        emitSucc();
        return pSx;

    case Linkage_Next:
        return pSx;

    case Linkage_Phi:
    {
        BBlock* const pCurr = m_pCurr;
        PhiI*   const pPhiI = m_pSucc->GetFirstI()->StaticCast<PhiI>();

        emitSucc();

        setPhiOperand(pPhiI, pCurr, pSx);

        return pPhiI->GetOutput();
    } // Linkage_Phi

    case Linkage_Return:
        if (NULL == m_pFrame || m_pFrame->GetOwner() != m_pOwner)
        {
            emitI(new RetI(pSx));
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }
        return setUnreachable();

    case Linkage_Unreachable:
        return Unreachable;

    default:
        COMPILER_INTERNAL_ERROR();
        return pSx;
    } // switch linkage
} // PassParse::emitLinkage

/// <summary>
///   Emit RuntimeCast instruction if needed. We should support following
///   compilation options and put them into attribute of RuntimeCast
///   instruction.
///   <list>
///     <item>no type check</item>
///     <item>simple type check</item>
///   </list>
/// </summary>
Register* PassParse::emitCast(
    const Type*  const pty,
    Register*    const pRx,
    Expect::Kind const eKind )
{
    ASSERT(! pty->Is<TyValues>());

    switch (Type::IsSubtype(pRx->GetTy(), pty))
    {
    case Subtype::No:
        if (Subtype::No == Type::IsSubtype(pty, pRx->GetTy()))
        {
            // We must detect incompatible types before calling
            // emitCast.
            DEBUG_FORMAT("RuntimeCast for incompatible type: ~S ~S~%",
                pRx->GetTy()->Unparse(), pty->Unparse() );

            COMPILER_INTERNAL_ERROR();
        }
        break;

    case Subtype::Yes:
        return pRx;

    case Subtype::Unknown:
        break;

    default:
        COMPILER_INTERNAL_ERROR();
        break;
    } // switch

    Register* const pRd = new Register(pRx->GetVar());
    //= <FIXME date="2008-08-17" by="yosi@msn.com">
    //=   Set attributes of RuntimeCast instruction from compilation options.
    //= </FIXME>
    if (Expect::Kind_Argument == eKind)
    {
        emitI(new ShouldBeI(pty, pRd, pRx));
    }
    else
    {
        emitI(new RuntimeCastI(pty, pRd, pRx));
    }
    return pRd;
} // PassParse::emitCast

Register* PassParse::emitLoadVar(
    const Type* const pty,
    Val         const cell )
{
    Register* const pRptr = new Register;
    if (value_cell_p(cell))
    {
        emitI(
            new SlotI(
                tyPtrT,
                pRptr,
                CLASS_value_cell,
                Qvalue,
                Literal::New(cell) ) );
    }
    else if (tlv_record_p(cell))
    {
        emitI(new TlvI(pRptr, cell));
    }
    else
    {
        COMPILER_INTERNAL_ERROR();
        return new Register;
    }

    Register* const pRd = new Register;
    emitI(new LoadI(pty, pRd, pRptr));

    //= <FIXME date="2007-09-16" by="yosi@msn.com">
    //=   Check unbound variable
    //= </FIXME>

    return pRd;
} // PassParse::emitLinkage

// Called by:
//  PassParse::parse_mutliple_value_prog1
//  PassParse::parse_unwind_proect
Operand* PassParse::emitRestoreValues(Operand* const pSx)
{
    if (Values* const pVx = pSx->DynamicCast<Values>())
    {
        Instruction* const pDefI = pVx->GetDefI();
        if (pDefI->Is<ValuesI>() || pDefI->Is<ValuesAI>())
        {
            // Move VALUES instruction to end of current block
            m_pCurr->MoveBeforeI(pDefI, NULL);
            return pVx;
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }
    } // if values
    return pSx;
} // PassParse::emitRestoreValues

/// <summary>
///   <para>
///     Put VALUES or VALUES* instruction at definition bblock.
///     emitRestoreValue moves this instruction to appropirate
///     bblock.
///   </para>
///   <para>
///     We use this method for multiple-value-prog1, return-from, and
///     unwind-protect.
///   </para>
/// </summary>
Operand* PassParse::emitSaveValues(Operand* const pSx)
{
    if (Values* pVx = pSx->DynamicCast<Values>())
    {
        Instruction* pDefI = pVx->GetDefI();
        if (pDefI->Is<ValuesI>())  return pVx;
        if (pDefI->Is<ValuesAI>()) return pVx;

        if (pDefI->Is<MvRestoreI>())
        {
            MvSaveI* const pMvSaveI = pDefI->GetRx()->GetDefI()->
                StaticCast<MvSaveI>();

            pVx   = pMvSaveI->GetVx();
            pDefI = pVx->GetDefI();
        } // if MvRestoreI

        Operand* pFnList = Literal::New(Qlist);
        {
            if (FunRef* const pFunRef = findFunRef(Qlist))
            {
                if (FunPro* const pFunPro = pFunRef->DynamicCast<FunPro>())
                {
                    pFnList = pFunPro->GetFunName();
                }
            }
        }

        Register* const pRx = new Register;

        pDefI->GetBB()->InsertAfterI(
            new CallI(tyList, pRx, pFnList, pVx),
            pDefI );

        Values* const pVd = new Values;
        pDefI->GetBB()->InsertAfterI(
            new ValuesAI(pVd, pRx),
            pDefI->GetNext() );

        return pVd;
    } // if values

    return pSx;
} // PassParse::emitSaveValues

void PassParse::emitStoreVar(Val const cell, Operand* const pSy)
{
    Register* const pRptr = new Register;
    if (value_cell_p(cell))
    {
        emitI(
            new SlotI(
                tyPtrT,
                pRptr,
                CLASS_value_cell,
                Qvalue,
                Literal::New(cell) ) );
    }
    else if (tlv_record_p(cell))
    {
        emitI(new TlvI(pRptr, cell));
    }
    else
    {
        COMPILER_INTERNAL_ERROR();
        return;
    }

    emitI(new StoreI(pRptr, pSy));
} // PassParse::emitStoreVar

Operand* PassParse::emitSucc()
{
    emitI(new JumpI(m_pSucc));
    m_pCurr = m_pSucc;
    return Unreachable;
} // PassParse::emitSucc

Operand* PassParse::emitUnreachable()
{
    foreach (ClFrame::Enum, oEnum, m_pFrame)
    {
        ClFrame* const pFrame = oEnum.Get();
        if (pFrame->GetOwner() != m_pOwner)
        {
            break;
        }

        emitI(new UseI(pFrame->GetRd()));
    } // for each frame

    emitI(new UnreachableI());
    return Unreachable;
} // PassParse::emitUnreachable

void PassParse::emitUnwind(ClFrame* pFrame)
{
    if (pFrame->GetOwner() != m_pOwner) return;

    emitI(new CloseI(pFrame->GetRd()));

    if (FinallyFrame* const pFinFrame = pFrame->DynamicCast<FinallyFrame>())
    {
        Values* const pVy = new Values;
        emitI(new ValuesI(pVy));
        Operand* const pFinally = pFinFrame->GetRd()->GetDefI()->GetSx();
        emitI(new CallI(tyVoid, Void, pFinally, pVy));
    }
} // PassParse::emitUnwind

void PassParse::emitUnwinds(ClFrame* pTo)
{
    foreach (ClFrame::Enum, oEnum, m_pFrame)
    {
        ClFrame* const pFrame = oEnum.Get();
        if (pFrame == pTo) break;
        emitUnwind(pFrame);
    } // for each frame
} // PassParse::emitUnwinds

// For let and let*
// Make one free register
Operand* PassParse::emitWeakRestoreValues(Operand* const pSx)
{
    if (Values* const pVx = pSx->DynamicCast<Values>())
    {
        m_pCurr->MoveBeforeI(pVx->GetDefI(), NULL);
        return pVx;
    }

    if (Register* const pRx = pSx->DynamicCast<Register>())
    {
        if (MvSaveI* const pMvSaveI = pRx->GetDefI()->DynamicCast<MvSaveI>())
        {
            Values* const pVx = pMvSaveI->GetVx();
            if (m_pCurr->GetLastI() == pMvSaveI)
            {
                return pVx;
            }
            else
            {
                Values* const pVd = new Values;
                emitI(new MvRestoreI(pVd, pRx));
                return pVd;
            }
        }
    } // if

    return pSx;
} // PassParse::emitWeakRestoreValues

Operand* PassParse::emitWeakSaveValues(Operand* const pSx)
{
    Values* const pVx = pSx->DynamicCast<Values>();
    if (NULL == pVx) return pSx;

    Instruction* const pDefI = pVx->GetDefI();

    if (pDefI->Is<MvRestoreI>())
    {
        return pDefI->GetVd();
    }

    if (pDefI->Is<ValuesI>())
    {
        return pDefI->GetOutput();
    }

    if (pDefI->Is<ValuesAI>())
    {
        return pDefI->GetOutput();
    }

    Register* const pRd = new Register;
    emitI(new MvSaveI(pRd, pVx));
    return pRd;
} // PassParse::emitWeakSaveValues

Operand* PassParse::endPhi(const Type* const pty)
{
    if (getLinkage() == Linkage_Return)
    {
        return Unreachable;
    }

    if (tyVoid == pty)
    {
        return emitLinkage(Void);
    }

    return emitLinkage(m_pCurr->GetFirstI()->GetOutput());
} // PassParse::endPhi

PassParse::Linkage PassParse::getLinkage() const
{
    if (NULL == m_pCurr) return Linkage_Unreachable;
    if (m_pSucc->IsExitBB()) return Linkage_Return;
    if (m_pSucc == m_pCurr)  return Linkage_Next;
    if (NULL == m_pSucc->GetFirstI()) return Linkage_Jump;
    if (m_pSucc->GetFirstI()->Is<PhiI>()) return Linkage_Phi;
    return Linkage_Jump;
} // PassParse::getLinkage

// Note: End of pBB must have JUMP or BRANCH instruction.
void PassParse::setPhiOperand(
    PhiI*       const pPhiI,
    BBlock*     const pBB,
    Operand*    pSx )
{
    if (NULL != pPhiI->GetVd() && ! pSx->Is<Values>())
    {
        ASSERT(
            pBB->GetLastI()->Is<JumpI>() ||
            pBB->GetLastI()->Is<JumpI>() );

        Values* const pVx = new Values;
        pBB->InsertBeforeI(new ValuesI(pVx, pSx), pBB->GetLastI());
        pSx = pVx;
    }

    pPhiI->AddOperand(pBB, pSx);
} // PassParse::setPhiOperand

BBlock* PassParse::startPhi(const Type* const pty)
{
    BBlock* pSucc = m_pSucc;

    switch (getLinkage())
    {
    case Linkage_Next:
        pSucc   = NULL;
        goto makePhi;

    case Linkage_Jump:
        goto makePhi;

    case Linkage_Phi:
        break;

    case Linkage_Return:
        break;

    default:
        COMPILER_INTERNAL_ERROR();
        break;

    makePhi:
    {
        BBlock* const pNewSucc = newBBlock();
        m_pSucc = pNewSucc;
        if (tyVoid != pty)
        {
            Output* const pRd = newOutput(pty);
            if (Void != pRd)
            {
                pNewSucc->AppendI(new PhiI(pty, pRd));
            }
        }
        break;
    } // makePhi
    } // switch linkage

    return pSucc;
} // PassParse::startPhi

} // Compiler
} // TinyCl
