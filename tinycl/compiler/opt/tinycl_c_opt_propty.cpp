#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Optimize - Typep
// tinycl_c_opt_typep.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_propty.cpp#10 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

class RegWork :
    public LocalObject,
    public SingleLinkedList_<Operand, LocalObject>
{
    public: typedef SingleLinkedList_<Operand, LocalObject> OperandList;

    private: IMm*   const m_pIMm;

    public: RegWork(IMm* const pIMm) :
        m_pIMm(pIMm) {}

    // For assignment operator
    private: RegWork& operator =(const RegWork&) {}

    // [G]
    public: Operand* GetTop() const
    {
        ASSERT(! IsEmpty());
        return GetFirst()->m_pNode;
    } // GetTop

    // [I]
    public: bool IsEmpty() const
        { return NULL == GetFirst(); }

    // [P]
    public: Operand* Pop()
    {
        ASSERT(! IsEmpty());
        OperandList::Cons* pCons = GetFirst();
        Operand* const pSx = pCons->m_pNode;
        SetFirst(pCons->m_pNext);
        return pSx;
    } // Pop

    public: void Push(Operand* pSx)
    {
        SetFirst(new(m_pIMm) OperandList::Cons(pSx, GetFirst()));
    } // Push
}; // RegWork

class RegEx :
    public Register
{
    // [G]
    public: Operand* GetCopy() const
    {
        if (IsEmpty())
        {
            return const_cast<RegEx*>(this);
        }

        Operand* const pSx = GetWork<RegWork>()->GetTop();
#if 1
        if (RegEx* pRx = pSx->DynamicCast<RegEx>())
        {
            return pRx->GetCopy();
        }
#endif
        return pSx;
    } // GetCopy

    // [I]
    public: bool IsEmpty() const
    {
        if (NULL == GetWork<RegWork>())
        {
            CLOG(1, "<li class='r'>~S doesn't have RegWork!</li>", this);
            return true;
        }
        return GetWork<RegWork>()->IsEmpty();
    } // IsEmpty

    // [P]
    public: Operand* Pop()
    {
        ASSERT(! IsEmpty());
        return GetWork<RegWork>()->Pop();
    } // Pop

    public: void Push(Operand* const pSx)
    {
        if (NULL == GetWork<RegWork>())
        {
            CLOG(1, "<li class='r'>~S doesn't have RegWork!</li>", this);
            COMPILER_INTERNAL_ERROR();
            return;
        }
        GetWork<RegWork>()->Push(pSx);
    } // Push
}; // RegEx

/// <summary>
///   Optimization pass for simplifies TYPEP instruction.
/// </summary>
class PassPropType :
    public Pass_<PassPropType, FunctionPass>,
    protected Mm
{
    public: static const char* GetName_() { return "PropType"; }

    private: class RegStack :
        public LocalObject,
        public SingleLinkedList_<Register, LocalObject>
    {
        private: typedef SingleLinkedList_<Register, LocalObject>
            RegSingleList;

        private: IMm* const m_pIMm;

        // ctor
        public: RegStack(IMm* const pIMm) :
            m_pIMm(pIMm) {}

        // For assignment operator
        private: RegStack& operator=(RegStack&) {}

        // [I]
        public: bool IsEmpty() const
            { return NULL == GetFirst(); }

        // [P]
        public: Register* Pop()
        {
            ASSERT(! IsEmpty());
            RegSingleList::Cons* const pFirst = GetFirst();
            Register* pRx = pFirst->m_pNode;
            SetFirst(pFirst->m_pNext);
            ASSERT(NULL != pRx->GetWork<RegEx>());
            return pRx;
        } // Pop

        public: void Push(Register* const pRx)
        {
            if (NULL == pRx->GetWork<RegEx>())
            {
                CLOG(1, "<li class='r'>~S doesn't have RegWork!</li>", pRx);
                COMPILER_INTERNAL_ERROR();
                return;
            }

            SetFirst(
                new(m_pIMm) RegSingleList::Cons(
                    pRx,
                    GetFirst() ) );
        } // Push
    }; // RegStack

    private: bool       m_fChanged;
    private: RegStack*  m_pKills;

    /// <summary>
    ///  Pass entry point.
    /// </summary>
    //= <FIXME date="2009-01-01" by="yosi@msn.com>
    //=   We should use Call Graph SCC.
    //= </FIXME>
    protected: override void processFunction(Function* pFun)
    {
        CLOG_SECTION(1, "<h3>Process ~S</h3>", pFun);

        pFun->ComputeDominance();

        m_fChanged = false;

        processBBlock(pFun->GetEntryBB());

        if (m_fChanged)
        {
            pFun->Clean();
        }

        pFun->UpdateValueTy();
    } // processFunction

    // [I]
    private: void insertAtFirst(
        BBlock*      const pBB,
        Instruction* const pI )
    {
        ASSERT(NULL != pBB);
        ASSERT(NULL != pI);

        foreach (BBlock::EnumI, oEnum, pBB)
        {
            if (! oEnum.Get()->Is<PhiI>())
            {
                pBB->InsertBeforeI(pI, oEnum.Get());
                return;
            }
        } // for
        CAN_NOT_HAPPEN();
    } // insertAtFirst

    private: bool isNil(const Operand* const pSx)
    {
        if (const Literal* const pLx = pSx->DynamicCast<const Literal>())
        {
            return pLx->GetDatum() == nil;
        }
        return false;
    } // isNil

    // [P]
    private: void processBBlock(BBlock* pBB)
    {
        CLOG_SECTION(1, "process ~S", pBB);

        RegStack oKills(this);
        m_pKills = &oKills;

        processPredBB(pBB);

        foreach (BBlock::EnumI, oEnum, pBB)
        {
            Instruction* const pI = oEnum.Get();

            if (Register* pRd = pI->GetRd())
            {
                pRd->SetWork(new(this) RegWork(this));
            }

            if (pI->Is<PhiI>())
            {
                continue;
            }

            foreach (Instruction::EnumOperand, oEnum, pI)
            {
                updateOperand(oEnum.GetBox());
            } // for

            if (CastInstruction* pCastI =
                    pI->DynamicCast<CastInstruction>() )
            {
                processCast(pCastI);
            }
        } // for insn

        // Update PHI
        foreach (BBlock::EnumSucc, oEnum, pBB)
        {
            BBlock* const pSuccBB = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pSuccBB)
            {
                PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
                if (NULL == pPhiI)
                {
                    break;
                }

                updateOperand(pPhiI->GetOperandBox(pBB));
            } // for insn
        } // for succ

        foreach (BBlock::EnumChild, oEnum, pBB)
        {
            processBBlock(oEnum.Get());
        } // for

        while (! oKills.IsEmpty())
        {
            Register* const pRx = oKills.Pop();
            pRx->Extend<RegEx>()->Pop();
        } // whil
    } // processBBlock

    private: void processCast(CastInstruction* const pCastI)
    {
        CLOG_SECTION(2, "process ~S", pCastI);

        if (RegEx* const pRx = pCastI->GetSx()->DynamicCast<RegEx>())
        {
            RegEx* const pRd = pCastI->GetRd()->Extend<RegEx>();

            const Type* const pty = Type::And(
                pRd->GetTy(),
                pRx->GetTy() );

            if (pRx->GetTy()->Equal(pty))
            {
                CLOG(2, "<li>useless: ~S => ~S</li>", pRx, pRd);
                pRd->ReplaceAll(pRx);
            }
            else
            {
                CLOG(2, "<li>copy ~S => ~S</li>", pRx, pRd);
                pCastI->SetTy(pty);
                pRx->Push(pRd);
                m_pKills->Push(pRx);
                m_fChanged = true;
            }
        } // if reg
    } // processCast

    // Propagate
    //  o TYPEP  + BRANCH
    //  o NE nil + BRANCH
    private: void processPredBB(BBlock* const pBB)
    {
        class Local
        {
            public: static BBlock* GetOnlyOnePred(BBlock* const pBB)
            {
                BBlock::EnumPred oEnum(pBB);
                if (oEnum.AtEnd())
                {
                    return NULL;
                }

                BBlock* const pPredBB = oEnum.Get();
                oEnum.Next();

                if (! oEnum.AtEnd())
                {
                    return NULL;
                }

                return pPredBB;
            } // GetOnlyOnePred
        }; // Lcoal

        BBlock* const pPredBB = Local::GetOnlyOnePred(pBB);
        if (NULL == pPredBB)
        {
            return;
        }

        BranchI* const pBranchI =
            pPredBB->GetLastI()->DynamicCast<BranchI>();
        if (NULL == pBranchI)
        {
            return;
        }

        Instruction* const pDefI = pBranchI->GetBx()->GetDefI();
        if (TypepI* const pTypepI = pDefI->DynamicCast<TypepI>())
        {
            CLOG_SECTION(2, "process ~S + ~S", pTypepI, pBranchI);

            Register* const pRx = pTypepI->GetRx();
            if (NULL == pRx)
            {
                return;
            }

            const Type* ptyA = pTypepI->GetSy()->StaticCast<Type>();

            const Type* pty = pBranchI->GetTrueBB() == pBB ?
                ptyA :
                Type::Diff(pRx->GetTy(), ptyA);

            if (! pRx->GetTy()->Equal(pty))
            {
                Register* const pRd = new Register(Variable::Get(pRx));
                Operand*  const pR2 = pRx->Extend<RegEx>()->GetCopy();
                insertAtFirst(pBB, new StaticCastI(pty, pRd, pR2));
                m_fChanged = true;
            }
        }
        else if (NeI* const pNeI = pDefI->DynamicCast<NeI>())
        {
            CLOG_SECTION(2, "process ~S + ~S", pNeI, pBranchI);

            Register* const pRx = pNeI->GetRx();
            if (NULL == pRx)
            {
                return;
            }

            if (! isNil(pNeI->GetSy()))
            {
                return;
            }

            const Type* ptyA = pRx->GetTy();

            const Type* pty = pBranchI->GetTrueBB() == pBB ?
                Type::Diff(ptyA, tyNull) :
                tyNull;

            if (! pRx->GetTy()->Equal(pty))
            {
                Register* const pRd = new Register(Variable::Get(pRx));
                Operand*  const pR2 = pRx->Extend<RegEx>()->GetCopy();
                insertAtFirst(pBB, new StaticCastI(pty, pRd, pR2));
                m_fChanged = true;
            }
        } // if
    } // processPredBB

    // [U]
    private: void updateOperand(OperandBox* const pBox)
    {
        if (RegEx* const pRx = pBox->GetOperand()->DynamicCast<RegEx>())
        {
            if (! pRx->IsEmpty())
            {
                Operand* pSx = pRx->GetCopy();

                CLOG(2, "<li>replace ~S in ~S by ~S</li>",
                    pRx, pBox->GetI(), pSx );

                pBox->Replace(pSx);
                m_fChanged = true;
            }
        }
    } // updateOperand
}; // PassPropType

DEFPASS(PropType)

} // Compiler
} // TinyCl
