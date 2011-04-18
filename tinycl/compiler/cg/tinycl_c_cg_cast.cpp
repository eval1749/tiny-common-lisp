#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Optimize - Typep
// tinycl_c_opt_typep.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_cast.cpp#6 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

class GeneralDbBase
{
    public: struct Entry
    {
        Val m_name;
        void (*m_pfnHandler)(Instruction*);
    }; // Entry
}; // GeneralDbBase

class GeneralDb :
    public FunDb_<GeneralDb, GeneralDbBase::Entry, 307>,
    public GeneralDbBase
{
    public: static GeneralDb* Create()
    {
        static Entry s_rgoEntry[] =
        {
            // [S]
            { Qslot_value,  handle_slot_value },

            { SETF_slot_value, handle_setf_slot_value },
        }; // s_rgoEntry

        GeneralDb* pGeneralDb = new GeneralDb;
        pGeneralDb->load(s_rgoEntry, lengthof(s_rgoEntry));
        return pGeneralDb;
    } // Create

    ////////////////////////////////////////////////////////////
    //
    // Handlers
    //
    // [S]
    private: static void handle_slot_value(
        Instruction*    const pCallI )
    {
        Operand* const pSx = pCallI->GetVy()->GetDefI()->
            GetOperandBox(0)->GetOperand();

        Operand* const pSy = pCallI->GetVy()->GetDefI()->
            GetOperandBox(1)->GetOperand();

        Register* const pRx = pSx->DynamicCast<Register>();
        Literal*  const pLy = pSy->DynamicCast<Literal>();

        if (NULL != pRx && NULL != pLy)
        {
            Context::Note(pCallI, "Use generic ~S ~S for ~S",
                Qslot_value,
                pLy->GetDatum(),
                pRx->GetTy()->Unparse() );
        }
    } // handle_slot_value

    private: static void handle_setf_slot_value(
        Instruction*    const pCallI )
    {
        Operand* const pSy = pCallI->GetVy()->GetDefI()->
            GetOperandBox(1)->GetOperand();

        Operand* const pSz = pCallI->GetVy()->GetDefI()->
            GetOperandBox(2)->GetOperand();

        Register* const pRy = pSy->DynamicCast<Register>();
        Literal*  const pLz = pSz->DynamicCast<Literal>();

        if (NULL != pRy && NULL != pLz)
        {
            Context::Note(pCallI, "Use generic ~S ~S for ~S",
                SETF_slot_value,
                pLz->GetDatum(),
                pRy->GetTy()->Unparse() );
        }
    } // handle_setf_slot_value
}; // GeneralDb

GeneralDb* GeneralDb::sm_pFunDb;

/// <summary>
///   Expand RUNTIMECAST and STATICCAST instructions.
/// </summary>
class PassRuntimeCast :
    public Pass_<PassRuntimeCast, FunctionPass>
{
    public: static const char* GetName_() { return "RuntimeCast"; }

    private: WorkList_<Instruction> m_oRuntimeCasts;
    private: FunName* m_pErrorFn;

    public: PassRuntimeCast() :
        m_pErrorFn(NULL) {}

    /// <summary>
    ///  Pass entry point.
    /// </summary>
    protected: override void processFunction(Function* pFun)
    {
        CLOG_SECTION(1, "<h3>Process ~S</h3>", pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();
                if (pI->Is<CallI>())
                {
                    processCall(pI);
                }
            } // for insn
        } // for

        foreach (Function::EnumBBlock, oEnum, pFun)
        {

            BBlock* const pBB = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();
                if (pI->Is<RuntimeCastI>())
                {
                    CLOG_SECTION(2, "process ~S", pI);
                    m_oRuntimeCasts.Push(pI);
                }
                else if (pI->Is<ShouldBeI>())
                {
                    CLOG_SECTION(2, "process ~S", pI);
                    pI->GetRd()->ReplaceAll(pI->GetSx());
                }
                else if (pI->Is<StaticCastI>())
                {
                    CLOG_SECTION(2, "process ~S", pI);
                    pI->GetRd()->ReplaceAll(pI->GetRx());
                }
            } // for insn
        } // for bb

        while (! m_oRuntimeCasts.IsEmpty())
        {
            expand(static_cast<RuntimeCastI*>(m_oRuntimeCasts.Pop()));
        } // while

        pFun->Clean();
    } // processFunction

    // [E]
    private: void expand(RuntimeCastI* const pI)
    {
        ASSERT(NULL != pI);

        CLOG_SECTION(2, "<h4>Expand ~S</h4>", pI);

        if (NULL == m_pErrorFn)
        {
            m_pErrorFn = new FunName(
                Qerror,
                nil,
                TyFunction::Parse(
                    list(Qfunction, list(t, QArest, Qt), nil) ));
        }

        BBlock* const pBB = pI->GetBBlock();

        Bool* const pBd = new Bool;
        pBB->InsertBeforeI(
            new TypepI(pBd, pI->GetRx(), pI->GetTy()),
            pI );

        BBlock* const pErrorBB = new BBlock;
        {
            pBB->GetFunction()->InsertBefore(
                pErrorBB,
                pBB->GetFunction()->GetExitBB() );

            Values* const pVy = new Values;
            ValuesI* const pValuesI = new ValuesI(pVy);
            pValuesI->AppendOperand(Literal::New(Qtype_error));
            pValuesI->AppendOperand(Literal::New(Kdatum));
            pValuesI->AppendOperand(pI->GetRx());
            pValuesI->AppendOperand(Literal::New(Kexpected_type));
            pValuesI->AppendOperand(Literal::New(pI->GetTy()->Unparse()));
            pErrorBB->AppendI(pValuesI);
            pErrorBB->AppendI(new CallI(tyVoid, Void, m_pErrorFn, pVy));
            pErrorBB->AppendI(new UnreachableI);
        }

        BBlock* const pAfterBB = pBB->SplitBefore(pI);

        pBB->ReplaceI(
            new BranchI(pBd, pAfterBB, pErrorBB),
            pBB->GetLastI() );

        pI->GetRd()->ReplaceAll(pI->GetRx());

        pAfterBB->RemoveI(pI);
    } // expand

    // [P]
    private: static void processCall(Instruction* const pI)
    {
        FunName* const pFunName = pI->GetSx()->DynamicCast<FunName>();
        if (NULL == pFunName)
        {
            return;
        }

        const GeneralDb::Entry* const pEntry =
            GeneralDb::Find(pFunName->GetName());

        if (NULL == pEntry)
        {
            return;
        }

        Values* const pVy = pI->GetVy();
        if (NULL == pVy)
        {
            // Note: We set %void in CG pass.
            return;
        }

        ValuesI* const pValuesI = pVy->GetDefI()->DynamicCast<ValuesI>();
        if (NULL == pValuesI)
        {
            return;
        }

        pEntry->m_pfnHandler(pI);
    } // processCall
}; // PassRuntimeCast

DEFPASS(RuntimeCast)

} // Compiler
} // TinyCl
