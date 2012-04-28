#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Expand Values
// arch/x86/tinycl_x86_c_values.cpp
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_values.cpp#14 $
//
#include "./tinycl_x86_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

using namespace X86;

static Val const
k_rgoHasArity[] =
{
    QvaluesA,
    // Qvalues_list,    // fundb replaces values-list to values*
}; // k_rgoHasArity

/// <summary>
///   Pass for expanding VALUES instruction.
///   <para>
///     Following instructions are related to this expansion.
///     <list>
///       <item><term>CALL</term></item>
///       <item><term>COUNT</term></item>
///       <item><term>OPENFINALLY</term>
///         <description>Same as CALL.</description>
///       </item>
///       <item><term>MVRESTORE</term>
///         <description>Restore $r0 from $tcb.mv_value[0].</description>
///       </item>
///       <item><term>MVSAVE</term>
///         <description>Save $r0 to $tcb.mv_value[0].</description>
///       </item>
///       <item><term>RET</term></item>
///       <item><term>SELECT</term>
///         <description>We don't need SELECT anymore.</description>
///       </item>
///     </list>
///   </para>
/// </summary>
class PassX86Values :
    public   Pass_<PassX86Values, FunctionPass>,
    protected InstructionDispatcher,
    protected X86Utility
{
    public: static const char* GetName_() { return "X86Values"; }

    private: typedef void (*RewriteFn)(Instruction*);

    /// <summary>Entry point</summary>
    protected: virtual void processFunction(Function* pFun) override
    {
        CLOG_SECTION(1, "<h2>process ~S</h2>", pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBB = oEnum.Get();
            BBlock::EnumI oEnumI(pBB);
            while (! oEnumI.AtEnd())
            {
                Instruction* pI = oEnumI.Get();
                oEnumI.Next();
                dispatch(pI);
            } // for each insn
        } // for each bb

        pFun->Clean();
    } // processFunction

    // [H]
    private: static bool hasArity(Operand* const pSx)
    {
        if (FunName* const pFunName = pSx->DynamicCast<FunName>())
        {
            for (
                const Val* p = &k_rgoHasArity[0];
                p < &k_rgoHasArity[lengthof(k_rgoHasArity)];
                p++ )
            {
                if (*p == pFunName->GetName())
                {
                    return true;
                }
            } // for
        } // if
        return false;
    } // hasArity

    // [R]
    private: void static rewriteForCall(Instruction* pDefI)
    {
        if (CallI* pCallI = pDefI->DynamicCast<CallI>())
        {
            if (hasArity(pCallI->GetSx()))
            {
                return;
            }

            Instruction* pRefI = pCallI->GetNext();
            BBlock*      pBB   = pRefI->GetBB();

            Pseudo* pQy = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQy, getTcb(), ThreadExtra_FixnumOne),
                pRefI );

            // Note: Operand of x86Bool instruction is dummy.
            Bool* pBx = new Bool;
            pBB->InsertBeforeI(
                new x86BoolI(pBx, getGpr($rn), tttn_C),
                pRefI );

            pBB->InsertBeforeI(
                new IfI(getGpr($rn), pBx, getGpr($rn), pQy),
                pRefI );

            return;
        } // CallI

        if (ValuesI* pValuesI = pDefI->DynamicCast<ValuesI>())
        {
            Instruction* pRefI = pValuesI->GetNext();
            BBlock*      pBB   = pRefI->GetBB();

            uint cOperands = pValuesI->CountOperands();
            if (0 == cOperands)
            {
                pBB->InsertBeforeI(
                    new LogXorI(tyInt, getGpr($rn), getGpr($rn), getGpr($rn)),
                    pRefI );
            }
            else
            {
                pBB->InsertBeforeI(
                    new CopyI(
                        getGpr($rn),
                        Literal::New(Fixnum::Encode(cOperands)) ),
                    pRefI );
            }
            return;
        } // ValuesI

        C_INTERNAL_ERROR("rewriteForCall: unexpected instruction");
    } // rewriteForCall

    private: void static rewriteForRet(Instruction* pDefI)
    {
        if (pDefI->Is<CallI>())
        {
            // nothing to do
            return;
        }

        if (ValuesI* pValuesI = pDefI->DynamicCast<ValuesI>())
        {
            Instruction* pRefI = pValuesI->GetNext();
            BBlock*      pBB   = pRefI->GetBB();

            uint cOperands = pValuesI->CountOperands();
            switch (cOperands)
            {
            case 0:
                pBB->InsertBeforeI(
                    new CopyI(getGpr($r0), Literal::New(nil)),
                    pRefI );

                pBB->InsertBeforeI(
                    new LogXorI(tyInt, getGpr($rn), getGpr($rn), getGpr($rn)),
                    pRefI );

                pBB->InsertBeforeI(new x86StcI(), pRefI);
                break;

            case 1:
                // This must be optimized before this pass.
                COMPILER_INTERNAL_ERROR();
                break;

            default:
                pBB->InsertBeforeI(
                    new CopyI(
                        getGpr($rn),
                        Literal::New(Fixnum::Encode(cOperands)) ),
                    pRefI );

                pBB->InsertBeforeI(new x86StcI(), pRefI);
                break;
            } // switch cOperands

            return;
        } // ValuesI

        C_INTERNAL_ERROR("rewriteForRet: Unexpected instruction");
    } // rewriteForRet

    private: static void walker(RewriteFn const pfn, Values* const pVy)
    {
        Instruction* const pDefI = pVy->GetDefI();

        if (pDefI->Is<CallI>())
        {
            if (Literal* const pLx = pDefI->GetSx()->DynamicCast<Literal>())
            {
                if (pLx->GetDatum() == QvaluesA)
                {
                    // Function values* always sets $rn.
                    return;
                }
            }

            pfn(pDefI);
            return;
        } // CallI

        if (MvRestoreI* const pMvRestoreI = pDefI->DynamicCast<MvRestoreI>())
        {
            Instruction* const pMvSaveI = pMvRestoreI->GetQx()->GetDefI();
            unless (pMvSaveI->Is<MvSaveI>())
            {
                C_INTERNAL_ERROR("Expect MvSave");
                return;
            }

            walker(pfn, pMvSaveI->GetVx());
            return;
        } // MvRestoreI

        if (pDefI->Is<NonlocalI>())
        {
            return;
        }

        if (PhiI* const pPhiI = pDefI->DynamicCast<PhiI>())
        {
            foreach (Instruction::EnumOperand, oEnum, pPhiI)
            {
                walker(pfn, oEnum.Get()->StaticCast<Values>());
            } // for each operand
            return;
        } // PhiI

        if (pDefI->Is<PrologueI>())
        {
            // Should be COUNT instruction.
            return;
        }

        if (pDefI->Is<ValuesI>())
        {
            pfn(pDefI);
            return;
        } // ValuesI

        C_INTERNAL_ERROR(pDefI->GetMnemonic());
    } // walker

    private: void processCall(Instruction* const pI)
    {
        CLOG_SECTION(2, "process ~S", pI);

        if (NULL != pI->GetVy())
        {
            if (NeedArity(pI->GetSx()))
            {
                walker(rewriteForCall, pI->GetVy());
            }
        } // if

        pI->GetOperandBox(1)->Replace(Void);
        pI->SetOutput(Void);
    } // Call

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //
    DefProcI(Call)
        { processCall(pI); }

    DefProcI(Count)
    {
        CLOG_SECTION(2, "process ~S", pI);
        walker(rewriteForCall, pI->GetVx());
        pI->GetBB()->RemoveI(pI);
    } // Count

    DefProcI(OpenFinally)
        { processCall(pI); }

    DefProcI(MvRestore)
    {
        CLOG_SECTION(2, "process ~S", pI);

        Pseudo* pQx = new Pseudo;
        pI->GetBB()->InsertBeforeI(
            new x86LeaI(pQx, getTcb(), offsetof(Thread, mv_value)),
            pI );

        pI->GetBB()->InsertBeforeI(
            new LoadI(getGpr($r0), pQx),
            pI );
    } // MvRestore

    DefProcI(MvSave)
    {
        CLOG_SECTION(2, "process ~S", pI);

        Pseudo* pQx = new Pseudo;
        pI->GetBB()->InsertBeforeI(
            new x86LeaI(pQx, getTcb(), offsetof(Thread, mv_value)),
            pI );

        pI->GetBB()->InsertBeforeI(
            new StoreI(pQx, getGpr($r0)),
            pI );
    } // MvSave

    DefProcI(Ret)
    {
        CLOG_SECTION(2, "process ~S", pI);

        BBlock* pBB = pI->GetBB();

        Operand* pSx = pI->GetSx();

        if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            if (pRx->GetDesc()->m_nId != $r0)
            {
                pBB->InsertBeforeI(new CopyI(getGpr($r0), pRx), pI);
            }

            pI->GetOperandBox(0)->Replace(Literal::New(Fixnum::Encode(1)));
        }
        else if (Values* pVx = pSx->DynamicCast<Values>())
        {
            walker(rewriteForRet, pVx);
            pI->GetOperandBox(0)->Replace(Literal::New(Qvalues));
        }
        else if (pSx->Is<VoidOutput>())
        {
            // nothing to do
        }
        else
        {
            pBB->InsertBeforeI(new CopyI(getGpr($r0), pSx), pI);
            pI->GetOperandBox(0)->Replace(Literal::New(Fixnum::Encode(1)));
        }
    } // Ret

    DefProcI(Select)
    {
        CLOG_SECTION(2, "process ~S", pI);
        pI->GetBB()->RemoveI(pI);
    } // Count
}; /// PassX86Values

DEFPASS(X86Values)

} // Compiler

} // TinyCl
