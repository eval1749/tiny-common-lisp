#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Ensure
// tinycl_x86_c_ensure.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_ensure.cpp#23 $
//
#include "./tinycl_x86_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

using namespace X86;

// COMISS
//  less than       ZF=0 CF=1
//  greater than    ZF=0 CF=0
//  equal           ZF=1 CF=0
static Tttn
k_rgeInt2FloatTttn[] =
{
    tttn_P,     //  0 O     => not used
    tttn_P,     //  1 NO    => not used
    tttn_P,     //  2 B     => not sued
    tttn_P,     //  3 NB    => not used
    tttn_E,     //  4 E     => ZF=1
    tttn_NE,    //  5 NE    => ZF=0
    tttn_P,     //  6 BE
    tttn_P,     //  7 NBE
    tttn_P,     //  8 S
    tttn_P,     //  9 NS
    tttn_P,     // 10 P
    tttn_NP,    // 11 NP
    tttn_B,     // 12 L     => CF=1
    tttn_NB,    // 13 GE    => CF=0
    tttn_NA,    // 14 LE    => ZF=1 and CF=1
    tttn_A,     // 15 G     => ZF=0 and CF=0
}; // k_rgeInt2FloatTttn

/// <summary>
///   Pass for ensuring instruction operands for X86 ISA.
/// </summary>
class PassX86Ensure :
    public    Pass_<PassX86Ensure, FunctionPass>,
    protected InstructionDispatcher,
    protected X86Utility
{
    public: static const char* GetName_() { return "X86Ensure"; }

    private: bool m_fChanged;

    public: PassX86Ensure() : m_fChanged(false) {}

    /// <summary>
    ///   An entry point of X86 Ensure pass.
    /// </summary>
    protected: override void processFunction(Function* pFun)
    {
        CLOG_SECTION(1, "<h2>process ~S</h2>", pFun);

        {
            CLOG_SECTION(2, "Bool user");
            foreach (Function::EnumOutput, oEnum, pFun)
            {
                if (Bool* pBd = oEnum.Get()->DynamicCast<Bool>())
                {
                    processBool(pBd);
                }
            } // for each output
        }

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();
            BBlock::EnumI oEnumI(pBB);
            while (! oEnumI.AtEnd())
            {
                Instruction* const pI = oEnumI.Get();
                oEnumI.Next();

                if (Values* const pVd = pI->GetVd())
                {
                    hostSelects(pVd, pI);
                }

                dispatch(pI);
            } // for each insn
        } // for each bb

        if (m_fChanged)
        {
            pFun->Clean();
        }
    } // processFunction

    // [E]
    /// <summary>
    ///  Make output and the first operand same.
    /// </summary>
    private: void ensureRdRx(Instruction* const pI)
    {
        ensureRdRn(pI, pI->GetOperandBox(0));
    } // ensureRdRx

    private: void ensureRdRn(
        Instruction* const pI,
        OperandBox*  const pBox )
    {
        ASSERT(pI->GetOutput() != pI->GetSx());

        if (Float* const pFd = pI->GetOutput()->DynamicCast<Float>())
        {
            Float* const pFx = new Float;
            pI->GetBBlock()->InsertBeforeI(
                new AssignI(pFx, pBox->GetOperand()),
                pI );

            pBox->Replace(pFx);

            Float* const pFnew = new Float;
            pI->SetOutput(pFnew);
            pI->GetBBlock()->InsertAfterI(new CopyI(pFd, pFnew), pI);
        }
        else if (Register* const pRd = pI->GetRd())
        {
            Register* const pRx = new Register;
            pI->GetBBlock()->InsertBeforeI(
                new AssignI(pRx, pBox->GetOperand()),
                pI );

            pBox->Replace(pRx);

            Register* const pRnew = new Register;
            pI->SetOutput(pRnew);
            pI->GetBBlock()->InsertAfterI(new CopyI(pRd, pRnew), pI);
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }
    } // ensureRdRx

    // [G]
    private: static Tttn getTttnForintCmp(Instruction* const pI)
    {
        if (pI->Is<EqI>()) return tttn_E;
        if (pI->Is<GeI>()) return tttn_GE;
        if (pI->Is<GtI>()) return tttn_G;
        if (pI->Is<LeI>()) return tttn_LE;
        if (pI->Is<LtI>()) return tttn_L;
        if (pI->Is<NeI>()) return tttn_NE;

        COMPILER_INTERNAL_ERROR();
        return tttn_Z;
    } // getTttnForintCmp

    // [H]
    private: void hostSelects(Values* const pVd, Instruction* const pDefI)
    {
        Instruction* pRefI = pDefI;
        for (;;)
        {
            pRefI = pRefI->GetNext();
            if (! pRefI->Is<PhiI>())
            {
                break;
            }
        } // while

        for (;;)
        {
            if (pRefI->Is<CountI>())
            {
                // Skip
            }
            else if (pRefI->Is<SelectI>())
            {
                // Skip
            }
            else
            {
                break;
            }

            pRefI = pRefI->GetNext();
        } // for

        foreach (Values::EnumUser, oEnum, pVd)
        {
            Instruction* const pUserI = oEnum.GetI();

            if (pUserI->Is<SelectI>() || pUserI->Is<CountI>())
            {
                if (pUserI != pRefI->GetPrev())
                {
                    pRefI->GetBB()->MoveBeforeI(pUserI, pRefI);
                }
            }
        } // for each user
    } // hostSelects

    // [I]
    private: static bool isRegOperand(Operand* pSx)
    {
        return pSx->Is<Physical>() || pSx->Is<Register>();
    } // isRegOperand

    // [P]
    private: void processBool(Bool* const pBd)
    {
        if (pBd->GetDefI()->Is<x86BoolI>())
        {
            return;
        }

        while (OperandBox* const pBox = pBd->GetFirst())
        {
            processBoolUser(pBd, pBox);
        } // while
    } // processBool

    // Note: We don't use integer equality for comparing float32. Since, we
    // don't want to handle -0, infinity and NaN, e.g. (= +0f0 -0f0) = true.
    private: void processBoolUser(Bool* const pBd, OperandBox* const pBox)
    {
        Pseudo* const pR1 = new Pseudo;
        Bool*   const pB2 = new Bool;

        Instruction* const pDefI = pBd->GetDefI();
        Instruction* const pUseI = pBox->GetI();

        Tttn eTttn = getTttnForintCmp(pDefI);

        if (pDefI->GetSx()->GetTy() == tyFloat32)
        {
            pUseI->GetBBlock()->InsertBeforeI(
                new x86CmpF32I(pR1, pDefI->GetSx(), pDefI->GetSy()),
                pUseI );

            eTttn = k_rgeInt2FloatTttn[eTttn];
        }
        else if (pDefI->GetSx()->GetTy() == tyFloat64)
        {
            pUseI->GetBBlock()->InsertBeforeI(
                new x86CmpF64I(pR1, pDefI->GetSx(), pDefI->GetSy()),
                pUseI );

            eTttn = k_rgeInt2FloatTttn[eTttn];
        }
        else
        {
            pUseI->GetBBlock()->InsertBeforeI(
                new x86CmpI(pR1, pDefI->GetSx(), pDefI->GetSy()),
                pUseI );
        }

        pUseI->GetBBlock()->InsertBeforeI(
            new x86BoolI(pB2, pR1, eTttn),
            pUseI );

        pBox->Replace(pB2);

        m_fChanged = true;
    } // processBoolUser

    private: override void processDefault(Instruction* pI)
    {
        if (Bool* const pBd = pI->GetBd())
        {
            processBool(pBd);
        }
    } // processDefault

    /// <summary>
    ///  Make an output to pseudo output.
    /// </summary>
    private: void pseudofy(Instruction* const pI)
    {
        if (SsaOutput* const pRd = 
                pI->GetOutput()->DynamicCast<SsaOutput>() )
        {
            Pseudo* const pQ2 = new Pseudo;
            pRd->ReplaceAll(pQ2);
            pI->SetOutput(pQ2);
        }
    } // pseudofy

    // [R]
    private: static void rewriteToCall(Val const callee, Instruction* const pI)
    {
        rewriteToCall(Literal::New(callee), pI);
    } // rewriteToCall

    private: static void rewriteToCall(
        Operand*     const pNewCallee,
        Instruction* const pI )
    {
        Operand* const pCallee = pI->GetSx();
        Values*  pVy     = pI->GetVy();

        Instruction* const pRefI = pI->GetVy()->GetDefI();

        BBlock* pBB = pI->GetBB();

        if (pRefI->Is<ValuesI>())
        {
            Pseudo* const pQp = new Pseudo;

            pBB->InsertBeforeI(
                new x86LeaI(pQp, getTcb(), offsetof(Thread, m_fn)),
                pRefI );

            pBB->InsertBeforeI(new StoreI(pQp, pCallee), pRefI);
        }
        else
        {
            Pseudo* const pQsave = new Pseudo;
            pBB->InsertBeforeI(new MvSaveI(pQsave, pVy), pI);

            Pseudo* const pQp = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQp, getTcb(), offsetof(Thread, m_fn)),
                pI );

            pBB->InsertBeforeI(new StoreI(pQp, pCallee), pI);

            pVy = new Values;
            pBB->InsertBeforeI(new MvRestoreI(pVy, pQsave), pI);
        } // if

        pBB->ReplaceI(
            new CallI(pI->GetTy(), pI->GetOutput(), pNewCallee, pVy),
            pI );
    } // rewriteToCall

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //
    DefProcI(Add) { ensureRdRx(pI); }

    DefProcI(Call)
    {
        Operand* const pSx = pI->GetSx();

        if (pSx->Is<FunName>())
        {
            // ok
        }
        else if (pSx->Is<Function>())
        {
            // ok
        }
        else if (pSx->Is<Literal>())
        {
            // ok
        }
        else
        {
            rewriteToCall(QBfuncall, pI);
        }
    } // Call

    DefProcI(Closure)
    {
        rewriteToCall(FunName::Intern(QBmake_closure), pI);
    } // Closure

    DefProcI(Div) { ensureRdRx(pI); }

    DefProcI(Go)
    {
        Values* const pVy = new Values;
        pI->GetBB()->InsertBeforeI(
            new ValuesI(pVy, pI->GetSx()),
            pI );

        pI->GetBB()->ReplaceI(
            new CallI(tyVoid, Void, QBgo, pVy),
            pI );
    } // Go

    DefProcI(If)
    {
        if (! isRegOperand(pI->GetSz()))
        {
            if (isRegOperand(pI->GetSy()))
            {
                CLOG_SECTION(2, "Swap operands: ~S", pI);

                m_fChanged = true;

                x86BoolI* const pBoolI =
                    pI->GetBx()->GetDefI()->StaticCast<x86BoolI>();

                Bool* const pBx = new Bool;

                pI->GetBB()->InsertBeforeI(
                    new x86BoolI(
                        pBx,
                        pBoolI->GetSx(),
                        static_cast<Tttn>(pBoolI->GetTttn() ^ 1) ),
                    pI );

                Operand* const pSy = pI->GetSy();
                Operand* const pSz = pI->GetSz();

                pI->GetOperandBox(0)->Replace(pBx);
                pI->GetOperandBox(1)->SetOperand(pSz);
                pI->GetOperandBox(2)->SetOperand(pSy);

                CLOG(2, "<li>~S</li>", pI);
                CLOG(2, "<li>~S</li>", pBoolI);
            }
            else
            {
                CLOG(2, "<li>Liteal to Reg: ~S</li>", pI);

                //= <FIXME date="2008-06-24" by="yosi@msn.com">
                //=     Should we do this by ensuring %bx def-use?
                //= </FIXME>

                // Insert Sz before CMP to allow using XOR.
                // Example:
                //  (defun foo (&optional (x 0)) x)
                //
                // CMP  %q1 <= ...          <- pRefI
                // BOOL %q2 <= %q1 ...
                Instruction* const pRefI = pI->GetBx()->GetDefI()->
                    GetSx()->StaticCast<Pseudo>()->GetDefI();

                Register* const pRz = new Register;

                pI->GetBBlock()->InsertBeforeI(
                    new CopyI(pRz, pI->GetSz()),
                    pRefI );

                pI->GetOperandBox(2)->Replace(pRz);
            }
        } // if %rz ins't register

        ensureRdRn(pI, pI->GetOperandBox(1));
    } // If

    DefProcI(LogAnd) { ensureRdRx(pI); }
    DefProcI(LogEqv) { unexpected(pI); }
    DefProcI(LogIOr) { ensureRdRx(pI); }
    DefProcI(LogXor) { ensureRdRx(pI); }

    DefProcI(Mul)        { ensureRdRx(pI); }
    DefProcI(MvSave)     { pseudofy(pI); }
    DefProcI(ReturnFrom) { rewriteToCall(QBreturn, pI); }
    DefProcI(Slot)       { pseudofy(pI); }
    DefProcI(Sub)        { ensureRdRx(pI); }
    DefProcI(Tlv)        { pseudofy(pI); }
    DefProcI(Throw)      { rewriteToCall(QBthrow, pI); }

    DefProcI(UnBox)
    {
        if (pI->GetTy() == tyFloat32 ||
            pI->GetTy() == tyFloat64 )
        {
            Register* const pR1 = new Register;

            pI->GetBB()->InsertBeforeI(
                new CopyI(pR1, pI->GetSx()),
                pI );

            pI->GetOperandBox(0)->Replace(pR1);
        } // if float32
    } // UnBox

    DefProcI(UpVarRef)
        { pseudofy(pI); }

    DefProcI(ValuesA)
    {
        Values* const pVy = new Values;
        ValuesI* const pValuesI = new ValuesI(pVy);
        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            pValuesI->AppendOperand(oEnum.Get());
        } // for each operand

        pI->GetBB()->InsertBeforeI(pValuesI, pI);

        pI->GetBB()->ReplaceI(
            new CallI(pI->GetTy(), pI->GetVd(), Literal::New(QvaluesA), pVy),
            pI );
    } // ValuesA

    DefProcI(x86Lea)
    {
        Pseudo* const pRd = pI->GetOutput()->DynamicCast<Pseudo>();
        if (NULL == pRd)
        {
            return;
        }

        foreach (Pseudo::EnumUser, oEnum, pRd)
        {
            Instruction* pUserI = oEnum.Get()->GetI();

            if (pUserI->Is<StoreI>())
            {
                foreach (Instruction::EnumOperand, oEnum, pI)
                {
                    if (Register* pRx = oEnum.Get()->DynamicCast<Register>())
                    {
                        pUserI->GetBB()->InsertAfterI(
                            new UseI(pRx),
                            pUserI );
                    }
                } // for operand
            } // if StoreI
        } // for user
    } // x86Lea
}; /// PassX86Ensure

DEFPASS(X86Ensure)

} // Compiler

} // TinyCl
