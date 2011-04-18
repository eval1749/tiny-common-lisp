#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Data Flow Analysis
// tinycl_c_dfa.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_dfa.cpp#6 $
//
#include "./tinycl_c_ir_dfa.h"

namespace TinyCl
{

namespace Compiler
{

BitVec BitVec::s_oEmpty(0, 0);

bool BBlock::IsLiveIn(int k) const
    { return Extend<DataFlowBB>()->GetIn()->IsOne(k); }

bool BBlock::IsLiveOut(int k) const
    { return Extend<DataFlowBB>()->GetOut()->IsOne(k); }

void BBlock::SetLiveIn(int k)
    { Extend<DataFlowBB>()->GetIn()->SetOne(k); }

void BBlock::SetLiveOut(int k)
    { Extend<DataFlowBB>()->GetOut()->SetOne(k); }

/// <summary>
///   Solve DFA equation backward.
///   <code>
///     LiveIn[B]  := UEVar[B] union (LiveOut[B] - VarKill[B])
///     LiveOut[B] := union LiveIn[S] for all succ
///         UEVar[B]   = Upward Exposed Variable in B
///         VarKill[B] = Variable killed in B
///   </code>
///   <list>
///     <item>Note: We use LiveKill[exit] for work bitvec.</item>
///     <item>Note: All bblock in pFun must be reachable.</item>
///   </list>
/// </summary>
void Dfa::SolveBackward(Function* const pFun)
{
    BitVec* pWork = pFun->GetExitBB()->Extend<DataFlowBB>()->GetKill();

    bool fChanged = true;
    uint cCount = 0;
    while (fChanged)
    {
        cCount += 1;

        fChanged = false;
        foreach (Function::EnumBBlockPreorderReverse, oEnum, pFun)
        {
            DataFlowBB* const pCurr = oEnum.Get()->Extend<DataFlowBB>();

            foreach (BBlock::EnumOutEdge, oEnum, pCurr)
            {
                DataFlowBB* const pSucc = oEnum.Get()->GetTo()->
                    Extend<DataFlowBB>();

                pCurr->GetOut()->Ior(pSucc->GetIn());
            } // for each succ

            pWork->Copy(pCurr->GetOut());
            pWork->AndC2(pCurr->GetKill());
            pWork->Ior(pCurr->GetIn());

            if (! pCurr->GetIn()->Equal(pWork))
            {
                BitVec* pPrev = pCurr->GetIn();
                pCurr->SetIn(pWork);
                pWork = pPrev;
                fChanged = true;
            }
        } // for each bblock
    } // while

    pWork->FillZero();
    pFun->GetExitBB()->Extend<DataFlowBB>()->SetKill(pWork);

    CLOG(1, "<li>SolveBackward ~S ~D iteration~%", pFun, cCount);

    if (pFun->GetEntryBB()->Extend<DataFlowBB>()->GetIn()->CountOne() > 0)
    {
        COMPILER_INTERNAL_ERROR();
    }
} // Dfa::SolveBackward

namespace DfaPrivate
{

class RegLiveness
{
    // Entry Point
    public: static void Run(Function* const pFun)
    {
        RegList oRegList;

        uint cRegs = 0;
        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRx = oEnum.Get();
            pRx->SetIndex(cRegs);
            cRegs += 1;
            oRegList.Append(pRx);
        } // for each reg

        if (0 == cRegs)
        {
            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
                pBB->InitDataFlow(0);
            } // for each bblock
            return;
        }

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
            pBB->InitDataFlow(cRegs);
            CLOG(2, "<h3>Local liveness ~S</h3>", pBB);
            computeLocal(pBB);
            dumpLiveness(pBB, &oRegList);
        } // for each bblock

        Dfa::SolveBackward(pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
            CLOG(2, "<h3>Local liveness ~S</h3>", pBB);
            dumpLiveness(pBB, &oRegList);
        } // for each bblock
    } // Run

    // [C]
    //  Computes VarKill[b] and UEVar[b] for specified bblock.
    private: static void computeLocal(DataFlowBB* const pBB)
    {
        BBlock::EnumI oEnumI(pBB);

        while (! oEnumI.AtEnd())
        {
            PhiI* const pPhiI = oEnumI.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI)
            {
                break;
            }

            if (Register* const pRd = pPhiI->GetRd())
            {
                pBB->SetKill(pRd->GetIndex());
            }

            oEnumI.Next();
        } // for each phi

        while (! oEnumI.AtEnd())
        {
            Instruction* pI = oEnumI.Get();
            oEnumI.Next();

            foreach (Instruction::EnumOperand, oEnumI, pI)
            {
                markUse(pBB, oEnumI.GetRx());
            } // for each operand

            if (Register* pRd = pI->GetRd())
            {
                pBB->SetKill(pRd->GetIndex());
            }
        } // for each insn

        // Mark Phi operands in succs
        foreach (BBlock::EnumSucc, oEnum, pBB)
        {
            BBlock* const pSucc = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pSucc)
            {
                PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
                if (NULL == pPhiI)
                {
                    break;
                }

                if (Register* const pRx = pPhiI->GetRx(pBB))
                {
                    markUse(pBB, pRx);
                }
            } // for ach insn
        } // for each succ
    } // computeLocal

    // [D]
    protected: static void dumpLiveness(
        DataFlowBB* pBB,
        RegList*    pRegList )
    {
        Val stream = Context::Get()->GetPass()->GetStream();
        if (nil == stream)
        {
            return;
        }

        write_string("<table border='1' cellpadding='3'>\n", stream);

        {
            uint cRegs = 0;
            write_string("<tr><td>LiveIn</td><td>", stream);
            foreach (RegList::Enum, oEnum, pRegList)
            {
                Register* pRx = oEnum.Get();
                if (pBB->IsLiveIn(pRx->GetIndex()))
                {
                    CLOG(1, " ~S", pRx);
                    cRegs += 1;
                }
            } // for each

            if (0 == cRegs)
            {
                write_string("<i>None</i>", stream);
            }

            write_string("</td></tr>\n", stream);
        }

        {
            uint cRegs = 0;
            write_string("<tr><td>LiveKill</td><td>", stream);
            foreach (RegList::Enum, oEnum, pRegList)
            {
                Register* pRx = oEnum.Get();
                if (pBB->IsKill(pRx->GetIndex()))
                {
                    CLOG(1, " ~S", pRx);
                    cRegs += 1;
                }
            } // for each

            if (0 == cRegs)
            {
                write_string("<i>None</i>", stream);
            }

            write_string("</td></tr>\n", stream);
        }

        {
            uint cRegs = 0;
            write_string("<tr><td>LiveOut</td><td>", stream);
            foreach (RegList::Enum, oEnum, pRegList)
            {
                Register* pRx = oEnum.Get();
                if (pBB->IsLiveOut(pRx->GetIndex()))
                {
                    CLOG(1, " ~S", pRx);
                    cRegs += 1;
                }
            } // for each

            if (0 == cRegs)
            {
                write_string("<i>None</i>", stream);
            }

            write_string("</td></tr>\n", stream);
        }

        write_string("</table>\n", stream);
    } // dumpLiveness

    // [M]
    private: static void markUse(DataFlowBB* const pBB, Register* const pRx)
    {
        if (NULL == pRx) return;
        if (! pBB->IsKill(pRx->GetIndex()))
        {
            pBB->SetIn(pRx->GetIndex());
        }
    } // markUse
}; // RegLiveness

class VarLiveness
{
    // Entry Point
    public: static void Run(Function* const pFun)
    {
        uint cVars = 0;
        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* const pVar = oEnum.Get();
            pVar->SetIndex(cVars);
            cVars += 1;
        } // for each reg

        if (0 == cVars)
        {
            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
                pBB->InitDataFlow(0);
            } // for each bblock
            return;
        }

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
            pBB->InitDataFlow(cVars);
            computeLocal(pBB);
        } // for each bblock

        Dfa::SolveBackward(pFun);
    } // Run

    // [C]
    // computeLocal
    //  Computes VarKill[b] and UEVar[b] for specified bblock.
    private: static void computeLocal(DataFlowBB* const pBB)
    {
        foreach (BBlock::EnumI, oEnum, pBB)
        {
            Instruction* const pI = oEnum.Get();

            if (pI->Is<LoadI>())
            {
                if (Variable* const pVar = mapRegToVar(pI->GetRx()))
                {
                    if (! pBB->IsKill(pVar->GetIndex()))
                    {
                        pBB->SetIn(pVar->GetIndex());
                    }
                }
            }
            else if (pI->Is<StoreI>())
            {
                if (Variable* const pVar = mapRegToVar(pI->GetRx()))
                {
                    pBB->SetKill(pVar->GetIndex());
                }
            }
            else if (pI->Is<VarDefI>())
            {
                Variable* const pVar = pI->GetSx()->StaticCast<Variable>();
                pBB->SetKill(pVar->GetIndex());
            }
        } // for each insn
    } // computeLocal

    // [M]
    private: static Variable* mapRegToVar(Register* const pRx)
    {
        SlotI* const pSlotI = pRx->GetDefI()->DynamicCast<SlotI>();
        if (NULL == pSlotI) return NULL;

        Register* const pRz = pSlotI->GetRz();
        if (NULL == pRz) return NULL;

        VarDefI* const pVarDefI = pRz->GetDefI()->DynamicCast<VarDefI>();
        if (NULL == pVarDefI) return NULL;

        return pVarDefI->GetSx()->StaticCast<Variable>();
    } // mapRegToVar

    private: static void markUse(
        DataFlowBB* const pBB,
        Variable*   const pVar )
    {
        int const k = pVar->GetIndex();
        if (! pBB->IsKill(k))
        {
            pBB->SetIn(k);
        }
    } // markUse
}; // VarLiveness

} // DfaPrivate

using namespace DfaPrivate;

bool Function::ComputeLiveness()
{
    RegLiveness::Run(this);
    return true;
} // Function::ComputeLiveness

bool Function::ComputeVarLiveness()
{
    VarLiveness::Run(this);
    return true;
} // Function::ComputeVarLiveness

} // Compiler

} // TinyCl
