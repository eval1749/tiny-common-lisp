#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Optimize - Closure
// tinycl_c_opt_closure.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_closure.cpp#7 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

namespace Closure
{

class VarEx : public Variable
{
    public: enum Usage
    {
        Usage_None      = 0,
        Usage_Read      = 1 << 0,
        Usage_Write     = 1 << 1,
    }; // Usage
    
    // ctor
    private: VarEx() { CAN_NOT_HAPPEN(); }

    // [G]
    public: Usage GetUsage() const
        { return static_cast<Usage>(GetFlag()); }

    // [R]
    public: void ResetUsage()
    {
        SetFlag(Usage_None);
        SetStorage(Storage_Register);
    } // ResetUsage

    // [S]
    public: void SetUsage(Usage e)
        { SetFlag(e); }
}; // VarEx

// Description:
//  This pass does following:
//    1 Collect upvars
//    2 Remove unreachable functions
class SubPassClosure :
    public Pass_<SubPassClosure, SubPass>
{
    public: static const char* GetName_() { return "Closure"; }

    private: typedef WorkList_<Variable> VarList;

    // Entry point
    public: static void Run()
    {
        SubPassClosure oPass;
        oPass.run();
    } // Run

    // [A]
    private: void addUpVars(Function* const pFun, VarList* const pVars)
    {
        foreach (VarList::Enum, oEnum, pVars)
        {
            Variable* const pVar = oEnum.Get();
            if (pVar->GetOwner() != pFun)
            {
                pFun->InternUpVar(pVar);
            }
        } // for each upvar
    } // addUpVars

    // [P]
    private: void processScc(Function* const pLeader)
    {
        CLOG_SECTION(2, "SCC ~D ~S", pLeader->m_nSccNum, pLeader);

        WorkList_<Function> oFuns;
        foreach (Function::EnumSccMember, oEnum, pLeader)
        {
            Function* const pFun = oEnum.Get();
            if (pFun->IsInList()) continue;

            oFuns.Push(pFun);
            foreach (Function::EnumOutEdge, oEnum, pFun)
            {
                Function* const pCallee = oEnum.GetNode();
                if (pCallee->IsInList()) continue;
                if (pCallee->m_pSccId == pLeader) continue;
                oFuns.Push(pCallee);
            } // for each callee
        } // for each member

        WorkList_<Variable> oUpVars;
        while (! oFuns.IsEmpty())
        {
            Function* const pFun = oFuns.Pop();
            foreach (Function::EnumUpVar, oEnum, pFun)
            {
                Variable* const pVar = oEnum.Get();
                if (! pVar->IsInList())
                {
                    oUpVars.Push(pVar);
                }
            } // for each upvar
        } // while

        foreach (Function::EnumSccMember, oEnum, pLeader)
        {
            Function* const pFun = oEnum.Get();
            CLOG_SECTION(3, "member ~S", pFun);
            addUpVars(pFun, &oUpVars);
            pFun->UpdateValueTy();

            // Mark closed variable. from inner functions.
            if (pFun->IsClosure())
            {
                foreach (Function::EnumUpVar, oEnum, pFun)
                {
                    VarEx* const pVar = oEnum.Get()->Extend<VarEx>();
                    if (VarEx::Storage_Stack == pVar->GetStorage())
                    {
                        if (pVar->GetUsage() & VarEx::Usage_Write)
                        {
                            CLOG(3, "<li>Closed ~S</li>", pVar);
                            pVar->SetStorage(VarEx::Storage_Closed);
                        }
                        else
                        {
                            CLOG(3, "<li>Literal ~S</li>", pVar);
                            pVar->SetStorage(VarEx::Storage_Literal);
                        }
                    }
                } // for
            } // if closure
        } // for each member
    } // processScc

    // [R]
    private: void run()
    {
        CLOG_SECTION(1, "<h3>~A</h3>~%", GetName());

        Module* const pM = Context::Get()->GetModule();
        pM->ComputeScc();

        foreach (Module::EnumScc, oEnum, pM)
        {
            processScc(oEnum.Get());
        } // for each fun

        // Note: Removing unused funciton is done in Pass destructor.
        #if 0
        {
            WorkList_<Function> oUnreachables;
            foreach (Module::EnumFunction, oEnum, pM)
            {
                Function* pFun = oEnum.Get();
                if (0 == pFun->m_nSccNum)
                {
                    oUnreachables.Push(pFun);
                }
            } // for each fun

            while (! oUnreachables.IsEmpty())
            {
                pM->RemoveFunction(oUnreachables.Pop());
            } // while
        }
        #endif
    } // run
}; // SubPassClosure

class SubPassVarStorage :
    public Pass_<SubPassVarStorage, SubPass>
{
    public: static const char* GetName_() { return "VarStorage"; }

    private: typedef WorkList_<Variable> VarList;

    // Entry point
    public: static void Run()
    {
        SubPassVarStorage oPass;
        oPass.run();
    } // Run

    // [R]
    private: void rewriteSlots(VarEx* pVar, Instruction* pI)
    {
        Val klass;
        Type* pty;
        switch (pVar->GetStorage())
        {
        case VarEx::Storage_Closed:
            klass = CLASS_closed_cell;
            pty = tyClosedCell;
            break;

        case VarEx::Storage_Literal:
            klass = CLASS_literal_cell;
            pty = tyLiteralCell;
            break;

        case VarEx::Storage_Register:
        case VarEx::Storage_Stack:
            klass = CLASS_stack_cell;
            pty = tyStackCell;
            break;

        default:
            COMPILER_INTERNAL_ERROR();
            return;
        } // switch storage

        CLOG_SECTION(3, "Rewrite ~S", pI);

        pI->SetTy(pty);

        Literal* pTyOperand = Literal::New(klass);

        foreach (SsaOutput::EnumUser, oEnum, pI->GetSsaOutput())
        {
            if (SlotI* pSlotI = oEnum.Get()->GetI()->DynamicCast<SlotI>())
            {
                CLOG(3, "<li>~S => ~S</li>", pSlotI, pTyOperand);
                pSlotI->GetOperandBox(0)->SetOperand(pTyOperand);
            }
        } // rewriteSlots
    } // rewriteSlots

    private: void run()
    {
        CLOG_SECTION(3, "<h3>~A</h3>", GetName());

        Module* pM = Context::Get()->GetModule();
        foreach (Module::EnumFunction, oEnum, pM)
        {
            Function* pFun = oEnum.Get();
            foreach (Function::EnumVar, oEnum, pFun)
            {
                VarEx* pVar = oEnum.Get()->Extend<VarEx>();
                rewriteSlots(pVar, oEnum.GetI());
            } // for each var

            foreach (Function::EnumUpVar, oEnum, pFun)
            {
                VarEx* pVar = oEnum.Get()->Extend<VarEx>();
                rewriteSlots(pVar, oEnum.GetI());
            } // for each upvar
        } // for each fun
    } // run
}; // SubPassVarStorage

class SubPassVarUsage :
    public Pass_<SubPassVarUsage, SubPass>
{
    public: static const char* GetName_() { return "VarUsage"; }

    // Entry point
    public: static void Run()
    {
        SubPassVarUsage oPass;
        oPass.run();
    } // Run

    // [S]
    private: void computeUsage(VarEx* pVar, SsaOutput* pRd)
    {
        foreach (SsaOutput::EnumUser, oEnum, pRd)
        {
            Register* pRp = oEnum.GetI()->GetRd();
            foreach (Register::EnumUser, oEnum, pRp)
            {
                if (oEnum.GetI()->Is<StoreI>())
                {
                    pVar->SetUsage(VarEx::Usage_Write);
                    CLOG(2, "<li>~S is write</li>", pVar);
                    break;
                }
            } // for each user
        } // for each user
        CLOG(2, "<li>~S is read-only</li>", pVar);
    } // computeUsage

    // [P]
    private: void processFun(Function* pFun)
    {
        CLOG_SECTION(2, "~S", pFun);

        foreach (Function::EnumVar, oEnum, pFun)
        {
            VarEx* pVar = oEnum.Get()->Extend<VarEx>();
            computeUsage(pVar, pVar->GetRd());
        } // for each var

        bool fClosure = pFun->IsClosure();

        foreach (Function::EnumUpVar, oEnum, pFun)
        {
            VarEx* pVar = oEnum.Get()->Extend<VarEx>();
            computeUsage(pVar, oEnum.GetI()->GetQd());

            if (fClosure)
            {
                if (pVar->GetUsage() & VarEx::Usage_Write)
                {
                    CLOG(3, "<li>Closed ~S</li>", pVar);
                    pVar->SetStorage(VarEx::Storage_Closed);
                }
                else
                {
                    CLOG(3, "<li>Literal ~S</li>", pVar);
                    pVar->SetStorage(VarEx::Storage_Literal);
                }
            }
            else if (VarEx::Storage_Register == pVar->GetStorage())
            {
                CLOG(3, "<li>Stack ~S</li>", pVar);
                pVar->SetStorage(VarEx::Storage_Stack);
            }
        } // for each var
    } // processFun

    // [R]
    private: void run()
    {
        CLOG_SECTION(1, "<h3>~A</h3>~%", GetName());

        Module* const pM = Context::Get()->GetModule();
        foreach (Module::EnumFunction, oEnum, pM)
        {
            Function* const pFun = oEnum.Get();
            foreach (Function::EnumVar, oEnum, pFun)
            {
                VarEx* const pVar = oEnum.Get()->Extend<VarEx>();
                pVar->ResetUsage();
            } // for eah var
        } // for each fun

        foreach (Module::EnumFunction, oEnum, pM)
        {
            Function* const pFun = oEnum.Get();
            processFun(pFun);
        } // for each fun
    } // run
}; // SubPassVarUsage

} // Closure

using namespace Closure;

class PassClosure :
    public Pass_<PassClosure>
{
    public: static const char* GetName_() { return "Closure"; }

    // Entry point
    public: static void Run()
    {
        SubPassVarUsage::Run();
        SubPassClosure::Run();
        SubPassVarStorage::Run();
    } // Run
}; /// PassClosure

DEFPASS(Closure)

} // Compiler

} // TinyCl
