#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Optimize - Cfg2Ssa
// tinycl_c_opt_cfg2ssa.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/opt/tinycl_c_opt_cfg2ssa.cpp#3 $
//
#include "./tinycl_c_opt.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

namespace Cfg2Ssa
{

class VarEx : public Variable
{
    public: enum Flag
    {
        Flag_None,

        Flag_Global,
        Flag_Liveness,
        Flag_Local,
    }; // Flag

    private: VarEx() {}

    public: bool IsGlobal(BBlock* const pBB) const
    {
        switch (GetFlag())
        {
        case Flag_Global:
            return true;

        case Flag_Liveness:
            return pBB->IsLiveIn(GetIndex());

        case Flag_Local:
            return false;
        } // switch flag

        CAN_NOT_HAPPEN();
    } // IsGlobal
}; // VarEx

class VarExt :
    public LocalObject,
    public SingleLinkedList_<Operand, LocalObject>
{
    public: typedef SingleLinkedList_<Operand, LocalObject> OperandList;

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
        Operand* pSx = pCons->m_pNode;
        SetFirst(pCons->m_pNext);
        return pSx;
    } // Pop
}; // VarExt

class SubPassInsertPhi :
    public Pass_<SubPassInsertPhi, SubPass>
{
    public: static const char* GetName_() { return "InsertPhi"; }

    private: Function*  m_pFun;

    // ctor
    private: SubPassInsertPhi(Function* pFun) :
        m_pFun(pFun) {}

    // Entry Point
    public: static void Run(Function* pFun)
    {
        SubPassInsertPhi oSubPass(pFun);
        oSubPass.run();
    } // Run

    // [I]
    private: void insertPhi(BBlock* pBB, Variable* pVar)
    {
        Register* const pRd = new Register(pVar);
        PhiI* const pPhiI = new PhiI(pVar->GetTy(), pRd);

        // Associate this PHI instruction to corresponding variable.
        pPhiI->SetWork(pVar);
        pBB->InsertBeforeI(pPhiI, pBB->GetFirstI());
    } // insertPhi

    // [P]
    private: void processVar(Variable* pVar)
    {
        CLOG(1, "<li>~S<ol>~%", pVar);

        Register* pRcell = pVar->GetRd();

        WorkList_<BBlock> oDoneList;
        oDoneList.Push(m_pFun->GetExitBB());

        WorkList_<BBlock> oWorkList;

        // Initialize work list
        foreach (BBlock::EnumFrontier, oEnum, m_pFun->GetEntryBB())
        {
            BBlock* pFrontierBB = oEnum.Get();
            if (! pFrontierBB->IsInList())
            {
                oWorkList.Push(pFrontierBB);
            }
        } // for each frontier

        // Add all STORE to work list
        foreach (Register::EnumUser, oEnum, pRcell)
        {
            SlotI* pSlotI = oEnum.GetI()->DynamicCast<SlotI>();
            if (NULL == pSlotI) continue;

            foreach (Register::EnumUser, oEnum, pSlotI->GetRd())
            {
                StoreI* pStoreI = oEnum.GetI()->DynamicCast<StoreI>();
                if (NULL == pStoreI)
                {
                    CLOG(1, "<li>~S</li>~%", oEnum.GetI());
                    continue;
                }

                BBlock* pBB = pStoreI->GetBBlock();
                if (pBB->IsInList()) continue;

                CLOG(1, "<li>~S</li>~%", pStoreI);

                foreach (BBlock::EnumFrontier, oEnum, pBB)
                {
                    BBlock* pFrontierBB = oEnum.Get();
                    if (! pFrontierBB->IsInList())
                    {
                        oWorkList.Push(pFrontierBB);
                    }
                } // for each frontier
            } // for each user of SLOT
        } // for each user of VARDEF

        while (! oWorkList.IsEmpty())
        {
            BBlock* pBB = oWorkList.Pop();
            if (pVar->Extend<VarEx>()->IsGlobal(pBB))
            {
                insertPhi(pBB, pVar);
            }

            oDoneList.Push(pBB);

            foreach (BBlock::EnumFrontier, oEnum, pBB)
            {
                BBlock* pFrontierBB = oEnum.Get();
                if (! pFrontierBB->IsInList())
                {
                    oWorkList.Push(pFrontierBB);
                }
            } // for each frontier
        } // while

        CLOG(1, "</ol></li>~%");
    } // processVar

    // [R]
    private: void run()
    {
        CLOG(1, "<h2>Insert Phi</h2>~%");
        CLOG(1, "<ol>~%");
        foreach (Function::EnumVar, oEnum, m_pFun)
        {
            Variable* pVar = oEnum.Get();
            if (VarEx::Flag_None == pVar->GetFlag())
            {
                continue;
            }
            processVar(pVar);
        } // for each var
        CLOG(1, "</ol>~%");
    } // run
}; // SubPassInternPhi

class SubPassPrepare :
    public Pass_<SubPassPrepare, SubPass>
{
    public: static const char* GetName_() { return "Prepare"; }

    // Entry Point
    public: static void Run(Function* pFun)
    {
        SubPassPrepare oSubPass;
        oSubPass.run(pFun);
    } // Run

    private: WorkList_<BBlock> m_oNonlocalBBs;

    // computeLiveness
    private: void computeLiveness(Function* const pFun)
    {
        CLOG(2, "<li><b>Compute Full Liveness</b></li>~%");

        pFun->ComputeVarLiveness();

        #if _DEBUG
        {
            CLOG(1, "<ol>~%");
            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                BBlock* const pBB = oEnum.Get();
                CLOG(1, "<li>~S liveIn={", pBB);
                foreach (Function::EnumVar, oEnum, pFun)
                {
                    Variable* const pVar = oEnum.Get();
                    if (pBB->IsLiveIn(pVar->GetIndex()))
                    {
                        CLOG(1, " ~S", pVar);
                    }
                } // for each var
                CLOG(1, "}</li>~%");
            } // for each bb
            CLOG(1, "</ol>~%");
        }
        #endif // _DEBUG

        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* const pVar = oEnum.Get();
            if (pVar->GetStorage() == Variable::Storage_Register)
            {
                CLOG(1, "<li>Live ~S</li>~%", pVar);
                pVar->SetFlag(VarEx::Flag_Liveness);
            }
            else
            {
                CLOG(1, "<li><b class=g>Skip</b> ~S</li>~%", pVar);
            }
        } // for each var

        while (! m_oNonlocalBBs.IsEmpty())
        {
            BBlock* const pBB = m_oNonlocalBBs.Pop();

            CLOG(2, "<li>Nonlocal ~S<ol>", pBB);
            foreach (Function::EnumVar, oEnum, pFun)
            {
                Variable* const pVar = oEnum.Get();
                if (pVar->GetFlag() == VarEx::Flag_Liveness)
                {
                    if (pBB->IsLiveIn(pVar->GetIndex()))
                    {
                        pVar->SetStorage(Variable::Storage_Stack);
                        pVar->SetFlag(VarEx::Flag_None);
                        CLOG(2, "<li>~S nonlocal at ~S</li>~%", pVar, pBB);
                    }
                } // if
            } // for each var
            CLOG(2, "</ol></li>~%");
        } // for each nonlocal bb
    } // computeLiveness

    /// <summary>
    //    Marks variable global if UseBB isn't DefBB for Semi-Prune SSA form.
    /// </summary>
    private: void computeSemiPrune(Function* const pFun)
    {
        CLOG(2, "<li><b>Compute Semi-Prune Liveness</b></li>~%");

        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* const pVar = oEnum.Get();
            if (pVar->GetStorage() != Variable::Storage_Register)
            {
                CLOG(1, "<li><b class=g>Skip</b> ~S</li>%", pVar);
            }
            else if (Instruction* const pDefI = pVar->GetDefI())
            {
                BBlock* const pDefBB = pDefI->GetBBlock();
                foreach (Register::EnumUser, oEnum, pVar->GetRd())
                {
                    Instruction* const pUseI = oEnum.GetI();
                    if (pUseI->GetBBlock() != pDefBB)
                    {
                        CLOG(1, "<li>Global ~S</li>~%", pVar);
                        pVar->SetFlag(VarEx::Flag_Global);
                        break;
                    }
                } // for each user
            }
            else
            {
                CLOG(1, "<li><b class=r>Skip</b> ~S (no def)</li>%", pVar);
            } // if
        } // for each var
    } // computeSemiPrune

    // [R]
    private: void run(Function* const pFun)
    {
        CLOG(1, "<h2>Prepare ~S</h2>~%", pFun);

        uint nIndex = 0;
        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* const pVar = oEnum.Get();

            // Make sure later sub pass doesn't see Flag_Liveness.
            pVar->SetFlag(VarEx::Flag_None);
            pVar->SetWork(NULL);

            if (NULL == pVar->GetDefI())
            {
                continue;
            }

            switch (pVar->GetStorage())
            {
            case Variable::Storage_Register:
                pVar->SetFlag(VarEx::Flag_Local);
                pVar->SetIndex(nIndex);
                nIndex += 1;
                break;
            } // switch storage
        } // for each var

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();

            foreach (BBlock::EnumI, oEnum, pBB)
            {
                if (PhiI* pPhiI = oEnum.Get()->DynamicCast<PhiI>())
                {
                    pPhiI->SetWork(NULL);
                }
                else
                {
                    break;
                }
            } // for insn

            foreach (BBlock::EnumInEdge, oEnum, pBB)
            {
                if (oEnum.Get()->GetKind() == CfgEdge::Kind_Nonlocal)
                {
                    m_oNonlocalBBs.Push(pBB);
                    break;
                }
            } // for each edge
        } // for each bb

        CLOG(1, "<ol>~%");
        if (m_oNonlocalBBs.IsEmpty())
        {
            computeSemiPrune(pFun);
        }
        else
        {
            computeLiveness(pFun);
        }
        CLOG(1, "</ol>~%");
    } // run
}; // SubPassPrepare

class SubPassRename :
    public    Pass_<SubPassRename, SubPass>,
    protected InstructionDispatcher,
    protected Mm
{
    public: static const char* GetName_() { return "Rename"; }

    private: typedef SingleLinkedList_<VarExt, LocalObject> VarExtList;

    private: VarExtList* m_pKillList;

    // ctor
    private: SubPassRename() {}

    // Entry Point
    public: static void Run(Function* pFun)
    {
        SubPassRename oSubPass;
        oSubPass.run(pFun);
    } // Run

    private: void run(Function* pFun)
    {
        CLOG(1, "<h3>rename ~S</h3>~%", pFun);
        CLOG(2, "<ol>~%");

        processBBlock(pFun->GetEntryBB());

        CLOG(2, "</ol>~%");
    } // run

    // [G]
    private: VarExt* getVarExt(Register* const pRx)
    {
        if (NULL == pRx) return NULL;

        SlotI* const pSlotI = pRx->GetDefI()->DynamicCast<SlotI>();
        if (NULL == pSlotI) return NULL;

        Register* const pRcell = pSlotI->GetRz();
        if (NULL == pRcell) return NULL;
        if (NULL == pRcell->GetDefI()) return NULL;

        VarDefI* const pVarDefI = pRcell->GetDefI()->DynamicCast<VarDefI>();
        if (NULL == pVarDefI) return NULL;

        return pVarDefI->GetSx()->StaticCast<Variable>()->GetWork<VarExt>();
    } // getVarExt

    // [P]
    private: void processBBlock(BBlock* const pBB)
    {
        CLOG(2, "<li>rename ~S<ol>~%", pBB);

        VarExtList oKillList;
        m_pKillList = &oKillList;

        {
            BBlock::EnumI oEnumI(pBB);
            while (! oEnumI.AtEnd())
            {
                Instruction* pI = oEnumI.Get();
                oEnumI.Next();
                dispatch(pI);
            } // for each insn
        }

        foreach (BBlock::EnumSucc, oEnum, pBB)
        {
            updatePhis(pBB, oEnum.Get());
        } // for each succ

        foreach (BBlock::EnumChild, oEnum, pBB)
        {
            processBBlock(oEnum.Get());
        } // for each child

        // Pop operand stack
        foreach (VarExtList::Enum, oEnum, &oKillList)
        {
            oEnum.Get()->Pop();
        } // for each var

        CLOG(2, "</ol></li>~%");
    } // processBBlock

    // [S]
    private: void setNewName(VarExt* const pVarExt, Operand* const pSx)
    {
        pVarExt->SetFirst(
            new(this) VarExt::OperandList::Cons(
                pSx,
                pVarExt->GetFirst() ) );

        m_pKillList->SetFirst(
            new(this) VarExtList::Cons(
                pVarExt,
                m_pKillList->GetFirst() ) );
    } // setNewName

    // [U]
    private: void updatePhis(BBlock* const pCurr, BBlock* const pSucc)
    {
        foreach (BBlock::EnumI, oEnum, pSucc)
        {
            PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI) break;

            Variable* const pVar = pPhiI->GetWork<Variable>();
            if (NULL == pVar)
            {
                CLOG(2, "<li>Skip ~S</li>~%", pPhiI);
                continue;
            }

            VarExt* const pVarExt = pVar->GetWork<VarExt>();
            if (NULL == pVarExt) continue;

            // Useless PHI instruction. It is created by loop.
            if (pVarExt->IsEmpty()) continue;

            Operand* const pSx = pVarExt->GetTop();

            CLOG(2, "<li><b class='g'>update</b> ~S + [~S ~S]</li>~%",
                pPhiI, pCurr, pSx );

            pPhiI->AddOperand(pCurr, pSx);
        } // for each phi
    } // updatePhis

    ////////////////////////////////////////////////////////////
    //
    // Instruction handlers
    //
    DefProcI(Load)
    {
        VarExt* const pVarExt = getVarExt(pI->GetRx());
        if (NULL == pVarExt) return;

        pI->GetRd()->ReplaceAll(pVarExt->GetTop());
        pI->GetBBlock()->RemoveI(pI);
    } // Load

    DefProcI(Phi)
    {
        Register* const pRd = pI->GetRd();
        if (NULL == pRd) return;

        Variable* const pVar = pRd->GetVar();
        if (NULL == pVar) return;

        VarExt* const pVarExt = pVar->GetWork<VarExt>();
        if (NULL == pVarExt) return;

        if (pI->GetWork<Variable>() == pVar)
        {
            setNewName(pVarExt, pRd);
        }
    } // Phi

    DefProcI(Store)
    {
        VarExt* const pVarExt = getVarExt(pI->GetRx());
        if (NULL == pVarExt) return;

        setNewName(pVarExt, pI->GetSy());
        pI->GetBBlock()->RemoveI(pI);
    } // Store

    DefProcI(VarDef)
    {
        Variable* const pVar = pI->GetSx()->StaticCast<Variable>();

        if (pVar->Extend<VarEx>()->GetFlag() == VarEx::Flag_None)
        {
            return;
        }

        CLOG(3, "<li>~S</li>~%", pI);

        VarExt* const pVarExt = new(this) VarExt;
        pVar->SetWork(pVarExt);

        setNewName(pVarExt, pI->GetSy());
    } // VarDef
}; // SubPassRename

} // Cfg2Ssa

using namespace Cfg2Ssa;

class PassCfg2Ssa :
    public Pass_<PassCfg2Ssa, FunctionPass>
{
    public: static const char* GetName_() { return "Cfg2Ssa"; }

    // Entry point
    protected: override void processFunction(Function* pFun)
    {
        pFun->ComputeDominance();
        SubPassPrepare::Run(pFun);
        SubPassInsertPhi::Run(pFun);
        SubPassRename::Run(pFun);
        pFun->Clean();
    } // Run
}; /// PassCfg2Ssa

DEFPASS(Cfg2Ssa)

} // Compiler
} // TinyCl
