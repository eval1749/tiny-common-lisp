#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Stack Slot Allocation
// compiler/tinycl_c_cg_stack.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_stack.cpp#6 $
//
#include "./tinycl_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

namespace
{

typedef WorkList_<FrameReg> FrameRegs;

struct FrameExt : LocalObject
{
    FrameRegs   m_oChildFrames;
}; // FrameExt

class SubPassFrame :
    public Pass_<SubPassFrame, SubPass>,
    protected Mm
{
    public: static const char* GetName_() { return "Frame"; }

    private: Function*  m_pFun;
    private: FrameReg*  m_pRoot;

    public: static void Run(Function* pFun)
    {
        SubPassFrame oPass(pFun);
        oPass.run();
    } // Run

    private: SubPassFrame(Function* pFun) :
        m_pFun(pFun),
        m_pRoot(new FrameReg(NULL, nil))
    {
        m_pRoot->SetWork(new(this) FrameExt);
    } // SubPassFrame

    private: void run()
    {
        CLOG(2, "<h3>Compute Frame for ~S, size=~D</h3>",
            m_pFun, m_pFun->m_cbFrame );

        constructTree();

        CLOG_SECTION(2, "<h3>Assign</h3>");
        assign(m_pRoot, m_pFun->m_cbFrame);
        CLOG(2, "<li><b>Frame size = ~D</b></li>", m_pFun->m_cbFrame);
    } // run

    // [A]
    private: void assign(FrameReg* pFd, uint ofs)
    {
        pFd->SetLocation(ofs);
        CLOG(2, "<li>~S ~D</li>~%", pFd, ofs);

        ofs += Context::Get()->GetTarget()->ComputeFrameSize(pFd);

        m_pFun->m_cbFrame = max(m_pFun->m_cbFrame, ofs);

        foreach (
            FrameRegs::Enum,
            oEnum,
            &pFd->GetWork<FrameExt>()->m_oChildFrames )
        {
            assign(oEnum.Get(), ofs);
        } // for each child

        pFd->GetWork<FrameExt>()->m_oChildFrames.MakeEmpty();
    } // assign

    // [C]
    private: void constructTree()
    {
        CLOG_SECTION(2, "<h3>Construct Frame Trees</h3>");
        foreach (Function::EnumFrameReg, oEnum, m_pFun)
        {
            FrameReg* pFd = oEnum.Get();
            if (NULL != pFd)
            {
                pFd->SetWork(new(this) FrameExt);
                setParent(pFd);
            }
        } // for each frame
    } // constructTree

    // [S]
    private: void setParent(FrameReg* pFd)
    {
        FrameReg* pParent = m_pRoot;
        for (
            FrameReg* pRunner = pFd->GetOuter();
            NULL != pRunner;
            pRunner = pRunner->GetOuter() )
        {
            if (Instruction* pDefI = pRunner->GetDefI())
            {
                if (pDefI->GetBB()->GetFunction() == m_pFun)
                {
                    pParent = pRunner;
                }
                break;
            }
        } // for each outer

        CLOG(2, "<li>~S parent=~S</li>~%", pFd, pParent);
        FrameExt* p = pParent->GetWork<FrameExt>();
        p->m_oChildFrames.Push(pFd);
    } // setParent
}; // SubPassFrame

} // namespace

/// <summary>
///   Stack Location Assignment pass.
/// </summary>
class PassStack :
    public Pass_<PassStack, FunctionPass>
{
    public: static const char* GetName_() { return "Stack"; }

    private: int        m_cbVar;
    private: Function*  m_pFun;

    // Entry Point
    private: virtual void processFunction(Function* pFun) override
    {
        m_cbVar = Context::Get()->GetTarget()->GetGprGroup()->m_cbWidth;
        m_pFun  = pFun;

        CLOG_SECTION(1, "<h3>process ~S stack=~D</h3>",
            pFun, pFun->m_cbFrame );

        {
            CLOG_SECTION(1, "<h4>Stack Variables in ~S</h4>", pFun);
            Function::EnumVar  oEnumVar(pFun);
            while (! oEnumVar.AtEnd())
            {
                Variable* const pVar = oEnumVar.Get();
                oEnumVar.Next();

                CLOG_SECTION(1, "<li>~S</li>", pVar);

                switch (pVar->GetStorage())
                {
                case Variable::Storage_Closed:
                case Variable::Storage_Literal:
                    if (VarHomeI* pVarHomeI = findHome(pVar))
                    {
                        // RA doesn't set VarHome with spill slot of %sy.
                        // So, we allocate new stack slot.
                        if (VarHome* const pHd = pVarHomeI->GetOutput()->
                                DynamicCast<VarHome>() )
                        {
                            StackSlot* const pMd = new StackSlot(
                                RegClass_Gpr,
                                assign(),
                                pVar );

                            pVarHomeI->SetOutput(pMd);
                        } // if VarHome

                        if (StackSlot* const pMd = pVarHomeI->GetOutput()->
                                DynamicCast<StackSlot>() )
                        {
                            int const ofs = pMd->GetLocation();
                            pVar->SetLocation(ofs);

                            CLOG(1, "<li>ofs=~D</li>", ofs);

                            pVarHomeI->GetBB()->ReplaceI(
                                new CopyI(pMd, pVarHomeI->GetSy()),
                                pVarHomeI );
                        }
                        else
                        {
                            COMPILER_INTERNAL_ERROR();
                        }
                    } // if
                    break;

                case Variable::Storage_Stack:
                    replaceStackVar(pVar);
                    break;
                    
                default:
                    COMPILER_INTERNAL_ERROR();
                    break;
                } // switch storage

                // For further optimization
                m_pFun->GetEntryBB()->MoveBeforeI(
                    pVar->GetDefI(),
                    m_pFun->GetEntryBB()->GetLastI() );
            } // for each var
        }

        if (pFun->IsClosure())
        {
            CLOG_SECTION(1, "<h4>Closed Variables in ~S</h4>", pFun);

            foreach (Function::EnumUpVar, oEnum, pFun)
            {
                Pseudo* const pQd = oEnum.GetI()->GetQd();
                foreach (Pseudo::EnumUser, oEnum, pQd)
                {
                    if (StackSlot* const pMd = oEnum.GetI()->GetOutput()->
                            DynamicCast<StackSlot>() )
                    {
                        if (oEnum.GetI()->Is<VarAnnexI>())
                        {
                            pMd->SetLocation(assign());
                        }
                        else
                        {
                            COMPILER_INTERNAL_ERROR();
                        }
                        break;
                    }
                } // for each user
            } // for each fun
        } // if closure

        SubPassFrame::Run(m_pFun);

        m_pFun->Clean();
    } // processFunction

    // [A]
    private: int assign()
    {
        int iLoc = m_pFun->m_cbFrame;
        m_pFun->m_cbFrame += m_cbVar;
        return iLoc;
    } // assign

    // [F]
    private: static VarHomeI* findHome(Variable* const pVar)
    {
        Instruction* pDefI = pVar->GetDefI();
        if (NULL == pDefI)
        {
            return NULL;
        }

        Pseudo* const pQd = pDefI->GetQd();
        if (NULL == pQd)
        {
            return NULL;
        }

        foreach (Pseudo::EnumUser, oEnum, pQd)
        {
            if (VarHomeI* pVarHomeI = oEnum.GetI()->DynamicCast<VarHomeI>())
            {
                return pVarHomeI;
            }
        } // for user

        COMPILER_INTERNAL_ERROR();
        return NULL;
    } // findHome

    // [V]
    /// <summary>
    ///  Replace variable reference with stack slot reference.
    /// </summary>
    private: void replaceStackVar(
        Variable* const pVar )
    {
        int const ofs = assign();
        CLOG(1, "<li>ofs=~D</li>", ofs);
        pVar->SetLocation(ofs);

        StackSlot* const pMd = new StackSlot(
            RegClass_Gpr,
            pVar->GetLocation(),
            pVar );

        Instruction* const pVarDefI = pVar->GetDefI();

        foreach (Pseudo::EnumUser, oEnum, pVarDefI->GetQd())
        {
            Instruction* pI = oEnum.GetI();

            if (VarHomeI* pVarHomeI = pI->DynamicCast<VarHomeI>())
            {
                pVarHomeI->GetBB()->ReplaceI(
                    new CopyI(pMd, pVarHomeI->GetSy()),
                    pVarHomeI );
            }
            else
            if (SlotI* const pSlotI = pI->DynamicCast<SlotI>())
            {
                Pseudo::EnumUser oEnumUser(pSlotI->GetQd());
                while (! oEnumUser.AtEnd())
                {
                    Instruction* const pI = oEnumUser.GetI();
                    oEnumUser.Next();

                    if (LoadI* const pLoadI =
                            pI->DynamicCast<LoadI>() )
                    {
                        pLoadI->GetBB()->ReplaceI(
                            new CopyI(pLoadI->GetOutput(), pMd),
                            pI );
                    }
                    else if (StoreI* const pStoreI =
                                pI->DynamicCast<StoreI>() )
                    {
                        pStoreI->GetBB()->ReplaceI(
                            new CopyI(pMd, pStoreI->GetSy()),
                            pI );
                    }
                    else
                    {
                        COMPILER_INTERNAL_ERROR();
                    }
                } // for each user
            }
            else
            {
                COMPILER_INTERNAL_ERROR();
            }
        } // for each slot
    } // replaceStackVar
}; // PassStack

DEFPASS(Stack)

} // Compiler

} // TinyCl
