#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - IR - Function
// tinycl_c_ir_fun.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_fun.cpp#16 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

class Static
{
    private: enum Usage
    {
        Usage_Primary,
        Usage_Values,
        Usage_Void,
    }; // Usage

    // [C]
    private: static Usage computeUsage(
        Function* const pFun )
    {
        if (pFun->GetFlavor() == Function::Flavor_Toplevel)
        {
            return Usage_Values;
        }

        if (pFun->HasUseSite())
        {
            return Usage_Values;
        }

        Usage eUsage = Usage_Void;

        foreach (Function::EnumCall, oEnum, pFun)
        {
            Instruction* pCallI = oEnum.Get()->GetI();

            if (NULL != pCallI->GetRd())
            {
                eUsage = Usage_Primary;
            }
            else if (NULL != pCallI->GetVd())
            {
                return Usage_Values;
            }
            else if (Void == pCallI->GetOutput())
            {
                // nothing to do
            }
            else
            {
                // Unsupported output
                return Usage_Values;
            }
        } // for call site

        return eUsage;
    } // computeUsage

    // [R]
    public: static void RewriteToUnreachable(
        Function* const pFun )
    {
        CLOG_SECTION(3, "Unreachable ~S", pFun);

        Function::EnumCall oEnum(pFun);
        while (! oEnum.AtEnd())
        {
            Instruction* const pCallI = oEnum.Get()->GetI();
            oEnum.Next();

            if (pCallI->GetBB() == NULL)
            {
                continue;
            }

            CLOG_SECTION(4, "process ~S", pCallI);

            while (Instruction* pRunnerI = pCallI->GetNext())
            {
                pRunnerI->GetBB()->RemoveI(pRunnerI);
            }

            pCallI->GetBB()->AppendI(new UnreachableI());
        } // while
    } // RewriteToUnrachable

    // [S]
    public: static void SetValueTy(
        Function* const   pFun,
        const Type* const pValueTy )
    {
        pFun->SetFunty(
            new TyFunction(
                pFun->GetFunty()->GetParamTy(),
                pValueTy ) );
    } // SetValueTy

    // [U]
    public: static bool UpdateValueSimply(
        Function* const pFun )
    {
        CLOG_SECTION(2, "Update Value Simply ~S", pFun);

        switch (computeUsage(pFun))
        {
        case Usage_Void:
            CLOG(2, "<li>usage is void</li>");
            return updateToVoid(pFun);

        case Usage_Primary:
            CLOG(2, "<li>usage is primary value</li>");
            return updatePrimary(pFun);

        case Usage_Values:
            CLOG(2, "<li>usage is values</li>");
            break;
        } // computeUsage

        return false;
    } // UpdateValueSimply

    private: static bool updatePrimary(
        Function* const pFun )
    {
        CLOG_SECTION(3, "Primary value ~S", pFun);

        bool fChanged = false;

        foreach (BBlock::EnumPred, oEnum, pFun->GetExitBB())
        {
            BBlock* const pPredBB = oEnum.Get();
            if (RetI* const pRetI = pPredBB->GetLastI()->DynamicCast<RetI>())
            {
                if (Values* pVx = pRetI->GetVx())
                {
                    fChanged = true;
                    Register* const pRx = new Register;
                    pRetI->GetBB()->InsertBeforeI(
                        new SelectI(
                            pVx->GetTy()->GetPrimaryTy(),
                            pRx,
                            pVx,
                            0 ),
                        pRetI );
                    pRetI->GetOperandBox(0)->Replace(pRx);
                }
            }
        } // for

        if (fChanged)
        {
            const Type* pty = tyT;
            foreach (BBlock::EnumPred, oEnum, pFun->GetExitBB())
            {
                BBlock* const pPredBB = oEnum.Get();
                if (RetI* const pRetI =
                        pPredBB->GetLastI()->DynamicCast<RetI>() )
                {
                    pty = Type::And(pty, pRetI->GetSx()->GetTy());
                }
            } // for

            Static::SetValueTy(pFun, pty);
        } // if

        return true;
    } // updatePrimary

    private: static bool updateToVoid(
        Function* const pFun )
    {
        CLOG_SECTION(3, "Void ~S", pFun);

        bool fChanged = false;
        foreach (BBlock::EnumPred, oEnum, pFun->GetExitBB())
        {
            BBlock* const pPredBB = oEnum.Get();
            if (RetI* const pRetI = pPredBB->GetLastI()->DynamicCast<RetI>())
            {
                if (pRetI->GetSx() != Void)
                {
                    fChanged = true;
                    pRetI->GetOperandBox(0)->Replace(Void);
                }
            }
        } // for

        if (fChanged)
        {
            Static::SetValueTy(pFun, tyT);
        }

        return fChanged;
    } // updateToVoid
}; // Static

// ctor
Function::Function(Flavor const eFlavor, Val const name) :
    m_cbFrame(0),
    m_eFlavor(eFlavor),
    m_rgfFlag(0),
    m_pFunty(tyUnknownFunction),
    m_fn(nil),
    m_name(name)
{
    SetIndex(++Context::Get()->m_cFunctions);

    BBlock* const pEntryBB = new BBlock;
    BBlock* const pExitBB  = new BBlock;
    BBlock* const pStartBB = new BBlock;

    LayoutList::Append(pEntryBB);
    LayoutList::Append(pStartBB);
    LayoutList::Append(pExitBB);

    pEntryBB->AppendI(new EntryI );
    pEntryBB->AppendI(new JumpI(pStartBB));
    pStartBB->AppendI(new PrologueI(new Values));
    pExitBB->AppendI(new ExitI);

    pEntryBB->AddEdge(pExitBB)->SetKind(CfgEdge::Kind_Pseudo);

    Context::Get()->GetModule()->Append(this);
} // Function::Function

/// <summary>
///   Cleans function object instructions.
/// </summary>
bool Function::Clean()
{
    if (! Context::Get()->CanContinue())
    {
        return false;
    }


    bool fClean = false;
    int iCount = 0;

    for (;;)
    {
        bool fMore = false;
        iCount += 1;

        WorkList_<BBlock> oUnrechables;
        WorkList_<Instruction> oWorkList;

        CLOG_SECTION(2, "Clean [~D] ~S", iCount, this);

        {
            CLOG_SECTION(2, "Optimize Instructions");

            foreach (Function::EnumBBlock, oEnum, this)
            {
                BBlock* pBBlock = oEnum.Get();

                if (! pBBlock->HasInEdge())
                {
                    if (GetEntryBB() != pBBlock)
                    {
                        oUnrechables.Push(pBBlock);
                        continue;
                    }
                }

                foreach (BBlock::EnumI, oEnum, pBBlock)
                {
                    Instruction* const pI = oEnum.Get();

                    foreach (Instruction::EnumOperand, oEnum, pI)
                    {
                        OperandBox* pBox = oEnum.GetBox();
                        Operand* pSx = pBox->GetOperand()->Compute();
                        if (NULL != pSx && pBox->GetOperand() != pSx)
                        {
                            CLOG(2, "<li>replace ~S by ~S in ~S</li>",
                                pBox->GetOperand(), pSx, pI );

                            pBox->Replace(pSx);
                            fMore = true;
                        }
                    } // for operand

                    if (pI->Optimize())
                    {
                        fMore = true;
                    }

                    oWorkList.Push(pI);
                } // for each insn
            } // for each bblock
        }

        if (! Context::Get()->CanContinue())
        {
            // Error occurred in Instruction::Optimize.
            return false;
        }

        if (! oUnrechables.IsEmpty())
        {
            CLOG_SECTION(2, "Remove unreachable");
            while (! oUnrechables.IsEmpty())
            {
                RemoveBBlock(oUnrechables.Pop());
            } // while
        }

        if (! oWorkList.IsEmpty())
        {
            CLOG_SECTION(2, "Remove useless");

            while (! oWorkList.IsEmpty())
            {
                Instruction* pI = oWorkList.Pop();
                if (NULL == pI->GetBB())
                {
                    CLOG(2, "<li><b class='r'>Already removed</b> ~S</li>",
                        pI );
                    continue;
                }

                if (! pI->IsUseless())
                {
                    continue;
                }

                fClean = true;
                fMore  = true;

                if (SsaOutput* pRd = pI->GetSsaOutput())
                {
                    foreach (SsaOutput::EnumUser, oEnum, pRd)
                    {
                        Instruction* pI = oEnum.Get()->GetI();
                        if (! pI->IsInList())
                        {
                            oWorkList.Push(pI);
                        }
                    } // for each user
                } // if

                pI->GetBB()->RemoveI(pI);
            } // while
        } // if

        if (! fMore)
        {
            break;
        }

        fClean = true;
    } // for

    return fClean;
} // Function::Clean

/// <summary>
///  Eliminates inifnite loop adding pseudo edge from arbitrary block in
///  infinite block to exit block.
/// </summary>
bool Function::EliminateInfiniteLoop()
{
    class DfsWalker
    {
        protected: int m_cVisits;
        private: Function* m_pFun;

        protected: DfsWalker(Function* const pFun) :
            m_cVisits(0),
            m_pFun(pFun)
        {
            resetFlags();
        } // DfsWalker

        public: ~DfsWalker()
        {
            resetFlags();
        } // ~DfsWalker

        // [G]
        public: int GetCount() const
            { return m_cVisits; }

        // [O]
        protected: bool onVisit(BBlock* const pBB)
        {
            if (pBB->GetFlag())
            {
                return true;
            }

            pBB->SetFlag(1);
            m_cVisits += 1;
            return false;
        } // onVisit

        // [R]
        private: void resetFlags()
        {
            foreach (EnumBBlock, oEnum, m_pFun)
            {
                oEnum.Get()->SetFlag(0);
            } // for
        } // resetFlags
    }; // DfsWalker

    class BackwardDfs : public DfsWalker
    {
        public: BackwardDfs(Function* const pFun) :
            DfsWalker(pFun)
        {
            Visit(pFun->GetExitBB());
        } // BackwardDfs

        public: void Visit(BBlock* const pBB)
        {
            if (onVisit(pBB))
            {
                return;
            }

            foreach (BBlock::EnumInEdge, oEnum, pBB)
            {
                Visit(oEnum.GetNode());
            } // for
        } // visit
    }; // BackwardDfs

    class ForewardDfs : public DfsWalker
    {
        private: ForewardDfs(Function* const pFun) :
            DfsWalker(pFun)
        {
            visit(pFun->GetEntryBB());
        } // ForewardDfs

        public: static int Run(Function* const pFun)
        {
            ForewardDfs oForewardDfs(pFun);
            return oForewardDfs.GetCount();
        } // Run

        private: void visit(BBlock* const pBB)
        {
            if (onVisit(pBB))
            {
                return;
            }

            foreach (BBlock::EnumOutEdge, oEnum, pBB)
            {
                visit(oEnum.GetNode());
            } // for
        } // visit
    }; // ForewardDfs

    int const cForwards = ForewardDfs::Run(this);

    BackwardDfs oBackwardDfs(this);

    if (oBackwardDfs.GetCount() == cForwards)
    {
        return false;
    }

    if (oBackwardDfs.GetCount() > cForwards)
    {
        C_INTERNAL_ERROR("There are unreachable blocks.");
        return false;
    }

    CLOG(1, "<li class=b>Eliminate Infinite Loop for ~S<ol>~%", this);

    for (;;)
    {
        CLOG(1, "<li>back/fore=~D/~D</li>~%",
            oBackwardDfs.GetCount(), cForwards );

        if (oBackwardDfs.GetCount() == cForwards)
        {
            break;
        }

        if (oBackwardDfs.GetCount() > cForwards)
        {
            C_INTERNAL_ERROR("There are unreachable blocks.");
            break;
        }

        BBlock* pBB = NULL;
        foreach (EnumBBlockPostorder, oEnum, this)
        {
            if (! oEnum.Get()->GetFlag())
            {
                pBB = oEnum.Get();
                break;
            }
        } // for

        if (NULL == pBB)
        {
            COMPILER_INTERNAL_ERROR();
            break;
        }

        pBB->AddEdge(GetExitBB())->SetKind(CfgEdge::Kind_Pseudo);
        oBackwardDfs.Visit(pBB);
    } // while

    CLOG(1, "</ol></li>~%");

    return true;
} // Function::EliminateInfiniteLoop

BBlock* Function::GetEntryBB() const
{
    return static_cast<const LayoutList*>(this)->GetFirst();
} // Function::GetEntryBB

BBlock* Function::GetExitBB() const
{
    return static_cast<const LayoutList*>(this)->GetLast();
} // Function::GetExitBB

PrologueI* Function::GetPrologueI() const
{
    return GetStartBB()->GetFirstI()->StaticCast<PrologueI>();
} // Function::GetPrologueI

Function* Function::GetSingleCaller() const
{
    Function* pCaller = NULL;
    foreach (Function::EnumInEdge, oEnum, this)
    {
        if (NULL != pCaller) return NULL;
        pCaller = oEnum.GetNode();
    } // for each inedge

    return pCaller;
} // Function::GetSingleCaller

BBlock* Function::GetStartBB() const
{
    return GetEntryBB()->GetLastI()->StaticCast<JumpI>()->
        GetLabel()->GetBB();
} // Function::GetStartBB

Pseudo* Function::FindUpVar(Variable* pVar) const
{
    ASSERT(pVar->GetOwner() != this);

    BBlock* pEntryBB = GetEntryBB();

    foreach (BBlock::EnumI, oEnum, pEntryBB)
    {
        UpVarDefI* pUpVarDefI = oEnum.Get()->DynamicCast<UpVarDefI>();
        if (NULL == pUpVarDefI) continue;
        if (pUpVarDefI->GetSx() == pVar) return pUpVarDefI->GetQd();
    } // for each insn

    return NULL;
} // Function::FindUpVar

/// <summary>
///   Returns true if this function has nonlocal exit point
/// </summary>
bool Function::HasNonlocalExitPoint() const
{
    foreach (EnumFrameReg, oEnum, this)
    {
        FrameReg* const pFd = oEnum.Get();

        Instruction* const pOpenI = pFd->GetDefI();
        if (NULL == pOpenI)
        {
            continue;
        }

        if (pOpenI->Is<OpenExitPointInstruction>())
        {
            if (! pOpenI->IsUseless())
            {
                return true;
            }
        }
    } // for each frame

    return false;
} // Function::HasNonlocalExitPoint

bool Function::HasUpVar() const
{
    EnumUpVar oEnumUpVar(this);
    return ! oEnumUpVar.AtEnd();
} // Function::HasUpvar

bool Function::HasUseSite() const
{
    foreach (EnumUser, oEnum, this)
    {
        if (! oEnum.Get()->GetI()->Is<OpenFinallyI>())
        {
            return true;
        }
    } // for each user

    return false;
} // Function::HasUseSite

void Function::HtmlPrint(Val stream, bool fDef) const
{
    class Local
    {
        public: static void HtmlPrintUsers(
            Val                     stream,
            const char*             psz,
            const Function::Users*  pUsers )
        {
            cformat(stream, "<h4>~A</h4>~%", psz);

            uint k = 0;
            foreach (Function::Users::Enum, oEnum, pUsers)
            {
                if (0 == k)
                {
                    cformat(stream, "<table border='1' cellpadding='5'>~%");
                }

                k += 1;

                Instruction* const pI = oEnum.Get()->GetI();

                BBlock* const pBB = pI->GetBB();

                cformat(stream, "<tr>");
                cformat(stream, "<td>~D</td>", k);
                cformat(stream, "<td>~S</td>", pBB->GetFunction());
                cformat(stream, "<td>~S</td>", pI);

                if (VarDefI* const pVarDefI =
                        pI->DynamicCast<VarDefI>() )
                {
                    const Variable* const pVar =
                        pVarDefI->GetSx()->StaticCast<Variable>();

                    cformat(stream, "<td>~S</td>", pVar->GetTy());
                }
                else if (UpVarDefI* const pUpVarDefI =
                            pI->DynamicCast<UpVarDefI>() )
                {
                    const Variable* const pVar =
                        pUpVarDefI->GetSx()->StaticCast<Variable>();

                    cformat(stream, "<td>~S</td>", pVar->GetTy());
                }

                cformat(stream, "</tr>~%");
            } // for each call

            cformat(stream, k ? "</table>~%" : "<i>None</i>~%");
        } // HtmlPrintUsers
    }; // Local

    static const char* k_rgszFlavor[] =
    {
        "anonymous",
        "finally",
        "named",
        "template",
        "toplevel",
    }; // k_rgszFlavor

    const char* pszFlavor = k_rgszFlavor[m_eFlavor];

    if (Flavor_Finally != m_eFlavor)
    {
        if (IsClosure())
        {
            pszFlavor = "closure";
        }
        else if (HasUpVar())
        {
            pszFlavor = "inner";
        }
    }

    if (! fDef)
    {
        cformat(stream, "<a href='#f~D'>[fn ~A ~W @ ~D]</a>",
            GetIndex(),
            pszFlavor,
            m_name,
            GetIndex() );
        return;
    }

    cformat(stream, "<h3 class='f' id='f~D'>[fn ~A ~W @ ~D]</h3>~%",
            GetIndex(),
            pszFlavor,
            m_name,
            GetIndex() );

    cformat(stream, "<h5>Type</h5>~W~%", GetTy()->Unparse());

    Local::HtmlPrintUsers(stream, "Calls",     &m_oCalls);
    Local::HtmlPrintUsers(stream, "Users",     &m_oUsers);
    Local::HtmlPrintUsers(stream, "Variables", &m_oVarDefs);
    Local::HtmlPrintUsers(stream, "UpVars",    &m_oUpVarDefs);

    // Call Graph
    cformat(stream, "<h5>Call Edges</h5>~%");
    cformat(stream, "<table border='1'>~%");
    {
        cformat(stream, "<tr><th>In</th><td><ol>");
        foreach (Function::EnumInEdge, oEnum, this)
        {
            cformat(stream, "<li>~S</li>", oEnum.Get()->GetFrom());
        } // for each edge
        cformat(stream, "</ol></td></tr>~%");
    }
    {
        cformat(stream, "<tr><th>Out</th><td><ol>");
        foreach (Function::EnumOutEdge, oEnum, this)
        {
            cformat(stream, "<li>~S</li>", oEnum.Get()->GetTo());
        } // for each edge
        cformat(stream, "</ol></td></tr>~%");
    }
    cformat(stream, "</table>~%");

    // Frames
    {
        cformat(stream, "<h4>Frames</h4>~%");
        uint cFrames = 0;
        foreach (EnumFrameReg, oEnum, this)
        {
            if (0 == cFrames)
            {
                cformat(stream, "<table border='1' cellpadding='5'>~%");
            }
            cFrames += 1;
            FrameReg* pFd = oEnum.Get();
            cformat(stream, "<tr>");
            cformat(stream, "<td>~D</td>", cFrames);
            cformat(stream, "<td>~S</td>", pFd);
            cformat(stream, "<td>~S</td>", pFd->GetDefI());
            cformat(stream, "</tr>~%");
        } // for each frame
        if (0 == cFrames)
        {
            cformat(stream, "<i>None</i><br/>~%");
        }
        else
        {
            cformat(stream, "</table>~%");
        }
    }

    // BBlocks
    format(stream, "<h4>BBlocks</h4>~%");
    foreach (EnumBBlock, oEnum, this)
    {
        BBlock* pBBlock = oEnum.Get();
        pBBlock->HtmlPrint(stream, true);
    } // for each bblock
} // Function::HtmlPrint

Pseudo* Function::InternUpVar(Variable* pVar)
{
    ASSERT(pVar->GetOwner() != this);

    if (Pseudo* const pQd = FindUpVar(pVar))
    {
        return pQd;
    }

    Pseudo* const pQd = new Pseudo;

    BBlock* const pEntryBB = GetEntryBB();

    pEntryBB->InsertBeforeI(
        new UpVarDefI(pQd, pVar),
        pEntryBB->GetLastI() );

    return pQd;
} // Function::InternUpVar

/// <summary>
///  Returns true if this function is closure.
/// </summary>
bool Function::IsClosure() const
{
    if (! HasUpVar())
    {
        return false;
    }

    foreach (EnumUser, oEnum, this)
    {
        if (! oEnum.Get()->GetI()->Is<OpenFinallyI>())
        {
            return true;
        }
    } // for each user

    return false;
} // Function::IsClosure

/// <summary>
///  Returns true if this function can't be closure. Even if this method
///  returns false, this function can't be closure.
/// </summary>
bool Function::IsNotClosure() const
{
    foreach (EnumUser, oEnum, this)
    {
        if (! oEnum.Get()->GetI()->Is<OpenFinallyI>())
        {
            return true;
        }
    } // for each user

    if (HasUpVar())
    {
        return false;
    }

    EnumOutEdge oEnumOutEdge(this);
    return oEnumOutEdge.AtEnd();
} // Function::IsClosure

bool Function::IsStackRestify() const
{
    return GetPrologueI()->GetLy() == QBstack_restify;
} // Function::IsStackRestify

void Function::MakeVoid()
{
    Function::EnumBBlock  oEnum(this);
    while (! oEnum.AtEnd())
    {
        BBlock* pBB = oEnum.Get();
        oEnum.Next();
        RemoveBBlock(pBB);
    } // while
} // Function::MakeVoid

BBlock* Function::MoveBBlock(BBlock* const pBB, BBlock* const pRefBB)
{
    ASSERT(pBB != pRefBB);
    CLOG(2, "<li>move ~S before ~S</li>~%", pBB, pRefBB);
    static_cast<LayoutList*>(this)->Delete(pBB);
    static_cast<LayoutList*>(this)->InsertBefore(pBB, pRefBB);
    return pBB;
} // Function::MoveBBlock

bool Function::NeedArity() const
{
    return nil != GetPrologueI()->GetLx();
} // Function::NeedArity

void Function::NumberInstructions()
{
    int iIndex = 0;
    ComputeDominance();
    foreach (EnumBBlock, oEnum, this)
    {
        BBlock* pBB = oEnum.Get();
        foreach (BBlock::EnumI, oEnum, pBB)
        {
            iIndex += 1;
            oEnum.Get()->SetIndex(iIndex);
        } // for each insn
    } // for each bblock
} // Function::NumberInstructions

void Function::Realize(OperandBox* const pBox)
{
    Instruction* const pUserI = pBox->GetI();
    if (pUserI->Is<UpVarBaseI>())
    {
        return;
    }

    if (pUserI->Is<CallI>())
    {
        m_oCalls.Append(pBox);
    }
    else
    {
        m_oUsers.Append(pBox);
    }

    Function* const pCaller = pBox->GetI()->GetBB()->GetFunction();
    CgEdge* pEdge = pCaller->FindEdgeTo(this);
    if (NULL == pEdge)
    {
        pEdge = pCaller->AddEdge(this);
    }

    pEdge->m_cUsers += 1;
} // Function::Realize

void Function::RemoveBBlock(BBlock* const pBBlock)
{
    CLOG(2, "<li><b class='r'>remove</b> ~S</li>~%", pBBlock);

    while (Instruction* pI = pBBlock->GetFirstI())
    {
        pBBlock->RemoveI(pI);
    }

    RemoveNode(pBBlock);
} // Function::RemoveBBlock

void Function::Unrealize(OperandBox* const pBox)
{
    Instruction* const pUserI = pBox->GetI();
    if (pUserI->Is<UpVarBaseI>())
    {
        return;
    }

    if (pUserI->Is<CallI>())
    {
        m_oCalls.Delete(pBox);
    }
    else
    {
        m_oUsers.Delete(pBox);
    }

    Function* const pCaller = pBox->GetI()->GetBB()->GetFunction();
    CgEdge*   const pEdge   = pCaller->FindEdgeTo(this);

    if (NULL == pEdge)
    {
        CLOG(1, "<li class='e'>Can't find edge from ~S to ~S</li>",
            pCaller, this );

        COMPILER_INTERNAL_ERROR();
        return;
    }

    pEdge->m_cUsers -= 1;
    if (0 == pEdge->m_cUsers)
    {
        pCaller->RemoveEdge(pEdge);
    }
} // Function::Unrealize

bool Function::UpdateValueTy()
{
    CLOG_SECTION(2, "Update value type of ~S", this);

    const Type* pty = tyNil;
    foreach (BBlock::EnumPred, oEnum, GetExitBB())
    {
        if (RetI* const pRetI = oEnum.Get()->GetLastI()->DynamicCast<RetI>())
        {
            const Type* ptyRet = pRetI->GetSx()->GetTy();
            pty = Type::Or(pty, ptyRet);
            CLOG(2, "<li>~S ~S => ~S</li>", pRetI, ptyRet, pty);
        }
    } // for

    if (GetFunty()->GetValueTy()->Equal(pty))
    {
        CLOG(2, "<li>Not changed</li>");
        return false;
    }

    CLOG(2, "<li><b class='r'>Changed</b> ~S</li>", pty);

    if (pty == tyNil)
    {
        Static::RewriteToUnreachable(this);
    }

    Static::SetValueTy(this, pty);
    return true;
} // Function::UpdateValueTy

} // Compiler
} // TinyCl
