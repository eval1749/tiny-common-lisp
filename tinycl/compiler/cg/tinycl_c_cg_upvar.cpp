#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - UpVar
// compiler/tinycl_c_cg_upvar.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_upvar.cpp#6 $
//
//
// Note:
//  We must remove unreachable functions call upvar users.
//
#define USE_STACK_CELL  0
#include "./tinycl_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

namespace CgUpVar
{

/// <summary>
///   Access methods of UpVar.
/// </summary>
enum Access
{
    Access_None,
                            // User     Storage     Path
    Access_Base,            // Inner    Stack       Variable
    Access_CellMarker,      // Closure  Closed      Closure
    Access_ClosedCell,      // Inner    Closed      Mixed
    Access_LiteralMarker,   // Closure  Literal     Closure
    Access_RegRo,           // Inner    Literal     Mixed
    Access_RegRw,           // NYI
    Access_Sp,              // Inner    Stack       Fixed
    Access_StackCell,       // Inner    Stack       Variable
}; // Access

static const char* k_rgpszAccess[] =
{
    "None",
    "Base",
    "CellMarker",
    "ClosedCell",
    "LiteralMarker",
    "RegRo",
    "RegRw",
    "Sp",
    "StackCell",
}; // k_rgpszAccess

enum Path
{
    Path_None,

    Path_Fixed,
    Path_Mixed,
    Path_Variable,

    Path_Limit,
}; // Path

static const char* k_rgpszPath[] =
{
    "None",
    "Fixed",
    "Mixed",
    "Variable",
}; // k_rgpszPath

/// <summary>
///  Represents up-level variable.
/// </summary>
//  m_pPlace    -- Where this variable comes from, home or annex.
class UpVar :
    public DoubleLinkedItem_<UpVar>,
    public LocalObject,
    public WorkListItem_<UpVar>
{
    private: Access     m_eAccess;
    private: Path       m_ePath;
    private: UpVarDefI* m_pDefI;
    private: Function*  m_pPlace;
    private: Output*    m_pRb;
    private: Output  *  m_pRx;

    public: UpVar(
        UpVarDefI* const pDefI,
        Function*  const pPlace,
        Path       const e ) :
            m_eAccess(Access_None),
            m_ePath(e),
            m_pPlace(pPlace),
            m_pDefI(pDefI),
            m_pRb(NULL),
            m_pRx(NULL) {}

    // [G]
    public: Access     GetAccess() const { return m_eAccess; }
    public: UpVarDefI* GetDefI()   const { return m_pDefI; }
    public: Path       GetPath()   const { return m_ePath; }
    public: Function*  GetPlace()  const { return m_pPlace; }
    public: Output*    GetRb()     const { return m_pRb; }
    public: Output*    GetRx()     const { return m_pRx; }

    public: Variable* GetVar()   const
        { return m_pDefI->GetSx()->StaticCast<Variable>(); }

    // [S]
    public: Access  SetAccess(Access e) { return m_eAccess = e; }
    public: Output* SetRb(Output* p)  { return m_pRb = p; }
    public: Output* SetRx(Output* p)  { return m_pRx = p; }
}; // UpVar

typedef DoubleLinkedList_<UpVar> UpVars;

/// <summary>
///   A work area for IR Function.
/// </summary>
class FunWork :
    public LocalObject,
    public DoubleLinkedList_<UpVar>
{
    public: Register* m_pRb;

    // ctor
    public: FunWork() :
        m_pRb(NULL) {}

    // [F]
    public: UpVar* FindUpVar(Variable* const pVar) const
    {
        foreach (UpVars::Enum, oEnum, this)
        {
            if (oEnum.Get()->GetVar() == pVar)
            {
                return oEnum.Get();
            }
        } // for each upvar

        // Note: We must remove unreachable functions call upvar users.
        CLOG(1, "<div class='e'>Can't find upvar ~S in ~S</div>",
            pVar );

        COMPILER_INTERNAL_ERROR();
        return NULL;
    } // FindUpVar
}; // FunWork

/// <summary>
///   Represents Extended IR Function for easy access to FunWork.
/// </summary>
class FunEx : public Function
{
    // [A]
    public: void AddExtraParam(Register* const pRx)
    {
        Instruction* const pPrologueI = GetPrologueI();
        foreach (Values::EnumUser, oEnum, pPrologueI->GetVd())
        {
            Instruction* const pI = oEnum.GetI();
            if (pI->Is<SelectI>())
            {
                pI->GetOperandBox(1)->Replace(
                    Integer::New(pI->GetIy() + 1) );
            }
            else if (pI->Is<CountI>())
            {
                foreach (Register::EnumUser, oEnum, pI->GetRd())
                {
                    Instruction* const pI = oEnum.GetI();
                    if (isCompareI(pI)) {
                        pI->GetOperandBox(1)->Replace(
                            Literal::New(add(pI->GetLy(), 1)) );
                    }
                } // for each user of COUNT
            }
        } // for each user of PROLOGUE

        pPrologueI->GetBBlock()->InsertAfterI(
            new SelectI(tyT, pRx, pPrologueI->GetVd(), 0),
            pPrologueI );
    } // AddExtraParam

    // [I]
    private: static bool isCompareI(Instruction* const pI)
    {
        if (pI->Is<GtI>())
        {
            return true;
        }

        if (pI->Is<NeI>())
        {
            return true;
        }

        return false;
    } // isCompareI

    public: bool IsStackRestify() const
    {
        return Qdynamic_extent == GetPrologueI()->GetLy();
    } // isStackRestify
}; // FunEx

/// <summary>
///   Represents Extended IR Variable.
/// </summary>
class VarEx : public Variable
{
    // [H]
    public: bool HasHome() const
    {
        foreach (Register::EnumUser, oEnum, GetDefI()->GetRd())
        {
            if (oEnum.GetI()->Is<VarHomeI>())
            {
                return true;
            }
        } // for use
        return false;
    } // HasHome

    private: static bool hasStore(SsaOutput* const pSd)
    {
        foreach (Register::EnumUser, oEnum, pSd)
        {
            SlotI* const pSlotI = oEnum.GetI()->StaticCast<SlotI>();
            foreach (Register::EnumUser, oEnum, pSlotI->GetRd())
            {
                if (oEnum.GetI()->Is<StoreI>())
                {
                    return true;
                }
            } // for each user of SLOT
        } // for each user of VARDEF

        return false;
    } // HasStore

    // [I]
    public: bool IsReadOnly() const
    {
        if (hasStore(GetRd()))
        {
            return true;
        }

        foreach (Module::EnumFunction, oEnum, Context::Get()->GetModule())
        {
            Function* const pFun = oEnum.Get();
            foreach (Function::EnumUpVar, oEnum, pFun)
            {
                if (oEnum.Get() != this)
                {
                    continue;
                }

                if (hasStore(oEnum.GetI()->GetSsaOutput()))
                {
                    return true;
                }
            } // for each upvar
        } // for each fun

        return false;
    } // IsReadOnly
}; // VarEx

/// <summary>
///   Pass for computing access path.
/// </summary>
class SubPassAccess :
    public Pass_<SubPassAccess, SubPass>
{
    public: static const char* GetName_() { return "Access"; }

    // ctor
    private: SubPassAccess() {}

    // Entry Point
    public: static void Run(Function* const pFun)
    {
        SubPassAccess oSubPass;
        oSubPass.run(pFun->Extend<FunEx>());
    } // Run

    // [D]
    private: void determineAccess(FunEx* const pFun)
    {
        FunWork* const pFunWork = pFun->GetWork<FunWork>();

        if (pFun->IsClosure())
        {
            foreach (UpVars::Enum, oEnum, pFunWork)
            {
                UpVar* const pUpVar = oEnum.Get();
                Access eAccess;
                switch (pUpVar->GetVar()->GetStorage())
                {
                case Variable::Storage_Closed:
                case Variable::Storage_Stack:
                    eAccess = Access_CellMarker;
                    break;

                case Variable::Storage_Literal:
                    eAccess = Access_LiteralMarker;
                    break;

                default:
                    COMPILER_INTERNAL_ERROR();
                    eAccess = Access_None;
                    break;
                } // switch storage

                pUpVar->SetAccess(eAccess);
            } // for each upvar
        }
        else
        {
            WorkList_<UpVar> oBases;
            uint cBases = 0;
            uint cExtra = 0;
            foreach (UpVars::Enum, oEnum, pFunWork)
            {
                UpVar* const pUpVar = oEnum.Get();
                switch (pUpVar->GetPath())
                {
                case Path_Fixed:
                    pUpVar->SetAccess(Access_Sp);
                    break;

                case Path_Mixed:
                    switch (pUpVar->GetVar()->GetStorage())
                    {
                    case Variable::Storage_Closed:
                        pUpVar->SetAccess(Access_ClosedCell);
                        break;

                    case Variable::Storage_Literal:
                        pUpVar->SetAccess(Access_RegRo);
                        break;

                    default:
                        COMPILER_INTERNAL_ERROR();
                        break;
                    } // switch storage
                    cExtra+= 1;
                    break;

                case Path_Variable:
                    oBases.Push(pUpVar);
                    pUpVar->SetAccess(Access_Base);
                    cBases += 1;
                    break;

                default:
                    COMPILER_INTERNAL_ERROR();
                    break;
                } // switch path
            } // for each upvar

            #if USE_STACK_CELL
                if (1 == cBases)
                {
                    // We use stack cell method instead of base method, since
                    // both method use one register but stack cell method
                    // doesn't need offset.
                    UpVar* const pUpVar = oBases.Pop();
                    pUpVar->SetAccess(Access_StackCell);
                } // if
            #endif
        } // if
    } // determineAccess

    private: void dumpAccess(FunEx* const pFun)
    {
        if (nil == Context::Get()->GetPass()->GetStream())
        {
            return;
        }

        FunWork* const pFunWork = pFun->GetWork<FunWork>();

        CLOG(1, "<h3>~S</h3>~%", pFun);

        if (NULL == pFunWork->GetFirst())
        {
            CLOG(1, "<i>No upvar</i><br/>");
            return;
        }

        CLOG(1, "<table border='1' cellpadding='2' cellspacing='2'>~%");
        CLOG(1,
            "<tr>"
                "<th>Var</th><th>Place</th><th>Path</th><th>Access</th>"
            "</tr>~%" );

        foreach (UpVars::Enum, oEnum, pFunWork)
        {
            UpVar* pUpVar = oEnum.Get();
            CLOG(1, "<tr>");
            CLOG(1, "<td>~S</td>", pUpVar->GetVar());
            CLOG(1, "<td>~S</td>", pUpVar->GetPlace());
            CLOG(1, "<td>~A</td>", k_rgpszPath[pUpVar->GetPath()]);
            CLOG(1, "<td>~A</td>", k_rgpszAccess[pUpVar->GetAccess()]);
            CLOG(1, "</tr>~%");
        } // for each upvar

        CLOG(1, "</table>~%");
    } // dumpAccess

    // [R]
    private: void run(FunEx* const pFun)
    {
        determineAccess(pFun);
        dumpAccess(pFun);
    } // run
}; // SubPassAccess

/// <summary>
///   Pass for updating callees
/// </summary>
class SubPassUpdateCallee :
    public Pass_<SubPassUpdateCallee, SubPass>
{
    public: static const char* GetName_() { return "UpdateCallee"; }

    // ctor
    private: SubPassUpdateCallee() {}

    // Entry Point
    public: static void Run(Function* const pFun)
    {
        SubPassUpdateCallee oSubPass;
        oSubPass.run(pFun->Extend<FunEx>());
    } // Run

    // [R]
    private: void run(FunEx* const pFun)
    {
        CLOG_SECTION(1, "Update callee ~S", pFun);

        uint nClosed = 0;
        WorkList_<Function> oBases;
        Physical* pRsp = NULL;

        Val const ty = list(Qvalues);
        Val tail = ty;

        foreach (UpVars::Enum, oEnum, pFun->GetWork<FunWork>())
        {
            UpVar* const pUpVar = oEnum.Get();

            CLOG_SECTION(1, "process ~S ~A",
                pUpVar->GetVar(), k_rgpszAccess[pUpVar->GetAccess()] );

            switch (pUpVar->GetAccess())
            {
            case Access_Base:
            {
                Function* pPlace = pUpVar->GetPlace();

                Register* pRb;
                if (pPlace->IsInList())
                {
                    pRb = pPlace->GetWork<FunWork>()->m_pRb;
                }
                else
                {
                    oBases.Push(pPlace);

                    pRb = new Register;
                    pFun->AddExtraParam(pRb);
                    pPlace->GetWork<FunWork>()->m_pRb = pRb;
                    tail = setf_cdr(list(t), tail);
                }

                pUpVar->SetRb(pRb);
                updateBaseRefs(pUpVar, pRb);
                break;
            } // Access_Base

            case Access_CellMarker:
            {
                ClosedMarker* const pCx = new ClosedMarker(nClosed);
                nClosed += 1;

                pUpVar->SetRx(pCx);

                pUpVar->GetDefI()->GetQd()->ReplaceAll(pCx);
                pUpVar->GetDefI()->SetOutput(pCx);
                break;
            } // Access_CellMarker

            case Access_ClosedCell:
            case Access_StackCell:
            {
                Register* const pRcell = new Register;
                pFun->AddExtraParam(pRcell);
                tail = setf_cdr(list(t), tail);

                pUpVar->SetRx(pRcell);
                pUpVar->GetDefI()->GetQd()->ReplaceAll(pRcell);
                pUpVar->GetDefI()->SetOutput(Void);
                break;
            } // Access_ClosedCell

            case Access_LiteralMarker:
            {
                ClosedMarker* const pCx = new ClosedMarker(nClosed);
                nClosed += 1;

                pUpVar->SetRx(pCx);

                foreach (Pseudo::EnumUser, oEnum, pUpVar->GetDefI()->GetQd())
                {
                    SlotI* const pSlotI = oEnum.GetI()->DynamicCast<SlotI>();
                    if (NULL == pSlotI)
                    {
                        COMPILER_INTERNAL_ERROR();
                        continue;
                    }

                    Register* const pRd = pSlotI->GetRd();
                    Register::EnumUser oEnumUser(pRd);
                    while (! oEnumUser.AtEnd())
                    {
                        LoadI* pLoadI = oEnumUser.GetI()->
                            DynamicCast<LoadI>();

                        oEnumUser.Next();

                        if (NULL != pLoadI)
                        {
                            pLoadI->GetRd()->ReplaceAll(pCx);
                        }
                        else
                        {
                            COMPILER_INTERNAL_ERROR();
                        }
                    } // for each LOAD
                } // for each SLOT

                pUpVar->GetDefI()->SetOutput(pCx);
                break;
            } // Access_LiteralMarker

            case Access_RegRo:
            {
                Register* const pRx = new Register;
                pFun->AddExtraParam(pRx);
                tail = setf_cdr(list(t), tail);

                updateRegRefs(pUpVar, pRx);
                pUpVar->SetRx(pRx);
                pUpVar->GetDefI()->SetOutput(Void);
                break;
            } // Access_RegRo

            case Access_Sp:
            {
                if (NULL == pRsp)
                {
                    Target* const pTarget = Context::Get()->GetTarget();
                    pRsp = pTarget->GetPhysical(
                        pTarget->GetGprGroup()->m_pRsp );
                }

                pUpVar->SetRb(pRsp);
                updateBaseRefs(pUpVar, pRsp);
                break;
            } // Access_Sp

            default:
                COMPILER_INTERNAL_ERROR();
                break;
            } // switch access
        } // for each upvar

        if (nil != cdr(ty))
        {
            PrologueI* const pPrologueI = pFun->GetPrologueI();
            Val const origty = pPrologueI->GetTy()->Unparse();
            if (consp(origty) && Qvalues == car(origty))
            {
                setf_cdr(cdr(origty), tail);
            }
            else
            {
                setf_cdr(list(origty), tail);
            }

            CLOG(1, "<li>Update ~S to ~W</li>", pPrologueI, ty);

            pPrologueI->SetTy(Type::Parse(ty));
        } // if
    } // run

    // [U]
    private: void updateBaseRefs(
        UpVar*  const pUpVar,
        Output* const pRb)
    {
        Pseudo* const pQx = pUpVar->GetDefI()->GetQd();
        Pseudo::EnumUser oEnumSlot(pQx);
        while (! oEnumSlot.AtEnd())
        {
            Instruction* const pI = oEnumSlot.GetI();
            oEnumSlot.Next();

            if (SlotI* const pSlotI = pI->DynamicCast<SlotI>())
            {
                switch (pUpVar->GetVar()->GetStorage())
                {
                case Variable::Storage_Closed:
                {
                    Register* pR2 = new Register;
                    pSlotI->GetBB()->InsertBeforeI(
                        new UpVarRefI(pR2, pRb, pQx),
                        pSlotI );

                    Register* pR3 = new Register;
                    pSlotI->GetBB()->InsertBeforeI(
                        new LoadI(tyClosedCell, pR3, pR2),
                        pSlotI );

                    pSlotI->GetOperandBox(2)->Replace(pR3);
                    break;
                } // closed

                case Variable::Storage_Literal:
                case Variable::Storage_Stack:
                    pSlotI->GetBB()->ReplaceI(
                        new UpVarRefI(pSlotI->GetRd(), pRb, pQx),
                        pSlotI );
                    break;

                default:
                    COMPILER_INTERNAL_ERROR();
                    break;
                } // switch storage
            }
            else
            {
                if (! pI->Is<UpVarRefI>())
                {
                    COMPILER_INTERNAL_ERROR();
                }
            }
        } // for each SLOT
    } // updateBaseRefs

    private: void updateRegRefs(
        UpVar*    const pUpVar,
        Register* const pRx )
    {
        foreach (Register::EnumUser, oEnum, pUpVar->GetDefI()->GetQd())
        {
            Instruction* const pUserI = oEnum.GetI();
            foreach (Register::EnumUser, oEnum, pUserI->GetRd())
            {
                Instruction* const pI = oEnum.GetI();
                if (pI->Is<LoadI>())
                {
                    pI->GetRd()->ReplaceAll(pRx);
                }
            } // for each user of SLOT
        } // for each user of UPVARDEF
    } // updateRegRefs
}; // SubPassUpdateCallee

/// <summary>
///   Update call site sub pass.
/// </summary>
class SubPassUpdateCallSite :
    public Pass_<SubPassUpdateCallSite, SubPass>
{
    public: static const char* GetName_() { return "UpdateCallSite"; }

    // ctor
    private: SubPassUpdateCallSite() {}

    // Entry Point
    public: static void Run(Function* const pFun)
    {
        SubPassUpdateCallSite oSubPass;
        oSubPass.run(pFun->Extend<FunEx>());
    } // Run

    // [G]
    private: static Physical* getSp()
    {
        Target* pTarget = Context::Get()->GetTarget();
        return pTarget->GetPhysical(pTarget->GetGprGroup()->m_pRsp);
    } // getSp

    // [I]
    private: static Register* insertLoad(
        Register*    const pRp,
        Instruction* const pRefI)
    {
        Register* const pRx = new Register;
        pRefI->GetBB()->InsertBeforeI(new LoadI(pRx, pRp), pRefI);
        return pRx;
    } // insertLoad

    private: static void internHome(Variable* const pVar)
    {
        Instruction* const pVarDefI = pVar->GetDefI();

        if (! pVarDefI->GetNext()->Is<VarHomeI>())
        {
            pVarDefI->GetBB()->InsertAfterI(
                new VarHomeI(
                    pVarDefI->GetTy(),
                    new VarHome(pVar),
                    pVarDefI->GetRd(),
                    pVarDefI->GetSy() ),
                pVarDefI );
        } // if
    } // internHome

    private: static void internPlace(UpVar* const pUpVarE)
    {
        Function* const pPlace = pUpVarE->GetPlace();
        Variable* const pVar = pUpVarE->GetVar();

        if (pVar->GetOwner() == pPlace)
        {
            internHome(pUpVarE->GetVar());
        }
        else if (pPlace->IsClosure())
        {
            UpVar* const pUpVarH = pPlace->GetWork<FunWork>()->FindUpVar(
                pUpVarE->GetVar() );

            if (NULL == pUpVarH)
            {
                COMPILER_INTERNAL_ERROR();
                return;
            }

            ClosedMarker* pCd = pUpVarH->GetDefI()->GetOutput()->
                StaticCast<ClosedMarker>();

            if (pUpVarH->GetRx() != pCd)
            {
                COMPILER_INTERNAL_ERROR();
                return;
            }

            foreach (ClosedMarker::EnumUser, oEnum, pCd)
            {
                if (oEnum.GetI()->Is<VarAnnexI>())
                {
                    return;
                }
            } // for each insn

            Instruction* const pPrologueI = pPlace->GetPrologueI();

            pPrologueI->GetBB()->InsertAfterI(
                new VarAnnexI(new StackSlot, pCd),
                pPrologueI );
        }
    } // internPlace

    // [L]
    private: static Output* loadCell(
        Function*    const pCaller,
        Variable*    const pVar,
        Instruction* const pValuesI )
    {
        if (pVar->GetOwner() == pCaller)
        {
            return pVar->GetRd();
        }

        UpVar* const pUpVarR = pCaller->GetWork<FunWork>()->FindUpVar(pVar);
        if (NULL == pUpVarR)
        {
            CLOG(1, "<li>~S = not found</li>", pVar);
            return NULL;
        }

        switch (pUpVarR->GetAccess())
        {
        case Access_Base:
        case Access_Sp:
        {
            if (pVar->GetOwner() == pUpVarR->GetPlace())
            {
                internHome(pVar);
            }

            Register* const pRp = new Register;
            pValuesI->GetBB()->InsertBeforeI(
                new UpVarRefI(
                    pRp, pUpVarR->GetRb(), pUpVarR->GetDefI()->GetQd() ),
                pValuesI );

            return insertLoad(pRp, pValuesI);
        } // Access_Base

        case Access_ClosedCell:
        case Access_StackCell:
            return pUpVarR->GetRx();

        case Access_CellMarker:
            return pUpVarR->GetDefI()->GetOutput();
        } // switch access

        COMPILER_INTERNAL_ERROR();
        return NULL;
    } // loadCell

    private: static Operand* loadValue(
        Function*    const pCaller,
        Variable*    const pVar,
        Instruction* const pValuesI )
    {
        if (pVar->GetOwner() == pCaller)
        {
            if (pVar->GetStorage() == Variable::Storage_Literal)
            {
                return pVar->GetDefI()->GetSy();
            }

            Register* const pRp = new Register;
            pValuesI->GetBB()->InsertBeforeI(
                new SlotI(
                    tyPtrT,
                    pRp,
                    CLASS_stack_cell,
                    Qvalue,
                    pVar->GetRd() ),
                pValuesI );

            return insertLoad(pRp, pValuesI);
        } // if owner

        UpVar* const pUpVarR = pCaller->GetWork<FunWork>()->FindUpVar(pVar);
        if (NULL == pUpVarR)
        {
            return NULL;
        }

        switch (pUpVarR->GetAccess())
        {
        case Access_Base:
        case Access_Sp:
        {
            Register* const pRp = new Register;
            pValuesI->GetBB()->InsertBeforeI(
                new UpVarRefI(
                    pRp, pUpVarR->GetRb(), pUpVarR->GetDefI()->GetQd() ),
                pValuesI );

            return insertLoad(pRp, pValuesI);
        } // Access_Base

        case Access_CellMarker:
        {
            Register* const pRp = new Register;
            pValuesI->GetBB()->InsertBeforeI(
                new SlotI(
                    tyPtrT, pRp,
                    CLASS_closed_cell, Qvalue,
                    pUpVarR->GetDefI()->GetOutput() ),
                pValuesI );
            return insertLoad(pRp, pValuesI);
        } // Access_CellMarker

        case Access_ClosedCell:
        {
            Register* const pRp = new Register;
            pValuesI->GetBB()->InsertBeforeI(
                new SlotI(
                    tyPtrT, pRp,
                    CLASS_closed_cell, Qvalue,
                    pUpVarR->GetRx() ),
                pValuesI );
            return insertLoad(pRp, pValuesI);
        } // Access_ClosedCell

        case Access_LiteralMarker:
        case Access_RegRo:
        case Access_RegRw:
            return pUpVarR->GetRx();

        case Access_StackCell:
            return insertLoad(
                pUpVarR->GetRx()->StaticCast<Register>(),
                pValuesI );
        } // switch access

        CLOG(1, "<div class='e'>Bad access for upvar ~S</div>",
            pVar );

        COMPILER_INTERNAL_ERROR();
        return NULL;
    } // loadValue

    // [R]
    private: void run(FunEx* const pFun)
    {
        CLOG_SECTION(1, "Update call site of ~S", pFun);

        if (pFun->HasUpVar())
        {
            foreach (Function::EnumCall, oEnum, pFun)
            {
                updateCallSite(oEnum.GetI(), pFun);
            } // for each call site

            foreach (Function::EnumUser, oEnum, pFun)
            {
                updateCallSite(oEnum.GetI(), pFun);
            } // for each usse site
        }
        else
        {
            Function::EnumUser oEnumUser(pFun);
            while (! oEnumUser.AtEnd())
            {
                Instruction* const pClosureI = oEnumUser.GetI()->
                    DynamicCast<ClosureI>();

                oEnumUser.Next();

                if (NULL == pClosureI)
                {
                    continue;
                }

                pClosureI->GetRd()->ReplaceAll(pFun);
            } // while
        } // if
    } // run

    // [U]
    private: void updateCallSite(
        Instruction* const pI,
        Function*    const pCallee )
    {
        Function* const pCaller = pI->GetBBlock()->GetFunction();

        CLOG_SECTION(1, "update ~S in ~S", pI, pCaller);

        Instruction* pValuesI = pI->GetVy()->GetDefI();

        // Call inner function bar with variable number of arguments.
        //  CALL    %v1 <= #'foo %v2
        //  CALL    %r3 <= #'bar %v1
        //      ==>
        //  CALL    %v1 <= #'foo %v2
        //  CALL    %r3 <= #'list %v1
        //  VALUES* %v4 <= ... extra ... %r3
        //  CALL    %r5 <= #'bar %v4
        unless (pValuesI->Is<ValuesI>() || pValuesI->Is<ValuesAI>())
        {
            Register* const pR1 = new Register;

            pI->GetBB()->InsertBeforeI(
                new CallI(tyT, pR1, Qlist, pI->GetVy()),
                pI );

            Values* const pVy = new Values;

            pValuesI = new ValuesAI(pVy, pR1);

            pI->GetBB()->InsertBeforeI(pValuesI, pI);
        }

        WorkList_<Function> oDone;

        OperandBox* const pRefBox = pValuesI->GetOperandBox(0);

        foreach (UpVars::Enum, oEnum, pCallee->GetWork<FunWork>())
        {
            UpVar*    const pUpVarE = oEnum.Get();
            Variable* const pVar = pUpVarE->GetVar();

            CLOG_SECTION(1, "Callee ~S <b>~A</b>",
                pVar, k_rgpszAccess[pUpVarE->GetAccess()] );

            Operand* pSx = NULL;

            switch (pUpVarE->GetAccess())
            {
            case Access_Base:
            {
                internPlace(pUpVarE);

                Function* const pPlace = pUpVarE->GetPlace();
                if (pPlace->IsInList())
                {
                    // We've already have base regiser.
                    break;
                }

                oDone.Push(pPlace);
                if (pVar->GetOwner() == pCaller)
                {
                    pSx = getSp();
                }
                else
                {
                    UpVar* const pUpVarR =
                        pCaller->GetWork<FunWork>()->FindUpVar(pVar);

                    if (NULL == pUpVarR)
                    {
                        // We can't find upvar reference.
                        break;
                    }

                    CLOG(2, "<li>Caller ~A</li>",
                        k_rgpszAccess[pUpVarE->GetAccess()] );

                    switch (pUpVarR->GetAccess())
                    {
                    case Access_Base:
                        pSx = pUpVarR->GetRb();
                        break;

                    case Access_Sp:
                    {
                        Register* const pR1 = new Register;
                        pValuesI->GetBB()->InsertBeforeI(
                            new UpVarBaseI(pR1, pVar->GetOwner()),
                            pValuesI );
                        pSx = pR1;
                        break;
                    } // Access_Sp

                    default:
                        COMPILER_INTERNAL_ERROR();
                        break;
                    } // switch access
                } // if
                break;
            } // Access_Base

            case Access_ClosedCell:
            case Access_CellMarker:
            case Access_StackCell:
                pSx = loadCell(pCaller, pVar, pValuesI);
                break;

            case Access_LiteralMarker:
            case Access_RegRo:
                pSx = loadValue(pCaller, pVar, pValuesI);
                break;

            case Access_Sp:
                internPlace(pUpVarE);
                break;

            default:
                CLOG(1, "<li class='e'>Unsupported access</li>~%");
                COMPILER_INTERNAL_ERROR();
                break;
            } // switch access

            if (NULL == pSx)
            {
                CLOG(1, "<li>~S = Nothing to do</li>", pVar);
            }
            else
            {
                CLOG(1, "<li>~S = ~S</li>", pVar, pSx);
                pValuesI->InsertOperandBefore(pSx, pRefBox);
            }
        } // for each upvar
    } // updateCallSite
}; // SubPassUpdateCallSite

/// <summary>
///   Pass for updating callees
/// </summary>
class SubPassUpdateOwner :
    public Pass_<SubPassUpdateOwner, SubPass>
{
    public: static const char* GetName_() { return "UpdateOwner"; }

    // ctor
    private: SubPassUpdateOwner() {}

    // Entry Point
    public: static void Run(Function* const pFun)
    {
        SubPassUpdateOwner oSubPass;
        oSubPass.run(pFun->Extend<FunEx>());
    } // Run

    // [P]
    private: static void processVar(Variable* const pVar)
    {
        VarDefI* pVarDefI = pVar->GetDefI()->DynamicCast<VarDefI>();
        if (NULL == pVarDefI)
        {
            COMPILER_INTERNAL_ERROR();
            return;
        }

        switch (pVar->GetStorage())
        {
        case Variable::Storage_Closed:
            break;

        case Variable::Storage_Literal:
            foreach (Register::EnumUser, oEnum, pVarDefI->GetRd())
            {
                if (SlotI* pSlotI =
                        oEnum.GetI()->DynamicCast<SlotI>() )
                {
                    foreach (
                        Register::EnumUser,
                        oEnum,
                        pSlotI->GetRd() )
                    {
                        if (LoadI* pLoadI =
                                oEnum.GetI()->DynamicCast<LoadI>() )
                        {
                            pLoadI->GetRd()->ReplaceAll(
                                pVarDefI->GetSy() );
                        }
                        else
                        {
                            COMPILER_INTERNAL_ERROR();
                        }
                    } // for
                } // if SlotI
            } // for use of VarDefI
            break;

        case Variable::Storage_Stack:
            pVarDefI->GetBB()->InsertAfterI(
                new VarHomeI(
                    pVarDefI->GetTy(),
                    new VarHome(pVar),
                    pVarDefI->GetRd(),
                    pVarDefI->GetSy() ),
                pVarDefI );
            break;

        default:
            COMPILER_INTERNAL_ERROR();
            break;
        } // swtich storage
    } // processVar

    // [R]
    private: void run(FunEx* const pFun)
    {
        CLOG_SECTION(1, "Update owner ~S", pFun);

        foreach (Function::EnumVar, oEnum, pFun)
        {
            processVar(oEnum.Get());
        }
    } // run
}; // SubPassUpdateOwner

} // CgUpVar

using namespace CgUpVar;

/// <summary>
///  UpVar pass.
/// </summary>
class PassUpVar :
    public Pass_<PassUpVar, ModulePass>,
    protected Mm
{
    public: static const char* GetName_() { return "UpVar"; }

    // Entry Point
    private: virtual void processModule(Module* pM) override
    {
        analyze(pM);

        foreach (Module::EnumFunction, oEnum, pM)
        {
            SubPassAccess::Run(oEnum.Get());
        } // for each fun

        {
            CLOG_SECTION(1, "<h3>Update Owner</h3>~%");
            foreach (Module::EnumFunction, oEnum, pM)
            {
                SubPassUpdateOwner::Run(oEnum.Get());
            } // for each fun
        }

        {
            CLOG_SECTION(1, "<h3>Update Callee</h3>~%");
            foreach (Module::EnumFunction, oEnum, pM)
            {
                SubPassUpdateCallee::Run(oEnum.Get());
            } // for each fun
        }

        {
            CLOG_SECTION(1, "<h3>Update Call Site</h3>~%");
            foreach (Module::EnumFunction, oEnum, pM)
            {
                SubPassUpdateCallSite::Run(oEnum.Get());
            } // for each fun
        }

        {
            CLOG(1, "<h3>Clean</h3>~%");
            foreach (Module::EnumFunction, oEnum, pM)
            {
                Function* pFun = oEnum.Get();
                pFun->Clean();
            } // for each fun
        }
    } // processModule

    // [A]
    private: void analyze(Module* const pM)
    {
        CLOG_SECTION(1, "<h2>Anayalyze</h2>");

        foreach (Module::EnumFunction, oEnum, pM)
        {
            FunEx* const pFun = oEnum.Get()->Extend<FunEx>();

            CLOG_SECTION(1, "analyze ~S", pFun);

            FunWork* pFunWork = new(this) FunWork;
            pFun->SetWork(pFunWork);
            foreach (Function::EnumUpVar, oEnum, pFun)
            {
                Variable* pVar = oEnum.Get();

                CLOG_SECTION(1, "analyze ~S", pVar);

                Function* pPlace;
                Path const ePath = computePath(pFun, pVar, &pPlace);

                UpVar* pUpVar = new(this) UpVar(
                    oEnum.GetI()->StaticCast<UpVarDefI>(),
                    pPlace,
                    ePath );

                oEnum.GetI()->SetWork(pUpVar);
                pFunWork->Append(pUpVar);

                CLOG(1, "<li><b>~A</b></li>", k_rgpszPath[ePath]);
            } // for each upvar
        } // for each fun
    } // analyze

    // [C]
    private: static Path computePath(
        FunEx*      const pCalleeIn,
        Variable*   const pVar,
        Function**  const out_pPlace )
    {
        *out_pPlace = NULL;

        Function* const pOwner = pVar->GetOwner();
        ASSERT(pOwner != pCalleeIn);

        Function* pCallee = pCalleeIn;

        if (pCallee->GetFlavor() == Function::Flavor_Finally)
        {
            // Finally function is called from two places.
        }
        else if (pCallee->IsStackRestify())
        {
            // Stack size is dynamically determined.
        }
        else
        {
            // Is callee accessed by fixed path?
            Path ePath = Path_Fixed;

            FunEx* pRunner = pCallee->Extend<FunEx>();
            for (;;)
            {
                FunEx* const pCaller =
                    pRunner->GetSingleCaller()->Extend<FunEx>();

                if (NULL == pCaller)
                {
                    break;
                }

                CLOG(1, "<li>single ~S</li>~%", pCaller);

                if (pOwner == pCaller || pCaller->IsClosure())
                {
                    *out_pPlace = pCaller;
                    return ePath;
                }

                if (pCaller->IsStackRestify() ||
                    pCaller->GetFlavor() == Function::Flavor_Finally )
                {
                    ePath = Path_Variable;
                }

                pRunner = pCaller;
            } // for
        } // if

        *out_pPlace = pVar->GetOwner();

        switch (pVar->GetStorage())
        {
        case Variable::Storage_Closed:
        case Variable::Storage_Literal:
            break;

        case Variable::Storage_Register:
        case Variable::Storage_Stack:
            return Path_Variable;

        default:
            COMPILER_INTERNAL_ERROR();
            return Path_Variable;
        } // switch eStorage

        WorkList_<Function> oPending;
        WorkList_<Function> oDone;

        Function* pPlace = NULL;
        for (;;)
        {
            oDone.Push(pCallee);

            foreach (Function::EnumInEdge, oEnum, pCallee)
            {
                Function* const pCaller = oEnum.GetNode();

                if (pCaller->IsInList())
                {
                    continue;
                }

                CLOG(1, "<li>multiple ~S</li>~%", pCaller);

                if (pOwner == pCaller || pCaller->IsClosure())
                {
                    if (NULL != pPlace)
                    {
                        *out_pPlace = NULL;
                        return Path_Mixed;
                    }

                    pPlace = pCaller;
                    oDone.Push(pCaller);
                }
                else
                {
                    oPending.Push(pCaller);
                }
            } // for each inedge

            if (oPending.IsEmpty())
            {
                return Path_Variable;
            }

            pCallee = oPending.Pop()->Extend<FunEx>();
        } // fo
    } // computePath
}; // PassUpVar

DEFPASS(UpVar)

} // Compiler

} // TinyCl
