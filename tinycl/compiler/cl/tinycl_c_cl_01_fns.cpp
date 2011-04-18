#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Utility Functions
// tinycl_c_cl_01_fns.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_01_fns.cpp#9 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

class VarEx : public Variable
{
    public: bool IsSSA() const
    {
        class Local
        {
            public: static bool HasStore(SsaOutput* const pRd)
            {
                foreach (SsaOutput::EnumUser, oEnum, pRd)
                {
                    Instruction* const pSlotI =
                        oEnum.GetI()->DynamicCast<SlotI>();

                    if (NULL == pSlotI)
                    {
                        continue;
                    }

                    foreach (Register::EnumUser, oEnum, pSlotI->GetRd())
                    {
                        if (oEnum.GetI()->Is<StoreI>())
                        {
                            return true;
                        }
                    } // for each user
                } // for each user

                return false;
            } // HasStore
        }; // Local

        Register* const pRd = GetRd();
        if (NULL == pRd)
        {
            return false;
        }

        if (Local::HasStore(pRd))
        {
            return false;
        }

        foreach (Module::EnumFunction, oEnum, Context::Get()->GetModule())
        {
            Function* const pFun = oEnum.Get();
            if (GetOwner() != pFun)
            {
                foreach (Function::EnumUpVar, oEnum, pFun)
                {
                    if (oEnum.Get() == this)
                    {
                        if (Local::HasStore(oEnum.GetI()->GetQd()))
                        {
                            return false;
                        }
                    }
                } // for upvar
            }
        } // for fun

        return true;
    } // Variable::IsSSA
}; // VarEx

// [C]
bool PassParse::checkSyntax(int iMin, int iMax, const char* psz, Val form)
{
    if (0 == iMax)
    {
        iMax = Fixnum::MostPositive;
    }

    Int iLen = Fixnum::Decode_(safe_list_length(form));
    if (iLen >= iMin && iLen <= iMax)
    {
        return true;
    }

    parseError("Expect ~S", make_string(psz));
    return false;
} // PassParse::checkSyntax

// [E]
Val PassParse::ensureVarName(Val name)
{
    if (symbolp(name))
    {
        VarRef* pVarRef = findVarRef(name);

        while (NULL != pVarRef)
        {
            if (pVarRef->Is<VarConst>())
            {
                parseError("Can't use constant name as variable: ~S", name);
                return make_symbol(make_string(L"e"));
            }

            pVarRef = pVarRef->GetOuter();
        } // while

        return name;
    } // if

    parseError("Invalid variable name ~S.", name);
    return make_symbol(make_string(L"err"));
} // PassParse::ensureVarName

// [F]
/// <summary>
///   Fold callee
/// </summary>
//= <FIXME date="2008-08-03" by="yosi@msn.com">
//=   We should have settings for known caller optimization.
//= </FIXME>
void PassParse::foldCallee(
    Operand* const pSx,
    Callee*  const out_oCallee )
{
    class Local
    {
        public: static bool IsKnownCallee(Val const fname)
        {
            Val pkg = nil;

            if (Symbol* const p = fname->DynamicCast<Symbol>())
            {
                pkg = p->m_package;
            }
            else if (SetfCell* const p = fname->DynamicCast<SetfCell>())
            {
                pkg = p->m_name->StaticCast<Symbol>()->m_package;
            }
            else
            {
                COMPILER_INTERNAL_ERROR();
            }

            return PKG_cl == pkg || PKG_si == pkg;
        } // IsKnownCallee
    }; // Local

    out_oCallee->m_pOperand = pSx;

    if (Register* const pRx = pSx->DynamicCast<Register>())
    {
        if (TyFunction* const pFunty =
                pRx->GetDefI()->GetTy()->DynamicCast<TyFunction>() )
        {
            out_oCallee->m_pFunty = pFunty;
        }

        if (LoadFunI* const pLoadFunI =
                pRx->GetDefI()->DynamicCast<LoadFunI>() )
        {
            out_oCallee->m_fNotinline = pLoadFunI->IsNotinline();

            if (FunName* const pFunName =
                    pLoadFunI->GetSx()->DynamicCast<FunName>() )
            {
                out_oCallee->m_pFunty   = pFunName->GetFunty();
                out_oCallee->m_pOperand = pFunName;
                out_oCallee->m_name     = pFunName->GetName();
            }
            else
            {
                Val fname = pLoadFunI->GetLx();
                if (Local::IsKnownCallee(fname))
                {
                    out_oCallee->m_pOperand = pLoadFunI->GetSx();
                    out_oCallee->m_name = fname;
                }
            }
        } // if LoadFunI
    } // if reg
} // PassParse::foldCallee

// [I]
/// <summary>
///   Report type incompatibility error.
/// </summary>
Operand* PassParse::incompatibleTypes(
    const Type*   pty1,
    const Type*   pty2,
    Val     )   // form
{
    parseError("~S and ~S are incompatible.",
        pty1->Unparse(),
        pty2->Unparse() );

    return emitLinkage(Unreachable);
} // PassParse::incompatibleTypes

/// <summary>
///   Report an ignored form.
/// </summary>
Operand* PassParse::ignoreForm(Val form)
{
    bool fIgnore = true;

    if (Symbol* p = form->DynamicCast<Symbol>())
    {
        // Ignore uninterned variable
        if (nil == p->m_package)
        {
            fIgnore = false;
        }
    }

    if (fIgnore)
    {
        styleWarn("Ignore form ~S", form);
    }

    return emitLinkage(Void);
} // PassParse::ignoreForm

// [N]
BBlock* PassParse::newBBlock()
{
    return m_pOwner->InsertBefore(new BBlock, m_pOwner->GetExitBB());
} // PassParse::newBBlock

Output* PassParse::newOutput(const Type* pty)
{
    if (tyVoid == pty)
    {
        return Void;
    }

    if (pty->Is<TyValues>())
    {
        return new Values;
    }

    return new Register(RegClass_Gpr);
} // PassParse::newOutput

// [O]
Operand* PassParse::optimizeLexVars(Operand* pSret)
{
    class Local
    {
        public: static Operand* ReplaceVarRefs(
            SsaOutput*  const pRcell,
            Operand*    const pSx,
            Operand*    pSret )
        {
            if (NULL == pRcell)
            {
                return pSret;
            }

            CLOG(4, "<ol>~%");
            foreach (SsaOutput::EnumUser, oEnum, pRcell)
            {
                SlotI* const pSlotI =
                    oEnum.Get()->GetI()->StaticCast<SlotI>();

                foreach (Register::EnumUser, oEnum, pSlotI->GetRd())
                {
                    if (LoadI* const pLoadI =
                            oEnum.Get()->GetI()->DynamicCast<LoadI>() )
                    {
                        Register* const pRd = pLoadI->GetRd();

                        if (Register* const pRx =
                                pSx->DynamicCast<Register>() )
                        {
                            if (NULL == pRx->GetVar())
                            {
                                pRx->SetVar(pRd->GetVar());
                            }
                        }

                        pRd->ReplaceAll(pSx);
                        if (pRd == pSret)
                        {
                            CLOG(4,
                                "<li>Use return value ~S instead of ~S.</li>",
                                pSx, pSret );

                            pSret = pSx;
                        }
                    }
                } // for each LoadI
            } // for each SlotI
            CLOG(4, "</ol>~%");
            return pSret;
        } // ReplaceVarRefs
    }; // Local

    CLOG_SECTION(3, "<h3>Optimize Lexical Variables ~S</h3>", m_pOwner);

    bool const fNlxp = m_pOwner->HasNonlocalExitPoint();

    Function::EnumVar oEnumVar(m_pOwner);
    while (! oEnumVar.AtEnd())
    {
        Variable* pVar = oEnumVar.Get();
        oEnumVar.Next();

        CLOG_SECTION(3, "~S", pVar);

        Register*    const pRd = pVar->GetRd();
        Instruction* const pVarDefI = pRd->GetDefI();
        Operand*     const pSy = pVarDefI->GetSy();

        if (! pVar->Extend<VarEx>()->IsSSA())
        {
            CLOG(3, "<li>Not SSA</li>");
        }
        else if (pSy->Is<Literal>())
        {
            CLOG(3, "<li>Constant variable</li>");
            // Replace constant in all functions
            foreach (
                Module::EnumFunction,
                oEnum,
                Context::Get()->GetModule() )
            {
                Function* pFun = oEnum.Get();

                if (pVar->GetOwner() != pFun)
                {
                    Pseudo* pRcell = pFun->FindUpVar(pVar);
                    if (NULL != pRcell)
                    {
                        Local::ReplaceVarRefs(pRcell, pSy, NULL);
                    }
                }
            } // for each fun
        }
        else if (fNlxp)
        {
            CLOG(3, "<li>has nonlocal exit point</li>");
        }
        else
        {
            CLOG(3, "<li>SSA Variable</li>");
            pSret = Local::ReplaceVarRefs(pRd, pSy, pSret);
        }
    } // for each var

    return pSret;
} // PassParse::optimizeLexVars

// [P]

void PassParse::parseError(
    const Expect*   const pExpect,
    Val             const form,
    const Type*     const pty )
{
    switch (pExpect->m_eKind)
    {
    case Expect::Kind_Argument:
        parseErrorV("Expected type ~S[~D] is ~S but ~S is ~S.",
            list(
                pExpect->m_name,
                Fixnum::Encode(pExpect->m_iNth),
                pExpect->m_pty->Unparse(),
                form,
                pty->Unparse() ) );
        break;

    default:
        parseError("Expect ~S but ~S is ~S.",
            pExpect->m_pty->Unparse(),
            form,
            pty->Unparse() );
        break;
    } // switch kind
} // PassParse::parserError

void PassParse::parseErrorV(const char* psz, Val args)
{
    writePrefix("Compile-Error");
    Val ctrl = make_string(psz);
    values_list(listA(t, ctrl, args));
    formatV(Thread::Get());
    write_char(Newline);

    push(list(ctrl, args), Context::Get()->m_errors);
} // PassParse::parseErrorV

PassParse::ClFrame* PassParse::popFrame()
{
    ClFrame* pFrame = m_pFrame;
    m_pFrame = m_pFrame->GetOuter();
    return pFrame;
} // PassParse::popFrame

void PassParse::pushFrame(ClFrame* pFrame)
{
    ASSERT(pFrame != m_pFrame);
    pFrame->m_pOwner = m_pOwner;
    pFrame->SetOuter(m_pFrame);
    m_pFrame = pFrame;
} // PassParse::pushFrame

// [R]
void PassParse::rememberLineNumber(Val form)
{
    ASSERT(consp(form));

    Val htb = TLV(Aread_line_number_tableA);
    if (! hash_table_p(htb))
    {
        return;
    }

    Val linenum = gethash(form, htb);
    if (nil != linenum)
    {
        m_linenum = linenum;
    }
} // rememberLineNumber

// [S]
void PassParse::styleWarnV(const char* psz, Val args)
{
    writePrefix("Style-Warning");
    Val ctrl = make_string(psz);
    values_list(listA(t, ctrl, args));
    formatV(Thread::Get());
    write_char(Newline);

    push(list(ctrl, args), Context::Get()->m_warnings);
} // PassParse::styleWarnV

// [W]
void PassParse::warnV(const char* psz, Val args)
{
    writePrefix("Warning");
    Val ctrl = make_string(psz);
    values_list(listA(t, ctrl, args));
    formatV(Thread::Get());
    write_char(Newline);

    push(list(ctrl, args), Context::Get()->m_warnings);
} // PassParse::warnV

void PassParse::writePrefix(const char* const pszClass)
{
    write_string("; ");
    write_string(pszClass);
    write_string(": ");

    if (nil == m_linenum)
    {
        return;
    }

    Val truename = Context::Get()->GetTruename();
    if (nil != truename)
    {
        format(t, "~A(~D): ", truename, xxadd(m_linenum, one));
    }
} // PassParse::writePrefix

} // Compiler

} // TinyCl
