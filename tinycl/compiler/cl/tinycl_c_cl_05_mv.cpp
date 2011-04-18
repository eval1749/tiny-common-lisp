#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 05 Data and Control-Flow
// tinycl_c_cl_05_mv.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_05_mv.cpp#7 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

void PassParse::parseMissingValues(
    const Expect*   const pExpect,
    TyValues::Enum* const pEnumTy,
    ValuesI*        const pValuesI )
{
    while (pEnumTy->IsRequired())
    {
        if (pEnumTy->Get()->IsType(nil))
        {
            if (NULL != pValuesI)
            {
                pValuesI->AppendOperand(Literal::New(nil));
            }

            styleWarn("Expect ~S but value is missing.",
                pExpect->m_pty->Unparse() );
        }
        else
        {
            parseError("Expect ~S but value is missing.",
                pExpect->m_pty->Unparse() );
            break;
        }

        pEnumTy->Next();
    } // while
} // PassParse::parseMissingValues

defparser(multiple_value_bind)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(multiple-value-bind (var*) form decl* form*)" );

    LexEnv oBindEnv(m_pLexEnv->GetTy());
    LexEnvScope oLexScope(&oBindEnv);

    Val bindings = cadr(form);
    if (lt(safe_list_length(bindings), 0))
    {
        parseError("Malformed bindings: ~S", bindings);
        return emitLinkage(Literal::New(nil));
    }

    foreach (List::Enum, oEnum, cadr(form))
    {
        Val name = oEnum.Get();
        if (! symbolp(name))
        {
            parseError("Variable name must be a symbol ~S", name);
            name = make_symbol(make_string("err"));
        }
        newVarLet(name);
    } // for each elt

    Val forms = parseDecls(cdddr(form));

    deactivateLexEnv();

    OpenBindI* pOpenBindI = NULL;

    Operand* pSx = parseOperand(
        Qmultiple_value_bind,
        tyValuesRestT,
        1,
        caddr(form) );

    if (Unreachable == pSx) return uselessForm(form);

    Values* pVx = pSx->DynamicCast<Values>();
    if (NULL == pVx)
    {
        pVx = new Values;
        emitI(new ValuesI(pVx, pSx));
    }

    Register* pRn = new Register;
    emitI(new CountI(pRn, pVx));

    uint nNth = 0;
    foreach (LexEnv::EnumVar, oEnum, m_pLexEnv)
    {
        VarLet* pVarLet = oEnum.Get()->DynamicCast<VarLet>();
        if (NULL == pVarLet) continue;

        Register* pRx = new Register;
        emitI(new SelectI(tyT, pRx, pVx, nNth));

        if (nNth > 0)
        {
            Bool* pBx = new Bool;
            emitI(new GtI(pBx, pRn, Literal::New(Fixnum::Encode(nNth))));

            Register* pRy = new Register;
            emitI(new IfI(pRy, pBx, pRx, Literal::New(nil)));

            pRx = pRy;
        }

        nNth += 1;

        pOpenBindI = emitBind(pVarLet, pRx, pOpenBindI);
    } // for each var

    return parseLetAux(pExpect, form, forms, pOpenBindI);
} // multiple_value_bind

defparser(multiple_value_call)
{
    CHECK_SYNTAX(2, MaxFormLength, "(multiple-value-call fn form*)");

    Val const fn_form = cadr(form);
    Val const forms   = cddr(form);

    if (nil == forms)
    {
        return parseForm(pExpect, list(Qfuncall, fn_form));
    }

    if (nil == cdr(forms))
    {
        Operand* const pSx = parseOperand(
            Qmultiple_value_call,
            tyT,
            0,
            fn_form );
        if (Unreachable == pSx)
        {
            return uselessForm(form);
        }

        Callee oCallee;
        foldCallee(pSx, &oCallee);

        Operand* pSy = parseOperand(
            Qmultiple_value_call,
            tyValuesRestT,
            1,
            car(forms) );
        if (Unreachable == pSy)
        {
            return uselessForm(form);
        }

        Values* pVy = pSy->DynamicCast<Values>();
        if (NULL == pVy)
        {
            pVy = new Values;
            emitI(new ValuesI(pVy, pSy));
        }

        return emitLinkage(processCall(pExpect, form, &oCallee, pVy));
    } // if

    // (multiple-value-call fn a1 a2 ... an)
    //  => (apply fn (nconc (multiple-value-list a1)
    //                      (multiple-value-list a2)
    //                      ...
    //                      (multiple-value-list an) ))
    Val args = list(Qnconc);
    Val tail = args;
    foreach (List::Enum, oEnum, forms)
    {
        Val frob = list(list(Qmultiple_value_list, oEnum.Get()));
        tail = setf_cdr(frob, args);
    } // for each value form
    return parseForm(pExpect, list(Qapply, fn_form, args));
} // multiple_value_call

defparser(multiple_value_prog1)
{
    CHECK_SYNTAX(2, MaxFormLength, "(multiple-value-prog1 form1 form*)");

    if (nil == cddr(form))
    {
        return parseForm(pExpect, cadr(form));
    }

    Operand* pSx = parseForm1(pExpect, cadr(form));
    if (Unreachable == pSx) return unreachableForm(form);

    BBlock* pSucc = setContinue();

    Expect oExpect(Qmultiple_value_prog1, tyVoid);
    if (Unreachable == parseForms(&oExpect, form, cddr(form)))
    {
        return Unreachable;
    }

    restoreSucc(pSucc);

    if (! pSx->Is<Values>())
    {
        return emitLinkage(pSx);
    }
    else
    {
        pSx = emitSaveValues(pSx);
        return emitLinkage(emitRestoreValues(pSx));
    }
} // multiple_value_prog1

defparser(nth_value)
{
    CHECK_SYNTAX(3, 3, "(nth-value nth form)");

    if (tyVoid == pExpect->m_pty)
    {
        return parseForms(pExpect, form, cdr(form));
    }

    Operand* pSx = parseOperand(Qnth_value, tyFixnum, 0, cadr(form));
    if (Unreachable == pSx) return uselessForm(form);

    Operand* pSy = parseOperand(Qnth_value, tyValuesRestT, 1, cadr(form));
    if (Unreachable == pSy) return uselessForm(form);

    Values* pVy = pSy->DynamicCast<Values>();
    if (NULL == pVy)
    {
        pVy = new Values;
        emitI(new ValuesI(pVy, pSy));
    }

    if (Literal* pLx = pSx->DynamicCast<Literal>())
    {
        Val nth = pLx->GetDatum();
        if (! fixnump(nth) || Fixnum::Decode_(nth) < 0)
        {
            parseError("Expect sequence-index instead of ~S.", nth);
            return emitLinkage(Literal::New(nil));
        }

        Register* pRd = new Register;
        emitI(new SelectI(pExpect->m_pty, pRd, pVy, Fixnum::Decode_(nth)));

        if (Fixnum::Encode(0) != nth)
        {
            Register* pRn = new Register;
            emitI(new CountI(pRn, pVy));

            Bool* pBd = new Bool;
            emitI(new GtI(pBd, pRn, Literal::New(nth)));

            Register* pRx = new Register;
            emitI(new IfI(pRx, pBd, pRd, Literal::New(nil)));

            pRd = pRx;
        } // if

        return emitLinkage(pRd);
    }
    else
    {
        Register* pRd = new Register;
        emitI(new NthValueI(pExpect->m_pty, pRd, pSx, pVy));
        return emitLinkage(pRd);
    } // if
} // nth_value

defparser(values)
{
    CHECK_SYNTAX(1, MaxFormLength, "(values form*)");

    if (tyVoid == pExpect->m_pty)
    {
        uselessForm(form);
        return parseForms(pExpect, form, cdr(form));
    }

    if (TyValues* ptyValues = pExpect->m_pty->DynamicCast<TyValues>())
    {
        Values* pVd = new Values;
        ValuesI* pValuesI = new ValuesI(pVd);

        TyValues::Enum oEnumTy(ptyValues);

        Val const ty = list(Qvalues);
        Val tail = ty;

        foreach (List::Enum, oEnum, cdr(form))
        {
            const Type* ptyExpect;
            if (oEnumTy.AtEnd())
            {
                styleWarn("Extra value: ~S", oEnum.Get());
                ptyExpect = tyVoid;
            }
            else
            {
                ptyExpect = oEnumTy.Get();
            }

            Expect oExpect(Qvalues, ptyExpect);
            Operand* const pSx = parseForm1(&oExpect, oEnum.Get());
            if (Unreachable == pSx)
            {
                return unreachableForm(form);
            }

            if (! oEnumTy.AtEnd())
            {
                tail = setf_cdr(list(pSx->GetTy()->Unparse()), tail);
                pValuesI->AppendOperand(pSx);
                oEnumTy.Next();
            }
            oExpect.m_iNth += 1;
        } // for each elt

        parseMissingValues(pExpect, &oEnumTy, pValuesI);
        pValuesI->SetTy(Type::Parse(ty));
        emitI(pValuesI);
        return emitLinkage(pVd);
    }
    else
    {
        Operand* pSx = parseForm1(pExpect, cadr(form));
        if (Unreachable == pSx)
        {
            return unreachableForm(form);
        }

        Expect oExpect(Qvalues, tyVoid);
        foreach (List::Enum, oEnum, cddr(form))
        {
            if (parseForm(&oExpect, oEnum.Get()) == Unreachable)
            {
                return unreachableForm(form);
            }
            oExpect.m_iNth += 1;
        } // for

        return emitLinkage(pSx);
    } // if
} // values

} // Compiler

} // TinyCl
