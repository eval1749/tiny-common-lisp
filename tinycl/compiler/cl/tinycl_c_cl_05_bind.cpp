#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 03 Evaluation and Compilation
// tinycl_c_cl_05_ctrl.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_05_bind.cpp#9 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

using namespace Private;

Val MakeMacroExpanderForm(Val, Val, Val);

namespace Compiler
{

static Val computeInnerFunctionName(Val key, Val fname, Function* pOuter)
{
    switch (pOuter->GetFlavor())
    {
    case Function::Flavor_Anonymous:
        if (pOuter->GetName() == Ktoplevel)
        {
            return fname;
        }
        else
        {
            return list(key, pOuter->GetName(), fname);
        }

    case Function::Flavor_Toplevel:
        return fname;

    default:
    {
        Val names = pOuter->GetName();
        if (consp(names) && (Qflet == car(names) || Qlabels == car(names)))
        {
            names = cdr(names);
        }
        else
        {
            names = list(names);
        }

        return cons(key, append(names, list(fname)));
    } // default
    } // switch flavor
} // computeInnerFunctionName

void PassParse::parseFunBinds(Val form)
{
    Val bindings = cadr(form);

    if (lt(safe_list_length(bindings), 0))
    {
        parseError("Malformed function bindings: ~S", bindings);
        return;
    }

    foreach (List::Enum, oEnum, bindings)
    {
        Val binding = oEnum.Get();

        if (lt(safe_list_length(binding), 0))
        {
            parseError("Malformed function binding: ~S", binding);
            return;
        }

        Val fname = car(binding);
        if (! function_name_p(fname))
        {
            parseError("Invalid function name ~S.", fname);
            fname = make_symbol(make_string(L"err"));
        }

        Function* pFun = new Function(
            Function::Flavor_Named,
            computeInnerFunctionName(car(form), fname, m_pOwner) );

        Val const name = consp(fname) ?
            intern_setf_cell(cadr(fname)) :
            fname;

        FunDef* pFunDef = new FunDef(name, pFun);

        pFunDef->m_form = binding;

        if (consp(fname))
        {
            // setf-function must be return the first parameter.
            pFunDef->m_oLexEnv.SetTy(tyT);
        }

        {
            LexEnvScope oScope(&pFunDef->m_oLexEnv);

            pFunDef->m_forms = parseLambdaList(
                binding,
                &pFunDef->m_oLambdaList );
        }

        addFunRef(pFunDef);
    } // for each binding
} // PassParse::parseFunBinds

Operand* PassParse::parseLetAux(
    const Expect*   pExpect,
    Val             form,
    Val             forms,
    OpenBindI*      pOpenBindI )
{
    activateLexEnv();

    Operand* pSx;

    if (NULL == pOpenBindI)
    {
        pSx = parseForms(pExpect, form, forms);
    }
    else
    {
        emitI(pOpenBindI);

        BindFrame* pFrame = pOpenBindI->GetOutput()->GetWork<BindFrame>();

        BBlock* pSucc = setContinue();

        pSx = parseForms(pExpect, form, forms);

        popFrame();

        if (Unreachable != pSx)
        {
            Operand* pRsave = emitWeakSaveValues(pSx);
            emitUnwind(pFrame);
            pSx = emitWeakRestoreValues(pRsave);
        }

        restoreSucc(pSucc);
        emitLinkage(pSx);
    } // if

    closeLexEnv();

    return pSx;
} // PassParse::parseLetAux

Operand* PassParse::parseSetfCall(
    const Expect*   pExpect,
    Val             place,
    Val             value,
    Operand*        pCallee )
{
    ExpectArg oExpect(car(place), 0, tyT);

    ValuesI* pValuesI = new ValuesI(new Values);
    pValuesI->AppendOperand(Literal::New(nil));

    oExpect.m_pty  = tyT;
    oExpect.m_iNth = 0;
    foreach (List::Enum, oEnum, cdr(place))
    {
        Operand* pSx = parseForm1(&oExpect, oEnum.Get());
        if (Unreachable == pSx) return uselessForm(place);
        pValuesI->AppendOperand(pSx);
        oExpect.m_iNth += 1;
    } // for each arg

    Operand* pSy = parseForm1(&oExpect, value);
    if (Unreachable == pSy) return uselessForm(place);
    pValuesI->GetOperandBox(0)->SetOperand(pSy);
    emitI(pValuesI);

    Output* pSd = newOutput(pExpect->m_pty);
    emitI(new CallI(pExpect->m_pty, pSd, pCallee, pValuesI->GetVd()));
    return emitLinkage(pSd);
} // PassParse::parseSetfCall

void PassParse::parseVarBinds(Val bindings)
{
    if (lt(safe_list_length(bindings), 0))
    {
        parseError("Malformed bindings: ~S", bindings);
        return;
    }

    foreach (List::Enum, oEnum, bindings)
    {
        Val binding  = oEnum.Get();
        Val name     = Fixnum::Encode(0);
        Val initform = nil;

        if (symbolp(binding))
        {
            name = binding;
        }
        else if (consp(binding))
        {
            if (nil == cdr(binding))
            {
                // (name)
                name = car(binding);
            }
            else if (consp(cdr(binding)))
            {
                initform = cadr(binding);
                if (nil == cddr(binding))
                {
                    name = car(binding);
                }
            }
        } // if

        if (! symbolp(name))
        {
            parseError("Malformed binding: ~S", binding);
            name = make_symbol(make_string("err"));
        }

        newVarLet(name, initform);
    } // for each binding
} // PassParse::parseVarBinds

defparser(flet)
{
    CHECK_SYNTAX(2, MaxFormLength, "(flet (binding*) decl* form*");

    LexEnv oBindEnv(m_pLexEnv->GetTy());
    LexEnvScope oLexScope(&oBindEnv);

    parseFunBinds(form);

    Val forms = parseDecls(cddr(form));

    deactivateLexEnv();

    foreach (LexEnv::EnumFun, oEnum, m_pLexEnv)
    {
        if (FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>())
        {
            processFunDef(pFunDef);
        }
    } // for each fun

    activateLexEnv();

    Operand* pSx = parseForms(pExpect, form, forms);

    closeLexEnv();

    return pSx;
} // flet

defparser(function)
{
    CHECK_SYNTAX(2, 2, "(function fn)");

    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }

    Val const fname = cadr(form);

    if (function_name_p(fname))
    {
        Val cell = fname;
        if (consp(fname))
        {
            cell = intern_setf_cell(cadr(fname));
        } // if

        FunRef* const pFunRef = internFunRef(cell);

        FunRef* pFunEnt = pFunRef;
        while (pFunEnt->Is<FunDcl>())
        {
            pFunEnt = pFunEnt->GetOuter();
        } // while

        markFunUse(pFunEnt);

        if (FunDef* const pFunDef = pFunEnt->DynamicCast<FunDef>())
        {
            // We use CLOSURE instruction, since
            //   o We don't know whether the function has upvar or not at this
            //     time.
            //   o We need to propagate function type and notinline.
            Values* const pVy = new Values;
            emitI(new ValuesI(pVy));

            Register* const pRd = new Register;

            ClosureI* const pClosureI = new ClosureI(
                pFunRef->GetFunty(),
                pRd,
                pFunDef->GetFunction(),
                pVy );

            pClosureI->SetNotinline(pFunRef->IsNotinline());

            emitI(pClosureI);
            return emitLinkage(pRd);
        } // if FunDef

        Operand* pSx;
        if (FunPro* const pFunPro = pFunEnt->DynamicCast<FunPro>())
        {
            pSx = pFunPro->GetFunName();
        }
        else
        {
            pSx = Literal::New(cell);
        }

        Register* pRd = new Register;

        LoadFunI* const pLoadFunI =
            new LoadFunI(pFunRef->GetFunty(), pRd, pSx);

        pLoadFunI->SetNotinline(pFunRef->IsNotinline());

        emitI(pLoadFunI);
        //= <FIXME date="2007-09-16" by="yosi@msn.com">
        //=   Check unbound function
        //= </FIXME>
        return emitLinkage(pRd);
    } // if function_name_p

    if (consp(fname) && Qlambda == car(fname))
    {
        return parseLambdaExpr(pExpect, fname);
    } // if lambda

    parseError("Expect function name: ~S", fname);
    return parseLiteral(pExpect, nil);
} // function

defparser(labels)
{
    CHECK_SYNTAX(2, MaxFormLength, "(labels (binding*) decl* form*");

    LexEnv oBindEnv(m_pLexEnv->GetTy());
    LexEnvScope oLexScope(&oBindEnv);

    parseFunBinds(form);

    Val forms = parseDecls(cddr(form));

    deactivateLexEnv();

    foreach (LexEnv::EnumFun, oEnum, m_pLexEnv)
    {
        if (FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>())
        {
            activateFunDcl(pFunDef);
        }
    } // for each fun

    foreach (LexEnv::EnumFun, oEnum, m_pLexEnv)
    {
        if (FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>())
        {
            processFunDef(pFunDef);
        }
    } // for each fun

    activateFreeDcls();

    Operand* pSx = parseForms(pExpect, form, forms);

    closeLexEnv();

    return pSx;
} // labels

defparser(let)
{
    CHECK_SYNTAX(2, MaxFormLength, "(let (binding*) decl* form*)");

    LexEnv oBindEnv(m_pLexEnv->GetTy());
    LexEnvScope oLexScope(&oBindEnv);

    parseVarBinds(cadr(form));

    Val forms = parseDecls(cddr(form));

    deactivateLexEnv();

    OpenBindI* pOpenBindI = NULL;

    foreach (LexEnv::EnumVar, oEnum, m_pLexEnv)
    {
        VarLet* pVarLet = oEnum.Get()->DynamicCast<VarLet>();
        if (NULL == pVarLet) continue;

        Expect oExpect(pVarLet->GetTy());
        Operand* pSx = parseForm1(&oExpect, pVarLet->GetInitForm());

        if (Unreachable == pSx) return setUnreachable();

        pOpenBindI = emitBind(pVarLet, pSx, pOpenBindI);
    } // for each var

    return parseLetAux(pExpect, form, forms, pOpenBindI);
} // let

defparser(letA)
{
    CHECK_SYNTAX(2, MaxFormLength, "(let* (binding*) decl* form*)");

    LexEnv oBindEnv(m_pLexEnv->GetTy());
    LexEnvScope oLexScope(&oBindEnv);

    parseVarBinds(cadr(form));

    Val const forms = parseDecls(cddr(form));

    deactivateLexEnv();

    ClFrame* const pCurrFrame = m_pFrame;

    foreach (LexEnv::EnumVar, oEnum, m_pLexEnv)
    {
        VarLet* const pVarLet = oEnum.Get()->DynamicCast<VarLet>();
        if (NULL == pVarLet)
        {
            continue;
        }

        Expect oExpect(pVarLet->GetTy());
        Operand* const pSx = parseForm1(&oExpect, pVarLet->GetInitForm());

        if (Unreachable == pSx)
        {
            return setUnreachable();
        }

        if (OpenBindI* const pOpenI = emitBind(pVarLet, pSx))
        {
            emitI(pOpenI);
        }

        activateVarDcl(pVarLet);
    } // for each var

    activateFreeDcls();

    Operand* pSx;
    if (m_pFrame == pCurrFrame)
    {
        // No special variable binding
        pSx = parseForms(pExpect, form, forms);
    }
    else
    {
        BBlock* const pSucc = setContinue();
        pSx = parseForms(pExpect, form, forms);

        if (Unreachable != pSx)
        {
            Operand* const pSsave = emitWeakSaveValues(pSx);
            while (m_pFrame != pCurrFrame)
            {
                emitUnwind(popFrame());
            } // while
            pSx = emitWeakRestoreValues(pSsave);
        }

        restoreSucc(pSucc);
        emitLinkage(pSx);
    } // if

    closeLexEnv();
    return pSx;
} // letA

defparser(macrolet)
{
    CHECK_SYNTAX(2, MaxFormLength, "(macrolet (macrodef*) decl* form*)");
    Val bindings = cadr(form);

    if (lt(safe_list_length(bindings), 0))
    {
        parseError("Malformed function bindings: ~S", bindings);
        return ignoreForm(form);
    }

    LexEnv oBindEnv(m_pLexEnv->GetTy());
    LexEnvScope oLexScope(&oBindEnv);

    foreach (List::Enum, oEnum, bindings)
    {
        Val binding = oEnum.Get();

        if (lt(safe_list_length(binding), 0))
        {
            parseError("Malformed macro binding: ~S", binding);
            return ignoreForm(form);
        }

        Val name = car(binding);
        if (! symbolp(name))
        {
            parseError("Invalid macro name ~S.", name);
            name = make_symbol(make_string(L"err"));
        }

        Val expander_form = MakeMacroExpanderForm(
            name,
            cadr(binding),
            cddr(binding) );

        Val expanderFactory = funcall(Qcompile_form, expander_form);

        if (nil != expanderFactory)
        {
            Val expander = funcall(expanderFactory);
            FunMac* pFunMac = new FunMac(name, expander);
            addFunRef(pFunMac);
        }
    } // for each binding

    Val forms = parseDecls(cddr(form));

    Operand* pSx = parseForms(pExpect, form, forms);

    closeLexEnv();

    return pSx;
} // macrolet

defnyi(progv)

defparser(setq)
{
    CHECK_SYNTAX(0, MaxFormLength, "(setq {place val}*)")

    Operand* pSy = Literal::New(nil);

    Val runner = cdr(form);
    while (consp(runner))
    {
        Val place = pop(runner);

        if (nil == runner)
        {
            parseError("Missing value form for ~S.", place);
        }

        if (! consp(runner))
        {
            break;
        }

        if (Unreachable == pSy)
        {
            unreachableForm(place);
            continue;
        }

        if (! symbolp(place))
        {
            parseError("Expected variable name: ~S", place);
            place = make_symbol(make_string("err"));
        }

        Val value = pop(runner);

        {
            Val name = place;

            VarRef* pVarRef = internVarRef(name);

            if (pVarRef->IsIgnore())
            {
                styleWarn("Use ignored variable ~S", name);
            }

            VarRef* pVarEnt = pVarRef;
            while (pVarEnt->Is<VarDcl>())
            {
                pVarEnt = pVarEnt->GetOuter();
            } // while

            if (VarConst* pContst = pVarEnt->DynamicCast<VarConst>())
            {
                parseError("Atempt to alter constant ~S.", name);
                pSy = Unreachable;
            }
            else if (VarDef* pVarDef = pVarEnt->DynamicCast<VarDef>())
            {
                Expect oExpect(Qsetq, pVarRef->GetTy());
                pSy = parseForm1(&oExpect, value);
                if (Unreachable == pSy) continue;

                Val cell = pVarDef->GetCell();
                if (nil != cell)
                {
                    emitStoreVar(cell, pSy);
                }
                else
                {
                    pVarDef->Mark(VarDef::Flag_Write);

                    Output*   pRcell = internVarCell(pVarDef);
                    Variable* pVar   = pVarDef->GetVar();
                    Register* pRptr  = new Register(pVar);

                    emitI(
                        new SlotI(
                            tyPtrT,
                            pRptr,
                            CLASS_closed_cell,
                            Qvalue,
                            pRcell ) );

                    emitI(new StoreI(pRptr, pSy));
                }
            }
            else if (VarMac* pVarMac = pVarEnt->DynamicCast<VarMac>())
            {
                Expect oExpect(nil == runner ? pExpect->m_pty : tyVoid);
                place = pVarMac->GetExpansion();
                pSy = parseForm(&oExpect, list(Qsetf, place, value));
            }
            else if (VarPro* pVarPro = pVarEnt->DynamicCast<VarPro>())
            {
                Expect oExpect(Qsetq, pVarRef->GetTy());
                pSy = parseForm1(&oExpect, value);
                if (Unreachable == pSy)
                {
                    continue;
                }

                emitStoreVar(pVarPro->GetCell(), pSy);
            }
            else
            {
                COMPILER_INTERNAL_ERROR();
            }
        }
    } // for each place/val

    if (nil != runner)
    {
        parseError("Malformed form: ~S", form);
    }

    return emitLinkage(pSy);
} // setq

} // Compiler

} // TinyCl
