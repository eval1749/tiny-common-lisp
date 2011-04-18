#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 03 Evaluation and Compilation
// tinycl_c_cl_03_eval.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_03_eval.cpp#19 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

Operand* PassParse::parseCall(
    const Expect*   const pExpect,
    Val             const form,
    const Callee*   const pCallee,
    Val             const args )
{
    ASSERT(NULL != pExpect);
    ASSERT(NULL != pCallee);

    Values*  const pVy = new Values;

    bool const fApply = car(form) == Qapply;

    Instruction* const pValuesI = fApply ?
        static_cast<Instruction*>(new ValuesAI(pVy)) :
        static_cast<Instruction*>(new ValuesI(pVy));

    // Parse arguments
    {
        TyFunction::EnumParam oEnumParam(pCallee->m_pFunty);
        int iNth = 0;
        foreach (List::Enum, oEnum, args)
        {
            if (oEnumParam.AtEnd())
            {
                parseError("Too many arguments");
                return Unreachable;
            }

            const Type* const pty =
                fApply && nil == cdr(oEnum.GetCons()) ?
                tyList :
                oEnumParam.Get();

            ExpectArg oExpectArg(pCallee->m_name, iNth, pty);
            Val const argForm = oEnum.Get();
            Operand* const pSx = parseForm1(&oExpectArg, argForm);
            if (pSx->Is<UnreachableOutput>())
            {
                return unreachableForm(form);
            }

            pValuesI->AppendOperand(pSx);

            iNth += 1;
            oEnumParam.Next();
        } // for arg

        if (oEnumParam.IsRequired())
        {
            parseError("Too few arguments");
            return Unreachable;
        }
    }

    emitI(pValuesI);

    return processCall(pExpect, form, pCallee, pVy);
} // PassParse::parseCall

Operand* PassParse::parseCompoundForm(
    const Expect* const pExpect,
    Val           const form )
{
    Val const op = car(form);

    FunRef* const pFunRef = internFunRef(op);

    if (! pFunRef->IsNotinline())
    {
        ParserFn pfn = s_oParserTab.Get(op);
        if (NULL != pfn)
        {
            return pfn(this, pExpect, form);
        }
    }

    if (FunMac* pFunMac = pFunRef->DynamicCast<FunMac>())
    {
        Val expansion = callMacroExpander(
            pFunMac->GetExpander(),
            form );
        return parseForm(pExpect, expansion);
    }

    Callee oCallee;
    oCallee.m_fNotinline = pFunRef->IsNotinline();
    oCallee.m_pFunty     = pFunRef->GetFunty();
    oCallee.m_name       = op;

    FunRef* pFunEnt = pFunRef;
    while (pFunEnt->Is<FunDcl>())
    {
        pFunEnt = pFunEnt->GetOuter();
    }

    if (FunDef* const pFunDef = pFunEnt->DynamicCast<FunDef>())
    {
        // FIXME 2008-09-23 yosi@msn.com We should incorporate override
        // ftype.
        markFunUse(pFunDef);
        oCallee.m_pOperand = pFunDef->GetFunction();
    }
    else if (FunPro* const pFunPro = pFunEnt->DynamicCast<FunPro>())
    {
        oCallee.m_pFunty = pFunPro->GetTy()->StaticCast<TyFunction>();

        if (oCallee.m_fNotinline)
        {
            oCallee.m_pOperand = Literal::New(op);
        }
        else
        {
            oCallee.m_pOperand = pFunPro->GetFunName();

            Val const alist = pFunPro->GetAlist();
            Val const varary = assq(Qc_varary, alist);

            if (nil != varary)
            {
                Val const arity = length(cdr(form));
                Val const entry = assq(arity, cdr(varary));
                if (nil != entry)
                {
                    Val const fixop = cdr(entry);
                    FunRef* const pFunRef = findFunRef(fixop);
                    if (FunPro* const pFunPro =
                            pFunRef->DynamicCast<FunPro>() )
                    {
                        FunName* const pFunName = pFunPro->GetFunName();
                        oCallee.m_pOperand = pFunName;
                        oCallee.m_pFunty = pFunName->GetFunty();
                    }
                    else
                    {
                        oCallee.m_pOperand= Literal::New(fixop);
                    }

                    CLOG(1, "<li>Use ~W instead of ~W</li>", fixop, op);
                } // if
            }
            else
            {
                Val const typep = assq(Qc_type_predicate, alist);
                if (nil != typep)
                {
                    return parseTypePredicate(pExpect, form, cdr(typep));
                }
            }
        } // if notinline
    }
    else
    {
        oCallee.m_pOperand = Literal::New(op);
    }

    return parseFunctionForm(pExpect, form, &oCallee);
} // PassParse::parseCompoundForm

Operand* PassParse::parseCons(const Expect* pExpect, Val form)
{
    s_oParserTab.InitIfNeeded();

    rememberLineNumber(form);

    Val op = car(form);

    if (symbolp(op))
    {
        return parseCompoundForm(pExpect, form);
    }
    else if (consp(op) && car(op) == Qlambda)
    {
        // 3.1.2.1.2.4 Lambda Forms
        Callee oCallee;
        Function* pFun = parseLambda(op);
        oCallee.m_pOperand = pFun;
        oCallee.m_pFunty = pFun->GetFunty();
        return parseFunctionForm(pExpect, form, &oCallee);
    }

    parseError("Unexpected operator: ~S", op);
    return parseLiteral(pExpect, nil);
} // PassParse::parseCons

Operand* PassParse::parseForm(const Expect* pExpect, Val form)
{
    if (tyNil == pExpect->m_pty)
    {
        return unreachableForm(form);
    }

    if (consp(form)) return parseCons(pExpect, form);
    if (symbolp(form)) return parseSymbol(pExpect, form);
    return parseLiteral(pExpect, form);
} // PassParse::parseForm

Operand* PassParse::parseForm1(const Expect* pExpect, Val form)
{
    BBlock* pSaveBB = m_pSucc;
    Linkage eLinkage = getLinkage();
    m_pSucc = m_pCurr;

    Operand* pSx = parseForm(pExpect, form);

    if (Linkage_Next != eLinkage)
    {
        m_pSucc = pSaveBB;
    }

    return pSx;
} // PassParse::parseForm1

bool PassParse::parseForm2(
    const Expect*   pExpect1,
    const Expect*   pExpect2,
    Operand**       out_pSx,
    Operand**       out_pSy,
    Val             forms )
{
    *out_pSx = parseForm1(pExpect1, car(forms));
    if (Unreachable == *out_pSx) return false;

    *out_pSy = parseForm1(pExpect2, cadr(forms));
    if (Unreachable == *out_pSy) return false;

    return true;
} // PassParse::parseForm2

Operand* PassParse::parseForms(const Expect* pExpect, Val form, Val runner)
{
    if (nil == runner)
    {
        if (tyVoid == pExpect->m_pty) return emitLinkage(Void);
        return parseLiteral(pExpect, nil);
    }

    Expect oExpectVoid(tyVoid);

    BBlock* pSucc = setContinue();

    while (consp(runner))
    {
        if (nil == cdr(runner))
        {
            restoreSucc(pSucc);
            return parseForm(pExpect, car(runner));
        }

        parseForm(&oExpectVoid, car(runner));

        runner = cdr(runner);
    } // for

    m_pSucc = pSucc;

    parseError("Malformed form: ~S", form);
    return parseLiteral(pExpect, nil);
} // PassParse::parseForms

Operand* PassParse::parseFunctionForm(
    const Expect*   const pExpect,
    Val             const form,
    const Callee*   const pCallee )
{
    return parseCall(pExpect, form, pCallee, cdr(form));
} // parseFunctionForm

Operand* PassParse::parseLiteral(
    const Expect* const pExpect,
    Val           const form )
{
    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }

    if (TyValues* const ptyValues = pExpect->m_pty->DynamicCast<TyValues>())
    {
        TyValues::Enum oEnumTy(ptyValues);
        if (oEnumTy.AtEnd())
        {
            parseError("Expect (values) instead of ~S", form);
        }
        else
        {
            if (! oEnumTy.Get()->IsType(form))
            {
                parseError(pExpect, form, Type::Parse(type_of(form)));
            }

            oEnumTy.Next();
            parseMissingValues(pExpect, &oEnumTy, NULL);
        }
    }
    else
    {
        if (! pExpect->m_pty->IsType(form))
        {
            parseError(pExpect, form, Type::Parse(type_of(form)));

            // REVIEW 2008-08-17 yosi@msn.com We should return Unrechable
            // instead of nil. When we return nil, we got AV.
            // See (defun foo (&optional x) (declare (type fixnum x)) x)
        }
    } // if

    return emitLinkage(Literal::New(form));
} // PassParse::parseLiteral

Operand* PassParse::parseSymbol(
    const Expect* const pExpect,
    Val           const symb )
{
    if (keywordp(symb))
    {
        return parseLiteral(pExpect, symb);
    }

    VarRef* pVarRef = internVarRef(symb);

    if (pVarRef->IsIgnore())
    {
        styleWarn("Use ignored variable ~S.", symb);
        pVarRef->Unmark(VarRef::Flag_Ignore);
    }

    markVarUse(pVarRef);

    const Type* ptyVal = pExpect->m_pty;

    if (const TyValues* const ptyValues = ptyVal->DynamicCast<TyValues>())
    {
        TyValues::Enum oEnumTy(ptyValues);
        if (oEnumTy.AtEnd())
        {
            parseError("Expect (values)");
            return emitLinkage(Literal::New(nil));
        }

        ptyVal = oEnumTy.Get();
        oEnumTy.Next();
        parseMissingValues(pExpect, &oEnumTy, NULL);
    } // if

    ptyVal = Type::And(ptyVal, pVarRef->GetTy());

    if (tyNil == ptyVal)
    {
        parseError(pExpect, symb, pVarRef->GetTy());
        return emitLinkage(Literal::New(nil));
    }

    VarRef* pVarEnt = pVarRef;
    while (pVarEnt->Is<VarDcl>())
    {
        pVarEnt = pVarEnt->GetOuter();
    } // while

    // Lexical variable
    if (VarDef* pVarDef = pVarEnt->DynamicCast<VarDef>())
    {
        if (tyVoid == pExpect->m_pty)
        {
            return ignoreForm(symb);
        }

        Val cell = pVarDef->GetCell();
        if (nil != cell)
        {
            Register* pRx = emitLoadVar(pVarRef->GetTy(), cell);
            return emitLinkage(pExpect, pRx);
        }

        Output*  pRcell = internVarCell(pVarDef);
        Variable* pVar  = pVarDef->GetVar();
        Register* pR1   = new Register;
        Register* pR2   = new Register(pVar);
        emitI(new SlotI(tyPtrT, pR1, CLASS_closed_cell, Qvalue, pRcell));
        emitI(new LoadI(pVarRef->GetTy(), pR2, pR1));
        return emitLinkage(pExpect, pR2);
    } // if lexical

    if (VarConst* pConst = pVarEnt->DynamicCast<VarConst>())
    {
        return parseLiteral(pExpect, pConst->GetValue());
    } // if constant

    // Special variable
    if (VarPro* pVarPro = pVarEnt->DynamicCast<VarPro>())
    {
        if (tyVoid == pExpect->m_pty)
        {
            return ignoreForm(symb);
        }

        Register* const pR1 = emitLoadVar(tyT, pVarPro->GetCell());
        Register* const pR2 = emitCast(pVarRef->GetTy(), pR1);
        return emitLinkage(pExpect, pR2);
    } // if special

    // Symbol-macro
    if (VarMac* pVarMac = pVarEnt->DynamicCast<VarMac>())
    {
        return parseForm(pExpect, pVarMac->GetExpansion());
    }

    COMPILER_INTERNAL_ERROR();
    return emitLinkage(Literal::New(symb));
} // PassParse::parseSymbol

Operand* PassParse::processCall(
    const Expect* const pExpect,
    Val           const form,
    const Callee* const pCallee,
    Values*       const pVy )
{
    const TyFunction* pFunty = pCallee->m_pFunty;

    const Type* pValueTy = pFunty->GetValueTy();
    if (tyNil == pValueTy)
    {
        emitCall(tyVoid, Void, pCallee, pVy);
        emitUnreachable();
        return emitLinkage(Unreachable);
    } // if nil

    if (pExpect->m_pty == tyVoid)
    {
        // We don't care value of callee.
        emitCall(tyVoid, Void, pCallee, pVy);
        return emitLinkage(Void);
    }

    if (TyValues* const ptyExpects = pExpect->m_pty->DynamicCast<TyValues>())
    {
        TyValues::Enum oEnumTy(ptyExpects);

        if (oEnumTy.AtEnd())
        {
            parseError("Expect (values)");
            return emitLinkage(Literal::New(nil));
        }

        if (TyValues* const pValueTys = pValueTy->DynamicCast<TyValues>())
        {
            // values <= values
            Val const ty = list(Qvalues);
            Val tail = ty;
            TyValues::Enum  oEnumTy2(pValueTys);

            enum
            {
                State_Opt,
                State_Req,
                State_Rest,
            } eState = State_Req;

            for (;;)
            {
                if (oEnumTy.AtEnd())
                {
                    // We don't care about extra values.
                    break;
                }

                if (oEnumTy2.AtEnd())
                {
                    // expect > values
                    parseMissingValues(pExpect, &oEnumTy, NULL);
                    break;
                }

                switch (eState)
                {
                case State_Req:
                    if (oEnumTy2.IsOptional())
                    {
                        tail = setf_cdr(list(QAoptional), tail);
                        eState = State_Opt;
                    }
                    else if (oEnumTy2.IsRest())
                    {
                        tail = setf_cdr(list(QArest), tail);
                        eState = State_Rest;
                    }
                    break;

                case State_Opt:
                    if (oEnumTy2.IsRest())
                    {
                        tail = setf_cdr(list(QArest), tail);
                        eState = State_Rest;
                    }
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // switch eState

                const Type* const pty = Type::And(
                    oEnumTy2.Get(),
                    oEnumTy.Get() );

                if (tyNil == pty)
                {
                    parseError(pExpect, form, pValueTy);
                    return emitLinkage(Literal::New(nil));
                }

                tail = setf_cdr(list(pty->Unparse()), tail);
                oEnumTy.Next();
                oEnumTy2.Next();

                if (State_Rest == eState)
                {
                    break;
                }
            } // for

            //= <FIXME date="2009-01-01" by="yosi@msn.com">
            //=   Check function value against exepcted type.
            //= </FIXME>

            Values* pVd = new Values;
            emitCall(Type::Parse(ty), pVd, pCallee, pVy);
            return emitLinkage(pVd);
        }
        else
        {
            // values <= single
            Register* const pR1 = new Register;
            emitCall(pValueTy, pR1, pCallee, pVy);

            if (Type::And(oEnumTy.Get(), pValueTy) == tyNil)
            {
                parseError(pExpect, form, pValueTy);
                return emitLinkage(Literal::New(nil));
            }

            Register* const pR2 = emitCast(
                ptyExpects->GetNthTy(0),
                pR1 );

            oEnumTy.Next();

            Values* const pVd = new Values;
            ValuesI* const pValuesI = new ValuesI(pVd, pR2);

            parseMissingValues(pExpect, &oEnumTy, pValuesI);

            emitI(pValuesI);
            return emitLinkage(pVd);
        } // if
    }
    else
    {
        if (TyValues* const pValueTys = pValueTy->DynamicCast<TyValues>())
        {
            // single <= values
            TyValues::Enum oEnum(pValueTys);
            if (oEnum.AtEnd())
            {
                // single <= (values)
                emitCall(tyVoid, Void, pCallee, pVy);
                return emitLinkage(Literal::New(nil));
            }

            pValueTy = oEnum.Get();
        } // if

        // single <= single
        Register* const pRd = new Register;
        emitCall(pValueTy, pRd, pCallee, pVy);

        const Type* ptyResult = Type::And(pExpect->m_pty, pValueTy);

        if (ptyResult == tyNil)
        {
            parseError(pExpect, form, pValueTy);
            return emitLinkage(Literal::New(nil));
        }

        return emitLinkage(pExpect, pRd);
    } // if
} // PassParse::processCall

defparser(declare)
{
    ASSERT(NULL != pExpect);
    parseError("Can't use ~S as form.", form);
    return Unreachable;
} // declare

defparser(eval_when)
{
    // FIXME 2008-08-03 yosi@msn.com NYI Compile eval-when
    CHECK_SYNTAX(2, MaxFormLength, "(eval-when (situation*) form*)");
    return parseForms(pExpect, form, cddr(form));
} // eval_when

defnyi(load_time_value)

defparser(locally)
{
    CHECK_SYNTAX(1, MaxFormLength, "(locally decl* form*)");

    Operand* pSx;
    {
        LexEnv oLexEnv(pExpect->m_pty);
        LexEnvScope oScope(&oLexEnv);
        Val forms = parseDecls(cdr(form), false);
        pSx = parseForms(pExpect, form, forms);
    }
    return pSx;
} // locally

defnyi(symbol_macrolet)

/// <summary>
///   Parse special form THE.
/// </summary>
defparser(the)
{
    CHECK_SYNTAX(3, 3, "(the type form)");

    const Type* pty = Type::And(pExpect->m_pty, Type::Parse(cadr(form)));
    if (tyNil == pty)
    {
        return incompatibleTypes(pty, pExpect->m_pty, form);
    }

    Expect oExpect(pty);
    Operand* const pSx = parseForm(&oExpect, caddr(form));
    if (Unreachable == pSx)
    {
        return ignoreForm(form);
    }

    return emitLinkage(pSx);
} // the

} // Compiler
} // TinyCl
