#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 03 Evaluation and Compilation
// tinycl_c_cl_03_lambda.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_03_lambda.cpp#8 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

const TyFunction* PassParse::computeFunty(
    const LambdaList* const pLambdaList,
    const Type*       const ptyValue )
{
    Val lty = list(nil);
    Val tail = lty;
    foreach (Params::Enum, oEnum, &pLambdaList->m_oReqs)
    {
        tail = setf_cdr(list(oEnum.Get()->GetTy()->Unparse()), tail);
    } // for

    if (! pLambdaList->m_oOpts.IsEmpty())
    {
        tail = setf_cdr(list(QAoptional), tail);
        foreach (Params::Enum, oEnum, &pLambdaList->m_oOpts)
        {
            tail = setf_cdr(list(oEnum.Get()->GetTy()->Unparse()), tail);
        } // for
    } // if &optional

    if (nil == pLambdaList->m_key)
    {
        if (! pLambdaList->m_oRests.IsEmpty())
        {
            tail = setf_cdr(list(QArest), tail);
            foreach (Params::Enum, oEnum, &pLambdaList->m_oRests)
            {
                tail = setf_cdr(list(t), tail);
            } // for
        } // if &rest
    }
    else
    {
        tail = setf_cdr(list(QAkey), tail);
        foreach (Params::Enum, oEnum, &pLambdaList->m_oKeys)
        {
            Val const elt = list(
                oEnum.Get()->m_key,
                oEnum.Get()->GetTy()->Unparse() );

            tail = setf_cdr(list(elt), tail);
        } // for rest

        if (QAallow_other_keys == pLambdaList->m_key)
        {
            tail = setf_cdr(list(QAallow_other_keys), tail);
        }
    } // if &key

    Val const funty = list(
        Qfunction,
        cdr(lty),
        ptyValue->Unparse() );

    return TyFunction::Parse(funty);
} // PassParse::computeFunty


PassParse::VarParam* PassParse::newParam(Val name)
{
    name = ensureVarName(name);
    Variable* pVar   = new Variable(name);
    VarParam* pParam = new(this) VarParam(pVar);
    pParam->SetCell(findVarCell(name));
    addVarRef(pParam);
    return pParam;
} // PassParse::newParam

PassParse::VarLet* PassParse::newVarLet(Val name, Val initform)
{
    name = ensureVarName(name);
    Variable* pVar = new Variable(name);
    VarLet* pVarLet = new(this) VarLet(pVar, initform);
    pVarLet->SetCell(findVarCell(name));
    addVarRef(pVarLet);
    return pVarLet;
} // PassParse::newVarLet

/// <summary>
///   Parses lambda form.
/// </summary>
/// <param name="form">Lambda form.</param>
/// <returns>A Function object of parsed result of lambda form.</returns>
Function* PassParse::parseLambda(Val form)
{
    Function* pFun = new Function(Function::Flavor_Anonymous);

    {
        OwnerScope oOwner(pFun);

        {
            LambdaList oLambdaList;
            LexEnv oLexEnv(tyValuesRestT, one);

            LexEnvScope oScope(&oLexEnv);

            Val forms = parseLambdaList(form, &oLambdaList);

            if (one != oLexEnv.m_name)
            {
                pFun->m_name = oLexEnv.m_name;
            }

            pFun->SetFunty(computeFunty(&oLambdaList, oLexEnv.GetTy()));

            processLambdaExpr(&oLambdaList, form, forms);

            closeLexEnv();
        }
    }

    return pFun;
} // PassParse::parseLambda

// 3.1.3 Lambda Expressions
Operand* PassParse::parseLambdaExpr(const Expect* pExpect, Val form)
{
    if (! checkSyntax(2, 0, "Invalid lambda form", form))
    {
        return emitLinkage(Unreachable);
    }

    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }

    Function* const pFun = parseLambda(form);

    if (pFun->IsNotClosure())
    {
        return emitLinkage(pFun);
    }

    Values* const pVy = new Values;
    emitI(new ValuesI(pVy));

    Register* const pRd = new Register;
    emitI(new ClosureI(pFun->GetFunty(), pRd, pFun, pVy));

    return emitLinkage(pRd);
} // PassParse::parseLambdaExpr

Val PassParse::parseLambdaList(Val form, LambdaList* pLambdaList)
{
    Val runner = cadr(form);

    int iMax = 0;
    int iMin = 0;

    enum State
    {
        State_Req,
        State_Opt,
        State_Rest,
        State_RestAfter,
        State_Key,
        State_AllowOtherKeys,
        State_Aux,
    } eState = State_Req;

    while (consp(runner))
    {
        Val token = pop(runner);

        switch (eState)
        {
        case State_Req:
            // 1/5 Required parameters
            if (QAoptional == token)
            {
                eState = State_Opt;
            }
            else if (QArest == token)
            {
                eState = State_Rest;
            }
            else if (QAkey == token)
            {
                pLambdaList->m_key = token;
                eState = State_Key;
            }
            else if (QAaux == token)
            {
                eState = State_Aux;
            }
            else
            {
                pLambdaList->m_oReqs.Append(newParam(token));
                iMax += 1;
                iMin += 1;
            }
            break;

        case State_Opt:
            // 2/5 Optional parameters
            if (QArest == token)
            {
                eState = State_Rest;
            }
            else if (QAkey == token)
            {
                pLambdaList->m_key = token;
                eState = State_Key;
            }
            else if (QAaux == token)
            {
                eState = State_Aux;
            }
            else
            {
                VarDef* pSVarDef = NULL;

                Val name     = Fixnum::Encode(0);
                Val initform = nil;

                if (symbolp(token))
                {
                    name = ensureVarName(token);
                }
                else
                {
                    switch (Fixnum::Decode_(safe_list_length(token)))
                    {
                    case 3:
                    {
                        pSVarDef = newVarLet(caddr(token));
                        // FALLTHROUGH
                    } // 3

                    case 2:
                        initform = cadr(token);
                        // FALLTHROUGH

                    case 1:
                        name = car(token);
                        break;
                    } // switch length
                } // if

                if (! symbolp(name))
                {
                    parseError("Invalid optional parameter: ~S", token);
                    name = make_symbol(make_string(L"err"));
                }

                VarParam* pParam = newParam(name);
                pParam->m_pSVarDef = pSVarDef;
                pParam->m_initform = initform;

                pLambdaList->m_oOpts.Append(pParam);
                iMax += 1;
            } // if
            break;

        case State_Rest:
            // 3/5 Rest parameter
            pLambdaList->m_oRests.Append(newParam(token));
            eState = State_RestAfter;
            break;

        case State_RestAfter:
            if (QAkey == token)
            {
                pLambdaList->m_key = token;
                eState = State_Key;
            }
            else if (QAaux == token)
            {
                eState = State_Aux;
            }
            else
            {
                parseError("More than ore symbol after &rest.");
                runner = nil;
            }
            break;

        case State_Key:
            // 4/5 Keyword parameters
            if (QAallow_other_keys == token)
            {
                pLambdaList->m_key = token;
                eState = State_AllowOtherKeys;
            }
            else if (QAaux == token)
            {
                eState = State_Aux;
            }
            else
            {
                Val name = Fixnum::Encode(0);
                Val initform = nil;

                VarLet* pSVarDef = NULL;

                if (symbolp(token))
                {
                    name = token;
                }
                else if (! consp(token))
                {
                    // invalid keyword parameter spec
                }
                else if (nil == cdr(token))
                {
                    name = car(token);
                }
                else if (consp(cdr(token)) && nil == cddr(token))
                {
                    name = car(token);
                    initform = cadr(token);
                }
                else if (consp(cdr(token)) && consp(cddr(token)) &&
                         nil == cdddr(token) )
                {
                    name = car(token);
                    initform = cadr(token);

                    pSVarDef = newVarLet(caddr(token));
                } // if

                Val key;
                if (symbolp(name))
                {
                    key = intern(
                        name->StaticCast<Symbol>()->m_name,
                        PKG_keyword );
                }
                else if (consp(name) &&
                         consp(cdr(name)) &&
                         nil == cddr(name) )
                {
                    key = car(name);
                    name = cadr(name);
                }
                else
                {
                    key = Fixnum::Encode(0);
                }

                unless (symbolp(name))
                {
                    parseError("Invalid keyword parameter ~S.", token);
                    name = Qerror;
                    key  = name;
                }

                VarParam* pParam = newParam(name);
                pParam->m_initform = initform;
                pParam->m_key      = key;
                pParam->m_pSVarDef = pSVarDef;

                pLambdaList->m_oKeys.Append(pParam);
            } // if
            break;

        case State_AllowOtherKeys:
            if (QAaux == token)
            {
                eState = State_Aux;
            }
            else
            {
                parseError("Extra name after &allow-other-keys: ~S", token);
            }
            break;

        case State_Aux:
        {
            // 5/5 Aux variables
            Val name = Fixnum::Encode(0);
            Val initform = nil;

            if (symbolp(token))
            {
                name = token;
            }
            else if (! consp(token))
            {
                // invalid aux
            }
            else if (nil == cdr(token))
            {
                name = car(token);
            }
            else if (consp(cdr(token)) && nil == cddr(token))
            {
                name = car(token);
                initform = cadr(token);
            }

            if (! symbolp(name))
            {
                name = Qerror;
                parseError("Invalid &aux parameter: ~S", token);
            }

            VarParam* pParam = newParam(name);
            pParam->m_initform = initform;
            pLambdaList->m_oAuxs.Append(pParam);
            break;
        } // State_Aux

        default:
            CAN_NOT_HAPPEN();
            // NOTREACHED
        } // switch estate
    } // while

    if (State_Rest == eState)
    {
        parseError("Missing symbol after &rest.");
    }
    else if (nil != runner)
    {
        parseError("Malformed lambda list: ~S", cadr(form));
    }

    if (nil != pLambdaList->m_key && pLambdaList->m_oRests.IsEmpty())
    {
        VarParam* pParam = newParam(QArest);
        pLambdaList->m_oRests.Append(pParam);
        pParam->Mark(VarParam::Flag_DynamicExtent);
        pParam->Mark(VarParam::Flag_Ignorable);
        // set dynamic-exten
    }

    Val forms = parseDecls(cddr(form));

    if (VarParam* pParam = pLambdaList->m_oRests.GetFirst())
    {
        // FIXME 2008-08-17 yosi@msn.com We should signal style-warning
        // for type declaration of rest parameter.
        pParam->SetTy(tyList);
    }

    deactivateLexEnv();

    return forms;
} // PassParse::parseLambdaList

void PassParse::processFunDef(FunDef* pFunDef)
{
    pFunDef->SetTy(
        computeFunty(
            &pFunDef->m_oLambdaList,
            pFunDef->m_oLexEnv.GetTy() ) );


    {
        OwnerScope oOwner(pFunDef->GetFunction());

        LexEnvScope oLexEnv(&pFunDef->m_oLexEnv);

        processLambdaExpr(
            &pFunDef->m_oLambdaList,
            pFunDef->m_form,
            pFunDef->m_forms );

        closeLexEnv();
    }

    pFunDef->SetTy(pFunDef->GetFunction()->GetFunty());
} // PassParse::processFunDef

/// <summary>
///   Process lambda expression.
/// </summary>
/// <param name="form">
///   One of following:
///   <list>
///     <item><term>(lambda lambda-list ...)</term>
///       <description>if flavor is Flavor_Anonymous</description>
///     </item>
///     <item><term>(fname lambda-list ...)</term>
///       <description>if flavor is Flavor_Named</description>
///     </item>
///   </list>
/// </param>
void PassParse::processLambdaExpr(
    LambdaList* const pLambdaList,
    Val         const form,
    Val         const forms )
{
    ASSERT(NULL != pLambdaList);

    ClFrame* const pOuterFrame = m_pFrame;

    processLambdaList(
        pLambdaList,
        m_pOwner->GetStartBB()->GetFirstI()->GetVd() );

    activateFreeDcls();

    BBlock* const pSucc = setContinue();

    Operand* pSx;
    Expect const oExpect(m_pLexEnv->GetTy());
    if (m_pOwner->GetFlavor() == Function::Flavor_Named)
    {
        Val bname = car(form);
        if (consp(bname)) bname = cadr(bname);
        pSx = parseBlock(&oExpect, form, bname, forms);
    }
    else
    {
        pSx = parseForms(&oExpect, form, forms);
    }

    pSx = optimizeLexVars(pSx);

    m_pSucc = pSucc;

    // Pop bind frames created by lambda-list.
    if (Unreachable != pSx)
    {
        Operand* pRsave = emitWeakSaveValues(pSx);
        emitUnwinds(pOuterFrame);
        pSx = emitWeakRestoreValues(pRsave);
    } // if

    m_pFrame = pOuterFrame;

    emitLinkage(pSx);
} // PassParse::processLambdaExpr

/// <summary>
///   Emit code for lambda-list processing. This code picks a paramter from
///   values register.
/// </summary>
/// <param name="pLambdaList">A lambda-list</param>
/// <param name="pVx">A values register contains parameters.</param>
void PassParse::processLambdaList(
    LambdaList* pLambdaList,
    Values*     pVx )
{
    Register* pRn = NULL;

    // Prepare
    {
        Val const ty = list(Qvalues);
        Val tail = ty;

        int iNth = 0;

        foreach (Params::Enum, oEnum, &pLambdaList->m_oReqs)
        {
            VarParam* const pParam = oEnum.Get();
            processParam(pParam, pVx, iNth);
            tail = setf_cdr(list(t), tail);
            iNth += 1;
        } // for each req

        int iMin = iNth;

        if (! pLambdaList->m_oOpts.IsEmpty())
        {
            tail = setf_cdr(list(QAoptional), tail);
        }

        foreach (Params::Enum, oEnum, &pLambdaList->m_oOpts)
        {
            VarParam* const pParam = oEnum.Get();
            processParam(pParam, pVx, iNth);
            tail = setf_cdr(list(t), tail);
            iNth += 1;
        } // for each req

        m_pOwner->SetArity(
            iMin,
            iNth,
            NULL != pLambdaList->m_oRests.GetFirst() );

        if (! pLambdaList->m_oOpts.IsEmpty())
        {
            pRn = new Register;
            emitI(new CountI(pRn, pVx));
        }

        if (VarParam* pParam = pLambdaList->m_oRests.GetFirst())
        {
            Register* const pRd = new Register(pParam->GetVar());
            emitI(new SelectI(tyList, pRd, pVx, iNth));
            tail = setf_cdr(list(Qlist), tail);
            pParam->m_pRx = pRd;
        }

        m_pOwner->GetPrologueI()->SetTy(Type::Parse(ty));
    } // Prepare

    int iNth = 0;
    BBlock* pSucc = m_pSucc;

    // Required parameters
    {
        OpenBindI* pOpenI = NULL;
        foreach (Params::Enum, oEnum, &pLambdaList->m_oReqs)
        {
            VarParam* const pParam = oEnum.Get();

            Register* const pRx =
                emitCast(pParam->GetTy(), pParam->m_pRx);

            pOpenI = emitBind(pParam, pRx, pOpenI);
            activateVarDcl(pParam);
            iNth += 1;
        } // for each req

        if (NULL != pOpenI)
        {
            emitI(pOpenI);
        }
    }

    // Optional parameters
    foreach (Params::Enum, oEnum, &pLambdaList->m_oOpts)
    {
        VarParam* const pParam = oEnum.Get();
        Bool* const pBx = new Bool;
        emitI(new GtI(pBx, pRn, Literal::New(Fixnum::Encode(iNth))));
        iNth += 1;
        processLambdaListOptional(pSucc, pParam, pBx);
        activateVarDcl(pParam);

        if (VarDef* const pSVarDef = pParam->m_pSVarDef)
        {
            activateVarDcl(pSVarDef);
        }
    } // for each opt

    if (VarParam* pParam = pLambdaList->m_oRests.GetFirst())
    {
        if (OpenBindI* const pOpenI = emitBind(pParam, pParam->m_pRx))
        {
            emitI(pOpenI);
        }

        activateVarDcl(pParam);

        m_pOwner->GetPrologueI()->GetOperandBox(1)->Replace(
            Literal::New(pParam->IsDynamicExtent() ?
                QBstack_restify :
                QBrestify ) );
    } // if rest

    if (nil != pLambdaList->m_key)
    {
        Values* const pVKeys = new Values;

        ParseKeysI* const pParseKeysI = new ParseKeysI(
            pVKeys,
            pLambdaList->m_oRests.GetFirst()->m_pRx,
            pLambdaList->m_key );

        foreach (Params::Enum, oEnum, &pLambdaList->m_oKeys)
        {
            Val const key = oEnum.Get()->m_key;
            pParseKeysI->AppendOperand(Literal::New(key));
        }

        emitI(pParseKeysI);

        Register* const pRflags = new Register;
        emitI(new SelectI(tyFixnum, pRflags, pVKeys, 0));

        foreach (Params::Enum, oEnum, &pLambdaList->m_oKeys)
        {
            VarParam* const pParam = oEnum.Get();
            pParam->m_pRx = new Register(pParam->GetVar());
            emitI(new KeyValI(pParam->m_pRx, pVKeys, pParam->m_key));
        } // for each key

        foreach (Params::Enum, oEnum, &pLambdaList->m_oKeys)
        {
            VarParam* const pParam = oEnum.Get();
            Bool* const pBx = new Bool;
            emitI(new KeySuppliedI(pBx, pRflags, pParam->m_key));
            processLambdaListOptional(pSucc, pParam, pBx);
            activateVarDcl(pParam);
            if (VarDef* const pSVarDef = pParam->m_pSVarDef)
            {
                activateVarDcl(pSVarDef);
            }
        } // for each key
    } // if key

    // Auxilliary
    foreach (Params::Enum, oEnum, &pLambdaList->m_oAuxs)
    {
        VarParam* const pParam = oEnum.Get();
        Expect const oExpect(pParam->GetVar()->GetName(), pParam->GetTy());
        Operand* const pSx = parseForm1(&oExpect, pParam->m_initform);
        if (Unreachable == pSx)
        {
            break;
        }

        if (OpenBindI* const pOpenI = emitBind(pParam, pSx))
        {
            emitI(pOpenI);
        }

        activateVarDcl(pParam);
    } // for each aux
} // PassParse::processLambdaList

/// <summary>
///   Emit code for optional parameter and keyword paramter.
/// </summary>
void PassParse::processLambdaListOptional(
    BBlock*     pSucc,
    VarParam*   pParam,
    Bool*       pBx )
{
    Register* pRd   = new Register(pParam->GetVar());
    Register* pRsup = NULL;

    if (constantp(pParam->m_initform))
    {
        Expect oExpect(pParam->GetTy());
        Operand*  pSz = parseForm1(&oExpect, pParam->m_initform);
        Register* pR2 = new Register(pParam->GetVar());
        emitI(new IfI(pR2, pBx, pParam->m_pRx, pSz));

        // Note: We don't need to check type if parameter isn't supplied.
        // Since literal value is an object of specified type.
        pRd = emitCast(pParam->GetTy(), pR2);

        if (NULL != pParam->m_pSVarDef)
        {
            pRsup = emitBool(pBx);
        }
    }
    else
    {
        BBlock* pGotBB  = newBBlock();
        BBlock* pNotBB  = newBBlock();
        BBlock* pJoinBB = newBBlock();

        emitI(new BranchI(pBx, pGotBB, pNotBB));

        setCurrSucc(pGotBB, pGotBB);

        Register* pRgot = emitCast(
            pParam->GetTy(),
            pParam->m_pRx );

        emitI(new JumpI(pJoinBB));

        setCurrSucc(pNotBB, pNotBB);
        Expect oExpect(pParam->GetTy());
        Operand* pSnot = parseForm1(&oExpect, pParam->m_initform);
        if (Unreachable == pSnot)
        {
            pRd = pRgot;
            setCurrSucc(pJoinBB, pSucc);
        }
        else
        {
            emitI(new JumpI(pJoinBB));

            setCurrSucc(pJoinBB, pSucc);

            {
                PhiI* pPhiI = new PhiI(pParam->GetTy(), pRd);
                emitI(pPhiI);
                pPhiI->AddOperand(pGotBB, pRgot);
                pPhiI->AddOperand(pNotBB, pSnot);
            }

            if (NULL != pParam->m_pSVarDef)
            {
                pRsup = new Register;
                PhiI* pPhiI = new PhiI(tyT, pRsup);
                emitI(pPhiI);
                pPhiI->AddOperand(pGotBB, True);
                pPhiI->AddOperand(pNotBB, Literal::New(nil));
            }
        } // if
    } // if

    OpenBindI* pOpenI = emitBind(pParam, pRd);

    if (NULL != pParam->m_pSVarDef)
    {
        pOpenI = emitBind(pParam->m_pSVarDef, pRsup, pOpenI);
    }

    if (NULL != pOpenI)
    {
        emitI(pOpenI);
    }
} // PassParse::processLambdaListOptional

Register* PassParse::processParam(VarParam* pParam, Values* pVx, int iNth)
{
    Register* pRd = new Register(pParam->GetVar());
    emitI(new SelectI(tyT, pRd, pVx, iNth));
    pParam->m_pRx = pRd;
    return pRd;
} // PassParse::processParam

/// <summary>
///   An entry point of parsing lambda expression.
/// </summary>
defparser(lambda)
{
    return parseLambdaExpr(pExpect, form);
} // lambda

} // Compiler

} // TinyCl
