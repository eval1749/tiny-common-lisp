#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Macros
// tinycl_macro.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_macro.cpp#20 $
//
#include "./tinycl.h"

namespace TinyCl
{

namespace Private
{

static Val mapcar(Val (*)(Val, Val), Val, Val);

/// <summary>
///   The backquote parser
/// </summary>
class Backquote
{
    private: typedef bool (*Predicate)(Val);
    private: typedef Val  (*Fn1)(Val);

    private: Val m_codes;

    // Entry Point
    public: static Val Run(Val const form)
    {
        Backquote oProcessor;
        oProcessor.parse(form, 1);
        return oProcessor.assemble();
    } // Run
    
    // ctro
    private: Backquote() :
        m_codes(nil) {}

    // [A]
    private: Val assemble()
    {
        Val astack = nil;   // argument stack
        Val ostack = nil;   // operator stack
        Val args   = nil;
        foreach (List::Enum, oEnum, m_codes)
        {
            Val const code = oEnum.Get();
            Val const opcode = car(code);

            if (opcode == Qeval)
            {
                push(cdr(code), args);
            }
            else if (opcode == Qfuncall)
            {
                Val arg = cons(pop(ostack), args);
                args = pop(astack);
                push(arg, args);
            }
            else if (opcode == Qquote)
            {
                Val datum = cdr(code);
                if (needQuote(datum))
                {
                    datum = list(Qquote, datum);
                }
                push(datum, args);
            }
            else
            {
                push(args, astack);

                if (opcode == Qvector)
                {
                    push(Qcoerce, ostack);
                    args = list(list(Qquote, Qvector));
                }
                else
                {
                    push(opcode, ostack);
                    args = nil;
                }
            }
        } // for each code
        return car(args);
    } // assemble

    // [E]
    private: void emit(Val const opcode)
        { emit(opcode, nil); }

    private: void emit(Val const opcode, Val const operand)
    {
        if (opcode == Qappend || opcode == Qnconc)
        {
            Val const last1 = car(m_codes);
            Val const last2 = cadr(m_codes);
            if (car(last2) == Qfuncall)
            {
                pop(m_codes);
                setf_car(car(last1), last2);
                setf_cdr(cdr(last1), last2);
            }
            else
            {
                emitAux(opcode, operand);
            }
            return;
        } // if append

        if (opcode == Qlist || opcode == Qcons || opcode == QlistA)
        {
            Val runner = m_codes;
            Val elts = nil;
            bool fFirst = opcode != Qlist;
            for (;;)
            {
                Val code = car(runner);
                if (car(code) == Qfuncall)
                {
                    setf_car(Qquote, code);
                    setf_cdr(elts, code);
                    m_codes = runner;
                    break;
                }

                if (car(code) != Qquote)
                {
                    emitAux(opcode, operand);
                    break;
                }

                if (fFirst)
                {
                    elts = cdr(code);
                    fFirst = false;
                }
                else
                {
                    push(cdr(code), elts);
                }

                runner = cdr(runner);
            } // for
            return;
        } // if cons

        if (opcode == Qvector)
        {
            Val const last = car(m_codes);
            if (car(last) == Qquote)
            {
                pop(m_codes);
                setf_car(Qquote, caar(m_codes));
            }
            else
            {
                emitAux(opcode, operand);
            }
            return;
        } // if vector

        emitAux(opcode, operand);
    } // emit

    private: void emitAux(Val opcode, Val operand)
    {
        push(cons(opcode, operand), m_codes);
    } // emitAux

    // [I]
    private: static bool isQuote(Val const x, Val const op)
    {
        if (atom(x))
        {
            return false;
        }

        if (car(x) != op)
        {
            return false;
        }

        unless (consp(cdr(x)) && nil == cddr(x))
        {
            error("Malformed ~S", x);
        }
        return true;
    } // isQuote

    private: static bool isSplicing(Val form)
    {
        return isQuote(form, Qunquote_splicing) ||
               isQuote(form, Qunquote_nsplicing);
    } // isSplicing

    // [N]
    private: static bool needQuote(Val const form)
    {
        return symbolp(form) || consp(form);
    } // needQuote

    // [P]
    private: void parse(Val const x, int const iLevel)
    {
        if (0 == iLevel)
        {
            emit(Qeval, x);
        }
        else if (x->Is<SimpleVector>())
        {
            parseVector(x, iLevel);
        }
        else if (atom(x))
        {
            emit(Qquote, x);
        }
        else if (isQuote(x, Qbackquote))
        {
            emit(Qfuncall);
            emit(Qquote, Qbackquote);
            parse(cadr(x), iLevel + 1);
            emit(Qlist);
        }
        else if (isQuote(x, Qunquote))
        {
            Val const operand = cadr(x);
            if (1 == iLevel)
            {
                emit(Qeval, operand);
            }
            else if (isQuote(operand, Qquote))
            {
                // (unquote (quote X)) = X
                parse(cadr(operand), iLevel - 1);
            }
            else
            {
                emit(Qfuncall);
                emit(Qquote, Qunquote);
                parse(operand, iLevel - 1);
                emit(Qlist);
            }
        }
        else if (isSplicing(x))
        {
            if (1 == iLevel)
            {
                error(",~C~S after `", unparse(x), cadr(x));
            }
            else
            {
                emit(Qfuncall);
                emit(Qquote, car(x));
                parse(cadr(x), iLevel - 1);
                emit(Qlist);
            }
        }
        else
        {
            parseList(x, iLevel);
        }
    } // parse

    private: void parseList(Val x, int const iLevel)
    {
        int cElts = 0;
        Val splicing = nil;
        Val ostack = nil;

        for (;;)
        {
            if (nil == x)
            {
                break;
            }

            if (atom(x) || isQuote(x, Qunquote) || isQuote(x, Qbackquote))
            {
                if (car(ostack) == Qlist)
                {
                    setf_car(1 == cElts ? Qcons : QlistA, ostack);
                }

                parse(x, iLevel);
                break;
            }

            if (isSplicing(x))
            {
                error("Dotted ,~C~S", unparse(x), x);
            }

            Val elt = pop(x);

            if (1 == iLevel && isSplicing(elt))
            {
                if (0 != cElts)
                {
                    setf_car(1 == cElts ? Qcons : QlistA, ostack);
                    cElts = 0;
                }

                if (car(elt) != splicing)
                {
                    splicing = car(elt);
                    push(
                        splicing == Qunquote_splicing ? Qappend : Qnconc,
                        ostack );
                    emit(Qfuncall);
                }

                emit(Qeval, cadr(elt));
            }
            else
            {
                splicing = nil;
                if (0 == cElts)
                {
                    push(Qlist, ostack);
                    emit(Qfuncall);
                }
                parse(elt, iLevel);
                cElts += 1;
            } // if
        } // for each elt

        while (nil != ostack)
        {
            emit(pop(ostack));
        } // while
    } // parseList

    private: void parseVector(Val const vector, int const iLevel)
    {
        Val elts = nil;
        Val* pTail = &elts;
        foreach (SimpleVector::Enum, oEnum, vector)
        {
            Val cons = list(oEnum.Get());
            *pTail = cons;
            pTail = &cons->StaticCast<Cons>()->m_cdr;
        } // for
        emit(Qfuncall, vector);
        parseList(elts, iLevel);
        emit(Qvector);
    } // parseVector

    private: static Val unparse(Val const x)
    {
        return Character::FromCode(car(x) == Qunquote_splicing ? '@' : '.');
    } // unparse
}; // Backquote

/// <summary>
///  The destructuring-bind parser.
/// </summary>
class ParseDestructuringBind
{
    private: bool   m_fMacroLambdaList;

    private: Val    m_funbinds;
    private: Val    m_check_end;
    private: Val    m_check_not_end;
    private: Val    m_mismatch;
    private: Val    m_varbinds;
    private: Val    m_vars;             // for ignore declaration
    private: Val    m_var_env;

    private: ParseDestructuringBind(
            Val  mismatch,
            bool fMacroLambdaList ) :
        m_fMacroLambdaList(fMacroLambdaList),
        m_funbinds(nil),
        m_check_end(make_symbol_("check-end")),
        m_check_not_end(make_symbol_("check-not-end")),
        m_mismatch(mismatch),
        m_varbinds(nil),
        m_vars(nil),
        m_var_env(nil) {}

    public: struct ParseResult
    {
        Val m_form;
        Val m_var_env;
    }; // ParseResult

    public: static ParseResult Run(
        Val     pattern,
        Val     src_form,
        Val     body,
        Val     mismatch,
        bool    fEnv )
    {
        ParseDestructuringBind oParser(mismatch, fEnv);

        return oParser.run(pattern, src_form, body);
    } // Run

    // [A]
    private: Val addBinding(const char* pszVar, Val value)
        { return addBinding(gensym(make_string(pszVar)), value); }

    private: Val addBinding(Val var, Val value)
    {
        push(list(var, value), m_varbinds);
        push(var, m_vars);
        return var;
    } // addBiding

    // [E]
    private: Val ensureVar(Val name)
    {
        if (! symbolp(name))
        {
            error("Variable name must be a symbol: ~S", name);
        }

        if (nil != memq(name, VAR(lambda_list_keywords)))
        {
            error("Can't use ~S as variable name.", name);
        }

        // Note: We don't check name is constant or not. Compiler will
        // signal it.

        return name;
    } // ensureVar

    private: void NoReturn extraToken(Val key, Val token)
        { error("Extra token ~S after ~S.", token, key); }

    // [M]
    private: Val makeReqPop(Val var, Val pat)
    {
        return list(Qprogn,
            list(m_check_not_end, var, list(Qquote, pat)),
            list(Qpop, var) );
    } // makeReqPop

    private: void NoReturn malformedSpec(Val key, Val spec)
        { error("Malformed ~S: ~S", key, spec); }

    // [P]
    private: void processPat(Val pattern, Val var_whole, Val src_form)
    {
        enum State
        {
            State_AllowOtherKeys,
            State_Aux,
            State_Env,
            State_EnvAfter,
            State_Key,
            State_Opt,
            State_Req,
            State_Rest,
            State_RestAfter,
            State_Whole,
            State_WholeAfter,
        } eState = State_Whole;

        State eStateEnv       = State_Env;

        Val key  = nil;
        Val keys = nil;
        Val var_rest = nil;

        Val var_runner = addBinding("runner", src_form);
        Val pat = pattern;
        while (consp(pat))
        {
            Val cur_pat = pat;
            Val token   = pop(pat);

          tryAgain:
            switch (eState)
            {
            case State_AllowOtherKeys:
                if (QAaux == token)
                {
                    eState = State_Aux;
                }
                else if (m_fMacroLambdaList && QAenvironment == token)
                {
                    eStateEnv = State_EnvAfter;
                    eState = State_Env;
                }
                else
                {
                    extraToken(QAallow_other_keys, token);
                }
                break;

            case State_Aux:
                if (m_fMacroLambdaList && QAenvironment == token)
                {
                    eState = State_EnvAfter;
                }
                else if (symbolp(token))
                {
                    addBinding(token, nil);
                }
                else if (consp(token))
                {
                    if (nil == cdr(token))
                    {
                        addBinding(token, nil);
                    }
                    else if (consp(cdr(token)) && nil == cddr(token))
                    {
                        addBinding(car(token), cadr(token));
                    }
                    else
                    {
                        malformedSpec(QAaux, token);
                    }
                }
                else
                {
                    malformedSpec(QAaux, token);
                } // if
                break;

            case State_Env:
                if (nil != m_var_env)
                {
                    error("&environment appeared more than once.");
                }
                m_var_env = ensureVar(token);
                eState = eStateEnv;
                break;

            case State_EnvAfter:
                extraToken(QAenvironment, token);
                break;

            case State_Key:
                if (QAallow_other_keys == token)
                {
                    key = token;
                    eState = State_AllowOtherKeys;
                }
                else if (QAaux == token)
                {
                    eState = State_Aux;
                }
                else if (m_fMacroLambdaList && QAenvironment == token)
                {
                    eStateEnv = eState;
                    eState = State_Env;
                }
                else
                {
                    if (nil == var_rest)
                    {
                        var_rest = addBinding("rest", var_runner);
                    }

                    Val initform = nil;
                    Val key;
                    Val svar = MARKER_unbound;
                    Val var_or_pat;

                    if (symbolp(token))
                    {
                        key = intern(symbol_name(token), PKG_keyword);
                        var_or_pat = token;
                    }
                    else if (consp(token))
                    {
                        if (nil == cdr(token))
                        {
                            // (var)
                            var_or_pat = token;
                        }
                        else if (consp(cdr(token)) && nil == cddr(token))
                        {
                            // (var initform)
                            var_or_pat = car(token);
                            initform   = cadr(token);
                        }
                        else if (consp(cdr(token)) &&
                                 consp(cddr(token)) &&
                                 nil == cdddr(token) )
                        {
                            // (var initform)
                            var_or_pat = car(token);
                            initform   = cadr(token);
                            svar       = caddr(token);
                        }
                        else
                        {
                            malformedSpec(QAkey, token);
                        }

                        if (symbolp(var_or_pat))
                        {
                            key = intern(
                                symbol_name(var_or_pat), 
                                PKG_keyword );
                        }
                        else if (consp(var_or_pat) &&
                                 consp(cdr(var_or_pat)) &&
                                 nil == cddr(var_or_pat) )
                        {
                            key = car(var_or_pat);
                            var_or_pat = cadr(var_or_pat);
                        }
                        else
                        {
                            malformedSpec(QAkey, token);
                        }
                    }
                    else
                    {
                        malformedSpec(QAkey, token);
                    }

                    Val kvar = gensym(make_string("k"));

                    addBinding(
                        kvar,
                        list(Qkey, list(Qquote, key), var_rest) );

                    if (m_fMacroLambdaList)
                    {
                        processPatOrVar(
                            var_or_pat,
                            list(Qif, kvar, list(Qcadr, kvar), initform) );
                    }
                    else if (symbolp(var_or_pat))
                    {
                        addBinding(var_or_pat, list(Qcar, kvar));
                    }
                    else
                    {
                        malformedSpec(QAkey, token);
                    }

                    if (MARKER_unbound != svar)
                    {
                        addBinding(svar, kvar);
                    }
                } // if
                break;

            case State_Opt:
                if (QAaux == token)
                {
                    eState = State_Aux;
                }
                else if (m_fMacroLambdaList && QAbody == token)
                {
                    eState = State_Rest;
                }
                else if (m_fMacroLambdaList && QAenvironment == token)
                {
                    eStateEnv = eState;
                    eState = State_Env;
                }
                else if (QAkey == token)
                {
                    key = token;
                    eState = State_Key;
                }
                else if (QAoptional == token)
                {
                    eState = State_Opt;
                }
                else if (QArest == token)
                {
                    eState = State_Rest;
                }
                else if (symbolp(token))
                {
                    addBinding(
                        token, 
                        list(Qif, var_runner,
                            list(Qpop, var_runner) ) );
                }
                else if(consp(token))
                {
                    Val var_or_pat = car(token);
                    Val initform = nil;
                    Val svar = MARKER_unbound;

                    if (nil == cdr(token))
                    {
                        // (var)
                    }
                    else if (consp(cdr(token)) && nil == cddr(token))
                    {
                        // (var initform)
                        initform = cadr(token);
                    }
                    else if (consp(cdr(token)) &&
                             consp(cddr(token)) &&
                             nil == cdddr(token) )
                    {
                        // (var initform svar)
                        initform = cadr(token);
                        svar = caddr(token);
                    }
                    else
                    {
                        malformedSpec(QAoptional, token);
                    }


                    Val ovar = nil;
                    if (MARKER_unbound != svar)
                    {
                        ovar = addBinding("o", var_runner);
                    }

                    processPatOrVar(
                        var_or_pat,
                        list(Qif, var_runner,
                                list(Qpop, var_runner),
                                initform ) );

                    if (nil != ovar)
                    {
                        addBinding(svar, ovar);
                    }
                }
                else
                {
                    malformedSpec(QAoptional, token);
                }
                break;

            case State_Req:
                if (QAaux == token)
                {
                    eState = State_Aux;
                }
                else if (m_fMacroLambdaList && QAbody == token)
                {
                    eState = State_Rest;
                }
                else if (m_fMacroLambdaList && QAenvironment == token)
                {
                    eStateEnv = eState;
                    eState = State_Env;
                }
                else if (QAkey == token)
                {
                    key = token;
                    eState = State_Key;
                }
                else if (QAoptional == token)
                {
                    eState = State_Opt;
                }
                else if (QArest == token)
                {
                    eState = State_Rest;
                }
                else
                {
                    processPatOrVar(token, makeReqPop(var_runner, cur_pat));
                }
                break;

            case State_Rest:
                ASSERT(nil == var_rest);
                var_rest = processPatOrVar(token, var_runner);
                eState = State_RestAfter;
                break;

            case State_RestAfter:
                if (QAaux == token)
                {
                    eState = State_Aux;
                }
                else if (m_fMacroLambdaList && QAenvironment == token)
                {
                    eStateEnv = eState;
                    eState = State_Env;
                }
                else if (QAkey == token)
                {
                    key = token;
                    eState = State_Key;
                }
                else
                {
                    extraToken(QArest, token);
                }
                break;

            case State_Whole:
                if (QAwhole == token)
                {
                    eState = State_WholeAfter;
                }
                else
                {
                    eState = State_Req;
                    goto tryAgain;
                }
                break;

            case State_WholeAfter:
                addBinding(token, var_whole);
                eState = State_Req;
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch eState
        } // while

        switch (eState)
        {
        case State_Env:
            error("Expect variable name after &environment.");

        case State_Rest:
            error("Expect variable name after &rest.");
        } // switch eState

        if (nil != pat)
        {
            addBinding(ensureVar(pat), var_runner);
        }
        else if (nil == var_rest)
        {
            addBinding(
                "end",
                list(m_check_end, var_runner, list(Qquote, pattern)) );
        }

        if (QAkey == key)
        {
            addBinding(
                "keys",
                list(Qcheck_keywords, list(Qquote, keys), var_rest) );
        }
    } // processPat

    private: Val processPatOrVar(Val token, Val src_form)
    {
        if (symbolp(token))
        {
            return addBinding(token, src_form);
        }
        else if (consp(token))
        {
            Val var_whole = addBinding("whole", src_form);
            processPat(token, var_whole, var_whole);
            return var_whole;
        }
        else
        {
            error("Expect pattern or variable: ~S", token);
        }
    } // processPatOrVar

    // [R]
    private: ParseResult run(
        Val pattern,
        Val src_form,
        Val body )
    {
        Val var_whole = addBinding("whole", src_form);

        if (m_fMacroLambdaList)
        {
            src_form = list(Qcdr, var_whole);
        }
        else
        {
            src_form = var_whole;
        }

        processPat(pattern, var_whole, src_form);

        m_varbinds = nreverse(m_varbinds);

        Val varCur = make_symbol_("cur");
        Val varPat = make_symbol_("pat");

        push(
            list(m_check_end, list(varCur, varPat),
                list(Qif, varCur,
                    list(m_mismatch, varCur, varPat) )),
            m_funbinds );

        push(
            list(m_check_not_end, list(varCur, varPat),
                list(Qif, list(Qconsp, varCur),
                    varCur,
                    list(m_mismatch, varCur, varPat) )),
            m_funbinds );

        ParseResult oResult;

        oResult.m_var_env = m_var_env;

        oResult.m_form =
            list(Qlabels, m_funbinds,
                list(Qdeclare, list(Qignorable,
                    list(Qfunction, m_check_end),
                    list(Qfunction, m_check_not_end) )),
                listA(QletA, m_varbinds,
                    list(Qdeclare, listA(Qignorable, m_vars)),
                    body ) );

        return oResult;
    } // run
}; // ParseDestructuringBind


// Description:
//  Represents return value of analyzeBody function.
struct Body
{
    Val m_decls;
    Val m_forms;

    Body(Val decls, Val forms) :
        m_decls(decls), m_forms(forms) {}
}; // Body

static Body analyzeBody(Val form, Val runner)
{
    Val  decls = nil;
    Val* pTail = &decls;
    while (consp(runner))
    {
        Val decl = car(runner);
        if (! (consp(decl) && Qdeclare == car(decl)))
        {
            break;
        }
        Val x = list(decl);
        *pTail = x;
        pTail = &x->StaticCast<Cons>()->m_cdr;
        
        runner = cdr(runner);
    } // while

    if (! listp(runner))
    {
        error("Malformed form: ~S", form);
    }

    return Body(decls, runner);
} // analyzeBody

static void checkSyntax(Val form, int iMin, int iMax)
{
    Val n = safe_list_length(form);
    if (nil == n) error("Malformed form: ~S", form);
    if (ge(n, iMin) && le(n, iMax)) return;
    error("Syntax error: ~S", form);
} // checkSyntax

static Val expand_decfincf(Val form, Val env, Val op)
{
    Val vars = get_setf_expansion(cadr(form), env);
    Thread* pth = Thread::Get();
    Val vals   = pth->mv_value[1];
    Val stores = pth->mv_value[2];
    Val write_form = pth->mv_value[3];
    Val read_form  = pth->mv_value[4];
    Val delta = nil == cddr(form) ? one : caddr(form);
    return
        list(Qlet, mapcar(list, vars, vals),
          list(Qlet, list(list(car(stores), list(op, read_form, delta))),
            write_form ));
} // expand_decfincf

static Val makeIgnorableForm(Val val)
{
    Val var = make_symbol_("x");
    return list(Qlet, list(list(var, val)), var);
} // makeIgnorableForm

static Val mapcan(Val (*pfn)(Val), Val runner)
{
    Val result = nil;
    Val* pTail = &result;
    foreach (List::Enum, oEnum, runner)
    {
        Val frob = pfn(oEnum.Get());
        *pTail = frob;
        frob = last(frob);
        if (consp(frob)) pTail = &frob->StaticCast<Cons>()->m_cdr;
    } // for each elt
    return result;
} // mapcan

static Val mapcar(Val (*pfn)(Val), Val runner)
{
    Val result = nil;
    Val* pTail = &result;
    foreach (List::Enum, oEnum, runner)
    {
        Val frob = list(pfn(oEnum.Get()));
        *pTail = frob;
        pTail = &frob->StaticCast<Cons>()->m_cdr;
    } // for each elt
    return result;
} // mapcar


static Val mapcar(Val (*pfn)(Val, Val), Val runner1, Val runner2)
{
    Val result = nil;
    Val* pTail = &result;
    List::Enum oEnum1(runner1);
    List::Enum oEnum2(runner2);
    while (! oEnum1.AtEnd() && ! oEnum2.AtEnd())
    {
        Val frob = list(pfn(oEnum1.Get(), oEnum2.Get()));
        *pTail = frob;
        pTail = &frob->StaticCast<Cons>()->m_cdr;
        oEnum1.Next();
        oEnum2.Next();
    } // for each elt
    return result;
} // mapcar

} // Private

using namespace Private;

#define defmacro(mp_name) \
    defun(expand_ ## mp_name, (Val form, Val))

#define defmacro_(mp_name) \
    defun(expand_ ## mp_name, (Val form, Val env))


#define CHECK_SYNTAX(mp_min, mp_max, mp_syntax) \
    checkSyntax(form, mp_min, mp_max)


Val MakeMacroExpanderForm(Val name, Val syntax, Val body)
{
    Val const mismatch = make_symbol_("mismatch");
    Val const var_form = make_symbol_("form");

    ParseDestructuringBind::ParseResult oResult =
        ParseDestructuringBind::Run(
            syntax,
            var_form,
            body,
            mismatch,
            true );

    if (nil == oResult.m_var_env)
    {
        oResult.m_var_env = make_symbol_("env");
    }

    // (lambda (#:form #:env)
    //  (labels (
    //    (mismatch (src pat)
    //      (error 'syntax-error
    //        :datum      src
    //        :form       #:form
    //        :pattern    pat
    //        :syntax     ',(cons name syntax) )) )
    //    (block ,name ,expander) ) )
    return
        list(Qlambda, list(var_form, oResult.m_var_env),
            list(Qdeclare, list(Qfunction_name, list(Kmacro, name))),
            list(Qlabels, list(
                list(mismatch, list(Qdatum, Qpattern),
                    list(Qerror, list(Qquote, Qsyntax_error),
                        Kdatum,     Qdatum,
                        Kform,      var_form,
                        Kpattern,   Qpattern,
                        Ksyntax,    list(Qquote, cons(name, syntax)) )) ),
                list(Qblock, name, oResult.m_form) ) );
} // MakeMacroExpanderForm

// [A]
defmacro(and)
{
    Val forms = cdr(form);
    if (nil == forms) return t;
    if (nil == cdr(forms)) return car(forms);
    return list(Qif, car(forms), cons(Qand, cdr(forms)));
} // and

// [B]

defmacro(backquote)
{
    CHECK_SYNTAX(2, 2, "(backquote template)");
    return Backquote::Run(cadr(form));
} // backquote

// [C]

defmacro(case)
{
    CHECK_SYNTAX(2, MaxFormLength,
        "(case key clause* [(otherwise form*)])" );

    class ExpandCase
    {
        public: static Val Run(Val form)
        {
            ExpandCase oExpander;
            return oExpander.expand(form);
        } // Run

        private: Val m_var;

        private: ExpandCase() :
            m_var(make_symbol_("k")) {}

        private: Val expand(Val form)
        {
            return list(Qlet, list(list(m_var, cadr(form))),
                cons(Qcond, mapcar(&ExpandCase::expandClause, cddr(form))) );
        } // expand

        private: Val expandClause(Val clause)
        {
            Val keys  = car(clause);
            Val forms = cdr(clause);

            if (Qotherwise == keys)
            {
                return cons(t, forms);
            }

            if (atom(keys))
            {
                return cons(makeTest(keys), forms);
            }

            if (nil == cdr(keys))
            {
                return cons(makeTest(car(keys)), forms);
            }

            return cons(
                cons(Qor, mapcar(&ExpandCase::makeTest, keys)),
                forms );
        } // expand

        private: Val makeTest(Val key)
        {
            return list(Qeql, m_var, list(Qquote, key));
        } // makeTest

        private: Val mapcar(Val (ExpandCase::*pfn)(Val), Val runner)
        {
            Val result = nil;
            Val* pTail = &result;
            foreach (List::Enum, oEnum, runner)
            {
                Val frob = list((this->*pfn)(oEnum.Get()));
                *pTail = frob;
                pTail = &frob->StaticCast<Cons>()->m_cdr;
            } // for each elt
            return result;
        } // mapcar
    }; // Internal

    return ExpandCase::Run(form);
} // case

defmacro(cond)
{
    CHECK_SYNTAX(1, MaxFormLength, "(cond clause*)");

    Val clauses = cdr(form);
    if (nil == clauses)
    {
        return makeIgnorableForm(nil);
    }

    Val clause = pop(clauses);

    Val test  = car(clause);
    Val forms = cdr(clause);

    if (t == test && nil == clauses)
    {
        return cons(Qprogn, forms);
    }

    if (nil != forms)
    {
        return list(Qif, test, cons(Qprogn, forms), cons(Qcond, clauses));
    }

    Val var = make_symbol_("x");
    return
        list(Qlet, list(list(var, test)),
            list(Qif, var, var, cons(Qcond, clauses)) );
} // cond

// [D]
defmacro_(decf)
{
    CHECK_SYNTAX(2, 3, "(decf place &optional delta)");
    return expand_decfincf(form, env, Q_);
} // decf

defmacro(declaim)
{
    CHECK_SYNTAX(1, MaxFormLength, "(declaim declspec*)");

    class Inner
    {
        public: static Val MakeProclaim(Val x)
            { return list(Qproclaim, list(Qquote, x)); }
    }; // Inner

    return 
      listA(Qeval_when, list(Kexecute, Kcompile_toplevel, Kload_toplevel),
        mapcar(Inner::MakeProclaim, cdr(form)) );
} // declaim

defmacro(destructuring_bind)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(destructuring-bind lambda-list expr decl* form*)" );

    Val const syntax = cadr(form);
    Val const expr   = caddr(form);
    Val const body   = cdddr(form);

    Val const mismatch = make_symbol_("mismatch");
    Val const var_expr = make_symbol_("expr");

    ParseDestructuringBind::ParseResult oResult =
        ParseDestructuringBind::Run(
            syntax,
            var_expr,
            body,
            mismatch,
            false );

    return
        list(Qlet, list(list(var_expr, expr)),
          list(Qlabels, list(
            list(mismatch, list(Qdatum, Qpattern),
                list(Qerror, list(Qquote, Qsyntax_error),
                    Kdatum,     Qdatum,
                    Kform,      var_expr,
                    Kpattern,   Qpattern,
                    Ksyntax,    list(Qquote, syntax) ))),
            oResult.m_form ));
} // destructuring_bind

defmacro(defmacro)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(defmacro name (lambda-list) decl* form*)" );

    Val const name   = cadr(form);
    Val const syntax = caddr(form);
    Val const body   = cdddr(form);

    Val const expander_form = MakeMacroExpanderForm(name, syntax, body);

    return list(QPdefmacro,
        list(Qquote, name),
        list(Qquote, syntax),
        expander_form );
} // defmacro

defmacro(defun)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(defun name lambda-list decl* form*)" );

    Val runner = cdr(form);
    Val fname       = pop(runner);
    Val lambda_list = pop(runner);
    Val body        = runner;

    return list(QPdefun,
        list(Qquote, fname),
        list(Qquote, lambda_list),
        list(Qlabels, list(
            listA(fname, lambda_list, body) ),
            list(Qfunction, fname) ));
} // defun

defmacro(dolist)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(dotlist (var limit [result]) declaration* {tag|statement}*)" );

    Val runner = cdr(form);
    Val spec_runner = pop(runner);
    Val var = pop(spec_runner);
    Val init = pop(spec_runner);

    Body oBody = analyzeBody(form, runner);

    Val ivar  = make_symbol(make_string("i"));
    Val label = make_symbol(make_string("dolist"));

    return list(Qblock, nil,
        list(Qlet,
                list(list(ivar, init)),
                list(Qdeclare, list(Qtype, ivar)),
            list(Qtagbody,
                label,
                // exit test
                list(Qif, list(Qendp, ivar),
                    listA(Qlet,
                          list(list(var, nil)),
                          list(Qdeclare, list(Qignorable, var)),
                        append(
                            oBody.m_decls,
                            list(listA(Qreturn, spec_runner)) ))),
                // loop body
                listA(Qlet,
                    list(list(var, list(Qcar, ivar))),
                    list(Qdeclare, list(Qignorable, var)),
                    append(
                        oBody.m_decls,
                        list(listA(Qtagbody, oBody.m_forms)) ) ),
                // step
                list(Qsetq, ivar, list(Qcdr, ivar)),
                // loop
                list(Qgo, label) )) );
} // dolist

defmacro(dotimes)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(dotimes (var limit [result]) declaration* {tag|statement}*)" );

    Val runner = cdr(form);
    Val spec_runner = pop(runner);
    Val var  = pop(spec_runner);
    Val limit = pop(spec_runner);

    Body oBody = analyzeBody(form, runner);

    Val ivar  = make_symbol(make_string("i"));
    Val nvar  = make_symbol(make_string("n"));
    Val label = make_symbol(make_string("dotimes"));

    return list(Qblock, nil,
        list(Qlet, list(
                list(ivar, zero),
                list(nvar, limit) ),
            list(Qdeclare, list(Qtype, list(Qinteger, zero, QA), ivar)),
            list(Qdeclare, list(Qtype, Qinteger, nvar)),
            list(Qtagbody,
                label,
                // exit test
                list(Qif, list(QGQ, ivar, nvar),
                    // result form
                    listA(Qlet, list(list(var, ivar)),
                            list(Qdeclare, list(Qignorable, var)),
                        append(oBody.m_decls, list(listA(Qreturn, spec_runner))) )),
                // loop body
                listA(Qlet, list(list(var, ivar)),
                        list(Qdeclare, list(Qignorable, var)),
                    append(oBody.m_decls, list(listA(Qtagbody, oBody.m_forms))) ),
                // step
                list(Qsetq, ivar, list(Q1P, ivar)),
                // loop
                list(Qgo, label) )) );
} // dotimes

// [H]
defmacro(handler_case)
{
    static Val sm_ignore;

    class HandlerCase
    {
        // clause ::= (typespec ([var]) decl* form*)
        // expand ::= 'typespec (lambda ([var]) decl* form*)
        public: static Val expandClause(Val clause)
        {
            unless (sm_ignore)
            {
                sm_ignore = list(Qdeclare,
                    list(Qignore, make_symbol(make_string("x"))) );
            }

            Val ll    = cadr(clause);
            Val forms = cddr(clause);
            if (nil == cadr(clause))
            {
                ll = list(cadr(cadr(sm_ignore)));
                forms = cons(sm_ignore, forms);
            }

            return list(
                list(Qquote, car(clause)),
                listA(Qlambda, ll, forms) );
        } // expandClause
    }; // HandlerCase

    CHECK_SYNTAX(2, MaxFormLength, "(handler-case form clause*)");
    return listA(QPhandler_case,
        list(Qlambda, nil, cadr(form)),
        mapcan(HandlerCase::expandClause, cddr(form)) );
} // handler_case

// [I]
defmacro_(incf)
{
    CHECK_SYNTAX(2, 3, "(incf place &optional delta)");
    return expand_decfincf(form, env, QP);
} // incf

defmacro(in_package)
{
    CHECK_SYNTAX(2, 2, "(in-pakcage name)");
    return list(Qsetq, QApackageA,
        list(Qfind_package, list(Qquote, cadr(form))) );
} // in_package

// [L]
defmacro(lambda)
{
    CHECK_SYNTAX(2, MaxFormLength, "(lambda lambda-list decl* form*)");
    return list(Qfunction, form);
} // lambda

defmacro(loop)
{
    Val loop = make_symbol(make_string("loop"));
    return
        list(Qblock, nil,
            list(Qtagbody, loop, cons(Qprogn, cdr(form)), list(Qgo, loop)) );
} // loop

// [M]
defmacro(multiple_value_bind)
{
    CHECK_SYNTAX(2, MaxFormLength,
        "(multiple-value-bind (var*) form decl* form*)" );

    Val rest = make_symbol_("r");

    Val vars  = cadr(form);
    Val vform = caddr(form);
    Val body  = cdddr(form);

    return list(Qmultiple_value_call,
        listA(Qlambda, cons(QAoptional, append(vars, list(QArest, rest))),
            list(Qdeclare, list(Qignore, rest)),
            list(Qdeclare, list(Qdynamic_extent, rest)),
            body ),
        vform );
} // multiple_value_bind

defmacro(multiple_value_list)
{
    CHECK_SYNTAX(2, 2, "(multiple-value-list form)");
    return list(Qmultiple_value_call, list(Qfunction, Qlist), cadr(form));
} // multiple_value_list

defmacro(multiple_value_setq)
{
    CHECK_SYNTAX(3, 3, "(multiple-value-setq (var*) form");

    Val vars  = cadr(form);
    Val vform = caddr(form);
    return list(Qvalues, list(Qsetf, cons(Qvalues, vars), vform));
} // multiple_value_list

// [N]
defmacro(nth_value)
{
    CHECK_SYNTAX(3, 3, "(nth-value n form)");
    Val nth   = cadr(form);
    Val vform = caddr(form);
    return list(Qnth, nth, list(Qmultiple_value_list, vform));
} // nth_value

// [O]
defmacro(or)
{
    Val forms = cdr(form);

    if (nil == forms) return nil;
    if (nil == cdr(forms)) return car(forms);

    Val var = make_symbol_("or");
    return
        list(Qlet, list(list(var, car(forms))),
            list(Qif, var, var, cons(Qor, cdr(forms))) );
} // or

// [P]
defmacro(pop)
{
    CHECK_SYNTAX(2, 2, "(pop place)");
    Val place = cadr(form);
    return list(Qprog1,
        list(Qcar, place),
        list(Qsetq, place, list(Qcdr, place)) );
} // pop

defmacro(prog)
{
    CHECK_SYNTAX(2, MaxFormLength, "(prog (binding*) decl* form*)");
    Val bindings = cadr(form);
    Val body     = cddr(form);
    Body oBody   = analyzeBody(form, body);
    return
        list(Qblock, nil,
            listA(Qlet, bindings,
                append(
                    oBody.m_decls,
                    list(cons(Qtagbody, oBody.m_forms)) )));
} // prog

defmacro(prog1)
{
    CHECK_SYNTAX(2, MaxFormLength, "(prog1 form1 form*)");
    Val form1 = cadr(form);
    Val forms = cddr(form);

    Val var = make_symbol_("x");
    return
        list(Qlet, list(list(var, form1)),
            cons(Qprogn, forms),
            var );
} // prog1

defmacro(progA)
{
    CHECK_SYNTAX(2, MaxFormLength, "(prog* (binding*) decl* form*)");
    Val bindings = cadr(form);
    Val body     = cddr(form);
    Body oBody = analyzeBody(form, body);
    return
        list(Qblock, nil,
            listA(QletA, bindings,
                append(
                    oBody.m_decls,
                    list(cons(Qtagbody, oBody.m_forms)) )));
} // progA

defmacro(prog2)
{
    CHECK_SYNTAX(3, MaxFormLength, "(prog2 form1 form2 form*)");
    Val form1 = cadr(form);
    Val form2 = caddr(form);
    Val forms = cdddr(form);

    Val var = make_symbol_("x");
    return
        list(Qlet, list(list(var, list(Qprogn, form1, form2))),
            cons(Qprogn, forms),
            var );
} // prog2

defmacro(push)
{
    CHECK_SYNTAX(3, 3, "(push item place)");
    Val runner = cdr(form);
    Val item = pop(runner);
    Val place = pop(runner);
    // TODO yosi@msn.com 2008-06-21 We must use get-setf-expansion for push.
    return list(Qsetq, place, list(Qcons, item, place));
} // push

// [R]
defmacro(return)
{
    CHECK_SYNTAX(1, 2, "(return [form])");
    if (nil == cdr(form)) return list(Qreturn_from);
    return list(Qreturn_from, cadr(form));
} // return

// [S]
defmacro_(setf)
{
    CHECK_SYNTAX(0, MaxFormLength, "(setf {place form}*)");

    Thread* pth = Thread::Get();

    Collector oForms;

    foreach (List::Enum, oEnum, cdr(form))
    {
        Val place = oEnum.Get();

        oEnum.Next();
        if (oEnum.AtEnd())
        {
            error("Missing setf value form for ~S.", place);
        }

        Val value = oEnum.Get();

        Val vars   = get_setf_expansion(place, env);
        Val vals   = pth->mv_value[1];
        Val stores = pth->mv_value[2];
        Val writer = pth->mv_value[3];

        if (nil == stores)
        {
            oForms.Add(list(Qprogn, cons(Qprogn, vals), writer));
        }
        else if (nil == vars &&
                 nil == vals &&
                 nil == cdr(stores) &&
                 car(writer) == Qsetq &&
                 cadr(writer) == place &&
                 car(stores) == caddr(writer) &&
                 nil == cdddr(writer) )
        {
            // setq
            oForms.Add(list(Qsetq, place, value));
        }
        else if (nil == cdr(stores))
        {
            // single store
            oForms.Add(
                list(Qlet, mapcar(list, vars, vals),
                    list(Qlet, list(list(car(stores), value)), writer) ) );
        }
        else
        {
            // multiple stores
            oForms.Add(
                list(Qlet, mapcar(list, vars, vals),
                    list(Qmultiple_value_bind, stores, value, writer) ) );
        }
    } // for

    Val forms = oForms.Get();
    if (nil == cdr(forms))
    {
        return car(forms);
    }

    return cons(Qprogn, forms);
} // setf

// [T]
defmacro(typecase)
{
    CHECK_SYNTAX(2, MaxFormLength, "(typecase form clause*)");
    Val const var = make_symbol_("x");
    Val const valform = cadr(form);
    Val const clauses = cddr(form);

    Collector oClauses;
    foreach (List::Enum, oEnum, clauses)
    {
        Val const clause = oEnum.Get();
        if (car(clause) == Qotherwise)
        {
            oClauses.Add(cons(t, cdr(clause)));
        }
        else
        {
            oClauses.Add(
                listA(
                    list(Qtypep, var, list(Qquote, car(clause))),
                    cdr(clause) ) );
        }
    } // for

    return
        list(Qlet, list(list(var, valform)),
          listA(Qcond, oClauses.Get()) );
} // typecase

// [U]
defmacro(unless)
{
    CHECK_SYNTAX(2, MaxFormLength, "(unless test form*)");
    Val test  = cadr(form);
    Val forms = cddr(form);
    return list(Qif, list(Qnot, test), cons(Qprogn, forms));
} // unless

// [W]
defmacro(when)
{
    CHECK_SYNTAX(2, MaxFormLength, "(when test form*)");
    Val test  = cadr(form);
    Val forms = cddr(form);
    return list(Qif, test, cons(Qprogn, forms));
} // when

} // TinyCl
