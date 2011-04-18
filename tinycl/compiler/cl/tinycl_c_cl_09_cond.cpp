#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 09 Conditions
// tinycl_c_cl_09_cond.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_09_cond.cpp#2 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

defparser(handler_bind)
{
    CHECK_SYNTAX(2, MaxFormLength, "(handler-bind binding* clause*)");

    FrameReg* const pFd = new FrameReg(
        NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
        Qhandler_case );

    Instruction* const pOpenI = emitI(new OpenHandlerI(pFd));

    foreach (List::Enum, oEnum, cadr(form))
    {
        Val const binding = oEnum.Get();
        if (two != safe_list_length(binding))
        {
            parseError("Invalid binding: ~S", binding);
            continue;
        }

        Val const typespec = car(binding);
        if (symbolp(typespec) || classp(typespec))
        {
            pOpenI->AppendOperand(Literal::New(typespec));
        }
        else
        {
            parseError("Expect type specifier instead of ~S", typespec);
            continue;
        }

        Expect oExpect(Type::Parse(Qfunction_designator));
        Operand* const pSx = parseForm1(&oExpect, cadr(binding));
        if (Unreachable == pSx)
        {
            continue;
        }
        pOpenI->AppendOperand(pSx);
    } // for binding

    HandlerFrame* const pHandlerFrame = new(this) HandlerFrame(pFd);

    pushFrame(pHandlerFrame);

    Operand* pSx = parseForms(pExpect, form, cddr(form));

    popFrame();

    if (Unreachable != pSx)
    {
        Operand* const pRsave = emitWeakSaveValues(pSx);
        emitUnwind(pHandlerFrame);
        pSx = emitWeakRestoreValues(pRsave);
    } // if not unreachable

    return emitLinkage(pSx);
} // handler_bind

defparser(handler_case)
{
    CHECK_SYNTAX(2, MaxFormLength, "(handler-case form clause*)");

    FrameReg* const pFd = new FrameReg(
        NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
        Qhandler_case );

    BBlock* const pSuccBB = startPhi(pExpect->m_pty);
    BBlock* const pJoinBB = m_pSucc;

    TryFrame* const pTryFrame = new(this) TryFrame(pFd, pJoinBB);
    pushFrame(pTryFrame);

    Instruction* const pOpenI = emitI(new OpenTryI(pFd));

    {
        BBlock* const pBB = newBBlock();
        emitI(new JumpI(pBB));
        setCurrSucc(pBB, pBB);
    }

    Val const no_error = assq(Kno_error, cddr(form));

    // Parse try form
    {
        Operand* pSx;
        if (nil == no_error)
        {
            pSx = parseForm1(pExpect, cadr(form));
        }
        else
        {
            Expect oExpect(Qhandler_case, tyValuesRestT);
            pSx = parseForm1(&oExpect, cadr(form));
        }

        popFrame();

        if (nil != no_error)
        {
            if (Unreachable == pSx)
            {
                uselessForm(no_error);
            }
            else
            {
                ClFrame* const pFrame = m_pFrame;

                LambdaList oLambdaList;
                LexEnv oLexEnv(pExpect->m_pty);
                LexEnvScope oScope(&oLexEnv);
                Val forms = parseLambdaList(no_error, &oLambdaList);
                processLambdaList(&oLambdaList, pSx->StaticCast<Values>());
                pSx = parseForms(pExpect, no_error, forms);
                closeLexEnv();

                emitUnwinds(pFrame);
            }
        } // if

        if (Unreachable != pSx)
        {
            Operand* const pRsave = emitWeakSaveValues(pSx);
            emitUnwind(pTryFrame);
            pSx = emitWeakRestoreValues(pRsave);

            emitI(new JumpI(pJoinBB));

            if (Void != pSx)
            {
                setPhiOperand(
                    pJoinBB->GetFirstI()->StaticCast<PhiI>(),
                    m_pCurr,
                    pSx );
            }
        } // if not unreachable
    }

    // Insert CATCH instruction
    {
        Instruction* const pRefI = pOpenI->GetNext();
        BBlock* const pBB = pRefI->GetBB();
        foreach (List::Enum, oEnum, cddr(form))
        {
            Val clause = oEnum.Get();
            if (Kno_error == car(clause)) continue;
            pBB->InsertBeforeI(
                new CatchI(pFd, newBBlock(), car(clause)),
                pRefI );
        } // for each clause
    }

    // Parse clause body
    {
        Instruction* pCatchI = pOpenI->GetNext();
        foreach (List::Enum, oEnum, cddr(form))
        {
            Val const clause = oEnum.Get();
            if (Kno_error == car(clause)) continue;

            OpenBindI* pOpenBindI = NULL;

            LexEnv oLexEnv(pExpect->m_pty);
            LexEnvScope oScope(&oLexEnv);

            m_pCurr = pCatchI->GetSy()->StaticCast<Label>()->GetBB();
            m_pSucc = m_pCurr;

            Val const ll = cadr(clause);
            if (nil == ll)
            {
                // no variable
            }
            else if (safe_list_length(ll) == one)
            {
                Values* const pVd = new Values;
                emitI(new NonlocalI(tyValuesRestT, pVd));

                Register* const pRd = new Register;
                emitI(new SelectI(pExpect->m_pty, pRd, pVd, 0));

                VarLet* const pVarLet = newVarLet(car(ll));
                pOpenBindI = emitBind(pVarLet, pRd);
                if (NULL != pOpenBindI) emitI(pOpenBindI);
            }
            else
            {
                parseError("Syntax error");
            }

            Val const forms = parseDecls(cddr(clause));

            Operand* pSx = parseForms(pExpect, clause, forms);

            if (NULL != pOpenBindI)
            {
                ClFrame* const pFrame = m_pFrame;
                popFrame();

                if (Unreachable != pSx)
                {
                    Operand* const pRsave = emitWeakSaveValues(pSx);
                    emitUnwind(pFrame);
                    pSx = emitWeakRestoreValues(pRsave);
                }
            } // if

            closeLexEnv();

            m_pSucc = pJoinBB;
            emitLinkage(pSx);

            pCatchI = pCatchI->GetNext();
        } // for each clause
    }

    setCurrSucc(pJoinBB, pSuccBB);
    return endPhi(pExpect->m_pty);
} // handler_case

} // Compiler

} // TinyCl
