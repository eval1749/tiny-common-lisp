#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 05 Data and Control-Flow
// tinycl_c_cl_03_data.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_05_data.cpp#8 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

Operand* PassParse::emitCall2(
    Val      const callee,
    Operand* const pSx,
    Operand* const pSy )
{
    Values* const pVy = new Values;
    emitI(new ValuesI(pVy, pSx, pSy));

    Register* const pRd = new Register;
    emitI(new CallI(tyT, pRd, Literal::New(callee), pVy));
    return pRd;
} // PassParse::emitCall2

Operand* PassParse::emitEq(
    Operand* pSx,
    Operand* pSy,
    Operand* pTrue )
{
    if (Literal* const pLx = pSx->DynamicCast<Literal>())
    {
        if (Literal* const pLy = pSy->DynamicCast<Literal>())
        {
            if (pLx->GetDatum() != pLy->GetDatum())
            {
                // (eq lit1 lit2)
                return Literal::New(nil);
            }
            else if (pLx->GetDatum() != nil)
            {
                // (eq non-nil non-nil)
                return pSx;
            }
            else
            {
                // (eq nil nil)
                return True;
            }
        } // if

        swap(pSx, pSy);
    }

    Bool* const pBd = new Bool;
    emitI(new EqI(pBd, pSx, pSy));

    if (NULL == pTrue)
    {
        pTrue = True;
    }

    Register* const pRd = new Register;
    // Use pSx for true
    //emitI(new IfI(pRd, pBd, Literal::True(pSx), Literal::New(nil)));
    emitI(new IfI(pRd, pBd, pTrue, Literal::New(nil)));

    return pRd;
} // PassParse::emitEq

Operand* PassParse::parseApply(
    const Expect* const pExpect,
    Val           const form )
{
    ExpectArg oExpect(car(form), 0, Type::Parse(Qfunction_designator));
    Operand* const pSx = parseForm1(&oExpect, cadr(form));
    if (Unreachable == pSx)
    {
        return uselessForm(form);
    }

    Callee oCallee;
    foldCallee(pSx, &oCallee);

    //= <FIXME date="2008-12-26" by="yosi@msn.com">
    //=   We should adjust value type of setf-function to primary value
    //=   only.
    //= </FIXME>

    return parseCall(pExpect, form, &oCallee, cddr(form));
} // PassParse::parseApply

Operand* PassParse::parseOperand(
    Val     name,
    Type*   pty,
    int     iNth,
    Val     form )
{
    ExpectArg oExpect(name, iNth, pty);
    return parseForm1(&oExpect, form);
} // PassParse::parseOperand

defparser(apply)
{
    CHECK_SYNTAX(2, MaxFormLength, "(apply fn form*)")
    return parseApply(pExpect, form);
} // apply

defparser(eq)
{
    class Local
    {
        public: static bool IsSafeForEq(Operand* pSx)
        {
            if (Literal* pLx = pSx->DynamicCast<Literal>())
            {
                Val datum = pLx->GetDatum();
                return ! (fixnump(datum) || characterp(datum));
            } // if

            return true;
        } // IsSafeForEq
    }; // Local

    CHECK_SYNTAX(3, 3, "(eq x y)");

    Operand* pSx;
    Operand* pSy;
    {
        ExpectArg oExpect1(Qeq, 0, tyT);
        ExpectArg oExpect2(Qeq, 1, tyT);

        if (! parseForm2(&oExpect1, &oExpect2, &pSx, &pSy, cdr(form)))
        {
            return unreachableForm(form);
        }
    }

    if (! Local::IsSafeForEq(pSx))
    {
        styleWarn("EQ ~S isn't portable.", cadr(form));
    }

    if (! Local::IsSafeForEq(pSy))
    {
        styleWarn("EQ ~S isn't portable.", caddr(form));
    }

    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }

    return emitLinkage(emitEq(pSx, pSy));
} // eq

defparser(funcall)
{
    CHECK_SYNTAX(2, MaxFormLength, "(funcall fn form*)")
    return parseApply(pExpect, form);
} // funcall

defparser(not)
{
    CHECK_SYNTAX(2, 2, "(not x)");

    Operand* const pSx = parseOperand(Qnot, tyT, 0, cadr(form));
    if (Unreachable == pSx)
    {
        return unreachableForm(form);
    }

    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }
    return emitLinkage(emitEq(pSx, Literal::New(nil), Literal::New(t)));
} // not

defparser(null)
{
    CHECK_SYNTAX(2, 2, "(null x)");
    return parse_not(pExpect, form);
} // null

defparser(quote)
{
    CHECK_SYNTAX(2, 2, "(quote datum)");
    return parseLiteral(pExpect, cadr(form));
} // quote

} // Compiler

} // TinyCl
