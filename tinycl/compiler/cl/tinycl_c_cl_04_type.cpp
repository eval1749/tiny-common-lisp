#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 04 Types and Classes
// tinycl_c_cl_04_type.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_04_type.cpp#2 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

Operand* PassParse::parseTypePredicate(
    const Expect*   pExpect,
    Val             form,
    Val             klass )
{
    if (two != safe_list_length(form))
    {
        parseError("Expect only one argument: ~S", form);
        return parseLiteral(pExpect, nil);
    }

    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }

    Operand* pSx;
    {
        ExpectArg oExpect(car(form), 0, tyT);
        pSx = parseForm1(&oExpect, cadr(form));
        if (Unreachable == pSx)
        {
            return unreachableForm(form);
        }
    }

    if (Register* pRx = pSx->DynamicCast<Register>())
    {
        Bool* pBd = new Bool;
        emitI(new TypepI(pBd, pRx, Type::Parse(klass)));
        Register* pRd = new Register;
        Operand* pTrue = pRx;
        if (typep(nil, klass)) pTrue = True;
        emitI(new IfI(pRd, pBd, pTrue, Literal::New(nil)));
        return emitLinkage(pRd);
    }
    else if (Literal* pLx = pSx->DynamicCast<Literal>())
    {
        Val val = typep(pLx->GetDatum(), klass) ? t : nil;
        return parseLiteral(pExpect, val);
    }
    else
    {
        parseError("Unexpected literal: ~S", cadr(form));
        return parseLiteral(pExpect, nil);
    }
} // ParserPass::parseTypePredicate

/// <summary>
///  Parse function typep.
/// </summary>
defparser(typep)
{
    CHECK_SYNTAX(3, 3, "(typep x typespec)");

    Operand* pSx;
    Operand* pSy;
    {
        ExpectArg oExpect1(Qtypep, 0, tyT);
        ExpectArg oExpect2(Qtypep, 1, Type::Parse(Qtype_specifier));

        if (! parseForm2(&oExpect1, &oExpect2, &pSx, &pSy, cdr(form)))
        {
            return unreachableForm(form);
        }
    }

    if (tyVoid == pExpect->m_pty)
    {
        return ignoreForm(form);
    }

    if (Literal* pLy = pSy->DynamicCast<Literal>())
    {
        if (Register* pRx = pSx->DynamicCast<Register>())
        {
            Bool* pBd = new Bool;
            Val typespec = pLy->GetDatum();
            emitI(new TypepI(pBd, pRx, Type::Parse(typespec)));
            Register* pRd = new Register;
            Operand* pTrue;

            // FIXME 2008-08-11 yosi@msn.com Undefined for malformed
            // type specifier causes error on TYPEP. However, we should
            // use TYPEP in safe way.
            #if 0
            {
                pTrue = pRx;
                if (typep(nil, typespec)) pTrue = True;
            }
            #else
            {
                pTrue = Literal::True(pSx);
            }
            #endif
            emitI(new IfI(pRd, pBd, pTrue, Literal::New(nil)));
            return emitLinkage(pRd);
        }

        if (Literal* pLx = pSx->DynamicCast<Literal>())
        {
            Val val = funcall(Qtypep, pLx->GetDatum(), pLy->GetDatum());
            return emitLinkage(Literal::New(val));
        }
        
        parseError("Unsupported operand: ~S", cadr(form));
        return unreachableForm(form);
    }

    return emitLinkage(emitCall2(Qtypep, pSx, pSy));
} // typep

} // Compiler
} // TinyCl
