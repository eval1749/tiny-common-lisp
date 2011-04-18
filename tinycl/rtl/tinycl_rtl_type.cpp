#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Miscellaneous
// tinycl_rtl_misc.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_type.cpp#3 $
//
#define DEBUG_HASH_TABLE 1
#include "../tinycl.h"

#include "../arch/generic/tinycl_gen.h"

namespace TinyCl
{

/// <summary>
///   Retreive type macro expansion
///   <para>
///     Note: We don't allow to put nil to m_types. If we detect it, we put
///     (lambda () nil) instead of nil.
///   </para>
/// </summary>
Val find_type(Val typespec, Val errorp, Val env)
{
    Val name = consp(typespec) ? car(typespec) : typespec;

    for (;;)
    {
        Environment* pEnv = env->DynamicCast<Environment>();
        if (NULL == pEnv)
        {
            if (nil != errorp)
            {
                error("Undefined type specifier: ~S", typespec);
            }
            return nil;
        }

        if (hash_table_p(pEnv->m_types))
        {
            Val found;
            Val thing = gethash(name, pEnv->m_types, nil, &found);
            if (nil != found)
            {
                return thing;
            }
        }

        env = pEnv->m_outer;
    } // for
} // find_type

static bool typepAux(Val obj, Val typespec, Val env)
{
    if (QA == typespec)
    {
        return true;
    }

    return typep(obj, typespec, env);
} // typepAux

static void checkTypeSyntax(Val form, int iMin, int iMax, char* szSyntax)
{
    Val n = safe_list_length(form);
    if (nil == n) error("Malformed type specifier: ~S", form);
    if (ge(n, iMin) && le(n, iMax)) return;
    error("Expect ~S: ~S", make_string(szSyntax), form);
} // checkTypeSyntax

static Val getTypeElement(Val form, int iNth, Val def)
{
    while (iNth > 0)
    {
        form = cdr(form);
        iNth -= 1;
    }
    return nil == form ? def : car(form);
} // getTypeElement

namespace CommonLisp
{

defun(type_of, (Val x))
{
    return classd_of(x)->StaticCast<ClassD>()->m_type;
} // type_of

defpred(typep, (Val obj, Val typespec, Val env))
{
    if (nil == env)
    {
        env = TLV(AenvironmentA);
    }

    check_type(env, environment);

    if (nil == typespec)
    {
        return false;
    } // if nil

    if (classp(typespec))
    {
        return subclassp(class_of(obj), typespec);
    } // if class

    if (symbolp(typespec))
    {
        Val klass = find_class(typespec, nil, env);
        if (nil != klass)
        {
            return subclassp(class_of(obj), klass);
        }

        Val expansion = find_type(typespec, t, env);
        if (functionp(expansion))
        {
            expansion = funcall(expansion, list(typespec));
        }
        return typep(obj, expansion, env);
    } // if symbol

    if (consp(typespec))
    {
        Val op = car(typespec);

        if (Qand == op)
        {
            foreach (List::Enum, oEnum, cdr(typespec))
            {
                if (! typep(obj, oEnum.Get(), env))
                {
                    return false;
                }
            }
            return true;
        } // if and

        if (Qcons == op)
        {
            if (! consp(obj))
            {
                return false;
            }

            Val runner = cdr(typespec);
            if (nil == runner)
            {
                return true;
            }

            if (! typepAux(car(obj), pop(runner), env))
            {
                return false;
            }

            if (nil == runner)
            {
                return true;
            }

            if (! typepAux(cdr(obj), pop(runner), env))
            {
                return false;
            }

            return true;
        } // if cons

        if (Qeql == op)
        {
            return eql(cadr(typespec), obj);
        } // if eql

        if (Qinteger == op)
        {
            if (! integerp(obj))
            {
                return false;
            }

            checkTypeSyntax(typespec, 1, 3, "(integer [min [max]])");
            Val min = getTypeElement(typespec, 1, QA);
            Val max = getTypeElement(typespec, 2, QA);

            if (consp(min))
            {
                min = add(car(min), one);
            }

            if (consp(max))
            {
                max = sub(car(max), one);
            }

            if (QA != min)
            {
                if (lt(obj, min))
                {
                    return false;
                }
            }

            if (QA != max)
            {
                if (gt(obj, max))
                {
                    return false;
                }
            }

            return true;
        } // if integer

        if (Qmember == op)
        {
            return nil != memv(obj, cdr(typespec));
        } // if member

        if (Qnot == op)
        {
            return ! typep(obj, cadr(typespec), env);
        } // if not

        if (Qor == op)
        {
            foreach (List::Enum, oEnum, cdr(typespec))
            {
                if (typep(obj, oEnum.Get(), env))
                {
                    return true;
                }
            }
            return false;
        } // if or

        if (Qsatisfies == op)
        {
            return nil != funcall(cadr(typespec), obj);
        } // if satisfies

        Val expansion = find_type(typespec, t, env);
        if (functionp(expansion))
        {
            expansion = funcall(expansion, typespec);
        }
        return typep(obj, expansion, env);
    } // if cons

    error("Invalid type-specifier: ~S", typespec);
} // typep

bool typep_(Val n, Val obj, Val typespec, Val env)
{
    if (two == n) env = nil;
    return typep(obj, typespec, env);
} // typep

} // CommonLisp
} // TInyCl
