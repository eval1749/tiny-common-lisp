#include "precomp.h"

//
// TinyCl - Compiler - Parse - 03 Evaluation and Compilation
// tinycl_c_cl_03_declare.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_03_declare.cpp#7 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

struct DeclareParserEntry
{
    Val m_id;
    void (*m_pfn)(PassParse*, Val);
}; // DeclareParserEntry

static DeclareParserEntry const
k_rgoDeclareParser[] =
{
    #undef defparser
    #undef defspecial

    #define defdeclare(mp_name) \
        { Q ## mp_name, PassParse::parse_declare__ ## mp_name },

    #include "../tinycl_compiler.inc"
}; // k_rgoDeclareParser

PassParse::FunRef*
PassParse::internFunDcl(Val name)
{
    if (! function_name_p(name))
    {
        parseError("Expect function name: ~S", name);
        return NULL;
    }

    const TyFunction* pFunty = tyUnknownFunction;
    uint  rgfFlag = 0;
    Val   datum = nil;
    if (FunRef* pFunRef = findFunRef(name))
    {
        rgfFlag = pFunRef->GetFlags() & VarRef::Flag_Inherit;
        pFunty = pFunRef->GetFunty();
        if (pFunRef->GetLexEnv() == m_pLexEnv)
        {
            return pFunRef;
        }

        if (FunPro* pFunPro = pFunRef->DynamicCast<FunPro>())
        {
            datum = pFunPro->GetAlist();
        }
    } // if

    FunDcl* pFunDcl = new FunDcl(pFunty, name, datum);
    addFunRef(pFunDcl);
    return pFunDcl;
} // PassParse::internFunDcl

PassParse::NameRef*
PassParse::internNameDcl(Val name)
{
    if (symbolp(name))
    {
        return internVarDcl(name);
    }
    else if (consp(name) &&
             car(name) == Qfunction &&
             consp(cdr(name)) &&
             cddr(name) == nil &&
             function_name_p(cadr(name)) )
    {
        return internFunDcl(cadr(name));
    }

    parseError("Expect function or variable name: ~S", name);
    return NULL;
} // PassParse::internNameDcl

PassParse::VarRef*
PassParse::internVarDcl(Val name)
{
    if (! symbolp(name))
    {
        parseError("Expect variable name: ~S", name);
        return NULL;
    }

    VarRef* pVarRef = internVarRef(name);
    if (pVarRef->GetLexEnv() == m_pLexEnv)
    {
        return pVarRef;
    }

    VarDcl* pVarDcl = new VarDcl(name);
    pVarDcl->SetFlags(pVarRef->GetFlags() & VarRef::Flag_Inherit);
    pVarDcl->SetTy(pVarRef->GetTy());
    addVarRef(pVarDcl);
    return pVarDcl;
} // PassParse::internVarDcl

void PassParse::parse_declare_declaration(Val declspec)
{
    parseError("Use declaim or proclaim for ~S.", declspec);
} // PassParse::parse_declare_declaration

void PassParse::parse_declare_dynamic_extent(Val declspec)
{
    foreach (List::Enum, oEnum, cdr(declspec))
    {
        if (NameRef* const pNameRef = internNameDcl(oEnum.Get()))
        {
            pNameRef->Mark(NameRef::Flag_DynamicExtent);

            if (FunDef* const pFunDef = pNameRef->DynamicCast<FunDef>())
            {
                pFunDef->GetFunction()->MarkFlag(
                    Function::Flag_DynamicExtent );
            }
        }
    } // for name
} // PassParse::parse_declare_dynamic_extent

void PassParse::parse_declare_ftype(Val declspec)
{
    const Type* pty = Type::Parse(cadr(declspec));
    //= <FIXME date="2008-06-29" by="yosi@msn.com">
    //=   We must check pty is subtype of function.
    //= </FIXME>
    foreach (List::Enum, oEnum, cddr(declspec))
    {
        if (FunRef* const pFunRef = internFunDcl(oEnum.Get()))
        {
            pFunRef->SetTy(pty);
        }
    } // for name
} // PassParse::parse_declare_ftype

void PassParse::parse_declare_function_name(Val declspec)
{
    if (! (consp(cdr(declspec)) && nil == cddr(declspec)))
    {
        parseError("Malformed function-name declaration: ~S",
            declspec );
        return;
    }

    if (symbolp(m_pLexEnv->m_name))
    {
        parseError("Function-name declaration appeared more than once: ~S",
            declspec );
        return;
    }

    if (one == m_pLexEnv->m_name)
    {
        m_pLexEnv->m_name = cadr(declspec);
        return;
    }

    parseError("Can't use function-name declaration here: ~S", declspec);
} // PassParse::parse_declare_function_name

void PassParse::parse_declare_ignore(Val declspec)
{
    foreach (List::Enum, oEnum, cdr(declspec))
    {
        NameRef* pNameRef = internNameDcl(oEnum.Get());
        if (NULL != pNameRef)
        {
            pNameRef->Mark(VarDcl::Flag_Ignore);
        }
    } // for name
} // PassParse::parse_declare_ignore

void PassParse::parse_declare_ignorable(Val declspec)
{
    foreach (List::Enum, oEnum, cdr(declspec))
    {
        if (NameRef* const pNameRef = internNameDcl(oEnum.Get()))
        {
            pNameRef->Mark(VarDcl::Flag_Ignorable);
        }
    } // for name
} // PassParse::parse_declare_ignorable

void PassParse::parse_declare_inline(Val declspec)
{
    foreach (List::Enum, oEnum, cdr(declspec))
    {
        if (FunRef* const pFunRef = internFunDcl(oEnum.Get()))
        {
            pFunRef->Mark(VarDcl::Flag_Inline);
        }
    } // for name
} // PassParse::parse_declare_inline

void PassParse::parse_declare_notinline(Val declspec)
{
    foreach (List::Enum, oEnum, cdr(declspec))
    {
        if (FunRef* const pFunRef = internFunDcl(oEnum.Get()))
        {
            pFunRef->Mark(VarDcl::Flag_Notinline);
        }
    } // for name
} // PassParse::parse_declare_notinline

void PassParse::parse_declare_optimize(Val declspec)
{
    // FIXME 2008-06-29 yosi@msn.com NYI declare optimize
    styleWarn("Ignore ~S", declspec);
} // PassParse::parse_declare_optimize

void PassParse::parse_declare_special(Val declspec)
{
    foreach (List::Enum, oEnum, cdr(declspec))
    {
        Val name = oEnum.Get();
        if (! symbolp(name))
        {
            parseError("Expect variable name: ~S", name);
            continue;
        }

        Val env = TLV(AenvironmentA);

        for (;;)
        {
            Environment* pEnv = env->StaticCast<Environment>();
            Val entry = gethash(name, pEnv->m_variables);
            if (nil == entry)
            {
                env = pEnv->m_outer;
                if (nil != env)
                {
                    continue;
                }
            }

            Val kind  = car(entry);
            Val datum = cdr(assq(kind, cdr(entry)));

            if (Kspecial == kind)
            {
                // OK
            }
            else if (nil == kind)
            {
                datum = intern_value_cell(name, Kspecial);
            }
            else
            {
                parseError("Can't declare ~S ~S as special.",
                    kind, name );
                continue;
            }

            Val frob = gethash(name, m_vartab);
            if (nil != frob)
            {
                VarRef* pVarRef = frob->To<VarRef>();
                if (VarDef* pVarDef = pVarRef->DynamicCast<VarDef>())
                {
                    if (pVarRef->GetLexEnv() == m_pLexEnv)
                    {
                        pVarDef->SetCell(datum);
                        continue;
                    }
                }
            } // if found

            addVarRef(new(this) VarPro(name, datum));
        } // for
    } // for name
} // PassParse::parse_declare_special

void PassParse::parse_declare_type(Val declspec)
{
    const Type* const pty = Type::Parse(cadr(declspec));
    foreach (List::Enum, oEnum, cddr(declspec))
    {
        VarRef* const pVarRef = internVarDcl(oEnum.Get());
        if (NULL != pVarRef)
        {
            pVarRef->SetTy(pty);

            if (VarDef* const pVarDef = pVarRef->DynamicCast<VarDef>())
            {
                if (Variable* const pVar = pVarDef->GetVar())
                {
                    pVar->SetTy(pty);
                }
            }
        }
    } // for name
} // PassParse::parse_declare_type

void PassParse::parse_declare_values(Val declspec)
{
    m_pLexEnv->SetTy(Type::Parse(declspec));
} // PassParse::parse_declare_special

void PassParse::parseDeclare(Val declspec)
{
    if (! consp(declspec))
    {
        parseError("Malformed declaration specifier: ~S", declspec);
        return;
    }

    for (
        const DeclareParserEntry* p = k_rgoDeclareParser;
        p < k_rgoDeclareParser + lengthof(k_rgoDeclareParser);
        p++ )
    {
        if (car(declspec) == p->m_id)
        {
            p->m_pfn(this, declspec);
            return;
        }
    } // for p

    parseError("Unknown declaration identifier: ~S", declspec);
} // PassParse::parseDeclare

Val PassParse::parseDecls(Val forms, bool fCanDoc)
{
    foreach (List::Enum, oEnum, forms)
    {
        Val form = oEnum.Get();

        if (fCanDoc && stringp(form))
        {
            // FIXME 2008-07-05 yosi@msn.com NYI doc-string
            fCanDoc = false;
            continue;
        }

        unless (consp(form) && Qdeclare == car(form))
        {
            return oEnum.GetCons();
        }

        foreach (List::Enum, oEnum, cdr(form))
        {
            parseDeclare(oEnum.Get());
        } // for
    } // for each elt
    return nil;
} // PassParse::parseDecls

} // Compiler
} // TInyCl
