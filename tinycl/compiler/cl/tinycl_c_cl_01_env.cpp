#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parser - Environment
// tinycl_c_cl_01_env.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_01_env.cpp#6 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

// [A]

void PassParse::activateFreeDcls()
{
    foreach (LexEnv::EnumFun, oEnum, m_pLexEnv)
    {
        if (! oEnum.Get()->Is<FunDef>())
        {
            activateFunDcl(oEnum.Get());
        }
    } // for each Fun

    foreach (LexEnv::EnumVar, oEnum, m_pLexEnv)
    {
        if (! oEnum.Get()->Is<VarDef>())
        {
            activateVarDcl(oEnum.Get());
        }
    } // for each var
} // PassParse::activateFreeDcls

void PassParse::activateFunDcl(FunRef* pFunRef)
{
    Val outer = gethash(pFunRef->GetName(), m_funtab);
    if (nil == outer)
    {
        pFunRef->SetOuter(NULL);
    }
    else
    {
        // pVarRef and pOuter may have same lexenv
        FunRef* pOuter = Fixnum::To<FunRef>(outer);
        ASSERT(pFunRef != pOuter);
        pFunRef->SetOuter(pOuter);
    }

    setf_gethash(Fixnum::Encode(pFunRef), pFunRef->GetName(), m_funtab);
} // PassParse::activateFunDcl

void PassParse::activateLexEnv()
{
    foreach (LexEnv::EnumFun, oEnum, m_pLexEnv)
    {
        activateFunDcl(oEnum.Get());
    } // for each Fun

    foreach (LexEnv::EnumVar, oEnum, m_pLexEnv)
    {
        activateVarDcl(oEnum.Get());
    } // for each var
} // PassParse::activateLexEnv

void PassParse::activateVarDcl(VarRef* pVarRef)
{
    Val outer = gethash(pVarRef->GetName(), m_vartab);
    if (nil == outer)
    {
        pVarRef->SetOuter(NULL);
    }
    else
    {
        // pVarRef and pOuter may have same lexenv
        VarRef* const pOuter = Fixnum::To<VarRef>(outer);
        if (pVarRef != pOuter)
        {
            // For (lambda (x x) x)
            pVarRef->SetOuter(pOuter);
        }
    } // if

    setf_gethash(Fixnum::Encode(pVarRef), pVarRef->GetName(), m_vartab);
} // PassParse::activateVarDcl

void PassParse::addFunRef(FunRef* pFunRef)
{
    m_pLexEnv->AddFunRef(pFunRef);
    activateFunDcl(pFunRef);
} // PassParse::addFunRef

void PassParse::addVarRef(VarRef* pVarRef)
{
    m_pLexEnv->AddVarRef(pVarRef);
    activateVarDcl(pVarRef);
} // PassParse::addVarRef

// [C]

Val PassParse::callMacroExpander(Val expander, Val form)
{
    if (! fboundp(Qc_call_macro_expander))
    {
        setf_symbol_function(
            symbol_function(Qfuncall),
            Qc_call_macro_expander );
    }

    Val expansion = funcall(Qc_call_macro_expander, expander, form, nil);

    Thread* pth = Thread::Get();
    if (pth->m_n == two)
    {
        Val cond = pth->mv_value[1];
        if (nil != cond)
        {
            parseError("Macro expansion failed with ~A.", cond);
        }
    }

    return expansion;
} // PassParse::callMacroExpander

// Note: We must not call optimizeLexVars here, since value of variable
// may be used as value of form.
void PassParse::closeLexEnv()
{
    foreach (LexEnv::EnumFun, oEnum, m_pLexEnv)
    {
        FunDef* const pFunDef = oEnum.Get()->DynamicCast<FunDef>();
        if (NULL == pFunDef) continue;
        if (pFunDef->IsIgnore()) continue;
        if (pFunDef->IsIgnorable()) continue;

        if (! pFunDef->IsFlag(FunDef::Flag_Used))
        {
            Val const name = pFunDef->GetName();
            if (SetfCell* p = name->DynamicCast<SetfCell>())
            {
                if (nil != p->m_name->StaticCast<Symbol>()->m_package)
                {
                    styleWarn("Function (setf ~S) isn't used.", p->m_name);
                }
            }
            else
            {
                if (nil != name->StaticCast<Symbol>()->m_package)
                {
                    styleWarn("Function ~S isn't used.", name);
                }
            }
        }
    } // for each Fun

    foreach (LexEnv::EnumVar, oEnum, m_pLexEnv)
    {
        VarDef* pVarDef = oEnum.Get()->DynamicCast<VarDef>();
        if (NULL == pVarDef) continue;
        if (nil != pVarDef->GetCell()) continue;
        if (pVarDef->IsIgnore()) continue;
        if (pVarDef->IsIgnorable()) continue;

        if (! pVarDef->IsFlag(FunDef::Flag_Read))
        {
            Val const name = pVarDef->GetName();
            if (nil != name->StaticCast<Symbol>()->m_package)
            {
                styleWarn("Variable ~S isn't used.", name);
            }
        }
    } // for each var

    deactivateLexEnv();
} // PassParse::closeLexEnv

// [D]
void PassParse::deactivateLexEnv()
{
    foreach (FunRefs::Enum, oEnum, m_pLexEnv)
    {
        FunRef* pFunRef = oEnum.Get();
        FunRef* pOuter = pFunRef->GetOuter();
        Val outer = pOuter ? Fixnum::Encode(pOuter) : nil;
        setf_gethash(outer, pFunRef->GetName(), m_funtab);
    } // for each funref

    foreach (VarRefs::Enum, oEnum, m_pLexEnv)
    {
        VarRef* pVarRef = oEnum.Get();
        VarRef* pOuter = pVarRef->GetOuter();
        Val outer = pOuter ? Fixnum::Encode(pOuter) : nil;
        setf_gethash(outer, pVarRef->GetName(), m_vartab);
    } // for each varref
} // PassParse::deactivateLexEnv

// [F]

PassParse::FunRef* PassParse::findFunRef(Val fname)
{
    Val cell = fname;
    if (consp(cell))
    {
        cell = intern_setf_cell(cadr(cell));
    }

    Val frob = gethash(cell, m_funtab);
    if (nil != frob)
    {
        return frob->To<FunRef>();
    }

    Val env = TLV(AenvironmentA);

    for (;;)
    {
        Environment* pEnv = env->StaticCast<Environment>();

        Val entry = gethash(cell, pEnv->m_functions);

        if (nil == entry)
        {
            env = pEnv->m_outer;
            if (nil == env)
            {
                return NULL;
            }

            continue;
        } // if

        Val kind  = car(entry);
        Val datum = cdr(assq(kind, cdr(entry)));

        FunRef* pFunRef;

        if (Kmacro == kind)
        {
            pFunRef = new(this) FunMac(cell, datum);
        }
        else if (Kfunction == kind)
        {
            Val const alist = cdr(entry);
            Val const funty = cdr(assq(Qftype, alist));

            TyFunction* const pFunty = Type::Parse(funty)->DynamicCast<TyFunction>();
            FunName* const pFunName = new FunName(
                fname, 
                alist, 
                NULL == pFunty ? tyUnknownFunction : pFunty );

            pFunRef = new(this) FunPro(pFunty, fname, pFunName);
        }
        else if (Kspecial_operator == kind)
        {
            pFunRef = new(this) FunSpecial(cell);
        }
        else
        {
            return NULL;
        }

        setf_gethash(Fixnum::Encode(pFunRef), cell, m_funtab);
        return pFunRef;
    } // for
} // PassParse::findFunRef

Val PassParse::findVarCell(Val name)
{
    for (
        VarRef* pRunner = findVarRef(name);
        NULL != pRunner;
        pRunner = pRunner->GetOuter() )
    {
        if (VarDcl* pVarDcl = pRunner->DynamicCast<VarDcl>())
        {
            Val cell = pVarDcl->GetCell();
            if (nil != cell) return cell;
        }
        else if (VarPro* pVarPro = pRunner->DynamicCast<VarPro>())
        {
            return pVarPro->GetCell();
        }
        else if (pRunner->Is<VarEntity>())
        {
            break;
        }
    } // for each var
    return nil;
} // PassParse::isSpecialVariable

PassParse::VarRef* PassParse::findVarRef(Val name)
{
    ASSERT(symbolp(name));

    Val frob = gethash(name, m_vartab);
    if (nil != frob)
    {
        return frob->To<VarRef>();
    }

    Val env = TLV(AenvironmentA);

    for (;;)
    {
        Environment* pEnv = env->StaticCast<Environment>();
        Val entry = gethash(name, pEnv->m_variables);

        if (nil == entry)
        {
            env = pEnv->m_outer;
            if (nil == env)
            {
                return NULL;
            }
            continue;
        }

        Val kind  = car(entry);
        Val datum = cdr(assq(kind, cdr(entry)));

        VarRef* pVarRef;

        if (Kconstant == kind)
        {
            pVarRef = new(this) VarConst(name, datum);
        }
        else if (Kspecial == kind)
        {
            pVarRef = new(this) VarPro(name, datum);
        }
        else if (Ksymbol_macro == kind)
        {
            pVarRef = new(this) VarMac(name, datum);
        }
        else
        {
            return NULL;
        }

        setf_gethash(Fixnum::Encode(pVarRef), name, m_vartab);
        return pVarRef;
    } // for
} // PassParse::findVarRef

PassParse::FunRef* PassParse::internFunRef(Val fname)
{
    Val cell = fname;
    if (consp(cell))
    {
        cell = intern_setf_cell(cadr(cell));
    }

    if (FunRef* const pFunRef = findFunRef(cell))
    {
        return pFunRef;
    }

    if (SetfCell* p = cell->DynamicCast<SetfCell>())
    {
        if (nil == p->m_function)
        {
            styleWarn("Use undefined function (setf ~S)", p->m_name);
        }
    }
    else if (Symbol* p = cell->DynamicCast<Symbol>())
    {
        if (nil == p->m_function)
        {
            styleWarn("Use undefined function ~S", cell);
        }
    }
    else
    {
        COMPILER_INTERNAL_ERROR();
        fname = make_symbol(make_string("err"));
    }

    FunName* pFunName = new FunName(fname, nil, tyUnknownFunction);
    FunPro* pFunPro = new(this) FunPro(tyUnknownFunction, fname, pFunName);
    setf_gethash(Fixnum::Encode(pFunPro), cell, m_funtab);
    return pFunPro;
} // PassParse::internFunRef

/// <summary>
///   Interns variable reference.
/// </summary>
/// <param name="name">A name of variable</param>
/// <returns>A VarRef</returns>
PassParse::VarRef* PassParse::internVarRef(Val const name)
{
    ASSERT(symbolp(name));

    VarRef* const pVarRef = findVarRef(name);
    if (NULL != pVarRef)
    {
        return pVarRef;
    }

    styleWarn("Use undefined variable ~S", name);

    Val cell = find_value_cell(name);
    if (nil == cell)
    {
        cell = intern_value_cell(name, Kspecial);
    }

    VarPro* const pVarPro = new(this) VarPro(name, cell);
    setf_gethash(Fixnum::Encode(pVarPro), name, m_vartab);
    return pVarPro;
} // PassParse::internVarRef

Output* PassParse::internVarCell(VarDef* pVarDef)
{
    Variable* pVar   = pVarDef->GetVar();
    Output*   pRcell = pVar->GetRd();

    if (pVar->GetOwner() != m_pOwner)
    {
        pRcell = m_pOwner->InternUpVar(pVar);
        pVarDef->m_cUpRefs += 1;
    }

    return pRcell;
} // PassParse::internVarCell

void PassParse::markFunUse(FunRef* pFunRef)
{
    ASSERT(NULL != pFunRef);

    for (
        FunRef* pRunner = pFunRef;
        NULL != pRunner;
        pRunner = pRunner->GetOuter() )
    {
        if (pRunner->IsIgnore())
        {
            styleWarn("Use ignored function~S.", pRunner->GetName());
        }

        if (! pRunner->Is<FunDcl>())
        {
            pRunner->Mark(FunDef::Flag_Used);
            break;
        }
    } // for
} // PassParse::markFunUse

void PassParse::markVarUse(VarRef* pVarRef)
{
    ASSERT(NULL != pVarRef);

    for (
        VarRef* pRunner = pVarRef;
        NULL != pRunner;
        pRunner = pRunner->GetOuter() )
    {
        if (pRunner->IsIgnore())
        {
            styleWarn("Use ignored variable ~S.", pRunner->GetName());
        }

        if (! pRunner->Is<VarDcl>())
        {
            pRunner->Mark(VarDef::Flag_Read);
            break;
        }
    } // for
} // PassParse::markVarUse

} // Compiler
} // TinyCl
