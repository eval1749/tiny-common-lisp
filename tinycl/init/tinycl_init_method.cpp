#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Initialization
// tinycl_init.cpp
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_init_method.cpp#4 $
//
#define DEBUG_GF        0
#define DEBUG_METHOD    1
#include "./tinycl_init.h"

#include "../tinycl_clos.h"

namespace TinyCl
{

#include "./tinycl_install_arity.h"
#include "../tinycl_methods.inc"

static MethodEntry k_rgoMethodEntry[] =
{
    #include "./tinycl_method_entry.h"
    #include "../tinycl_methods.inc"
}; // k_rgoMethodEntry

static Val makeFname(Val fname)
{
    if (Symbol* q = fname->DynamicCast<Symbol>())
    {
        return fname;
    }
    else if (SetfCell* q = fname->DynamicCast<SetfCell>())
    {
        return list(Qsetf, q->m_name);
    }
    else
    {
        CAN_NOT_HAPPEN();
    }
} // makeFname

static Val makeLambdaList(int iMin, int iMax, int iRest)
{
    Val ll = nil;
    Val* px = &ll;
    char sz[2];
    sz[1] = 0;
    int k = 0;
    for (int i = 0; i < iMin; i++)
    {
        sz[0] = static_cast<char>('A' + k);
        k++;
        *px = list(intern(sz));
        px = &(*px)->StaticCast<Cons>()->m_cdr;
    } // for i

    if (iMin != iMax)
    {
        *px = list(QAoptional);
        px = &(*px)->StaticCast<Cons>()->m_cdr;
        for (int i = 0; i < iMin; i++)
        {
            sz[0] = static_cast<char>('A' + k);
            k++;
            *px = list(intern(sz));
            px = &(*px)->StaticCast<Cons>()->m_cdr;
        } // for i
    }

    if (iRest)
    {
        *px = list(QAoptional);
        px = &(*px)->StaticCast<Cons>()->m_cdr;

        sz[0] = static_cast<char>('A' + k);
        k++;
        *px = list(intern(sz));
    }

    return ll;
} // makeLambdaList

static Val installStaticGf(const MethodEntry* const p)
{
    Thread* const pth = Thread::Get();

    Val const mcache = pth->AllocRecord(CLASSD_method_cache);
    Val const finfo  = pth->AllocRecord(CLASSD_function_information);

    Val const gf = allocate_funcallable_instance(
        CLASSD_standard_generic_function );

    {
        GenericFunction* const q = gf->StaticCast<GenericFunction>();
        q->m_function_information = finfo;
        q->m_method_cache         = mcache;
        q->m_method_class         = CLASS_standard_method;
        q->m_method_combination   = VAR(Pstandard_method_combinationP);
        q->m_methods              = nil;
        q->m_name                 = makeFname(p->m_fname);
        q->m_plist                = nil;
    }

    {
        FunctionInformation* const q = finfo->StaticCast<FunctionInformation>();
        q->m_documentation = nil;
        q->m_max_params    = Fixnum::Encode(p->m_iMax);
        q->m_min_params    = Fixnum::Encode(p->m_iMin);
        q->m_keys          = nil;
        q->m_rest          = p->m_iRest ? t : nil;
    }

    {

        static Val order_zero;
        static Val order_one;

        Val order;
        if (symbolp(p->m_fname))
        {
            if (0 == order_zero)
            {
                order_zero = make_vector(one);
            }
            order = order_zero;
        }
        else
        {
            if (0 == order_one)
            {
                order_one = make_vector(one);
                setf_svref(one, order_one, zero);
            }
            order = order_one;
        }

        int const nLineSize  = 2;
        int cEntreis = nLineSize * p->m_iCacheSize;

        if (Qprint_object == p->m_fname)
        {
            cEntreis = nLineSize * 201;
        }

        MethodCache* const q = mcache->StaticCast<MethodCache>();
        q->m_count            = zero;
        q->m_default_emf      = nil;
        q->m_eql_caches       = nil;
        q->m_line_size        = Fixnum::Encode(nLineSize);
        q->m_order            = order;
        q->m_vector           = make_vector(Fixnum::Encode(cEntreis));
    }

    if (Symbol* const q = p->m_fname->DynamicCast<Symbol>())
    {
        q->m_function = gf;
    }
    else if (SetfCell* const q = p->m_fname->DynamicCast<SetfCell>())
    {
        q->m_function = gf;
    }
    else
    {
        error(Qtype_error,
            Kdatum, p->m_fname, 
            Kexpected_type, list(Qor, Qsymbol, Qsetf_cell) );
    }

    set_funcallable_instance_function(gf, MakeDiscriminator(gf, p->m_iVals));

    return gf;
} // InstallStaticGf

static void installMethod(MethodCache* pCache, Val klass, Val fn)
{
    Class* pClass = klass->StaticCast<Class>();

    if (nil != pClass->m_instanced)
    {
        Val classd = pClass->m_instanced;
        if (nil == pCache->StaticGet(&classd))
        {
            #if DEBUG_METHOD
                DEBUG_FORMAT("~S~%", klass);
            #endif

            pCache->StaticPut(&classd, fn);
        } // if
    } // if

    foreach (List::Enum, oEnum, pClass->m_direct_subclasses)
    {
        installMethod(pCache, oEnum.Get(), fn);
    } // for each subclass
} // installMethod

Val InstallStaticMethod(const MethodEntry* p)
{
    Thread* pth = Thread::Get();

    Val gf = symbolp(p->m_fname) ?
        p->m_fname->StaticCast<Symbol>()->m_function :
        p->m_fname->StaticCast<SetfCell>()->m_function;

    if (! functionp(gf))
    {
        gf = installStaticGf(p);
    }

    #if DEBUG_GF || DEBUG_METHOD
        DEBUG_FORMAT("~S~%", p->m_fname);
    #endif

    Val method = pth->AllocInstance(CLASSD_standard_method);

    Val fn = MakeWrapper(
        list(Qmethod,
            function_name(gf),
            list(class_name(p->m_class)) ),
        p->m_iMin,
        p->m_iMax,
        p->m_iRest,
        p->m_iVals,
        false,
        p->m_psz );

    // Make a method instance
    {
        Method* q = method->StaticCast<Method>();

        q->m_function = fn;
        q->m_generic_function = gf;
        q->m_lambda_list = makeLambdaList(p->m_iMin, p->m_iMax, p->m_iRest);
        q->m_qualifiers = nil;
        q->m_specializers = list(p->m_class);
    }

    push(method, p->m_class->StaticCast<Class>()->m_direct_methods);

    // Install method into method-cache
    {
        GenericFunction* pGf = gf->StaticCast<GenericFunction>();

        MethodCache* pCache = pGf->m_method_cache->StaticCast<MethodCache>();

        push(method, pGf->m_methods);

        installMethod(pCache, p->m_class, fn);
    }

    return method;
} // InstallStaticMethod

void InstallStaticMethods(const MethodEntry* pStart, uint cEntries)
{
    const MethodEntry* pEnd = pStart + cEntries;
    for (const MethodEntry* p = pStart; p < pEnd; p++)
    {
        InstallStaticMethod(p);
    } // for p
} // InstallStaticMethods

void InstallStaticMethods()
{
    InstallStaticMethods(k_rgoMethodEntry, lengthof(k_rgoMethodEntry));
} // InstallStaticMethods

void PopulateMethodCache(Val klass)
{
    Class* pClass = klass->StaticCast<Class>();
    Val classd = pClass->m_instanced;
    if (nil == classd) return;

    #if DEBUG_GF
        DEBUG_FORMAT("~S~%", pClass->m_class_precedence_list);
    #endif

    foreach (List::Enum, oEnum, cdr(pClass->m_class_precedence_list))
    {
        Class* pSuper = oEnum.Get()->StaticCast<Class>();
        foreach (List::Enum, oEnum, pSuper->m_direct_methods)
        {
            Method* pMethod = oEnum.Get()->StaticCast<Method>();

            GenericFunction* pGf = pMethod->m_generic_function->
                StaticCast<GenericFunction>();

            MethodCache* pCache = pGf->m_method_cache->
                StaticCast<MethodCache>();

            if (nil == pCache->StaticGet(&classd))
            {
                #if DEBUG_GF
                    DEBUG_FORMAT("install ~S ~S~%",
                        klass, pMethod->m_function );
                #endif

                pCache->StaticPut(&classd, pMethod->m_function);
            }
        } // for each method
    } // for each super
} // PopulateMethodCache

} // TinyCl
