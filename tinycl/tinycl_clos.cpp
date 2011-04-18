#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - CLOS Runtime
// tinycl_rtl_clos.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_clos.cpp#5 $
//
#define DEBUG_GF 1
#include "./tinycl_clos.h"

namespace TinyCl
{

/// <summary>
///  Simple discriminator
/// </summary>
Val discriminator(Thread* const pth, Val const gf)
{
    GenericFunction* const
        pGf = gf->StaticCast<GenericFunction>();

    MethodCache* const
        pCache = pGf->m_method_cache->StaticCast<MethodCache>();

    const UInt cSpecializers = pCache->GetSpecializerCount();
    ASSERT(cSpecializers >= 1);
    ASSERT(cSpecializers <= lengthof(pth->mv_value));

    UInt nHash = 0;
    foreach (
        SimpleVector::Enum,
        oEnum,
        pCache->m_order->StaticCast<SimpleVector>() )
    {
        Int nNth = Fixnum::Decode_(oEnum.Get());
        Val arg = pth->mv_value[nNth];
        Val classd = classd_of(arg);
        if (zero == classd->StaticCast<ClassD>()->m_hash_code)
        {
            Val args = pth->ValuesToList();
            funcall(Qupdate_obsolte_instance, arg);
            pth->ValuesFromList(args);
            classd = classd_of(arg);
        }

        nHash = MethodCache::Hash(classd, nHash);
    } // for each arg

    Val emf = pCache->Get(pth, nHash);

    if (nil == emf)
    {
        Val const args = pth->ValuesToList();
        #if DEBUG_GF
            DEBUG_FORMAT("~S ~S~%", gf, args);
        #endif
        emf = funcall(Qcompute_effective_method_using_arguments, gf, args);
        pth->ValuesFromList(args);
        pCache->Put(pth, nHash, emf);
    } // if

    return funcall_(emf);
} // discriminator

bool funcallable_standard_object_p(Val x)
{
    if (! functionp(x)) return false;
    Val classd = x->StaticCast<CodeObject>()->m_classd;
    return subclassp(
        classd->StaticCast<ClassD>()->m_class,
        CLASS_funcallable_standard_object );
} // funcallable_standard_object_p

} // TinyCl
