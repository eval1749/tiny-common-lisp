#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Garbage Collector
// tinycl_gc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_gc.cpp#3 $
//
#define DEBUG_GC 1
#include "../../tinycl_dll_link.h"
#include "../../tinycl_gc.h"
#include "./tinycl_x86.h"
#include "./tinycl_x86_ke_gcmap.h"

namespace TinyCl
{

using namespace X86;

// [P]
// Updates relative addresses
void Gc::prepareCode(Val newfn, Val oldfn)
{
    FunObj* pOld = reinterpret_cast<FunObj*>(
        oldfn->ToInt() - FunObj::Tag );

    if (0 == pOld->m_cookie)
    {
        return;
    }

    FunObj* pNew = reinterpret_cast<FunObj*>(
        newfn->ToInt() - FunObj::Tag );

    foreach (FunObj::EnumAnnot, oEnum, pNew)
    {
        FunObj::Annot oAnnot = oEnum.Get();

        switch (oAnnot.m_eKind)
        {
        case FunObj::Annot::Kind_NamedCallee:
        case FunObj::Annot::Kind_LocalCallee:
        {
            Val callee = pOld->FetchCallee(oAnnot.m_ofs);
            pNew->PatchCallee(oAnnot.m_ofs, callee);
            break;
        } // NamedCallee

        case FunObj::Annot::Kind_AbsLabel:
        {
            Int nLabel = pOld->FetchUn(oAnnot.m_ofs);
            nLabel -= pOld->ToInt();
            nLabel += pNew->ToInt();
            pNew->PatchUn(oAnnot.m_ofs, nLabel);
            break;
        } // AbsLabel
        } // switch Annot
    } // for each Annot
} // Gc::prepareCode

// [R]
void Gc::rememberCodeIf(int iAge, Val fn)
{
    FunObj* pFunObj = fn->StaticCast<FunObj>();

    if (0 == pFunObj->m_cookie)
    {
        remember(iAge, &pFunObj->m_classd);
        remember(iAge, &pFunObj->m_frob);
        return;
    } // if
    bool fHasOldToYoung = false;

    foreach (FunObj::EnumAnnot, oEnum, pFunObj)
    {
        FunObj::Annot oAnnot = oEnum.Get();

        Val val = nil;
        int ofs = oAnnot.m_ofs;
        switch (oAnnot.m_eKind)
        {
        case FunObj::Annot::Kind_ClosedLit:
        case FunObj::Annot::Kind_LispVal:
            val = pFunObj->FetchVal(ofs);
            break;

        case FunObj::Annot::Kind_ClosedVar:
        {
            UInt nAddr = pFunObj->FetchUn(ofs);
            if (nAddr > 0xFFFF)
            {
                nAddr -= offsetof(ClosedCell, m_value);
                val = reinterpret_cast<ClosedCell*>(nAddr)->Encode();
            } // if
            break;
        } // closed var

        case FunObj::Annot::Kind_DllLink:
        {
            DllEntry* pEntry = pFunObj->FetchDllEntry(ofs);
            val = pEntry->m_proc_info;
            break;
        } // dlllink

        case FunObj::Annot::Kind_NamedCallee:
        case FunObj::Annot::Kind_LocalCallee:
            val = pFunObj->FetchCallee(ofs);
            break;

        case FunObj::Annot::Kind_SymFun:
            val = pFunObj->FetchSymFun(ofs);
            break;

        case FunObj::Annot::Kind_SymSetf:
            val = pFunObj->FetchSymSetf(ofs);
            break;

        case FunObj::Annot::Kind_SymVal:
            val = pFunObj->FetchSymVal(ofs);
            break;
        } // switch annot

        if (nil != val)
        {
            if (ageOf(val) < iAge)
            {
                fHasOldToYoung = true;
            }
        } // if
    } // for each annot

    if (fHasOldToYoung)
    {
        rememberCode(iAge, fn);
    }
    else
    {
        remember(iAge, &pFunObj->m_classd);
        remember(iAge, &pFunObj->m_frob);
    }
} // Gc::rememberCodeIf

// [S]
void Gc::scanCode(int const iAge, Val const fn)
{
    FunObj* const pFunObj = fn->StaticCast<FunObj>();

    if (0 == pFunObj->m_cookie)
    {
        updateCell(iAge, &pFunObj->m_classd);
        updateCell(iAge, &pFunObj->m_frob);
        return;
    } // if

    // We'll remember m_classd and m_name after annotation
    // scanning.
    pFunObj->m_classd = moveObject(pFunObj->m_classd);
    pFunObj->m_frob   = moveObject(pFunObj->m_frob);

    bool fHasOldToYoung = false;

    foreach (FunObj::EnumAnnot, oEnum, pFunObj)
    {
        FunObj::Annot oAnnot = oEnum.Get();

        Val val = nil;
        int ofs = oAnnot.m_ofs;
        switch (oAnnot.m_eKind)
        {
        case FunObj::Annot::Kind_ClosedLit:
        case FunObj::Annot::Kind_LispVal:
            val = moveObject(pFunObj->FetchVal(ofs));
            pFunObj->PatchVal(ofs, val);
            break;

        case FunObj::Annot::Kind_ClosedVar:
        {
            UInt nAddr = pFunObj->FetchUn(ofs);
            if (nAddr > 0xFFFF)
            {
                nAddr -= offsetof(ClosedCell, m_value);
                val = reinterpret_cast<ClosedCell*>(nAddr)->Encode();
                val = moveObject(val);
                pFunObj->PatchUn(
                    ofs,
                    &val->StaticCast<ClosedCell>()->m_value );
            } // if
            break;
        } // closed var

        case FunObj::Annot::Kind_DllLink:
        {
            DllEntry* const pEntry = pFunObj->FetchDllEntry(ofs);
            val = moveObject(pEntry->m_proc_info);
            pEntry->m_proc_info = val;
            break;
        } // dlllink

        case FunObj::Annot::Kind_NamedCallee:
        case FunObj::Annot::Kind_LocalCallee:
            val = pFunObj->FetchCallee(ofs);
            val = moveObject(val);
            pFunObj->PatchCallee(ofs, val);
            break;

        case FunObj::Annot::Kind_SymFun:
            val = pFunObj->FetchSymFun(ofs);
            val = moveObject(val);
            pFunObj->PatchSymFun(ofs, val);
            break;

        case FunObj::Annot::Kind_SymSetf:
            val = pFunObj->FetchSymSetf(ofs);
            val = moveObject(val);
            pFunObj->PatchSymSetf(ofs, val);
            break;

        case FunObj::Annot::Kind_SymVal:
            val = pFunObj->FetchSymVal(ofs);
            val = moveObject(val);
            pFunObj->PatchSymVal(ofs, val);
            break;
        } // switch annot

        if (nil != val)
        {
            if (ageOf(val) < iAge)
            {
                fHasOldToYoung = true;
            }
        } // if
    } // for each annot

    if (fHasOldToYoung)
    {
        rememberCode(iAge, fn);
    }
    else
    {
        remember(iAge, &pFunObj->m_classd);
        remember(iAge, &pFunObj->m_frob);
    }
} // Gc::scanCode

// [I]
void Gc::updateFrame(Thread* pth, Frame* pFrame)
{
    ASSERT(NULL != pth);
    ASSERT(NULL != pFrame);

    #if DEBUG_GC
        if (FunctionFrame* p = pFrame->DynamicCast<FunctionFrame>())
        {
            DEBUG_FORMAT("~X~X Fun ~S~%",
                Fixnum::Encode(pFrame->ToInt() >> 4),
                Fixnum::Encode(pFrame->ToInt() & 15),
                p->m_fn );
        }
        else
        {
            DEBUG_FORMAT("~X~X ~C~C~C~%",
                Fixnum::Encode(pFrame->ToInt() >> 4),
                Fixnum::Encode(pFrame->ToInt() & 15),
                Character::FromCode((pFrame->m_eFrame >> 24) & 255),
                Character::FromCode((pFrame->m_eFrame >> 16) & 255),
                Character::FromCode((pFrame->m_eFrame >>  8) & 255) );
        }
    #endif

    if (BindFrame* const p = pFrame->DynamicCast<BindFrame>())
    {
        foreach (BindFrame::EnumEntry, oEnum, p)
        {
            BindFrame::Entry* const pEntry = oEnum.Get();
            pEntry->m_name  = moveObject(pEntry->m_name);
            pEntry->m_value = moveObject(pEntry->m_value);
        } // for entry
    }
    else if (pFrame->Is<BlockFrame>() ||
             pFrame->Is<CatchFrame>() )
    {
        XferFrame* const p = reinterpret_cast<XferFrame*>(pFrame);

        p->m_name = moveObject(p->m_name);

        if (Fixnum::Decode_(p->m_n) >= 4)
        {
            p->mv_value[3] = moveObject(p->mv_value[3]);
        }

        if (Fixnum::Decode_(p->m_n) >= 3)
        {
            p->mv_value[2] = moveObject(p->mv_value[2]);
        }

        if (Fixnum::Decode_(p->m_n) >= 2)
        {
            p->mv_value[1] = moveObject(p->mv_value[1]);
        }

        if (Fixnum::Decode_(p->m_n) >= 1)
        {
            p->mv_value[0] = moveObject(p->mv_value[0]);
        }
    }
    else if (FinallyFrame* const p = pFrame->DynamicCast<FinallyFrame>())
    {
        p->m_finally = moveObject(p->m_finally);
        Val* const pvalStart = reinterpret_cast<Val*>(p + 1);
        Val* const pvalEnd   = pvalStart + Fixnum::Decode_(p->m_n);
        for (Val* pval = pvalStart; pval < pvalEnd; pval++)
        {
            *pval = moveObject(*pval);
        } // for
    }
    else if (FunctionFrame* const p = pFrame->DynamicCast<FunctionFrame>())
    {
        uint const ofsRa = p->m_nCodeIndex;
        uint const ofsCall = ofsRa - GcMap::cbCall;

        Val const fn = moveObject(p->m_fn);
        FunObj* const pFunObj = fn->DynamicCast<FunObj>();

        *reinterpret_cast<Int*>(p->m_pStart) = 
            pFunObj->ToInt() + sizeof(FunObj) + ofsRa;

        GcMap oGcMap(pFunObj->GetGcMap(), pFunObj->GetGcMapSize());
        const GcMap::Bits* pBits = oGcMap.FindAt(ofsCall);
        if (NULL == pBits)
        {
            DEBUG_PRINTF(" No GcMap at %04x\n", ofsCall);
            return;
        }

        DEBUG_PRINTF(" GcDesc=%x at %04x\n", *pBits, ofsCall);

        uint nBits = *pBits++;
        bool fContinue = 0 != (nBits & 1);
        nBits >>= 1;
        GcMap::Kind eKind = static_cast<GcMap::Kind>(nBits & 3);
        nBits >>= 2;
        uint cBits = GcMap::BitsInWord - 3;
        switch (eKind)
        {
        case GcMap::StdCallDesc:
            break;

        case GcMap::CallDesc2:
            cBits -= 3;
            nBits >>= 3;
            break;

        case GcMap::CallDesc3:
            cBits -= 8;
            nBits >>= 8;
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch eKind

        Val* pval = p->m_pval;
        for (;;)
        {
            while (cBits > 0)
            {
                if (nBits & 1)
                {
                    DEBUG_FORMAT(" [esp+%ld]=%p\n",
                        pval - p->m_pval, *pval );

                    *pval = moveObject(*pval);
                }

                pval++;
                nBits >>= 1;
                cBits -= 1;
            } // while

            if (! fContinue)
            {
                break;
            }

            nBits = *pBits++;
            fContinue = 0 != (nBits & 1);
            nBits >>= 1;
            cBits -= 1;
        } // for
    }
    else if (HandlerFrame* const p = pFrame->DynamicCast<HandlerFrame>())
    {
        foreach (HandlerFrame::EnumCatch, oEnum, p)
        {
            HandlerFrame::Catch* pEntry = oEnum.Get();
            pEntry->m_fn   = moveObject(pEntry->m_fn);
            pEntry->m_type = moveObject(pEntry->m_type);
        } // for
    }
    else if (TagsFrame* const p = pFrame->DynamicCast<TagsFrame>())
    {
        p->m_fn = moveObject(p->m_fn);
        foreach (TagsFrame::EnumTag, oEnum, p)
        {
            TagsFrame::Tag* pEntry = oEnum.Get();
            pEntry->m_name = moveObject(pEntry->m_name);
        } // for
    }
    else if (TryCatchFrame* const p = pFrame->DynamicCast<TryCatchFrame>())
    {
        p->m_fn = moveObject(p->m_fn);
        foreach (TryCatchFrame::EnumCatch, oEnum, p)
        {
            TryCatchFrame::Catch* pEntry = oEnum.Get();
            pEntry->m_type = moveObject(pEntry->m_type);
        } // for
    }
    else
    {
        //CAN_NOT_HAPPEN();
    }
} // Gc::updateFrame

void Gc::updateThreadAfter(Thread*)
    {}

void Gc::updateThreadBefore(Thread*)
    {}


} // TinyCl
