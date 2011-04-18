#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - x86 - Runtime
// tinycl_x86_rtl.cpp
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_rtl.cpp#11 $
//
#include "./tinycl_x86.h"

namespace TinyCl
{

using namespace X86;

namespace
{

static Val copyCodeAnnot(
    Thread*         const pth,
    Val             const newfun,
    Val             const templ,
    FunObj::Annot   const oAnnot )
{
    const FunObj* const pTempl = templ->StaticCast<FunObj>();
    FunObj*       const pNewFun = newfun->StaticCast<FunObj>();

    switch (oAnnot.m_eKind)
    {
    case FunObj::Annot::Kind_NamedCallee:
    {
        Val const callee = pTempl->FetchCallee(oAnnot.m_ofs);
        Val value0 = pth->mv_value[0];
        Val value1 = pth->mv_value[1];
        register_caller(callee, newfun);
        pth->mv_value[0] = value0;
        pth->mv_value[1] = value1;
        goto update_call_site;
    } // NamedCallee

    case FunObj::Annot::Kind_LocalCallee:
        goto update_call_site;

    update_call_site:
    {
        Val callee = pTempl->FetchCallee(oAnnot.m_ofs);
        pNewFun->PatchCallee(oAnnot.m_ofs, callee);
        break;
    } // update_call_site

    case FunObj::Annot::Kind_AbsLabel:
    {
        Int iAddr = pTempl->FetchUn(oAnnot.m_ofs);
            iAddr -= templ->ToInt();
            iAddr += newfun->ToInt();

        pNewFun->PatchUn(oAnnot.m_ofs, static_cast<UInt>(iAddr));
        break;
    } // Kind_AbsLabel
    } // switch annon

    return newfun;
} // copyCodeAnnot

} // namespace

namespace CommonLisp
{

// [C]
defun(constantly, (Val const value))
{
    // 0000 B8 xx xx xx xx MOV EAX, imm
    // 0005 33 D2          XOR EDX, EDX
    // 0007 C3             RET
    static const uint8 k_rgbFin[] =
    {
        0xB8, 0x00, 0x00, 0x00, 0x00,
        0x33, 0xD2,
        0xC3,
    }; // k_rgbFin

    FunObj::Annot const oAnnot(FunObj::Annot::Kind_LispVal, 1);

    Val const fn = makeFunObj(
        CLASSD_native_code_function,
        nil,
        0,
        k_rgbFin,
        lengthof(k_rgbFin),
        &oAnnot,
        4 );

    fn->StaticCast<FunObj>()->PatchVal(1, value);
    return fn;
} // constantly

} // CommonLisp

Val CallFunction(Val fn, Val* mv, uint n)
{
    Thread* pth = Thread::Get();
    ::CopyMemory(pth->mv_value, mv, sizeof(Val) * n);
    pth->m_fn = fn;
    pth->m_n  = Fixnum::Encode(n);

    typedef Val (__fastcall *Fn)(Thread*);

    Fn pfn = reinterpret_cast<Fn>(
        QBBfuncall->StaticCast<Symbol>()->
            m_function->StaticCast<FunObj>()->GetCodeStart() );

    return pfn(pth);
} // CallFunction

size_t Host::ComputeFunFrameSize(Val const fn, const RaSlot* const pRa)
{
    FunObj* const pFunObj = fn->StaticCast<FunObj>();

    switch (pFunObj->GetFrameKind())
    {
    case FunObj::FrameKind_Fixed:
        return pFunObj->GetFixedFrameSize();

    case FunObj::FrameKind_Restify:
    {
        UInt* const pRaPtr = reinterpret_cast<UInt*>(
            pRa->ToInt() + pFunObj->GetFixedFrameSize() );

        return *pRaPtr - pRa->ToInt();
    } // variable

    default:
        CAN_NOT_HAPPEN();
    } // switch kind
} // Host::ComputeFunFrameSize

Val makeFunObj(
    Val                     const classd,
    Val                     const name,
    int                     const cbFrame,
    const uint8*            const pbTempl,
    size_t                  const cbCode,
    const FunObj::Annot*    const prgoAnnot,
    size_t                  const cbAnnots )
{
    size_t const cbGcMap = 4;

    size_t cbFunObj  = sizeof(FunObj) + sizeof(FunObj::FunDesc);
           cbFunObj += RoundUp(cbCode, 4);
           cbFunObj += cbAnnots;
           cbFunObj += cbGcMap;
           cbFunObj  = RoundUp(cbFunObj, FunObj::Align);

    Val const fn = Thread::Get()->AllocCode(classd, cbFunObj);

    FunObj* const pFunObj = fn->StaticCast<FunObj>();

    uint8* const pbCode = pFunObj->GetCodeStart();
    ::CopyMemory(pbCode, pbTempl, cbCode);

    FunObj::FunDesc* pDesc = pFunObj->GetFunDesc();
    pDesc->m_nFrame = (cbFrame + 4) << 2;
    pDesc->m_cbCode = static_cast<uint32>(cbCode);

    pDesc->m_ofsAnnot = static_cast<uint32>(
        sizeof(FunObj) +
        RoundUp(cbCode, 4) );

    pDesc->m_ofsGcMap = pDesc->m_ofsAnnot + cbAnnots;

    ::CopyMemory(
        pFunObj->GetAnnotStart(),
        prgoAnnot,
        cbAnnots );

    pFunObj->GetGcMap()[0] = 0;

    pFunObj->m_frob   = name;
    pFunObj->m_cookie = reinterpret_cast<Val>(
        static_cast<Int>(FunObj::Cookie) );

    return fn;
} // makeFunObj

//////////////////////////////////////////////////////////////////////
//
// Map Program Counter to Function Object
//  o We don't check m_classd here, since this function is called
//    during GC.
Val Host::MapRaToFn(const RaSlot* const pRa)
{
    uint8* pbRunner = reinterpret_cast<uint8*>(pRa->m_nIp & ~15);

    for (;;)
    {
        FunObj* const pFunObj = reinterpret_cast<FunObj*>(pbRunner);

        if (FunObj::Cookie == pFunObj->m_cookie->ToInt())
        {
            return pFunObj->Encode();
        }

        pbRunner -= 16;
    } // for
} // Host::MapRaToFn

void Thread::PlatformRestart()
{
    ThreadExtra* const p = reinterpret_cast<ThreadExtra*>(
        ToInt() - sizeof(ThreadExtra) );

    p->m_FixnumOne = Fixnum::Encode(1);

    #define codeStart(mp_x) \
        Q ## mp_x ->StaticCast<Symbol>()-> \
            m_function->StaticCast<FunObj>()->GetCodeStart()

    p->m_ArityError        = codeStart(Barity_error);
    p->m_NotFunction       = codeStart(Bnot_function);
    p->m_TypeError         = codeStart(Ptype_error);
    p->m_UndefinedFunction = codeStart(Pundefined_function);

    #undef codeStart
} // Thread::PlatformRestart

defun(Bmake_closureV, (Thread* pth))
{
    Val const templ = pth->m_fn;
    FunObj* const pTempl = templ->StaticCast<FunObj>();

    Val closure = pth->AllocCode(
        pTempl->m_classd,
        pTempl->m_cbFunction );

    FunObj* const pClosure = closure->StaticCast<FunObj>();

    ::CopyMemory(
        pClosure + 1,
        pTempl + 1,
        pTempl->m_cbFunction - sizeof(*pTempl) );

    foreach (FunObj::EnumAnnot, oEnum, pTempl)
    {
        FunObj::Annot const oAnnot = oEnum.Get();

        switch (oAnnot.m_eKind)
        {
        case FunObj::Annot::Kind_ClosedLit:
        {
            Int const i = Fixnum::Decode_(pTempl->FetchVal(oAnnot.m_ofs));
            pClosure->PatchVal(oAnnot.m_ofs, pth->mv_value[i]);
            break;
        } // ClosedLit

        case FunObj::Annot::Kind_ClosedVar:
        {
            Int const i = Fixnum::Decode_(pTempl->FetchVal(oAnnot.m_ofs));
            Int iPtr = pth->mv_value[i]->StaticCast<ClosedCell>()->ToInt();
                iPtr += offsetof(ClosedCell, m_value);

            pClosure->PatchUn(
                oAnnot.m_ofs,
                static_cast<UInt>(iPtr) );
            break;
        } // ClosedVar

        default:
            copyCodeAnnot(pth, closure, templ, oAnnot);
            break;
        } // switch Annot
    } // for each Annot

    pClosure->m_frob = pTempl->m_frob;

    // Make closure GC aware.
    pClosure->m_cookie = reinterpret_cast<Val>(
        static_cast<Int>(FunObj::Cookie) );

    return closure;
} // Bmake_closure

// [A]
defun(allocate_funcallable_instance, (Val const classd))
{
    // 0000 E8 xx xx xx xx CALL !uninitialized_funcallable_instance
    // 0005 C3             RET
    static uint8 const k_rgbFin[] =
    {
        0xE8, 0x00, 0x00, 0x00, 0x00,
        0xC3,
    }; // k_rgbFin

    FunObj::Annot const oAnnot(FunObj::Annot::Kind_LocalCallee, 1);

    // FIXME 2008-01-05 yosi@msn.com Should we have Thread::AllocStorage?
    size_t cb = classd->StaticCast<ClassD>()->GetFixedSize();
    cb -= sizeof(Layout_storage);
    Int cSlots = cb / sizeof(Val);

    Val const storage = Thread::Get()->AllocVector(
        CLASSD_storage,
        Fixnum::Encode(cSlots) );

    {
        Storage* const p = storage->StaticCast<Storage>();
        p->m_storaged = classd;
        for (Int i = 0; i < cSlots; i++)
        {
            p->GetStart()[i] = MARKER_unbound;
        } // for i
    }

    Val const fin = makeFunObj(
        classd,
        storage,
        0,
        k_rgbFin,
        lengthof(k_rgbFin),
        &oAnnot,
        4 );

    fin->StaticCast<FunObj>()->PatchCallee(
        1,
        symbol_function(QBuninitialized_funcallable_instance) );

    {
        FunObj* const p = fin->StaticCast<FunObj>();
        p->m_frob = storage;
    }

    return fin;
} // allocate_funcallable_instance

// [F]
defun(funcallable_instance_function, (Val fin))
{
    check_type(fin, funcallable_standard_object);
    return fin->StaticCast<FunObj>()->FetchCallee(1);
} // function_instance_function

defun(function_name, (Val const fn))
{
    check_type(fn, function);
    FunObj* const pFun = fn->StaticCast<FunObj>();
    Val frob = pFun->m_frob;
    if (Storage* p = frob->DynamicCast<Storage>())
    {
        frob = reinterpret_cast<Layout_generic_function*>(p)->m_name;
    }
    return frob;
} // function_name

defun(make_not_function_function, (Val const name))
{
    // 0000 B8 imm32    mov $r0, '<name>
    // 0005 FF 65 E8    jmp [EBP+SVC_not_function]
    static const uint8 k_rgbNotFunction[] =
    {
        0xB8, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0x65, 0xE8,
    }; // k_rgbNotFunction

    Val fname = name;
    if (SetfCell* p = name->DynamicCast<SetfCell>())
    {
        fname = list(Qsetf, p->m_name);
    }

    FunObj::Annot const oAnnot(FunObj::Annot::Kind_LispVal, 1);

    Val const fn = makeFunObj(
        CLASSD_not_function_function,
        fname,
        0,
        k_rgbNotFunction,
        sizeof(k_rgbNotFunction),
        &oAnnot,
        4 );

    fn->StaticCast<FunObj>()->PatchVal(1, fname);

    return fn;
} // make_not_function_function

defun(make_undefined_function_function, (Val const name))
{
    // 0000 B8 imm32    mov $r0, '<name>
    // 0005 FF 65 00    jmp [EBP+SVC_undefined_function]
    static const uint8 k_rgbUndefinedFunction[] =
    {
        0xB8, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0x65, 0xF0,
    }; // k_rgbUndefinedFunction

    Val fname = name;
    if (SetfCell* p = name->DynamicCast<SetfCell>())
    {
        fname = list(Qsetf, p->m_name);
    }

    FunObj::Annot const oAnnot(FunObj::Annot::Kind_LispVal, 1);

    Val const fn = makeFunObj(
        CLASSD_undefined_function_function,
        fname,
        0,
        k_rgbUndefinedFunction,
        sizeof(k_rgbUndefinedFunction),
        &oAnnot,
        4 );

    fn->StaticCast<FunObj>()->PatchVal(1, fname);

    return fn;
} // make_undefined_function_function

defun(set_funcallable_instance_function, (Val const fin, Val const fn))
{
    check_type(fin, funcallable_standard_object);
    check_type(fn, function);
    {
        FunObj* const p = fin->StaticCast<FunObj>();
        p->GetCodeStart()[0] = op_JMP_Jv;
        p->PatchCallee(1, fn);
    }
    return fn;
} // st_funcallable_instance_function

defun(update_callers, (Val const cell, Val const new_callee))
{
    ASSERT(symbolp(cell) || setf_cell_p(cell));

    Val const entry = intern_callee(cell);
    Val const caller_set = cdr(entry);
    Val const old_callee = car(entry);
    Val const end = svref(caller_set, zero);
    Val count = zero;
    for (
        Val index = one;
        lt(index, end);
        index = add(index, 1) )
    {
        Val caller = svref(caller_set, index);
        FunObj* pCaller = caller->StaticCast<FunObj>();
        foreach (FunObj::EnumAnnot, oEnum, pCaller)
        {
            FunObj::Annot oAnnot = oEnum.Get();
            if (FunObj::Annot::Kind_NamedCallee == oAnnot.m_eKind)
            {
                Val callee = pCaller->FetchCallee(oAnnot.m_ofs);
                if (callee == old_callee)
                {
                    pCaller->PatchCallee(oAnnot.m_ofs, new_callee);
                    count = add(count, 1);
                } // if
            } // if
        } // for each annot
    } // for each elt

    setf_car(new_callee, entry);

    return count;
} // update_callers

} // TinyCl
