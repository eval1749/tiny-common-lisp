#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Functions for Generic ISA
// arch/generic/tinycl_gen_fns.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/generic/tinycl_gen_fns.cpp#17 $
//
#define DEBUG_ENUM_STACK 1
#include "./tinycl_gen.h"

#include "../../tinycl_gc.h"
#include "../../tinycl_kernel.h"

namespace TinyCl
{

using namespace Generic;

Val __fastcall CallLisp(Thread*);

namespace
{

static void restoreValues(
    Thread*    const pth,
    XferFrame* const pFrame)
{
    if (le(pFrame->m_n, lengthof(pFrame->mv_value)))
    {
        pth->m_n = pFrame->m_n;
        for (Int i = 0; i < Fixnum::Decode_(pth->m_n); i++)
        {
            pth->mv_value[i] = pFrame->mv_value[i];
        } // for i
    }
    else
    {
        pth->ValuesFromList(pFrame->mv_value[0]);
    }
} // restoreValues

static void saveValues(
    Thread*    const pth,
    XferFrame* const pFrame)
{
    pFrame->m_n = pth->m_n;
    if (le(pth->m_n, lengthof(pFrame->mv_value)))
    {
        for (Int i = 0; i < Fixnum::Decode_(pth->m_n); i++)
        {
            pFrame->mv_value[i] = pth->mv_value[i];
        } // for i
    }
    else
    {
        pFrame->mv_value[0] = pth->ValuesToList();
    }
} // saveValues

} // namespace

Val BindFrame::Entry::GetCell() const
{
    if (m_name->Is<ValueCell>())
    {
        return m_name;
    }

    if (m_name->Is<Fixnum>())
    {
        Int const ofs = m_name->ToInt() - offsetof(Thread, mv_tlv);
        Val const index = reinterpret_cast<Val>(ofs);
        return svref(VAR(Atlv_vectorA), index);
    }

    return nil;
} // BindFrame::Entry::GetCell

/// <summary>
///   Unwinding operation of BindFrame.
/// </summary>
void BindFrame::Unwind(Thread* pth)
{
    foreach (EnumEntry, oEnum, this)
    {
        Entry* const p = oEnum.Get();
        if (fixnump(p->m_name))
        {
            Int const ofsTlv = reinterpret_cast<Int>(p->m_name);
            *reinterpret_cast<Val*>(pth->ToInt() + ofsTlv) = p->m_value;
        }
        else if (value_cell_p(p->m_name))
        {
            p->m_name->StaticCast<ValueCell>()->m_value = p->m_value;
        }
        else
        {
            error("Broken bind frame at #x~X",
                MakeInt(reinterpret_cast<Int>(this)) );
        }
    } // for each entry
} // BindFrame::Unwind

/// <summary>
///   Calls finally function with arguments in this frame.
///   <para>Note: Values has been saved.</para>
/// </summary>
void __fastcall FinallyFrame::Unwind(Thread* const pth)
{
    ::CopyMemory(
        pth->mv_value,
        this + 1,
        sizeof(Val) * Fixnum::Decode_(m_n) );

    pth->m_n = m_n;
    pth->m_fn = m_finally;
    CallLisp(pth);
} // FinallyFrame::Unwind

bool Gc::canGc()
{
    foreach (Executive::EnumThread, oEnum, Executive::Get())
    {
        Thread* const pThread = oEnum.Get();
        foreach (Thread::EnumStack, oEnum, pThread)
        {
            Frame* const pFrame = oEnum.Get();
            if (pFrame->Is<GcDisableFrame>())
            {
                DEBUG_PRINTF("Gc disable @%p\n", pFrame);
                return false;
            }

            if (pFrame->Is<ToForeignFrame>())
            {
                DEBUG_PRINTF("ToForeign @%p\n", pFrame);
                return false;
            }
        } // for frame
    } // for thread

    return true;
} // Gc::canGc

/// <summary>
///   Unwinds to specified by pFrame. If fFinally is true, values are
///   saved for executing finally procedure.
/// </summary>
void Thread::Unwinds(
    Frame* const pFrame,
    bool   const fFinally )
{
    if (fFinally)
    {
        saveValues(this, reinterpret_cast<XferFrame*>(pFrame));
    }

    foreach (Thread::EnumFrame, oEnum, this)
    {
        Frame* const pRunner = oEnum.Get();
        if (pRunner == pFrame)
        {
            break;
        }

        if (BlockFrame* const pFrame =
                pRunner->DynamicCast<BlockFrame>() )
        {
            pFrame->MakeAbandoned();
        }
        else if (CatchFrame* const pFrame =
                    pRunner->DynamicCast<CatchFrame>() )
        {
            pFrame->MakeAbandoned();
        }
        else if (TagsFrame* const pFrame =
                    pRunner->DynamicCast<TagsFrame>() )
        {
            pFrame->MakeAbandoned();
        }
    } // for each frame

    foreach (Thread::EnumFrame, oEnum, this)
    {
        Frame* const pRunner = oEnum.Get();
        if (pRunner == pFrame)
        {
            if (fFinally)
            {
                restoreValues(this, reinterpret_cast<XferFrame*>(pFrame));
            }
            return;
        }

        if (BindFrame* const pFrame =
                pRunner->DynamicCast<BindFrame>() )
        {
            pFrame->Unwind(this);
        }
        else if (FinallyFrame* const pFrame =
                    pRunner->DynamicCast<FinallyFrame>() )
        {
            m_fp = Fixnum::Encode(pFrame->m_pOuter);
            pFrame->Unwind(this);
        }
    } // for each frame

    CAN_NOT_HAPPEN();
} // Thread::Unwinds

RaSlot* ToForeignFrame::GetRaSlot()
{
    return reinterpret_cast<RaSlot*>(
        ToInt() - m_cbArgs - sizeof(RaSlot) );
} // ToForeignFrame::GetRaSlot

void Thread::EnumStack::setupFunFrame(RaSlot* pRa, Frame* pOuter)
{
    // Compute function frame that calls foreign function.
    Val fn = Host::MapRaToFn(pRa);

    size_t cbFunFrame = Host::ComputeFunFrameSize(fn, pRa);

    m_oFunFrame.m_pStart = pRa;

    m_oFunFrame.m_pEnd = reinterpret_cast<RaSlot*>(
        reinterpret_cast<Int>(m_oFunFrame.m_pStart) + cbFunFrame );

    #if DEBUG_ENUM_STACK
        DEBUG_FORMAT("~X-~X cb=~D ~S\n",
            Fixnum::Encode(reinterpret_cast<Int>(m_oFunFrame.m_pStart)),
            Fixnum::Encode(m_oFunFrame.m_pEnd->ToInt()),
            Fixnum::Encode(cbFunFrame),
            fn->StaticCast<Function>()->
                To<Layout_native_code_function>()->m_name );
    #endif

    m_oFunFrame.m_fn     = fn;
    m_oFunFrame.m_pOuter = pOuter;
    m_oFunFrame.m_pval   = reinterpret_cast<Val*>(pRa + 1);
    m_oFunFrame.m_pRa    = m_oFunFrame.m_pEnd;

    m_oFunFrame.m_nCodeIndex =
        pRa->m_nIp -
        fn->StaticCast<Function>()->ToInt() -
        sizeof(Layout_native_code_function);

    m_p = &m_oFunFrame;
} // Thread::EnumStack::computeFunctionFrame

/// <summary>
///   Advances the enumerator to the next stack element of the stack.
/// </summary>
void Thread::EnumStack::Next()
{
    ASSERT(NULL != m_p);

    #if DEBUG_ENUM_STACK
        DEBUG_PRINTF("%p %c%c%c outer=%p\n",
            m_p,
            (m_p->m_eFrame >> 24) & 0xff,
            (m_p->m_eFrame >> 16) & 0xff,
            (m_p->m_eFrame >>  8) & 0xff,
            m_p->m_pOuter );
    #endif

    if (ToForeignFrame* const p = m_p->DynamicCast<ToForeignFrame>())
    {
        setupFunFrame(p->GetRaSlot(), p->m_pOuter);
    }
    else if (ToKernelFrame* const p = m_p->DynamicCast<ToKernelFrame>())
    {
        setupFunFrame(p->GetRaSlot(), p->m_pOuter);
    }
    else
    {
        m_p = m_p->m_pOuter;

        FunctionFrame* const q = &m_oFunFrame;

        if (NULL == m_p)
        {
            DEBUG_PRINTF("We are at end.\n");
        }
        else if (q->m_fn == nil)
        {
            DEBUG_PRINTF("%p: We don't have function frame\n", m_p);
        }
        else if (q->HasFrame(m_p))
        {
            // m_p is a frame in current function frame.
        }
        else if (m_p->Is<FromForeignFrame>() &&
                 m_p->ToInt() == reinterpret_cast<Int>(q->m_pEnd + 1) )
        {
            // Current lisp function frame end
            DEBUG_PRINTF("%p: FromForeignFrame\n", m_p);
            q->m_fn = nil;
        }
        else
        {
            setupFunFrame(q->m_pEnd, m_p);
        }
    }
} // Thread::EnumStack::Next

defun(CommonLisp::funcallV, (Thread* const pth))
{
    pth->m_fn = pth->mv_value[0];

    ::CopyMemory(
        pth->mv_value,
        pth->mv_value + 1,
        Fixnum::Decode_(pth->m_n) * sizeof(Val) );

    pth->m_n = pth->m_n - Fixnum::One;

    return funcall_(pth->m_fn);
} // funcall

void __declspec(noreturn) __fastcall
Bgo(Val tag)
{
    TagsFrame::Tag* const pTag =
        reinterpret_cast<TagsFrame::Tag*>(tag);

    Thread* const pth = Thread::Get();

    foreach (Thread::EnumFrame, oEnum, pth)
    {
        Frame* const pRunner = oEnum.Get();
        if (TagsFrame* const pFrame = pRunner->DynamicCast<TagsFrame>())
        {
            if (pFrame->HasTag(pTag))
            {
                if (pFrame->IsAbandoned())
                {
                    error("Can't transfer control to abandoned tagbody ~S.",
                        pTag->m_name );
                }

                pth->Unwinds(pFrame);
                pFrame->Transfer(pth, pTag);
                // NOTREACHED
            }
        }
    } // for each frame

    error("Tagbody has gone.");
} // Bgo

defun(make_closed_cell, (Val datum))
{
    Val const cell = Thread::Get()->AllocRecord(CLASSD_closed_cell);
    ClosedCell* const p = cell->StaticCast<ClosedCell>();
    p->m_value = datum;
    return cell;
} // make_closed_cell

void __declspec(noreturn) __fastcall
BreturnV(Thread* const pth)
{
    bool fFinally = false;
    foreach (Thread::EnumFrame, oEnum, pth)
    {
        Frame* const pRunner = oEnum.Get();
        if (FinallyFrame* const pFrame =
                pRunner->DynamicCast<FinallyFrame>() )
        {
            fFinally = true;
        }
        else if (BlockFrame* const pFrame =
                    pRunner->DynamicCast<BlockFrame>() )
        {
            if (pFrame == reinterpret_cast<BlockFrame*>(pth->m_fn))
            {
                if (pFrame->IsAbandoned())
                {
                    error("Can't transfer cotnrol to abandoned block ~S.",
                        pFrame->m_name );
                }

                pth->Unwinds(pFrame, fFinally);
                pFrame->Transfer(pth);
                // NOTREACHED
            }
        } // if
    } // for each frame

    error("There is no such block ~S.",
        reinterpret_cast<BlockFrame*>(pth->m_fn)->m_name );
} // BreturnV

void __declspec(noreturn) __fastcall
BthrowV(Thread* const pth)
{
    Val const tag = pth->m_fn;

    bool fFinally = false;
    foreach (Thread::EnumFrame, oEnum, pth)
    {
        Frame* const pRunner = oEnum.Get();
        if (FinallyFrame* const pFrame =
                pRunner->DynamicCast<FinallyFrame>() )
        {
            fFinally = true;
        }
        else if (CatchFrame* const pFrame =
                    pRunner->DynamicCast<CatchFrame>() )
        {
            if (pFrame->m_name == tag)
            {
                if (pFrame->IsAbandoned())
                {
                    error("Can't transfer cotnrol to abandoned catch ~S.",
                        tag );
                }

                pth->Unwinds(pFrame, fFinally);
                pFrame->Transfer(pth);
                // NOTREACHED
            }
        }
    } // for each frame

    // si::catch-tag-not-seen
    error("There is no such catch tag ~S.", tag);
} // BthrowV

/// <summary>
///   C++ class for accessing lisp class STACK-FRAME.
/// </summary>
class StackFrame : public Record_<StackFrame, Layout_stack_frame>
{
    public: static Val ClassD_() { return CLASSD_stack_frame; }
}; // StackFrame

/// <summary>
///   Make list of stack frames of current thread.
/// </summary>
/// <returns>A list of stack frames</returns>
defun(make_stack_trace, ())
{
    Thread* pth = Thread::Get();

    Collector oTrace;
    foreach (Thread::EnumStack, oEnum, pth)
    {
        Frame* const pFrame = oEnum.Get();

        if (FunctionFrame* const p = pFrame->DynamicCast<FunctionFrame>())
        {
            Val const stack_frame = pth->AllocRecord(CLASSD_stack_frame);

            StackFrame* const pStackFrame =
                stack_frame->StaticCast<StackFrame>();

            pStackFrame->m_code_index  = Fixnum::Encode(p->m_nCodeIndex);
            pStackFrame->m_function    = p->m_fn;
            pStackFrame->m_line_number = zero;
            pStackFrame->m_line_column = zero;
            pStackFrame->m_pathname    = nil;

            oTrace.Add(stack_frame);
        } // if function frame
    } // for each frame
    return oTrace.Get();
} // make_stack_trace

/// <summary>
///   Printer for STACK-FRAME object.
/// </summary>
defmethod(print_object, stack_frame, (Val const frame, Val const stream))
{
    StackFrame* const p = frame->StaticCast<StackFrame>();
    format(stream, "#<Stack-Frame ~S at ~4,'0X>",
        function_name(p->m_function),
        p->m_code_index );
    return frame;
} // print_object

} // TinyCl
