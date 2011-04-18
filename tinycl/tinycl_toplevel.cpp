 #include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Toplevel
// tinycl_toplevel.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_toplevel.cpp#8 $
//
#include "./tinycl.h"
#include "../tinycl/init/tinycl_init.h"

namespace TinyCl
{

using namespace Private;

#if _DEBUG
    void PrintSxHashTable();
#endif

static void backtrace()
{
    int nNth = 0;
    foreach (Thread::EnumStack, oEnum, Thread::Get())
    {
        Frame* pFrame = oEnum.Get();
        Val addr;
        if (FunctionFrame* p = pFrame->DynamicCast<FunctionFrame>())
        {
            addr = MakeUInt(reinterpret_cast<UInt>(p->m_pRa));
        }
        else
        {
            addr = MakeUInt(pFrame->ToInt());
        }

        format(t, "; [~D] ~X ~C~C~C",
            Fixnum::Encode(nNth),
            addr,
            Character::FromCode((pFrame->m_eFrame >> 24) & 0xff),
            Character::FromCode((pFrame->m_eFrame >> 16) & 0xff),
            Character::FromCode((pFrame->m_eFrame >>  8) & 0xff) );
            
        if (FunctionFrame* p = pFrame->DynamicCast<FunctionFrame>())
        {
            format(t, " ~S~%", p->m_fn);
        }
        else if (BindFrame* p = pFrame->DynamicCast<BindFrame>())
        {
            foreach (BindFrame::EnumEntry, oEnum, p)
            {
                Val cell = oEnum.Get()->GetCell();
                if (ValueCell* p = cell->DynamicCast<ValueCell>())
                {
                    format(t, " ~S", p->m_name);
                }
                else if (TlvRecord* p = cell->DynamicCast<TlvRecord>())
                {
                    format(t, " ~S", p->m_name);
                }
                else
                {
                    format(t, " *~S", oEnum.Get()->m_name);
                }
            } // for each entry
            format(t, "~%");
        }
        else
        {
            write_char(TinyCl::Private::Newline);
        }
        
        force_output();

        nNth += 1;
    } // for each stack frame
} // backtrace

static void rememberValues(Val vals, bool fShow = false)
{
    if (nil == vals)
    {
        when (fShow) format(t, "; No values.~%");
        return;
    }

    // (shiftf *** ** * val)
    TLV(AAA) = TLV(AA);
    TLV(AA)  = TLV(A);
    TLV(A)   = car(vals);

    // (shiftf /// // / vals)
    TLV(SSS) = TLV(SS);
    TLV(SS)  = TLV(S);
    TLV(S)   = vals;

    unless (fShow) return;

    if (nil == cdr(vals))
    {
        Val val = car(vals);
        format(t, "; ~S: ~S~%", type_of(val), val);
    }
    else
    {
        Val nth = zero;
        foreach (List::Enum, oEnum, vals)
        {
            Val val = oEnum.Get();
            format(t, "; [~D] ~S: ~S~%",
                nth,
                type_of(val),
                val );
            nth = add(nth, 1);
        } // for
    } // if
} // rememberValues

// toplevel
void toplevel(Val cond)
{
    BindFrameScope_<2> oLet;
    oLet.Bind(TLV_Acmdl_levelA, add(TLV(Acmdl_levelA), one));
    oLet.Bind(TLV_AconditionA,  cond);

    if (nil != cond)
    {
        format(t, "~%; Error: ~A~%", cond);
        force_output();
        rememberValues(list(cond));
    }

    // Note: Variable eof will contain garbage after GC.
    Val eof = list(Keof);
    for (;;)
    {
        write_string(car(TLV(ApackageA)->StaticCast<Package>()->m_names));
        for (Val k = zero; lt(k, TLV(Acmdl_levelA)); k = add(k, 1))
        {
            write_char('>');
        } // for k
        write_char(' ');
        force_output();

        Val form = read(nil, nil, eof);
        when (form == eof) break;

        if (InternSymbol("BT") == form)
        {
            backtrace();
            continue;
        }

        if (InternSymbol("DES") == form)
        {
            describe(TLV(A));
            continue;
        }

        if (InternSymbol("DIS") == form)
        {
            disassemble(TLV(A));
            continue;
        }

        if (InternSymbol("M1") == form)
        {
            print_object(macroexpand_1(read()), TLV(Astandard_outputA));
            write_char(Private::Newline);
            continue;
        }

        #if _DEBUG
            if (InternSymbol("SX") == form)
            {
                PrintSxHashTable();
                continue;
            }
        #endif

        if (symbolp(form) && ! boundp(form))
        {
            format(t, "; Variable ~S is not bound.~%", form);
            continue;
        }

        TLV(_) = form;   // (setq - form)

        eval(form);

        Val vals = Thread::Get()->ValuesToList();

        TLV(PPP) = TLV(PP);  // (setq +++ ++)
        TLV(PP)  = TLV(P);   // (setq ++ +)
        TLV(P)   = TLV(_);   // (setq + -)

        rememberValues(vals, true);
    } // for
} // toplevel

} // TinyCl
