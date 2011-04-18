#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - Edit Buffer
// listener/winapp/ed_buffer.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_util.cpp#2 $
//
#include "./ed_util.h"

using namespace TinyCl;

namespace Editor
{

bool char_in_set_p(char16 wch, Val set)
{
    if (SimpleString* p = set->StaticCast<SimpleString>())
    {
        return NULL != ::lstrchrW(p->GetStart(), wch);
    } // if simple_string

    if (StringObject* p = set->StaticCast<StringObject>())
    {
        const char16* pwchStart = p->GetStart();
        const char16* pwchEnd   = pwchStart + p->GetLength();
        for (const char16* pwch = pwchStart; pwch < pwchEnd; pwch++)
        {
            if (*pwch == wch) return true;
        } // for pwch
        return false;
    } // if string_object

    SignalTypeError(set, Qstring);
} // char_in_set_p

} // Editor
