#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Hooks
// editor/rtl_hooks.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_hook.cpp#1 $
//
//
#include "./rtl_defs.h"

namespace Editor
{

Val run_hooks(Val hook, Val a)
    { values(a); return run_hooks(hook); }

Val run_hooks(Val hook)
{
    Val args = Thread::Get()->ValuesToList();
    Val hooks = gethash(hook, VAR(AhooksA));
    if (nil == hooks) return nil;
    foreach (List::Enum, oEnum, hooks)
    {
        values_list(args);
        funcall_(oEnum.Get());
    } // for each hook
    return hook;
} // run_hooks

} // Editor
