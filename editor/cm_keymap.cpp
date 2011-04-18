#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Keymap;
// editor/cm_keymap.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_keymap.cpp#2 $
//
//
#include "./cm_keymap.h"

namespace Editor
{

Keymap::Keymap()
{
    m_bindings = make_hash_table(
        Ksize, Fixnum::Encode(3001),
        Ktest, Qeq );
} // Keymap::Keymap

// [G]
/// <summary>
///   Retreive corresponding entry in keymap for specified event.
/// </summary>
/// <returns>A command, keymap or nil</returns>
Val Keymap::Get(Val event) const
{
    if (! characterp(event) && ! fixnump(event))
    {
        event = type_of(event);
    }
    return gethash(event, m_bindings);
} // Keymap::Get

// [S]
void Keymap::Set(Val event, Val entry)
{
    setf_gethash(entry, event, m_bindings);
} // Keymap::Set

} // Editor
