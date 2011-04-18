//////////////////////////////////////////////////////////////////////////////
//
// Editor - Command Loop
// editor/cm_cmdl.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_keymap.h#1 $
//
#if !defined(INCLUDE_editor_keymap_h)
#define INCLUDE_editor_keymap_h

#include "./ed_defs.h"

namespace Editor
{

class Keymap : public EditorObject_<Keymap, Layout_keymap>
{
    public: static Val ClassD_() { return CLASSD_keymap; }

    public: Keymap();

    // [G]
    public: Val Get(Val) const;

    // [S]
    public: void Set(Val, Val);
}; // Keymap


} // Editor

#endif //!defined(INCLUDE_editor_keymap_h)
