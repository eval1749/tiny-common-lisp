//////////////////////////////////////////////////////////////////////////////
//
// Editor - Lisp Object Declarations
// editor/ed_object.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_layout.h#1 $
//
#if !defined(INCLUDE_editor_layout_h)
#define INCLUDE_editor_layout_h

namespace Editor
{

#define defabstract(mp_cname, mp_meta, mp_layout) \
    struct Layout_ ## mp_cname mp_layout {

#define deflayout(mp_cname, mp_meta, mp_layout) \
    defabstract(mp_cname, mp_meta, mp_layout)

#define direct_slot__(mp_cty, mp_field, mp_cname, mp_ty) \
    mp_cty mp_field;

#define endlayout(mp_cname) };

#include "./ed_layout.inc"

} // Editor

#endif //!defined(INCLUDE_editor_layout_h)
