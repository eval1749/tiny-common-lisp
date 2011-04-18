//////////////////////////////////////////////////////////////////////////////
//
// Editor - Style
// editor/ed_style.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_style.h#3 $
//
#if !defined(INCLUDE_editor_style_h)
#define INCLUDE_editor_style_h

#include "./ed_defs.h"

namespace Editor
{

class Style :
    public EditorObject_<Style, Layout_style>
{
    public: static Val ClassD_() { return CLASSD_style; }

    // ctor
    public: Style();
}; // Style

CASSERT(sizeof(Style) == sizeof(Layout_style));

} // Editor

#endif //!defined(INCLUDE_editor_style_h)
