//////////////////////////////////////////////////////////////////////////////
//
// Editor - Mode
// editor/ed_mode.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_mode.h#3 $
//
#if !defined(INCLUDE_editor_mode_h)
#define INCLUDE_editor_mode_h

#include "./ed_defs.h"

namespace Editor
{
class Mode : public Si::Record_<Mode, Layout_mode>
{
    public: static bool Is_(const Datum* const x)
        { return subclassp(class_of(const_cast<Val>(x)), CLASS_mode); }
}; // Mode


} // Editor

#endif //!defined(INCLUDE_editor_mode_h)
