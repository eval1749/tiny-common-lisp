//////////////////////////////////////////////////////////////////////////////
//
// Editor - Command Loop
// editor/cm_cmdl.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_cmdl.h#1 $
//
#if !defined(INCLUDE_editor_command_state_h)
#define INCLUDE_editor_command_state_h

#include "./ed_defs.h"

namespace Editor
{

class CommandState : public EditorObject_<CommandState, Layout_command_state>
{
    public: static Val ClassD_() { return CLASSD_command_state; }

    public: CommandState();
    public: void PushEvent(Val, Val, Val = nil, Val = nil, Val = nil);
}; // CommandState

} // Editor

#define defcommand(mp_name) \
    extern "C" Val __fastcall Command_ ## mp_name ()

#endif //!defined(INCLUDE_editor_command_state_h)
