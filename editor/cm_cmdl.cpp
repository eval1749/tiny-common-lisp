#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Command Loop
// editor/cm_cmdl.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_cmdl.cpp#9 $
//
//
#include "./cm_cmdl.h"

#include "./cm_keymap.h"
#include "./cm_mode.h"

#include "./ed_buffer.h"
#include "./ed_objects.h"
#include "./ed_selection.h"
#include "./ed_text_window.h"

#include "./rtl_defs.h"

namespace Editor
{

/// <summary>
///   Construct command processor
/// </summary>
CommandState::CommandState()
{
    m_arg    = nil;
    m_events = nil;
    m_keymap = nil;
    m_state  = nil;
    m_window = nil;
} // CommandState::CommandState

// [P]

/// <summary>
///   Tells an event to command processor.
/// </summary>
/// <param name="window">A window where event occured.</param>
/// <param name="event">An event to tell.</param>
// FIXME yosi@msn.com 2008-06-19 How do we handle arg[0-2]? Are they for
// mouse event?
//
// FIXME yosi@msn.com 2008-06-19 Why do we implement command dispatcher in
// lisp?
void CommandState::PushEvent(
    Val window,
    Val event,
    Val,    //arg0,
    Val,    //arg1,
    Val )   //arg2
{
    if (window != m_window)
    {
        // FIXME 2007-12-24 yosi@msn.com Warn if m_events isn't nil.
        m_arg    = nil;
        m_window = window;
        m_keymap = nil;
        m_events = nil;
    } // if

    Val selection = nil;

    if (nil == m_keymap)
    {
        if (TextWindow* pWindow = m_window->DynamicCast<TextWindow>())
        {
            Buffer* pBuffer = pWindow->m_selection->StaticCast<Selection>()->
                GetBuffer();

            m_keymap = pBuffer->m_mode->StaticCast<Mode>()->m_keymap;
            selection = pWindow->m_selection;
        } // if

        if (nil == m_keymap)
        {
            m_keymap = VAR(Aglobal_keymapA);
        }
    } // if

    Val entry = m_keymap->StaticCast<Keymap>()->Get(event);

    if (nil == entry && nil == m_events)
    {
        // Use entry in *global-keymap* for the first key stroken.
        entry = VAR(Aglobal_keymapA)->StaticCast<Keymap>()->Get(event);
    }

    if (nil == entry)
    {
        // No binding
        // FIXME yosi@msn.com 2008-06-19 NYI reporting No key binding.
        format(t, "~&No bindings for ~S~%", event);
        m_window = nil;
    }
    else if (Command* pCommand = entry->DynamicCast<Command>())
    {
        push(event, m_events);
        TLV(AeventA)     = event;
        TLV(AprefixA)    = m_arg;
        TLV(AselectionA) = selection;

        m_window = nil;

        Val cond = Si::CallFunction(pCommand->m_function);
        if (nil != cond)
        {
            // FIXME 2008-08-03 yosi@msn.com We should gc-protect entry.
            // Note: "entry" points a garbage when GC is occured.

            // FIXME 2008-08-03 yosi@msn.com We should report error in
            // status bar or message window.
            format(t, "~&Error: ~S: ~S~%", entry, cond);
        }

        // Note: "this" points a garbage when GC is occured.
        CommandState* pState = VAR(Acommand_stateA)->
            StaticCast<CommandState>();

        // FIXME yosi@msn.com 2008-06-19 We should use TLV(AeventsA) instead
        // of C-object member variable.
        pState->m_events = nil;
    }
    else if (keymapp(entry))
    {
        push(event, m_events);
        m_keymap = entry;
    }
} // CommandState::PushKey

/// <summary>
///  Bootstrap event dispatcher. Genesis overwrites this function.
/// </summary>
defun(dispatch_event, (Val cmdstate, Val window, Val event))
{
    cmdstate->StaticCast<CommandState>()->PushEvent(window, event);
    return cmdstate;
} // dispatch_event

} // Editor
