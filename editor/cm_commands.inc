// -*- Mode: C++; -*-
// Editor - Default Keymap
//  editor/cm_commands.inc
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_commands.cpp#5 $
//

#if !defined(CommandEntry)
    #define CommandEntry(mp_key, mp_name)
#endif

// CommandEntry macro in defined in
//  o ed_init.cpp   -- for populating *global-keymap*
//  o rtl_init.cpp  -- in InstallCommands = making lisp function wrapps
//                     C-function.

// See enum Char and enum Key defined in ed_defs.h

CommandEntry(UnAssigned,                backward_window)
CommandEntry(UnAssigned,                backward_window_extend)
CommandEntry(UnAssigned,                forward_window)
CommandEntry(UnAssigned,                forward_window_extend)

CommandEntry(UnAssigned,                graphic_key)

CommandEntry(Backspace,                 backward_delete_char)
CommandEntry(Delete,                    forward_delete_char)
CommandEntry(Down,                      forward_line)
CommandEntry(End,                       end_key)
CommandEntry(Enter,                     enter_key)
CommandEntry(Home,                      home_key)
CommandEntry(Left,                      backward_char)
CommandEntry(PageDown,                  forward_screen)
CommandEntry(PageUp,                    backward_screen)
CommandEntry(Right,                     forward_char)
CommandEntry(Up,                        backward_line)

CommandEntry(Control_(A),               select_all)
CommandEntry(Control_(B),               list_buffer)
CommandEntry(Control_(C),               copy_to_clipboard)
CommandEntry(Control_(F),               find_or_replace)
CommandEntry(Control_(G),               goto_line)
CommandEntry(Control_(H),               find_or_replace)
CommandEntry(Control_(O),               open_file)
CommandEntry(Control_(S),               save_file)
CommandEntry(Control_(V),               paste_from_clipboard)
CommandEntry(Control_(W),               close_buffer)
CommandEntry(Control_(X),               cut_to_clipboard)
CommandEntry(Control_(Y),               redo)
CommandEntry(Control_(Z),               undo)

CommandEntry(Control_(Backspace),       backward_delete_word)
CommandEntry(Control_(Comma),           reconvert)
CommandEntry(Control_(Delete),          forward_delete_word)
CommandEntry(Control_(Down),            goto_close_paren)
CommandEntry(Control_(End),             end_of_buffer)
CommandEntry(Control_(Enter),           eval_last_form)
CommandEntry(Control_(Home),            start_of_buffer)
CommandEntry(Control_(Left),            backward_word)
CommandEntry(Control_(Right),           forward_word)
CommandEntry(Control_(Tab),             next_window)
CommandEntry(Control_(Up),              goto_open_paren)

CommandEntry(Control_Shift_(0),         close_this_window)
CommandEntry(Control_Shift_(1),         close_other_windows)
CommandEntry(Control_Shift_(2),         split_window_vertically)
CommandEntry(Control_Shift_(3),         new_frame)
CommandEntry(Control_Shift_(4),         new_frame_and_close)
CommandEntry(Control_Shift_(5),         split_window_horizontally)

CommandEntry(Control_Shift_(Delete),    copy_to_clipboard)
CommandEntry(Control_Shift_(Down),      goto_close_paren_extend)
CommandEntry(Control_Shift_(End),       end_of_buffer_extend)
CommandEntry(Control_Shift_(Home),      start_of_buffer_extend)
CommandEntry(Control_Shift_(Left),      backward_word_extend)
CommandEntry(Control_Shift_(Right),     forward_word_extend)
CommandEntry(Control_Shift_(Tab),       previous_window)
CommandEntry(Control_Shift_(Up),        goto_open_paren_extend)

CommandEntry(Shift_(Delete),            cut_to_clipboard)
CommandEntry(Shift_(Down),              forward_line_extend)
CommandEntry(Shift_(End),               end_key_extend)
CommandEntry(Shift_(Left),              backward_char_extend)
CommandEntry(Shift_(Home),              home_key_extend)
CommandEntry(Shift_(Insert),            paste_from_clipboard)
CommandEntry(Shift_(PageDown),          forward_screen_extend)
CommandEntry(Shift_(PageUp),            backward_screen_extend)
CommandEntry(Shift_(Right),             forward_char_extend)
CommandEntry(Shift_(Up),                backward_line_extend)

#undef CommandEntry
