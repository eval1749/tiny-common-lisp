// -*- Mode: C++; -*-
//
// Editor - Runtime - Function Definitions
// editor/ed_rtl.inc
//
// This file is part of Evita.
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_buffer.cpp#5 $
//
#include "../tinycl/tinycl_begin_list.h"

// [A]
FUN_FIX(activate_window, 1, 1)
FUN_FIX(add_window, 2, 1)
METHOD_FIX(analyze_buffer, cPP_lexer, 2, 1)
METHOD_FIX(analyze_buffer, text_lexer, 2, 1)

// [B]
FUN_FIX(buffer_match, 2, 1)

// [C]
FUN_FIX(close_window, 1, 1)

// [D]
FUN_FIX(dispatch_event, 3, 1)

// [M]
METHOD_FIX(make_lexer, cPP_mode, 2, 1)
METHOD_FIX(make_lexer, text_mode, 2, 1)
FUN_VAR(make_range, 1, 3, 1)
FUN_FIX(make_keymap, 0, 1)

// [N]
METHOD_FIX(next_match_using_source, range, 2, 2)

// [P]
METHOD_FIX(print_object, buffer,                2, 1)
METHOD_FIX(print_object, command,               2, 1)
METHOD_FIX(print_object, interval,              2, 1)
METHOD_FIX(print_object, range,                 2, 1)
METHOD_FIX(print_object, undo_begin_record,     2, 1)
METHOD_FIX(print_object, undo_delete_record,    2, 1)
METHOD_FIX(print_object, undo_end_record,       2, 1)
METHOD_FIX(print_object, undo_insert_record,    2, 1)

// [R]
METHOD_FIX(range_string, range, 1, 1)

METHOD_REST(realize_instance, buffer,       1, 1, 1)
METHOD_REST(realize_instance, frame,        1, 1, 1)
METHOD_REST(realize_instance, range,        1, 1, 1)
METHOD_REST(realize_instance, text_window,  1, 1, 1)

// [S]
FUN_FIX(start_load_file, 3, 1)
FUN_FIX(show_window, 1, 1)
METHOD_FIX(stream_read_char,    buffer_input_stream,  1, 1)
METHOD_FIX(stream_unread_char,  buffer_input_stream,  2, 1)
METHOD_FIX(stream_write_char,   buffer_output_stream, 2, 1)

// [U]
METHOD_FIX(unrealize_instance, frame, 1, 1)
METHOD_FIX(unrealize_instance, range, 1, 1)
METHOD_FIX(unrealize_instance, text_window, 1, 1)

#include "../tinycl/tinycl_end_list.h"
