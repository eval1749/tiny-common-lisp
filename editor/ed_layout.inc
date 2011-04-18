// -*- Mode: C++; -*-
// Edtior - Lisp Layout
// ed_layout.inc
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_defs.h#1 $
//

#include "../tinycl/tinycl_begin_list.h"

defabstract(object, structure, : TinyCl::Layout_structure_object)
    direct_super_(structure_object)
endabstract(object)

deflayout(double_linked_list, structure, : Layout_object)
    direct_super_(object)
    direct_slot_(head, nil, or_double_linked_list_null)
    direct_slot_(tail, nil, or_double_linked_list_null)
    direct_slot_(nil,  nil, nil)
endlayout(double_linked_list)

defabstract(double_linked_item, structure, : Layout_object)
    direct_super_(object)
    direct_slot_(next, nil, or_double_linked_item_null)
    direct_slot_(prev, nil, or_double_linked_item_null)
endabstract(double_linked_list)

deflayout(buffer, structure, : Layout_double_linked_item)
    direct_super_(double_linked_item)

    // Buffer Text
    direct_slot_(blob,      nil, fixnum)
    direct_slot_(gap_end,   nil, sequence_index)
    direct_slot_(gap_start, nil, sequence_index)
    direct_slot_(heap,      nil, integer)
    direct_slot_(length,    nil, sequence_index)
    direct_slot_(size,      nil, sequence_index)

    // Buffer Core
    direct_slot_(tick,  nil, fixnum)

    // Buffer File
    direct_slot_(char_tick,         nil, fixnum)
    direct_slot_(external_format,   nil, external_format)
    direct_slot_(file_write_date,   nil, integer)
    direct_slot_(file_state,        nil, t)
    direct_slot_(name,              nil, simple_string)
    direct_slot_(pathname,          nil, t)
    direct_slot_(read_only,         nil, t)
    direct_slot_(state,             nil, symbol)

    direct_slot_(first_interval, nil, or_interval_null)
    direct_slot_(last_interval, nil, or_interval_null)

    direct_slot_(first_range, nil, or_range_null)
    direct_slot_(last_range, nil, or_range_null)

    direct_slot_(first_tracker, nil, or_tracker_null)
    direct_slot_(last_tracker,  nil, or_tracker_null)

    direct_slot_(interval_root, nil, interval)

    // Undo
    direct_slot_(first_undo, nil, or_undo_record_null)
    direct_slot_(last_undo,  nil, or_undo_record_null)
    direct_slot_(redo,       nil, or_undo_record_null)
    direct_slot_(undo_state, nil, symbol)
    direct_slot_(undo,       nil, or_undo_record_null)

    // Window
    direct_slot_(windows,  nil, list)

    // Command
    direct_slot_(lexer,     nil, lexer)
    direct_slot_(mode,      nil, mode)
    direct_slot_(style_map, nil, or_hash_table_null)

    direct_slot_(nil, nil, nil)
endlayout(buffer)

deflayout(tracker, structure, : Layout_object)
    direct_super_(object)

    // Lexer uses slot "buffer"
    direct_slot_(buffer,    Kbuffer,    or_buffer_null)
    direct_slot_(end,       nil,        sequence_index)
    direct_slot_(next,      nil,        or_tracker_null)
    direct_slot_(prev,      nil,        or_tracker_null)
    direct_slot_(start,     nil,        sequence_index)
endlayout(tracker)

deflayout(range, structure, : TinyCl::Layout_range)
    direct_super_(si_range)

    direct_slot_(buffer,    Kbuffer,    or_buffer_null)
    direct_slot_(next,      nil,        or_range_null)
    direct_slot_(prev,      nil,        or_range_null)
endlayout(range)

deflayout(interval, structure, : Layout_object)
    direct_super_(object)

    // Slot "buffer" is just for print-object
    direct_slot_(buffer,    Kbuffer,    or_buffer_null)
    direct_slot_(end,       Kend,       sequence_index)
    direct_slot_(start,     Kstart,     sequence_index)

    // style
    direct_slot_(background,        nil,    or_color_null)
    direct_slot_(color,             nil,    or_color_null)
    direct_slot_(font_family,       nil,    or_string_list)
    direct_slot_(font_size,         nil,    or_font_size_null)
    direct_slot_(font_style,        nil,    symbol)
    direct_slot_(font_weight,       nil,    symbol)
    direct_slot_(marker,            nil,    or_color_null)
    direct_slot_(state,             nil,    t)
    direct_slot_(text_decoration,   nil,    symbol)

    // Interval list item
    direct_slot_(next,      nil,        or_interval_null)
    direct_slot_(prev,      nil,        or_interval_null)

    // Treap slots
    direct_slot_(priority,  nil,    fixnum)
    direct_slot_(left,      nil,    or_interval_null)
    direct_slot_(right,     nil,    or_interval_null)
endlayout(interval)

deflayout(style, structure, : Layout_object)
    direct_super_(object)

    direct_slot_(color,             nil,    color)
    direct_slot_(background,        nil,    color)
    direct_slot_(marker,            nil,    color)
    direct_slot_(font_family,       nil,    t)
    direct_slot_(font_size,         nil,    fixnum)
    direct_slot_(font_style,        nil,    symbol)
    direct_slot_(font_weight,       nil,    symbol)
    direct_slot_(text_decoration,   nil,    fixnum)

    direct_slot_(nil, nil, nil)
endlayout(style)

// Undo
defabstract(undo_record, structure, : Layout_double_linked_item)
    direct_super_(double_linked_item)
endabstract(undo_record)

#define CLASSD_undo_record_min  CLASSD_undo_begin_record

deflayout(undo_begin_record, structure, : Layout_undo_record)
    direct_super_(undo_record)
    direct_slot_(name, nil, symbol)
endlayout(undo_begin_record)

deflayout(undo_delete_record, structure, : Layout_undo_record)
    direct_super_(undo_record)

    direct_slot_(end,     nil, sequence_index)
    direct_slot_(start,   nil, sequence_index)
    direct_slot_(strings, nil, cons)
endlayout(undo_delete_record)

deflayout(undo_end_record, structure, : Layout_undo_record)
    direct_super_(undo_record)

    direct_slot_(name, nil, symbol)
endlayout(undo_end_record)

deflayout(undo_insert_record, structure, : Layout_undo_record)
    direct_super_(undo_record)

    direct_slot_(end,   nil, sequence_index)
    direct_slot_(start, nil, sequence_index)
    
    direct_slot_(nil, nil, nil)
endlayout(undo_insert_record)

#define CLASSD_undo_record_max CLASSD_undo_insert_record

// Windowing
deflayout(selection, structure, : Layout_range)
    direct_super_(range)

    direct_slot_(active,        Kstart,     member_end_start)
    direct_slot_(goal_x,        minus_one,  fixnum)
    direct_slot_(goal_y,        minus_one,  fixnum)
    direct_slot_(line_number,   nil,        fixnum)
    direct_slot_(window,        nil,        or_window_null)
endlayout(selection)

defabstract(window, structure, : Layout_object)
    direct_super_(object)

    direct_slot_(active_tick,   nil, fixnum)
    direct_slot_(parent,        nil, or_window_null)
    direct_slot_(peer,          nil, fixnum)
    direct_slot_(visible,       nil, boolean)
endabstract(window)

defabstract(container, structure, : Layout_window)
    direct_super_(window)
    direct_slot_(child_windows, nil, list)
endabstract(container)

#define CLASSD_window_min CLASSD_text_window

deflayout(text_window, structure, : Layout_window)
    direct_super_(window)

    direct_slot_(blink,     nil, range)
    direct_slot_(range,     nil, range)
    direct_slot_(selection, nil, selection)
endlayout(text_window)

deflayout(frame, structure, : Layout_container)
    direct_super_(container)
endlayout(frame)

#define CLASSD_window_max CLASSD_frame

deflayout(font, structure, : Layout_object)
    direct_super_(object)

    direct_slot_(family,    nil,    string)
    direct_slot_(peer,      nil,    fixnum)
    direct_slot_(size,      nil,    fixnum)
    direct_slot_(style,     nil,    symbol)
    direct_slot_(weight,    nil,    symbol)
endlayout(font)

// Command
deflayout(keymap, structure, : Layout_object)
    direct_super_(object)

    direct_slot_(bindings, nil, hash_table)
endlayout(keymap)

deflayout(command, structure, : Layout_structure_object)
    direct_super_(object)

    direct_slot_(function,   Kfunction,  function)
    direct_slot_(name,       Kname,      symbol)
    direct_slot_(plist,      Kplist,     list)
endlayout(command)

deflayout(command_state, structure, : Layout_object)
    direct_super_(object)

    direct_slot_(arg, nil, t)
    direct_slot_(events, nil, list)
    direct_slot_(keymap, nil, or_keymap_null)
    direct_slot_(state,  nil, symbol)
    direct_slot_(window, nil, t)
endlayout(command_state)

// Mode
deflayout(lexer, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(tracker,       nil, tracker)
    direct_slot_(keywords,      nil, hash_table)
    direct_slot_(lexeme,        nil, sequence_index)
    direct_slot_(position,      nil, sequence_index)
    direct_slot_(state,         nil, symbol)
endlayout(lexer)

defabstract(mode, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(keymap,        nil,        keymap)
    direct_slot_(keywords,      nil,        or_hash_table_null)
    direct_slot_(string,        Kstring,    string)
    direct_slot_(style_map,     nil,        hash_table)
    direct_slot_(syntax_table,  nil,        syntax_table)
endabstract(mode)

deflayout(syntax_table, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(name,      nil, string)
    direct_slot_(table,     nil, hash_table)
    direct_slot_(vector,    nil, simple_vector)
endlayout(syntax_table)

#define defmode(mp_cname, mp_string) \
    deflayout(mp_cname ## _mode, structure, : Layout_mode) \
        direct_super_(mode) \
    endlayout(mp_cname ## _mode) \
    deflayout(mp_cname ## _lexer, structure, : Layout_lexer) \
        direct_super_(lexer) \
    endlayout(mp_cname ## _lexer) \

defmode(c,              "C")
defmode(cPP,            "C++")
defmode(common_lisp,    "COMMON-LISP")
defmode(cS,             "C#")
defmode(html,           "HTML")
defmode(java,           "JAVA")
defmode(javascript,     "JAVASCRIPT")
defmode(mason,          "MASON")
defmode(perl,           "PERL")
defmode(scheme,         "SCHEME")
defmode(text,           "TEXT")
defmode(xml,            "XML")
defmode(xsd,            "XSD")
defmode(xslt,           "XSLT")

#undef defmode

// Streams
defstorage(buffer_input_stream, standard, : Layout_input_stream)
    direct_super_(input_stream)

    direct_slot_(buffer,    Kbuffer,    buffer)
    direct_slot_(current,   Kstart,     range)
    direct_slot_(limit,     Kend,       or_range_null)
endstorage(buffer_input_stream)

defstorage(buffer_output_stream, standard, : Layout_output_stream)
    direct_super_(output_stream)

    direct_slot_(buffer,    Kbuffer,    buffer)
    direct_slot_(current,   Kstart,     or_range_null)

    direct_slot_(nil, nil, nil)
endstorage(buffer_output_stream)

// Conditions
defabstract(buffer_error, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(buffer, Kbuffer, buffer)
endabstract(buffer_error)

defstorage(buffer_not_ready, standard, : Layout_buffer_error)
    direct_super_(buffer_error)
endstorage(buffer_not_ready)

defstorage(buffer_read_only, standard, : Layout_buffer_error)
    direct_super_(buffer_error)
endstorage(buffer_read_only)

defstorage(invalid_position, standard, : Layout_buffer_error)
    direct_super_(buffer_error)
    direct_super_(type_error)
endstorage(invalid_position)

defstorage(invalid_range, standard, : Layout_buffer_error)
    direct_super_(buffer_error)
    direct_super_(type_error)
endstorage(invalid_range)

defstorage(wrong_buffer, standard, : Layout_buffer_error)
    direct_super_(buffer_error)
    direct_slot_(range, Krange, range)
endstorage(invalid_range)

#include "../tinycl/tinycl_end_list.h"
