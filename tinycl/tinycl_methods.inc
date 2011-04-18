// -*- Mode: C++; -*-
//
// TinyCl - Pre-Build Methods
// tinycl_methods.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_rtl.h#4 $
//
// Note:
//  More specific method must be placed before.
//
#include "./tinycl_begin_list.h"

#if _WIN32
METHOD_FIX(make_pathname_using_host,    windows_host, 6, 1)
METHOD_FIX(parse_namestring_using_host, windows_host, 4, 2)
METHOD_FIX(print_object,                windows_pathname, 2, 1)
METHOD_FIX(truename,                    windows_pathname, 1, 1)
METHOD_FIX(unparse_pathname,            windows_pathname, 3, 1)
#endif // _WIN32

// [C]
METHOD_REST(close, html_encode_stream, 1, 1, 1)
METHOD_REST(close, platform_stream, 1, 1, 1)
METHOD_REST(close, stream, 1, 1, 1)

// [M]
METHOD_REST(make_instance, funcallable_standard_class, 1, 1, 1)
METHOD_REST(make_instance, standard_class, 1, 1, 1)
METHOD_REST(make_instance, structure_class, 1, 1, 1)
METHOD_REST(make_instance, symbol, 1, 1, 1)
METHOD_FIX(make_pathname_using_host, logical_pathname, 6, 1)

// [N]
METHOD_FIX(next_match_using_source, string_range, 2, 2)

// [P]
#if _WIN32
METHOD_FIX(pathname_equal, windows_pathname, 2, 1)
#endif // _WIN32

METHOD_FIX(print_object, basic_host,            2, 1)
METHOD_FIX(print_object, bignum,                2, 1)
METHOD_FIX(print_object, character,             2, 1)
METHOD_FIX(print_object, charset,               2, 1)
METHOD_FIX(print_object, class,                 2, 1)
METHOD_FIX(print_object, class_description,     2, 1)
METHOD_FIX(print_object, double_float,          2, 1)
METHOD_FIX(print_object, double_float_complex,  2, 1)
METHOD_FIX(print_object, cons,                  2, 1)
METHOD_FIX(print_object, external_format,       2, 1)
METHOD_FIX(print_object, fixnum,                2, 1)
METHOD_FIX(print_object, function,              2, 1)
METHOD_FIX(print_object, hash_table,            2, 1)
METHOD_FIX(print_object, package,               2, 1)
METHOD_FIX(print_object, ratio,                 2, 1)
METHOD_FIX(print_object, rational_complex,      2, 1)
METHOD_FIX(print_object, regex,                 2, 1)
METHOD_FIX(print_object, regex_match,           2, 1)
METHOD_FIX(print_object, setf_cell,             2, 1)
METHOD_FIX(print_object, simple_string,         2, 1)
METHOD_FIX(print_object, simple_vector,         2, 1)
METHOD_FIX(print_object, single_float,          2, 1)
METHOD_FIX(print_object, single_float_complex,  2, 1)
METHOD_FIX(print_object, stack_frame,           2, 1)
METHOD_FIX(print_object, standard_method,       2, 1)
METHOD_FIX(print_object, symbol,                2, 1)
METHOD_FIX(print_object, value_cell,            2, 1)
METHOD_FIX(print_object, t,                     2, 1)

// [R]
METHOD_FIX(range_string, string_range, 1, 1)

// [S]
METHOD_FIX(slot_value_using_class,      funcallable_standard_class, 3, 1)
METHOD_FIX_SETF(slot_value_using_class, funcallable_standard_class, 4)
METHOD_FIX(slot_value_using_class,      standard_class, 3, 1)
METHOD_FIX_SETF(slot_value_using_class, standard_class, 4)
METHOD_FIX(slot_value_using_class,      structure_class, 3, 1)
METHOD_FIX_SETF(slot_value_using_class, structure_class, 4)

// [Stream C]
METHOD_FIX(stream_clear_input,      stream, 1, 1)
METHOD_FIX(stream_clear_output,     stream, 1, 1)

// [Stream F]
METHOD_FIX(stream_finish_output,    stream, 1, 1)

METHOD_FIX(stream_force_output,     platform_stream, 1, 1)
METHOD_FIX(stream_force_output,     stream, 1, 1)

// [Stream L]
METHOD_FIX(stream_line_column,  platform_stream,    1, 1)
METHOD_FIX(stream_line_number,  platform_stream,    1, 1)

METHOD_FIX(stream_line_column,  stream,             1, 1)
METHOD_FIX(stream_line_number,  stream,             1, 1)


// [Stream P]
METHOD_FIX(stream_pathname, file_stream, 1, 1)
METHOD_FIX(stream_pathname, stream, 1, 1)


// [Stream R]
METHOD_FIX(stream_read_char,    platform_stream,      1, 1)
METHOD_FIX(stream_read_char,    string_input_stream,  1, 1)

// [Stream U]
METHOD_FIX(stream_unread_char,  string_input_stream,  2, 1)
METHOD_FIX(stream_unread_char,  platform_stream,      2, 1)

// [Stream W]
METHOD_FIX(stream_write_char,   html_encode_stream,   2, 1)
METHOD_FIX(stream_write_char,   platform_stream,      2, 1)
METHOD_FIX(stream_write_char,   string_output_stream, 2, 1)

METHOD_VAR(stream_write_string, html_encode_stream, 2, 4, 1, (Val, Val, Val=zero, Val=nil))
METHOD_VAR(stream_write_string, platform_stream, 2, 4, 1, (Val, Val, Val=zero, Val=nil))
METHOD_VAR(stream_write_string, output_stream, 2, 4, 1, (Val, Val, Val=zero, Val=nil))

// [T]
METHOD_FIX(truename, t, 1, 1)

#include "./tinycl_end_list.h"
