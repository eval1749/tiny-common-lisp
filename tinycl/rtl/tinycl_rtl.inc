// -*- Mode: C++; -*-
// TinyCl - Built-In Functions
// tinycl_rtl.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl.inc#11 $

#include "../tinycl_begin_list.h"

FUN_BOOL(eq_2, 2)
FUN_BOOL(ge_2, 2)
FUN_BOOL(gt_2, 2)
FUN_BOOL(le_2, 2)
FUN_BOOL(lt_2, 2)
FUN_BOOL(ne_2, 2)
FUN_FIX(add_2, 2, 1)
FUN_FIX(div_2, 2, 1)
FUN_FIX(mul_2, 2, 1)
FUN_FIX(sub_1, 1, 1)
FUN_FIX(sub_2, 2, 1)

// [1]
//FUN_FIX(C1P, 1, 1) // 1+
//FUN_FIX(C1_, 1, 1) // 1-

// [!]
FUN_ENTRY(QBgo, 1, 1, 0, FunRet_NoReturn, Bgo)
FUN_REST(Bmake_closure, 0, 0, 1)
FUN_ENTRY(QBreturn, 0, 0, 1, FunRet_NoReturn, BreturnV)
FUN_ENTRY(QBthrow,  0, 0, 1, FunRet_NoReturn, BthrowV)

// [%]
FUN_FIX(Pdefmacro,  3, 1)
FUN_FIX(Pdefun,     3, 1)

// [A]
FUN_FIX(allocate_binobj, 1, 1)
FUN_FIX(allocate_binvec, 2, 1)
FUN_FIX(allocate_record, 1, 1)
FUN_REST(append, 0, 0, 1)
FUN_FIX(appendS2, 2, 1)
FUN_BOOL(array_has_fill_pointer_p, 1)
FUN_BOOL(arrayp, 1)
FUN_FIX(assq, 2, 1)
FUN_FIX(ash, 2, 1)

// [B]
FUN_BOOL(boundp, 1)

// [C]
FUN_FIX(car, 1,  1)
FUN_FIX(cdr, 1,  1)

FUN_FIX(caar, 1,  1)
FUN_FIX(cadr, 1,  1)
FUN_FIX(cdar, 1,  1)
FUN_FIX(cddr, 1,  1)

FUN_FIX(caaar,            1,  1)
FUN_FIX(caadr,            1,  1)
FUN_FIX(cadar,            1,  1)
FUN_FIX(caddr,            1,  1)
FUN_FIX(cdaar,            1,  1)
FUN_FIX(cdadr,            1,  1)
FUN_FIX(cddar,            1,  1)
FUN_FIX(cdddr,            1,  1)

FUN_FIX(caaaar,            1,  1)
FUN_FIX(caaadr,            1,  1)
FUN_FIX(caadar,            1,  1)
FUN_FIX(caaddr,            1,  1)
FUN_FIX(cadaar,            1,  1)
FUN_FIX(cadadr,            1,  1)
FUN_FIX(caddar,            1,  1)
FUN_FIX(cadddr,            1,  1)
FUN_FIX(cdaaar,            1,  1)
FUN_FIX(cdaadr,            1,  1)
FUN_FIX(cdadar,            1,  1)
FUN_FIX(cdaddr,            1,  1)
FUN_FIX(cddaar,            1,  1)
FUN_FIX(cddadr,            1,  1)
FUN_FIX(cdddar,            1,  1)
FUN_FIX(cddddr,            1,  1)

FUN_BOOL(characterp,            1)
FUN_FIX(char_downcase,          1, 1)
FUN_FIX(char_name,              1, 1)
FUN_FIX(classd_of,              1, 1)
FUN_FIX(clrhash,                1, 1)
FUN_FIX(code_char,              1, 1)
FUN_ENTRY(Qcollect_garbage, 0, 0, 3, 2, collect_garbageV)
FUN_FIX(compile_form,           1, 3)
FUN_REST(compile_regex,         1, 1, 1)
FUN_FIX(compute_discriminator,  1, 1)
FUN_FIX(cons,                   2, 1)
FUN_FIX(constantly,             1, 1)
FUN_BOOL(consp,                 1)
FUN_FIX(copy_seq,               1, 1)
FUN_FIX(coerceSbignumSfloat32,  1, 1)
FUN_FIX(coerceSbignumSfloat64,  1, 1)

// [D]
FUN_VAR(digit_char,   1, 2, 1)
FUN_VAR(digit_char_p, 1, 2, 1)
FUN_FIX(describe,     1, 1)
FUN_FIX(disassemble,  1, 1)

// [E]
FUN_FIX(elt, 2, 1)
FUN_ENTRY(Qerror, 1, 1, 1, FunRet_NoReturn, errorV)
FUN_BOOL(eq, 2)
FUN_BOOL(eql, 2)
FUN_BOOL(equal, 2)
FUN_ENTRY(Qeval,  1, 1, 0, FunRet_Arbitrary, eval)

// [F]
FUN_BOOL(fboundp, 1)
FUN_FIX(fdefinition, 1, 1)
FUN_FIX(file_namestring, 1, 1)
FUN_REST(fill, 2, 2, 1)
FUN_REST(find, 2, 2, 1)
FUN_VAR(find_class, 1, 3, 1)
FUN_FIX(find_package,    1, 1)
FUN_VAR(find_symbol, 1, 2, 2)
FUN_VAR(force_output,  0, 1, 1)
FUN_REST(format,  2, 2, 1)
FUN_REST(funcall, 1, 1, FunRet_Arbitrary)
FUN_FIX(funcallable_instance_function, 1, 1)
FUN_VAR(function_information, 1, 2, 3)

// [I]
FUN_VAR(intern, 1, 2, 2)
FUN_FIX(interactive_stream_p, 1, 1)

// [G]
FUN_VAR(gensym, 0, 1, 1)
FUN_FIX(get_decoded_time, 0, 9)
FUN_VAR(getf, 2, 3, 1)
FUN_VAR(gethash, 2, 3, 2)
FUN_FIX(get_output_stream_string, 1, 1)
FUN_VAR(get_setf_expansion, 1, 2, 5)

// [L]
//FUN_FIX(length, 1, 1)
FUN_ENTRY(Qlength, 1, 1, 0, 1, cl_length)
FUN_REST(list,  0, 0, 1)
FUN_REST(listA, 1, 1, 1)
FUN_FIX(logandS2, 2, 1)
FUN_FIX(logeqvS2, 2, 1)
FUN_FIX(logiorS2, 2, 1)
FUN_FIX(logxorS2, 2, 1)
FUN_FIX(long_site_name, 0, 1)

// [M]
FUN_FIX(machine_instance,                   0, 1)
FUN_VAR(macro_function,                     1, 2, 1)
FUN_VAR(macroexpand_1,                      1, 2, 2)
FUN_FIX(make_closed_cell,                   1, 1)
FUN_REST(make_hash_table,                   1, 1, 1)
FUN_REST(make_list,                         1, 1, 1)
FUN_FIX(make_not_function_function,         1, 1)
FUN_REST(make_pathname,                     0, 0, 1)
FUN_REST(make_sequence,                     2, 2, 1)
FUN_FIX(make_stack_trace,                   0, 1)
FUN_VAR(make_string_input_stream,           1, 3, 1)
FUN_REST(make_string_output_stream,         0, 0, 1)
FUN_FIX(make_undefined_function_function,   1, 1)
FUN_FIX(make_symbol,                        1, 1)
FUN_FIX(make_vector,                        1, 1)
FUN_FIX(memq,                               2, 1)
FUN_VAR(merge_pathnames,                    1, 3, 1)

// [N]
FUN_FIX(name_char, 1, 1)
FUN_FIX(namestring, 1, 1)
FUN_FIX(nreverse, 1, 1)

// [O]
FUN_REST(open, 1, 1, 1)

// [P]
FUN_REST(parse_namestring, 1, 3, 2)
FUN_FIX(pathname, 1, 1)
FUN_BOOL(pathnamep, 1)
FUN_REST(pathname_device, 1, 1, 1)
FUN_REST(pathname_directory, 1, 1, 1)
FUN_REST(pathname_host, 1, 1, 1)
FUN_REST(pathname_name, 1, 1, 1)
FUN_REST(pathname_type, 1, 1, 1)
FUN_FIX(pathname_version, 1, 1)
FUN_REST(position, 2, 2, 1)
FUN_VAR(print, 1, 2, 1)
FUN_FIX(print_object, 2, 1)

// [R]
FUN_VAR(read, 0, 4, 1)
FUN_VAR(read_char, 0, 3, 1)
FUN_FIX(rem, 2, 1)
FUN_FIX(remhash, 2, 1)
FUN_FIX(reverse, 1, 1)
FUN_VAR(room, 0, 1, 1)

// [S]
FUN_FIX(save_image, 1, 1)
FUN_FIX(set_funcallable_instance_function, 2, 1)
FUN_FIX(short_site_name, 0, 1)
FUN_REST(signal, 1, 1, 1)
FUN_FIX(slot_value, 2, 1)
FUN_BOOL(streamp, 1)
FUN_FIX(string,  1,  1)
FUN_BOOL(string_eq, 2)
FUN_VAR(string_match, 2, 4, 1)
FUN_VAR(subseq, 2, 3, 1)
FUN_FIX(svref,           2, 1)
FUN_FIX(sxhash,          1, 1)
FUN_FIX(symbol_function, 1, 1)
FUN_FIX(symbol_value,    1, 1)
FUN_BOOL(symbolp, 1)

// [T]
FUN_VAR(truncate, 1, 2, 2)
FUN_ENTRY(Qtypep, 2, 3, 0, FunRet_Bool, typep_)

// [U]
FUN_FIX(update_callers, 2, 1)

// [V]
FUN_VAR(variable_information, 1, 2, 3)

// [W]
FUN_VAR(write_char, 1, 2, 1)
FUN_REST(write_string, 1, 2, 1)

// setf

// [C]
FUN_FIX_SETF(car, 2)
FUN_FIX_SETF(cdr, 2)

FUN_FIX_SETF(caar, 2)
FUN_FIX_SETF(cadr, 2)
FUN_FIX_SETF(cdar, 2)
FUN_FIX_SETF(cddr, 2)

// [E]
FUN_FIX_SETF(elt, 3)

// [F]
FUN_FIX_SETF(fill_pointer, 2)

// [G]
FUN_VAR_SETF(gethash, 3, 4)

// [M]
FUN_VAR_SETF(macro_function, 2, 3)

// [S]
FUN_FIX_SETF(slot_value, 3)
FUN_FIX_SETF(svref, 3)
FUN_FIX_SETF(symbol_value, 2)

#include "../tinycl_end_list.h"
