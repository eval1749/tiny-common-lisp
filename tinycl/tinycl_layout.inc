// -*- Mode: C++; -*-
// TinyCl - Object Layout
// tinycl_layout.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_layout.inc#47 $
//
#include "./tinycl_begin_list.h"

defabstract(t, built_in,)
endabstract(t)

defabstract(record_object, built_in,)
    direct_super_(t)
    direct_slot_(classd, nil, classd)
endabstract(record_object)

defabstract(structure_object, structure, : Layout_record_object)
    direct_super_(record_object)
endabstract(structure_object)

// extra_length
//  Add NUL at end of simple-string.
deflayout(class_description, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(class,                     nil, class)         // [1]
    direct_slot_(hash_code,                 nil, fixnum)        // [2]
    direct_slot_(format,                    nil, fixnum)        // [3]
    direct_slot_(element_size,              nil, fixnum)        // [4]
    direct_slot_(fixed_size,                nil, fixnum)        // [5]
    direct_slot_(type,                      nil, type_specifier)// [6]
    direct_slot_(element_type,              nil, type_specifier)// [7]
    direct_slot_(class_precedence_list,     nil, list)          // [8]
    direct_slot_(slots,                     nil, list)          // [9]
    direct_slot_(extra_length,              nil, fixnum)        // [10]

    // Note: We must make size of class is multiple sizeof(Val) * 2 for
    // static allocation.
    direct_slot_(nil,                       nil, fixnum)        // [11]
endlayout(class_description)

defabstract(instance, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(storage, nil, storage)
endabstract(instance)

deflayout(storage, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(storaged,  nil, classd)
endlayout(storage)

// 3 Evaluation and Compilation
deflayout(environment, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(latch,     nil, or_latch_null)
    direct_slot_(variables, nil, hash_table)
    direct_slot_(functions, nil, hash_table)
    direct_slot_(outer,      nil, or_environment_null)
    direct_slot_(types,      nil, or_hash_table_null)
    direct_slot_(classes,    nil, or_hash_table_null)
    direct_slot_(others,     nil, or_hash_table_null)
endlayout(environment)

// 4 Types and Classes
defabstract(standard_object, standard, : Layout_storage)
    direct_super_(t)
endabstract(standard_object)

defabstract(compiled_function, built_in,)
    direct_super_(function)
endabstract(compiled_function)

defabstract(metaobject, standard, : Layout_standard_object)
    direct_super_(standard_object)
endabstract(metaobject)

defabstract(specializer, standard, : Layout_metaobject)
    direct_super_(metaobject)

    direct_slot_(plist,             nil, list)      // [2]
    direct_slot_(flags,             nil, fixnum)    // [3]
    direct_slot_(direct_methods,    nil, list)      // [4]
endabstract(specializer)

defabstract(class, standard, : Layout_specializer)
    direct_super_(specializer)

    direct_slot_(name,                  nil, symbol)            // [5]
    direct_slot_(direct_superclasses,   nil, list)              // [6]
    direct_slot_(direct_subclasses,     nil, list)              // [7]
    direct_slot_(class_precedence_list, nil, list)              // [8]
    direct_slot_(instanced,             nil, or_classd_null)    // [9]
    direct_slot_(direct_slots,          nil, list)              // [10]
    direct_slot_(slots,                 nil, list)              // [11]
    direct_slot_(prototype,             nil, t)                 // [12]

    // Note: We must make size of class is multiple sizeof(Val) * 2 for
    // static allocation.
    direct_slot_(nil,                   nil, t)                 // [13]
endabstract(class)

defstorage(built_in_class, standard, : Layout_class)
    direct_super_(class)
endstorage(built_in_class)

defabstract(function, built_in,)
    direct_super_(t)
endabstract(function)

deflayout(function_information, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(documentation, nil, or_string_null)
    direct_slot_(max_params,    nil, fixnum)
    direct_slot_(min_params,    nil, fixnum)
    direct_slot_(keys,          nil, list)
    direct_slot_(rest,          nil, t)
endlayout(function_information)

defabstract(method, standard, : Layout_metaobject)
    direct_super_(metaobject)
endabstract(method)

// eql_caches   alist of function and hash-tables
deflayout(method_cache, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(count,             nil, sequence_index)
    direct_slot_(default_emf,       nil, or_function_null)
    direct_slot_(eql_caches,        nil, list)
    direct_slot_(line_size,         nil, sequence_index)
    direct_slot_(order,             nil, simple_vector)
    direct_slot_(vector,            nil, simple_vector)

    //direct_slot_(nil, nil, nil)
endlayout(method_cache)

defstorage(method_combination, standard, : Layout_metaobject)
    direct_super_(metaobject)
endstorage(method_combination)

defstorage(standard_method_combination, standard, : Layout_method_combination)
    direct_super_(method_combination)
    direct_slot_(options, Koptions, list)
    direct_slot_(type, Ktype, symbol)
endstorage(standard_method_combination)

defstorage(long_method_combination, standard, : Layout_standard_method_combination)
    direct_super_(standard_method_combination)
endstorage(long_method_combination)

defstorage(short_method_combination, standard, : Layout_standard_method_combination)
    direct_super_(standard_method_combination)
    direct_slot_(operator, Koperator, symbol)
    direct_slot_(identity_with_one_argument, Kidentity_with_one_argument, boolean)
endstorage(short_method_combination)

defstorage(standard_class, standard, : Layout_class)
    direct_super_(standard_base_class)
endstorage(standard_class)

defstorage(standard_method, standard, : Layout_standard_object)
    direct_super_(method)
    direct_slot_(function,          Kfunction,          function)
    direct_slot_(generic_function,  Kgeneric_function,
        or_generic_function_null )
    direct_slot_(lambda_list,       Klambda_list,       list)
    direct_slot_(qualifiers,        Kqualifiers,        list)
    direct_slot_(specializers,      Kspecializers,      list)
endstorage(standard_method)

defstorage(structure_class, standard, : Layout_class)
    direct_super_(class)
endstorage(structure_class)

defstorage(simple_type_error, standard, : Layout_standard_object)
    direct_super_(simple_condition)
    direct_super_(type_error)
endstorage(simple_type_error)

defstorage(type_error, standard, : Layout_standard_object)
    direct_super_(error)

    direct_slot_(datum,         Kdatum, t)
    direct_slot_(expected_type, Kexpected_type, type_specifier)
endstorage(type_error)

// 5 Data and Control Flow

defabstract(control_error, standard, : Layout_standard_object)
    direct_super_(error)
endabstract(control_error)

deflayout(marker, built_in, : Layout_structure_object)
    direct_super_(t)
    direct_slot_(value, nil, t)
endlayout(marker)

defabstract(native_code_object, built_in,)
    direct_super_(compiled_function)

    direct_slot_(length,    nil, fixnum)
    direct_slot_(classd,    nil, classd)
    direct_slot_(cookie,    nil, fixnum)
endabstract(native_code_object)

deflayout(native_code_function, built_in, : Layout_native_code_object)
    direct_super_(native_code_object)
    direct_slot_(name, nil, function_name)
endlayout(native_code_function)

deflayout(native_code_closure, built_in, : Layout_native_code_function)
    direct_super_(native_code_function)
endlayout(native_code_closure)

defabstract(program_error, standard, : Layout_standard_object)
    direct_super_(error)
endabstract(program_error)

defabstract(cell_error, standard, : Layout_standard_object)
    direct_super_(error)
    direct_slot_(name, Kname, t)
endabstract(cell_error)

defstorage(syntax_error, standard, : Layout_program_error)
    direct_super_(program_error)
    direct_slot_(datum,     Kdatum,     t)
    direct_slot_(form,      Kform,      t)
    direct_slot_(pattern,   Kpattern,   t)
    direct_slot_(syntax,    Ksyntax,    cons)
endstorage(syntax_error)

defstorage(not_function, standard, : Layout_program_error)
    direct_super_(program_error)
    direct_slot_(name, Kname, function_name)
endstorage(not_function)

// function class for special-operators and macros
deflayout(not_function_function, built_in, : Layout_native_code_closure)
    direct_super_(native_code_closure)
endlayout(not_function_function)

// 5.3.69 undefined-function
defstorage(undefined_function, standard, : Layout_cell_error)
    direct_super_(cell_error)
endstorage(undefined_function)

deflayout(undefined_function_function, built_in, : Layout_native_code_object)
    direct_super_(native_code_function)
endlayout(undefined_function_function)

// 7 Objects
defabstract(funcallable_instance, built_in, : Layout_native_code_object)
    direct_super_(native_code_object)
    direct_slot_(storage, nil, storage)
endabstract(funcallable_instance)

defabstract(funcallable_standard_object, funcallable_standard, : Layout_standard_object)
    direct_super_(standard_object)
    direct_super_(funcallable_instance)
endabstract(funcallable_standard_object)

defabstract(generic_function, funcallable_standard, : Layout_standard_object)
    direct_super_(funcallable_standard_object)

    direct_slot_(method_cache,          nil,                    or_method_cache_null)
    direct_slot_(method_class,          Kmethod_class,          class)
    direct_slot_(method_combination,    Kmethod_combination,    class)
    direct_slot_(methods,               nil,                    list)
    direct_slot_(name,                  Kname,                  function_name)
    direct_slot_(plist,                 nil,                    list)
    direct_slot_(function_information,  nil,                    or_function_information_null)

    //direct_slot_(nil, nil, nil)
endabstract(generic_function)

defstorage(standard_generic_function, funcallable_standard, : Layout_generic_function)
    direct_super_(generic_function)
endstorage(standard_generic_function)

defabstract(slot_definition, standard, : Layout_metaobject)
    direct_super_(metaobject)

    direct_slot_(allocation,     Kallocation,    member_Kinstance_Kclass)
    direct_slot_(initargs,       Kinitargs,      list)
    direct_slot_(initform,       Kinitform,      t)
    direct_slot_(initfunction,   Kinitfunction,  or_function_null)
    direct_slot_(name,           Kname,          symbol)
    direct_slot_(type,           Ktype,          type_specifier)
endabstract(slot_definition)

defabstract(direct_slot_definition, standard, : Layout_slot_definition)
    direct_super_(slot_definition)

    direct_slot_(readers,    Kreaders, list)
    direct_slot_(writers,    Kwriters, list)
endabstract(direct_slot_definition)

defabstract(effective_slot_definition, standard, : Layout_slot_definition)
    direct_super_(slot_definition)

    // FIXME 2007-12-04 yosi@msn.com Do we use class-plist for abstract
    // of class slot?
    direct_slot_(location,    Klocation, or_sequence_index_cons)
endabstract(effective_slot_definition)

defabstract(standard_slot_definition, standard, : Layout_slot_definition)
    direct_super_(slot_definition)
endabstract(standard_slot_definition)

defstorage(standard_direct_slot_definition, standard, : Layout_direct_slot_definition)
    direct_super_(standard_slot_definition)
    direct_super_(direct_slot_definition)
endstorage(standard_direct_slot_definition)

defstorage(standard_effective_slot_definition, standard, : Layout_effective_slot_definition)
    direct_super_(standard_slot_definition)
    direct_super_(effective_slot_definition)
endstorage(standard_effective_slot_definition)

defstorage(funcallable_standard_class, standard, : Layout_class)
    direct_super_(standard_base_class)
endstorage(funcallable_standard_class)

defabstract(standard_base_class, standard, : Layout_standard_object)
    direct_super_(class)
endabstract(standard_base_class)

defstorage(class_not_finalized, standard, : Layout_standard_object)
    direct_super_(error)
    direct_slot_(class, Kclass, class)
endstorage(class_not_finalized)

defstorage(unbound_slot, standard, : Layout_standard_object)
    direct_super_(cell_error)
endstorage(unbound_slot)

// 8 Structures
defabstract(structure_slot_definition, standard, : Layout_slot_definition)
    direct_super_(slot_definition)
endabstract(0)

defstorage(structure_direct_slot_definition, standard, : Layout_direct_slot_definition)
    direct_super_(structure_slot_definition)
    direct_super_(direct_slot_definition)
    direct_slot_(read_only, nil, t)
endstorage(0)

defstorage(structure_effective_slot_definition, standard, : Layout_effective_slot_definition)
    direct_super_(structure_slot_definition)
    direct_super_(effective_slot_definition)
endstorage(0)

// 9 Conditions
defabstract(condition, standard, : Layout_standard_object)
    direct_super_(standard_object)
endabstract(condition)

defabstract(error, standard, : Layout_standard_object)
    direct_super_(serious_condition)
endabstract(error)

defstorage(invalid_object, standard, : Layout_standard_object)
    direct_super_(error)
    direct_slot_(object, Kobject, t)
endstorage(invalid_object)

defabstract(parse_error, standard, : Layout_standard_object)
    direct_super_(error)
endabstract(parse_error)

defabstract(serious_condition, standard, : Layout_standard_object)
    direct_super_(condition)
endabstract(serious_condition)

defstorage(simple_condition, standard, : Layout_standard_object)
    direct_super_(condition)
    direct_slot_(format_arguments, Kformat_arguments,  list)
    direct_slot_(format_control,   Kformat_control,    format_control)
endstorage(simple_condition)

defstorage(storage_condition, standard, : Layout_standard_object)
    direct_super_(serious_condition)
endstorage(storage_condition)

defstorage(simple_error, standard, : Layout_simple_condition)
    direct_super_(simple_condition)
    direct_super_(error)
endstorage(simple_error)

defabstract(style_warning, standard, : Layout_standard_object)
    direct_super_(condition)
endabstract(style_warning)

defabstract(warning, standard, : Layout_standard_object)
    direct_super_(condition)
endabstract(warning)

defstorage(simple_warning, standard, : Layout_simple_condition)
    direct_super_(simple_condition)
    direct_super_(warning)
endstorage(simple_warning)

defstorage(
    invalid_keyword_argument,
    standard,
    : Layout_type_error )
    direct_super_(type_error)
endstorage(invalid_keyword_argument)

defstorage(
    odd_number_of_keyword_arguments,
    standard,
    : Layout_standard_object )
    direct_super_(program_error)
    direct_slot_(arguments,   Karguments,  list)
endstorage(odd_number_of_keyword_arguments)

defstorage(too_few_arguments, standard, : Layout_standard_object)
    direct_super_(program_error)
    direct_slot_(arguments, Karguments, list)
    direct_slot_(function,  Kfunction,  function)
endstorage(too_few_arguments)

defstorage(too_many_arguments, standard, : Layout_standard_object)
    direct_super_(program_error)
    direct_slot_(arguments, Karguments, list)
    direct_slot_(function,  Kfunction,  function)
endstorage(too_many_arguments)

defstorage(too_many_values, standard, : Layout_standard_object)
    direct_super_(program_error)
endstorage(too_many_values)

defstorage(
    unrecognized_keyword_argument,
    standard,
    : Layout_standard_object )
    direct_super_(program_error)
    direct_slot_(key,   Kkey,  symbol)
    direct_slot_(keys,  Kkeys, simple_vector)
endstorage(unrecognized_keyword_argument)

deflayout(restart, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(function,
        Kfunction, function_designator )

    direct_slot_(interactive_function,
        Kinteractive_function, function_designator )

    direct_slot_(name,
        Kname, symbol )

    direct_slot_(report_function,
        Kreport_function, function_designator )

    direct_slot_(test_function,
        Ktest_function, function_designator )
endlayout(restart)

// 10 Symbols
deflayout(symbol, built_in, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(hash_code,     nil, fixnum)
    direct_slot_(name,          nil, simple_string)
    direct_slot_(package,       nil, or_package_null)
    direct_slot_(function,      nil, or_function_null)
    direct_slot_(plist,         nil, list)
endlayout(symbol)

// 10.2.20 unbound-variable
defstorage(unbound_variable, standard, : Layout_cell_error)
    direct_super_(cell_error)
endstorage(unbound_variable)

// 11 Packages
deflayout(package, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(external_table,        nil, simple_vector)
    direct_slot_(internal_table,        nil, simple_vector)
    direct_slot_(names,                 nil, list)
    direct_slot_(use_list,              nil, list)
    direct_slot_(used_by_list,          nil, list)
    direct_slot_(shadowing_symbols,     nil, list)
    direct_slot_(protect,               nil, symbol)
endlayout(package)

defabstract(package_error, standard, : Layout_standard_object)
    direct_super_(error)
    direct_slot_(package, Kpackage, package)
endabstract(package_error)

// 12 Numbers
defstorage(arithmetic_error, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(operation, Koperation, symbol)
    direct_slot_(operands,  Koperands,  list)
endstorage(arithmetic_error)

deflayout(byte_specifier, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(position,  Kposition,  unsigned_byte)
    direct_slot_(size,      Ksize,      unsigned_byte)
endlayout(byte_specifier)

defstorage(division_by_zero, standard, : Layout_error)
    direct_super_(error)
endstorage(division_by_zero)

defstorage(floating_point_inexact, standard, : Layout_standard_object)
    direct_super_(error)
endstorage(floating_point_inexact)

defstorage(floating_point_invalid_operation, standard, : Layout_standard_object)
    direct_super_(error)
endstorage(floating_point_invalid_operation)

defstorage(floating_point_overflow, standard, : Layout_standard_object)
    direct_super_(error)
endstorage(floating_point_overflow)

defstorage(floating_point_underflow, standard, : Layout_standard_object)
    direct_super_(error)
endstorage(floating_point_underflow)

deflayout(bignum, built_in,)
    direct_super_(integer)
    direct_slot_(classd, nil, classd)
    direct_slot_(length, nil, sequence_index)
endlayout(bignum)

defabstract(complex, built_in,)
    direct_super_(number)
endabstract(complex)

defabstract(float, built_in,)
    direct_super_(real)
endabstract(float)

defabstract(integer, built_in,)
    direct_super_(rational)
endabstract(integer)

defabstract(number, built_in,)
    direct_super_(t)
endabstract(number)

defabstract(rational, built_in,)
    direct_super_(real)
endabstract(rational)

defabstract(real, built_in,)
    direct_super_(number)
endabstract(real)

deflayout(ratio, built_in,)
    direct_super_(rational)
    direct_slot_(classd, nil, classd)
    direct_slot_(denominator,   nil, integer)
    direct_slot_(numerator,     nil, integer)
endlayout(ratio)

#define CLASSD_rational_max CLASSD_ratio

#if SIZEOF_VAL == 4
deflayout(double_float, built_in,)
    direct_super_(float)
    direct_slot_(classd, nil, classd)
    direct_slot_(pad, nil, fixnum)
    direct_slot__(float64, m_dbl, nil, float64)
endlayout(double_float)
#elif SIZEOF_VAL == 8
deflayout(double_float, built_in,)
    direct_super_(float)
    direct_slot_(classd, nil, classd)
    direct_slot__(float64, m_dbl, nil, float64)
endlayout(double_float)
#endif

deflayout(single_float, built_in,)
    direct_super_(float)
    direct_slot_(classd, nil, classd)
    direct_slot__(float, m_flt, nil, float32)
endlayout(single_float)

#define CLASSD_real_max CLASSD_single_float

#define CLASSD_complex_min CLASSD_rational_complex

deflayout(rational_complex, built_in,)
    direct_super_(rational)
    direct_slot_(classd, nil, classd)
    direct_slot_(real,  nil, rational)
    direct_slot_(imag,  nil, rational)
endlayout(ratio)

#if SIZEOF_VAL == 4
deflayout(double_float_complex, built_in,)
    direct_super_(complex)
    direct_slot_(classd, nil, classd)
    direct_slot_(pad, nil, fixnum)
    direct_slot__(float64, m_dblReal, nil, float64)
    direct_slot__(float64, m_dblImag, nil, float64)
endlayout(double_float_complex)
#elif SIZEOF_VAL == 8
deflayout(double_float_complex, built_in,)
    direct_super_(complex)
    direct_slot_(classd, nil, classd)
    direct_slot___(float64, m_dblReal, nil, float64)
    direct_slot___(float64, m_dblImag, nil, float64)
endlayout(double_float_complex)
#endif

deflayout(single_float_complex, built_in,)
    direct_super_(complex)
    direct_slot_(classd, nil, classd)
    direct_slot_(pad, nil, fixnum)
    direct_slot__(float32, m_fltReal, nil, float32)
    direct_slot__(float32, m_fltImag, nil, float32)
endlayout(single_float_complex)

#define CLASSD_complex_max CLASSD_single_float_complex

deflayout(fixnum, built_in,)
    direct_super_(integer)
endlayout(fixnum)

deflayout(random_state, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(value, nil, fixnum)
endlayout(random_state)

// 13 Characters
deflayout(character, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(data, nil, fixnum)
endlayout(character)

// 14 Conses
deflayout(cons, built_in,)
    direct_super_(list)
    direct_slot_(car, nil, t)
    direct_slot_(cdr, nil, t)
endlayout(cons)

defabstract(list, built_in,)
    direct_super_(sequence)
endabstract(list)

deflayout(null, built_in,)
    direct_super_(symbol)
    direct_super_(list)
endlayout(null)

// 15 Arrays
// 16 Strings
#define CLASSD_array_min    CLASSD_array_object

defabstract(array, built_in, : Layout_structure_object)
    direct_super_(t)
endabstract(array)

deflayout(array_object, built_in, : Layout_structure_object)
    direct_super_(array)
    direct_slot_(rank,          nil, array_rank)
    direct_slot_(flags,         nil, fixnum)
    direct_slot_(displaced_to,  nil, array)
    direct_slot_(offset,        nil, sequence_index)
    direct_slot_(total_size,    nil, sequence_index)
endlayout(array_object)

defabstract(data_vector, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_super_(vector)
    direct_slot_(length, nil, sequence_index)
endabstract(data_vector)

#define CLASSD_data_vector_min  CLASSD_simple_vector
#define CLASSD_vector_min       CLASSD_data_vector_min

deflayout(simple_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(simple_vector)

deflayout(double_float_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(double_float_vector)

deflayout(double_float_complex_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(double_float_complex_vector)

deflayout(single_float_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(single_float_vector)

deflayout(single_float_complex_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(single_float_complex_vector)

deflayout(signed_byte_8_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(signed_byte_8_vector)

deflayout(signed_byte_16_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(signed_byte_16_vector)

deflayout(signed_byte_32_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(signed_byte_32_vector)

deflayout(unsigned_byte_8_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(unsigned_byte_8_vector)

deflayout(unsigned_byte_16_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(unsigned_byte_16_vector)

deflayout(unsigned_byte_32_vector, built_in, : Layout_data_vector)
    direct_super_(data_vector)
endlayout(unsigned_byte_32_vector)

deflayout(simple_bit_vector, built_in, : Layout_data_vector)
    direct_super_(bit_vector)
    direct_super_(data_vector)
endlayout(simple_bit_vector)

deflayout(simple_string, built_in, : Layout_data_vector)
    direct_super_(string)
    direct_super_(data_vector)
endlayout(simple_string)

#define CLASSD_data_vector_max  CLASSD_simple_string

defabstract(bit_vector, built_in,)
    direct_super_(vector)
endabstract(bit_vector)

defabstract(string, built_in,)
    direct_super_(vector)
endabstract(string)

defabstract(vector, built_in,)
    direct_super_(sequence)
    direct_super_(array)
endabstract(vector)

deflayout(vector_object, built_in, : Layout_structure_object)
    direct_super_(vector)
    direct_slot_(fill_pointer,  nil, sequence_index)
    direct_slot_(flags,         nil, fixnum)
    direct_slot_(displaced_to,  nil, array)
    direct_slot_(offset,        nil, sequence_index)
    direct_slot_(total_size,    nil, sequence_index)
endlayout(vector_object)

deflayout(bit_vector_object, built_in, : Layout_vector_object)
    direct_super_(vector_object)
endlayout(bit_vector_object)

deflayout(string_object, built_in, : Layout_vector_object)
    direct_super_(vector_object)
    direct_super_(string)
endlayout(string_object)

#define CLASSD_vector_max   CLASSD_string_object
#define CLASSD_array_max    CLASSD_vector_max

// 17 Sequences

defabstract(sequence, built_in,)
    direct_super_(t)
endabstract(sequence)

defstorage(unbound_index, standard, : Layout_standard_object)
    direct_super_(type_error)
    direct_slot_(sequence, nil, sequence)
endstorage(unbound_index)

// 18 Hash Tables
deflayout(hash_table, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(vector,        nil, simple_vector)
    direct_slot_(test,          nil, symbol)
    direct_slot_(rehash_size,   nil, fixnum)
endlayout(hash_table)

// 19 Filenames
deflayout(pathname, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(host,      Khost,      pathname_host)
    direct_slot_(device,    Kdevice,    t)
    direct_slot_(directory, Kdirectory, or_cons_symbol)
    direct_slot_(name,      Kname,      t)
    direct_slot_(type,      Ktype,      t)
    direct_slot_(version,   Kversion,   t)
endlayout(pathname)

deflayout(basic_host, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(customary_case,    nil,        t)
    direct_slot_(default_device,    nil,        t)
    direct_slot_(local_case,        nil,        t)
    direct_slot_(name,              Kname,      simple_string)
endlayout(basic_host)

deflayout(logical_host, structure, : Layout_basic_host)
    direct_super_(basic_host)
endlayout(logical_host)

deflayout(logical_pathname, structure, : Layout_pathname)
    direct_super_(pathname)
endlayout(logical_pathname)

deflayout(physical_host, structure, : Layout_basic_host)
    direct_super_(basic_host)
endlayout(physical_host)

deflayout(physical_pathname, structure, : Layout_pathname)
    direct_super_(pathname)
endlayout(physical_pathname)

#if _WIN32
deflayout(windows_host, structure, : Layout_basic_host)
    direct_super_(basic_host)
endlayout(windows_host)

deflayout(windows_pathname, structure, : Layout_pathname)
    direct_super_(physical_pathname)
endlayout(windows_pathname)
#endif // _WIN32

defstorage(pathname_not_matched, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(pathname, Kpathname, pathname)
    direct_slot_(wildcard, Kwildcard, pathname)
endstorage(pathname_not_matched)

defstorage(pathname_parse_error, standard, : Layout_parse_error)
    direct_super_(parse_error)
    direct_slot_(host,      Khost,      basic_host)
    direct_slot_(position,  Kposition,  sequence_index)
    direct_slot_(string,    Kstring,    string)
endstorage(pathname_parse_error)

defstorage(pathname_host_not_found, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(name, Kname, string)
endstorage(pathname_host_not_found)

// 20 Files
defabstract(file_error, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(pathname, Kpathname, pathname_designator)
endabstract(file_error)

defstorage(file_not_found, standard, : Layout_error)
    direct_super_(file_error)
endstorage(file_not_found)

defstorage(path_not_found, standard, : Layout_error)
    direct_super_(file_error)
endstorage(path_not_found)

// 21 Streams
defabstract(stream, standard, : Layout_standard_object)
    direct_super_(standard_object)
    direct_slot_(flags, Kflags, fixnum)
endabstract(stream)

defabstract(input_stream, standard, : Layout_stream)
    direct_super_(stream)
endabstract(input_stream)

defabstract(output_stream, standard, : Layout_stream)
    direct_super_(stream)
endabstract(output_stream)

defabstract(io_stream, standard, : Layout_stream)
    direct_super_(input_stream)
    direct_super_(output_stream)
endabstract(io_stream)

defstorage(string_input_stream, standard, : Layout_input_stream)
    direct_super_(input_stream)

    direct_slot_(end,    Kend,      sequence_index)
    direct_slot_(index,  Kindex,    sequence_index)
    direct_slot_(start,  Kstart,    sequence_index)
    direct_slot_(string, Kstring,   string)
endstorage(string_input_stream)

defstorage(string_output_stream, standard, : Layout_output_stream)
    direct_super_(output_stream)

    direct_slot_(string, nil, string)
    direct_slot_(column, nil, sequence_index)
endstorage(string_output_stream)

defstorage(platform_error, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(code,      Kcode,      integer)
    direct_slot_(operation, Koperation, symbol)
    direct_slot_(operands,  Koperands,  list)
endstorage(platform_error)

deflayout(charset, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(name,          nil, symbol)
    direct_slot_(mime_name,     nil, or_simple_string_null)
    direct_slot_(code_page,     nil, fixnum)
endlayout(charset)

deflayout(external_format, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(charset,   nil, charset)
    direct_slot_(eol,       nil, symbol)    // :cr :crlf :crlf
    direct_slot_(flags,     nil, fixnum)
endlayout(external_format)

defabstract(platform_stream, standard, : Layout_stream)
    direct_super_(stream)
    direct_slot_(external_format,   nil, external_format)
    direct_slot_(blob,              nil, t)
endabstract(platform_stream)

defstorage(file_stream, standard, : Layout_platform_stream)
    direct_super_(platform_stream)
    direct_slot_(pathname, nil, pathname)
endstorage(file_stream)

#if _WIN32
defstorage(console_stream, standard, : Layout_platform_stream)
    direct_super_(platform_stream)
endstorage(console_stream)
#endif // _WIN32

defabstract(stream_error, standard, : Layout_standard_object)
    direct_super_(error)
    direct_slot_(stream, Kstream, stream)
endabstract(stream_error)

defstorage(end_of_file, standard, : Layout_stream_error)
    direct_super_(stream_error)
endstorage(end_of_file)

// 22 Printer
deflayout(pprint_dispatch_table, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(table, nil, hash_table)
endlayout(pprint_dispatch_table)

defstorage(print_not_readable, standard, : Layout_standard_object)
    direct_super_(error)
endstorage(print_not_readable)

// 23 Reader
deflayout(readtable, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(case,      nil, readtable_case)
    direct_slot_(table,     nil, hash_table)
    direct_slot_(vector,    nil, simple_vector)
endlayout(readtable)

defabstract(reader_error, standard, : Layout_stream_error)
    direct_super_(parse_error)
    direct_super_(stream_error)
endabstract(reader_error)

defstorage(reader_eof_error, standard, : Layout_stream_error)
    direct_super_(reader_error)
    direct_super_(simple_error)
    direct_slot_(start, Kstart, sequence_end)
endstorage(reader_eof_error)

defstorage(simple_reader_error, standard, : Layout_stream_error)
    direct_super_(reader_error)
    direct_super_(simple_error)
    inherited_slot_(format_control)
    inherited_slot_(format_arguments)
endstorage(simple_reader_error)

// 49 Closed Cell
deflayout(closed_cell, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(value, nil, t)
endlayout(clsoed_cell)

// 49 Setf Cell
deflayout(setf_cell, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(function,  nil, or_function_null)
    direct_slot_(name,      nil, symbol)

    // Note: We must make size of class is multiple sizeof(Val) * 2 for
    // static allocation.
    direct_slot_(nil,       nil, fixnum)
endlayout(setf_cell)

// 49 Tlv Record
deflayout(tlv_record, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(name,  nil, symbol)
    direct_slot_(index, nil, sequence_index)
    direct_slot_(value, nil, t)
endlayout(tlv_record)

// 49 Value Cell
deflayout(value_cell, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(value, nil, t)
    direct_slot_(name,  nil, symbol)
    direct_slot_(kind,  nil, member_Kconstant_Kspecial)
endlayout(value_cell)

// 49 Thread
deflayout(thread, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(name,  nil, t)
endlayout(thread)

deflayout(latch, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(name, nil, string)
endlayout(latch)

// 50 Weak Pointer
deflayout(weak_pointer, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(value, nil, t)
endlayout(weak_pointer)

deflayout(weak_vector_leader, built_in, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(kind, nil, symbol)
endlayout(weak_vector_leader)

// Dll Link
deflayout(dll_file_info, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(handle, nil, nil)
    direct_slot_(pathname, nil, nil)
    direct_slot_(table, nil, nil)
endlayout(dll_file_info)

deflayout(dll_proc_info, structure, : Layout_structure_object)
    direct_super_(structure_object)

    direct_slot_(dll_file_info, nil, dll_file_info)
    direct_slot_(name, nil, nil)
    direct_slot_(entry, nil, nil)
endlayout(dll_proc_info)

defstorage(dll_link_error, standard, : Layout_platform_error)
    direct_super_(platform_error)
endstorage(dll_link_error)

// HTML encode stream
defstorage(html_encode_stream, standard, : Layout_output_stream)
    direct_super_(output_stream)
    direct_slot_(mode,   nil, symbol) // (member nil :html :attribute)
    direct_slot_(stream, Kstream, stream)
endstorage(html_encode_stream)

// Regex
defabstract(range, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(end,   nil, sequence_index)
    direct_slot_(start, nil, sequence_index)
endabstract(range)

deflayout(regex, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(blob, nil, t)
    direct_slot_(captures, nil, simple_vector)
    direct_slot_(string, nil, simple_string)
endlayout(regex)

deflayout(regex_match, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(captures,  nil, simple_vector)     // [1]
    direct_slot_(matched_p, nil, boolean)           // [2]
    direct_slot_(regex,     nil, regex)             // [3]
    direct_slot_(range,     nil, range)             // [4]
endlayout(regex_match)

deflayout(string_range, built_in, : Layout_range)
    direct_super_(range)
    direct_slot_(string,    nil, string)
endlayout(string_range)

// Compiler
defstorage(pseudo_class, standard, : Layout_class)
    direct_super_(class)
endstorage(pseudo_class)

defabstract(pseudo_object, pseudo, )
    direct_super_(t)
endabstract(pseudo_object)

deflayout(literal_cell, pseudo, : Layout_pseudo_object)
    direct_super_(pseudo_object)
    direct_slot_(value, nil, t)
endlayout(literal_cell)

deflayout(stack_cell, pseudo, : Layout_pseudo_object)
    direct_super_(pseudo_object)
    direct_slot_(value, nil, t)
endlayout(stack_cell)

// <summary>
//   Stack-Frame represents a function call on the stack of thread.
// </summary>
deflayout(stack_frame, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(code_index,    nil, sequence_index)
    direct_slot_(function,      nil, function)
    direct_slot_(line_column,   nil, sequence_index)
    direct_slot_(line_number,   nil, sequence_index)
    direct_slot_(pathname,      nil, or_pathname_null)
endlayout(stack_frame)

// Realize-instance protocol
defstorage(already_realized, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(object, Kobject, t)
endstorage(already_realized)

defstorage(not_realized, standard, : Layout_error)
    direct_super_(error)
    direct_slot_(object, Kobject, t)
endstorage(not_realized)

#include "./xml/xml_layout.inc"

#include "./tinycl_end_list.h"
