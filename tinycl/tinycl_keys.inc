// -*- Mode: C++; -*-
// TinyCl - Keywords
// tinycl_keys.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_syms.inc#18 $
//

#include "./tinycl_begin_list.h"

// [0]
defkeyword(32bit,               "32BIT")                // *features*
defkeyword(64bit,               "64BIT")                // *features*

// [A]
defkeyword(abort,               "ABORT")
defkeyword(absolute,            "ABSOLUTE")
defkeyword(allocation,          "ALLOCATION")
defkeyword(allow_other_keys,    "ALLOW-OTHER-KEYS")
defkeyword(ansi_cl,             "ANSI-CL")              // *features*
defkeyword(array,               "ARRAY")                // printer
defkeyword(append,              "APPEND")
defkeyword(arguments,           "ARGUMENTS")
defkeyword(attribute,           "ATTRIBUTE")            // html-encode-stream

// [B]
defkeyword(back,                "BACK")                 // pathname
defkeyword(backspace,           "BACKSPACE")            // char name
defkeyword(base,                "BASE")                 // printer
defkeyword(big_endian,          "BIG-ENDIAN")           // big-endian
defkeyword(block,               "BLOCK")                // pprint

// [C]
defkeyword(capitalize,          "CAPITALIZE")           // printer
defkeyword(capture,             "CAPTURE")              // regex
defkeyword(case,                "CASE")                 // pathname-name
defkeyword(character,           "CHARACTER")
defkeyword(circle,              "CIRCLE")               // printer
defkeyword(class,               "CLASS")
defkeyword(code,                "CODE")                 // open
defkeyword(common,              "COMMON")               // pathname-name
defkeyword(common_lisp,         "COMMON-LISP")          // *features*
defkeyword(compile_toplevel,    "COMPILE-TOPLEVEL")     // eval-when
defkeyword(compile_time_too,    "COMPILE-TIME-TOO")
defkeyword(constant,            "CONSTANT")             // value-cell
defkeyword(count,               "COUNT")
defkeyword(cr,                  "CR")
defkeyword(create,              "CREATE")               // open
defkeyword(crlf,                "CRLF")
defkeyword(current,             "CURRENT")              // pprint

// [D]
defkeyword(data,                "DATA")                 // xmldom
defkeyword(datum,               "DATUM")
defkeyword(default,             "DEFAULT")              // open
defkeyword(defaults,            "DEFAULTS")             // make-pathname
defkeyword(device,              "DEVICE")
defkeyword(direction,           "DIRECTION")            // open
defkeyword(directory,           "DIRECTORY")
defkeyword(discriminator,       "DISCRIMINATOR")
defkeyword(document,            "DOCUMENT")             // xmldom
defkeyword(downcase,            "DOWNCASE")             // readtable-case

// [E]
defkeyword(end,                 "END")
defkeyword(end1,                "END1")
defkeyword(end2,                "END2")
defkeyword(eof,                 "EOF")                  // stream-read-char
defkeyword(error,               "ERROR")                // open
defkeyword(escape,              "ESCAPE")               // printer
defkeyword(execute,             "EXECUTE")              // eval-when
defkeyword(expected_type,       "EXPECTED-TYPE")
defkeyword(extended_syntax,     "EXTENDED-SYNTAX")      // regex
defkeyword(external,            "EXTERNAL")             // find-symbol
defkeyword(external_format,     "EXTERNAL-FORMAT")      // open

// [F]
defkeyword(fill,                "FILL")                 // pprint
defkeyword(finally,             "FINALLY")              // unwind-protect
defkeyword(flags,               "FLAGS")
defkeyword(free,                "FREE")
defkeyword(form,                "FORM")
defkeyword(format_arguments,    "FORMAT-ARGUMENTS")
defkeyword(format_control,      "FORMAT-CONTROL")
defkeyword(from_end,            "FROM-END")
defkeyword(function,            "FUNCTION")

// [G]
defkeyword(generic_function,    "GENERIC-FUNCTION")
defkeyword(gensym,              "GENSYM")               // printer

// [H]
defkeyword(host,                "HOST")                 // pathname
defkeyword(html,                "HTML")                 // html-encode-stream

// [I]
defkeyword(identity_with_one_argument, "IDENTITY-WITH-ONE-ARGUMENT")
defkeyword(ieee_floating_point,     "IEEE-FLOATING-POINT")  // *features*
defkeyword(initial_element,         "INITIAL-ELEMENT")
defkeyword(invalid,                 "INVALID")
defkeyword(invert,                  "INVERT")
defkeyword(if_does_not_exist,       "IF-DOES-NOT-EXIST")    // open
defkeyword(if_exists,               "IF-EXISTS")            // open
defkeyword(ignore_case,             "IGNORE-CASE")          // regex
defkeyword(index,                   "INDEX")
defkeyword(inherited,               "INHERITED")            // find-symbol
defkeyword(initargs,                "INITARGS")
defkeyword(initform,                "INITFORM")
defkeyword(initfunction,            "INITFUNCTION")
defkeyword(initial_value,           "INITIAL-VALUE")
defkeyword(input,                   "INPUT")
defkeyword(instance,                "INSTANCE")
defkeyword(interactive_function,    "INTERACTIVE-FUNCTION")
defkeyword(internal,                "INTERNAL")             // find-symbol
defkeyword(io,                      "IO")

// [J]
defkeyword(junk_allowed,        "JUNK-ALLOWED")         // parse-namestring

// [L]
defkeyword(lambda_list,         "LAMBDA-LIST")
defkeyword(length,              "LENGTH")               // printer
defkeyword(level,               "LEVEL")                // printer
defkeyword(lines,               "LINES")                // printer
defkeyword(lf,                  "LF")
defkeyword(line,                "LINE")                 // pprint
defkeyword(line_relative,       "LINE-RELATIVE")        // pprint
defkeyword(linear,              "LINEAR")               // pprint
defkeyword(linefeed,            "LINEFEED")
defkeyword(little_endian,       "LITTLE-ENDIAN")        // *features*
defkeyword(lexical,             "LEXICAL")
defkeyword(load_toplevel,       "LOAD-TOPLEVEL")        // eval-when
defkeyword(local,               "LOCAL")                // pathname-name
defkeyword(location,            "LOCATION")

// [K]
defkeyword(key,                 "KEY")
defkeyword(keys,                "KEYS")

// [M]
defkeyword(macro,               "MACRO")
defkeyword(mandatory,           "MANDATORY")            // pprint
defkeyword(max_age,             "MAX-AGE")
defkeyword(method_class,        "METHOD-CLASS")
defkeyword(method_combination,  "METHOD-COMBINATION")
defkeyword(min_age,             "MIN-AGE")
defkeyword(miser,               "MISER")                // pprint
defkeyword(miser_width,         "MISER-WIDTH")          // pprint
defkeyword(mixed,               "MIXED")
defkeyword(multiple_line,       "MULTIPLE-LINE")        // regex

// [N]
defkeyword(name,                "NAME")
defkeyword(newest,              "NEWEST")
defkeyword(newline,             "NEWLINE")
defkeyword(new_version,         "NEW-VERSION")
defkeyword(nicknames,           "NICKNAMES")            // make-package
defkeyword(no_error,            "NO-ERROR")             // handler-case
defkeyword(node,                "NODE")                 // xmldom

// [O]
defkeyword(object,              "OBJECT")
defkeyword(operation,           "OPERATION")
defkeyword(operands,            "OPERANDS")
defkeyword(operator,            "OPERATOR")
defkeyword(options,             "OPTIONS")
defkeyword(output,              "OUTPUT")
defkeyword(overwrite,           "OVERWRITE")

// [P]
defkeyword(package,             "PACKAGE")
defkeyword(page,                "PAGE")
defkeyword(parent_node,         "PARENT-NODE")          // xmldom
defkeyword(pattern,             "PATTERN")
defkeyword(pathname,            "PATHNAME")
defkeyword(plist,               "PLIST")
defkeyword(position,            "POSITION")
defkeyword(pprint_dispatch,     "PPRINT-DISPATCH")      // printer
defkeyword(prefix,              "PREFIX")               // xmldom
defkeyword(pretty,              "PRETTY")               // printer
defkeyword(preserve,            "PRESERVE")
defkeyword(preserve_whitespace, "PRESERVE-WHITESPACE")
defkeyword(probe,               "PROBE")                // open

// [Q]
defkeyword(qualifiers,          "QUALIFIERS")

// [R]
defkeyword(radix,               "RADIX")                // printer
defkeyword(readably,            "READABLY")             // printer
defkeyword(readers,             "READERS")
defkeyword(rehash_size,         "REHASH-SIZE")
defkeyword(rehash_threshold,    "REHASH-THRESHOLD")
defkeyword(relative,            "RELATIVE")
defkeyword(removed,             "REMOVED")
defkeyword(rename,              "RENAME")               // open
defkeyword(rename_and_delete,   "RENAME-AND-DELETE")    // open
defkeyword(report_function,     "REPORT-FUNCTION")
defkeyword(return,              "RETURN")               // char name
defkeyword(right_margin,        "RIGHT-MARGIN")         // ptiner
defkeyword(rubout,              "RUBOUT")               // char name

// [S]
defkeyword(section,             "SECTION")              // pprint
defkeyword(section_relative,    "SECTION-RELATIVE")     // pprint
defkeyword(sequence,            "SEQUENCE")
defkeyword(shift_jis,           "SHIFT_JIS")
defkeyword(single_line,         "SINGLE-LINE")          // regex
defkeyword(size,                "SIZE")
defkeyword(space,               "SPACE")
defkeyword(special,             "SPECIAL")
defkeyword(special_operator,    "SPECIAL-OPERATOR")
defkeyword(specializers,        "SPECIALIZERS")
defkeyword(start,               "START")
defkeyword(start1,              "START1")
defkeyword(start2,              "START2")
defkeyword(stream,              "STREAM")
defkeyword(string,              "STRING")
defkeyword(supersede,           "SUPERSEDE")
defkeyword(symbol_macro,        "SYMBOL-MACRO")
defkeyword(syntax,              "SYNTAX")

// [T]
defkeyword(tab,                 "TAB")                  // char name
defkeyword(test,                "TEST")
defkeyword(test_function,       "TEST-FUNCTION")
defkeyword(test_not,            "TEST-NOT")
defkeyword(type,                "TYPE")
defkeyword(toplevel,            "TOPLEVEL")             // compiler

// [U]
defkeyword(unbound,             "UNBOUND")
defkeyword(unspecific,          "UNSPECIFIC")
defkeyword(upcase,              "UPCASE")
defkeyword(use,                 "USE")                  // make-package
defkeyword(utf_7,               "UTF-7")
defkeyword(utf_8,               "UTF-8")
defkeyword(utf_16,              "UTF-16")
defkeyword(utf_16be,            "UTF-16BE")
defkeyword(utf_16le,            "UTF-16LE")

// [V]
defkeyword(version,             "VERSION")
//defkeyword(variable,            "VARIABLE")

// [W]
defkeyword(wild,                "WILD")
defkeyword(wild_1,              "WILD-1")
defkeyword(wildcard,            "WILDCARD")
defkeyword(wild_inferior,       "WILD-INFERIOR")
#if _WIN32
defkeyword(win32,               "WIN32")                // *features*
#endif
defkeyword(writers,             "WRITERS")

// [X]
#if _WIN32
defkeyword(x86,                 "X86")                  // *features*
#endif

#include "./tinycl_end_list.h"
