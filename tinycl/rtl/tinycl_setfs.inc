// -*- Mode: C++; -*-
// TinyCl - Setf Cells
// tinycl_setf.inc
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//

#if !defined(defsetf)
    #define defsetf(mp_cname)
#endif // !defined(defsetf)

// [!]
defsetf(Btlv)

// [A]
defsetf(aref)

// [B]
defsetf(bit)

// [C]
defsetf(car) defsetf(cdr)

defsetf(caar) defsetf(cadr) defsetf(cdar) defsetf(cddr)

defsetf(caaar) defsetf(caadr) defsetf(cadar) defsetf(caddr)
defsetf(cdaar) defsetf(cdadr) defsetf(cddar) defsetf(cdddr)

defsetf(caaaar) defsetf(caaadr) defsetf(caadar) defsetf(caaddr)
defsetf(cadaar) defsetf(cadadr) defsetf(caddar) defsetf(cadddr)
defsetf(cdaaar) defsetf(cdaadr) defsetf(cdadar) defsetf(cdaddr)
defsetf(cddaar) defsetf(cddadr) defsetf(cdddar) defsetf(cddddr)

defsetf(char)
defsetf(class_name)
defsetf(compiler_macro_function)

// [D]
defsetf(documentation)

// [E]
defsetf(eighth)
defsetf(elt)

// [F]
defsetf(fdefinition)
defsetf(fifth)
defsetf(fill_pointer)
defsetf(find_class)
defsetf(first)
defsetf(fourth)

// [G]
defsetf(get)
defsetf(getf)
defsetf(gethash)

// [L]
defsetf(logical_pathname_translations)

// [M]
defsetf(macro_function)

// [N]
defsetf(ninth)
defsetf(nth)

// [R]
defsetf(readtable_case)
defsetf(rest)
defsetf(row_major_aref)

// [S]
defsetf(sbit)
defsetf(schar)
defsetf(second)
defsetf(seventh)
defsetf(sixth)
defsetf(slot_value)
defsetf(slot_value_using_class)
defsetf(subseq)
defsetf(svref)
defsetf(symbol_function)
defsetf(symbol_plist)
defsetf(symbol_value)

// [T]
defsetf(tenth)
defsetf(third)

#undef defsetf
