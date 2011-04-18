//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions
// tinycl_rtl2.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl2.h#1 $
//
#if !defined(INCLUDE_tinycl_rtl2_h)
#define INCLUDE_tinycl_rtl2_h

namespace TinyCl
{

Val __fastcall CallLisp(Thread*);

// [!]
void NoReturn __fastcall Bgo(Val);
defun(Bmake_closureV, (Thread*));
void NoReturn __fastcall BreturnV(Thread*);
void NoReturn __fastcall BthrowV(Thread*);

// [%]
defun(Pdefun, (Val, Val, Val));

// [F]
Val find_slot(Val, Val);
Val find_type(Val, Val, Val);
defun(function_information, (Val, Val = nil));
defun(function_name, (Val));
Val funcall_(Val);

// [V]
defun(variable_information, (Val, Val = nil));

namespace CommonLisp
{
// [A]
defun(appendV, (Thread*));
defun(apply, (Val, Val));

// [C]
defun(class_name, (Val));
defun(class_of, (Val));
bool constantp(Val, Val = nil);

// [D]
defun(describe, (Val));
defun(disassemble, (Val));

// [E]
defun(eval, (Val));

// [F]
defun_setf(fdefinition, (Val, Val));
defun(funcallV, (Thread*));

// [G]
defun(gensym, (Val = MARKER_unbound));
defun(get_setf_expansion, (Val, Val = nil));

// [L]
defun(last, (Val, Val = one));

// [P]
defun(print_, (Val, Val, Val));

// [M]
Val macro_function(Val, Val = nil);
Val setf_macro_function(Val, Val, Val = nil);
Val macroexpand_1(Val, Val = nil);

// [S]
defun_setf(symbol_function, (Val, Val));
defun_setf(symbol_value, (Val, Val));

} // CommonLisp
} // TinyCl

#endif //!defined(INCLUDE_tinycl_rtl2_h)
