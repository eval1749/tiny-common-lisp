// -*- Mode: C++; -*-
//
// TinyCl - TinyCl Compiler Static Objects
// tinycl_compiler.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_debugger.h#4 $
//
// Description:
//  This file contains static objects used in compiler.
//
#if !defined(defdeclare)
    #define defdeclare(mp_name)
#endif

#if !defined(definstruction)
    #define definstruction(mp_name, mp_format)
#endif

#if !defined(defparser)
    #define defparser(mp_name)
#endif

#if !defined(defspecial)
    #define defspecial(mp_name)
#endif

#if !defined(defstatic)
    #define defstatic(mp_type, mp_cname)
#endif

#if !defined(defstatic2)
    #define defstatic2(mp_type, mp_cname, mp_a)
#endif

#if !defined(Pass)
    #define Pass(mp_name, mp_bits)
#endif

//////////////////////////////////////////////////////////////////////
//
// Parser
//
defspecial(block)                  // 1
defspecial(catch)                  // 2
defspecial(eval_when)              // 3
defspecial(flet)                   // 4
defspecial(function)               // 5
defspecial(go)                     // 6
defspecial(if)                     // 7
defspecial(labels)                 // 8
defspecial(let)                    // 9
defspecial(letA)                   // 10
defspecial(load_time_value)        // 11
defspecial(locally)                // 12
defspecial(macrolet)               // 13
defspecial(multiple_value_call)    // 14
defspecial(multiple_value_prog1)   // 15
defspecial(progn)                  // 16
defspecial(progv)                  // 17
defspecial(quote)                  // 18
defspecial(return_from)            // 19
defspecial(setq)                   // 20
defspecial(symbol_macrolet)        // 21
defspecial(tagbody)                // 22
defspecial(the)                    // 23
defspecial(throw)                  // 24
defspecial(unwind_protect)         // 25

// Functions and Macros
defparser(apply)
defparser(declare)
defparser(eq)
defparser(funcall)
defparser(handler_bind)
defparser(handler_case)
defparser(lambda)
defparser(multiple_value_bind)
defparser(not)
defparser(nth_value)
defparser(null)
defparser(return)
defparser(typep)
defparser(values)

// Declaration Identifier
defdeclare(declaration)
defdeclare(dynamic_extent)
defdeclare(ftype)
defdeclare(function_name)
defdeclare(ignorable)
defdeclare(ignore)
defdeclare(inline)
defdeclare(notinline)
defdeclare(optimize)
defdeclare(special)
defdeclare(type)
defdeclare(values)

//////////////////////////////////////////////////////////////////////
//
// Passes
//
// Note: Order of this declaration is exectuion order.
Pass(X86Target,     All)
Pass(Parse,         All)
Pass(Clean,         All)    // Make self and mutal-tail-call
Pass(TailCall,      All)
Pass(Inline,        All)
Pass(Closure,       All)
Pass(Cfg2Ssa,       All)
Pass(Typep,         All)
Pass(PropType,      All)
Pass(Clean,         All)

// Code genration
Pass(SplitClosure,  All)
Pass(UpVar,         All)
Pass(RuntimeCast,   All)
Pass(X86Typep,      All)
Pass(X86Lower,      All)
Pass(Clean,         All)    // Clean TYPEP expansion and Lower
Pass(X86Ensure,     All)
Pass(Ra,            All)
Pass(Stack,         All)
Pass(Arity,         All)
Pass(X86Values,     All)
Pass(FinalClean,    All)    // Remove empty bblock created by X86Values
Pass(Layout,        All)
Pass(Assemble,      All)

//////////////////////////////////////////////////////////////////////
//
// IR Instructions
//

// [A]
definstruction(Add,         "ADD      ty %rd <= %sx %sy")
definstruction(Assign,      "ASSIGN   ty %rd <= %sx")

// [B]
definstruction(Box,         "BOX ty %rd <= %sx")
definstruction(Branch,      "BRANCH %bx labely labelz")

// [C]
definstruction(Call,        "CALL ty %d        <= %sx %vy")
definstruction(Catch,       "CATCH                %fx typespec label")
definstruction(Close,       "CLOSE                %rx")
definstruction(Closure,     "CLOSURE ty %fd    <= %sx %vy")
definstruction(Convert,     "CONVERT ty %rd <= %rx")  // for function float
definstruction(Copy,        "COPY ty %rd       <= %sx")
definstruction(Count,       "COUNT  fixnum %rd <= %vx")

// [D]
definstruction(Div,         "DIV    ty %rd <= %sx %sy")

// [E]
definstruction(Entry,       "ENTRY")
definstruction(Eq,          "EQ %bd <= %sx %sy")
definstruction(Exit,        "EXIT")

// [F]
definstruction(Frame,       "FRAME ty %rd <= %rx")

// [G]
definstruction(Ge,          "GE %bd <= %sx %sy")
definstruction(Go,          "GO %rx")
definstruction(Gt,          "GT %bd <= %sx %sy")

// [I]
definstruction(If,          "IF ty %rd <= %bx %sy %sz")

// [J]
definstruction(Jump,        "JUMP label")

// [K]
definstruction(KeySupplied, "KEYSUPPLIED bool %bd <= %rx key")
definstruction(KeyVal,      "KEYVAL ty %rd <= %vx key")

// [L]
definstruction(Le,          "LE         %bd <= %sx %sy")
definstruction(Load,        "LOAD    ty %rd <= %rx")
definstruction(LoadFun,     "LOADFUN ty %rd <= fn")
definstruction(LogAnd,      "LOGAND  ty %rd <= %sx %sy")
definstruction(LogEqv,      "LOGEQV  ty %rd <= %sx %sy")
definstruction(LogIor,      "LOGIOR  ty %rd <= %sx %sy")
definstruction(LogXor,      "LOGXOR  ty %rd <= %sx %sy")
definstruction(Lt,          "LT         %bd <= %sx %sy")

// [M]
definstruction(Mul,         "MUL    ty %rd <= %sx %sy")
definstruction(MvRestore,   "MVRESTORE %vd <= %sx")
definstruction(MvSave,      "MVSAVE    %sd <= %vx")


// [N]
definstruction(Ne,          "NE          %bd <= %sx %sy")
definstruction(Nonlocal,    "NONLOCAL    %rd <=")
definstruction(NthValue,    "NTHVALUE ty %rd <= %rx %vy")

// [O]
definstruction(OpenBind,    "OPENBIND    %fd <= (var %sx)*")
definstruction(OpenBlock,   "OPENBLOCK   %fd <= name label")
definstruction(OpenCatch,   "OPENCATCH   %fd <= %sx label")
definstruction(OpenFinally, "OPENFINALLY %fd <= %fn %vy")
definstruction(OpenHandler, "OPENHANDLER %fd <= type fn ...")
definstruction(OpenTags,    "OPENTAGS    %fd <=")
definstruction(OpenTry,     "OPENTRY    %fd <=")

// [P]
definstruction(ParseKeys,   "PARSEKEYS ty %vd <= %rx type key...")
definstruction(Phi,         "PHI       ty %d <= (label %sx)...")
definstruction(Prologue,    "PROLOGUE  ty %vd <= checkArity resify")

// [P]
definstruction(PhiCopy,     "PHICOPY %rd <= %sx")

// [R]
definstruction(Reload,      "RELOAD      ty %pd <= %mx")
definstruction(Ret,         "RET                   %sx")
definstruction(ReturnFrom,  "RETURNFROM            %rx %vy")
definstruction(RuntimeCast, "RUNTIMECAST ty %rd <= %rx")

// [S]
definstruction(Select,      "SELECT     ty %rd <= %vx nth")
definstruction(Shl,         "SHL        ty %rd <= %sx %sy")
definstruction(ShouldBe,    "SHOULDBE   ty %rd <= %sx")
definstruction(Shr,         "SHR        ty %rd <= %sx %sy")
definstruction(Slot,        "SLOT       ty %rd <= class slot %sz")
definstruction(Spill,       "SPILL      ty %md <= %px")
definstruction(StaticCast,  "STATICCAST ty %rd <= %rx")
definstruction(Store,       "STORE                %rx %sy")
definstruction(Sub,         "SUB        ty %rd <= %sx %sy")
definstruction(Swap,        "SWAP       ty %pd <= %px")

// [T]
definstruction(TagDef,      "TAGDEF     ty   %rd <= %fx label")
definstruction(Throw,       "THROW                  %sx %vy")
definstruction(Tlv,         "TLV        ty   %rd <= tlvrec")
definstruction(Typep,       "TYPEP      bool %bd <= %sx type")

// [U]
definstruction(UnBox,       "UNBOX ty %rd <= %sx")
definstruction(Unreachable, "UNREACHABLE")
definstruction(UpVarBase,   "UPVARBASE   %rd <= fn")
definstruction(UpVarDef,    "UPVARDEF ty %rd <= var")
definstruction(UpVarRef,    "UPVARREF ty %rd <= %rx %qy")
definstruction(Use,         "USE %rx")

// [V]
definstruction(Values,      "VALUES   ty %vd <= %sx...")
definstruction(ValuesA,     "VALUES*  ty %vd <= %sx...")
definstruction(VarAnnex,    "VARANNEX ty %md <= %cx")
definstruction(VarDef,      "VARDEF   ty %rd <= var %sy")
definstruction(VarHome,     "VARHOME  ty %md <= %qx %ry")
definstruction(VarRef,      "VARREF   ty %rd <= %qx")

// X86 specific instructions
definstruction(x86Bool,     "X86BOOL   bool   %bd <= %ix int")
definstruction(x86Clc,      "X86CLC")       // for values
definstruction(x86CvtFloat, "X86CVTFLOAT ty   %fx <= %rd")
definstruction(x86CvtInt,   "X86CVTINT   ty   %rd <= %fx")
definstruction(x86Encode,   "X86ENCODE   ty   %rd <= %rx") // for float
definstruction(x86Cmp,      "X86CMP      int  %id <= %sx %sy")
definstruction(x86CmpF32,   "X86CMPF32   int  %id <= %sx %sy")
definstruction(x86CmpF64,   "X86CMPF64   int  %id <= %sx %sy")
definstruction(x86Lea,      "X86LEA      int  %rd <= %rx %sy")
definstruction(x86Stc,      "X86STC") // for values
definstruction(x86Test,     "X86TEST     int  %id <= %sx %sy")
definstruction(x86Zero,     "X86ZERO     ty   %rd <=") // for +0.0


//////////////////////////////////////////////////////////////////////
//
// Static Objects
//
defstatic2(TyClass, tyBitVector,    CLASS_bit_vector)
defstatic2(TyClass, tyCharacter,    CLASS_character)
defstatic2(TyClass, tyClosedCell,   CLASS_closed_cell)
defstatic2(TyClass, tyCons,         CLASS_cons)
defstatic2(TyClass, tyDoubleFloat,  CLASS_double_float)
defstatic2(TyClass, tyFixnum,       CLASS_fixnum)
defstatic2(TyClass, tyFunction,     CLASS_function)
defstatic2(TyClass, tyList,         CLASS_list)
defstatic2(TyClass, tyLiteralCell,  CLASS_literal_cell)
defstatic2(TyClass, tyNull,         CLASS_null)
defstatic2(TyClass, tyNumber,       CLASS_number)
defstatic2(TyClass, tyPathname,     CLASS_pathname)
defstatic2(TyClass, tySingleFloat,  CLASS_single_float)
defstatic2(TyClass, tyStackCell,    CLASS_stack_cell)
defstatic2(TyClass, tyString,       CLASS_string)
defstatic2(TyClass, tySymbol,       CLASS_symbol)
defstatic2(TyClass, tyT,            CLASS_t)

defstatic2(TyForeign, tyBool,        Qbool)
defstatic2(TyForeign, tyFloat32,     Qfloat32)
defstatic2(TyForeign, tyFloat64,     Qfloat64)
defstatic2(TyForeign, tyInt,         Qint)
defstatic2(TyForeign, tyInt32,       Qint32)
defstatic2(TyForeign, tyNil,         nil)
defstatic2(TyForeign, tyPtrT,        Qptr)
defstatic2(TyForeign, tyUInt,        Quint)
defstatic2(TyForeign, tyUInt32,      Quint32)
defstatic2(TyForeign, tyVoid,        Qvoid)

defstatic2(TyValuesRestT, tyValuesRestT, tyT)

defstatic(
    TyUnknownFunction,
    tyUnknownFunction )


defstatic(Bool,              Bool_False)
defstatic(Bool,              Bool_True)
defstatic(TrueOperand,       True)
defstatic(UnreachableOutput, Unreachable)
defstatic(VoidOutput,        Void)

#undef defdeclare
#undef definstruction
#undef defparser
#undef defspecial
#undef defstatic
#undef defstatic2
#undef Pass
