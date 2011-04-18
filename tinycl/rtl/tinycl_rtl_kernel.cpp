#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Kernel
// tinycl_rtl_kernel.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_kernel.cpp#3 $
//
#include "../tinycl.h"

namespace TinyCl
{

using namespace Private;

defun(allocate_binobj, (Val const classd))
{
    return Thread::Get()->AllocBinObj(classd);
} // allocate_binobj

defun(allocate_binvec, (Val const classd, Val const length))
{
    return Thread::Get()->AllocBinVec(classd, length);
} // allocate_binvec

defun(allocate_record, (Val const classd))
{
    return Thread::Get()->AllocRecord(classd);
} // allocate_record

//    Current = weak object             cons
//      +-----------+                   +--------+
//  [0] |  free o---+---+ <------+      | callee |
//      +-----------+   |        |      +--------+
//  [1] | caller_1  |   |        +------+---o    |
//      +-----------+   |               +--------+
//  [2] | caller_2  |   |
//      +-----------+   |
//        ...           |
//      +-----------+   |
//      |           |<--+
//      +-----------+ 

enum { Default_Caller_Set_Length = 10 };

Val intern_callee(Val const cell)
{
    ASSERT(symbolp(cell) || setf_cell_p(cell));

    Val const entry = gethash(cell, VAR(Acaller_tableA));
    if (nil != entry)
    {
        return entry;
    }

    Val const caller_set = make_caller_set(
        Fixnum::Encode(Default_Caller_Set_Length) );

    Val callee;
    {
        if (Symbol* p = cell->DynamicCast<Symbol>())
        {
            callee = p->m_function;
        }
        else if (SetfCell* p = cell->DynamicCast<SetfCell>())
        {
            callee = p->m_function;
        }
        else
        {
            CAN_NOT_HAPPEN();
        }

        if (nil == callee)
        {
            callee = make_undefined_function_function(cell);
        }
    } // callee

    return setf_gethash(cons(callee, caller_set), cell, VAR(Acaller_tableA));
} // intern_callee

Val make_caller_set(Val const length)
{
    Val const caller_set = make_vector(length);
    setf_svref(one, caller_set, zero);
    return caller_set;
} // make_caller_set

Val register_caller(Val const fname, Val const caller)
{
    Val cell;
    Val name = fname;
    if (functionp(name))
    {
        // FIXME 2007-11-06 yosi@msn.com NYI support GF in register_caller.
        name = function_name(name);
    }

    if (consp(name))
    {
        cell = intern_setf_cell(cadr(name));
    }
    else
    {
        cell = name;
    }

    unless (symbolp(cell) || setf_cell_p(cell))
    {
        SignalTypeError(name, Qfunction_name);
    }

    check_type(caller, function);

    Val entry = intern_callee(cell);

    Val caller_set = cdr(entry);

    Val const free = svref(caller_set, zero);
    Val home = free;
    for (Val index = one; index < free; index = add(index, 1))
    {
        Val const present = svref(caller_set, index);
        if (present == caller)
        {
            return car(entry);
        }

        if (! functionp(present))
        {
            if (home > index)
            {
                home = index;
            }
        }
    } // for

    if (ge(home, length(caller_set)))
    {
        Val const old_caller_set = caller_set;
        caller_set = make_caller_set(truncate(mul(home, 120), 100));

        ::CopyMemory(
            caller_set->StaticCast<SimpleVector>()->GetStart(),
            old_caller_set->StaticCast<SimpleVector>()->GetStart(),
            Fixnum::Decode_(home) * sizeof(Val) );
    }

    if (home == free)
    {
        setf_svref(add(free, 1), caller_set, zero);
    }

    setf_svref(caller, caller_set, home);

    return car(entry);
} // register_caller

/// <summary>
///   Save running lisp memory image into specified file.
/// </summary>
/// <param name="thing">pathname designator</param>
/// <returns>True if succeeded</returns>
defun(save_image, (Val const thing))
{
    Val const filename = namestring(pathname(thing));
    TinyCl::Mm::Save(filename->StaticCast<SimpleString>()->GetStart());
    return t;
} // save_image

} // TinyCl
