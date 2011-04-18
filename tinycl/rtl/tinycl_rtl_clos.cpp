#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime CLOS
// tinycl_rtl_clos.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_clos.cpp#4 $
//
#include "../tinycl_clos.h"

namespace TinyCl
{

using namespace Private;

class Static
{
    private: static Val findArg(Val key, Val args)
    {
        foreach (List::Enum, oEnum, args)
        {
            if (oEnum.Get() == key)
            {
                return oEnum.GetCons();
            }

            oEnum.Next();
            
            if (oEnum.AtEnd())
            {
                error("Invalid initargs: ~S", args);
            }
        } // for

        return nil;
    } // findArg

    // [I]
    public: static void InitSlots(
        Class* const pClass,
        Val*   const prxSlot,
        Val    const args,
        bool   const fUnbound = false)
    {
        ASSERT(NULL != pClass);
        ASSERT(NULL != prxSlot);

        foreach (List::Enum, oEnum, pClass->m_slots)
        {
            Layout_effective_slot_definition* const pESlotD =
                oEnum.Get()->StaticCast<Instance>()->
                    GetStorage<Layout_effective_slot_definition>();

            Int const iLoc = Fixnum::Decode_(pESlotD->m_location);

            if (0 == iLoc && Qclass_description == pESlotD->m_name)
            {
                continue;
            }

            Val initarg = nil;
            foreach (List::Enum, oEnum, pESlotD->m_initargs)
            {
                initarg = findArg(oEnum.Get(), args);
                if (nil != initarg)
                {
                    break;
                }
            } // for each initarg

            Val val;
            if (nil != initarg)
            {
                val = cadr(initarg);
            }
            else if (nil != pESlotD->m_initfunction)
            {
                val = funcall(pESlotD->m_initfunction);
            }
            else if (fUnbound)
            {
                val = MARKER_unbound;
            }
            else
            {
                continue;
            }

            prxSlot[iLoc] = val;
        } // for each eslotd
    } // InitSlots
}; // Static

// [F]
Val find_slot(Val klass, Val name)
{
    foreach (List::Enum, oEnum, klass->StaticCast<Class>()->m_slots)
    {
        Val eslotd = oEnum.Get();
        Layout_effective_slot_definition* pElotD =
            eslotd->StaticCast<Instance>()->
                GetStorage<Layout_effective_slot_definition>();
        if (pElotD->m_name == name)
        {
            return eslotd;
        }
    } // for each slot
    return nil;
} // find_slot



// [M]
defmethod(make_instance, funcallable_standard_class, (Thread* const pth))
{
    Val const klass = pth->mv_value[0];
    Val const args = pth->ValuesToList(1);
    Class* const pClass = klass->StaticCast<Class>();
    if (nil == pClass->m_instanced)
    {
        error(Qclass_not_finalized, Kclass, klass);
    }

    Val const fin = allocate_funcallable_instance(pClass->m_instanced);

    Val* const prxSlot = fin->StaticCast<FuncallableInstance>()->
        GetStorage<Storage>()->GetStart();

    Static::InitSlots(pClass, prxSlot, args);

    return fin;
} // make_instance

defmethod(make_instance, standard_class, (Thread* const pth))
{
    Val const klass = pth->mv_value[0];
    Val const args = pth->ValuesToList(1);
    Class* const pClass = klass->StaticCast<Class>();
    if (nil == pClass->m_instanced)
    {
        error(Qclass_not_finalized, Kclass, klass);
    }

    Val const obj = pth->AllocInstance(pClass->m_instanced);

    Val* const prxSlot = obj->StaticCast<Instance>()->
        GetStorage<Storage>()->GetStart();

    Static::InitSlots(pClass, prxSlot, args, true);

    return obj;
} // make_instance

defmethod(make_instance, structure_class, (Thread* const pth))
{
    Val const klass = pth->mv_value[0];
    Val const args = pth->ValuesToList(1);
    Class* const pClass = klass->StaticCast<Class>();
    Val const obj = pth->AllocRecord(pClass->m_instanced);

    Val* const prxSlot = obj->StaticCast<Record>()->To<Val>();

    Static::InitSlots(pClass, prxSlot, args);

    return obj;
} // make_instance

defmethod(make_instance, symbol, (Thread* const pth))
{
    Val args = pth->ValuesToList();
    Val klass = find_class(pth->mv_value[0]);
    pth->ValuesFromList(args);
    pth->mv_value[0] = klass;
    return funcall_(Qmake_instance);
} // make_instance

// [P]
defmethod(print_object, standard_method, (Val const x, Val const s))
{
    Method* const p = x->StaticCast<Method>();
    format(s, "#<~S ~S ",
        class_name(class_of(x)),
        functionp(p->m_generic_function) ?
            function_name(p->m_generic_function) : nil );

    foreach (List::Enum, oEnum, p->m_qualifiers)
    {
        format(s, "~S ", oEnum.Get());
    } // for each qualifier

    Val sep = Character::FromCode(0x28);
    foreach (List::Enum, oEnum, p->m_specializers)
    {
        Val const klass = oEnum.Get();
        write_char(sep, s);
        print_object(class_name(klass), s);
        sep = Character::FromCode(Space);
    } // for each class

    write_string(")>", s);
    return x;
} // print_object standard_method

// [S]
defun(slot_value, (Val inst, Val name))
{
    check_type(name, symbol);
    Val klass  = class_of(inst);
    Val eslotd = find_slot(klass, name);
    if (nil == eslotd)
    {
        funcall(Qslot_missing, klass, inst, name, Qslot_value);
    }
    return funcall(Qslot_value_using_class, klass, inst, eslotd);
} // slot_value

defun_setf(slot_value, (Val newval, Val inst, Val name))
{
    check_type(name, symbol);
    Val klass  = class_of(inst);
    Val eslotd = find_slot(klass, name);
    if (nil == eslotd)
    {
        funcall(Qslot_missing, klass, inst, name, Qslot_value);
    }

    Val writer = SETF_slot_value_using_class->
        StaticCast<SetfCell>()->m_function;

    return funcall(writer, newval, klass, inst, eslotd);
} // slot_value

defmethod(slot_value_using_class, funcallable_standard_class,
                (Val, Val inst, Val eslotd) )
{
    Layout_effective_slot_definition* pESlotD =
        eslotd->StaticCast<Instance>()->
            GetStorage<Layout_effective_slot_definition>();

    Int  iLoc = Fixnum::Decode_(pESlotD->m_location);

    Val* pStorage = inst->StaticCast<FuncallableInstance>()->
        GetStorage<Storage>()->GetStart();

    return pStorage[iLoc];
} // slot_value_using_class

defmethod_setf(slot_value_using_class, funcallable_standard_class,
                (Val newval, Val, Val inst, Val eslotd) )
{
    Layout_effective_slot_definition* pESlotD =
        eslotd->StaticCast<Instance>()->
            GetStorage<Layout_effective_slot_definition>();

    Int  iLoc = Fixnum::Decode_(pESlotD->m_location);

    Val* pStorage = inst->StaticCast<FuncallableInstance>()->
        GetStorage<Storage>()->GetStart();

    return pStorage[iLoc] = newval;
} // slot_value_using_class

defmethod(slot_value_using_class, standard_class,
                (Val, Val inst, Val eslotd) )
{
    Layout_effective_slot_definition* pESlotD =
        eslotd->StaticCast<Instance>()->
            GetStorage<Layout_effective_slot_definition>();

    Int  iLoc = Fixnum::Decode_(pESlotD->m_location);

    Val* pStorage = inst->StaticCast<Instance>()->
        GetStorage<Storage>()->GetStart();

    return pStorage[iLoc];
} // slot_value_using_class

defmethod_setf(slot_value_using_class, standard_class,
                (Val newval, Val, Val inst, Val eslotd) )
{
    Layout_effective_slot_definition* pESlotD =
        eslotd->StaticCast<Instance>()->
            GetStorage<Layout_effective_slot_definition>();

    Int  iLoc = Fixnum::Decode_(pESlotD->m_location);

    Val* pStorage = inst->StaticCast<Instance>()->
        GetStorage<Storage>()->GetStart();

    return pStorage[iLoc] = newval;
} // slot_value_using_class

defmethod(slot_value_using_class, structure_class,
                (Val, Val inst, Val eslotd) )
{
    Layout_effective_slot_definition* pESlotD =
        eslotd->StaticCast<Instance>()->
            GetStorage<Layout_effective_slot_definition>();

    Int  iLoc = Fixnum::Decode_(pESlotD->m_location);
    Val* pStorage = inst->StaticCast<Record>()->To<Val>();
    return pStorage[iLoc];
} // slot_value_using_class

defmethod_setf(slot_value_using_class, structure_class,
                (Val newval, Val, Val inst, Val eslotd) )
{
    Layout_effective_slot_definition* pESlotD =
        eslotd->StaticCast<Instance>()->
            GetStorage<Layout_effective_slot_definition>();

    Int  iLoc = Fixnum::Decode_(pESlotD->m_location);
    Val* pStorage = inst->StaticCast<Record>()->To<Val>();
    return pStorage[iLoc] = newval;
} // slot_value_using_class

// [S]

defpred(subclassp, (Val const class1, Val const class2))
{
    //check_type(class1, class);
    //check_type(class2, class);

    if (class1 == class2)
    {
        return true;
    }

    Layout_class* const pClass1 = class1->StaticCast<Instance>()->
        GetStorage<Layout_class>();

    Val const cpl1 = pClass1->m_class_precedence_list;
    if (nil != cpl1)
    {
        return nil != memq(class2, cpl1);
    }

    foreach (List::Enum, oEnum, pClass1->m_direct_superclasses)
    {
        if (subclassp(oEnum.Get(), class2))
        {
            return true;
        }
    } // for each super

    return false;
} // subclassp

} // TinyCl
