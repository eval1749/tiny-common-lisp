#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime
// tinycl_rtl2.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl2.cpp#5 $
//
#include "../tinycl.h"
#include "../tinycl_clos.h"

namespace TinyCl
{

namespace Private
{
static bool checkSituation(Val situations)
{
    foreach (List::Enum, oEnum, situations)
    {
        Val situation = oEnum.Get();
        when (Kexecute == situation) return true;
        when (Qeval    == situation) return true;

        if (Kload_toplevel == situation ||
            Qload          == situation )
        {
            return nil != TLV(Aload_pathnameA);
        }
    } // for
    return false;
} // checkSituation

static Val compileAndEval(Val form)
{
    Val fn = compile_form(form);
    if (! functionp(fn))
    {
        return values(nil);
    }

    return funcall(fn);
} // compileAndEval

static Val ensureTopLevelEnv(Val env)
{
    if (nil == env)
    {
        env = TLV(AenvironmentA);
    }

    for (;;)
    {
        check_type(env, environment);
        Environment* pEnv = env->StaticCast<Environment>();
        if (nil != pEnv->m_classes)
        {
            return env;
        }
        env = pEnv->m_outer;
    } // for
} // ensureTopLevelEnv

static Val evalForms(Val forms)
{
    if (nil == forms) return values(nil);

    Val val = nil;
    foreach (List::Enum, oEnum, forms)
    {
        val = eval(oEnum.Get());
    } // for each elt
    return val;
} // evalForms

static Val getVarInfo(Val name)
{
    check_type(name, symbol);

    Val vartab = TLV(AenvironmentA)->StaticCast<Environment>()->
        m_variables;

    return gethash(name, vartab);
} // getVarInfo

} // Private

using namespace Private;

// [%]
defun(Pdefmacro, (Val fname, Val, Val fn))
{
    setf_macro_function(fn, fname);
    return fname;
} // Pdefmacro

defun(Pdefun, (Val fname, Val, Val fn))
{
    // FIXME 2007-11-23 yosi@msn.com Keep traced function traced.
    setf_fdefinition(fn, fname);
    return fname;
} // Pdefun

// [F]
Val funcall_(Val fn)
{
    if (symbolp(fn)) fn = symbol_function(fn);
    unless (functionp(fn)) SignalTypeError(fn, Qfunction);
    Thread* pt = Thread::Get();
    pt->m_fn = fn;
    return CallLisp(pt);
} // funcall_

defun(function_information, (Val const fname, Val const envd))
{
    Val env = nil == envd ? TLV(AenvironmentA) : envd;

    Val key;
    if (symbolp(fname))
    {
        key = fname;
    }
    else if (setf_cell_p(fname))
    {
        key = fname;
    }
    else if (function_name_p(fname))
    {
        key = intern_setf_cell(cadr(fname));
    }
    else
    {
        SignalTypeError(fname, Qfunction_name);
    }

    for (;;)
    {
        check_type(env, environment);
        Environment* const pEnv = env->StaticCast<Environment>();
        Val const frob = gethash(key, pEnv->m_functions);
        if (nil != frob)
        {
            return values(
                car(frob),
                nil == pEnv->m_classes ? t : nil,
                cdr(frob) );
        }

        env = pEnv->m_outer;
        if (nil == env)
        {
            return values(nil, nil, nil);
        }
    } // for
} // function_information

// [V]
defun(variable_information, (Val const name, Val const envd))
{
    check_type(name, symbol);

    if (name->StaticCast<Symbol>()->m_package == PKG_keyword)
    {
        return values(Kconstant, nil, list(cons(Kconstant, name)));
    }

    Val env = nil == envd ? TLV(AenvironmentA) : envd;

    for (;;)
    {
        check_type(env, environment);
        Environment* const pEnv = env->StaticCast<Environment>();
        Val const frob = gethash(name, pEnv->m_variables);
        if (nil != frob)
        {
            return values(
                car(frob),
                nil == pEnv->m_classes ? t : nil,
                cdr(frob) );
        }

        env = pEnv->m_outer;
        if (nil == env)
        {
            return values(nil, nil, nil);
        }
    } // for
} // variable_information

namespace CommonLisp
{

using namespace TinyCl;

// [A]
defun(appendV, (Thread* pt))
{
    Val anchor = nil;
    Val* pTail = &anchor;
    Thread::EnumValue oEnumValue(pt);
    while (! oEnumValue.AtEnd())
    {
        Val obj = oEnumValue.Get();
        oEnumValue.Next();
        if (oEnumValue.AtEnd())
        {
            *pTail = obj;
            break;
        }

        check_type(obj, list);

        foreach (List::Enum, oEnum, obj)
        {
            Val cons = list(oEnum.Get());
            *pTail = cons;
            pTail = &cons->StaticCast<Cons>()->m_cdr;
        } // for each elt
    } // for each value
    return anchor;
} // appendV

defun(apply, (Val fn, Val args))
{
    values_list(args);
    return funcall_(fn);
} // apply

// [C]

defun(class_name, (Val klass))
{
    check_type(klass, class);
    return klass->StaticCast<Instance>()->GetStorage<Layout_class>()->m_name;
} // class_name

defun(class_of, (Val x))
{
    return classd_of(x)->StaticCast<ClassD>()->m_class;
} // class_of

defpred(constantp, (Val form, Val))
{
    if (nil == form)
    {
        return true;
    }

    if (Symbol* p = form->DynamicCast<Symbol>())
    {
        if (p->m_package == PKG_keyword)
        {
            return true;
        }

        Val vartab = TLV(AenvironmentA)->StaticCast<Environment>()->
            m_variables;

        Val entry = gethash(form, vartab);
        return car(entry) == Kconstant;
    }

    if (consp(form))
    {
        return car(form) == Qquote;
    }

    return true;
} // constantp

// [D]
defun(describe, (Val obj))
{
    format(t, "; ~S is class of ~S.~%", obj, class_name(class_of(obj)));

    ClassD* pClassD = classd_of(obj)->StaticCast<ClassD>();

    if (Instance* p = obj->DynamicCast<Instance>())
    {
        Val* pSlot = p->GetStorage<Val>() + 2;

        format(t, ";~%");

        foreach (List::Enum, oEnum, pClassD->m_slots)
        {
            Layout_effective_slot_definition* pESlotD =
                oEnum.Get()->StaticCast<Instance>()->
                    GetStorage<Layout_effective_slot_definition>();

            format(t, ";   [~D] ~S = ~S~%",
                pESlotD->m_location,
                pESlotD->m_name,
                pSlot[Fixnum::Decode_(pESlotD->m_location)] );
        } // for each eslotd

        format(t, ";~%");

        return obj;
    } // if instance

    if (Record* p = obj->DynamicCast<Record>())
    {
        Val* pSlot = reinterpret_cast<Val*>(p);

        foreach (List::Enum, oEnum, pClassD->m_slots)
        {
            Layout_effective_slot_definition* pESlotD =
                oEnum.Get()->StaticCast<Instance>()->
                    GetStorage<Layout_effective_slot_definition>();

            format(t, "; [~D] ~S = ~S~%",
                pESlotD->m_location,
                pESlotD->m_name,
                pSlot[Fixnum::Decode_(pESlotD->m_location)] );
        } // for each eslotd
        return obj;
    } // if record

    if (GenericFunction* p = obj->DynamicCast<GenericFunction>())
    {
        Val* pSlot = reinterpret_cast<Val*>(p)  + 2;

        format(t, ";~%");

        foreach (List::Enum, oEnum, pClassD->m_slots)
        {
            Layout_effective_slot_definition* pESlotD =
                oEnum.Get()->StaticCast<Instance>()->
                    GetStorage<Layout_effective_slot_definition>();

            format(t, ";   [~D] ~S = ~S~%",
                pESlotD->m_location,
                pESlotD->m_name,
                pSlot[Fixnum::Decode_(pESlotD->m_location)] );
        } // for each eslotd

        format(t, ";~%");

        return obj;
    } // if generic function

    return obj;
} // describe

// [E]

defun(eval, (Val form))
{
    if (symbolp(form))
    {
        Val entry = getVarInfo(form);
        if (car(entry) == Ksymbol_macro)
        {
            Val expansion = cdr(assq(Ksymbol_macro, cdr(entry)));
            return eval(expansion);
        }

        return values(symbol_value(form));
    } // if symbol

    unless (consp(form)) return values(form);

    Val op = car(form);

    unless (symbolp(op)) { error("Invalid form: ~S", form); }

    if (op == Qeval_when)
    {
        if (checkSituation(cadr(form)))
        {
            return evalForms(cddr(form));
        }
        else
        {
            return values(nil);
        }
    } // if eval_when

    if (op == Qif)
    {
        if (nil != eval(cadr(form)))
        {
            return eval(car(cddr(form)));
        }
        else
        {
            return eval(cadr(cddr(form)));
        }
    } // if if

    if (op == Qfunction)
    {
        Val fname = cadr(form);
        if (consp(fname) && Qlambda == car(fname))
        {
            return compileAndEval(form);
        }

        return values(fdefinition(fname));
    } // if function

    if (Qprogn == op)
    {
        return evalForms(cdr(form));
    } // if progn

    if (Qquote == op)
    {
        return values(cadr(form));
    } // if quote


    if (Qsetq == op)
    {
        Val value = nil;
        foreach (List::Enum, oEnum, cdr(form))
        {
            Val name = oEnum.Get();
            oEnum.Next();

            if (oEnum.AtEnd())
            {
                error("Missing value form for setq ~S.", name);
            }

            value = oEnum.Get();

            Val entry = getVarInfo(name);

            if (car(entry) == Ksymbol_macro)
            {
                Val expansion = cdr(assq(Ksymbol_macro, cdr(entry)));
                eval(list(Qsetf, expansion, value));
            }
            else
            {
                value = eval(value);
                setf_symbol_value(value, name);
            }
        } // for each elt
        return values(value);
    } // if setq

    {
        Val funtab = TLV(AenvironmentA)->StaticCast<Environment>()->
            m_functions;

        Val entry = gethash(op, funtab);
        Val kind  = car(entry);

        if (Kspecial_operator == kind)
        {
            return compileAndEval(form);
        }

        if (Kmacro == kind)
        {
            Val expander = cdr(assq(Kmacro, cdr(entry)));
            return eval(funcall(expander, form, nil));
        }
    }

    Val head = list(nil);
    Val tail = head;
    foreach (List::Enum, oEnum, cdr(form))
    {
        Val val = eval(oEnum.Get());
        tail = setf_cdr(list(val), tail);
    } // for each elt

    return apply(op, cdr(head));
} // eval

// [F]
defun(funcall, (Val fn))
    { values(); return funcall_(fn); }

defun(funcall, (Val fn, Val a))
    { values(a); return funcall_(fn); }

defun(funcall, (Val fn, Val a, Val b))
    { values(a, b); return funcall_(fn); }

defun(funcall, (Val fn, Val a, Val b, Val c))
    { values(a, b, c); return funcall_(fn); }

defun(funcall, (Val fn, Val a, Val b, Val c, Val d))
    { values(a, b, c, d); return funcall_(fn); }

defun(funcall, (Val fn, Val a, Val b, Val c, Val d, Val e))
    { values(a, b, c, d, e); return funcall_(fn); }

defun(funcall, (Val fn, Val a, Val b, Val c, Val d, Val e, Val f))
    { values(a, b, c, d, e, f); return funcall_(fn); }

defun(funcall, (Val fn, Val a, Val b, Val c, Val d, Val e, Val f, Val g))
    { values(a, b, c, d, e, f, g); return funcall_(fn); }

// [G]
defun(gensym, (Val x))
{
    if (integerp(x))
    {
        return make_symbol(format(nil, "G~D", x));
    }

    if (MARKER_unbound == x)
    {
        x = make_string("G");
    }

    if (stringp(x))
    {
        Val counter = TLV(Agensym_counterA);
        TLV(Agensym_counterA) = add(counter, one);
        return make_symbol(format(nil, "~A~D", x, counter));
    }

    SignalTypeError(x, list(Qor, Qsymbol, Qunsigned_byte));
} // gensym

defun(get_setf_expansion, (Val place, Val env))
{
    class Expander
    {
        private: static Val expandCons(Val place, Val env)
        {
            for (;;)
            {
                Val kind = function_information(car(place), env);
                Val localp = Thread::Get()->mv_value[1];
                Val alist  = Thread::Get()->mv_value[2];

                if (nil != localp)
                {
                    if (nil == kind || Kfunction == kind)
                    {
                        // flet or labels
                        return expandFunPlace(place);
                    }

                    if (Kmacro == kind)
                    {
                        // macrolet
                        Val expander = cdr(assq(Kmacro, alist));
                        place = funcall(
                            TLV(Amacroexpand_hookA),
                            expander,
                            place,
                            env );
                        continue;
                    }
                }
                else
                {
                    Val setfname = intern_setf_cell(car(place));
                    kind = function_information(setfname, env);
                    alist  = Thread::Get()->mv_value[2];

                    if (Kmacro == kind)
                    {
                        // defsetf
                        Val expander = cdr(assq(Kmacro, alist));
                        return funcall(
                            TLV(Amacroexpand_hookA),
                            expander,
                            place,
                            env );
                    }

                    if (Kfunction == kind || nil == kind)
                    {
                        return expandFunPlace(place);
                    }
                } // if

                error("Invalid setf place ~S", place);
            } // for
        } // expandCons

        private: static Val expandFunPlace(Val place)
        {
            Val args = nil;
            Val vals = nil;
            Val vars = nil;
            Val reader = car(place);

            foreach (List::Enum, oEnum, cdr(place))
            {
                Val arg = oEnum.Get();
                if (needSave(arg))
                {
                    push(arg, vals);
                    arg = gensym();
                    push(arg, vars);
                }
                push(arg, args);
            } // for each arg

            args = nreverse(args);
            vals = nreverse(vals);
            vars = nreverse(vars);

            Val newval = gensym();
            return values(
                vars,
                vals,
                list(newval),
                listA(Qfuncall,
                    list(Qfunction, list(Qsetf, reader)),
                    newval,
                    args ),
                cons(reader, args) );
        } // expandFunPlace

        private: static Val expandSymbol(Val symb, Val env)
        {
            Environment* pEnv = env->StaticCast<Environment>();
            Val frob = gethash(symb, pEnv->m_variables);
            Val kind = car(frob);
            if (nil == kind || Klexical == kind || Kspecial == kind)
            {
                Val newvar = gensym();
                return values(
                    nil,
                    nil,
                    list(newvar),
                    list(Qsetq, symb, newvar),
                    symb );
            }

            if (Ksymbol_macro == kind)
            {
                Val expansion = cdr(assq(Ksymbol_macro, cdr(frob)));
                return Run(expansion, env);
            }

            error("Can't alter constant ~S.", symb);
        } // expandSymbol

        private: static bool needSave(Val form)
        {
            if (consp(form) || symbolp(form))
            {
                return ! constantp(form);
            }

            return false;
        } // needSave

        public: static Val Run(Val place, Val env)
        {
            if (nil == env)
            {
                env = TLV(AenvironmentA);
            }

            check_type(env, environment);

            if (symbolp(place))
            {
                return expandSymbol(place, env);
            }

            if (consp(place))
            {
                return expandCons(place, env);
            }

            error("Invalid setf place: ~S", place);
        } // Run
    }; // Expander

    return Expander::Run(place, env);
} // get_setf_expansion

// [L]
defun(last, (Val x, Val n))
{
    check_type(x, list);
    Val i = zero;
    for (Val y = x; consp(y); y = cdr(y))
    {
        if (ge(i, n)) x = cdr(x);
        i = add(i, 1);
    } // for elt
    return x;
} // last

// [M]
Val macro_function(Val name, Val env)
{
    check_type(name, symbol);

    if (nil == env)
    {
        env = TLV(AenvironmentA);
    }

    check_type(env, environment);

    Environment* pEnv = env->StaticCast<Environment>();
    Val frob = gethash(name, pEnv->m_functions);
    return cdr(assq(Kmacro, cdr(frob)));
} // macro_function

Val macroexpand_1(Val form, Val env)
{
    if (nil == env) env = TLV(AenvironmentA);
    check_type(env, environment);

    if (consp(form))
    {
        Val expander =
            symbolp(car(form)) ? macro_function(car(form), env) : nil;
        if (nil != expander)
        {
            Val expansion = funcall(expander, form, env);
            return values(expansion, expander);
        }
    }
    else if (symbolp(form))
    {
        Val vartab = env->StaticCast<Environment>()->m_variables;
        Val frob = gethash(form, vartab);
        if (car(frob) == Ksymbol_macro)
        {
            Val kons = assq(Ksymbol_macro, cdr(frob));
            return values(cdr(kons), kons);
        }
    }

    return values(form, nil);
} // macroexpand_1

Val setf_macro_function(Val expander, Val name, Val env)
{
    check_type(expander, function);
    check_type(name, symbol);

    Environment* pEnv = ensureTopLevelEnv(env)->StaticCast<Environment>();
    Val frob = gethash(name, pEnv->m_functions);
    if (nil == frob)
    {
        setf_gethash(
            list(Kmacro, cons(Kmacro, expander)),
            name,
            pEnv->m_functions );
    }
    else
    {
        setf_car(Kmacro, frob);
        Val pair = assq(Kmacro, cdr(frob));
        if (consp(pair))
        {
            setf_cdr(expander, pair);
        }
        else
        {
            setf_cdr(list(cons(Kmacro, expander)), frob);
        }
    }

    setf_symbol_function(
        make_not_function_function(name),
        name );

    return expander;
} // macro_function

// [R]
Val room(Val x)
{
    class ObjStat
    {
        private: Val m_htb;

        public: ObjStat() :
            m_htb(make_hash_table(Ktest, Qeq, Ksize, Fixnum::Encode(200))) {}

        public: void Run()
        {
            foreach (Mm::EnumArea, oEnum, Thread::Get())
            {
                Mm::Area* pArea = oEnum.Get();
                switch (pArea->GetScanType())
                {
                case Mm::Area::ScanType_BinObj:
                case Mm::Area::ScanType_Record:
                    foreach (Mm::Area::EnumRecord, oEnum, pArea)
                    {
                        Record* pRecord = oEnum.Get();
                        ClassD* pClassD = pRecord->GetClassD();

                        addObject(
                            class_name(pClassD->m_class),
                            pRecord->GetSize(),
                            1 );
                    } // for each object
                    break;

                case Mm::Area::ScanType_Cons:
                    addObject(
                        Qcons,
                        pArea->m_ofsFree - sizeof(Mm::Area),
                        (pArea->m_ofsFree - sizeof(Mm::Area)) / sizeof(Cons) );
                    break;

                case Mm::Area::ScanType_Code:
                    foreach (Mm::Area::EnumCode, oEnum, pArea)
                    {
                        CodeObject* pCodeObj = oEnum.Get();
                        ClassD* pClassD = pCodeObj->GetClassD();

                        addObject(
                            class_name(pClassD->m_class),
                            pCodeObj->GetSize(),
                            1 );
                    } // for each object
                    break;
                } // switch type
            } // for area
        } // Run

        // [A]
        private: void addObject(Val name, size_t cbObjects, uint cObjects)
        {
            Val count_size = gethash(name, m_htb);
            if (nil == count_size)
            {
                count_size = cons(zero, zero);
                setf_gethash(count_size, name, m_htb);
            }
            setf_car(add(car(count_size), cbObjects), count_size);
            setf_cdr(add(cdr(count_size), cObjects),  count_size);
        } // addObject

        // [P]
        public: void Print()
        {
            Val count = zero;
            Val size  = zero;
            foreach (HashTable::Enum, oEnum, m_htb)
            {
                Val name = oEnum.GetKey();
                Val count_size = oEnum.GetVal();
                format(t, ";  ~12:D  ~10:D    ~S~%",
                    cdr(count_size),
                    car(count_size),
                    name );
                count = add(count, car(count_size));
                size  = add(size,  cdr(count_size));
            } // for each entry

            format(t, ";  ------------+-----------+----------------------~%");
            format(t, ";  ~12:D  ~10:D    Total~%", size, count);
        } // show
    }; // ObjStat

    ObjStat oObjStat;
    oObjStat.Run();

    format(t, "; Heap ~X~X...~X~X...~X~X",
        Fixnum::Encode(Mm::GetStart()->ToInt() >> 4),
        Fixnum::Encode(Mm::GetStart()->ToInt() & 15),

        Fixnum::Encode(Mm::GetCommit()->ToInt() >> 4),
        Fixnum::Encode(Mm::GetCommit()->ToInt() & 15),

        Fixnum::Encode(Mm::GetEnd()->ToInt() >> 4),
        Fixnum::Encode(Mm::GetEnd()->ToInt() & 15) );

    format(t, " ~:D/~:D KB~%",
        Fixnum::Encode(
            (Mm::GetCommit()->ToInt() - Mm::GetStart()->ToInt()) /
            1024 ),

        Fixnum::Encode(
            (Mm::GetEnd()->ToInt() - Mm::GetStart()->ToInt()) /
            1024 ) );

    format(t, ";~%; Objects:~%");
    oObjStat.Print();

    if (t == x)
    {
        format(t, ";~%; Area:~%");
        foreach (Mm::EnumArea, oEnum, Thread::Get())
        {
            const Mm::Area* pArea = oEnum.Get();

            format(t, ";   ~X~X ~2D ~A ~10:D/~10:D~%",
                Fixnum::Encode(pArea->ToInt() >> 4),
                Fixnum::Encode(pArea->ToInt() & 15),
                Fixnum::Encode(pArea->m_nFlags & Mm::Area::Flags_AgeMask),
                make_string(pArea->GetString()),
                Fixnum::Encode(pArea->m_ofsFree),
                Fixnum::Encode(pArea->m_cbArea) );
        } // for each area
    }

    format(t, ";~%");

    return nil;
} // room

// [S]

defun_setf(fdefinition, (Val fn, Val fname))
{
    check_type(fn, function);

    Val cell;
    if (symbolp(fname))
    {
        cell = fname;
        cell->StaticCast<Symbol>()->m_function = fn;
    }
    else if (function_name_p(fname))
    {
        cell = intern_setf_cell(cadr(fname));
        cell->StaticCast<SetfCell>()->m_function = fn;
    }
    else
    {
        SignalTypeError(fname, Qfunction_name);
    }

    return update_callers(cell, fn);
} // fdefinition

defun_setf(symbol_function, (Val fn, Val name))
{
    check_type(fn, function);
    check_type(name, symbol);
    name->StaticCast<Symbol>()->m_function = fn;
    return update_callers(name, fn);
} // symbol_function

defun_setf(symbol_value, (Val value, Val name))
{
    Val entry = getVarInfo(name);
    if (car(entry) == Kconstant)
    {
        error("Can't alter constant ~S.", name);
    }

    if (car(entry) == Ksymbol_macro)
    {
        error("Can't set value of symbol-macro ~S.", name);
    }

    Val cell = intern_value_cell(name, Kspecial);
    if (ValueCell* p = cell->DynamicCast<ValueCell>())
    {
        return p->m_value = value;
    }
    else if (TlvRecord* p = cell->DynamicCast<TlvRecord>())
    {
        Int iIndex = Fixnum::Decode_(p->m_index);
        return *Thread::TlvPtr_(iIndex) = value;
    }

    error("Broken environment for ~S.", name);
} // setf_symbol_value

} // CommonLisp
} // TinyCl
