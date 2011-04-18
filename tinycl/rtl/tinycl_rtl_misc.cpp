#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Miscellaneous
// tinycl_rtl_misc.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_misc.cpp#8 $
//
#define DEBUG_HASH_TABLE 1
#include "../tinycl.h"

#include "../arch/generic/tinycl_gen.h"

namespace TinyCl
{

using namespace Generic;
using namespace Private;

namespace
{

// EnumSybmol - Enumrates symbols in package vector.
class EnumSymbol
{
    private: Int    m_iCount;
    private: Val*   m_px;

    public: EnumSymbol(Val vec)
    {
        SimpleVector* p = vec->StaticCast<SimpleVector>();
        m_iCount = Fixnum::Decode_(p->GetStart()[0]);
        m_px     = p->GetStart();
        if (0 == m_iCount)
        {
            m_iCount = -1;
        }
        else
        {
            next();
        }
    } // EnumSymbol

    public: bool AtEnd() const { return m_iCount < 0; }
    public: Val  Get()   const { ASSERT(!AtEnd()); return *m_px; }
    public: void Next()        { ASSERT(!AtEnd()); next(); }

    private: void next()
    {
        m_iCount -= 1;
        for (;;)
        {
            m_px++;
            if (symbolp(*m_px))
            {
                return;
            }
        } // for
    } // next
}; // EnumSymbol

// valuesToList
static Val valuesToList(int iStart)
{
    Thread* p = Thread::Get();
    int iEnd = static_cast<int>(Fixnum::Decode_(p->m_n));
    Val runner = nil;
    for (int i = iEnd - 1; i >= iStart; i -= 1)
    {
        runner = cons(p->mv_value[i], runner);
    } // for i
    return runner;
} // valuesToList

} // namespace

/// <summary>
///   Put specified key-value pair into specified entry.
/// </summary>
void HashTable::Put(Entry* pEntry, Val key, Val val)
{
    ASSERT(pEntry >= GetTop());
    ASSERT(pEntry <  GetBtm());

    pEntry->Set(key, val);

    SimpleVector* p = m_vector->StaticCast<SimpleVector>();
    p->GetStart()[0] = xxadd(GetCount(), one);
} // HashTable::Put

/// <summary>
///   Remove specified entry.
/// </summary>
void HashTable::Remove(Entry* pEntry)
{
    ASSERT(pEntry >= GetTop());
    ASSERT(pEntry <  GetBtm());

    if (computeNextEntry(pEntry)->IsEmpty() &&
        computePrevEntry(pEntry)->IsEmpty() )
    {
        pEntry->MarkEmpty();
    }
    else
    {
        pEntry->MarkRemoved();
    }

    SimpleVector* p = m_vector->StaticCast<SimpleVector>();
    p->GetStart()[0] = xxsub(GetCount(), one);
} // HashTable::Remove

Val StringObject::GetData(Val* out_offset)
{
    *out_offset = zero;

    StringObject* p = this;
    for (;;)
    {
        Val displaced = p->m_displaced_to;
        if (displaced->Is<SimpleString>())
        {
            return displaced;
        }

        if (! displaced->Is<StringObject>())
        {
            error("Broken string object ~S", p->Encode());
        }

        *out_offset = xxadd(*out_offset, p->m_offset);
        p = displaced->StaticCast<StringObject>();
    } // for
} // StringObject::GetData

char16* StringObject::GetStart() const
{
    Int iOffset = 0;
    const StringObject* p = this;
    for (;;)
    {
        Val displaced = p->m_displaced_to;
        if (displaced->Is<SimpleString>())
        {
            SimpleString* q= displaced->StaticCast<SimpleString>();
            return q->GetStart() + iOffset;
        }

        if (! displaced->Is<StringObject>())
        {
            error("Broken string object ~S", p->Encode());
        }

        iOffset += Fixnum::Decode_(p->m_offset);
        p = displaced->StaticCast<StringObject>();
    } // for
} // StringObject::GetStart

// [%]
defun(Pdeftlv, (Val name, Val, Val initval, Val))
{
    Val tlvrec = Thread::Get()->AllocRecord(CLASSD_tlv_record);

    Val index = VAR(Atlv_indexA);

    VAR(Atlv_indexA) = add(index, 1);

    TlvRecord* p = tlvrec->StaticCast<TlvRecord>();

    p->m_classd = CLASSD_tlv_record;
    p->m_name   = name;
    p->m_index  = index;
    p->m_value  = initval;
    setf_gethash(tlvrec, name, VAR(Avalue_tableA));

    setf_svref(tlvrec, VAR(Atlv_vectorA), index);

    Val frob = cons(Kspecial, list(cons(Kspecial, tlvrec)));
    Val vartab = TLV(AenvironmentA)->StaticCast<Environment>()->m_variables;
    setf_gethash(frob, name, vartab);

    *Thread::TlvPtr_(Fixnum::Decode_(index)) = initval;

    return tlvrec;
} // Pdeftlv

defun(Pdefvar, (Val name, Val initval, Val))
{
    check_type(name, symbol);
    Val cell = intern_value_cell(name, Kspecial);
    ValueCell* p = cell->StaticCast<ValueCell>();
    if (MARKER_unbound == p->m_value)
    {
        p->m_value = initval;
    }
    // Note: return value of %defvar is value-cell instead of symbol for
    // easy access to value-cell at application initialization.
    return cell;
} // Pdefvar

// [A]
defun(assq, (Val key, Val alist))
{
    foreach (List::Enum, oEnum, alist)
    {
        Val key_val = oEnum.Get();
        if (car(key_val) == key) return key_val;
    }
    return nil;
} // assq

// [C]
defun(classd_of, (Val x))
{
    switch (x->GetTag())
    {
    case Arch::Tag_Cons:
        return CLASSD_cons;

    case Arch::Tag_Fixnum:
    case Arch::Tag_Fixnum1:
        return CLASSD_fixnum;

    case Arch::Tag_Null:
        return CLASSD_null;

    case Arch::Tag_Record:
        return x->StaticCast<Record>()->m_classd;

    case Arch::Tag_FunObj:
        return x->StaticCast<Function>()->m_classd;
    } // tag

    error("Broken object ~S", MakeUInt(x->ToInt()));
} // classd_of

defpred(classp, (Val x))
{
    if (Instance* p = x->DynamicCast<Instance>())
    {
        return nil != memq(
            CLASS_class,
            p->m_classd->StaticCast<ClassD>()->m_class_precedence_list );
    }
    return false;
} // classp

defun(cl_length, (Val x))
{
    if (vectorp(x))
    {
        return x->StaticCast<Vector>()->m_length;
    } // if vector

    if (nil == x)
    {
        return zero;
    } // if null

    if (consp(x))
    {
        Val n = zero;
        for (;;)
        {
            n = add(n, 1);
            x = cdr(x);
            if (endp(x))
            {
                return n;
            }
        } // for
    } // if cons

    SignalTypeError(x, Qsequence);
} // cl_length

defun(coerce_to_condition, (Val klass, Val frob, Val args))
{
    format(t, "coerce_to_condtion: ~S ~S ~S~%", klass, frob, args);
    force_output();

    if (typep(frob, CLASS_condition))
    {
        return frob;
    }

    if (simple_string_p(frob))
    {
        args = list(
            Kformat_control, frob,
            Kformat_arguments, args );

        frob = klass;
    }

    if (symbolp(frob))
    {
        klass = find_class(frob);
    }
    else if (classp(frob))
    {
        klass = frob;
    }
    else
    {
        SignalTypeError(frob, list(Qor, Qcondition, Qstring));
    }

    if (! subclassp(klass, CLASS_condition))
    {
        error("~S must be a subclass of ~S.", klass, CLASS_condition);
    }

    Val const classd = klass->StaticCast<Instance>()->
        GetStorage<Layout_class>()->m_instanced;

    Val const cond = Thread::Get()->AllocInstance(classd);

    ClassD* const pClassD = classd->StaticCast<ClassD>();

    Val* const pStorage = cond->StaticCast<Instance>()->
        GetStorage<Storage>()->GetStart();

    foreach (List::Enum, oEnum, pClassD->m_slots)
    {
        Layout_effective_slot_definition* const pESlotD =
            oEnum.Get()->StaticCast<Instance>()->
                GetStorage<Layout_effective_slot_definition>();

        Int const iLoc = Fixnum::Decode_(pESlotD->m_location);

        Val initarg = nil;
        foreach (List::Enum, oEnum, pESlotD->m_initargs)
        {
            initarg = memq(oEnum.Get(), args);
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
        else
        {
            val = MARKER_unbound;
        }

        pStorage[iLoc] = val;
    } // for each eslotd

    return cond;
} // coerce_to_condition

// [D]

defun(delq, (Val item, Val list))
{
    Val prev = nil;
    foreach (List::Enum, oEnum, list)
    {
        Val cons = oEnum.GetCons();

        if (oEnum.Get() == item)
        {
            Val rest = cdr(cons);
            if (nil == prev)
            {
                return rest;
            }

            setf_cdr(rest, cons);
            return list;
        } // if

        prev = cons;
    } // for each bufer

    return list;
} // delq

// [E]
/// <summary>
///   Returns package object of specified package designator.
/// </summary>
/// <param name="pkgd">A package designator</param>
/// <returns>A package object</returns>
Val ensure_package(Val pkgd)
{
    if (pkgd->Is<Package>())
    {
        return pkgd;
    }

    Val const pkg = find_package(pkgd);
    if (nil == pkg)
    {
        error("No such package ~S.", pkgd);
    }
    return pkg;
} // ensure_package

// [F]
defun(find_setf_cell, (Val symb))
{
    check_type(symb, symbol);
    return gethash(symb, VAR(Asetf_tableA));
} // find_setf_cell

defun(find_value_cell, (Val symb))
{
    check_type(symb, symbol);
    return gethash(symb, VAR(Avalue_tableA));
} // find_value_cell

defpred(function_name_p, (Val x))
{
    if (symbolp(x)) return true;

    return
        consp(x) &&
        Qsetf == car(x) &&
        consp(cdr(x)) &&
        symbolp(cadr(x)) &&
        nil == cddr(x);
} // function_name_p

// [G]
static bool htb_need_rehash_p(Val);
static void htb_put(Val, Val, Val);
static void htb_rehash(Val);
static Val  htb_sxhash(Val, Val);

static bool htb_need_rehash_p(Val htb)
{
    Val vec       = htb->StaticCast<HashTable>()->m_vector;
    Val count     = svref(vec, zero);
    Val threshold = svref(vec, one);
    return ge(count, threshold);
} // htb_need_rehash_p

static void htb_put(Val htb, Val key, Val val)
{
    ASSERT(MARKER_free != key);
    ASSERT(MARKER_removed != key);

    HashTable::EnumAll::Arg oArg(
        htb->StaticCast<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    foreach (HashTable::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.GetKey();
        ASSERT(key != present);
        if (MARKER_free == present)
        {
            oEnum.SetKey(key);
            oEnum.SetVal(val);
            return;
        }
    } // for each slot
    error("Broken hash-table ~S.", htb);
} // htb_put

static void htb_rehash(Val htb)
{
    Val oldvec = htb->StaticCast<HashTable>()->m_vector;
    Val count  = svref(oldvec, zero);

    Val oldthreshold = svref(oldvec, one);
    Val oldsize      = sub(truncate(length(oldvec), 2), 2);

    Val rehash_size = htb->StaticCast<HashTable>()->m_rehash_size;

    Val newsize = truncate(mul(oldsize, rehash_size), 100);
    Val newvec  = make_vector(add(mul(newsize, 2), 2));

    Val newthreshold = truncate(mul(oldthreshold, rehash_size), 100);

    htb->StaticCast<HashTable>()->m_vector = newvec;

    #if DEBUG_HASH_TABLE
        DEBUG_FORMAT("size=~D=>~D thrshold=~D=>~D~%",
            oldsize, newsize,
            oldthreshold, newthreshold );
    #endif

    fill(newvec, MARKER_free);

    setf_svref(count,        newvec, zero);
    setf_svref(newthreshold, newvec, one);

    foreach (HashTable::EnumVec, oEnum, oldvec)
    {
        htb_put(htb, oEnum.GetKey(), oEnum.GetVal());
    } // for each slot
} // htb_rehash

static Val htb_sxhash(Val htb, Val x)
{
    HashTable* p = htb->StaticCast<HashTable>();

    if (p->m_test == Qeq)
    {
        return sxhash_eq(x);
    }

    if (p->m_test == Qeql)
    {
        return sxhash_eql(x);
    }

    if (p->m_test == Qequal)
    {
        return sxhash(x);
    }

    if (p->m_test == Qequalp)
    {
        return sxhash_equalp(x);
    }

    error("Unsupported hash-table ~S.", htb);
} // htb_test

static bool htb_test(Val htb, Val x, Val y)
{
    HashTable* p = htb->StaticCast<HashTable>();

    if (p->m_test == Qeq)
    {
        return x == y;
    }

    if (p->m_test == Qeql)
    {
        return eql(x, y);
    }

    if (p->m_test == Qequal)
    {
        return equal(x, y);
    }

    if (p->m_test == Qequalp)
    {
        return equalp(x, y);
    }

    error("Unsupported hash-table ~S.", htb);
} // htb_test

// [I]
defun(intern_setf_cell, (Val name))
{
    Val cell = find_setf_cell(name);
    if (nil == cell)
    {
        cell = Thread::Get()->AllocRecord(CLASSD_setf_cell);
        SetfCell* p = cell->StaticCast<SetfCell>();
        p->m_name     = name;
        p->m_function = nil;

        setf_gethash(cell, name, VAR(Asetf_tableA));
    }
    return cell;
} // intern_setf_cell

defun(intern_value_cell, (Val name, Val kind))
{
    Val cell = find_value_cell(name);
    if (nil == cell)
    {
        cell = Thread::Get()->AllocRecord(CLASSD_value_cell);
        ValueCell* p = cell->StaticCast<ValueCell>();
        p->m_name  = name;
        p->m_kind  = kind;
        p->m_value = MARKER_unbound;

        setf_gethash(cell, name, VAR(Avalue_tableA));
    }
    return cell;
} // intern_value_cell

static Val internal_package_get(Val vec, Val name)
{
    Val hc     = name->StaticCast<SimpleString>()->Hash();
    Val top    = one;
    Val btm    = length(vec);
    Val runner = add(rem(hc, sub(btm, top)), top);

    for (;;)
    {
        Val present = svref(vec, runner);
        if (zero == present) return nil;

        Symbol* p = present->DynamicCast<Symbol>();
        if (NULL != p && string_eq(p->m_name, name)) return runner;

        runner = add(runner, 1);
        if (runner == btm)
        {
            runner = top;
        }
    } // for
} // internal_package_get

// internal_package_put
Val internal_package_put(Val vec, Val sym)
{
    Val count = svref(vec, zero);
    if (gt(mul(count, 100), mul(length(vec), 60)))
    {
        Val newvec = make_vector(truncate(mul(length(vec), 130), 100));
        setf_svref(zero, newvec, zero);
        foreach (EnumSymbol, oEnum, vec)
        {
            newvec = internal_package_put(newvec, oEnum.Get());
        } // for each symbol
        vec = newvec;
    } // if

    Val hc     = sym->StaticCast<Symbol>()->m_hash_code;
    Val top    = one;
    Val btm    = length(vec);
    Val runner = add(rem(hc, sub(btm, top)), top);
    Val home   = zero;

    for (;;)
    {
        Val present = svref(vec, runner);
        if (zero == present)
        {
            if (zero == home)
            {
                home = runner;
            }

            setf_svref(sym, vec, home);
            setf_svref(add(count, 1), vec, zero);
            break;
        }

        if (sym == present)
        {
            break;
        }

        if (one == present)
        {
            if (zero == home)
            {
                home = runner;
            }
        }

        runner = add(runner, 1);
        if (runner == btm)
        {
            runner = top;
        }
    } // for

    return vec;
} // internal_package_put

// [M]
Val make_platform_error(const char* psz, uint nError)
{
    Val error = Thread::Get()->AllocInstance(CLASSD_platform_error);
    PlatformError* p = error->StaticCast<PlatformError>();
    p->m_operation = make_string(psz);
    p->m_code      = MakeUInt(nError);
    return error;
} // make_platform_error

defun(make_vector, (Val length))
{
    return Thread::Get()->AllocVector(
        CLASSD_simple_vector,
        length );
} // make_vector

defun(memq, (Val const item, Val const list))
{
    foreach (List::Enum, oEnum, list)
    {
        if (oEnum.Get() == item)
        {
            return oEnum.GetCons();
        }
    } // for
    return nil;
} // memq

defun(memv, (Val const item, Val const list))
{
    foreach (List::Enum, oEnum, list)
    {
        if (eql(oEnum.Get(), item))
        {
            return oEnum.GetCons();
        }
    } // for
    return nil;
} // memv

// [P]
void parseKeys(Thread* pth, Int iStart, const KeyArg* prgoKey, uint cKeys)
{
    Int iEnd = Fixnum::Decode_(pth->m_n);
    ASSERT(iEnd <= Arch::MultipleValuesLimit);

    bool fAllowOtherKeys = false;

    for (Int i = iStart; i < iEnd; i += 2)
    {
        Val key = pth->mv_value[i];
        if (Kallow_other_keys == key)
        {
            fAllowOtherKeys = true;
            break;
        }
    } // for i

    if ((iEnd - iStart) % 2)
    {
        if (fAllowOtherKeys)
        {
            pth->mv_value[iEnd] = nil;
        }
        else
        {
            Val args = pth->ValuesToList();
            for (Int i = 0; i < iStart; i++)
            {
                pop(args);
            } // for i

            error(Qodd_number_of_keyword_arguments,
                Karguments, args );
        }
    } // if odd

    for (Int i = iStart; i < iEnd; i += 2)
    {
        Val key = pth->mv_value[i];
        bool fFound = false;
        for (
            const KeyArg* pRunner = prgoKey;
            pRunner < &prgoKey[cKeys];
            pRunner++ )
        {
            if (pRunner->m_key == key)
            {
                *pRunner->m_pval = pth->mv_value[i+1];
                fFound = true;
                break;
            }
        } // for key

        if (! fFound && ! fAllowOtherKeys)
        {
            Val keys = nil;
            for (
                const KeyArg* pRunner = prgoKey;
                pRunner < &prgoKey[cKeys];
                pRunner++ )
            {
                push(pRunner->m_key, keys);
            } // for key

            keys = nreverse(keys);

            if (! symbolp(key))
            {
                error(Qinvalid_keyword_argument,
                    Kdatum, key,
                    Kexpected_type, listA(Qmember, keys),
                    Kkeys,          keys );
            }
            else
            {
                error(Qunrecognized_keyword_argument,
                    Kkey,  key,
                    Kkeys, keys );
            }
        } // if not found
    } // for i
} // parseKeys

// [S]
defun(safe_list_length, (Val slow))
{
    int n = 0;
    Val fast = slow;

    while (consp(fast))
    {
        fast = cdr(fast);
        n += 1;

        if (! consp(fast)) break;
        fast = cdr(fast);
        n += 1;

        slow = cdr(slow);
        if (slow == fast) break;
    } // while

    if (nil != fast)
    {
        n = -1;
    }

    return Fixnum::Encode(n);
} // safe_list_length

defun(sxhash_eql, (Val x))
{
    return sxhash_eq(x);
} // sxhash_eql

defun(sxhash_equalp, (Val x))
{
    if (stringp(x))
    {
        StringData oX(x);

        Int iHashCode = 0;

        foreach (StringData::Enum, oEnum, &oX)
        {
            Character* p = Character::FromCode(oEnum.Get())->
                StaticCast<Character>();

            iHashCode ^= p->Upcase()->StaticCast<Character>()->ToCode();
            iHashCode  = ((iHashCode & 0xFFFF) << 8) | (iHashCode >> 16);
        } // for

        iHashCode |= 1;
        iHashCode &= Fixnum::MostPositive;

        return Fixnum::Encode(iHashCode);
    } // if string

    return sxhash(x);
} // sxhash_equalp

// [T]
void NoReturn SignalTypeError(Val datum, Val expected_type)
{
    error(Qtype_error, Kdatum, datum, Kexpected_type, expected_type);
} // SignalTypeError

namespace CommonLisp
{

using namespace TinyCl;

// [A]
defun(appendS2, (Val x, Val y))
{
    if (endp(x)) return y;
    Val head = list(car(x));
    Val tail = head;
    foreach (List::Enum, oEnum, cdr(x))
    {
        tail = setf_cdr(list(oEnum.Get()), tail);
    } // for each elt
    setf_cdr(y, tail);
    return head;
} // appendS2

// [A]
defpred(array_has_fill_pointer_p, (Val x))
{
    if (VectorObject* p = x->DynamicCast<VectorObject>())
    {
        return (p->m_flags->ToInt() & Carray_flag_fill_pointer->ToInt()) != 0;
    }

    if (arrayp(x))
    {
        return false;
    }

    SignalTypeError(x, Qarray);
} // array_has_fill_pointer_p

defpred(arrayp, (Val x))
{
    if (Record* p = x->DynamicCast<Record>())
    {
        return p->m_classd >= CLASSD_array_min &&
               p->m_classd <= CLASSD_array_max;
    } // if
    return false;
} // arrayp

// [B]
defpred(boundp, (Val symb))
{
    check_type(symb, symbol);

    if (PKG_keyword == symb->StaticCast<Symbol>()->m_package)
    {
        return true;
    }

    Val cell = find_value_cell(symb);

    if (ValueCell* p = cell->DynamicCast<ValueCell>())
    {
        return MARKER_unbound != p->m_value;
    }

    if (TlvRecord* p = cell->DynamicCast<TlvRecord>())
    {
        Int iIndex = Fixnum::Decode_(p->m_index);
        return MARKER_unbound != *Thread::TlvPtr_(iIndex);
    }

    return false;
} // boundp

// [C]
defun(car, (Val x))
    { check_type(x, list); return x->StaticCast<List>()->m_car; }

defun(cdr, (Val x))
    { check_type(x, list); return x->StaticCast<List>()->m_cdr; }

defun(caar, (Val x)) { return car(car(x)); }
defun(cadr, (Val x)) { return car(cdr(x)); }
defun(cdar, (Val x)) { return cdr(car(x)); }
defun(cddr, (Val x)) { return cdr(cdr(x)); }

defun(caaar, (Val x)) { return car(car(car(x))); }
defun(caadr, (Val x)) { return car(car(cdr(x))); }
defun(cadar, (Val x)) { return car(cdr(car(x))); }
defun(caddr, (Val x)) { return car(cdr(cdr(x))); }

defun(cdaar, (Val x)) { return cdr(car(car(x))); }
defun(cdadr, (Val x)) { return cdr(car(cdr(x))); }
defun(cddar, (Val x)) { return cdr(cdr(car(x))); }
defun(cdddr, (Val x)) { return cdr(cdr(cdr(x))); }

defun(caaaar, (Val x)) { return car(car(car(car(x)))); }
defun(caaadr, (Val x)) { return car(car(car(cdr(x)))); }
defun(caadar, (Val x)) { return car(car(cdr(car(x)))); }
defun(caaddr, (Val x)) { return car(car(cdr(cdr(x)))); }
defun(cadaar, (Val x)) { return car(cdr(car(car(x)))); }
defun(cadadr, (Val x)) { return car(cdr(car(cdr(x)))); }
defun(caddar, (Val x)) { return car(cdr(cdr(car(x)))); }
defun(cadddr, (Val x)) { return car(cdr(cdr(cdr(x)))); }

defun(cdaaar, (Val x)) { return cdr(car(car(car(x)))); }
defun(cdaadr, (Val x)) { return cdr(car(car(cdr(x)))); }
defun(cdadar, (Val x)) { return cdr(car(cdr(car(x)))); }
defun(cdaddr, (Val x)) { return cdr(car(cdr(cdr(x)))); }
defun(cddaar, (Val x)) { return cdr(cdr(car(car(x)))); }
defun(cddadr, (Val x)) { return cdr(cdr(car(cdr(x)))); }
defun(cdddar, (Val x)) { return cdr(cdr(cdr(car(x)))); }
defun(cddddr, (Val x)) { return cdr(cdr(cdr(cdr(x)))); }

defun_setf(car, (Val x, Val kons))
{
    check_type(kons, cons);
    return kons->StaticCast<Cons>()->m_car = x;
} // setf_car

defun_setf(cdr, (Val x, Val kons))
{
    check_type(kons, cons);
    return kons->StaticCast<Cons>()->m_cdr = x;
} // setf_cdr

defun_setf(caar, (Val v, Val l)) { return setf_car(v, car(l)); }
defun_setf(cadr, (Val v, Val l)) { return setf_car(v, cdr(l)); }
defun_setf(cdar, (Val v, Val l)) { return setf_cdr(v, car(l)); }
defun_setf(cddr, (Val v, Val l)) { return setf_cdr(v, cdr(l)); }

defun_setf(caaar, (Val v, Val l)) { return setf_car(v, caar(l)); }
defun_setf(caadr, (Val v, Val l)) { return setf_car(v, cadr(l)); }
defun_setf(cadar, (Val v, Val l)) { return setf_car(v, caar(l)); }
defun_setf(caddr, (Val v, Val l)) { return setf_car(v, cadr(l)); }
defun_setf(cdaar, (Val v, Val l)) { return setf_cdr(v, caar(l)); }
defun_setf(cdadr, (Val v, Val l)) { return setf_cdr(v, cadr(l)); }
defun_setf(cddar, (Val v, Val l)) { return setf_cdr(v, caar(l)); }
defun_setf(cdddr, (Val v, Val l)) { return setf_cdr(v, cadr(l)); }

defpred(characterp, (Val x))
{
    return x->Is<Character>();
} // characterp

defun(char_downcase, (Val ch))
{
    check_type(ch, character);
    Character* p = ch->StaticCast<Character>();
    return p->Downcase();
} // char_downcase

defun(char_name, (Val ch))
{
    check_type(ch, character);
    return values(gethash(ch, VAR(Achar_name_tableA)));
} // char_name

defun(char_upcase, (Val ch))
{
    check_type(ch, character);
    Character* p = ch->StaticCast<Character>();
    return p->Upcase();
} // char_upcase

defun(clrhash, (Val htb))
{
    check_type(htb, hash_table);

    Val vec = htb->StaticCast<HashTable>()->m_vector;

    Val threshold = svref(vec, one);

    fill(vec, MARKER_free);
    setf_svref(zero, vec, zero);
    setf_svref(threshold, vec, one);

    return htb;
} // clrhash

defun(code_char, (Val x))
{
    unless (x >= Fixnum::Encode(0) && x < Fixnum::Encode(Character::Limit))
    {
        SignalTypeError(
            x,
            list(
                Qinteger,
                Fixnum::Encode(0),
                Fixnum::Encode(Character::Limit - 1) ) );
    }

    return Character::FromCode(static_cast<char16>(
        Fixnum::Decode_(x) ) );
} // code_char

defun(cons, (Val a, Val b))
    { return Thread::Get()->AllocCons(a, b); }

// [D]
defun(digit_char, (Val weight, Val base))
{
    unless (fixnump(base) && ge(base, 2) && le(base, 36))
    {
        SignalTypeError(base, list(Qinteger, two, Fixnum::Encode(36)));
    }

    unless (fixnump(weight) && ge(weight, 0) && lt(weight, base))
    {
        SignalTypeError(weight, list(Qinteger, zero, sub(base, 1)));
    }

    unless (lt(weight, base)) return nil;

    Int iWeight = Fixnum::Decode_(weight);

    if (iWeight < 10)
    {
        iWeight += '0';
    }
    else
    {
        iWeight += 'A' - 10;
    }

    return Character::FromCode(static_cast<char16>(iWeight));
} // digit_char

defun(digit_char_p, (Val ch, Val base))
{
    check_type(ch, character);

    unless (fixnump(base) && ge(base, 2) && le(base, 36))
    {
        SignalTypeError(base, list(Qinteger, two, Fixnum::Encode(36)));
    }

    uint wch   = ch->StaticCast<Character>()->ToCode();
    uint iBase = static_cast<uint>(Fixnum::Decode_(base));

    if (iBase <= 10 || wch <= '9')
    {
        wch -= '0';
        return wch < iBase ? Fixnum::Encode(wch) : nil;
    }

    if (wch >= 'A' && wch <= 'Z')
    {
        wch -= 'A' - 10;
        return wch < iBase ? Fixnum::Encode(wch) : nil;
    }

    if (wch >= 'a' && wch <= 'z')
    {
        wch -= 'a' - 10;
        return wch < iBase ? Fixnum::Encode(wch) : nil;
    }

    return nil;
} // digit_char_p

// [E]
defpred(endp, (Val x))
{
    if (nil == x) return true;
    if (consp(x)) return false;
    SignalTypeError(x, Qlist);
} // endp

defpred(eql, (Val x, Val y))
{
    if (x == y) return t;

    if (Bignum* p = x->DynamicCast<Bignum>())
    {
        if (Bignum* q = y->DynamicCast<Bignum>())
        {
            if (p->GetLength() != q->GetLength()) return false;
            return 0 == ::memcmp(
                p->GetStart(),
                q->GetStart(),
                sizeof(Arch::Bigit) * p->GetLength() );
        } // if bignum
        return false;
    } // if bignum

    return false;
} // eql

// TODO yosi@msn.com 2008-06-22 We should use lisp version of equal for
// interrupting recursive structure arguments.
defpred(equal, (Val x, Val y))
{
    if (x == y)
    {
        return true;
    }

    if (consp(x))
    {
        unless (consp(y)) return false;
        for (;;)
        {
            Cons* p = x->StaticCast<Cons>();
            Cons* q = y->StaticCast<Cons>();

            if (! equal(p->m_car, q->m_car))
            {
                return false;
            }

            x = p->m_cdr;
            y = q->m_cdr;

            if (! consp(x))
            {
                return ! consp(y) && equal(x, y);
            }

            if (! consp(y))
            {
                return false;
            }
        } // for
    } // if cons

    if (stringp(x))
    {
        return stringp(y) && string_eq(x, y);
    } // if string

    return eql(x, y);
} // equalp

defpred(equalp, (Val x, Val y))
{
    if (x == y)
    {
        return true;
    }

    if (consp(x))
    {
        unless (consp(y)) return false;
        for (;;)
        {
            Cons* p = x->StaticCast<Cons>();
            Cons* q = y->StaticCast<Cons>();

            if (! equalp(p->m_car, q->m_car))
            {
                return false;
            }

            x = p->m_cdr;
            y = q->m_cdr;

            if (! consp(x))
            {
                return ! consp(y) && equalp(x, y);
            }

            if (! consp(y))
            {
                return false;
            }
        } // fo
    } // if cons

    if (stringp(x))
    {
        return stringp(y) && string_equal(x, y);
    } // if string

    return equal(x, y);
} // equalp

/// <summary>
///   Error function with varaible number of parameters.
/// </summary>
void NoReturn errorV(Thread* const pth)
{
    ASSERT(Thread::Get() == pth);

    Val const frob = pth->mv_value[0];
    Val const args = valuesToList(1);

    DEBUG_FORMAT("~S ~S~%", frob, args);

    if (::IsDebuggerPresent())
    {
        __debugbreak();
    }

    Val const cond = coerce_to_condition(Qsimple_error, frob, args);
    values(cond);
    signalV(pth);
    funcall(Qinvoke_debugger, cond);
} // error

void NoReturn error(Val a)
    { values(a); errorV(Thread::Get()); }

void NoReturn error(Val a, Val b)
    { values(a, b); errorV(Thread::Get()); }

void NoReturn error(Val a, Val b, Val c)
    { values(a, b, c); errorV(Thread::Get()); }

void NoReturn error(Val a, Val b, Val c, Val d)
    { values(a, b, c, d); errorV(Thread::Get()); }

void NoReturn error(Val a, Val b, Val c, Val d, Val e)
    { values(a, b, c, d, e); errorV(Thread::Get()); }

void NoReturn error(Val a, Val b, Val c, Val d, Val e, Val f)
    { values(a, b, c, d, e, f); errorV(Thread::Get()); }

void NoReturn error(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
    { values(a, b, c, d, e, f, g); errorV(Thread::Get()); }

void NoReturn error(const char* psz)
    { error(make_string(psz)); }

void NoReturn error(const char* psz, Val a)
    { error(make_string(psz), a); }

void NoReturn error(const char* psz, Val a, Val b)
    { error(make_string(psz), a, b); }

void NoReturn error(const char* psz, Val a, Val b, Val c)
    { error(make_string(psz), a, b, c); }

// [F]

defpred(fboundp, (Val x))
{
    if (Symbol* p = x->DynamicCast<Symbol>())
    {
        return nil != p->m_function;
    } // if

    if (function_name_p(x))
    {
        Val cell = find_setf_cell(cadr(x));
        if (SetfCell* p = cell->DynamicCast<SetfCell>())
        {
            return nil != p->m_function;
        }
        return false;
    } // if

    SignalTypeError(x, Qfunction_name);
} // fboundp

defun(fdefinition, (Val const fname))
{
    if (symbolp(fname))
    {
        return symbol_function(fname);
    }

    if (function_name_p(fname))
    {
        Val const cell = find_setf_cell(cadr(fname));
        if (SetfCell* const p = cell->DynamicCast<SetfCell>())
        {
            Val const fn = p->m_function;
            if (! functionp(fn)) goto undef;
            return fn;
        }

        goto undef;
    }

    SignalTypeError(fname, Qfunction_name);

  undef:
    error(Qundefined_function, Kname, fname);
} // fdefinition

defun_setf(fill_pointer, (Val const n, Val const vec))
{
    if (! array_has_fill_pointer_p(vec))
    {
        error("~S doesn't have fill-pointer.", vec);
    }

    VectorObject* const p = vec->StaticCast<VectorObject>();

    if (lt(n, 0) || gt(n, p->m_total_size))
    {
        error(Qunbound_index,
            Kdatum,     n,
            Kexpected_type, list(Qinteger, zero, p->m_total_size),
            Ksequence,  vec );
    }

    p->m_fill_pointer = n;

    return n;
} // fill_pointer

Val find_class(Val const name, Val const errorp, Val env)
{
    check_type(name, symbol);
    if (nil == env) env = TLV(AenvironmentA);
    check_type(env, environment);

    Environment* const pEnv = env->StaticCast<Environment>();
    Val const klass = gethash(name, pEnv->m_classes);
    if (nil == klass && nil != errorp)
    {
        error("No such class ~S.", name);
    }
    return klass;
} // find_class

Val find_symbol(Val name, Val pkgd, Val* out_status)
{
    check_type(name, string);

    Val const pkg = ensure_package(pkgd);

    Package* const p = pkg->StaticCast<Package>();

    // internal table
    {
        Val vec = p->m_internal_table;
        Val index = internal_package_get(vec, name);
        if (nil != index)
        {
            *out_status = Kinternal;
            return svref(vec, index);
        }
    }

    // external table
    {
        Val vec = p->m_external_table;
        Val index = internal_package_get(vec, name);
        if (nil != index)
        {
            *out_status = Kexternal;
            return svref(vec, index);
        }
    }

    foreach (List::Enum, oEnum, p->m_use_list)
    {
        Val use = oEnum.Get();
        Val vec = use->StaticCast<Package>()->m_external_table;
        Val index = internal_package_get(vec, name);
        if (nil != index)
        {
            *out_status = Kinherited;
            return svref(vec, index);
        }
    } // for each package

    *out_status = nil;
    return nil;
} // find_symbol

Val find_symbol(Val name, Val pkg)
{
    Val status;
    Val symbol = find_symbol(name, pkg, &status);
    return values(symbol, status);
} // find_symbol

Val find_symbol(Val name)
{
    return find_symbol(name, TLV(ApackageA));
} // find_symbol

defun(find_package, (Val name))
{
    name = string(name);
    foreach (List::Enum, oEnum, VAR(ApackagesA))
    {
        Val pkg = oEnum.Get();
        foreach (List::Enum, oEnum, pkg->StaticCast<Package>()->m_names)
        {
            Val present = oEnum.Get();
            if (string_eq(present, name)) return pkg;
        } // for each name
    } // for
    return nil;
} // find_package

// [G]
Val getf(Val plist, Val key, Val defval)
{
    foreach (List::Enum, oEnum, plist)
    {
        Val present = oEnum.Get();

        if (! oEnum.AtEnd()) oEnum.Next();

        if (present == key)
        {
            return car(oEnum.GetCons());
        }
    } // for each elt
    return defval;
} // getf

Val gethash(Val key, Val htb, Val def, Val* out_found)
{
    if (MARKER_free == key || MARKER_removed == key)
    {
        error("Invalid hash-table key ~S", key);
    }

    check_type(htb, hash_table);

    HashTable::EnumAll::Arg oArg(
        htb->StaticCast<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    foreach (HashTable::EnumAll, oEnum, oArg)
    {
        const HashTable::Entry* pEntry = oEnum.Get();
        if (pEntry->IsEmpty())
        {
            break;
        }

        if (pEntry->IsRemoved())
        {
            // skip removed entry
        }
        else if (pEntry->GetKey() == key ||
            htb_test(htb, key, pEntry->GetKey()) )
        {
            *out_found = t;
            return oEnum.GetVal();
        }
    } // for each slot

    *out_found = nil;
    return def;
} // gethash

defun_setf(gethash, (Val val, Val key, Val htb))
{
    if (MARKER_free == key || MARKER_removed == key)
    {
        error("Invalid hash-table key ~S", key);
    }

    check_type(htb, hash_table);

    HashTable::EnumAll::Arg oArg(
        htb->StaticCast<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    HashTable::EnumAll::Entry* pHome = NULL;

    foreach (HashTable::EnumAll, oEnum, oArg)
    {
        HashTable::Entry* pEntry = oEnum.Get();

        if (pEntry->IsEmpty())
        {
            if (NULL == pHome)
            {
                pHome = oEnum.Get();
            }

            htb->StaticCast<HashTable>()->Put(pHome, key, val);

            if (htb_need_rehash_p(htb))
            {
                htb_rehash(htb);
            }
            return val;
        }

        if (pEntry->IsRemoved())
        {
            if (NULL == pHome)
            {
                pHome = oEnum.Get();
            }
        }
        else if (key == pEntry->GetKey() ||
                 htb_test(htb, key, pEntry->GetKey()) )
        {
            return oEnum.Get()->SetVal(val);
        }
    } // for each slot

    error("Broken hash-table ~S", htb);
} // setf_gethash

// [H]
defun(hash_table_count, (Val htb))
{
    check_type(htb, hash_table);
    HashTable* p = htb->StaticCast<HashTable>();
    return svref(p->m_vector, zero);
} // hash_table_count

// [I]
Val intern(const char* psz)
    { return intern(psz, TLV(ApackageA)); }

Val intern(const char* psz, Val pkg)
{
    StackString_<> oName(psz);
    return intern(oName, pkg);
} // intern

Val intern(Val name)
    { return intern(name, TLV(ApackageA)); }

Val intern(Val const name, Val const pkgd)
{
    check_type(name, string);

    Val const pkg = ensure_package(pkgd);

    Val status;
    Val sym = find_symbol(name, pkg, &status);
    if  (nil == status)
    {
        sym = make_symbol(name);

        Package* const p = pkg->StaticCast<Package>();

        // Note: symbol in keyword package is awlays exported.
        if (PKG_keyword == pkg)
        {
            p->m_external_table =
                internal_package_put(p->m_external_table, sym);
        }
        else
        {
            p->m_internal_table =
                internal_package_put(p->m_internal_table, sym);
        }

        sym->StaticCast<Symbol>()->m_package = pkg;
    } // if
    return values(sym, status);
} // intern

// [K]
defpred(keywordp, (Val x))
{
    unless (symbolp(x)) return false;
    return PKG_keyword == x->StaticCast<Symbol>()->m_package;
} // keywordp

// [L]
defun(listV, (Thread* pt))
{
    Val x = nil;
    foreach (Thread::EnumValueReverse, oEnum, pt)
    {
        x = cons(oEnum.Get(), x);
    } // for each val
    return x;
} // listV

defun(listAV, (Thread* pt))
{
    if (Fixnum::Encode(1) == pt->m_n)
    {
        return pt->mv_value[0];
    }

    Thread::EnumValueReverse oEnum(pt);
    Val x = oEnum.Get();
    for (;;)
    {
        oEnum.Next();
        if (oEnum.AtEnd()) break;
        x = cons(oEnum.Get(), x);
    } // for each val
    return x;
} // listAV

// [M]
defun(make_hash_tableV, (Thread* pth))
{
    Val rehash_size      = Fixnum::Encode(130);
    Val rehash_threshold = Fixnum::Encode(65);
    Val size             = Fixnum::Encode(31);
    Val test             = Qeql;

    KeyArg rgoKey[] =
    {
        KEYARG(rehash_size),
        KEYARG(rehash_threshold),
        KEYARG(size),
        KEYARG(test),
    }; // rgoKey

    parseKeys(pth, 0, rgoKey, lengthof(rgoKey));

    Val vec = make_vector(mul(size, 2));

    Val threshold = truncate(mul(size, rehash_threshold), 100);
    setf_svref(threshold, vec, one);

    Val htb = Thread::Get()->AllocRecord(CLASSD_hash_table);
    HashTable* p = htb->StaticCast<HashTable>();
    p->m_rehash_size = rehash_size;
    p->m_test        = test;
    p->m_vector      = vec;

    return clrhash(htb);
} // make_hash_table

Val make_hash_table(Val a, Val b)
    { values(a, b);  return make_hash_tableV(Thread::Get()); }

Val make_hash_table(Val a, Val b, Val c, Val d)
    { values(a, b, c, d);  return make_hash_tableV(Thread::Get()); }

defun(make_packageV, (Thread* pth))
{
    Val nicknames = nil;
    Val use       = MARKER_unbound;

    KeyArg rgoKey[] =
    {
        KEYARG(nicknames),
        KEYARG(use),
    }; // rgoKey

    Val name = string(pth->mv_value[0]);

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    if (MARKER_unbound == use)
    {
        use = list(PKG_cl);
    }

    nicknames = copy_list(nicknames);
    foreach (List::Enum, oEnum, nicknames)
    {
        oEnum.Set(string(oEnum.Get()));
    } // for each nickname

    Val use_list = copy_list(use);
    foreach (List::Enum, oEnum, use_list)
    {
        oEnum.Set(ensure_package(oEnum.Get()));
    } // for each use

    Val package = pth->AllocRecord(CLASSD_package);
    Package* p = package->StaticCast<Package>();

    p->m_external_table    = make_vector(Fixnum::Encode(31));
    p->m_internal_table    = make_vector(Fixnum::Encode(31));
    p->m_names             = cons(name, nicknames);
    p->m_protect           = nil;
    p->m_shadowing_symbols = nil;
    p->m_used_by_list      = nil;
    p->m_use_list          = use_list;

    push(package, VAR(ApackagesA));

    return package;
} // make_packageV

Val make_string(Val len)
{
    Int iLen = fixnump(len) ? Fixnum::Decode_(len) : -1;
    if (iLen < 0 || iLen > Arch::ArrayTotalSizeLimit)
    {
        SignalTypeError(len, Qsequence_index);
    }

    return Thread::Get()->AllocBinVec(CLASSD_simple_string, len);
} // make_string

Val make_string(const char* psz)
    { return make_string(psz, ::lstrlenA(psz)); }

Val make_string(const char* pchStart, size_t cch)
{
    Val str= Thread::Get()->AllocBinVec(
        CLASSD_simple_string,
        Fixnum::Encode(cch) );

    char16* pwsz = str->StaticCast<SimpleString>()->GetStart();
    const char* pchEnd = pchStart + cch;
    for (const char* pch = pchStart; pch < pchEnd; pch++)
    {
        *pwsz++ = *pch;
    } // for pch
    return str;
} // make_string

Val make_string(const char16* pwch, size_t cwch)
{
    Val str = Thread::Get()->AllocBinVec(
        CLASSD_simple_string,
        Fixnum::Encode(cwch) );

    CopyMemory(
        str->StaticCast<SimpleString>()->GetStart(),
        pwch,
        sizeof(char16) * cwch );

    return str;
} // make_string

Val make_string(const char16* pwsz)
    { return make_string(pwsz, lstrlenW(pwsz)); }

defun(make_symbol, (Val name))
{
    check_type(name, simple_string);

    if (name->Is<StackString>())
    {
        SimpleString* p = name->StaticCast<SimpleString>();
        name = make_string(p->GetStart(), Fixnum::Decode_(p->m_length));
    }

    Val x = Thread::Get()->AllocRecord(CLASSD_symbol);
    Symbol* p = x->StaticCast<Symbol>();
    p->m_function  = nil;
    p->m_hash_code = name->StaticCast<SimpleString>()->Hash();
    p->m_name      = name;
    p->m_package   = nil;
    p->m_plist     = nil;
    return x;
} // make_symbol

// [N]
defun(name_char, (Val x))
{
    if (characterp(x))
    {
        return x;
    }

    if (stringp(x))
    {
        return values(gethash(x, VAR(Aname_char_tableA)));
    }

    if (symbolp(x))
    {
        Val y = x->StaticCast<Symbol>()->m_name;
        return values(gethash(y, VAR(Aname_char_tableA)));
    }

    SignalTypeError(x, Qstring_designator);
} // name_char

// [R]
defun(remhash, (Val key, Val htb))
{
    if (MARKER_free == key || MARKER_removed == key)
    {
        error("Invalid hash-table key ~S", key);
    }

    check_type(htb, hash_table);

    HashTable::EnumAll::Arg oArg(
        htb->StaticCast<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    foreach (HashTable::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.GetKey();

        if (MARKER_free == present)
        {
            return nil;
        }

        if (key == present || htb_test(htb, key, present))
        {
            htb->StaticCast<HashTable>()->Remove(oEnum.Get());
            return htb;
        }
    } // for each slot

    error("Broken hash-table ~S", htb);
} // remhash

// [S]
defun(signalV, (Thread* pth))
{
    ASSERT(Thread::Get() == pth);

    Val frob = pth->mv_value[0];
    Val args = valuesToList(1);

    Val const cond = coerce_to_condition(Qsimple_condition, frob, args);

    Val const curr = pth->m_fp;

    foreach (Thread::EnumFrame, oEnum, pth)
    {
        Frame* pRunner = oEnum.Get();

        if (HandlerFrame* pFrame = pRunner->DynamicCast<HandlerFrame>())
        {
            foreach (HandlerFrame::EnumCatch, oEnum, pFrame)
            {
                HandlerFrame::Catch* pCatch = oEnum.Get();
                if (typep(cond, pCatch->m_type))
                {
                    // Disable current handler cluster
                    pth->m_fp = Fixnum::Encode(pFrame->m_pOuter);
                    funcall(pCatch->m_fn, cond);
                    break;
                } // if
            } // for each catch
        }
        else if (TryCatchFrame* pFrame =
                    pRunner->DynamicCast<TryCatchFrame>() )
        {
            foreach (TryCatchFrame::EnumCatch, oEnum, pFrame)
            {
                TryCatchFrame::Catch* pCatch = oEnum.Get();
                if (typep(cond, pCatch->m_type))
                {
                    pth->Unwinds(pFrame);
                    pth->m_fp = Fixnum::Encode(pFrame->m_pOuter);
                    pFrame->Transfer(pth, pCatch, cond);
                    // NOTREACHED
                } // if
            } // for each catch
        }
    } // for each frame

    // Restore original handlers
    pth->m_fp = curr;
    return nil;
} // signalV

defun(sxhash, (Val x))
{
    class SxHash
    {
        // Compute Adler32(RFC1950)
        public: static Val accumlate(Val acc, Val datum)
        {
            UInt nHash = ComputeHash(
                Fixnum::Decode_(datum),
                Fixnum::Decode_(acc) );

            nHash &= Fixnum::MostPositive;
            return Fixnum::Encode(nHash);
        } // accumlate

        public: static Val Hash(Val x, Val hash, int iDepth)
        {
            for (;;)
            {
                if (nil == x) return hash;

                iDepth += 1;

                if (BasicHost* p = x->DynamicCast<BasicHost>())
                {
                    hash = Hash(hash, p->m_classd, iDepth);
                    hash = Hash(hash, p->m_name, iDepth);
                    return hash;
                } // if basic_host

                if (Cons* p = x->DynamicCast<Cons>())
                {
                    hash = Hash(p->m_car, hash, iDepth);
                    x = p->m_cdr;
                    continue;
                } // if cons

                if (Pathname* p = x->DynamicCast<Pathname>())
                {
                    hash = Hash(hash, p->m_host, iDepth);
                    hash = Hash(hash, p->m_device, iDepth);
                    hash = Hash(hash, p->m_directory, iDepth);
                    hash = Hash(hash, p->m_name, iDepth);
                    hash = Hash(hash, p->m_type, iDepth);
                    hash = Hash(hash, p->m_version, iDepth);
                    return hash;
                } // if pathname

                if (SimpleString* p = x->DynamicCast<SimpleString>())
                {
                    return accumlate(hash, p->Hash());
                } // if simple_string

                return accumlate(hash, sxhash_eql(x));
            } // for
        } // Hash
    }; // SxHash

    return SxHash::Hash(x, zero, 0);
} // sxhash

defun(symbol_function, (Val x))
{
    check_type(x, symbol);
    Val fn = x->StaticCast<Symbol>()->m_function;
    unless (functionp(fn)) error(Qundefined_function, Kname, x);
    return fn;
} // symbol_function

defun(symbol_value, (Val symb))
{
    if (nil == symb)
    {
        return nil;
    }

    check_type(symb, symbol);

    if (PKG_keyword == symb->StaticCast<Symbol>()->m_package)
    {
        return symb;
    }

    Val const cell = find_value_cell(symb);

    if (ValueCell* const p = cell->DynamicCast<ValueCell>())
    {
        Val const val = p->m_value;
        if (MARKER_unbound == val) goto unbound;
        return val;
    }

    if (TlvRecord* const p = cell->DynamicCast<TlvRecord>())
    {
        Int iIndex = Fixnum::Decode_(p->m_index);
        Val const val = *Thread::TlvPtr_(iIndex);
        if (MARKER_unbound == val) goto unbound;
        return val;
    }

  unbound:
    error(Qunbound_variable, Kname, symb);
} // symbol_value

// [U]
defun(use_package, (Val pkgs, Val pkg))
{
    class UsePackage
    {
        public: static void Run(Val pkgs, Val pkg)
        {
            pkg = ensure_package(pkg);
            Package* p = pkg->StaticCast<Package>();
            if (! consp(pkgs))
            {
                usePackage(pkgs, p);
            }
            else
            {
                foreach (List::Enum, oEnum, pkgs)
                {
                    usePackage(oEnum.Get(), p);
                } // for each pkg
            } // if
        } // Run

        private: static void usePackage(Val to_use, Package* p)
        {
            to_use = ensure_package(to_use);
            if (nil == memq(to_use, p->m_use_list))
            {
                push(to_use, p->m_use_list);

                push(
                    p->Encode(),
                    to_use->StaticCast<Package>()->m_used_by_list );
            }
        } // usePackage
    }; // UsePackage
    UsePackage::Run(pkgs, pkg);
    return t;
} // use_package

// [V]
defun(values, ())
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(0);
    return nil;
} // values

defun(values, (Val a))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(1);
    p->mv_value[0] = a;
    return a;
} // values


defun(values, (Val a, Val b))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(2);
    p->mv_value[0] = a; p->mv_value[1] = b;
    return a;
} // values

defun(values, (Val a, Val b, Val c))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(3);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    return a;
} // values

defun(values, (Val a, Val b, Val c, Val d))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(4);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    p->mv_value[3] = d;
    return a;
} // values

defun(values, (Val a, Val b, Val c, Val d, Val e))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(5);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    p->mv_value[3] = d; p->mv_value[4] = e;
    return a;
} // values

defun(values, (Val a, Val b, Val c, Val d, Val e, Val f))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(6);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    p->mv_value[3] = d; p->mv_value[4] = e; p->mv_value[5] = f;
    return a;
} // values

defun(values, (Val a, Val b, Val c, Val d, Val e, Val f, Val g))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(7);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    p->mv_value[3] = d; p->mv_value[4] = e; p->mv_value[5] = f;
    p->mv_value[6] = g;
    return a;
} // values

defun(values, (Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(8);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    p->mv_value[3] = d; p->mv_value[4] = e; p->mv_value[5] = f;
    p->mv_value[6] = g; p->mv_value[7] = h;
    return a;
} // values

defun(values, (Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h, Val i))
{
    Thread* p = Thread::Get();
    p->m_n = Fixnum::Encode(9);
    p->mv_value[0] = a; p->mv_value[1] = b; p->mv_value[2] = c;
    p->mv_value[3] = d; p->mv_value[4] = e; p->mv_value[5] = f;
    p->mv_value[6] = g; p->mv_value[7] = h; p->mv_value[8] = i;
    return a;
} // values

defun(values_list, (Val list))
{
    return Thread::Get()->ValuesFromList(list);
} // values_list

} // CommonLisp
} // TinyCl
