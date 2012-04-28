#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Initialization
// tinycl_init.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_init.cpp#17 $
//
#define DEBUG_CLASS     0
#define DEBUG_CLASSD    0
#define DEBUG_FINALIZE  0
#define DEBUG_SLOTS     0
#include "./tinycl_init.h"
#include "../tinycl_clos.h"
#include "../tinycl_sxhash.h"

#include "../rtl/tinycl_bignum.h"
#include "../rtl/tinycl_float.h"

namespace TinyCl
{

using namespace Internal;

void DllRestart(HMODULE);
void InstallCompilerStatics();
void InstallStaticFunctions();
void InstallStaticFunctions2();
void InstallStaticMethods();
void Platform_InstallStaticObjects();
void PlatformRestart(Thread*);

static Val s_ConstantlyNil;

#if USE_MODERN_MODE
Val ImportSymbolString(const char* const pszIn)
{
    char sz[200];
    char* pszDst = sz;
    for (const char* psz = pszIn; 0 != *psz; psz++)
    {
        *pszDst++ = *psz >= 'A' && *psz <= 'Z' ?
            *psz - 'A' + 'a' :
            *psz;
    } // for psz

    *pszDst= 0;

    return make_string(sz);
} // ImportSymbolString
#else
Val ImportSymbolString(const char* const psz)
    { return make_string(psz); }
#endif

Val InternSymbol(const char* psz)
    { return intern(ImportSymbolString(psz)); }

class StdMethodCombination :
    public Instance_<
        StdMethodCombination,
        Layout_standard_method_combination >
{
    public: static Int Decode_(const Datum* const x)
    {
        return Storage::Decode_(x->StaticCast<Instance>()->m_storage);
    } // Decode_
}; // StdMethodCombination

static Val makeStandardMethodCombination()
{
    Val const mc = Thread::Get()->AllocInstance(
        CLASSD_standard_method_combination );

    StdMethodCombination* const p =
        mc->StaticCast<StdMethodCombination>();

    p->m_options = nil;
    p->m_type    = Qstandard;

    return mc;
} // makeStandardMethodCombination

/// <summary>
///   Installs static functions described in specified table.
/// </summary>
/// <param name="pStart">Start address of function table</param>
/// <param name="cEntries">Number of entries in function table</param>
void InstallStaticFunctions(
    const FunEntry* const pStart,
    uint            const cEntries )
{
    const FunEntry* const pEnd = pStart + cEntries;
    for (const FunEntry* p = pStart; p < pEnd; p++)
    {
        Val cell = p->m_fname;

        Val fn;

        Val fname = cell;
        if (SetfCell* const p = cell->DynamicCast<SetfCell>())
        {
            fname = list(Qsetf, p->m_name);
            fn = p->m_function;
        }
        else if (Symbol* const p = cell->DynamicCast<Symbol>())
        {
            fn = p->m_function;
        }
        else
        {
            CAN_NOT_HAPPEN();
        }

        if (nil == fn)
        {
            Val fn = MakeWrapper(
                    fname,
                    p->m_iMin,
                    p->m_iMax,
                    p->m_iRest & 1,
                    p->m_iVals,
                    p->m_iRest & 2,
                    p->m_psz );

            if (setf_cell_p(cell))
            {
                cell->StaticCast<SetfCell>()->m_function = fn;
            }
            else
            {
                fname->StaticCast<Symbol>()->m_function = fn;
            }
        } // if
    } // for each entry
} // InstallStaticFunctions

void InstallType(Val const name, Val const typespec)
{
    Environment* const pEnv = VAR(Aruntime_environmentA)->
        StaticCast<Environment>();

    setf_gethash(typespec, name, pEnv->m_types);
} // InstallType

static void installVarType(Val const name, Val const typespec)
{
    Environment* const pEnv = VAR(Aruntime_environmentA)->
        StaticCast<Environment>();

    Val const frob = gethash(name, pEnv->m_variables);
    if (! consp(frob))
    {
        CAN_NOT_HAPPEN();
    }

    setf_cdr(cons(cons(Qtype, typespec), cdr(frob)), frob);
} // installVarType

namespace
{

class ComputeCpl
{
    struct Klass
    {
        Val     m_class;
        bool    m_fVisit;
    }; // Klass

    struct Pair
    {
        Klass*  m_pSub;
        Klass*  m_pSuper;
    }; // Pair

    Klass   m_rgoKlass[100];
    UINT    m_cClasses;
    Pair    m_rgoPair[100];
    UINT    m_cPairs;

    public: static Val Run(Val klass)
    {
        ComputeCpl oCompute;
        return oCompute.run(klass);
    } // Run

    private: ComputeCpl() :
        m_cClasses(0),
        m_cPairs(0) {}

    private: Val run(Val klass)
    {
        walk(mapClassToKlass(klass));

        Val cpl = nil;

        uint cPairs = m_cPairs;
        while (cPairs >= 1)
        {
            Klass* pKlass = findLeastClass();
                ASSERT(NULL != pKlass);

            for (
                Pair* pRunner = &m_rgoPair[0];
                pRunner < &m_rgoPair[m_cPairs];
                pRunner++ )
            {
                if (pRunner->m_pSuper == pKlass)
                {
                    pRunner->m_pSuper = NULL;
                    pRunner->m_pSub = NULL;
                    cPairs -= 1;
                }
            } // for

            cpl = cons(pKlass->m_class, cpl);
        } // cPairs

        return cons(klass, cpl);
    } // Run

    Klass* findLeastClass()
    {
        for (
            Pair* pRunner = &m_rgoPair[m_cPairs - 1];
            pRunner >= &m_rgoPair[0];
            pRunner-- )
        {
            if (NULL != pRunner->m_pSuper)
            {
                if (NULL == findSubClass(pRunner->m_pSuper))
                {
                    return pRunner->m_pSuper;
                }
            }
        } // for
        CAN_NOT_HAPPEN();
    } // findLeastClass

    Klass* findSubClass(Klass* pKlass)
    {
        for (
            Pair* pRunner = &m_rgoPair[0];
            pRunner < &m_rgoPair[m_cPairs];
            pRunner++ )
        {
            if (pRunner->m_pSub == pKlass)
            {
                return pRunner->m_pSub;
            }
        } // for
        return NULL;
    } // findSubClass

    // mapClassToKlass
    Klass* mapClassToKlass(Val klass)
    {
        for (
            Klass* pRunner = &m_rgoKlass[0];
            pRunner < &m_rgoKlass[m_cClasses];
            pRunner++ )
        {
            if (pRunner->m_class == klass)
            {
                return pRunner;
            }
        } // for
        ASSERT(m_cClasses < lengthof(m_rgoKlass));

        Klass* pKlass = &m_rgoKlass[m_cClasses];
        pKlass->m_class = klass;
        pKlass->m_fVisit = false;
        m_cClasses += 1;
        return pKlass;
    } // mapClassToKlass

    // walk
    //  Construct Rc and R.
    //      Rc = { (C, C1) (C1, C2) ... (Cn-1, Cn) }
    //      R  = union Rc where c = all of S
    //          where S is set of C and its superclases
    //
    void walk(Klass* pKlass)
    {
        if (pKlass->m_fVisit)
        {
            return;
        }

        pKlass->m_fVisit = true;

        foreach (
            List::Enum,
            oEnum,
            pKlass->m_class->StaticCast<Class>()->m_direct_superclasses )
        {
            Klass* pSuper = mapClassToKlass(oEnum.Get());

            m_rgoPair[m_cPairs].m_pSub   = pKlass;
            m_rgoPair[m_cPairs].m_pSuper = pSuper;
                m_cPairs += 1;
            walk(pSuper);
            pKlass = pSuper;
        } // for each direct super class
    } // walk
}; // ComputeCpl

class ClassFinalizer
{
    public: static Val InstallSlots(Val klass)
    {
        #if DEBUG_SLOTS
            DEBUG_FORMAT("~S~%", klass);
        #endif

        Class* pClass = klass->StaticCast<Class>();
        ASSERT(nil == pClass->m_slots);

       if (nil == pClass->m_class_precedence_list)
       {
            Val cpl = computeCpl(klass);
            pClass->m_class_precedence_list = cpl;
       }

        Val slots = nreverse(computeSlots(klass, nil));
        pClass->m_slots = slots;
        return slots;
    } // InstallSlots

    public: static void Run(Val klass)
    {
        Class* pClass = klass->StaticCast<Class>();

        #if DEBUG_FINALIZE
            DEBUG_FORMAT("~S~%", klass);
        #endif

        if (nil != pClass->m_class_precedence_list)
        {
            return;
        }

        foreach (List::Enum, oEnum, pClass->m_direct_slots)
        {
            DSlotD* const p = oEnum.Get()->StaticCast<DSlotD>();
            if (typep(nil, p->m_type))
            {
                p->m_initfunction = s_ConstantlyNil;
            }
        } // for each dslotd

        foreach (List::Enum, oEnum, pClass->m_direct_superclasses)
        {
            ClassFinalizer::Run(oEnum.Get());
        } // for each super

        Val const cpl = computeCpl(klass);
        pClass->m_class_precedence_list = cpl;

        Val const instanced =  pClass->m_instanced;
        if (nil == instanced)
        {
             return;
        }

        #if DEBUG_CLASSD
            DEBUG_FORMAT("~X~X ~S~%",
                Fixnum::Encode(instanced->ToInt() >> 4),
                Fixnum::Encode(instanced->ToInt() & 15),
                pClass->m_name );
        #endif

        ClassD* const pInstanceD = instanced->StaticCast<ClassD>();

        pInstanceD->m_class_precedence_list = cpl;
        pInstanceD->m_slots = InstallSlots(klass);;

        pInstanceD->m_hash_code = pClass->m_name->
            StaticCast<Symbol>()->m_hash_code;
    } // Run

    // [C]
    private: static Val computeCpl(Val klass)
    {
        Class* const p = klass->StaticCast<Class>();
        if (nil != p->m_class_precedence_list)
        {
            return p->m_class_precedence_list;
        }

        Val const supers = p->m_direct_superclasses;
        ASSERT(nil != supers);
        if (nil == cdr(supers))
        {
            return cons(klass, ComputeCpl::Run(car(supers)));
        }
        return ComputeCpl::Run(klass);
    } // computeCpl

    private: static Val computeSlots(Val klass, Val slots)
    {
        if (CLASS_t == klass)
        {
            return slots;
        }

        Class* const pClass = klass->StaticCast<Class>();
        foreach (List::Enum, oEnum, pClass->m_direct_superclasses)
        {
            Val super = oEnum.Get();
            if (CLASS_funcallable_instance != super)
            {
                slots = computeSlots(super, slots);
            }
        } // for each super

        Val location = Fixnum::Encode(0);

        if (nil != slots)
        {
            location = car(slots)->StaticCast<ESlotD>()->m_location;
            location = add(location, 1);
        }

        Val eslotdd = CLASSD_standard_effective_slot_definition;
        if (CLASSD_structure_class == klass->StaticCast<Class>()->m_classd)
        {
            eslotdd = CLASSD_structure_effective_slot_definition;
        }

        foreach (List::Enum, oEnum, pClass->m_direct_slots)
        {
            Val dslotd = oEnum.Get();
            Val name = dslotd->StaticCast<DSlotD>()->m_name;
            if (nil == name) continue;
            if (! findSlotD(name, slots))
            {
                DSlotD* pDSlotD = dslotd->StaticCast<DSlotD>();

                Val eslotd = Thread::Get()->AllocInstance(eslotdd);

                ESlotD* p = eslotd->StaticCast<ESlotD>();
                p->m_allocation     = pDSlotD->m_allocation;
                p->m_initargs       = pDSlotD->m_initargs;
                p->m_initform       = pDSlotD->m_initform;
                p->m_initfunction   = pDSlotD->m_initfunction;
                p->m_location       = location;
                p->m_name           = name;
                p->m_type           = pDSlotD->m_type;

                #if DEBUG_SLOT
                    DEBUG_FORMAT("  ~S ~D ~S~%",
                        klass->StaticCast<Class>()->m_name,
                        location,
                        pDSlotD->m_name );
                #endif

                location = add(location, 1);

                push(eslotd, slots);
            }
        } // for each dslotd

        return slots;
    } // computeSlots

    // [F]
    private: static bool findSlotD(Val name, Val slots)
    {
        foreach (List::Enum, oEnum, slots)
        {
            ESlotD* p = oEnum.Get()->StaticCast<ESlotD>();
            if (p->m_name == name)
            {
                return true;
            }
        } // for each eslotd
        return false;
    } // findSlotD

    // [S]
    public: static void SetSlotInitForm(
        Val const klass,
        Val const slot_name,
        Val const initform,
        Val const initvalue )
    {
        Val eslotd = find_slot(klass, slot_name);
        if (nil == eslotd)
        {
            error("~S doesn't have slot ~S.", klass, slot_name);
        }

        ESlotD* const pESlotD = eslotd->StaticCast<ESlotD>();
        pESlotD->m_initform = initform;
        pESlotD->m_initfunction = constantly(initvalue);
    } // SetSlotInitForm
}; // ClassFinalizer

class InstallDirectSlots
{
    private: static void installSlot(Val klass, Val slotdd, ...)
    {
        Val* pp = &klass->StaticCast<Class>()->m_direct_slots;
        *pp = nil;

        va_list args;
        va_start(args, slotdd);
        for (;;)
        {
            Val name = va_arg(args, Val);
            if (nil == name) break;

            Val initarg  = va_arg(args, Val);
            Val typespec = va_arg(args, Val);

            Val dslotd = Thread::Get()->AllocInstance(slotdd);
            DSlotD* p = dslotd->StaticCast<DSlotD>();
            p->m_name           = name;
            p->m_allocation     = Kinstance;
            p->m_initargs       = nil == initarg ? nil : list(initarg);
            p->m_initform       = nil;
            p->m_initfunction   = nil;
            p->m_readers        = nil;
            p->m_type           = typespec;
            p->m_writers        = nil;

            Val kons = list(dslotd);
            *pp = kons;
            pp = &kons->StaticCast<Cons>()->m_cdr;
        } // for
        va_end(args);
    } // installSlot
}; // InstallDirectSlots

static void installAliasFunctions()
{
    struct Alias
    {
        Val m_alias;
        Val m_real;
    }; // Alias

    static const Alias k_rgoAlias[] =
    {
        { Qfirst,   Qcar    },
        { Qsecond,  Qcadr   },
        { Qthird,   Qcaddr  },
        { Qfourth,  Qcadddr },
        { Qrest,    Qcdr    },

        { SETF_first,   SETF_car    },
        { SETF_second,  SETF_cadr   },
        { SETF_third,   SETF_caddr  },
        { SETF_fourth,  SETF_cadddr },
        { SETF_rest,    SETF_cdr    },
    }; // k_rgoAlias

    for (
        const Alias* p = k_rgoAlias;
        p < k_rgoAlias + lengthof(k_rgoAlias);
        p++ )
    {
        if (Symbol* q = p->m_alias->DynamicCast<Symbol>())
        {
            if (nil == q->m_function)
            {
                q->m_function = p->m_real->StaticCast<Symbol>()->m_function;
            }
        }
        else if (SetfCell* q = p->m_alias->DynamicCast<SetfCell>())
        {
            if (nil == q->m_function)
            {
                q->m_function = p->m_real->StaticCast<SetfCell>()->m_function;
            }
        }
        else
        {
            CAN_NOT_HAPPEN();
        }
    } // for p
} // installAliasFunctions

static void installFloat32(
    Layout_single_float* const p,
    uint                 const s,
    int32                const e,
    int32                const i )
{
    p->m_classd = CLASSD_single_float;

    Float32Impl::Layout* const q =
        reinterpret_cast<Float32Impl::Layout*>(&p->m_flt);

    q->m_nSign        = s;
    q->m_nExponent    = e;
    q->m_nSignificand = i;
} // installFloat32

static void installFloat64(
    Layout_double_float* const p,
    uint32               const h,
    uint32               const l )
{
    p->m_classd = CLASSD_double_float;
    Arch::Float64* q = reinterpret_cast<Arch::Float64*>(&p->m_dbl);
    q->m_hl.h = h;
    q->m_hl.l = l;
} // installFloat64

static void installFloat64(
    Layout_double_float* const p,
    uint                 const s,
    int32                const e,
    int32                const h,
    int32                const l )
{
    p->m_classd = CLASSD_double_float;

    Float64Impl::Layout* const q =
        reinterpret_cast<Float64Impl::Layout*>(&p->m_dbl);

    q->m_nSign         = s;
    q->m_nExponent     = e;
    q->m_nSignificandH = h;
    q->m_nSignificandL = l;
} // installFloat64

} // namespace

void FinalizeInheritance(Val const klass)
{
    ClassFinalizer::Run(klass);
} // FinalizeInheritance

void InstallStaticClass(Val const klass,  ...)
{
    #if DEBUG_CLASS
        DEBUG_FORMAT("~S~%", klass);
    #endif

    Class* pClass = klass->StaticCast<Class>();

    setf_gethash(
        klass,
        pClass->m_name,
        VAR(Aruntime_environmentA)->StaticCast<Environment>()->m_classes );

    va_list args;
    va_start(args, klass);

    Val obj;

    // Direct super classes
    {
        Val* pp = &pClass->m_direct_superclasses;
        *pp = nil;
        for (;;)
        {
            obj = va_arg(args, Val);
            if (symbolp(obj)) break;

            *pp = list(obj);
            pp = &(*pp)->StaticCast<Cons>()->m_cdr;
        } // for

        foreach (List::Enum, oEnum, pClass->m_direct_superclasses)
        {
            Val super = oEnum.Get();

            push(
                klass,
                super->StaticCast<Class>()->m_direct_subclasses );
        } // for each super
    }

    Val metaclass = class_of(klass);

    Val slotdd;
    if (CLASS_structure_class == metaclass)
    {
        slotdd = CLASSD_structure_direct_slot_definition;
    }
    else
    {
        slotdd = CLASSD_standard_direct_slot_definition;
    }

    // Direct Slots
    {
        Val* pp = &klass->StaticCast<Class>()->m_direct_slots;
        *pp = nil;

        while (nil != obj)
        {
            Val const slot_name = obj;
            Val const initarg   = va_arg(args, Val);
            Val const typespec  = va_arg(args, Val);

            Val const dslotd = Thread::Get()->AllocInstance(slotdd);
            DSlotD* const p = dslotd->StaticCast<DSlotD>();
            p->m_name           = slot_name;
            p->m_allocation     = Kinstance;
            p->m_initargs       = nil == initarg ? nil : list(initarg);
            p->m_initform       = nil;
            p->m_initfunction   = nil;
            p->m_readers        = nil;
            p->m_type           = typespec;
            p->m_writers        = nil;

            *pp = list(dslotd);
            pp = &(*pp)->StaticCast<Cons>()->m_cdr;

            obj = va_arg(args, Val);
        } // while
    }

    va_end(args);
} // InstalStaticClass

void InstallStaticClassD(
    Val             const metaclassd,
    Val             const name,
    Val             const instanced,
    Val             const klass,
    ClassD::Format  const eFormat,
    size_t          const cbFixedIn )
{
    size_t const cbFixed = RoundUp(cbFixedIn, Arch::Align_Record);

    if (nil != instanced)
    {
        ASSERT(cbFixed > 0);

        ClassD* pInstanceD =
            reinterpret_cast<ClassD*>(instanced->StaticCast<Record>());

        pInstanceD->m_class_precedence_list = nil;

        pInstanceD->m_classd       = CLASSD_classd;
        pInstanceD->m_class        = klass;
        pInstanceD->m_format       = Datum::FromInt<Datum>(eFormat);
        pInstanceD->m_fixed_size   = Fixnum::Encode(cbFixed);
        pInstanceD->m_type         = name;
    }

    Instance* const p = reinterpret_cast<Instance*>(Record::Decode_(klass));
    p->m_classd  = metaclassd;

    Layout_class* const s = reinterpret_cast<Layout_class*>(
        p->ToInt() + sizeof(Instance) );

    p->m_storage = reinterpret_cast<Storage*>(s)->Encode();

    s->m_classd                = CLASSD_storage;
    s->m_storaged              = metaclassd;

    s->m_class_precedence_list = nil;
    s->m_direct_methods        = nil;
    s->m_direct_subclasses     = nil;
    s->m_instanced             = instanced;
    s->m_name                  = name;
    s->m_plist                 = nil;
    s->m_prototype             = nil;
    s->m_slots                 = nil;
} // InstallStaticClassD

void installStaticSymbol(
    Val         const pkg,
    Val         const sym,
    const char* const pszName )
{
    Symbol* const p = reinterpret_cast<Symbol*>(Record::Decode_(sym));
    p->m_classd    = CLASSD_symbol;
    p->m_name      = ImportSymbolString(pszName);
    p->m_hash_code = p->m_name->StaticCast<SimpleString>()->Hash();
    p->m_package   = pkg;
    p->m_plist     = nil;
    p->m_function  = nil;
} // installStaticSymbol

void InstallStaticExternalSymbol(Val pkg, Val sym, const char* psz)
{
    installStaticSymbol(pkg, sym, psz);

    Package* p = pkg->StaticCast<Package>();
    p->m_external_table = internal_package_put(p->m_external_table, sym);
} // InstallStaticExternalSymbol


void InstallStaticInternalSymbol(Val pkg, Val sym, const char* psz)
{
    installStaticSymbol(pkg, sym, psz);

    if (nil != pkg)
    {
        Package* p = pkg->StaticCast<Package>();
        p->m_internal_table = internal_package_put(p->m_internal_table, sym);
    }
} // InstallStaticInternalSymbol

static void installStaticMacro(Val name, const char* pszFn)
{
    Val expander = MakeWrapper(
        list(Kmacro, name),
        2,
        2, 0,
        1,
        false,
        pszFn );

    setf_macro_function(expander, name);
} // installStaticMacro

void InstallStaticPackage(
    Val         const pkg,
    const char* const pszNames,
    int         const cExternals,
    int         const cInternals,
    ... )
{
    pkg->StaticCast<Record>()->m_classd = CLASSD_package;

    Package* const p = pkg->StaticCast<Package>();
    p->m_external_table = make_vector(Fixnum::Encode(cExternals));
    p->m_internal_table = make_vector(Fixnum::Encode(cInternals));
    p->m_names = nil;
    p->m_use_list = nil;
    p->m_used_by_list = nil;
    p->m_shadowing_symbols = nil;
    p->m_protect = nil;

    {
        char sz[100];
        char* pszDst = sz;
        const char* pszSrc = pszNames;
        Val* px = &p->m_names;
        for (;;)
        {
            if (' ' == *pszSrc || 0 == *pszSrc)
            {
                *pszDst = 0;
                Val cons = list(ImportSymbolString(sz));
                *px = cons;
                px = &cons->StaticCast<Cons>()->m_cdr;

                if (0 == *pszSrc)
                {
                    break;
                }

                pszDst = sz;
                pszSrc++;
            }
            else
            {
                *pszDst++ = *pszSrc++;
            }
        } // for each name
    }

    VAR(ApackagesA) = cons(pkg, VAR(ApackagesA));

    va_list args;
    va_start(args, cInternals);
    for (;;)
    {
        Val const use = va_arg(args, Val);
        if (nil == use)
        {
            break;
        }
        use_package(use, pkg);
    } // for
    va_end(args);
} // InstallStaticPackage

void InstallStaticSetfCell(Val const cell, Val const name)
{
    SetfCell* const p = reinterpret_cast<SetfCell*>(Record::Decode_(cell));
    p->m_classd   = SetfCell::ClassD_();
    p->m_name     = name;
    p->m_function = nil;

    setf_gethash(cell, name, VAR(Asetf_tableA));
} // installSetfCell

static void installSpecialOperator(Val name, Val funtab)
{
    setf_gethash(list(Kspecial_operator), name, funtab);
    setf_symbol_function(make_not_function_function(name), name);
} // installSpecialOperator

void InstallStaticTlvRecord(
    Val tlvrec,
    Val name,
    Int iIndex,
    Val init )
{
    Val vartab = VAR(Aruntime_environmentA)->StaticCast<Environment>()->
        m_variables;

    TlvRecord* p = reinterpret_cast<TlvRecord*>(Record::Decode_(tlvrec));
    p->m_classd = TlvRecord::ClassD_();
    p->m_name   = name;
    p->m_index  = Fixnum::Encode(iIndex);
    p->m_value  = init;
    setf_gethash(tlvrec, name, VAR(Avalue_tableA));

    setf_svref(tlvrec, VAR(Atlv_vectorA), Fixnum::Encode(iIndex));

    Val frob = cons(Kspecial, list(cons(Kspecial, tlvrec)));
    setf_gethash(frob, name, vartab);

    *Thread::TlvPtr_(iIndex) = init;

    Val next_index = xxadd(p->m_index, one);

    if (VAR(Atlv_indexA) < next_index)
    {
        VAR(Atlv_indexA) = next_index;
    }
} // InstallStaticTlvRecord

void InstallStaticValueCell(
    Val cell,
    Val name,
    Val init,
    Val kind )
{
    ValueCell* p = reinterpret_cast<ValueCell*>(Record::Decode_(cell));
    p->m_classd = ValueCell::ClassD_();
    p->m_name   = name;
    p->m_kind   = kind;

    if (Fixnum::Encode(0) == p->m_value)
    {
        p->m_value  = init;
    }

    setf_gethash(cell, name, VAR(Avalue_tableA));

    Val frob;
    if (Kconstant == kind)
    {
        frob = cons(Kconstant, list(cons(Kconstant, init)));
    }
    else
    {
        frob = cons(Kspecial, list(cons(Kspecial, cell)));
    }

    Val vartab = VAR(Aruntime_environmentA)->StaticCast<Environment>()->
        m_variables;

    setf_gethash(frob, name, vartab);
} // InstallStaticValueCell

#define FUN_VAR(mp_name, mp_min, mp_max, mp_vals) \
    defun_optional(mp_name, mp_min, mp_max)

#define FUN_VAR_SETF(mp_name, mp_min, mp_max) \
    defun_setf_optional(mp_name, mp_min, mp_max)

#include "../rtl/tinycl_rtl.inc"

#define defun_2(mp_ty, mp_name) \
    mp_ty mp_name ## _2(Val a, Val b) { return (mp_name)(a, b); }

defun_2(bool, eq)
defun_2(bool, ge)
defun_2(bool, gt)
defun_2(bool, le)
defun_2(bool, lt)
defun_2(bool, ne)
defun_2(Val, add)
defun_2(Val, div)
defun_2(Val, mul)
defun_2(Val, sub)

#if 0
// Note: 2008-07-07 yosi@msn.com We use lisp version instead of C version
// for reducing executable size.
defun(C1_, (Val x)) { return sub(x, one); }
defun(C1P,  (Val x)) { return add(x, one); }
#endif

namespace
{

#include "./tinycl_init_entry.h"

#define Qeq_2       QQS2
#define Qge_2       QGQS2
#define Qgt_2       QGS2
#define Qle_2       QLQS2
#define Qlt_2       QLS2
#define Qne_2       QSQS2
#define Qadd_2      QPS2
#define Qdiv_2      QSS2
#define Qmul_2      QAS2
#define Qsub_1      Q_S1
#define Qsub_2      Q_S2
#define Qstring_eq  QstringQ

#define QC1P        Q1P
#define QC1_        Q1_

static const FunEntry k_rgoFunEntry[] =
{
    #include "../rtl/tinycl_rtl.inc"
}; // k_rgoFunEntry

#undef Qeq

static void installTypes()
{
    #define eql_(x) list(Qeql, x)

    // [A]
    InstallType(Qarray_rank,
        list(Qinteger, zero, Fixnum::Encode(Arch::ArrayRankLimit)) );

    // [B]
    InstallType(Qboolean,
        list(Qmember, nil, t) );

    // [F]
    InstallType(Qformat_control,
        list(Qor, Qfunction, Qstring) );

    InstallType(Qfunction_designator,
        list(Qor, Qfunction, Qsymbol) );

    InstallType(Qfunction_name,
        list(Qor,
            Qsymbol,
            list(Qcons, list(Qeql, Qsetf), list(Qcons, Qsymbol, Qnull)) ) );

    // [I]
    InstallType(Qinput_stream_designator,
        list(Qor,
            Qnull,
            list(Qeql, t),
            list(Qand, Qstream, list(Qsatisfies, Qinput_stream_p)) ) );

    // [O]
    InstallType(Qoutput_stream_designator,
        list(Qor,
            Qnull,
            list(Qeql, t),
            list(Qand, Qstream, list(Qsatisfies, Qoutput_stream_p))  ) );

    // [P]
    InstallType(Qpackage_designator,
        list(Qor, Qpackage, Qstring_designator) );

    InstallType(Qpathname_designator,
        list(Qor, Qpathname, Qstring_designator) );

    InstallType(Qpathname_device_designator,
        list(Qor, Qstring, Qnull, list(Qeql, Kunspecific)) );

    InstallType(Qpathname_directory_designator,
        list(Qor, Qstring, Qlist, eql_(Kunspecific), list(Qeql, Kwild)) );

    InstallType(Qpathname_host_designator,
        list(Qor, Qpathname_host, Qstring, list(Qeql, Kunspecific)) );

    InstallType(Qpathname_host,
        Qbasic_host );

    InstallType(Qpathname_name_designator,
        list(Qor, Qstring, Qlist, eql_(Kunspecific), eql_(Kwild)) );

    InstallType(Qpathname_type_designator,
        list(Qor, Qstring, Qlist, eql_(Kunspecific), eql_(Kwild)) );

    InstallType(Qpathname_version_designator,
        list(Qor,
                Qunsigned_byte,
                list(Qmember, Kwild, Knewest, Kunspecific, nil) ) );

    InstallType(Qproper_list,
        Qlist );

    // [R]
    InstallType(Qreadtable_case,
        list(Qmember, Kdowncase, Kinvert, Kpreserve, Kupcase) );

    InstallType(Qrestart_designator,
        list(Qor, Qrestart, list(Qand, Qsymbol, list(Qnot, Qnull))) );

    // [S]
    InstallType(Qsequence_end,
        list(Qor, Qnull, Qsequence_index) );

    InstallType(Qsequence_index,
        list(Qinteger, zero, Fixnum::Encode(Fixnum::MostPositive >> 1)) );

    InstallType(Qstring_designator,
        list(Qor, Qstring, Qcharacter, Qsymbol) );

    InstallType(Qtype_specifier,
        list(Qor, Qclass, Qcons, Qsymbol) );


    // [U]
    InstallType(Qunsigned_byte,
        list(Qinteger, zero, QA) );

    #undef eql_
} // installTypes

static Val makeStandardReadtable()
{
    Val rt = Thread::Get()->AllocRecord(CLASSD_readtable);
    Readtable* p = rt->StaticCast<Readtable>();

    #if USE_MODERN_MODE
        p->m_case = Kpreserve;
    #else
        p->m_case = Kupcase;
    #endif

    p->m_vector = make_vector(Fixnum::Encode(128));

    p->m_table = make_hash_table(
        Ksize, Fixnum::Encode(51),
        Ktest, Qeq );

    return rt;
} // makeStandardReadtable

static void makeStaticObjects(HMODULE hSelf)
{
    #define PRINT_BASE(mp_x) DEBUG_PRINTF(#mp_x "=%x\n", mp_x)

    PRINT_BASE(BINO_BASE);
    PRINT_BASE(CHAR_BASE);
    PRINT_BASE(RECO_BASE);
    PRINT_BASE(CLASSD_BASE);
    PRINT_BASE(CLASS_BASE);
    PRINT_BASE(SYMBOL_BASE);
    PRINT_BASE(SETF_BASE);
    PRINT_BASE(TLVREC_BASE);
    PRINT_BASE(VAR_BASE);
    PRINT_BASE(STATIC_OBJECT_END);

    // Nil
    {
        Null* p = reinterpret_cast<Null*>(
            RECO_BASE + offsetof(StaticReco, m_Nil) );

        p->m_classd   = CLASSD_simple_vector;

        p->m_length   = Fixnum::Encode(
            (sizeof(Null) - sizeof(Layout_data_vector)) /
            sizeof(Val) );

        p->m_Cons.m_car = nil;
        p->m_Cons.m_cdr = nil;
    }

    // Static Bino
    {
        StaticBino* p = reinterpret_cast<StaticBino*>(BINO_BASE);

        p->m_Free.m_classd    = CLASSD_marker;
        p->m_Free.m_value     = Kfree;

        p->m_Removed.m_classd = CLASSD_marker;
        p->m_Removed.m_value  = Kremoved;

        p->m_Unbound.m_classd = CLASSD_marker;
        p->m_Unbound.m_value  = Kunbound;

        p->m_zero64.m_classd = CLASSD_double_float;

        // PI = 3.14159265358979323846
        installFloat64(&p->m_pi64, 0x400921FB, 0x54442D18);

        #define defloat64(mp_pkg, mp_name, mp_s, mp_e, mp_h, mp_l) \
            installFloat64(&p->m_ ## mp_name, mp_s, mp_e, mp_h, mp_l);

        #define defloat32(mp_pkg, mp_name, mp_s, mp_e, mp_i) \
            installFloat32(&p->m_ ## mp_name, mp_s, mp_e, mp_i);

        #include "../tinycl_float.inc"

        p->m_bignum_most_positive_float32.m_classd = CLASSD_bignum;
        p->m_bignum_most_positive_float64.m_classd = CLASSD_bignum;

        #if 4 == SIZEOF_VAL
        {
            BignumImpl* const q =
                BIGNUM_most_positive_float32->StaticCast<BignumImpl>();

            uint const k = 128 / BigitBits + 1;
            q->m_length = Fixnum::Encode(k);
            q->GetStart()[k - 2] = 0xffffff;
        }
        {
            BignumImpl* const q =
                BIGNUM_most_positive_float64->StaticCast<BignumImpl>();

            uint const k = 1024 / BigitBits + 1;
            q->m_length = Fixnum::Encode(k);
            q->GetStart()[k - 2] = 0xffffffffu;
            q->GetStart()[k - 3] = 0xfffff800u;
        }
        #elif 8 == SIZEOF_VAL
        {
            BignumImpl* const q =
                BIGNUM_most_positive_float32->StaticCast<BignumImpl>();

            uint const k = 128 / sizeof(Bigit) + 1;
            q->m_length = Fixnum::Encode(k);
            q->GetStart()[k - 2] = 0xffffff0000000000llu;
        }
        {
            BignumImpl* const q =
                BIGNUM_most_positive_float64->StaticCast<BignumImpl>();

            uint const k = 1024 / sizeof(Bigit) + 1;
            q->m_length = Fixnum::Encode(k);
            q->GetStart()[k - 2] = 0xfffffffffffff800llu;
        }
        #else
            #error "Unsupported SIZEOF_VAL"
        #endif
    }

    ASSERT(car(nil) == nil);
    ASSERT(cdr(nil) == nil);

    #if DEBUG_CLASSD
        DEBUG_PRINTF("nil       = %p\n", nil);
        DEBUG_PRINTF("unbound   = %p\n", MARKER_unbound);
        DEBUG_PRINTF("free      = %p\n", MARKER_free);
        DEBUG_PRINTF("removed   = %p\n", MARKER_removed);
    #endif

    {
        #include "./tinycl_install_classd.h"
        #include "../tinycl_layout.inc"
    }

    {
        CLASSD_simple_string->StaticCast<ClassD>()->m_extra_length =
            Fixnum::Encode(1);

        #define initVectorClass(mp_name, mp_bits, mp_elty) \
        { \
            ClassD* p = CLASSD_ ## mp_name ->StaticCast<ClassD>(); \
            p->m_element_size = Fixnum::Encode(mp_bits); \
            p->m_element_type = mp_elty; \
            p->m_format       = reinterpret_cast<Val>( \
                ClassD::Format_BinVec ); \
        }

        CLASSD_storage->StaticCast<ClassD>()->m_format =
            reinterpret_cast<Val>(ClassD::Format_Storage);

        CLASSD_simple_vector->StaticCast<ClassD>()->m_format =
            reinterpret_cast<Val>(ClassD::Format_Vector);

        initVectorClass(
            bignum,
            Arch::BigitBits,
            list(Qunsigned_byte, Fixnum::Encode(Arch::BigitBits)) );

        initVectorClass(simple_string, 16, Qcharacter);

        initVectorClass(double_float_vector, 64, Qdouble_float);
        initVectorClass(single_float_vector, 32, Qsingle_float);

        initVectorClass(double_float_complex_vector, 128,
            list(Qcomplex, Qdouble_float) );

        initVectorClass(single_float_complex_vector, 64,
            list(Qcomplex, Qsingle_float) );

        initVectorClass(signed_byte_8_vector, 8,
            list(Qsigned_byte, Fixnum::Encode(8)) );

        initVectorClass(signed_byte_16_vector, 16,
            list(Qsigned_byte, Fixnum::Encode(16)) );

        initVectorClass(signed_byte_32_vector, 32,
            list(Qsigned_byte, Fixnum::Encode(32)) );

        initVectorClass(unsigned_byte_8_vector, 8,
            list(Qunsigned_byte, Fixnum::Encode(8)) );

        initVectorClass(unsigned_byte_16_vector, 16,
            list(Qunsigned_byte, Fixnum::Encode(16)) );

        initVectorClass(unsigned_byte_32_vector, 32,
            list(Qunsigned_byte, Fixnum::Encode(32)) );
    }

    // Make we can use VAR(name) macro
    {
        #define defvar(mp_pkg, mp_cname, mp_init) \
            VAR_ ## mp_cname->StaticCast<Record>()->m_classd = \
            CLASSD_value_cell;

        #include "../tinycl_vars.inc"
    }

    {
        VAR(ApackagesA) = nil;

        #include "./tinycl_install_object.h"
        #include "../tinycl_pkgs.inc"
    }

    // Install NIL
    {
        Symbol* p = reinterpret_cast<Symbol*>(
            &reinterpret_cast<Null*>(Null::Decode_(nil))->m_Symbol );

        Val strNIL = ImportSymbolString("NIL");
        p->m_classd    = CLASSD_null;
        p->m_function  = nil;
        p->m_hash_code = strNIL->StaticCast<SimpleString>()->Hash();
        p->m_name      = strNIL;
        p->m_package   = PKG_cl;
        p->m_plist     = nil;

        internal_package_put(
            PKG_cl->StaticCast<Package>()->m_external_table,
            nil );
    }

    // Install Static Symbols
    {
        #include "./tinycl_install_object.h"
        #include "../tinycl_keys.inc"
    }

    {
        #include "./tinycl_install_object.h"
        #include "../tinycl_syms_cl.inc"
    }

    {
        #define PKG_nil nil
        #include "./tinycl_install_object.h"
        #include "../tinycl_syms.inc"
    }

    // Install environment
    {
        Val env = Thread::Get()->AllocRecord(CLASSD_environment);
        Environment* pEnv = env->StaticCast<Environment>();

        pEnv->m_outer = nil;

        pEnv->m_classes = make_hash_table(
            Ksize, Fixnum::Encode(1003),
            Ktest, Qeq );

        pEnv->m_functions = make_hash_table(
            Ksize, Fixnum::Encode(1003),
            Ktest, Qeq );

        pEnv->m_others = make_hash_table(
            Ksize, Fixnum::Encode(1003),
            Ktest, Qeq );

        pEnv->m_types = make_hash_table(
            Ksize, Fixnum::Encode(1003),
            Ktest, Qeq );

        pEnv->m_variables = make_hash_table(
            Ksize, Fixnum::Encode(1003),
            Ktest, Qeq );

        VAR(Aruntime_environmentA) = env;
    }

    // Install variables
    {
        VAR(Asetf_tableA) = make_hash_table(
            Ksize,  Fixnum::Encode(1003),
            Ktest,  Qeq );

        // Note: Since zero is uninitailzied variable marker, so we
        // use one instead of zero.
        VAR(Atlv_indexA) = one;

        VAR(Atlv_vectorA) = make_vector(
            Fixnum::Encode(Arch::TlvLimits) );

        VAR(Avalue_tableA) = make_hash_table(
            Ksize,  Fixnum::Encode(1003),
            Ktest,  Qeq );

        #define TINYCL_LIST
        #include "./tinycl_install_object.h"
        #include "../tinycl_vars.inc"
        #include "../tinycl_tlvs.inc"
        #include "../rtl/tinycl_setfs.inc"
        #include "../tinycl_float.inc"
        #undef TINYCL_LIST
        #include "../tinycl_end_list.h"
    }

    // Install caller table
    VAR(Acaller_tableA) = make_hash_table(
        Ksize,  Fixnum::Encode(1003),
        Ktest,  Qeq );

    use_package(PKG_clos, PKG_si);
    TLV(ApackageA) = PKG_si;

    installTypes();

    s_ConstantlyNil = constantly(nil);

    {
        #define Qclassd     Qclass_description
        #define Qinstanced  Qinstance_description
        #define Qstoraged   Qstorage_description

        Val Qmember_Kinstance_Kclass   = list(Qmember, Kclass, Kinstance);
        Val Qmember_Kconstant_Kspecial = list(Qmember, Kconstant, Kspecial);

        Val Qor_classd_null             = list(Qor, Qclassd, Qnull);
        Val Qor_cons_symbol             = list(Qor, Qcons, Qsymbol);
        Val Qor_environment_null        = list(Qor, Qenvironment, Qnull);
        Val Qor_function_null           = list(Qor, Qfunction, Qnull);
        Val Qor_function_information_null =
            list(Qor, Qfunction_information, Qnull);
        Val Qor_generic_function_null   = list(Qor, Qgeneric_function, Qnull);
        Val Qor_hash_table_null         = list(Qor, Qhash_table, Qnull);
        Val Qor_latch_null              = list(Qor, Qlatch, Qnull);
        Val Qor_method_cache_null       = list(Qor, Qmethod_cache, Qnull);
        Val Qor_package_null            = list(Qor, Qpackage, Qnull);
        Val Qor_pathname_null           = list(Qor, Qpathname, Qnull);
        Val Qor_sequence_index_cons     = list(Qor, Qsequence_index, Qcons);
        Val Qor_simple_string_null      = list(Qor, Qsimple_string, Qnull);
        Val Qor_string_null             = list(Qor, Qstring, Qnull);
        Val Qor_dom_node_null           = list(Qor, Qdom_node, Qnull);
        Val Qor_dom_expanded_name_null  = list(Qor, Qdom_expanded_name, Qnull);

        #include "./tinycl_install_class.h"
        #include "../tinycl_layout.inc"

        CLASS_t->StaticCast<Class>()->m_class_precedence_list =
            list(CLASS_t);

        #include "./tinycl_finalize_class.h"
        #include "../tinycl_layout.inc"

        ClassFinalizer::SetSlotInitForm(
            CLASS_standard_generic_function,
            Qmethod_class,
            CLASS_standard_method,
            CLASS_standard_method );

        ClassFinalizer::InstallSlots(CLASS_record_object);
        ClassFinalizer::InstallSlots(CLASS_native_code_object);
        ClassFinalizer::InstallSlots(CLASS_instance);
        ClassFinalizer::InstallSlots(CLASS_funcallable_instance);

        ASSERT(
            Fixnum::Encode(sizeof(Layout_standard_class)) ==
            CLASSD_standard_class->StaticCast<ClassD>()->m_fixed_size );

        ASSERT(
            Fixnum::Encode(sizeof(Layout_classd)) ==
            CLASSD_classd->StaticCast<ClassD>()->m_fixed_size );

        // FIXME 2008-10-05 yosi@msn.com We should add typespec to C-defvar.
        installVarType(QAcaller_tableA,     Qhash_table);
        installVarType(QAcharset_tableA,    Qhash_table);
        installVarType(QAenvironmentA,      Qenvironment);
        installVarType(QAchar_name_tableA,  Qhash_table);
        installVarType(QApackageA,          Qpackage);
        installVarType(QApackagesA,         Qlist);
        installVarType(QAsetf_tableA,       Qhash_table);
        installVarType(QAtlv_vectorA,       Qsimple_vector);
        installVarType(QAvalue_tableA,      Qhash_table);
    }

    // Install character objects
    Platform_InstallStaticObjects();

    // Install char-name
    // This intialization requires character objects.
    {
        Val const name2ch = make_hash_table(Ktest, Qequalp);
        VAR(Aname_char_tableA) = name2ch;

        Val const ch2name = make_hash_table(Ktest, Qeq);
        VAR(Achar_name_tableA) = ch2name;

        #define defcharname(mp_code, mp_name) \
        { \
            Val ch   = Character::FromCode(mp_code); \
            Val name = make_string(mp_name); \
            setf_gethash(name, ch, ch2name); \
            setf_gethash(ch, name, name2ch); \
        }

        defcharname(0x09, "Tab");
        defcharname(0x0A, "Newline");
        defcharname(0x0B, "Backspace");
        defcharname(0x0C, "Page");
        defcharname(0x0D, "Return");
        defcharname(0x20, "Space");
    }

    DllRestart(hSelf);

    InstallStaticFunctions();
    InstallStaticMethods();

    // From now, we can allocate object using thread.
    InstallStaticFunctions(k_rgoFunEntry, lengthof(k_rgoFunEntry));
    InstallStaticFunctions2();

    {
        Val const funtab = VAR(Aruntime_environmentA)->
                StaticCast<Environment>()->m_functions;

        #define defspecial(mp_name) \
            installSpecialOperator(Q ## mp_name, funtab);

        #include "../compiler/tinycl_compiler.inc"

        setf_symbol_function(
            make_not_function_function(Qdeclare),
            Qdeclare );
    }

    // Install macro expander
    {
        #define defmacro(mp_name) \
            installStaticMacro(Q ## mp_name, "expand_" #mp_name);

        #include "../tinycl_macros.inc"
    }

    // [B]
    InstallPredicate(
        Qbit_vector_p,
        Qor,
        CLASSD_simple_bit_vector,
        CLASSD_bit_vector_object );

    INSTALL_STATIC_READER(byte_position, byte_specifier, position);
    INSTALL_STATIC_READER(byte_size,     byte_specifier, size);

    // [C]
    InstallPredicate(Qcharacterp, CLASSD_character);

    InstallPredicate(
        Qcomplexp,
        CLASSD_complex_min,
        CLASSD_complex_max );

    // [F]
    InstallPredicate(
        Qfloatp,
        Qor,
        Qdouble_float,
        Qsingle_float );

    // [H]
    InstallPredicate(Qhash_table_p, CLASSD_hash_table);

    INSTALL_STATIC_READER(hash_table_test, hash_table, test);

    // [N]
    InstallPredicate(Qnumberp, CLASSD_bignum, CLASSD_complex_max);

    // [R]
    INSTALL_STATIC_READER(readtable_case, readtable, case);
    INSTALL_STATIC_READER(restart_name, restart, name);

    // FIXME 2008-01-14 yosi@msn.com random-state-p
    InstallPredicate(Qrationalp, CLASSD_bignum, CLASSD_rational_max);
    InstallPredicate(Qreadtablep, CLASSD_readtable);
    InstallPredicate(Qrealp, CLASSD_bignum, CLASSD_real_max);

    // [S]
    InstallPredicate(Qsimple_bit_vector_p, CLASSD_simple_bit_vector);
    InstallPredicate(Qsimple_string_p, CLASSD_simple_string);
    InstallPredicate(Qsimple_vector_p, CLASSD_simple_vector);
    InstallPredicate(Qstringp, Qor, Qsimple_string, Qstring_object);
    InstallPredicate(Qsymbolp, CLASSD_symbol);

    // [V]
    InstallPredicate(Qvectorp, CLASSD_vector_min, CLASSD_vector_max);

    installAliasFunctions();

    InstallCompilerStatics();
} // makeStaticObjects

} // namespace

/// <summary>
///  Finalize static lisp objects.
/// </summary>
/// <param name="pth">Lisp thread object</param>
void Mm::FinalizeStatic(Thread* pth)
{
    foreach (Mm::EnumArea, oEnum, pth)
    {
        Mm::Area* pArea = oEnum.Get();
        if (0 == pArea->GetAge())
        {
            pArea->m_nFlags |= Mm::Area::Age_Static;

            switch (pArea->GetScanType())
            {
            case Area::ScanType_BinObj:
            case Area::ScanType_Code:
                pArea->SetReadOnly();
                break;
            } // switch scanType
        } // if
    } // for each area

    pth->Restart();

    TLV(ApackageA) = PKG_cl_user;
} // Mm::FinalizeStatic

/// <summary>
///   Initialize TinyCl runtime environment.
/// </summary>
/// <param name="pParam">
///   Configuration parameters for lisp environment.
/// </param>
/// <returns>Thread object for lisp environment</returns>
Thread* Initialize(const InitParams* pParams)
{
    new Executive;

    Thread* pth = Thread::PlatformStart(pParams);

    Executive::Get()->AddThread(pth);

    if (INVALID_HANDLE_VALUE == pParams->m_hImage)
    {
        makeStaticObjects(pParams->m_hSelf);

        if (NULL == pParams->m_pvStaticEnd)
        {
            Mm::FinalizeStatic(pth);
        }

        SxHashArea::Initialize();

        PlatformRestart(pth);
    }
    else
    {
        Mm::Load(pParams->m_hImage);
        DllRestart(pParams->m_hSelf);

        PlatformRestart(pth);

        Int n = Fixnum::Decode_(VAR(Atlv_indexA));

        Val* pStart =
            VAR(Atlv_vectorA)->StaticCast<SimpleVector>()->GetStart();

        Val* pEnd = pStart + n;

        Val* pTlv = Thread::TlvPtr_(1);

        for (Val* pval = pStart + 1; pval < pEnd; pval++)
        {
            Val tlvrec = *pval;
            TlvRecord* pTlvRec = tlvrec->StaticCast<TlvRecord>();
            *pTlv++ = pTlvRec->m_value;
        } // for pval
    } // if

    return pth;
} // Initialize

} // TinyCl
