#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - IR - Function Database
// tinycl_c_ir_fundb.cpp
//
// Copyright (C) 1996-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_fundb.cpp#31 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

class Static
{
    // [I]
    /// <summary>
    ///   Returns true if specified class has fixed layout.
    /// </summary>
    public: static bool IsFixedClass(Val const klass)
    {
        if (klass == CLASS_t)
        {
            return false;
        }

        Class* pClass = klass->StaticCast<Class>();
        if (nil == pClass->m_instanced)
        {
            return false;
        }

        if (typep(klass, CLASS_built_in_class))
        {
            return true;
        }

        if (typep(klass, CLASS_structure_class))
        {
            return true;
        }

        //= <FIXME date="2008-12-23" by="yosi@msn.com">
        //=  We should support fixed location of instance slot of
        //=  standard-class.
        //= </FIXME>

        return false;
    } // IsFixedClass
}; // StaticCast

class ComputeFunDbBase
{
    public: struct Entry
    {
        Val m_name;
        Operand* (*m_pfnCompute)(const CallI*);
    };// Entry
}; // ComputeFunDbBase

class ComputeFunDb :
    public FunDb_<ComputeFunDb, ComputeFunDbBase::Entry, 307>,
    public ComputeFunDbBase
{
    public: static ComputeFunDb* Create()
    {
        static Entry s_rgoEntry[] =
        {
            // [I]
            { Qidentity,  compute_identity },
        }; // s_rgoEntry

        ComputeFunDb* pFunDb = new ComputeFunDb;
        pFunDb->load(s_rgoEntry, lengthof(s_rgoEntry));
        return pFunDb;
    } // Create

    ////////////////////////////////////////////////////////////
    //
    // Function handlers
    //
    // [Comppute I]
    private: static Operand* compute_identity(const CallI* const pCallI)
    {
        if (ValuesI* const pValuesI =
                pCallI->GetVy()->GetDefI()->DynamicCast<ValuesI>() )
        {
            if (1 == pValuesI->CountOperands())
            {
                return pValuesI->GetSx();
            }
        }

        return NULL;
    } // compute_identity
}; // ComputeFunDb

ComputeFunDb* ComputeFunDb::sm_pFunDb;

/// <summary>
///   Entry definition of function database.
/// </summary>
class InlineFunDbBase
{
    /// <summary>
    ///   Function attributes.
    ///   <list type="table">
    ///     <item><term>Constantly</term>
    ///       <description>
    ///         Function can be applied constant folding.
    ///       </description>
    ///     </item>
    ///     <item><term>Voidy</term>
    ///       <description>
    ///         The value of function isn't constant, e.g. constructors,
    ///         however, we can remove function call when value isn't used.
    ///       </description>
    ///     </item>
    ///   </list>
    /// </summary>
    public: enum Flag
    {
        None        = 0,
        Constantly  = 1 << 0,
        Voidy       = 1 << 1,

        Pure = Constantly | Voidy,
    }; // Flag

    public: struct Entry
    {
        Val m_name;
        int m_rgfFlag;
        bool (*m_pfnOptimize)(CallI*);

        bool IsConstantly() const
            { return 0 != (m_rgfFlag & Constantly); }

        bool IsVoidy() const
            { return 0 != (m_rgfFlag & Voidy); }
    }; // Entry
}; // InlineFunDbBase

/// <summary>
///   Function database.
/// </summary>
class InlineFunDb :
    public FunDb_<InlineFunDb, InlineFunDbBase::Entry, 307>,
    public InlineFunDbBase
{
    public: static InlineFunDb* Create()
    {
        InlineFunDb* pFunDb = new InlineFunDb;
        pFunDb->init();
        return pFunDb;
    } // Create

    private: void init()
    {
        static Entry s_rgoEntry[] =
        {
            // [*]
            { QAS2,     Pure, optimize_AS2 },   // +/2
            { QPS2,     Pure, optimize_PS2 },   // +/2
            { QSS2,     Pure, optimize_SS2 },   // //2
            { Q_S2,     Pure, optimize__S2 },   // -/2

            { QGS2,     Pure, optimize_GS2 },   // >/2
            { QGQS2,    Pure, optimize_GQS2 },  // >=/2
            { QLS2,     Pure, optimize_LS2 },   // </2
            { QLQS2,    Pure, optimize_LQS2 },  // </2
            { QQS2,     Pure, optimize_QS2 },   // =/2
            { QSQS2,    Pure, optimize_SQS2 },  // /=/2

            // [C]
            { Qcar,         Pure, optimize_car },
            { Qcdr,         Pure, optimize_cdr },
            { QcharGS2,     Pure, optimize_charGS2 },   // char>/2
            { QcharGQS2,    Pure, optimize_charGQS2 },  // char>=/2
            { QcharLS2,     Pure, optimize_charLS2 },   // char</2
            { QcharLQS2,    Pure, optimize_charLQS2 },  // char</2
            { QcharQS2,     Pure, optimize_charQS2 },   // char=/2
            { QcharSQS2,    Pure, optimize_charSQS2 },  // char/=/2

            // [E]
            { Qendp,    Pure, optimize_endp },
            { Qeq,      Pure, optimize_eq },
            { Qeql,     Pure, optimize_eql },
            { Qequal,   Pure, optimize_equal },
            { Qerror,   None, optimize_error },

            // [F]
            { Qfirst,   Pure, optimize_car },
            { Qfloat,   Pure, optimize_float },

            // [H]
            { Qhash_table_count, Pure, optimize_hash_table_count },
            { Qhash_table_test,  Pure, optimize_hash_table_test },

            // [I]
            { Qidentity,    Pure,       optimize_identity },
            { Qintern,      Constantly, optimize_none },

            // [L]
            { Qlist,        Voidy, optimize_list },
            { QlistA,       Voidy, optimize_listA },
            { QlogandS2,    Pure,  optimize_logand },
            { QlogeqvS2,    Pure,  optimize_logeqv },
            { QlogiorS2,    Pure,  optimize_logior },
            { Qlognot,      Pure,  optimize_lognot },
            { QlogxorS2,    Pure,  optimize_logxor },

            // [R]
            { Qreadtable_case,  Pure, optimize_readtable_case },
            { Qrest,            Pure, optimize_cdr },

            // [S]
            { Qslot_value,          Pure, optimize_slot_value },
            { Qsymbol_function,     Pure, optimize_symbol_function},
            { Qsymbol_name,         Pure, optimize_symbol_name },
            { Qsymbol_package,      Pure, optimize_symbol_package },
            { Qsymbol_plist,        Pure, optimize_symbol_plist },

            // [V]
            { Qvalues_list,         Pure, optimize_values_list },

            // [setf C]
            { SETF_car,             0, optimize_SETF_car },
            { SETF_cdr,             0, optimize_SETF_cdr },

            // [setf F]
            { SETF_first,           0, optimize_SETF_car },
            { SETF_rest,            0, optimize_SETF_cdr },

            // [setf S]
            { SETF_slot_value,      0, optimize_SETF_slot_value },
        }; // s_rgoEntry

        load(s_rgoEntry, lengthof(s_rgoEntry));
    } // init

    // [E]
    // ShouldBe ty %r1 <= %s2
    // =>
    // RuntimeCast ty %r3 <= %s2
    private: static Operand* ensureTyped(
        const Type* const pty,
        Operand*    const pSx )
    {
        Register* const pR1 = pSx->DynamicCast<Register>();
        if (NULL == pR1)
        {
            return pSx;
        }

        ShouldBeI* const pShouldBeI =
            pR1->GetDefI()->DynamicCast<ShouldBeI>();

        if (NULL == pShouldBeI)
        {
            return pSx;
        }

        Operand*  const pS2 = pShouldBeI->GetSx();
        Register* const pR3 = new Register(Variable::Get(pR1));

        pR1->ReplaceAll(pR3);

        pShouldBeI->GetBB()->ReplaceI(
            new RuntimeCastI(pty, pR3, pS2),
            pShouldBeI );

        return pR3;
    } // ensureTyped

    // [F]
    private: static Float* fetchFloat(
        Instruction* const pCallI,
        Operand*     const pSx,
        Type*        const pty )
    {
        // BOX %fd <= %sx
        if (Register* pRx = pSx->DynamicCast<Register>())
        {
            if (BoxI* const pBoxI = pRx->GetDefI()->DynamicCast<BoxI>())
            {
                if (Float* pFx = pBoxI->GetSx()->StaticCast<Float>())
                {
                    if (pFx->GetDefI()->GetTy() == pty)
                    {
                        return pFx;
                    }
                }
            }
        } // if

        Operand* const pS1 = ensureTyped(pty, pSx);

        Float* const pFx = new Float(Variable::Get(pS1));

        pCallI->GetBB()->InsertBeforeI(
            new UnBoxI(pty, pFx, pS1),
            pCallI );

        return pFx;
    } // fetchFloat

    private: static Register* fetchInt(
        Instruction* const pCallI,
        Operand*     const pSx )
    {
        Type* const pty = tyInt;

        if (Register* pRx = pSx->DynamicCast<Register>())
        {
            if (BoxI* const pBoxI = pRx->GetDefI()->DynamicCast<BoxI>())
            {
                if (Register* pFx = pBoxI->GetSx()->StaticCast<Register>())
                {
                    if (pFx->GetDefI()->GetTy() == pty)
                    {
                        return pFx;
                    }
                }
            }
        } // if

        Operand* const pS1 = ensureTyped(pty, pSx);

        Register* const pFx = new Register(Variable::Get(pS1));

        pCallI->GetBB()->InsertBeforeI(
            new UnBoxI(pty, pFx, pS1),
            pCallI );

        return pFx;
    } // fetchInt

    private: static Register* fetchTyped(
        const Type* const pty,
        Register*   const pRx )
    {
        if (NULL == pRx)
        {
            return NULL;
        }

        if (! pRx->GetTy()->IsSubtypeOf(pty))
        {
            return NULL;
        }

        return ensureTyped(pty, pRx)->StaticCast<Register>();
    } // fetchTyped

    // [P]
    /// <summary>
    ///   Expands reader function into SLOT instruciton.
    /// </summary>
    private: static bool processReader(
        CallI* const pCallI,
        Val    const klass,
        Val    const klass2,
        Val    const slot_name )
    {
        Values* const pVy = pCallI->GetVy();
        if (NULL == pVy)
        {
            return false;
        }

        ValuesI* const pValuesI = pCallI->GetVy()->
            GetDefI()->DynamicCast<ValuesI>();
        if (NULL == pValuesI)
        {
            return false;
        }

        Register* const pRx = fetchTyped(
            Type::Parse(klass),
            pValuesI->GetRx() );
        if (NULL == pRx)
        {
            return false;
        }

        CLOG_SECTION(1, "<b>optimize</b> ~S", pCallI);

        Register* const pR1 = new Register;

        pCallI->GetBB()->InsertBeforeI(
            new SlotI(tyPtrT, pR1, klass2, slot_name, pRx),
            pCallI );

        if (Register* const pRd = pCallI->GetRd())
        {
            pCallI->GetBB()->ReplaceI(new LoadI(pRd, pR1), pCallI);
        }
        else if (Values* const pVd = pCallI->GetVd())
        {
            Register* const pR2 = new Register;
            pCallI->GetBB()->InsertBeforeI(new LoadI(pR2, pR1), pCallI);

            Values* const pV3 = new Values;
            pVd->ReplaceAll(pV3);
            pCallI->GetBB()->ReplaceI(new ValuesI(pV3, pR2), pCallI);
        }
        else
        {
            C_INTERNAL_ERROR("Call: Unexpected output");
        }

        return true;
    } // processReader

    private: static bool processReader(
        CallI* const pCallI,
        Val    const klass,
        Val    const slot_name )
    {
        return processReader(pCallI, klass, klass, slot_name);
    } // processReader

    #define defreader3(mp_fn, mp_class, mp_slot) \
        private: static bool optimize_ ## mp_fn(CallI* pCallI) \
        { \
            return processReader(pCallI, CLASS_ ## mp_class, Q ## mp_slot); \
        }

    #define defreader(mp_class, mp_slot) \
        defreader3(mp_class ## _ ## mp_slot, mp_class, mp_slot)

    // [R]
    private: static void replaceOutput(
        CallI*      const pCallI,
        Operand*    const pSx )
    {
        if (Register* const pRd = pCallI->GetRd())
        {
            pRd->ReplaceAll(pSx);
        }
        else if (Values* const pVd  = pCallI->GetVd())
        {
            pCallI->GetBB()->InsertAfterI(
                new ValuesI(pVd, pSx),
                pCallI );
        }
    } // replaceOutput

    // [Optmize *]
    private: template<class Instruction_>
        static bool optimize_Arith_(
            CallI* const pCallI )
    {
        const ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Operand* const pSx = pValuesI->GetSx();
        Operand* const pSy = pValuesI->GetSy();

        const Type* const pTx = pSx->GetTy();
        const Type* const pTy = pSy->GetTy();

        if (Subtype::Yes == pTx->IsSubtypeOf(tySingleFloat) &&
            Subtype::Yes == pTy->IsSubtypeOf(tySingleFloat) )
        {
            Float* const pFx = fetchFloat(pCallI, pSx, tyFloat32);
            Float* const pFy = fetchFloat(pCallI, pSy, tyFloat32);
            Float* const pFd = new Float(Variable::Get(pCallI->GetRd()));

            const Type* const pty = Type::And(tySingleFloat, pCallI->GetTy());

            pCallI->GetBB()->InsertBeforeI(
                new Instruction_(tyFloat32, pFd, pFx, pFy),
                pCallI );

            pCallI->GetBB()->ReplaceI(
                new BoxI(pty, pCallI->GetRd(), pFd),
                pCallI );
            return true;
        } // if single-float operation

        if (Subtype::Yes == pTx->IsSubtypeOf(tyDoubleFloat) &&
            Subtype::Yes == pTy->IsSubtypeOf(tyDoubleFloat) )
        {
            Float* const pFx = fetchFloat(pCallI, pSx, tyFloat64);
            Float* const pFy = fetchFloat(pCallI, pSy, tyFloat64);
            Float* const pFd = new Float(Variable::Get(pCallI->GetRd()));

            const Type* const pty = Type::And(tyDoubleFloat, pCallI->GetTy());

            pCallI->GetBB()->InsertBeforeI(
                new Instruction_(tyFloat64, pFd, pFx, pFy),
                pCallI );

            pCallI->GetBB()->ReplaceI(
                new BoxI(pty, pCallI->GetRd(), pFd),
                pCallI );

            return true;
        } // if double-float operation

        return false;
    } // optimize_Arith_

    // */2
    private: static bool optimize_AS2(CallI* const pCallI)
        { return optimize_Arith_<MulI>(pCallI); }

    // +/2
    private: static bool optimize_PS2(CallI* const pCallI)
        { return optimize_Arith_<AddI>(pCallI); }

    // //2
    private: static bool optimize_SS2(CallI* const pCallI)
        { return optimize_Arith_<DivI>(pCallI); }

    // -/2
    private: static bool optimize__S2(CallI* const pCallI)
        { return optimize_Arith_<SubI>(pCallI); }

    private: template<class Instruction_>
        static bool optimize_Cmp_(CallI* const pCallI)
    {
        const ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Operand* const pSx = pValuesI->GetSx();
        Operand* const pSy = pValuesI->GetSy();

        const Type* const pTx = pSx->GetTy();
        const Type* const pTy = pSy->GetTy();

        if (Subtype::Yes == pTx->IsSubtypeOf(tyFixnum) &&
            Subtype::Yes == pTy->IsSubtypeOf(tyFixnum) )
        {
            Bool*  const pB1 = new Bool;

            pCallI->GetBB()->InsertBeforeI(
                new Instruction_(pB1, pSx, pSy),
                pCallI );

            pCallI->GetBB()->ReplaceI(
                new IfI(pCallI->GetTy(), pCallI->GetRd(),
                            pB1, True, Literal::New(nil) ),
                pCallI );
            return true;
        } // if fixnum comparison

        //= <FIXME date="2008-12-24" by="yosi@msn.com">
        //=   We should support literal operand promotion. For example:
        //=     (> x 1) => (> x 1.0)
        //=     (> (the double-float x) 1f0) => (> x 1d0)
        //= </FIXME>
        if (Subtype::Yes == pTx->IsSubtypeOf(tySingleFloat) &&
            Subtype::Yes == pTy->IsSubtypeOf(tySingleFloat) )
        {
            Bool*  const pB1 = new Bool;
            Float* const pFx = new Float(Variable::Get(pSx));
            Float* const pFy = new Float(Variable::Get(pSy));

            pCallI->GetBB()->InsertBeforeI(
                new UnBoxI(tyFloat32, pFx, pSx),
                pCallI );

            pCallI->GetBB()->InsertBeforeI(
                new UnBoxI(tyFloat32, pFy, pSy),
                pCallI );

            pCallI->GetBB()->InsertBeforeI(
                new Instruction_(pB1, pFx, pFy),
                pCallI );

            pCallI->GetBB()->ReplaceI(
                new IfI(pCallI->GetTy(), pCallI->GetRd(),
                            pB1, True, Literal::New(nil) ),
                pCallI );

            return true;
        } // if single-float comparison

        if (Subtype::Yes == pTx->IsSubtypeOf(tyDoubleFloat) &&
            Subtype::Yes == pTy->IsSubtypeOf(tyDoubleFloat) )
        {
            Bool*  const pB1 = new Bool;
            Float* const pFx = new Float(Variable::Get(pSx));
            Float* const pFy = new Float(Variable::Get(pSy));

            pCallI->GetBB()->InsertBeforeI(
                new UnBoxI(tyFloat64, pFx, pSx),
                pCallI );

            pCallI->GetBB()->InsertBeforeI(
                new UnBoxI(tyFloat64, pFy, pSy),
                pCallI );

            pCallI->GetBB()->InsertBeforeI(
                new Instruction_(pB1, pFx, pFy),
                pCallI );

            pCallI->GetBB()->ReplaceI(
                new IfI(pCallI->GetTy(), pCallI->GetRd(),
                            pB1, True, Literal::New(nil) ),
                pCallI );

            return true;
        } // if double-float comparison

        return false;
    } // optimize_Cmp_

    private: static bool optimize_GS2(CallI* const pCallI)
        { return optimize_Cmp_<GeI>(pCallI); }

    private: static bool optimize_GQS2(CallI* const pCallI)
        { return optimize_Cmp_<GtI>(pCallI); }

    private: static bool optimize_LS2(CallI* const pCallI)
        { return optimize_Cmp_<LtI>(pCallI); }

    private: static bool optimize_LQS2(CallI* const pCallI)
        { return optimize_Cmp_<LeI>(pCallI); }

    private: static bool optimize_QS2(CallI* const pCallI)
        { return optimize_Cmp_<EqI>(pCallI); }

    private: static bool optimize_SQS2(CallI* const pCallI)
        { return optimize_Cmp_<NeI>(pCallI); }

    // [Optimize C]
    private: static bool optimize_car(CallI* const pCallI)
        { return processReader(pCallI, CLASS_list, CLASS_cons, Qcar); }

    private: static bool optimize_cdr(CallI* const pCallI)
        { return processReader(pCallI, CLASS_list, CLASS_cons, Qcdr); }

    private: template<class Instruction_>
        static bool optimize_CharCmp_(CallI* const pCallI)
    {
        const ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Operand* const pSx = pValuesI->GetSx();
        Operand* const pSy = pValuesI->GetSy();

        const Type* const pTx = pSx->GetTy();
        const Type* const pTy = pSy->GetTy();

        if (Subtype::Yes == pTx->IsSubtypeOf(tyCharacter) &&
            Subtype::Yes == pTy->IsSubtypeOf(tyCharacter) )
        {
            Bool* const pB1 = new Bool;

            pCallI->GetBB()->InsertBeforeI(
                new Instruction_(pB1, pSx, pSy),
                pCallI );

            pCallI->GetBB()->ReplaceI(
                new IfI(pCallI->GetTy(), pCallI->GetRd(),
                            pB1, True, Literal::New(nil) ),
                pCallI );
            return true;
        } // if character operation

        return false;
    } // optimize_charCmp_

    private: static bool optimize_charGS2(CallI* const pCallI)
            { return optimize_CharCmp_<GeI>(pCallI); }

    private: static bool optimize_charGQS2(CallI* const pCallI)
        { return optimize_CharCmp_<GtI>(pCallI); }

    private: static bool optimize_charLS2(CallI* const pCallI)
        { return optimize_CharCmp_<LtI>(pCallI); }

    private: static bool optimize_charLQS2(CallI* const pCallI)
        { return optimize_CharCmp_<LeI>(pCallI); }

    private: static bool optimize_charQS2(CallI* const pCallI)
        { return optimize_CharCmp_<EqI>(pCallI); }

    private: static bool optimize_charSQS2(CallI* const pCallI)
        { return optimize_CharCmp_<NeI>(pCallI); }

    // [Optimize E]
    //  VALUES %vy <= %sx
    //  CALL %rd <= #'endp %vy
    //  =>
    //  RUNTIMECAST list %r1 <= %sx
    //  NE %b2 <= %r1 'nil
    //  IF %r3 <= %b2 'nil True
    private: static bool optimize_endp(CallI* const pCallI)
    {
        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Operand* const pR1 = ensureTyped(tyList, pValuesI->GetSx());

        Bool* const pB2 = new Bool;
        pCallI->GetBB()->InsertBeforeI(
            new EqI(pB2, pR1, Literal::New(nil)),
            pCallI );

        Register* const pR3 = new Register;
        replaceOutput(pCallI, pR3);

        pCallI->GetBB()->ReplaceI(
            new IfI(tyT, pR3, pB2, True, Literal::New(nil)),
            pCallI );

        return true;
    } // optimize_endp

    private: static bool optimize_eq(CallI* const pCallI)
    {
        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Bool* const pB1 = new Bool;

        pCallI->GetBB()->InsertBeforeI(
            new EqI(pB1, pValuesI->GetSx(), pValuesI->GetSy()),
            pCallI );

        Register* const pR2 = new Register;

        replaceOutput(pCallI, pR2);

        pCallI->GetBB()->ReplaceI(
            new IfI(tyT, pR2, pB1, True, Literal::New(nil)),
            pCallI );

        return true;
    } // optimize_eq

    private: static bool optimize_eql(CallI* const pCallI)
    {
        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        const Type* const pTx = pValuesI->GetSx()->GetTy();
        const Type* const pTy = pValuesI->GetSy()->GetTy();
        const Type* const pTz = Type::And(pTx, pTy);

        if (pTz == tyNil)
        {
            CLOG(1, "Always false");
            replaceOutput(pCallI, Literal::New(nil));
            return true;
        }

        if (Subtype::Yes == pTz->IsSubtypeOf(tyFixnum))
        {
            return optimize_eq(pCallI);
        }

        if (Subtype::Yes == pTz->IsSubtypeOf(tyNumber))
        {
            return false;
        }

        return optimize_eq(pCallI);
    } // optimize_eql

    private: static bool optimize_equal(CallI* const pCallI)
    {
        class Local
        {
            public: static bool CanEq(const Type* const pTz)
            {
                if (tyNil != Type::And(pTz, tyList))
                {
                    return false;
                }

                if (tyNil != Type::And(pTz, tyString))
                {
                    return false;
                }

                if (tyNil != Type::And(pTz, tyPathname))
                {
                    return false;
                }

                if (tyNil != Type::And(pTz, tyBitVector))
                {
                    return false;
                }

                return true;
            } // CanEq
        }; // Local

        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        const Type* const pTx = pValuesI->GetSx()->GetTy();
        const Type* const pTy = pValuesI->GetSy()->GetTy();
        const Type* const pTz = Type::And(pTx, pTy);

        if (pTz == tyNil)
        {
            CLOG(1, "Always false");
            replaceOutput(pCallI, Literal::New(nil));
            return true;
        }

        if (Subtype::Yes == pTz->IsSubtypeOf(tyFixnum))
        {
            return optimize_eq(pCallI);
        }

        if (Subtype::Yes == pTz->IsSubtypeOf(tyNumber))
        {
            pCallI->GetOperandBox(0)->Replace(FunName::Intern(Qeql));
            return true;
        }

        if (Local::CanEq(pTz))
        {
            return optimize_eql(pCallI);
        }

        return false;
    } // optimize_equal

    private: static bool optimize_error(CallI* const pCallI)
    {
        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Instruction::EnumOperand oEnum(pValuesI);
        if (! oEnum.AtEnd())
        {
            if (Literal* const pLx = oEnum.Get()->DynamicCast<Literal>())
            {
                if (pLx->GetDatum() == Qtype_error)
                {
                    Operand* pDatum = NULL;
                    Operand* pExpectedType = NULL;
                    oEnum.Next();
                    enum State
                    {
                        State_Datum,
                        State_ExpectedType,
                        State_Key,
                    } eState = State_Key;
                    while (! oEnum.AtEnd())
                    {
                        if (State_Key == eState)
                        {
                            if (Literal* const pLx =
                                    oEnum.Get()->DynamicCast<Literal>() )
                            {
                                if (pLx->GetDatum() == Kdatum)
                                {
                                    eState = State_Datum;
                                }
                                else if (pLx->GetDatum() == Kexpected_type)
                                {
                                    eState = State_ExpectedType;
                                }
                                else
                                {
                                    break;
                                }
                            }
                        }
                        else if (State_Datum == eState)
                        {
                            if (NULL == pDatum)
                            {
                                pDatum = oEnum.Get();
                            }
                            
                            eState = State_Key;
                        }
                        else if (State_ExpectedType == eState)
                        {
                            if (NULL == pExpectedType)
                            {
                                pExpectedType= oEnum.Get();
                            }

                            eState = State_Key;
                        }

                        oEnum.Next();
                    } // while

                    if (oEnum.AtEnd() &&
                        NULL != pDatum &&
                        NULL != pExpectedType )
                    {
                        Values* const pVy = new Values;

                        pCallI->GetBB()->InsertBeforeI(
                            new ValuesI(pVy, pDatum, pExpectedType),
                            pCallI );

                        pCallI->GetOperandBox(0)->Replace(
                            Literal::New(QPtype_error) );

                        pCallI->GetOperandBox(1)->Replace(pVy);

                        return true;
                    }
                }
            } // if pLx
        } // if

        return false;
    } // optimize_error

    // [Optimize F]
    private: static bool optimize_float(CallI* const pCallI)
    {
        class Local
        {
            public: static bool Lower(
                Instruction* const pCallI,
                ValuesI*     const pValuesI )
            {
                Operand* const pSy = pValuesI->GetSy();
                const Type* const pTy = pSy->GetTy();
                if (Subtype::Yes == pTy->IsSubtypeOf(tySingleFloat))
                {
                    return Lower(pCallI, pValuesI, tySingleFloat, tyFloat32);
                }

                if (Subtype::Yes == pTy->IsSubtypeOf(tyDoubleFloat))
                {
                    return Lower(pCallI, pValuesI, tyDoubleFloat, tyFloat64);
                }

                return false;
            } // Lower

            public: static bool Lower(
                Instruction* const pCallI,
                ValuesI*     const pValuesI,
                Type*        const ptyOut,
                Type*        const ptyNat )
            {
                Operand*    const pSx = pValuesI->GetSx();
                const Type* const pTy = pSx->GetTy();

                if (Subtype::Yes == pTy->IsSubtypeOf(tyFixnum))
                {
                    emitConvert(
                        pCallI,
                        fetchInt(pCallI, pSx),
                        ptyOut,
                        ptyNat );
                    return true;
                } // if fixnum

                if (Subtype::Yes == pTy->IsSubtypeOf(tyDoubleFloat))
                {
                    emitConvert(
                        pCallI,
                        fetchFloat(pCallI, pSx, tyFloat64),
                        ptyOut,
                        ptyNat );
                    return true;
                } // if single-float

                if (Subtype::Yes == pTy->IsSubtypeOf(tySingleFloat))
                {
                    emitConvert(
                        pCallI,
                        fetchFloat(pCallI, pSx, tyFloat32),
                        ptyOut,
                        ptyNat );
                    return true;
                } // if single-float

                return false;
            } // Lower

            private: static void emitConvert(
                Instruction* const pCallI,
                Register*    const pR1,
                Type*        const ptyOut,
                Type*        const ptyNat )
            {
                Float* const pF2 = new Float(
                    Variable::Get(pCallI->GetRd()) );

                pCallI->GetBB()->InsertBeforeI(
                    new ConvertI(ptyNat, pF2, pR1),
                    pCallI );

                pCallI->GetBB()->ReplaceI(
                    new BoxI(ptyOut, pCallI->GetRd(), pF2),
                    pCallI );
            } // emitConvert
        }; // Local

        ValuesI* const pValuesI = pCallI->GetVy()->GetDefI()->
            StaticCast<ValuesI>();
        switch (pValuesI->CountOperands())
        {
        case 1:
            return Local::Lower(pCallI, pValuesI, tySingleFloat, tyFloat32);

        case 2:
            return Local::Lower(pCallI, pValuesI);
        } // switch arity
        return false;
    } // lower_float

    // [Optimize H]
    defreader(hash_table, count)
    defreader(hash_table, test)

    // [Optimize I]
    private: static bool optimize_identity(CallI* const pCallI)
    {
        Instruction* const pDefI = pCallI->GetVy()->GetDefI();
        if (! pDefI->Is<ValuesI>())
        {
            Register* const pR1 = new Register;

            replaceOutput(pCallI, pR1);

            const Type* pty = pDefI->GetTy()->GetPrimaryTy();

            pCallI->GetBB()->ReplaceI(
                new SelectI(pty, pR1, pCallI->GetVy(), 0),
                pCallI );

            return true;
        } // if ValuesI
        return false;
    } // optimize_identity

    // [Optimize L]
    private: static bool optimize_list(CallI* const pCallI)
    {
        if (tyList != pCallI->GetTy())
        {
            return false;
        }

        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        if (0 == pValuesI->CountOperands())
        {
            if (Register* const pRd = pCallI->GetRd())
            {
                pRd->ReplaceAll(Literal::New(nil));
                return true;
            }

            return false;
        }
        else
        {
            pCallI->SetTy(tyCons);
            return true;
        }
    } // optimize_list

    private: static bool optimize_listA(CallI* const pCallI)
    {
        ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        if (1 == pValuesI->CountOperands())
        {
            if (Register* const pRd = pCallI->GetRd())
            {
                pRd->ReplaceAll(pValuesI->GetSx());
                return true;
            }

            return false;
        }
        else
        {
            if (pCallI->GetTy() == tyCons)
            {
                return false;
            }

            pCallI->SetTy(tyCons);
            return true;
        }
    } // optimize_listA

    // <summary>
    //  Template class for logical arithmetic functions.
    //  <list>
    //    <item>CALL logand/2 fixnum fixnum => LOGAND fixnum fixnum</item>
    //  </list>
    // </summary>
    private: template<class Instruction_>
        static bool optimize_log_(CallI* const pCallI)
    {
        const ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Operand* const pSx = pValuesI->GetSx();
        const Type*    const pTx = pSx->GetTy();

        Operand* const pSy = pValuesI->GetSy();
        const Type*    const pTy = pSy->GetTy();

        if (Subtype::Yes == pTx->IsSubtypeOf(tyFixnum) &&
            Subtype::Yes == pTy->IsSubtypeOf(tyFixnum) )
        {
            pCallI->GetBB()->ReplaceI(
                new Instruction_(tyFixnum, pCallI->GetRd(), pSx, pSy),
                pCallI );
            return true;
        } // if fixnum operation

        return false;
    } // optimize_log_

    private: static bool optimize_logand(CallI* const pCallI)
        { return optimize_log_<LogAndI>(pCallI); }

    private: static bool optimize_logeqv(CallI* const pCallI)
        { return optimize_log_<LogEqvI>(pCallI); }

    private: static bool optimize_logior(CallI* const pCallI)
        { return optimize_log_<LogIorI>(pCallI); }

    private: static bool optimize_lognot(CallI* const pCallI)
    {
        const ValuesI* const pValuesI =
            pCallI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        Operand* const pSx = pValuesI->GetSx();
        const Type*    const pTx = pSx->GetTy();

        if (Subtype::Yes == pTx->IsSubtypeOf(tyFixnum))
        {
            Literal* const pLy = Literal::New(Fixnum::Encode(-1));

            pCallI->GetBB()->ReplaceI(
                new LogXorI(tyFixnum, pCallI->GetRd(), pSx, pLy),
                pCallI );
            return true;
        } // if fixnum

        return false;
    } // optimize_lognot

    private: static bool optimize_logxor(CallI* const pCallI)
        { return optimize_log_<LogXorI>(pCallI); }

    // [Optimize N]
    private: static bool optimize_none(CallI*)
        { return false; }

    // [Optimize R]
    defreader(readtable, case)

    // [Optimize S]
    private: static Layout_effective_slot_definition*
        computeEslotD(
            CallI*    const pCallI,
            Register* const pRx,
            Operand*  const pSy,
            Val*      const out_klass )
    {
        if (NULL == pRx)
        {
            return NULL;
        }

        Literal* const pLy = pSy->DynamicCast<Literal>();
        if (NULL == pLy)
        {
            return NULL;
        }

        Val const slot_name = pLy->GetDatum();
        if (! symbolp(slot_name))
        {
            Context::Get()->Error("Expect symbol instead of ~S", slot_name);
            return NULL;
        }

        TyClass* const pTyClass = pRx->GetTy()->DynamicCast<TyClass>();
        if (NULL == pTyClass)
        {
            CLOG(1, "<li>~S: Not class: ~S</li>", pCallI, pRx->GetTy());
            return NULL;
        }

        Val const klass = pTyClass->GetClass();
        if (! Static::IsFixedClass(klass))
        {
            CLOG(1, "<li>~S: Not sealed class: ~W</li>",
                pCallI, class_name(klass) );
            return NULL;
        }

        CLOG(1, "<li>~S is ~W</li>", pCallI, class_name(klass));

        Val const eslotd = find_slot(klass, slot_name);
        if (nil == eslotd)
        {
            Context::Get()->Note(pCallI,
                "~S doesn't have slot ~S.",
                klass, slot_name );

            return NULL;
        }

        *out_klass = klass;

        Layout_effective_slot_definition* const pESlotD =
            eslotd->StaticCast<Instance>()->
                GetStorage<Layout_effective_slot_definition>();

        return pESlotD;
    } // computeEslotD

    private: static bool optimize_slot_value(CallI* const pCallI)
    {
        ValuesI* const pValuesI = pCallI->GetVy()->
            GetDefI()->StaticCast<ValuesI>();

        Register* const pRx = pValuesI->GetRx();

        Val klass;
        const Layout_effective_slot_definition* const
            pESlotD = computeEslotD(
                pCallI,
                pRx,
                pValuesI->GetSy(),
                &klass );

        if (NULL == pESlotD)
        {
            return false;
        }

        const Type* const pty = Type::Parse(pESlotD->m_type);

        Register* const pR1 = new Register;
        pCallI->GetBB()->InsertBeforeI(
            new SlotI(tyPtrT, pR1, klass, pESlotD->m_name, pRx),
            pCallI );

        pCallI->GetBB()->ReplaceI(
            new LoadI(pty, pCallI->GetRd(), pR1),
            pCallI );

        return true;
    } // optimize_slot_value

    // [V]
    private: static bool optimize_values_list(CallI* const pCallI)
    {
        pCallI->GetOperandBox(0)->Replace(FunName::Intern(QvaluesA));
        return true;
    } // optimize_values_list

    // [Setf C]
    private: static bool optimize_SETF_cxr(
        CallI* const pCallI,
        Val const    name )
    {
        ValuesI* const pValuesI = pCallI->GetVy()->
            GetDefI()->StaticCast<ValuesI>();

        Register* const pRy = pValuesI->GetRy();
        if (NULL == pRy)
        {
            Context::Get()->Error("Can't clobber literal: ~S",
                pValuesI->GetLy() );
            return false;
        }

        if (tyNil == Type::And(tyCons, pRy->GetTy()))
        {
            Context::Get()->Error("Expect cons");
            return false;
        }

        CLOG(1, "<li>Lower ~S<ol>~%", pCallI);

        Operand* const pSx = pValuesI->GetSx();

        replaceOutput(pCallI, pSx);

        Register* pR1 = new Register;
        pCallI->GetBB()->InsertBeforeI(
            new SlotI(tyPtrT, pR1, CLASS_cons, name, pRy),
            pCallI );

        pCallI->GetBB()->ReplaceI(
            new StoreI(pR1, pSx),
            pCallI );

        CLOG(2, "</ol></li>~%");

        return true;
    } // optimize_SETF_cxr

    private: static bool optimize_SETF_car(CallI* const pCallI)
        { return optimize_SETF_cxr(pCallI, Qcar); }

    private: static bool optimize_SETF_cdr(CallI* const pCallI)
        { return optimize_SETF_cxr(pCallI, Qcdr); }

    // [Setf S]
    private: static bool optimize_SETF_slot_value(CallI* const pCallI)
    {
        ValuesI* const pValuesI = pCallI->GetVy()->
            GetDefI()->StaticCast<ValuesI>();

        Register* const pRx = pValuesI->GetRy();

        Val klass;
        const Layout_effective_slot_definition* const
            pESlotD = computeEslotD(
                pCallI,
                pRx,
                pValuesI->GetSz(),
                &klass );

        if (NULL == pESlotD)
        {
            return false;
        }

        Operand* const pSx = pValuesI->GetSx();

        replaceOutput(pCallI, pSx);

        Register* const pR1 = new Register;
        pCallI->GetBB()->InsertBeforeI(
            new SlotI(tyPtrT, pR1, klass, pESlotD->m_name, pRx),
            pCallI );

        pCallI->GetBB()->ReplaceI(
            new StoreI(pR1, pSx),
            pCallI );

        return true;
    } // optimize_SETF_slot_value

    defreader(symbol, function)
    defreader(symbol, name)
    defreader(symbol, package)
    defreader(symbol, plist)
}; // InlineFunDb

InlineFunDb* InlineFunDb::sm_pFunDb;

/// <summary>
///   Compute value of known function if all operands are constant.
/// </summary>
Operand* CallI::Compute() const
{
    if (IsNotinline())
    {
        return NULL;
    }

    if (GetTy() == tyNull)
    {
        return Literal::New(nil);
    }

    Values* const pVy = GetVy();
    if (NULL == pVy)
    {
        // Note: We set %void in CG pass.
        return NULL;
    }

    FunName* const pFunName = GetSx()->DynamicCast<FunName>();
    if (NULL == pFunName)
    {
        return NULL;
    }

    if (const ComputeFunDb::Entry* const pEntry =
                    ComputeFunDb::Find(pFunName->GetName()) )
    {
        return pEntry->m_pfnCompute(this);
    }

    const InlineFunDb::Entry* const
        pEntry = InlineFunDb::Find(pFunName->GetName());

    if (NULL == pEntry)
    {
        return NULL;
    }

    if (! pEntry->IsConstantly())
    {
        return NULL;
    }

    ValuesI* const pValuesI = pVy->GetDefI()->DynamicCast<ValuesI>();
    if (NULL == pValuesI)
    {
        return NULL;
    }

    foreach (EnumOperand, oEnum, pValuesI)
    {
        if (! oEnum.Get()->Is<Literal>())
        {
            return NULL;
        }
    } // for

    format(t, "; Debug-Note: Fold constant ~S~%", pFunName->GetName());

    //= <FIXME date="2008-12-25" by="yosi@msn.com">
    //=   We should move computing find-class value to another place.
    if (pFunName->GetName() == Qfind_class)
    {
        Val klass = funcall(Qfind_class, pValuesI->GetLx(), nil);
        if (nil == klass)
        {
            return NULL;
        }
        return Literal::New(klass);
    }
    //= </FIXME>

    Thread* const pth = Thread::Get();
    int iNth = 0;
    foreach (EnumOperand, oEnum, pValuesI)
    {
        pth->mv_value[iNth] = oEnum.Get()->StaticCast<Literal>()->GetDatum();
        iNth++;
    } // for
    pth->m_n = Fixnum::Encode(iNth);

    //= <FIXME date="2008-10-05" by="yosi@msn.com">
    //=   We should catch errors during computing value of function.
    //= </FIXME>
    return Literal::New(funcall_(pFunName->GetName()));
} // CallI::Compute

/// <summary>
///   Returns true if function is Voidy.
/// </summary>
bool CallI::IsUseless() const
{
    if (NULL == GetVy())
    {
        // Note: We set %void in CG pass.
        return false;
    }

    if (GetOutput() != Void)
    {
        return false;
    }

    FunName* const pFunName = GetSx()->DynamicCast<FunName>();
    if (NULL == pFunName)
    {
        return false;
    }

    const InlineFunDb::Entry* const pEntry =
        InlineFunDb::Find(pFunName->GetName());

    if (NULL == pEntry)
    {
        return false;
    }

    if (! pEntry->IsVoidy())
    {
        return false;
    }

    format(t, "; Void ~S~%", pFunName->GetName());
    return true;
} // CallI::IsUseless

/// <summary>
///   Optimizes CALL instruction if callee is knwon function.
/// </summary>
bool CallI::Optimize()
{
    bool fVoidy = false;

    if (Register* const pRd = GetRd())
    {
        if (pRd->IsEmpty())
        {
            SetOutput(Void);
            fVoidy = true;
        }
    }

    if (Register* const pRx = GetRx())
    {
        Instruction* const pDefI = pRx->GetDefI();

        const Type* ptyVal = NULL;

        if (LoadFunI* const pLoadFunI = pDefI->DynamicCast<LoadFunI>())
        {
            SetNotinline(pLoadFunI->IsNotinline());

            if (FunName* const pFunName =
                    pLoadFunI->GetSx()->DynamicCast<FunName>() )
            {
                CLOG(2, "<li>~S + ~S</li>~%", this, pLoadFunI);

                GetOperandBox(0)->Replace(pFunName);
                ptyVal = pFunName->GetFunty()->GetValueTy();
            }
            else if (Function* const pFun =
                        pLoadFunI->GetSx()->DynamicCast<Function>() )
            {
                CLOG(2, "<li>~S + ~S</li>~%", this, pLoadFunI);

                GetOperandBox(0)->Replace(pFun);

                if (TyFunction* const pFunty =
                        pFun->GetTy()->DynamicCast<TyFunction>() )
                {
                    ptyVal = pFunty->GetValueTy();
                }
            }
        }
        else if (ClosureI* const pClosureI = pDefI->DynamicCast<ClosureI>())
        {
            Function* const pFun =
                pClosureI->GetSx()->StaticCast<Function>();

            if (pFun->IsNotClosure())
            {
                CLOG(2, "<li>~S + ~S</li>~%", this, pClosureI);

                SetNotinline(pClosureI->IsNotinline());
                GetOperandBox(0)->Replace(pFun);
                ptyVal = pFun->GetFunty()->GetValueTy();
            }
        } // if

        if (NULL != ptyVal)
        {
            const Type* pty = Type::And(ptyVal, GetTy());
            if (tyNil == pty)
            {
                Context::Get()->Error("Type mismatched");
            }
            else
            {
                SetTy(pty);
            }
        }

        if (IsNotinline())
        {
            return false;
        }
    } // if callee is reg

    if (Function* const pFun = GetSx()->DynamicCast<Function>())
    {
        if (Values* const pVd = GetVd())
        {
            const Type* ptyVal = pFun->GetFunty()->GetValueTy();
            if (GetTy()->Equal(ptyVal))
            {
                return false;
            }

            if (! ptyVal->Is<TyValues>())
            {
                Register* const pRd = new Register;
                SetOutput(pRd);

                TyValues* const ptyValues = TyValues::Parse(
                    list(Qvalues, ptyVal->Unparse()) );

                GetBB()->InsertAfterI(
                    new ValuesI(ptyValues, pVd, pRd),
                    this );
            }

            CLOG(2, "<li>Change ~S to ~S</li>", this, ptyVal);
            SetTy(ptyVal);
        }
        else
        {
            const Type* ptyVal = pFun->GetFunty()->GetValueTy()->
                GetPrimaryTy();

            if (GetTy()->Equal(ptyVal))
            {
                return false;
            }

            CLOG(2, "<li>Change ~S to ~S</li>", this, ptyVal);
            SetTy(ptyVal);
        } // if
        return true;
    } // if function

    if (IsNotinline())
    {
        return false;
    }

    FunName* const pFunName = GetSx()->DynamicCast<FunName>();
    if (NULL == pFunName)
    {
        // Unknown function or known function marked notinline.
        return false;
    }

    const InlineFunDb::Entry* const pEntry =
        InlineFunDb::Find(pFunName->GetName());

    if (NULL == pEntry)
    {
        return false;
    }

    if (fVoidy && pEntry->IsVoidy())
    {
        return true;
    }

    Values* const pVy = GetVy();
    if (NULL == pVy)
    {
        // Note: We set %void in CG pass.
        return false;
    }

    ValuesI* const pValuesI = pVy->GetDefI()->DynamicCast<ValuesI>();
    if (NULL == pValuesI)
    {
        return false;
    }

    CLOG_SECTION(2, "<b>optimize</b> ~S", this);
    return pEntry->m_pfnOptimize(this);
} // CallI::Optimize

} // Compiler
} // TinyCl
