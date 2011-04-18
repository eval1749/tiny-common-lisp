#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler
// compiler/tinycl_c_init.cpp
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/tinycl_c_init.cpp#31 $
//
#include "./tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

inline Val acons(Val key, Val val, Val alist)
    { return cons(cons(key, val), alist); }

static void installFunAnnot(
    Val const fname,
    Val const key,
    Val const val)
{
    Environment* const pEnv =
        VAR(Aruntime_environmentA)->StaticCast<Environment>();

    Val const frob = gethash(fname, pEnv->m_functions);
    if (Kfunction != car(frob))
    {
        error("Missing ftype ~S", fname);
    }

    setf_cdr(acons(key, val, cdr(frob)), frob);
} // installFunAnnot

static void installFunType(Val const fname, Val const funty)
{
    Environment* const pEnv =
        VAR(Aruntime_environmentA)->StaticCast<Environment>();

    Val const frob = gethash(fname, pEnv->m_functions);
    if (nil != frob)
    {
        error("Duplicated ftype ~S", fname);
    }

    setf_gethash(
        list(Kfunction, cons(Qftype, funty)),
        fname,
        pEnv->m_functions );
} // installFunType

static void installFunDb()
{
    class CachedList
    {
        private: struct Slot
        {
            Val m_val;
        }; // Slot

        private: int  m_cHits;
        private: int  m_cTries;
        private: Slot m_rgoEntry[1503];

        public: CachedList() :
            m_cHits(0),
            m_cTries(0)
        {
            ::ZeroMemory(&m_rgoEntry, sizeof(m_rgoEntry));
        } // CachedList

        public: ~CachedList()
        {
            DEBUG_PRINTF("CachedList: %d/%d\n", m_cHits, m_cTries);
        } // ~CachedList

        private: Val get(const Val* const pv, int const c)
        {
            Int const nHash = hash(pv, c);
            Slot* const pTop   = m_rgoEntry;
            Slot* const pBtm   = m_rgoEntry + lengthof(m_rgoEntry);
            Slot* const pStart = m_rgoEntry + (nHash % lengthof(m_rgoEntry));
            Slot* pRunner = pStart;

            m_cTries += 1;

            for (;;)
            {
                Val const present = pRunner->m_val;
                if (0 == present)
                {
                    return nil;
                }

                if (match(present, pv, c))
                {
                    m_cHits += 1;
                    return present;
                }

                pRunner++;
                if (pRunner >= pBtm)
                {
                    pRunner = pTop;
                }

                if (pRunner == pStart)
                {
                    error("Cons cache is full.");
                }
            } // for
        } // put

        private: int hash(const Val* const pvStart, int const c)
        {
            Int iHash = 0;
            const Val* pvEnd = pvStart + c;
            for (const Val* pv = pvStart; pv < pvEnd; pv++)
            {
                iHash <<= 3;
                iHash ^= (*pv)->ToInt() >> Fixnum::TagBits;
                pv++;
            } // for pv
            return iHash & Fixnum::MostPositive;
        } // hash

        private: Val intern(int const c)
        {
            Val mv[50];

            ASSERT(c >= 1);
            ASSERT(c <= lengthof(mv));

            Thread* const pth = Thread::Get();

            ::CopyMemory(mv, pth->mv_value, sizeof(Val) * c);

            for (int k = 0; k < c; k += 1)
            {
                Val const present = get(mv + k, c - k);
                if (nil != present)
                {
                    if (0 == k)
                    {
                        return present;
                    }

                    ::CopyMemory(pth->mv_value, mv, sizeof(Val) * k);
                    pth->mv_value[k] = present;
                    pth->m_n = Fixnum::Encode(k + 1);
                    pth->m_fn = symbol_function(QlistA);

                    return putAll(mv, c, CallLisp(pth));
                } // if
            } // for k

            pth->m_n = Fixnum::Encode(c);
            pth->m_fn = symbol_function(Qlist);
            return putAll(mv, c, CallLisp(pth));
        } // intern

        private: Val put(const Val* const pv, int const c, Val list)
        {
            Int const nHash = hash(pv, c);
            Slot* const pTop   = m_rgoEntry;
            Slot* const pBtm   = m_rgoEntry + lengthof(m_rgoEntry);
            Slot* const pStart = m_rgoEntry + (nHash % lengthof(m_rgoEntry));
            Slot* pRunner = pStart;

            for (;;)
            {
                Val const present = pRunner->m_val;
                if (0 == present)
                {
                    pRunner->m_val = list;
                    return list;
                }

                ASSERT(! match(present, pv, c));

                pRunner++;
                if (pRunner >= pBtm)
                {
                    pRunner = pTop;
                }

                if (pRunner == pStart)
                {
                    error("Cons cache is full.");
                }
            } // for
        } // put

        private: Val putAll(const Val* const pv, int const c, Val const list)
        {
            put(pv, c, list);

            if (c >= 2)
            {
                if (get(pv + 1, c - 1) == nil)
                {
                    putAll(pv + 1, c - 1, cdr(list));
                }
            }

            return list;
        } // putAll

        public: Val Make()
            { return nil; }

        public: Val Make(Val const a)
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            return intern(1);
        } // Make

        public: Val Make(Val const a, Val const b)
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            return intern(2);
        } // Make

        public: Val Make(Val const a, Val const b, Val const c)
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            return intern(3);
        } // Make

        public: Val Make(Val const a, Val const b, Val const c, Val const d)
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            return intern(4);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            return intern(5);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            return intern(6);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            return intern(7);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            return intern(8);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            return intern(9);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            return intern(10);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            return intern(11);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            return intern(12);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l,
            Val const m )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            pth->mv_value[12] = m;
            return intern(13);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l,
            Val const m,
            Val const n )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            pth->mv_value[12] = m;
            pth->mv_value[13] = n;
            return intern(14);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l,
            Val const m,
            Val const n,
            Val const o )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            pth->mv_value[12] = m;
            pth->mv_value[13] = n;
            pth->mv_value[14] = o;
            return intern(15);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l,
            Val const m,
            Val const n,
            Val const o,
            Val const p )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            pth->mv_value[12] = m;
            pth->mv_value[13] = n;
            pth->mv_value[14] = o;
            pth->mv_value[15] = p;
            return intern(16);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l,
            Val const m,
            Val const n,
            Val const o,
            Val const p,
            Val const q )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            pth->mv_value[12] = m;
            pth->mv_value[13] = n;
            pth->mv_value[14] = o;
            pth->mv_value[15] = p;
            pth->mv_value[16] = q;
            return intern(17);
        } // Make

        public: Val Make(
            Val const a,
            Val const b,
            Val const c,
            Val const d,
            Val const e,
            Val const f,
            Val const g,
            Val const h,
            Val const i,
            Val const j,
            Val const k,
            Val const l,
            Val const m,
            Val const n,
            Val const o,
            Val const p,
            Val const q,
            Val const r )
        {
            Thread* pth = Thread::Get();
            pth->mv_value[0] = a;
            pth->mv_value[1] = b;
            pth->mv_value[2] = c;
            pth->mv_value[3] = d;
            pth->mv_value[4] = e;
            pth->mv_value[5] = f;
            pth->mv_value[6] = g;
            pth->mv_value[7] = h;
            pth->mv_value[8] = i;
            pth->mv_value[9] = j;
            pth->mv_value[10] = k;
            pth->mv_value[11] = l;
            pth->mv_value[12] = m;
            pth->mv_value[13] = n;
            pth->mv_value[14] = o;
            pth->mv_value[15] = p;
            pth->mv_value[16] = q;
            pth->mv_value[17] = r;
            return intern(18);
        } // Make

        private: bool match(Val x, const Val* pvStart, int c)
        {
            const Val* pvEnd = pvStart + c;
            for (const Val* pv = pvStart; pv < pvEnd; pv++)
            {
                if (nil == x)
                {
                    return false;
                }

                if (pop(x) != *pv)
                {
                    return false;
                }
            } // for pv
            return nil == x;
        } // match
    } oC;

    #define _ oC.Make

    #define defannot(mp_name, mp_key, mp_val) \
        installFunAnnot(Q ## mp_name, Q ## mp_key, mp_val)

    #define defftype(mp_name, mp_ll, mp_v) \
        installFunType( \
            Q ## mp_name, \
            oC.Make(Qfunction, oC.Make mp_ll, mp_v) )

    #define defftype_setf(mp_name, mp_ll, mp_v) \
        installFunType( \
            SETF_ ## mp_name, \
            oC.Make(Qfunction, oC.Make mp_ll, mp_v) )

    #define deflist(mp_name) \
    { \
        defftype(mp_name, (Qlist), t);  \
        defftype_setf(mp_name, (t, Qcons), t);  \
    }

    #define eql_(x) \
        oC.Make(Qeql, x)

    #define member_(...) \
        oC.Make(Qmember, __VA_ARGS__)

    #define or_(...) \
        oC.Make(Qor, __VA_ARGS__)

    #define values_(...) \
        oC.Make(Qvalues, __VA_ARGS__)

    Val const values_Arest_t = values_(QArest, t);

    // [!]
    defftype(Bmake_closure, (QArest, t), Qfunction);

    // [+]
    defftype(P, (QArest, Qnumber), Qnumber);            // +
    defftype(_, (Qnumber, QArest, Qnumber), Qnumber);   // -
    defftype(A, (QArest, Qnumber), Qnumber);            // *
    defftype(S, (Qnumber, QArest, Qnumber), Qnumber);   // /

    defftype(_S1, (Qnumber), Qnumber);                  // -/1
    defftype(SS1, (Qnumber), Qnumber);                  // //1

    defftype(PS2, (Qnumber, Qnumber), Qnumber);         // +/2
    defftype(_S2, (Qnumber, Qnumber), Qnumber);         // -/2
    defftype(AS2, (Qnumber, Qnumber), Qnumber);         // */2
    defftype(SS2, (Qnumber, Qnumber), Qnumber);         // //2

    defftype(G,  (Qreal, QArest, Qreal), t);            // >
    defftype(GQ, (Qreal, QArest, Qreal), t);            // >=
    defftype(L,  (Qreal, QArest, Qreal), t);            // <
    defftype(LQ, (Qreal, QArest, Qreal), t);            // <=
    defftype(Q,  (Qnumber, QArest, Qnumber), t);        // =
    defftype(SQ, (Qnumber, QArest, Qnumber), t);        // /=

    defftype(GS2,  (Qreal, Qreal), t);                  // >/2
    defftype(GQS2, (Qreal, Qreal), t);                  // >=/2
    defftype(LS2,  (Qreal, Qreal), t);                  // </2
    defftype(LQS2, (Qreal, Qreal), t);                  // <=/2
    defftype(QS2,  (Qnumber, Qnumber), t);              // =/2
    defftype(SQS2, (Qnumber, Qnumber), t);              // /=/2

    // [A]
    defftype(abort, (QAoptional, or_(Qcondition, Qnull)), nil);
    defftype(acons, (t, t, t), Qlist);
    defftype(address_of, (t), Qfixnum);

    defftype(adjoin,
        (t, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(append, (QArest, t), t);
    defftype(appendS2, (Qproper_list, t), t);

    defftype(apply, (Qfunction_designator, QArest, t),
        values_Arest_t );

    defftype(ash, (Qinteger, Qinteger), Qinteger);

    defftype(assoc,
        (t, Qlist, QAkey,
            _(Kkey, Qfunction_designator),
            _(Ktest, Qfunction_designator) ),
        t );

    defftype(assoc_if,
        (Qfunction_designator, Qlist, QAkey,
            _(Kkey, Qfunction_designator) ),
        t );

    defftype(assoc_if_not,
        (Qfunction_designator, Qlist, QAkey,
            _(Kkey, Qfunction_designator) ),
        t );

    // [B]
    defftype(both_case_p, (Qcharacter), t);
    defftype(bounding_index_error, (Qsequence, t, t), nil);
    defftype(break, (Qformat_control, QArest, t), Qnull);
    defftype(butlast, (Qlist, QAoptional, Qsequence_index), Qlist);
    defftype(byte, (Qunsigned_byte, Qunsigned_byte), Qbyte_specifier);
    defftype(byte_position, (Qbyte_specifier), Qunsigned_byte);
    defftype(byte_size,     (Qbyte_specifier), Qunsigned_byte);

    // [C]
    deflist(car);
    deflist(cdr);

    deflist(caar);
    deflist(cadr);
    deflist(cdar);
    deflist(cddr);

    deflist(caaar);
    deflist(caadr);
    deflist(cadar);
    deflist(caddr);
    deflist(cdaar);
    deflist(cdadr);
    deflist(cddar);
    deflist(cdddr);

    deflist(caaaar);
    deflist(caaadr);
    deflist(caadar);
    deflist(caaddr);
    deflist(cadaar);
    deflist(cadadr);
    deflist(caddar);
    deflist(cadddr);
    deflist(cdaaar);
    deflist(cdaadr);
    deflist(cdadar);
    deflist(cdaddr);
    deflist(cddaar);
    deflist(cddadr);
    deflist(cdddar);
    deflist(cddddr);

    defftype(ceiling, (Qreal, QAoptional, Qreal), values_(Qinteger, Qreal));

    defftype(charG,  (Qcharacter, QArest, Qcharacter), t);  // char>
    defftype(charGQ, (Qcharacter, QArest, Qcharacter), t);  // char>=
    defftype(charL,  (Qcharacter, QArest, Qcharacter), t);  // char<
    defftype(charLQ, (Qcharacter, QArest, Qcharacter), t);  // char<=
    defftype(charQ,  (Qcharacter, QArest, Qcharacter), t);  // char=
    defftype(charSQ, (Qcharacter, QArest, Qcharacter), t);  // char/=

    defftype(charGS2,  (Qcharacter, Qcharacter), t);  // char>/2
    defftype(charGQS2, (Qcharacter, Qcharacter), t);  // char>=/2
    defftype(charLS2,  (Qcharacter, Qcharacter), t);  // char</2
    defftype(charLQS2, (Qcharacter, Qcharacter), t);  // char<=/2
    defftype(charQS2,  (Qcharacter, Qcharacter), t);  // char=/2
    defftype(charSQS2, (Qcharacter, Qcharacter), t);  // char/=/2

    defftype(char_code,
        (Qcharacter),
        _(Qinteger, zero, Fixnum::Encode(Character::Max)) );

    defftype(char_greaterp,     (Qcharacter, QArest, Qcharacter), t);   // char-greaterp
    defftype(char_not_lessp,    (Qcharacter, QArest, Qcharacter), t);   // char-not-lessp
    defftype(char_lessp,        (Qcharacter, QArest, Qcharacter), t);   // char-lessp
    defftype(char_not_greaterp, (Qcharacter, QArest, Qcharacter), t);   // char-not-greaterp
    defftype(char_equal,        (Qcharacter, QArest, Qcharacter), t);   // char-equal
    defftype(char_not_equal,    (Qcharacter, QArest, Qcharacter), t);   // char-not-equal

    defftype(code_char,
        (_(Qinteger, zero, Fixnum::Encode(Character::Max))),
        _(Qcharacter, Qnull) );

    defftype(compute_restarts,
        (QAoptional, or_(Qcondition, Qnull)),
        Qlist );

    defftype(cons, (t, t), Qcons);
    defftype(continue, (QAoptional, or_(Qcondition, Qnull)), nil);

    defftype(copy_alist, (Qlist), Qlist);
    defftype(copy_list, (Qlist), Qlist);
    defftype(copy_tree, (t), t);

    defftype(copy_pprint_dispatch,
        (QAoptional, or_(Qpprint_dispatch_table, Qnull)),
        Qpprint_dispatch_table );

    // [D]
    defftype(delete,
        (t, Qsequence, QAkey,
            _(Kcount, t),
            _(Kend, Qsequence_end),
            _(Kfrom_end, t),
            _(Kkey, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qsequence );

    defftype(delete_if,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kcount, t),
            _(Kend, Qsequence_end),
            _(Kfrom_end, t),
            _(Kkey, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qsequence );

    defftype(delete_if_not,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kcount, t),
            _(Kend, Qsequence_end),
            _(Kfrom_end, t),
            _(Kkey, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qsequence );

    defftype(deposit_field, (Qinteger, Qbyte_specifier, Qinteger), Qinteger);
    defftype(dpb,           (Qinteger, Qbyte_specifier, Qinteger), Qinteger);

    // [E]
    deflist(eighth);
    defftype(endp,      (Qlist), t);
    defftype(eq,        (t, t), t);
    defftype(eql,       (t, t), t);
    defftype(equal,     (t, t), t);
    defftype(equalp,    (t, t), t);
    defftype(error,     (Qcondition_designator, QArest, t), nil);

    // [F]
    defftype(fceiling,  (Qreal, QAoptional, Qreal), values_(Qfloat, Qreal));
    defftype(ffloor,    (Qreal, QAoptional, Qreal), values_(Qfloat, Qreal));

    deflist(fifth);

    defftype(find_all_symbols,
        (Qstring_designator),
        Qlist );

    defftype(find_class,
        (Qsymbol, QAoptional, t, or_(Qenvironment, Qnull)),
        or_(Qclass, Qnull) );

    defftype(find_package,
        (Qstring_designator),
        or_(Qpackage, Qnull) );

    defftype(find_restart,
        (Qrestart_designator, QAoptional, or_(Qcondition, Qnull)),
        or_(Qrestart, Qnull) );

    defftype(find_value_cell, (Qsymbol),
        or_(Qtlv_record, Qvalue_cell, Qnull) );

    defftype(find_setf_cell, (Qsymbol), 
        or_(Qsetf_cell,  Qnull) );

    defftype(find_symbol,
        (Qstring, QAoptional, Qpackage_designator),
        values_(
            Qsymbol,
            member_(nil, Kexternal, Kinherited, Kinternal) ) );

    deflist(first);

    defftype(float, (Qreal, QAoptional, Qfloat), Qfloat);
    defftype(floor, (Qreal, QAoptional, Qreal), values_(Qinteger, Qreal));

    defftype(format,
        (or_( Qstream, Qnull, eql_(t), Qstring),
         Qformat_control,
         QArest, t ),
        or_(Qstring, Qnull) );

    deflist(fourth);
    defftype(fround,    (Qreal, QAoptional, Qreal), values_(Qfloat, Qreal));
    defftype(ftruncate, (Qreal, QAoptional, Qreal), values_(Qfloat, Qreal));

    defftype(funcall, (Qfunction_designator, QArest, t),
        values_Arest_t );

    // [G]
    defftype(getf,
        (Qproper_list, t, QAoptional),
        t );

    defftype_setf(getf,
        (t, Qproper_list, t, QAoptional),
        t );

    defftype(get_properties,
        (Qproper_list, Qproper_list),
        values_(t, t, Qlist) );

    // [I]
    defftype(identity, (t), t);

    defftype(intern,
        (Qstring, QAoptional, Qpackage_designator),
        values_(
            Qsymbol,
            member_(nil, Kexternal, Kinherited, Kinternal) ) );

    defftype(intern_value_cell,
        (Qsymbol, member_(Kconstant, Kspecial)),
        or_(Qtlv_record, Qvalue_cell) );

    defftype(intern_setf_cell,  (Qsymbol), Qsetf_cell);

    defftype(intersection,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(invalid_method_error, (Qmethod, Qformat_control, QArest, t), t);
    defftype(invoke_debugger, (Qcondition), nil);

    defftype(invoke_restart,
        (Qrestart_designator, QArest, t),
        values_Arest_t );

    defftype(invoke_restart_interactively,
        (Qrestart_designator, QArest, t),
        values_Arest_t );

    // [L]
    defftype(last,   (Qlist, QAoptional, Qsequence_index), t);

    defftype(ldb,      (Qbyte_specifier, Qinteger), Qunsigned_byte);
    defftype(ldb_test, (Qbyte_specifier, Qinteger), Qt);
    defftype(ldiff, (Qlist, t), Qlist);

    defftype(list,   (QArest, t),        Qlist);
    defftype(listA,  (t, QArest, t),     t);
    defftype(list_all_packages, (), Qlist);
    defftype(list_length, (Qlist), or_(Qsequence_index, Qnull));

    defftype(logand,    (QArest, Qinteger),     Qinteger);
    defftype(logandc1,  (Qinteger, Qinteger),   Qinteger);
    defftype(logandc2,  (Qinteger, Qinteger),   Qinteger);
    defftype(logandS2,  (Qinteger, Qinteger),   Qinteger);
    defftype(logbitp,   (Qunsigned_byte, Qinteger), t);
    defftype(logcount,  (Qinteger),             Qunsigned_byte);
    defftype(logeqv,    (QArest, Qinteger),     Qinteger);
    defftype(logeqvS2,  (Qinteger, Qinteger),   Qinteger);
    defftype(logior,    (QArest, Qinteger),     Qinteger);
    defftype(logiorS2,  (Qinteger, Qinteger),   Qinteger);
    defftype(lognand,   (Qinteger, Qinteger),   Qinteger);
    defftype(lognor,    (Qinteger, Qinteger),   Qinteger);
    defftype(lognot,    (Qinteger),             Qinteger);
    defftype(logorc1,   (Qinteger, Qinteger),   Qinteger);
    defftype(logorc2,   (Qinteger, Qinteger),   Qinteger);
    defftype(logtest,   (Qinteger, Qinteger),   t);
    defftype(logxor,    (QArest, Qinteger),     Qinteger);
    defftype(logxorS2,  (Qinteger, Qinteger),   Qinteger);

    defftype(lower_case_p, (Qcharacter), t);

    // [M]
    #define defmapX(mp_name) \
    { \
        defftype(mp_name,  \
            (Qfunction_designator, Qlist, QArest, Qlist), Qlist ); \
        defftype(mp_name ## S1, \
            (Qfunction_designator, Qlist), Qlist ); \
        defftype(mp_name ## S2, \
            (Qfunction_designator, Qlist, Qlist), Qlist ); \
        defftype(mp_name ## S3, \
            (Qfunction_designator, Qlist, Qlist, Qlist), Qlist ); \
        defannot(mp_name, c_varary, _( \
            cons(Fixnum::Encode(2), Q ## mp_name ## S1), \
            cons(Fixnum::Encode(3), Q ## mp_name ## S2), \
            cons(Fixnum::Encode(4), Q ## mp_name ## S3) ) ); \
    } // defmapX

    defmapX(mapc);
    defmapX(mapcar);
    defmapX(mapcan);
    defmapX(mapcon);
    defmapX(mapl);
    defmapX(maplist);

    defftype(mask_field, (Qbyte_specifier, Qinteger), Qunsigned_byte);

    defftype(member,
        (t, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(member_if,
        (Qfunction_designator, Qproper_list, QAkey,
            _(Kkey, Qfunction_designator) ),
        Qproper_list );

    defftype(member_if_not,
        (Qfunction_designator, Qproper_list, QAkey,
            _(Kkey, Qfunction_designator) ),
        Qproper_list );

    defftype(method_combination_error, (Qformat_control, QArest, t), t);
    defftype(minusp, (Qreal), t);
    defftype(muffle_warning, (QAoptional, or_(Qcondition, Qnull)), nil);

    // [N]
    defftype(nbutlast, (Qlist, QAoptional, Qsequence_index), Qlist);
    defftype(nconc,(QArest, t), t);
    defftype(nconcS2, (Qlist, t), t);

    defftype(nintersection,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    deflist(ninth);
    defftype(nreconc, (Qlist, t), Qlist);
    defftype(nreverse, (Qsequence), Qsequence);

    defftype(nstring_capitalize,
        (Qstring, QAkey, _(Kstart, Qsequence_index), _(Kend, Qsequence_end)),
        Qstring );

    defftype(nstring_downcase,
        (Qstring, QAkey, _(Kstart, Qsequence_index), _(Kend, Qsequence_end)),
        Qstring );

    defftype(nstring_upcase,
        (Qstring, QAkey, _(Kstart, Qsequence_index), _(Kend, Qsequence_end)),
        Qstring );

    defftype(nth, (Qsequence_index, Qlist), t);
    defftype_setf(nth, (t, Qsequence_index, Qlist), t);
    defftype(nthcdr, (Qsequence_index, Qlist), t);

    defftype(nset_difference,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(nset_exclusive_or,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(nsublis,
        (Qlist, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)),
            _(Ktest,     or_(Qfunction_designator)),
            _(Ktest_not, or_(Qfunction_designator)) ),
        t );

    defftype(nsubst,
        (t, t, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)),
            _(Ktest,     or_(Qfunction_designator)),
            _(Ktest_not, or_(Qfunction_designator)) ),
        t );

    defftype(nsubst_if,
        (t, Qfunction_designator, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)) ),
        t );

    defftype(nsubst_if_not,
        (t, Qfunction_designator, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)) ),
        t );

    defftype(nunion,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    // [P]
    defftype(package_name, (Qpackage_designator), or_(Qstring, Qnull));
    defftype(package_nicknames, (Qpackage_designator), Qlist);
    defftype(package_shadowing_symbols, (Qpackage_designator), Qlist);
    defftype(package_use_list, (Qpackage_designator), Qlist);
    defftype(package_used_by_list, (Qpackage_designator), Qlist);

    defftype(pairlis,
        (Qproper_list, Qproper_list, QAoptional, Qlist),
        Qlist );

    defftype(parse_integer,
        (Qstring, QAkey,
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end),
            _(Kradix, _(Qinteger, two, Fixnum::Encode(36))),
            _(Kjunk_allowed, t) ),
        values_(Qinteger, Qsequence_index) );

    defftype(parse_namestring,
        (or_(Qstring, Qpathname, Qstream),
            QAoptional,
                Qpathname_host_designator,
                Qpathname_designator,
            QAkey,
                _(Kstart, Qsequence_index),
                _(Kend, Qsequence_end),
                _(Kjunk_allowed, t) ),
        values_(Qpathname, Qsequence_index) );

    defftype(pathname, (Qpathname_designator), Qpathname);

    defftype(pathname_device,
        (Qpathname_designator),
        Qpathname_device_designator );

    defftype(pathname_directory,
        (Qpathname_designator),
        Qpathname_directory_designator );

    defftype(pathname_host,
        (Qpathname_designator),
        Qpathname_host_designator );

    defftype(pathname_name,
        (Qpathname_designator),
        Qpathname_name_designator );

    defftype(pathname_type,
        (Qpathname_designator),
        Qpathname_type_designator );

    defftype(pathname_version,
        (Qpathname_designator),
        Qpathname_version_designator );

    defftype(peek_char,
        (QAoptional,
            or_(Qcharacter, eql_(t), Qnull),
            Qinput_stream_designator,
            t,
            t,
            t ),
        Qcharacter );

    defftype(phase, (Qnumber), Qnumber);
    defftype(plusp, (Qreal), t);

    defftype(position,
        (t, Qsequence, QAkey,
            _(Kfrom_end, t),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end),
            _(Kkey, Qfunction_designator) ),
        or_(Qsequence_index, Qnull) );

    defftype(position_if,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kfrom_end, t),
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end),
            _(Kkey, Qfunction_designator) ),
        or_(Qsequence_index, Qnull) );

    defftype(position_if_not,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kfrom_end, t),
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end),
            _(Kkey, Qfunction_designator) ),
        or_(Qsequence_index, Qnull) );

    defftype(pprint, (t, QAoptional, Qoutput_stream_designator),
        _(Qvalues) );

    defftype(pprint_dispatch,
        (t, QAoptional, or_(Qpprint_dispatch_table, Qnull)),
        values_(Qfunction_designator, t) );

    defftype(pprint_fill,
        (Qoutput_stream_designator, t, QAoptional, t, t),
        Qnull );

    defftype(pprint_indent,
        (member_(Kblock, Kcurrent), Qreal,
         QAoptional, Qoutput_stream_designator ),
        Qnull );

    defftype(pprint_linear,
        (Qoutput_stream_designator, t, QAoptional, t, t),
        Qnull );

    defftype(pprint_newline,
        (member_(Kfill, Klinear, Kmandatory, Kmiser),
         QAoptional, Qoutput_stream_designator  ),
        Qnull );

    defftype(pprint_tab,
        (member_(Kline, Kline_relative, Ksection, Ksection_relative),
         Qunsigned_byte,
         Qunsigned_byte,
         QAoptional, Qoutput_stream_designator  ),
        Qnull );

    defftype(pprint_tabular,
        (Qoutput_stream_designator, t, QAoptional, t, t, Qunsigned_byte),
        Qnull );

    defftype(prin1, (t, QAoptional, Qoutput_stream_designator), t);
    defftype(prin1_to_string, (t), Qstring);
    defftype(princ, (t, QAoptional, Qoutput_stream_designator), t);
    defftype(print, (t, QAoptional, Qoutput_stream_designator), t);
    defftype(princ_to_string, (t), Qstring);
    defftype(print_object, (t, Qstream), t);
    defftype(probe_file, (Qpathname_designator), or_(Qpathname, Qnull));
    defftype(proclaim, (Qlist), t);
    defftype(provide, (Qstring_designator), t);

    // [R]
    defftype(random,
        (or_(Qunsigned_byte, _(Qfloat, zero))),
        (or_(Qunsigned_byte, _(Qfloat, zero))) );

    defftype(rassoc,
        (t, Qlist, QAkey,
            _(Kkey, Qfunction_designator),
            _(Ktest, Qfunction_designator) ),
        t );

    defftype(rassoc_if,
        (Qfunction_designator, Qlist, QAkey,
            _(Kkey, Qfunction_designator) ),
        t );

    defftype(rassoc_if_not,
        (Qfunction_designator, Qlist, QAkey,
            _(Kkey, Qfunction_designator) ),
        t );

    defftype(rational, (Qreal), Qrational);
    defftype(rationalize, (Qreal), Qrational);

    defftype(read,
        (QAoptional, Qinput_stream_designator, t, t, t),
        t );

    defftype(read_byte,
        (Qstream, QAoptional, t, t),
        Qinteger );

    defftype(read_char,
        (Qstream, QAoptional, t, t, t),
        t );

    defftype(read_char_no_hang,
        (Qstream, QAoptional, t, t, t),
        t );

    defftype(read_delimited_list,
        (Qcharacter, QAoptional, Qinput_stream_designator, t),
        Qlist );

    defftype(read_from_string,
        (Qstring, QAoptional, t, t, QAkey,
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end),
            _(Kpreserve_whitespace) ),
        values_(t, Qsequence_index) );

    defftype(read_line,
        (QAoptional, Qinput_stream_designator, t, t, t),
        values_(t, t) );

    defftype(read_preserving_whitespace,
        (QAoptional, Qinput_stream_designator, t, t, t),
        t );

    defftype(read_sequence,
        (Qsequence, Qstream , QAkey,
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end) ),
        Qsequence_index );

    defftype(readtable_case, (Qreadtable), Qreadtable_case);
    defftype(realpart, (Qnumber), Qreal);

    defftype(reduce,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kkey, Qfunction_designator),
            _(Kfrom_end, t),
            _(Kstart, Qsequence_index),
            _(Kend, Qsequence_end),
            _(Kinitial_value, t) ),
        t );

    defftype(reinitialize_instance,
        (t, QAkey, QAallow_other_keys),
        t );

    defftype(rem, (Qreal, Qreal), Qreal);
    defftype(remhash, (t, Qhash_table), t);

    defftype(remove,
        (t, Qsequence, QAkey,
            _(Kcount, t),
            _(Kend, Qsequence_end),
            _(Kfrom_end, t),
            _(Kkey, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qsequence );

    defftype(remove_if,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kcount, t),
            _(Kend, Qsequence_end),
            _(Kfrom_end, t),
            _(Kkey, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qsequence );

    defftype(remove_if_not,
        (Qfunction_designator, Qsequence, QAkey,
            _(Kcount, t),
            _(Kend, Qsequence_end),
            _(Kfrom_end, t),
            _(Kkey, Qfunction_designator),
            _(Kstart, Qsequence_index),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qsequence );

    defftype(remove_method, (Qgeneric_function, Qmethod), Qgeneric_function);
    defftype(remprop, (Qsymbol, t), t);

    defftype(rename_file,
        (Qpathname_designator, Qpathname_designator),
        values_(Qpathname, Qpathname, Qpathname) );

    defftype(rename_package,
        (Qpackage_designator, Qpackage_designator,
            QAoptional, or_(Qstring, Qlist) ),
        Qpackage );

    defftype(replace,
        (Qsequence, Qsequence, QAkey,
            _(Kend1, Qsequence_end),
            _(Kend2, Qsequence_end),
            _(Kstart1, Qsequence_index),
            _(Kstart2, Qsequence_index) ),
        Qsequence );

    defftype(require,
        (Qstring_designator, QAoptional, or_(Qstring, Qlist)),
        t );

    deflist(rest);
    defftype(restart_name, (Qrestart), Qsymbol);
    defftype(revappend, (Qlist, t), Qlist);
    defftype(reverse, (Qsequence), Qsequence);
    defftype(room, (member_(t, nil, Kdefault)), values_Arest_t);
    defftype(round, (Qreal, QAoptional, Qreal), values_(Qinteger, Qreal));
    defftype(row_major_aref, (Qarray, Qsequence_index), t);
    defftype_setf(row_major_aref, (t, Qarray, Qsequence_index), t);

    // [S]
    defftype(sbit,
        (_(Qsimple_array, Qbit), QArest, Qsequence_index),
        Qbit );

    defftype_setf(sbit,
        (Qbit, _(Qsimple_array, Qbit), QArest, Qsequence_index),
        Qbit );

    defftype(scale_float, (Qfloat, Qunsigned_byte), Qfloat);

    defftype(schar, (Qsimple_string, Qsequence_index), Qcharacter);

    defftype_setf(schar,
        (Qcharacter, Qsimple_string, Qsequence_index),
        Qcharacter );

    defftype(search,
        (Qsequence, Qsequence, QAkey,
            _(Kfrom_end, t),
            _(Ktest, Qfunction_designator),
            _(Ktest_not, Qfunction_designator),
            _(Kkey, Qfunction_designator),
            _(Kstart1, Qsequence_index),
            _(Kend1, Qsequence_end),
            _(Kstart2, Qsequence_index),
            _(Kend2, Qsequence_end) ),
        or_(Qsequence_index, Qnull) );

    deflist(second);
    defftype(sequence_index_error, (Qsequence, t), nil);
    defftype(set, (Qsymbol, t), t);

    defftype(set_difference,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(set_exclusive_or,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(set_pprint_dispatch,
        (Qtype_specifier, or_(Qfunction_designator, Qnull),
         QAoptional, Qreal, Qpprint_dispatch_table ),
        Qnull );

    deflist(seventh);

    defftype(shadow,
        (or_(Qstring, Qlist), QAoptional, Qpackage_designator),
        eql_(t) );

    defftype(shared_initialize,
        (t, or_(Qlist, eql_(t)), QArest, t),
        t );

    defftype(short_site_name, (), or_(Qstring, Qnull));

    defftype(signal, (Qcondition_designator, QArest, t), Qnull);
    defftype(signum, (Qnumber), Qnumber);
    defftype(sin, (Qnumber), Qnumber);
    defftype(sinh, (Qnumber), Qnumber);
    deflist(sixth);
    defftype(sleep, (_(Qreal, 0)), Qnull);
    defftype(slot_boundp,       (t, Qsymbol), t);
    defftype(slot_exists_p,     (t, Qsymbol), t);
    defftype(slot_makunbound,   (t, Qsymbol), t);

    defftype(slot_missing,
        (Qclass, t, Qsymbol,
            member_(Qsetq, Qslot_boundp, Qslot_makunbound, Qslot_value),
            QAoptional, t ),
        values_Arest_t );

    defftype(slot_unbound, (Qclass, t, Qsymbol), values_Arest_t);

    defftype(slot_value,      (t, Qsymbol), t);
    defftype_setf(slot_value, (t, t, Qsymbol), t);

    defftype(software_type,     (), or_(Qstring, Qnull));
    defftype(software_version,  (), or_(Qstring, Qnull));

    defftype(some,
        (Qfunction_designator, Qsequence, QArest, Qsequence),
        t );

    defftype(sort,
        (Qsequence, Qfunction_designator,
            QAkey, _(Kkey, Qfunction_designator) ),
        Qsequence );

    defftype(special_operator_p, (Qsymbol), t);
    defftype(sqrt, (Qnumber), Qnumber);

    defftype(stable_sort,
        (Qsequence, Qfunction_designator,
            QAkey, _(Kkey, Qfunction_designator) ),
        Qsequence );

    defftype(standard_char_p, (Qcharacter), t);
    defftype(stream_element_type, (Qstream), Qtype_specifier);
    defftype(stream_error_stream, (Qstream_error), Qstream);
    defftype(stream_external_format, (Qstream), Qexternal_format);

    defftype(string, (or_(Qstring, Qsymbol, Qcharacter)), Qstring);

    #define defftype_string_cmp2(mp_name, mp_values) \
        defftype(mp_name, \
            (Qstring, Qstring, QAkey, \
                _(Kstart1, Qsequence_index), \
                _(Kend1, Qsequence_end), \
                _(Kstart2, Qsequence_index), \
                _(Kend2, Qsequence_end) ), \
            mp_values )

    #define defftype_string_cmp(mp_name) \
        defftype_string_cmp2(mp_name, or_(Qsequence_index, Qnull))

    #define defftype_string_eq(mp_name) \
        defftype_string_cmp2(mp_name, t)

    defftype_string_eq(stringG);        // string>
    defftype_string_eq(stringGQ);       // string>=
    defftype_string_eq(stringL);        // string<
    defftype_string_eq(stringLQ);       // string<=
    defftype_string_eq(stringQ);        // string=
    defftype_string_eq(stringSQ);       // string/=

    defftype_string_eq(string_equal);
    defftype_string_cmp(string_greaterp);
    defftype_string_cmp(string_lessp);
    defftype_string_eq(string_not_equal);
    defftype_string_cmp(string_not_greaterp);
    defftype_string_cmp(string_not_lessp);

    defftype(string_capitalize,
        (Qstring, QAkey, _(Kstart, Qsequence_index), _(Kend, Qsequence_end)),
        Qstring );

    defftype(string_downcase,
        (Qstring, QAkey, _(Kstart, Qsequence_index), _(Kend, Qsequence_end)),
        Qstring );

    defftype(string_left_trim,  (Qsequence, Qstring), Qstring);
    defftype(string_right_trim, (Qsequence, Qstring), Qstring);
    defftype(string_trim,       (Qsequence, Qstring), Qstring);

    defftype(string_upcase,
        (Qstring, QAkey, _(Kstart, Qsequence_index), _(Kend, Qsequence_end)),
        Qstring );

    defftype(store_value,       (QAoptional, or_(Qcondition, Qnull)), Qnull);

    defftype(sublis,
        (Qlist, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)),
            _(Ktest,     or_(Qfunction_designator)),
            _(Ktest_not, or_(Qfunction_designator)) ),
        t );

    defftype(subsetp,
        (Qlist, Qlist, QAkey,
            _(Kkey, Qfunction_designator),
            _(Ktest, Qfunction_designator) ),
        t );

    defftype(subst,
        (t, t, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)),
            _(Ktest,     or_(Qfunction_designator)),
            _(Ktest_not, or_(Qfunction_designator)) ),
        t );

    defftype(subst_if,
        (t, Qfunction_designator, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)) ),
        t );

    defftype(subst_if_not,
        (t, Qfunction_designator, t, QAkey,
            _(Kkey,     or_(Qfunction_designator)) ),
        t );

    defftype(sxhash,
        (t),
        _(Qunsigned_byte, Fixnum::Encode(Fixnum::Bits - 1)) );

    defftype(symbol_function,   (Qsymbol), Qfunction);
    defftype(symbol_name,       (Qsymbol), Qsimple_string);
    defftype(symbol_package,    (Qsymbol), or_(Qpackage, Qnull));
    defftype(symbol_plist,      (Qsymbol), Qlist);

    defftype_setf(symbol_function,  (Qfunction, Qsymbol), Qfunction);
    defftype_setf(symbol_plist,     (Qlist, Qsymbol), Qlist);

    // [T]
    defftype(tailp, (t, Qlist), t);
    defftype(tan,  (Qnumber), Qnumber);
    defftype(tanh, (Qnumber), Qnumber);
    deflist(tenth);
    defftype(terpri, (QAoptional, Qoutput_stream_designator), Qnull);
    deflist(third);

    defftype(translate_logical_pathname,
        (Qpathname_designator, QAkey, QAallow_other_keys),
        Qpathname );

    defftype(translate_pathname,
        (Qpathname_designator, Qpathname_designator, Qpathname_designator,
            QAkey, QAallow_other_keys ),
        Qpathname );

    defftype(tree_equal, (t, t, QAkey,
        _(Ktest, Qfunction_designator),
        _(Ktest_not, Qfunction_designator) ),
        t );

    defftype(truename, (Qpathname_designator), Qpathname);
    defftype(truncate, (Qreal, QAoptional, Qreal), values_(Qinteger, Qreal));
    defftype(type_of, (t), Qtype_specifier);
    defftype(typep, (t, Qtype_specifier, QAoptional, t), t);

    // [U]
    defftype(unexport,
        (or_(Qlist, Qsymbol), QAoptional, Qpackage_designator),
        eql_(t) );

    defftype(unintern,
        (Qsymbol, QAoptional, Qpackage_designator),
        eql_(t) );

    defftype(union,
        (Qproper_list, Qproper_list, QAkey,
            _(Kkey,      Qfunction_designator),
            _(Ktest,     Qfunction_designator),
            _(Ktest_not, Qfunction_designator) ),
        Qproper_list );

    defftype(unread_char,
        (Qcharacter, QAoptional, Qinput_stream_designator),
        Qnull );

    defftype(unuse_package,
        (or_(Qpackage_designator, Qlist), QAoptional, Qpackage_designator),
        eql_(t) );

    defftype(update_instance_for_different_class,
        (Qclass, Qclass, QAkey, QAallow_other_keys),
        t );

    defftype(update_instance_for_redefined_class,
        (t, Qlist, Qlist, Qlist, QAkey, QAallow_other_keys),
        values_Arest_t );

    defftype(upgraded_array_element_type,
        (Qtype_specifier, QAoptional, or_(Qenvironment, Qnull)),
        Qtype_specifier );

    defftype(upgraded_complex_part_type,
        (Qtype_specifier, QAoptional, or_(Qenvironment, Qnull)),
        Qtype_specifier );

    defftype(upper_case_p, (Qcharacter), t);

    defftype(use_package,
        (or_(Qpackage_designator, Qlist), QAoptional, Qpackage_designator),
        eql_(t) );

    defftype(user_homedir_pathname,
        (QAoptional, or_(Qstring, Qlist, eql_(Kunspecific))),
        Qpathname );

    defftype(use_value, (QAoptional, or_(Qcondition, Qnull)), Qnull);

    // [V]
    defftype(values,      (QArest, t), values_Arest_t);
    defftype(valuesA,     (QArest, t), values_Arest_t);
    defftype(values_list, (t),         values_Arest_t);
    defftype(vector,      (QArest, t), Qsimple_vector);
    defftype(vector_pop,  (Qvector),   t);
    defftype(vector_push, (Qvector, Qsequence_index),   t);
    defftype(vector_push_extend, (Qvector, Qsequence_index),   t);

    // [W]
    defftype(warn, (Qcondition_designator, QArest, t), Qnull);

    defftype(wild_pathname_p,
        (Qpathname_designator, QAoptional,
            member_(nil, Khost, Kdevice, Kdirectory, Kname, Ktype, Kversion) ),
        t );

    defftype(write,
        (t, QAkey,
            _(Karray, t),
            _(Kbase, _(Qinteger, Fixnum::Encode(2), Fixnum::Encode(36))),
            _(Kcase, member_(Kcapitalize, Kdowncase, Kupcase)),
            _(Kcircle, t),
            _(Kescape, t),
            _(Kgensym, t),
            _(Klength, or_(Qunsigned_byte, Qnull)),
            _(Klevel, or_(Qunsigned_byte, Qnull)),
            _(Klines, or_(Qunsigned_byte, Qnull)),
            _(Kmiser_width, or_(Qunsigned_byte, Qnull)),
            _(Kpprint_dispatch, Qpprint_dispatch_table),
            _(Kpretty, t),
            _(Kradix, t),
            _(Kreadably, t),
            _(Kright_margin, or_(Qunsigned_byte, Qnull)),
            _(Kstream, Qoutput_stream_designator) ),
        t );

    defftype(
        write_byte,
        (Qfixnum, QAoptional, Qoutput_stream_designator),
        Qfixnum );

    defftype(
        write_char,
        (Qcharacter, QAoptional, Qoutput_stream_designator),
        Qcharacter );

    defftype(write_line,
        (Qstring, QAoptional, Qoutput_stream_designator,
            QAkey, _(Kend, Qsequence_end), _(Kstart, Qsequence_index) ),
        Qstring );

    defftype(write_sequence,
        (Qsequence, Qstream,
            QAkey, _(Kend, Qsequence_end), _(Kstart, Qsequence_index) ),
        Qstring );

    defftype(write_string,
        (Qstring, QAoptional, Qoutput_stream_designator,
            QAkey, _(Kend, Qsequence_end), _(Kstart, Qsequence_index) ),
        Qstring );

    defftype(write_to_string,
        (t, QAkey,
            _(Karray, t),
            _(Kbase, _(Qinteger, Fixnum::Encode(2), Fixnum::Encode(36))),
            _(Kcase, member_(Kcapitalize, Kdowncase, Kupcase)),
            _(Kcircle, t),
            _(Kescape, t),
            _(Kgensym, t),
            _(Klength, or_(Qunsigned_byte, Qnull)),
            _(Klevel, or_(Qunsigned_byte, Qnull)),
            _(Klines, or_(Qunsigned_byte, Qnull)),
            _(Kmiser_width, or_(Qunsigned_byte, Qnull)),
            _(Kpprint_dispatch, Qpprint_dispatch_table),
            _(Kpretty, t),
            _(Kradix, t),
            _(Kreadably, t),
            _(Kright_margin, or_(Qunsigned_byte, Qnull)) ),
        Qstring );

    // [Y]
    defftype(yes_or_no_p, (QAoptional, Qformat_control, QArest, t), t);
    defftype(y_or_n_p, (QAoptional, Qformat_control, QArest, t), t);

    // [Z]
    defftype(zerop, (Qreal), t);

    // Variable arity
    #define defvararity_2(mp_name) \
        defannot(mp_name,  c_varary, _(cons(two, Q ## mp_name ## S2)))

    defvararity_2(P); // +
    defvararity_2(A); // *

    defannot(_, c_varary, _(cons(one, Q_S1), cons(two, Q_S2))); // -
    defannot(S, c_varary, _(cons(one, QSS1), cons(two, QSS2))); // /

    defvararity_2(G); // >
    defvararity_2(GQ);// >=
    defvararity_2(L); // <
    defvararity_2(LQ);// <=
    defvararity_2(Q); // =
    defvararity_2(SQ);// /=

    // [A]
    defvararity_2(append);

    // [C]
    defvararity_2(charG); // char>
    defvararity_2(charGQ);// char>=
    defvararity_2(charL); // char<
    defvararity_2(charLQ);// char<=
    defvararity_2(charQ); // char=
    defvararity_2(charSQ);// char/=

    defvararity_2(char_greaterp);     // char-greaterp
    defvararity_2(char_not_lessp);    // char-not-lessp
    defvararity_2(char_lessp);        // char-lessp
    defvararity_2(char_not_greaterp); // char-not-greaterp
    defvararity_2(char_equal);        // char-equal
    defvararity_2(char_not_equal);    // char-not-equal

    // [L]
    defvararity_2(logand);
    defvararity_2(logeqv);
    defvararity_2(logior);
    defvararity_2(logxor);

    // [N]
    defvararity_2(nconc);

    // Type predicates
    #define defftypep(mp_name, mp_class) \
    { \
        defftype(mp_name, (t), t); \
        defannot(mp_name, c_type_predicate, CLASS_ ## mp_class); \
    }

    defftypep(arrayp,               array);
    defftypep(bit_vector_p,         bit_vector);
    defftypep(characterp,           character);
    defftypep(compiled_function_p,  function);
    defftypep(complexp,             complex);
    defftypep(consp,                cons);
    defftypep(floatp,               float);
    defftypep(functionp,            function);
    defftypep(hash_table_p,         hash_table);
    defftypep(integerp,             integer);
    defftypep(listp,                list);
    defftypep(numberp,              number);
    defftypep(packagep,             package);
    defftypep(pathnamep,            pathname);
    defftypep(random_state_p,       random_state);
    defftypep(rationalp,            rational);
    defftypep(readtablep,           readtable);
    defftypep(realp,                real);
    defftypep(simple_bit_vector_p,  simple_bit_vector);
    defftypep(simple_string_p,      simple_string);
    defftypep(simple_vector_p,      simple_vector);
    defftypep(streamp,              stream);
    defftypep(stringp,              string);
    defftypep(simple_vector,        simple_vector);
    defftypep(symbolp,              symbol);
    defftypep(vectorp,              vector);
} // installFTypes

} // Compiler

void InstallCompilerStatics()
{
    Compiler::installFunDb();
} // InstallCompilerStatics

} // TInyCl
