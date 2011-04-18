#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - X86 - Built-In Functions
// arch/x86/tinycl_x86_init.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_init.cpp#32 $
//
#include "./tinycl_x86_asm.h"

#include "../../tinycl_clos.h"
#include "../../init/tinycl_init.h"

namespace TinyCl
{

using namespace X86;

namespace
{

static void installCharCmpCi(Val name, Tttn tttn)
{
    class MyAsm : protected X86Asm
    {
        public: void Run(Val name, Tttn tttn)
        {
            m_name = name;
            emitPrologue(2, 2);

            Label not_char_0;
            Label not_char_1;

            lea($r2, ea($r0, -CHAR_u0000->ToInt()));
            cmp($r2, sizeof(Character) * Character::Max);
            ja(not_char_0);

            lea($r2, ea($r1, -CHAR_u0000->ToInt()));
            cmp($r2, sizeof(Character) * Character::Max);
            ja(not_char_1);

            // (setq $r0 (char-upper $r0))
            mov($r3, ea($r0, OffsetOf(Character, m_data)));
            mov($r2, $r3);
            shr($r2, Character::CaseShiftCount);
            shl($r2, Character::ShiftCount);
            add($r2, CHAR_u0000->ToInt());
            test($r3, Character::Attr_LowerCase);
            cmovne($r0, $r2);

            // (setq $r1 (char-upper $r1))
            mov($r3, ea($r1, OffsetOf(Character, m_data)));
            mov($r2, $r3);
            shr($r2, Character::CaseShiftCount);
            shl($r2, Character::ShiftCount);
            add($r2, CHAR_u0000->ToInt());
            test($r3, Character::Attr_LowerCase);
            cmovne($r1, $r2);

            mov($r2, nil);
            cmp($r0, $r1);
            cmovcc(tttn, $r0, $r2);
            or($r1, $r1);
            ret();

          label(not_char_1);
            mov($r0, $r1);

          label(not_char_0);
            mov($r1, Qcharacter);
            thread_svc(TypeError);

            emitEpilogue();
            install();
        } // Run
    } oAsm;
    oAsm.Run(name, tttn);
} // installCharCmpCi

static void installCharCmpCs(Val name, Tttn tttn)
{
    class MyAsm : protected X86Asm
    {
        public: void Run(Val name, Tttn tttn)
        {
            m_name = name;
            emitPrologue(2, 2);

            Label not_char_0;
            Label not_char_1;

            lea($r2, ea($r0, -CHAR_u0000->ToInt()));
            cmp($r2, sizeof(Character) * Character::Max);
            ja(not_char_0);

            lea($r2, ea($r1, -CHAR_u0000->ToInt()));
            cmp($r2, sizeof(Character) * Character::Max);
            ja(not_char_1);

            mov($r2, nil);
            cmp($r0, $r1);
            cmovcc(tttn, $r0, $r2);
            or($r1, $r1);
            ret();

          label(not_char_1);
            mov($r0, $r1);

          label(not_char_0);
            mov($r1, Qcharacter);
            thread_svc(TypeError);

            emitEpilogue();
            install();
        } // Run
    } oAsm;
    oAsm.Run(name, tttn);
} // installCharCmpCs

static void installCharPred(Val name, int iAttr)
{
    class MyAsm : protected X86Asm
    {
        public: void Run(Val name, int iAttr)
        {
            m_name = name;
            emitPrologue(1, 1);

            Label not_char;

            lea($r1, ea($r0, -CHAR_u0000->ToInt()));
            cmp($r1, sizeof(Character) * Character::Max);
            ja(not_char);

            mov($r2, nil);
            test(ea($r0, OffsetOf(Character, m_data)), iAttr);
            cmove($r0, $r2);
            ret();

          label(not_char);
            mov($r1, Qcharacter);
            thread_svc(TypeError);

            emitEpilogue();
            install();
        } // Run
    } oAsm;
    oAsm.Run(name, iAttr);
} // installCharPred

static void installCharCase(Val name, int iAttr)
{
    class MyAsm : protected X86Asm
    {
        public: void Run(Val name, int iAttr)
        {
            m_name = name;
            emitPrologue(1, 1);

            Label not_char;

            lea($r1, ea($r0, -CHAR_u0000->ToInt()));
            cmp($r1, sizeof(Character) * Character::Max);
            ja(not_char);

            mov($r1, ea($r0, OffsetOf(Character, m_data)));
            mov($r2, $r1);
            shr($r2, Character::CaseShiftCount);
            shl($r2, Character::ShiftCount);
            add($r2, CHAR_u0000->ToInt());
            test($r1, iAttr);
            cmovne($r0, $r2);

            ret();

          label(not_char);
            mov($r1, Qcharacter);
            thread_svc(TypeError);

            emitEpilogue();
            install();
        } // Run
    } oAsm;
    oAsm.Run(name, iAttr);
} // InstallCase

static void installCxR(Val reader, int ofs)
{
    class AsmReader : protected X86Asm
    {
        public: void Run(Val reader, int ofs)
        {
            m_name = reader;
            emitPrologue(1, 1);

            Label type_error_cons;

            lea($r1, ea($r0, -Cons::Tag2));
            and($r1, 3);     // CF=0
            jne(type_error_cons);
            mov($r0, ea($r0, static_cast<int32>(ofs - Cons::Tag2)));
            ret();
          label(type_error_cons);
            mov($r1, Qlist);
            thread_svc(TypeError);

            emitEpilogue();
            install();
        } // Run
    } oAsmReader;

    oAsmReader.Run(reader, ofs);

    class AsmWriter : protected X86Asm
    {
        public: void Run(Val reader, int ofs)
        {
            m_name = list(Qsetf, reader);
            emitPrologue(2, 2);

            Label type_error_cons;

            lea($r2, ea($r1, -Cons::Tag2));
            and($r2, 7);     // CF=0
            jne(type_error_cons);
            mov(ea($r1, ofs - Cons::Tag2), $r0);
            ret();
          label(type_error_cons);
            mov($r0, $r1);
            mov($r1, Qcons);
            thread_svc(TypeError);

            emitEpilogue();
            install();
        } // Run
    } oAsmWriter;
    oAsmWriter.Run(reader, ofs);
} // Install

static void installCxR(Val reader, Val first, Val rest)
{
    class AsmReader : protected X86Asm
    {
        public: void Run(Val reader, Val first, Val rest)
        {
            m_name = reader;
            emitPrologue(1, 1);

            call(rest);

            mov($rn, one);
            jmp(first);

            emitEpilogue();
            install();
        } // Run
    } oAsmReader;

    oAsmReader.Run(reader, first, rest);

    class AsmWriter : protected X86Asm
    {
        public: void Run(Val reader, Val first, Val rest)
        {
            m_name = list(Qsetf, reader);
            emitPrologue(1, 1);

            call(rest);

            mov($rn, one);
            jmp(first);

            emitEpilogue();
            install();
        } // Run
    } oAsmWriter;
    oAsmWriter.Run(reader, find_setf_cell(first), rest);
} // installCxR

} // namespace

/// <summary>
///   Install type predicate for pointer tag.
/// </summary>
void InstallPredicate(
    Val name,
    int iTag,
    int iTagMask = Arch::TagMask )
{
    class AsmPred : protected X86Asm
    {
        public: void Run(Val name, int iTag, int iTagMask)
        {
            m_name = name;
            emitPrologue(1, 1);

            mov($r2, nil);

            lea($r1, ea($r0, -iTag));

            if (Qlistp == name)
            {
                mov($r0, $tcb);
            }

            test($r1, iTagMask);   // CF=0
            cmovne($r0, $r2);
            ret();

            emitEpilogue();
            install();
        } // Run
    } oAsmPred;
    oAsmPred.Run(name, iTag, iTagMask);
} // InstallPredicate

/// <summary>
///   Install type predicate for simple record type.
/// </summary>
void InstallPredicate(Val name, Val c1)
{
    class AsmPred : protected X86Asm
    {
        public: void Run(Val name, Val c1)
        {
            m_name = name;
            emitPrologue(1, 1);

            Label ret_false;
            Label ret_true;

            mov($r2, nil);

            if (Qsymbolp == name)
            {
                cmp($r0, $r2);
                je(ret_true);
            }

            lea($r1, ea($r0, -Arch::Tag_Record));
            test($r1, Arch::TagMask);   // CF=0
            jne(ret_false);

            mov($r0, ea($r1));

            cmp($r0, c1);
            cmovne($r0, $r2);
            xor($r1, $r1);
            ret();

          label(ret_false);
            mov($r0, $r2);

          label(ret_true);
            if (Qsymbolp == name)
            {
                mov($r0, $tcb);
            }
            ret();

            emitEpilogue();
            install();
        } // Run
    } oAsmPred;
    oAsmPred.Run(name, c1);
} // InstallPredicate

void InstallPredicate(Val name, Val min, Val max)
{
    class AsmPred : protected X86Asm
    {
        public: void Run(Val name, Val min, Val max)
        {
            m_name = name;
            emitPrologue(1, 1);

            Label ret_false;
            Label ret_true;

            if (CLASSD_bignum == min)
            {
                test($r0, 3);
                je(ret_true);
            }

            mov($r2, nil);
            lea($r1, ea($r0, -Arch::Tag_Record));
            test($r1, Arch::TagMask);   // CF=0
            jne(ret_false);

            mov($r0, ea($r1));
            sub($r0, min);
            cmp($r0, max->ToInt() - min->ToInt());
            cmova($r0, $r2);
            xor($r1, $r1);

          label(ret_true);
            ret();

          label(ret_false);
            mov($r0, $r2);
            ret();

            emitEpilogue();
            install();
        } // Run
    } oAsmPred;
    oAsmPred.Run(name, min, max);
} // InstallPredicate

/// <summary>
///   Install type predicate for two records.
/// </summary>
void InstallPredicate(Val name, Val, Val c1, Val c2)
{
    class AsmPred : protected X86Asm
    {
        public: void Run(Val name, Val c1, Val c2)
        {
            m_name = name;
            emitPrologue(1, 1);

            Label ret_false;
            Label ret_false_clc;
            Label ret_true;

            mov($r2, nil);
            lea($r1, ea($r0, -Arch::Tag_Record));
            test($r1, Arch::TagMask);   // CF=0
            jne(ret_false);

            mov($r0, ea($r1));

            cmp($r0, c1);
            je(ret_true);

            cmp($r0, c2);
            cmovne($r0, $r2);
            xor($r1, $r1);
            ret();

          label(ret_false_clc);
            or($r0, $r0);

          label(ret_false);
            mov($r0, $r2);

          label(ret_true);
            ret();

            emitEpilogue();
            install();
        } // Run
    } oAsmPred;
    oAsmPred.Run(name, c1, c2);
} // InstallPredicate

static void installStaticFunction__B_b()
{
    DefAsm(Bbox_float32, ())
        struct LocalFrame : ToForeignFrame
        {
            float32 m_flt;
        }; // LocalFrame

        frame(FunObj::FrameKind_Fixed, sizeof(LocalFrame));

        sub($sp, sizeof(LocalFrame));

        // Push ToForeignFrame
        mov($r1, ea_m_fp());
        xor($rn, $rn);
        mov(ea_($sp, LocalFrame, m_pOuter), $r1);
        mov(ea_($sp, LocalFrame, m_eFrame), Frame::Type_ToForeign);
        mov(ea_($sp, LocalFrame, m_cbArgs), $rn);   // m_cbArgs=0
        mov(ea_m_fp(), $sp);

        movss(ea_($sp, LocalFrame, m_flt), xmm0);

        // Call C-function
        mov(ecx, CLASSD_single_float);
        call(".", "allocate_binobj");

        // Pop ToForeign
        mov($r1, ea_($sp, LocalFrame, m_pOuter));
        mov(ea_m_fp(), $r1);

        movss(xmm0, ea_($sp, LocalFrame, m_flt));
        movss(ea(eax, OffsetOf(SingleFloat, m_flt)), xmm0);

        add($sp, sizeof(LocalFrame));
        ret();
    EndAsm(Bbox_float32)

    DefAsm(Bbox_float64, ())
        struct LocalFrame : ToForeignFrame
        {
            float64 m_flt;
        }; // LocalFrame

        frame(FunObj::FrameKind_Fixed, sizeof(LocalFrame));

        sub($sp, sizeof(LocalFrame));

        // Push ToForeignFrame
        mov($r1, ea_m_fp());
        xor($rn, $rn);
        mov(ea_($sp, LocalFrame, m_pOuter), $r1);
        mov(ea_($sp, LocalFrame, m_eFrame), Frame::Type_ToForeign);
        mov(ea_($sp, LocalFrame, m_cbArgs), $rn);   // m_cbArgs=0
        mov(ea_m_fp(), $sp);

        movsd(ea_($sp, LocalFrame, m_flt), xmm0);

        // Call C-function
        mov(ecx, CLASSD_double_float);
        call(".", "allocate_binobj");

        // Pop ToForeign
        mov($r1, ea_($sp, LocalFrame, m_pOuter));
        mov(ea_m_fp(), $r1);

        movsd(xmm0, ea_($sp, LocalFrame, m_flt));
        movsd(ea(eax, OffsetOf(DoubleFloat, m_dbl)), xmm0);

        add($sp, sizeof(LocalFrame));
        ret();
    EndAsm(Bbox_float64)
} // installStaticFunction__B_b

static void installStaticFunction__B_t()
{
    DefAsm(Btlv, (1, 1))
        Label not_fixnum;

        test($r0, Fixnum::TagMask); // CF=0
        jne(not_fixnum);

        mov($r0, ea($tcb, offsetof(Thread, mv_tlv), $r0));
        ret();

      label(not_fixnum);
        mov($r1, Qfixnum);
        thread_svc(TypeError);
    EndAsm(Btlv)

    #define Qsetf_Btlv list(Qsetf, QBtlv)
    DefAsm(setf_Btlv, (2, 2))
        Label not_fixnum;

        test($r1, Fixnum::TagMask); // CF=0
        jne(not_fixnum);

        mov(ea($tcb, offsetof(Thread, mv_tlv), $r1), $r0);
        ret();

      label(not_fixnum);
        mov($r0, $r1);
        mov($r1, Qfixnum);
        thread_svc(TypeError);
    EndAsm(setf_Btlv)
    #undef Qsetf_Btlv
} // installStaticFunction__B_t

static void installStaticFunction__B_u()
{
    DefAsm(Bunbox_int32, (1, 1))
        Label not_int32;
        Label unbox_bignum;

        test($r0, Fixnum::TagMask);
        jne(unbox_bignum);

        // fixnum
        sar($r0, Fixnum::TagBits);
        ret();

      label(unbox_bignum);
        lea($r1, ea($r0, -Arch::Tag_Record));
        test($r1, Arch::TagMask);   // CF=0
        jne(not_int32);

        cmp(ea($r1, offsetof(Layout_bignum, m_classd)), CLASSD_bignum);
        jne(not_int32);

        // bignum
        cmp(ea($r1, offsetof(Layout_bignum, m_length)), one);
        jne(not_int32);

        mov($r0, ea($r1, sizeof(Layout_bignum)));
        ret();

      label(not_int32);
        mov($r1, list(Qsigned_byte, Fixnum::Encode(32)));
        thread_svc(TypeError);
    EndAsm(Bunbox_int32)

    DefAsm(Bunbox_uint32, (1, 1))
        Label got_uint32;
        Label not_uint32;
        Label unbox_bignum;
        Label unbox_bignum2;

        test($r0, Fixnum::TagMask);
        jne(unbox_bignum);

        test($r0, $r0);
        jl(not_uint32);

        // fixnum
        sar($r0, Fixnum::TagBits);
        ret();

      label(unbox_bignum);
        lea($r1, ea($r0, -Arch::Tag_Record));
        test($r1, Arch::TagMask);   // CF=0
        jne(not_uint32);

        cmp(ea($r1, offsetof(Layout_bignum, m_classd)), CLASSD_bignum);
        jne(not_uint32);

        mov($rn, ea($r1, offsetof(Layout_bignum, m_length)));
        mov($r2, ea($r1, sizeof(Layout_bignum)));

        cmp($rn, one);
        jne(unbox_bignum2);

        test($r2, $r2);
        jl(not_uint32);
        // bignum one bigit

      label(got_uint32);
        mov($r0, $r2);
        ret();

      label(unbox_bignum2);
        cmp($rn, two);
        jne(not_uint32);

        // bignum two bigits
        mov($r1, ea($r2, sizeof(Val) * 2));
        test($r1, $r1);
        je(not_uint32);

      label(not_uint32);
        mov($r1, list(Qunsigned_byte, Fixnum::Encode(32)));
        thread_svc(TypeError);
    EndAsm(Bunbox_uint32)
} // installStaticFunction__B_u

static void installStaticFunction__P()
{
    DefAsm(Pexit_lisp, (1, 1))
        call(QBunbox_uint32);
        push($r0);
        call("KERNEL32.DLL", "ExitProcess");
    EndAsm(Pexit_lisp)

    // %type-error
    // $r0 = datum
    // $r1 = typespec
    DefAsm(Ptype_error, ())
        mov(ea_m_fn(), Qerror);
        mov($r2, $r0);
        mov($r4, $r1);
        mov($r0, Qtype_error);
        mov($r1, Kdatum);
        mov($r3, Kexpected_type);
        mov($rn, Fixnum::Encode(5));
        jmp(QBfuncall);
    EndAsm(Ptype_error);

    // %undefined-function
    // $r0 = function-name
    DefAsm(Pundefined_function, ())
        mov(ea_m_fn(), Qerror);
        mov($r2, $r0);
        mov($r0, Qundefined_function);
        mov($r1, Kname);
        mov($rn, Fixnum::Encode(3));
        jmp(QBfuncall);
    EndAsm(Pundefined_function)
} // installStaticFunction__P

static void installStaticFunction_A()
{
    installCharPred(Qalpha_char_p, Character::Attr_Alpha);
    installCharPred(Qalphanumericp, Character::Attr_AlphaNumeric);
    InstallPredicate(Qarrayp, CLASSD_array_min, CLASSD_array_max);

    DefAsm(address_of, (1, 1))
        shl($r0, Fixnum::TagBits);
        or($r0, $r0);   // CF=0
        ret();
    EndAsm(address_of)

    // (atom x) = (not (consp x))
    DefAsm(atom, (1, 1))
        lea($r1, ea($r0, -Cons::Tag2));
        and($r1, 7); // also set CF=0
        mov($r0, nil);
        mov($r2, Qt);
        cmovne($r0, $r2);
        ret();
    EndAsm(atom)
} // installStaticFunction_A

static void installStaticFunction_B()
{
    installCharPred(Qboth_case_p, Character::Attr_BothCase);
} // installStaticFunction_B

static void installStaticFunction_C()
{
    installCxR(Qcar, offsetof(Cons, m_car));
    installCxR(Qcdr, offsetof(Cons, m_cdr));

    installCxR(Qcaar, Qcar, Qcar);   // 00
    installCxR(Qcadr, Qcar, Qcdr);   // 01

    installCxR(Qcdar, Qcdr, Qcar);   // 10
    installCxR(Qcddr, Qcdr, Qcdr);   // 11

    installCxR(Qcaaar, Qcar, Qcaar);    // 000
    installCxR(Qcaadr, Qcar, Qcadr);    // 001
    installCxR(Qcadar, Qcar, Qcdar);    // 010
    installCxR(Qcaddr, Qcar, Qcddr);    // 011

    installCxR(Qcdaar, Qcdr, Qcaar);    // 100
    installCxR(Qcdadr, Qcdr, Qcadr);    // 101
    installCxR(Qcddar, Qcdr, Qcdar);    // 110
    installCxR(Qcdddr, Qcdr, Qcddr);    // 111

    installCxR(Qcaaaar, Qcar, Qcaaar); // 0000
    installCxR(Qcaaadr, Qcar, Qcaadr); // 0001
    installCxR(Qcaadar, Qcar, Qcadar); // 0010
    installCxR(Qcaaddr, Qcar, Qcaddr); // 0011
    installCxR(Qcadaar, Qcar, Qcdaar); // 0100
    installCxR(Qcadadr, Qcar, Qcdadr); // 0101
    installCxR(Qcaddar, Qcar, Qcddar); // 0110
    installCxR(Qcadddr, Qcar, Qcdddr); // 0111

    installCxR(Qcdaaar, Qcdr, Qcaaar); // 1000
    installCxR(Qcdaadr, Qcdr, Qcaadr); // 1001
    installCxR(Qcdadar, Qcdr, Qcadar); // 1010
    installCxR(Qcdaddr, Qcdr, Qcaddr); // 1011
    installCxR(Qcddaar, Qcdr, Qcdaar); // 1100
    installCxR(Qcddadr, Qcar, Qcdadr); // 1101
    installCxR(Qcdddar, Qcar, Qcddar); // 1110
    installCxR(Qcddddr, Qcar, Qcdddr); // 1111

    installCharCmpCs(QcharGS2,    tttn_BE);   // char>/2
    installCharCmpCs(QcharGQS2,   tttn_B);    // char>=/2

    installCharCmpCs(QcharLS2,    tttn_AE);   // char</2
    installCharCmpCs(QcharLQS2,   tttn_A);    // char<=/2

    installCharCmpCs(QcharQS2,    tttn_NE);   // char=/2
    installCharCmpCs(QcharSQS2,   tttn_E);    // char/=/2

    installCharCmpCi(Qchar_equalS2,     tttn_NE);
    installCharCmpCi(Qchar_not_equalS2, tttn_E);

    installCharCmpCi(Qchar_greaterpS2,       tttn_BE);
    installCharCmpCi(Qchar_not_greaterpS2,   tttn_A);

    installCharCmpCi(Qchar_lesspS2,     tttn_AE);
    installCharCmpCi(Qchar_not_lesspS2, tttn_B);

    DefAsm(char_category, (1, 1))
        Label not_char;

        lea($r1, ea($r0, -CHAR_u0000->ToInt()));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, ea($r0, OffsetOf(Character, m_data)));
        and($r0, Character::Attr_CategoryMask);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        thread_svc(TypeError);
    EndAsm(char_category)

    // char-code character => integer
    DefAsm(char_code, (1, 1))
        Label not_char;

        lea($r1, ea($r0, static_cast<int32>(CHAR_u0000->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        sub($r0, CHAR_u0000);
        shr($r0, Character::ShiftCount - Fixnum::TagBits);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        thread_svc(TypeError);
    EndAsm(char_code)

    installCharCase(Qchar_downcase, Character::Attr_UpperCase);
    installCharCase(Qchar_upcase,   Character::Attr_LowerCase);

    setf_symbol_function(symbol_function(Qchar_code), Qchar_int);

    DefAsm(classd_of, (1, 1))
        Label tagtbl;

        mov($r1, $r0);

        and($r1, 15);
        add($r1, $r1);  // $r1 = ($r1 & 15) * 2
        add($r1, $r1);  // $r1 = ($r1 & 15) * 4
        jmp(ea($r1, tagtbl));

      label_def(tag_fixnum);
        mov($r0, CLASSD_fixnum);
        ret();

      label_def(tag_null);
        mov($r0, CLASSD_null);
        ret();

      label_def(tag_instance);
        mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
        ret();

      label_def(tag_record);
        mov($r0, ea($r0, OffsetOf(Record, m_classd)));
        ret();

      label_def(tag_cons);
        mov($r0, CLASSD_cons);
        ret();

      label_def(tag_function);
        mov($r0, ea($r0, OffsetOf(Function, m_classd)));
        ret();

      label_def(tag_invalid);
        mov($r0, CLASSD_invalid_object);
        ret();

        align(4);
      label(tagtbl);
        dd(tag_fixnum);         //  0 0000
        dd(tag_record);         //  1 0001
        dd(tag_null);           //  2 0010
        dd(tag_invalid);        //  3 0011

        dd(tag_fixnum);         //  4 0100
        dd(tag_function);       //  5 0101
        dd(tag_cons);           //  6 0110
        dd(tag_instance);       //  7 0111

        dd(tag_fixnum);         //  8 1000
        dd(tag_record);         //  9 1001
        dd(tag_invalid);        // 10 1010
        dd(tag_invalid);        // 11 1011

        dd(tag_fixnum);         // 12 1100
        dd(tag_invalid);        // 13 1101
        dd(tag_cons);           // 14 1110
        dd(tag_instance);       // 15 1111
    EndAsm(classd_of)

    // CLASS-OF is almost as same as TYPE-OF. We can implement class-of as
    //  (class-of x) = (find-class (type-of x)) or
    //  (type-of x) = (class-name (class-of x)
    // For fast execution of class-of, we implment class-of in assembler.
    DefAsm(class_of, (1, 1))
        Label tagtbl;

        mov($r1, $r0);

        and($r1, 15);
        add($r1, $r1);  // $r1 = ($r1 & 15) * 2
        add($r1, $r1);  // $r1 = ($r1 & 15) * 4
        jmp(ea($r1, tagtbl));

      label_def(tag_fixnum);
        mov($r0, CLASS_fixnum);
        ret();

      label_def(tag_null);
        mov($r0, CLASS_null);
        ret();

      label_def(tag_instance);
        mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
        ret();

      label_def(tag_record);
        mov($r0, ea($r0, OffsetOf(Record, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
        ret();

      label_def(tag_cons);
        mov($r0, CLASS_cons);
        ret();

      label_def(tag_function);
        mov($r0, ea($r0, OffsetOf(Function, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
        ret();

      label_def(tag_invalid);
        mov($r0, CLASS_invalid_object);
        ret();

        align(4);
      label(tagtbl);
        dd(tag_fixnum);         //  0 0000
        dd(tag_record);         //  1 0001
        dd(tag_null);           //  2 0010
        dd(tag_invalid);        //  3 0011

        dd(tag_fixnum);         //  4 0100
        dd(tag_function);       //  5 0101
        dd(tag_cons);           //  6 0110
        dd(tag_instance);       //  7 0111

        dd(tag_fixnum);         //  8 1000
        dd(tag_record);         //  9 1001
        dd(tag_invalid);        // 10 1010
        dd(tag_invalid);        // 11 1011

        dd(tag_fixnum);         // 12 1100
        dd(tag_invalid);        // 13 1101
        dd(tag_cons);           // 14 1110
        dd(tag_instance);       // 15 1111
    EndAsm(class_of)

    // code-char integer => (or character null)
    DefAsm(code_char, (1, 1))
        Label got_fixnum;
        Label not_int;
        Label ret_false;

        test($r0, Fixnum::TagMask);
        je(got_fixnum);

        test($r0, Record::Tag);
        jne(not_int);

        cmp(ea($r0, OffsetOf(Record, m_classd)), CLASSD_bignum);
        jne(not_int);

      label(ret_false);
        mov($r0, nil);
        ret();

      label(got_fixnum);
        cmp($r0, Character::Max * Fixnum::One);
        ja(ret_false);

        ASSERT(sizeof(Character) == 8);
        lea($r0, ea(static_cast<int32>(CHAR_u0000->ToInt()),
                    $r0_2 ) );
        ret();

      label(not_int);
        mov($r1, Qchar_code);
        thread_svc(TypeError);
    EndAsm(code_char)

    InstallPredicate(Qconsp, Arch::Tag_Cons);
} // installStaticFunction_C

static void installStaticFunction_E()
{
    // (endp x) = (or (null x) (if (consp x) nil (error 'type-error)))
    DefAsm(endp, (1, 1))
        Label ret_true;
        Label type_error;

        mov($r2, nil);
        cmp($r0, $r2);
        je(ret_true);           // CF=0 if $r0 is nil.

        lea($r1, ea($r0, -Cons::Tag2));
        and($r1, 7);     // CF=0
        jne(type_error);
        mov($r0, $r2);
        ret();

      label(ret_true);
        mov($r0, $true);
        ret();

      label(type_error);
        mov($r1, Qcons);
        thread_svc(TypeError);
    EndAsm(endp)

    DefAsm(eq, (2, 2))
        cmp($r0, $r1);
        mov($r0, nil);
        mov($r1, $tcb);
        cmove($r0, $r1);
        xor($r1, $r1);  // xor may be faster than clc.
        ret();
    EndAsm(eq)
} // installStaticFunction_E

static void installStaticFunction_F()
{
    InstallPredicate(Qfunctionp, Arch::Tag_FunObj);

    // We can implement funcall as
    //  (defun funcall (fn &rest args)
    //    (multiple-value-call fn (values-list args)) )
    // We don't want to construct list for rest arguments.
    DefAsm(funcall, (1, Arch::MultipleValuesLimit))
        Label do_call;
        Label got_symbol;
        Label not_function;
        Label undefined_function;

        mov(ea_mv_value(1), $r1);
        mov(ea_mv_value(2), $r2);
        mov(ea_mv_value(3), $r3);
        mov(ea_mv_value(4), $r4);

        // Is funcallable object?
        lea($r1, ea($r0, -FunObj::Tag));
        test($r1, Arch::TagMask);
        je(do_call);

        // Is symbol?
        cmp($r0, nil);
        je(got_symbol);

        lea($r1, ea($r0, -Symbol::Tag));
        test($r1, Symbol::TagMask);
        jne(not_function);

      label(got_symbol);
        mov($r1, $r0);
        and($r1, ~Arch::TagMask);
        mov($r1, ea($r1, offsetof(Symbol, m_function)));
        cmp($r1, nil);
        je(undefined_function);

        mov($r0, $r1);

      label(do_call);
        add($r0, sizeof(FunObj) - FunObj::Tag);
        mov(ea(esp, -4), $r0);
        sub($rn, one);  // -1 for fn
        mov(ea_m_n(), $rn);

        // ecx = $rn
        shr(ecx, 2);
        lea(esi, ea_mv_value(1));
        lea(edi, ea_mv_value(0));

        emitOp(op_REP);
        emitOp(op_MOVSD);

        mov($r0, ea_mv_value(0));
        mov($r1, ea_mv_value(1));
        mov($r2, ea_mv_value(2));
        mov($r3, ea_mv_value(3));
        mov($r4, ea_mv_value(4));
        mov($rn, ea_m_n());
        jmp(ea(esp, -4));

      label(not_function);
        mov($r1, Qfunction_designator);
        thread_svc(TypeError);

      label(undefined_function);
        thread_svc(UndefinedFunction);
    EndAsm(funcall)
} // installStaticFunction_F

static void installStaticFunction_G()
{
    installCharPred(Qgraphic_char_p, Character::Attr_Graphic);

    DefAsm(get_internal_real_time, (0, 0))
        frame(FunObj::FrameKind_Fixed, sizeof(ToForeignFrame));

        sub($sp, sizeof(ToForeignFrame));
        mov($r0, ea_m_fp());
        mov(ea_($sp, ToForeignFrame, m_pOuter), $sp);
        mov(ea_($sp, ToForeignFrame, m_eFrame), Frame::Type_ToForeign);
        mov(ea_($sp, ToForeignFrame, m_cbArgs), $rn);   // m_cbArgs=0
        mov(ea_m_fp(), $sp);
        call("KERNEL32.DLL", "GetTickCount");
        mov($r1, ea_($sp, ToForeignFrame, m_pOuter));
        and($r0, Fixnum::MostPositive);
        shl($r0, Fixnum::TagBits);
        mov(ea_m_fp(), $r1);
        add($sp, sizeof(ToForeignFrame));
        ret();
    EndAsm(get_internal_real_time)
} // installStaticFunction_G

static void installStaticFunction_I()
{
    // [I]
    DefAsm(identity, (1, 1))
        ret();
    EndAsm(identity)

    // integerp = (or fixnum bignum)
    DefAsm(integerp, (1, 1))
        Label ret_false;
        Label ret_true;

        test($r0, 3);
        je(ret_true);

        lea($r1, ea($r0, -Arch::Tag_Record));
        mov($r2, nil);
        test($r1, Arch::TagMask);
        jne(ret_false);

        cmp(ea($r1), CLASSD_bignum);
        cmovne($r0, $r2);

      label(ret_true);
        ret();

      label(ret_false);
        mov($r0, $r2);
        ret();
    EndAsm(integerp)
} // installStaticFunction_I

static void installStaticFunction_K()
{
    DefAsm(keywordp, (1, 1))
        Label ret_false;

        lea($r1, ea($r0, -Arch::Tag_Record));
        mov($r2, nil);
        cmp(ea($r1), CLASSD_symbol);
        jne(ret_false);

        cmp(ea($r1, offsetof(Layout_symbol, m_package)), PKG_keyword);
        cmovne($r0, $r2);
        ret();

      label(ret_false);
        mov($r0, $r2);
        ret();
    EndAsm(keywordp)
} // installStaticFunction_K

static void installStaticFunction_L()
{
    InstallPredicate(Qlistp, Arch::Tag_Cons, 3);
    installCharPred(Qlower_case_p, Character::Attr_LowerCase);
} // installStaticFunction_L

static void installStaticFunction_N()
{
    #define define_not_or_null(mp_name) \
        DefAsm(mp_name, (1, 1)) \
            mov($r1, nil); \
            cmp($r0, $r1); \
            mov($r0, t); \
            cmovne($r0, $r1); \
            xor($r1, $r1);  \
            ret(); \
        EndAsm(mp_name)

    define_not_or_null(not)
    define_not_or_null(null)

    #undef define_not_or_null
} // installStaticFunction_N

static void installStaticFunction_P()
{
    InstallPredicate(Qpackagep, CLASSD_package);
    // FIXME 2008-01-14 yosi@msn.com pathnamep
} // installStaticFunction_P

static void installStaticFunction_S()
{
    installCharPred(Qstandard_char_p, Character::Attr_Standard);

    DefAsm(schar, (2, 2))
        Label bad_index;
        Label not_simple_string;

        lea($r2, ea($r0, -SimpleString::Tag));
        test($r2, Arch::TagMask);
        jne(not_simple_string);

        cmp(ea($r0, -SimpleString::Tag), CLASSD_simple_string);
        jne(not_simple_string);

        test($r1, Fixnum::TagMask);
        jne(bad_index);

        cmp($r1, ea($r0, OffsetOf(SimpleString, m_length)));
        jae(bad_index);

        shr($r1, 1);
        movzxw(
            $r0,
            ea($r0, sizeof(Layout_simple_string) - SimpleString::Tag, $r1) );
        shl($r0, Character::ShiftCount);
        add($r0, CHAR_u0000->ToInt());

        ret();

      label(bad_index);
        mov($rn, two);
        mov(ea_m_fn(), Qsequence_index_error);
        call(QBfuncall);

      label(not_simple_string);
        mov($r1, Qsimple_string);
        thread_svc(TypeError);
    EndAsm(schar)

    #define Qsetf_schar list(Qsetf, Qschar)
    DefAsm(setf_schar, (3, 3))
        Label bad_index;
        Label not_character;
        Label not_simple_string;

        lea($rn, ea($r0, -Character::Tag));
        test($rn, Arch::TagMask);
        jne(not_character);

        cmp(ea($r0, -Character::Tag), CLASSD_character);
        jne(not_character);

        lea($r3, ea($r1, -SimpleString::Tag));
        test($r3, Arch::TagMask);
        jne(not_simple_string);

        cmp(ea($r1, -SimpleString::Tag), CLASSD_simple_string);
        jne(not_simple_string);

        test($r2, Fixnum::TagMask);
        jne(bad_index);

        cmp($r2, ea($r1, OffsetOf(SimpleString, m_length)));
        jae(bad_index);

        lea($rn, ea($r0, -CHAR_u0000->ToInt()));
        shr($rn, Character::ShiftCount);

        shr($r2, 1);
        emitOp(op_OPDSIZ);
        mov(
            ea($r1, sizeof(Layout_simple_string) - SimpleString::Tag, $r2),
            $rn );

        ret();

      label(bad_index);
        mov($r0, $r1);
        mov($r1, $r2);
        mov($rn, two);
        mov(ea_m_fn(), Qsequence_index_error);
        call(QBfuncall);

      label(not_character);
        mov($r1, Qcharacter);
        thread_svc(TypeError);

      label(not_simple_string);
        mov($r0, $r1);
        mov($r1, Qsimple_string);
        thread_svc(TypeError);
    EndAsm(setf_schar)
    #undef Qsetf_schar
} // installStaticFunction_S

static void installStaticFunction_T()
{
    DefAsm(type_of, (1, 1))
        Label tagtbl;

        mov($r1, $r0);
        and($r1, 15);
        add($r1, $r1);  // $r1 = ($r1 & 15) * 2
        add($r1, $r1);  // $r1 = ($r1 & 15) * 4
        jmp(ea($r1, tagtbl));

      label_def(tag_fixnum);
        mov($r0, Qfixnum);
        ret();

      label_def(tag_null);
        mov($r0, Qnull);
        ret();

      label_def(tag_instance);
        mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_type)));
        ret();

      label_def(tag_record);
        mov($r0, ea($r0, OffsetOf(Record, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_type)));
        ret();

      label_def(tag_cons);
        mov($r0, Qcons);
        ret();

      label_def(tag_function);
        mov($r0, ea($r0, OffsetOf(Function, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_type)));
        ret();

      label_def(tag_invalid);
        mov($r0, Qinvalid_object);
        ret();

        align(4);
      label(tagtbl);
        dd(tag_fixnum);         //  0 0000
        dd(tag_record);         //  1 0001
        dd(tag_null);           //  2 0010
        dd(tag_invalid);        //  3 0011

        dd(tag_fixnum);         //  4 0100
        dd(tag_function);       //  5 0101
        dd(tag_cons);           //  6 0110
        dd(tag_instance);       //  7 0111

        dd(tag_fixnum);         //  8 1000
        dd(tag_record);         //  9 1001
        dd(tag_invalid);        // 10 1010
        dd(tag_invalid);        // 11 1011

        dd(tag_fixnum);         // 12 1100
        dd(tag_invalid);        // 13 1101
        dd(tag_cons);           // 14 1110
        dd(tag_instance);       // 15 1111
    EndAsm(type_of)
} // installStaticFunction_T

static void installStaticFunction_U()
{
    DefAsm(unbound_marker, ())
        mov($r0, MARKER_unbound);
        ret();
    EndAsm(unbound_marker)

    installCharPred(Qupper_case_p, Character::Attr_UpperCase);
} // installStaticFunction_U

static void installStaticFunction_V()
{
    DefAsm(values, ())
        stc();          // tell caller $rn holds number of values.
        ret();
    EndAsm(values)

    // values*
    //  This function is almost same as values but the last argument
    //  must be list and elemetns of the list to values.
    //
    //  This function always sets CF=1 even if number of values is one.
    //
    DefAsm(valuesA, (1, 1, 1))
        Label exit_0, exit_1, exit_2, exit_3, exit_4;
        Label exit_more_than_5;
        Label exit_tbl;

        Label from_0, from_2, from_3, from_4, from_5;
        Label from_tbl;

        Label cont_3, cont_4;

        Label not_list;
        Label loop;
        Label loop_end;

        Label too_many_arguments;

        mov(ea_mv_value(0), $r0);
        mov(ea_mv_value(1), $r1);
        sub($rn, one);
        cmp($rn, Fixnum::Encode(4));
        jg(from_5);
        jmp(ea($rn, from_tbl));

      label(from_0); mov($r1, $r0); jmp(loop);
      label(from_2); mov($r1, $r2); jmp(loop);
      label(from_3); mov($r1, $r3); jmp(cont_3);
      label(from_4); mov($r1, $r4); jmp(cont_4);

      label(from_5); mov($r1, ea_mv_value($rn));
                     mov(ea_mv_value(4), $r4);
      label(cont_4); mov(ea_mv_value(3), $r3);
      label(cont_3); mov(ea_mv_value(2), $r2);

      label(loop);
        // loop_end if $r1 == nil
        cmp($r1, nil);
        je(loop_end);

        // Can we have one more value?
        cmp($rn, Fixnum::Encode(Arch::MultipleValuesLimit));
        jge(too_many_arguments);

        // not_list if $r1 isn't cons
        lea($r0, ea($r1, -Cons::Tag2));
        and($r0, 7);
        jne(not_list);

        // $r0 = car($r1)
        mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::Tag2));

        // mv_value[$rn] = $r0
        mov(ea_mv_value($rn), $r0);

        // $rn += 1
        add($rn, one);

        // $r1 = cdr($r1)
        mov($r1, ea($r1,  offsetof(Cons, m_cdr) - Cons::Tag2));
        jmp(loop);

      label(loop_end);
        cmp($rn, Fixnum::Encode(5));
        jge(exit_more_than_5);
        jmp(ea($rn, exit_tbl));

        // Set register values
      label(exit_more_than_5); mov($r4, ea_mv_value(4));
      label(exit_4); mov($r3, ea_mv_value(3));
      label(exit_3); mov($r2, ea_mv_value(2));
      label(exit_2); mov($r1, ea_mv_value(1));
      label(exit_1); mov($r0, ea_mv_value(0));
      label(exit_0);
        stc();
        ret();

      label(not_list);
        mov($r0, $r1);
        mov($r1, Qlist);
        thread_svc(TypeError);

      label(too_many_arguments);
        mov($r0, Qtoo_many_values);
        mov($rn, one);
        mov(ea_m_fn(), Qerror);
        call(QBfuncall);

        align(4);
      label(from_tbl);
        dd(from_0);
        dd(loop);
        dd(from_2);
        dd(from_3);
        dd(from_4);

        align(4);
      label(exit_tbl);
        dd(exit_0);
        dd(exit_1);
        dd(exit_2);
        dd(exit_3);
        dd(exit_4);
    EndAsm(valuesA)

    DefAsm(values_list, (1, 1))
        jmp(QvaluesA);
    EndAsm(values_list)
} // installStaticFunction_V

static void installStaticFunction_W()
{
    installCharPred(Qwhitespace_char_p, Character::Attr_Whitespace);

    {
        class MyAsm : protected X86Asm
        {
            public: void Run()
            {
                m_name = QAmacroexpand_hookA;
                emitPrologue(3, 3);

                mov(ea_m_fn(), $r0);
                mov($r0, $r1);
                mov($r1, $r2);
                mov($rn, two);
                jmp(QBfuncall);

                emitEpilogue();

                Val hook = install();
                TLV(Amacroexpand_hookA) = hook;

                Val tlvrec = svref(
                    VAR(Atlv_vectorA),
                    Fixnum::Encode(TLV_Amacroexpand_hookA) );

                tlvrec->StaticCast<TlvRecord>()->m_value = hook;
            } // Run
        } oAsm;
        oAsm.Run();
    }
} // installStaticFunction_W

/// <summary>
///   Pass 1 installation of static functions.
///   <para>
///     Note: You can't call function ERROR in static functions installed
///     in this function.
///   </para>
/// </summary>
void InstallStaticFunctions()
{
    // [!]
    // !funcall
    //  thread.m_fn <- callable
    DefAsm(Bfuncall, ())
        Label got_symbol;
        Label not_function;
        Label test_function;
        Label test_setf;
        Label undefined_function;

        mov(ea_mv_value(0), $r0);
        mov(ea_mv_value(1), $r1);

        mov($r0, ea_m_fn());

        // test callee is symbol
        cmp($r0, nil);
        je(got_symbol);

        lea($r1, ea($r0, -Symbol::Tag));
        test($r1, Symbol::TagMask);
        jne(test_function);

        cmp(ea($r1, offsetof(Symbol, m_classd)), CLASSD_symbol);
        jne(test_setf);

      label(got_symbol);
        mov($r0, $r0);
        and($r1, ~Symbol::TagMask);
        mov($r1, ea($r1, offsetof(Symbol, m_function)));
        cmp($r1, nil);
        je(undefined_function);

        mov($r0, $r1);

        // test callee is function
      label(test_function);
        lea($r1, ea($r0, -FunObj::Tag));
        and($r1, Arch::TagMask);
        jne(not_function);

        add($r0, sizeof(FunObj) - FunObj::Tag);
        mov(ea($sp, -4), $r0);
        mov($r0, ea_mv_value(0));
        mov($r1, ea_mv_value(1));
        jmp(ea($sp, -4));

        // test callee is setf
      label(test_setf);
        cmp(ea($r1, offsetof(SetfCell, m_classd)), CLASSD_setf_cell);
        jne(not_function);

        mov($r1, ea($r1, OffsetOf(SetfCell, m_function)));
        cmp($r1, nil);
        je(undefined_function);

        mov($r0, $r1);
        jmp(test_function);

        // Signal undefined-function error
      label(undefined_function);
        mov($r0, ea_m_fn());
        thread_svc(UndefinedFunction);

        // Signal type-error function-designator
      label(not_function);
        mov($r0, ea_m_fn());
        mov($r1, Qfunction_designator);
        thread_svc(TypeError);
    EndAsm(Bfuncall)

    #if _WIN32
    // !!funcall
    //  ecx = thread
    //  Call lisp function within FromFroeign frame and TryCatch frame.
    //
    //  Note: This function is called by foreign function.
    DefAsm(BBfuncall, ())

        struct LocalFrame
        {
            TryCatchFrame           m_oTryCatchFrame;
            TryCatchFrame::Catch    m_oCatch;
            FromForeignFrame        m_oFromForeignFrame;
            int32                   m_ebx;
            int32                   m_ebp;
            int32                   m_esi;
            int32                   m_edi;
        }; // LocalFrame

        frame(FunObj::FrameKind_Fixed, sizeof(LocalFrame));

        Label catch_error;

        sub($ESP, sizeof(LocalFrame));

        // Save caller save register for C
        mov(ea($sp, offsetof(LocalFrame, m_ebx)), $EBX);
        mov(ea($sp, offsetof(LocalFrame, m_ebp)), $EBP);
        mov(ea($sp, offsetof(LocalFrame, m_esi)), $ESI);
        mov(ea($sp, offsetof(LocalFrame, m_edi)), $EDI);

        // ebp <- pThread
        mov($tcb, ecx);

        // Make FromForeign Frame
        lea($r1,
            ea($sp,
                offsetof(LocalFrame, m_oFromForeignFrame) +
                offsetof(FromForeignFrame, m_pOuter) ) );

        mov($r0, ea_m_fp());

        mov(ea($r1), $r0);

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oFromForeignFrame) +
                offsetof(FromForeignFrame, m_eFrame) ),
            Frame::Type_FromForeign );

        // Make TryCatchFrame
        mov(
            ea($sp,
                offsetof(LocalFrame, m_oTryCatchFrame) +
                offsetof(TryCatchFrame, m_pOuter) ),
            $r1 );

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oTryCatchFrame) +
                offsetof(TryCatchFrame, m_eFrame) ),
            Frame::Type_TryCatch );

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oTryCatchFrame) +
                offsetof(TryCatchFrame, m_sp) ),
            $sp );

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oTryCatchFrame) +
                offsetof(TryCatchFrame, m_fn) ),
            FUNOBJ_self );

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oTryCatchFrame) +
                offsetof(TryCatchFrame, m_cbCatchs) ),
            sizeof(TryCatchFrame::Catch) );

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oCatch) +
                offsetof(TryCatchFrame::Catch, m_type) ),
            Qerror );

        mov(
            ea($sp,
                offsetof(LocalFrame, m_oCatch) +
                offsetof(TryCatchFrame::Catch, m_nIp) ),
            catch_error );

        // Set $fp
        mov(ea_m_fp(), $sp);

        // Set arguments
        mov($rn, ea_m_n());
        mov($r0, ea_mv_value(0));
        mov($r1, ea_mv_value(1));
        mov($r2, ea_mv_value(2));
        mov($r3, ea_mv_value(3));
        mov($r4, ea_mv_value(4));

        // Call lisp function
        call(QBfuncall);

        // Save primary value
        mov(ea_mv_value(0), $r0);

        // Set number of values
        mov($r0, one);
        cmovnc($rn, $r0);
        mov(ea_m_n(), $rn);

        // Save values
        mov(ea_mv_value(1), $r1);
        mov(ea_mv_value(2), $r2);
        mov(ea_mv_value(3), $r3);
        mov(ea_mv_value(4), $r4);

        mov($EAX, nil);

      label(catch_error);
        // Restore m_fp
        mov($ECX,
            ea($sp,
                offsetof(LocalFrame, m_oFromForeignFrame) +
                offsetof(FromForeignFrame, m_pOuter) ) );
        mov(ea_m_fp(), ecx);

        // Restore caller save register
        mov($EBX, ea($sp, offsetof(LocalFrame, m_ebx)));
        mov($EBP, ea($sp, offsetof(LocalFrame, m_ebp)));
        mov($ESI, ea($sp, offsetof(LocalFrame, m_esi)));
        mov($EDI, ea($sp, offsetof(LocalFrame, m_edi)));

        add($sp, sizeof(LocalFrame));

        ret();
    EndAsm(BBfuncall)
    #endif

    // !&allow-other-keys
    //  $r0 = rest (list)
    //  $r1 = keys (simple-vector)
    DefAsm(BAallow_other_keys, (2, 2))
        Label parse_key_loop_got;
        Label parse_key_loop_test;
        Label parse_key_loop_loop;
        Label parse_$rloop_end;
        Label parse_$rloop_odd;
        Label parse_$rloop_step;
        Label parse_$rloop_test;

        // $r2 = start of keys
        lea($r2, ea($r1,
            sizeof(SimpleVector) - SimpleVector::Tag ) );

        // $r3 = end of keys
        mov($r3, ea($r1,
            offsetof(SimpleVector, m_length) - SimpleVector::Tag ) );
        add($r3, $r2);

        mov($r1, $r0);      // runner = rest

        xor($r4, $r4);
        mov(ea_mv_value(0), $r4);

        jmp(parse_$rloop_test);

      label(parse_key_loop_got);
        test(ea_mv_value(0), $rn);    // Do we already have the key?
        jne(parse_$rloop_step);       // Yes, we have.

        or(ea_mv_value(0), $rn);      // Set flag for the key

        // get value from list
        mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::Tag2));

        // save it to thread.mv_value
        sub($r4, $r2);
        mov(ea($tcb, offsetof(Thread, mv_value[1]), $r4), $r0);
        jmp(parse_$rloop_step);

        ////////////////////////////////////////////////////////////
        //
        // arg loop
        //
      label_def(parse_$rloop);
        mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::Tag2));
        mov($r1, ea($r1, offsetof(Cons, m_cdr) - Cons::Tag2));

        lea($r4, ea($r1, -Cons::Tag2));
        and($r4, Cons::Tag2 >> 1);
        jne(parse_$rloop_odd);

        ////////////////////////////////////////////////////////////
        //
        // parse_key_loop
        //  $r0   key
        //  $r2   start of key
        //  $r3   end of key
        //  $r4   key runner
        //  reg_n   bit
        //
        //  $r1   arg runner
        mov($rn, one);              // bit = 1
        mov($r4, $r2);              // key_runner = key_start
        jmp(parse_key_loop_test);

      label(parse_key_loop_loop);
        cmp($r0, ea($r4));          // key == *key_runner
        je(parse_key_loop_got);

        shl($rn, 1);                  // bit <<= 1
        add($r4, sizeof(Val));        // key_runner++

      label(parse_key_loop_test);
        cmp($r4, $r3);              // key_runner == key_start
        jne(parse_key_loop_loop);

        ////////////////////////////////////////////////////////////
        //
        // arg loop
        //
      label(parse_$rloop_step);
        mov($r1, ea($r1, offsetof(Cons, m_cdr) - Cons::Tag2));

      label(parse_$rloop_test);
        lea($r0, ea($r1, -Cons::Tag2));
        and($r0, Cons::Tag2 >> 1);
        je(parse_$rloop);

        ////////////////////////////////////////////////////////////
        //
        // End of parse key
        //
      label(parse_$rloop_odd);
      label(parse_$rloop_end);
        mov($rn, ea($r2, -4));
        add($rn, Fixnum::One);

        mov($r0, ea_mv_value(0));
        mov($r1, ea_mv_value(1));
        mov($r2, ea_mv_value(2));
        mov($r3, ea_mv_value(3));
        mov($r4, ea_mv_value(4));

        stc();
        ret();
    EndAsm(BAallow_other_keys)

    //////////////////////////////////////////////////////////////////////
    //
    // .check-keys
    //  $r0 = rest (list)
    //  $r1 = keys (simple-vector)
    DefAsm(BAkey, (2, 2))
        Label check_end;
        Label check_got;
        Label check_test;

        frame(FunObj::FrameKind_Fixed, 8);

        sub($sp, 8);
        mov(ea($sp, 0), nil);   // allow-other-keys
        mov(ea($sp, 4), $r0); // rest
        mov($r2, $r1);      // keys
        mov($r1, $r0);      // save rest
        mov($r3, $r0);      // runner
        jmp(check_test);

      label_def(check_loop);
        mov($r0, ea($r3, offsetof(Cons, m_car) - Cons::Tag2));
        mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag2));

        // Is runner cons?
        lea($r4, ea($r3, -Cons::Tag2));
        and($r4, Cons::Tag2 >> 1);
        jne(check_end);

        // Is key :allow-other-keys?
        cmp($r0, Kallow_other_keys);
        je(check_got);

        mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag2));

      label(check_test);
        // Is runner cons?
        lea($r0, ea($r3, -Cons::Tag2));
        and($r0, Cons::Tag2 >> 1);
        je(check_loop);
        jmp(check_end);

      label(check_got);
        mov($r0, ea($r3, offsetof(Cons, m_car) - Cons::Tag2));
        mov(ea($sp, 0), $r0);

      label(check_end);

        ////////////////////////////////////////////////////////////
        //
        // Parse keywords argumetns
        //
        Label parse_$rend;
        Label parse_$rnot_cons;
        Label parse_$rodd;
        Label parse_$rstep;
        Label parse_$rtest;

        Label parse_key_got;
        Label parse_key_invalid;
        Label parse_key_test;

        mov($r3, $r1);  // runner
        xor($r0, $r0);  // flags
        mov(ea_mv_value(0), $r0);

        // $r1 = end of keys
        mov($r1, ea($r2,
            offsetof(SimpleVector, m_length) - SimpleVector::Tag ) );

        lea($r1, ea($r1,
            sizeof(SimpleVector) - SimpleVector::Tag, $r2 ) );

        // $r2 = start of keys
        add($r2, sizeof(SimpleVector) - SimpleVector::Tag);

        jmp(parse_$rtest);

      label_def(parse_$rtop);
        lea($r4, ea($r3, -Cons::Tag2));
        and($r4, Cons::Tag2 >> 1);
        jne(parse_$rnot_cons);

        mov($rn, ea($r3, offsetof(Cons, m_car) - Cons::Tag2));
        mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag2));

        cmp($r3, nil);
        je(parse_$rodd);

        lea($r4, ea($r3, -Cons::Tag2));
        and($r4, Cons::Tag2 >> 1);
        jne(parse_$rnot_cons);

        ////////////////////////////////////////////////////////////
        //
        // Check key in table
        //  $r0   bit
        //  $r1   end of keys.mv_element
        //  $r2   start keys.mv_element
        //  $r3   runner
        //  $r4   ptr to keys.mv_element
        //  $rn   key
        //
        mov($r4, $r2);
        mov($r0, one);
        jmp(parse_key_test);

      label_def(parse_key_loop);
        cmp($rn, ea($r4));
        je(parse_key_got);

        shl($r0, 1);
        add($r4, sizeof(Val));

      label(parse_key_test);
        cmp($r4, $r1);
        jne(parse_key_loop);

        // found unrecognized key
        cmp($rn, Kallow_other_keys); // key == :allow-other-keys
        je(parse_$rstep);

        cmp(ea($sp, 0), nil);   // allow-other-keys?
        jne(parse_$rstep);

        // Is key symbol?
        lea($r0, ea($rn, -Symbol::Tag));
        and($r0, 13);
        jne(parse_key_invalid);

        // (error 'si:unrecognized-keyword-argument :key key :keys keys)
        lea($r4,
            ea($r2,
               -static_cast<Int>(sizeof(SimpleVector)) +
               SimpleVector::Tag ) );

        mov($r3, Kkeys);
        mov($r2, $rn);
        mov($r1, Kkey);
        mov($r0, Qunrecognized_keyword_argument);
        mov($rn, Fixnum::Encode(5));
        mov(ea_m_fn(), Qerror);
        call(QBfuncall);

      label(parse_key_invalid);
        mov($r4, Qsymbol);
        mov($r3, Kexpected_type);
        mov($r2, $rn);
        mov($r1, Kdatum);
        mov($r0, Qinvalid_keyword_argument);
        mov($rn, Fixnum::Encode(5));
        mov(ea_m_fn(), Qerror);
        call(QBfuncall);

      label(parse_key_got);
        test(ea_mv_value(0), $r0);    // Do we have the key?
        jne(parse_$rstep);

        // get key value
        sub($r4, $r2);
        mov($rn, ea($r3, offsetof(Cons, m_car) - Cons::Tag2));

        // store to thread.mv_value
        mov(ea($tcb, offsetof(Thread, mv_value[1]), $r4), $rn);

        // Set flag for key
        or(ea_mv_value(0), $r0);

        ////////////////////////////////////////////////////////////
        //
        // arg loop
        //
      label(parse_$rstep);
        mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag2));

      label(parse_$rtest);
        cmp($r3, nil);
        jne(parse_$rtop);

      label(parse_$rend);
        mov($rn, ea($r2, -4));
        add($rn, Fixnum::One);

        mov($r0, ea_mv_value(0));
        mov($r1, ea_mv_value(1));
        mov($r2, ea_mv_value(2));
        mov($r3, ea_mv_value(3));
        mov($r4, ea_mv_value(4));

        add($sp, 8);
        stc();
        ret();

      label(parse_$rodd);
        cmp(ea($sp, 0), nil);   // allow-other-keys?
        jne(parse_$rend);

        mov($r0, Qodd_number_of_keyword_arguments);
        mov($r1, Karguments);
        mov($r2, ea($sp, 4)); // rest
        mov($rn, Fixnum::Encode(3));
        mov(ea_m_fn(), Qerror);
        call(QBfuncall);

      label(parse_$rnot_cons);
        cmp(ea($sp, 0), nil);   // allow-other-keys?
        jne(parse_$rend);

        mov($r0, ea($sp, 4)); // rest
        mov($r1, Qcons);
        thread_svc(TypeError);
    EndAsm(BAkey)

    // !arity-error
    DefAsm(Barity_error, ())
        frame(FunObj::FrameKind_Restify);

        mov(ea_mv_value(0), $r0);
        mov(ea_mv_value(1), $r1);
        mov(ea_mv_value(2), $r2);
        mov(ea_mv_value(3), $r3);
        mov(ea_mv_value(4), $r4);

        // Set class of condition to be signaled.
        mov($r0, Qtoo_many_arguments);
        mov($r1, Qtoo_few_arguments);
        cmovl($r0, $r1);

        // Get callee and store it into %thread.fn
        pop($r2);         // $r2 <- return address of callee
        and($r2, ~15);
        sub($r2, sizeof(FunObj));

        Label callee_done;
        Label callee_loop;
        Label callee_next;

        // Avoid FunObj::Cookie in 16 byte alignment.
        emitOp(op_NOP);

      label(callee_loop);
        cmp(ea($r2, offsetof(FunObj, m_cookie)), FunObj::Cookie);
        je(callee_done);
      label(callee_next);
        sub($r2, 16);       // 16 is alignment of function object
        jmp(callee_loop);

      label(callee_done);
        add($r2, FunObj::Tag); // $r2 = callee

        // Make restify frame
        //  argument list on stack => $r4
        Label args_done;
        Label args_loop;

        mov($r1, $sp);    // $r1 <= RP
        mov($r4, nil);

        test($rn, $rn);
        je(args_done);

        // Adjust stack for align 8
        mov(ea($sp, -4), $r4);  // make align pad gc-able
        and($sp, ~7);

        lea($r3, ea_mv_value($rn));

      label(args_loop);
        sub($r3, sizeof(Val));
        push($r4);            // set cdr <- $r4
        push(ea($r3));        // set car <- [esi]
        // FIXME 2008-01-03 yosi@msn.com We should use more meaningful name
        // instead of Cons::Tag2.
        lea($r4, ea($sp, Cons::Tag2));
        sub($rn, sizeof(Val));
        jne(args_loop);

      label(args_done);
        // $r4 = args

        push($r1);        // RP(RA address pointer)

        // Call function "error".
        mov($r1, Kfunction);
        mov($r3, Karguments);
        mov($rn, Fixnum::Encode(5));
        mov(ea_m_fn(), Qerror);
        call(QBfuncall);
    EndAsm(Barity_error)

    // !restify
    DefAsm(Brestify, ())
        Label alloc_cons_area;
        Label cont;
        Label exit;
        Label loop;
        Label rest_0;
        Label restore_eax;

        mov(ea_m_n(), $rn);
        mov(ea_mv_value(4), $r4);
        mov(ea_mv_value(3), $r3);
        mov(ea_mv_value(2), $r2);
        mov(ea_mv_value(1), $r1);
        mov(ea_mv_value(0), $r0);

        // How many number of rest parameters do we get?
        mov(ecx, ea_m_fn());
        mov(eax, ea_m_n());
        mov(esi, eax);
        sub(eax,  ecx);
        test(eax, eax);
        jle(rest_0);

        // pThread->m_rgcbObject[T_Cons] += cElts * sizeof(SCons)
        add(eax, eax);
        add(ea_mv_size(Cons::Tag2), eax);

        // pArea = pThread->m_pConsArea
        mov(ebx, ea($tcb, offsetof(Thread, m_pConsArea)));

     label(cont);
        // edx <- pArea->m_ofsFree
        // edi <- pArea->m_ofsFree + cbElts
        mov(edx, ea(ebx, offsetof(TinyCl::Mm::Area, m_ofsFree)));
        lea(edi, ea(edx, 0, eax));

        // pArea + pArea->m_ofsFree + cbElts > pArea->m_cbArea
        cmp(edi, ea(ebx, offsetof(TinyCl::Mm::Area, m_cbArea)));
        ja(alloc_cons_area);

        mov(ea(ebx, offsetof(TinyCl::Mm::Area, m_ofsFree)), edi);
        add(edi, ebx);
        add(edx, ebx);

        lea(esi, ea($tcb, offsetof(Thread, mv_value), esi));
        mov(eax, nil);

     label(loop);
        // eax <- last cons
        // edx <- pArea + pArea->m_ofsFree
        // edi <- pArea + pArea->m_ofsFree + cbElts
        // esi <- &pThread->m_rgxValue[i]
        sub(esi, sizeof(Val));
        sub(edi, sizeof(Cons));
        mov(ea(edi, offsetof(Cons, m_cdr)), eax);
        mov(eax, ea(esi));
        mov(ea(edi, offsetof(Cons, m_car)), eax);
        lea(eax, ea(edi, Cons::Tag2));
        cmp(edi, edx);
        jne(loop);

     label(exit);
        // eax -> list of rest parameters
        // ecx -> start position of rest parameter
        mov(ea($tcb, offsetof(Thread, mv_value), ecx), eax);
        mov($r0, ea_mv_value(0));
        mov($r1, ea_mv_value(1));
        mov($r3, ea_mv_value(3));
        mov($r2, ea_mv_value(2));
        mov($r4, ea_mv_value(4));
        mov($rn, ea_m_n());
        ret();

    label(rest_0);
        mov(eax, nil);
        jmp(exit);

    label(alloc_cons_area);
        // eax <- netls * sizeof(SCons);
        // ecx <- start position
        // ebx -> pArea
        mov($r0, Fixnum::Encode(sizeof(Cons)));
        call(ea($tcb, SVC_alloc_cons_area));
        mov(ebx, eax);
        mov(ecx, ea_m_fn());
        mov(eax, ea_m_n());
        mov(esi, eax);
        sub(eax, ecx);;
        add(eax, eax);
        jmp(cont);
    EndAsm(Brestify)

    ////////////////////////////////////////////////////////////
    //
    //  !stack-restify
    //
    //  Arguments and Values:
    //    m_fn  <- start position of rest parameters.
    //    ecx   <- nparams (in fixnum)
    //    eax   -> mv_value[start]
    //    all registers are preserved.
    // 
    //  Restify Frame:
    //  (low)       +----------------+
    //      ESP  -> |    slot[0]     |
    //              +----------------+
    //                    ...
    //              +----------------+
    //              | slot[cSlots-1] |
    //              +----------------+
    //              |    RP  o-------+------+
    //              +----------------+      |
    //              |    rest[0]     |      |
    //              +----------------+      |
    //              |    next        |      |
    //              +----------------+      |
    //              |    rest[1]     |      |
    //              +----------------+      |
    //              |    next        |      |
    //              +----------------+      |
    //                    ...               |
    //              +----------------+      |
    //              | rest[cRests-1] |      |
    //              +----------------+      |
    //              |    nil         |      |
    //              +----------------+      |
    //              |    pad[0]      |      |
    //              +----------------+      |
    //              |  RA of caller  |  <---+
    //              +----------------+
    //  (high)
    DefAsm(Bstack_restify, ())
        Label save_reg_0;
        Label save_reg_1;
        Label save_reg_2;
        Label save_reg_3;
        Label save_reg_4;
        Label save_reg_5;

        Label restore_1;
        Label restore_2;
        Label restore_3;
        Label restore_4;
        Label restore_5;

        Label exit;
        Label exit_tbl;
        Label rest_0;
        Label loop;

        Label save_reg_tbl;

        mov(ea_m_n(), $rn);

        // Uncache register parameters.
        cmp($rn, Fixnum::Encode(5));
        ja(save_reg_5);
        jmp(ea($rn, save_reg_tbl));

      label(save_reg_5); mov(ea_mv_value(4), $r4);
      label(save_reg_4); mov(ea_mv_value(3), $r3);
      label(save_reg_3); mov(ea_mv_value(2), $r2);
      label(save_reg_2); mov(ea_mv_value(1), $r1);
      label(save_reg_1); mov(ea_mv_value(0), $r0);

      label(save_reg_0);

        // How many number of rest parameters do we get?
        mov(eax, ecx);
        mov(esi, ecx);
        mov(ecx, ea_m_fn());
        sub(eax, ecx);
        test(eax, eax);
        jle(rest_0);


        // number of arguments -> esi
        // esi <- &pThread->mv_value[n]
        lea(esi, ea($tcb, offsetof(Thread, mv_value), esi));

        add(eax, eax);

        // esp-> RA
        //       caller   <- esp
        pop(edx);           // edx <- RA
        mov(ebx, $sp);      // ebx <- esp

        // Align esp to 8
        mov(ea($sp, -4), eax);
        and($sp, ~7);

        // esp-> RA
        //       RP o---------+
        // edx-> rest[0]      |
        //       next[0]      |
        //       rest[1]      |
        //       next[1]      |
        //       ...          |
        //       rest[n-1]    |
        //       nil          |
        // edi-> pad[0]       |
        // ebx-> caller <-----+
        mov(edi, $sp);
        sub($sp, eax);
        mov(eax, $sp);
        push(ebx);  // RP
        push(edx);  // RA

        mov(edx, eax);
        mov(eax, nil);

        // From the last cons to the first cons.
      label(loop);
        sub(edi, sizeof(Cons));
        mov(ea(edi, offsetof(Cons, m_cdr)), eax);
        sub(esi, sizeof(Val));
        mov(eax, ea(esi));
        mov(ea(edi, offsetof(Cons, m_car)), eax);

        lea(eax, ea(edi, Cons::Tag2));

        cmp(edi, edx);
        jne(loop);

      label(exit);
        // eax <- list of rest parameters
        // ecx <- start position of rest parameter
        mov(ea($tcb, offsetof(Thread, mv_value), ecx), eax);

        mov($r0, ea_mv_value( 0));
        mov($r1, ea_mv_value( 1));
        mov($r2, ea_mv_value( 2));
        mov($r3, ea_mv_value( 3));
        mov($r4, ea_mv_value( 4));

        mov($rn, ea_m_n());
        ret();

      label(rest_0);
        //                    RA      <- esp
        // esp' -> RA         RP = esp+8
        //         caller     caller
        mov(eax, ea($sp));
        sub($sp, 4);
        mov(ea($sp), eax);
        lea(eax, ea($sp, 8));
        mov(ea($sp, 4), eax);
        mov(eax, nil);
        jmp(exit);

        align(4);
      label(save_reg_tbl);
        dd(save_reg_0);  // #x00
        dd(save_reg_1);  // #x04
        dd(save_reg_2);  // #x08
        dd(save_reg_3);  // #x0C
        dd(save_reg_4);  // #x10
        dd(save_reg_5);  // #x10
    EndAsm(Bstack_restify)

    DefAsm(Buninitialized_funcallable_instance, ())
        mov(ea_m_fn(), Qerror);
        mov($r0, Quninitialized_funcallable_instance);
        mov($rn, one);
        jmp(QBfuncall);
    EndAsm(Buninitialized_funcallable_instance)

    installStaticFunction__B_b();
    installStaticFunction__B_t();
    installStaticFunction__B_u();
    installStaticFunction__P();

    installStaticFunction_A();
    installStaticFunction_B();
    installStaticFunction_C();
    installStaticFunction_E();
    installStaticFunction_F();
    installStaticFunction_G();
    installStaticFunction_I();
    installStaticFunction_K();
    installStaticFunction_L();
    installStaticFunction_N();
    installStaticFunction_P();
    installStaticFunction_S();
    installStaticFunction_T();
    installStaticFunction_U();
    installStaticFunction_V();
    installStaticFunction_W();
} // InstallStaticFunctions

/// <summary>
///  Pass 2 installation of static functions.
///  <para>
//     Note: You can call functions created by InstallStaticFunctions and
///    Dll functions.
///  </para>
/// </summary>
void InstallStaticFunctions2()
{
    DefAsm(Bnot_function, ())
        mov($r2, $r0);
        mov($r0, Qnot_function);
        mov($r1, Kname);
        mov($rn, Fixnum::Encode(3));
        jmp(Qerror);
    EndAsm(Bnot_function)
} // InstallStaticFunctions2

/// <summary>
///   Install reader function of record object.
/// </summary>
/// <param name="name">A name of reader</param>
/// <param name="classd">A class-description</param>
/// <param name="ofs">An offset of slot to read</param>
Val InstallStaticReader(Val name, Val classd, size_t ofs)
{
    class MyAsm : protected X86Asm
    {
        public: Val Run(Val name, Val classd, size_t ofs)
        {
            Label not_type;

            m_name = name;
            emitPrologue(1, 1);

            lea($r1, ea($r0, -Arch::Tag_Record));
            test($r1, Arch::TagMask);   // CF=0
            jne(not_type);

            cmp(ea($r1), classd);
            jne(not_type);

            mov($r0, ea($r1, ofs));
            or($r1, $r1);
            ret();

          label(not_type);
            mov($r1, class_name(classd->StaticCast<ClassD>()->m_class));
            thread_svc(TypeError);

            emitEpilogue();
            return install();
        } // Run
    } oAsm;
    return oAsm.Run(name, classd, ofs);
} // InstallStaticReader

/// <summary>
///   Makes a new generic-function's descriminator.
/// </summary>
/// <param name="gf">A generic-function object</param>
/// <param name="iVals">Number of values of GF</param>
Val MakeDiscriminator(Val gf, int iVals)
{
    class MyAsm : protected X86Asm
    {
        public: Val Run(Val gf, int iVals)
        {
            GenericFunction* pGf = gf->StaticCast<GenericFunction>();

            m_name = list(Kdiscriminator, pGf->m_name);

            FunctionInformation* pInfo =
                pGf->m_function_information->
                    StaticCast<FunctionInformation>();

            const int iMin  = Fixnum::Decode_(pInfo->m_min_params);
            const int iMax  = Fixnum::Decode_(pInfo->m_max_params);
            const int iRest = pInfo->m_rest == nil ? 0 : 1;


            emitPrologue(iMin, iMax, iRest);

            sub($sp, sizeof(ToForeignFrame));
            mov(ea_m_n(), $rn);

            // [1] Save arguments to thread
            for (int i = 0; i < lengthof(k_rgeRegVal); i++)
            {
                if (! iRest && i == iMax) break;
                mov(ea_mv_value(i), k_rgeRegVal[i]);
            } // for i

            // [2] Setup foreign frame
            mov(ecx, ea_m_fp());
            mov(ea_($sp, ToForeignFrame, m_pOuter), ecx);
            xor(ecx, ecx);
            mov(ea_($sp, ToForeignFrame, m_eFrame), Frame::Type_ToForeign);
            mov(ea_($sp, ToForeignFrame, m_cbArgs), ecx);   // m_cbArgs=0

            // [3] Setup arguments
            mov(ecx, $tcb);
            mov(edx, gf);

            // [4] Call foreign function
            call(".", "discriminator");

            // [5] Pop ToForeign frame
            mov($rn, ea_($sp, ToForeignFrame, m_pOuter));
            mov(ea_m_fp(), $rn);

            // [6] Get values
            add($sp, sizeof(ToForeignFrame));
            emitValues(iVals);
            ret();

            emitEpilogue();
            return finish();
        } // Run
    } oAsm;
    return oAsm.Run(gf, iVals);
} // MakeDiscriminator

/// <summary>
///  Makes a new wrapper function of calling procedure in DLL.
///  <list>
///    <item><term>1</term>
///     <description>Setup foreign arguments</description>
///    </item>
///    <item><term>2</term>
///     <description>Setup "To Foregin" frame</description>
///    </item>
///    <item><term>3</term>
///     <description>Prepare for calling foreign function</description>
///    </item>
///    <item><term>4</term>
///     <description>Call foreign function</description>
///    </item>
///    <item><term>5</term>
///     <description>Pop foreign frame</description>
///    </item>
///    <item><term>6</term>
///     <description>Set return values</description>
///    </item>
///   </list>
/// </summary>
/// <param name="fname">A function-name</param>
/// <param name="iMin">A minimum number of parameters</param>
/// <param name="iMax">A maximum number of parameters</param>
/// <param name="iRest">1 if function takes rest parameter</param>
/// <param name="iVals">Number of values of function</param>
/// <param name="pszProc">A name of procedure</param>
/// <param name="pszLib">A pathname or filename of DLL</param>
Val MakeWrapper(
    Val         const fname,
    int         const iMin,
    int         const iMax,
    int         const iRest,
    int         const iVals,
    int         const fKernel,
    const char* const pszProc,
    const char* const pszLib )
{
    class MyAsm : protected X86Asm
    {
        public: Val Run(
            Val         const fname,
            int         const iMin,
            int         const iMax,
            int         const iRest,
            int         const iVals,
            int         const fKernel,
            const char* const pszProc,
            const char* const pszLib )
        {
            m_name = fname;

            // C's fastcall uses two registers, ECX and EDX
            // for arguments.
            const int cForeignRegArgs = 2;

            int cbArgs;

            if (iRest)
            {
                cbArgs = 0;
            }
            else if (iMin != iMax)
            {
                cbArgs = (iMax + 1 - cForeignRegArgs) * sizeof(Val);
                cbArgs = max(cbArgs, 0);
            }
            else
            {
                cbArgs = (iMax - cForeignRegArgs) * sizeof(Val);
                cbArgs = max(cbArgs, 0);
            }

            ASSERT(cbArgs >= 0);

            m_cbFrame = cbArgs + sizeof(ToForeignFrame);

            emitPrologue(iMin, iMax, iRest);

            sub($sp, m_cbFrame);

            // [1] Set foreign arguments
            if (iRest)
            {
                mov(ea_m_n(), $rn);
                for (int i = 0; i < lengthof(k_rgeRegVal); i++)
                {
                    mov(ea_mv_value(i), k_rgeRegVal[i]);
                } // for i
            }
            else
            {
                int i = cForeignRegArgs;

                if (iMin != iMax)
                {
                    // We pass number of arguments as the first argument.
                    i -= 1;
                }

                for (int ofsArg = 0; ofsArg < cbArgs; ofsArg += sizeof(Val))
                {
                    if (i < lengthof(k_rgeRegVal))
                    {
                        mov(ea($sp, ofsArg), k_rgeRegVal[i]);
                    }
                    else
                    {
                        mov($rn, ea_mv_value(i));
                        mov(ea($sp, ofsArg), $rn);
                    }
                    i++;
                } // for ofsArg
            } // if

            // [2] Setup foreign frame
            ASSERT(cForeignRegArgs < 3);

            mov($r3, ea_m_fp());

            if (0 == cbArgs)
            {
                mov(ea($sp, offsetof(ToForeignFrame, m_pOuter) + cbArgs),
                    $r3 );

                xor($r3, $r3);

                mov(ea($sp, offsetof(ToForeignFrame, m_eFrame) + cbArgs),
                    fKernel ? Frame::Type_ToKernel : Frame::Type_ToForeign );

                mov(ea_($sp, ToForeignFrame, m_cbArgs), $r3);
                mov(ea_m_fp(), $sp);
            }
            else
            {
                lea($r4, ea($sp, cbArgs));

                mov(ea($r4, 0), $r3);

                mov(ea($sp, offsetof(ToForeignFrame, m_eFrame) + cbArgs),
                    fKernel ? Frame::Type_ToKernel : Frame::Type_ToForeign );

                mov(ea($sp, offsetof(ToForeignFrame, m_cbArgs) + cbArgs),
                    cbArgs );

                mov(ea_m_fp(), $r4);
            } // if

            // [3] Prepare for calling foreign function
            if (iRest)
            {
                mov(ecx, $tcb);
            }
            else if (iMin != iMax)
            {
                // pass number of arguments as the first argument.
                mov(edx, $r0);
            }
            else
            {
                ASSERT(edx == $r1);
                //if (iMax >= 2) mov(edx, $r1);
                if (iMax >= 1) mov(ecx, $r0);
            } // if

            // [4] Call foreign function
            call(NULL == pszLib ? "." : pszLib, pszProc);

            // [5] Pop foreign frame
            mov(edx, ea($sp, 0));
            mov(ea_m_fp(), edx);
            add($sp, m_cbFrame - cbArgs);

            // [6] Set return values
            emitValues(iVals);
            ret();

            emitEpilogue();
            return finish();
        } // Run
    } oAsm;
    return oAsm.Run(
        fname,
        iMin,
        iMax,
        iRest,
        iVals,
        fKernel,
        pszProc,
        pszLib );
} // MakeWrapper

defun(compute_discriminator, (Val gf))
{
    return MakeDiscriminator(gf, FunRet_Arbitrary);
} // compute_discriminator

} // TinyCl
