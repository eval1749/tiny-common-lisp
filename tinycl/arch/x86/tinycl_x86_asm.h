// -*- Mode: C++; -*-
//
// TinyCl - Definitions for X86 Assembler
// tinycl/arch/x86/tinycl_x86_asm.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_asm.h#20 $
//
#if !defined(INCLUDE_tinycl_x86_asm_h)
#define INCLUDE_tinycl_x86_asm_h

#include "./tinycl_x86.h"

namespace TinyCl
{

namespace X86
{

class Mm
{
    public: class Object
    {
        public: void operator delete(void*) {}

        public: void* operator new(size_t cb)
            { return sm_pMm->Alloc(cb); }

        public: void* operator new(size_t cb, Mm* pMm)
            { return pMm->Alloc(cb); }
    }; // Object

    private: static Mm* sm_pMm;

    private: HANDLE m_hHeap;

    protected: Mm() :
        m_hHeap(::HeapCreate(HEAP_NO_SERIALIZE, 0, 0))
    {
        sm_pMm = this;
    } // Mm

    public: ~Mm()
    {
        if (NULL != m_hHeap)
        {
            ::HeapDestroy(m_hHeap);
        }
    } // ~Mm

    // [A]
    public: void* Alloc(size_t cb)
        { return ::HeapAlloc(m_hHeap, HEAP_ZERO_MEMORY, cb); }
}; // Mm

class Asm : public Mm
{
    protected: typedef FunObj::Annot::Kind AnnotKind;

    protected: class Annot :
        public DoubleLinkedItem_<Annot>,
        public Mm::Object
    {
        public: AnnotKind   m_eAnnot;
        public: int         m_nAddr;
        public: int         m_ofs;
        public: Val         m_datum;

        public: Annot(AnnotKind e, int nAddr, int ofs, Val datum) :
            m_eAnnot(e), m_nAddr(nAddr), m_ofs(ofs), m_datum(datum) {}
    }; // Annot

    protected: class Label :
        public DoubleLinkedItem_<Label>
    {
        public: int m_nAddr;

        public: Label(int nAddr = 0) :
            m_nAddr(nAddr) {}
    }; // Label

    protected: class Span:
        public Castable_<Span>,
        public DoubleLinkedItem_<Span>,
        public Mm::Object,
        public WorkListItem_<Span>
    {
        public: int m_nAddr;
        public: int m_ofs;

        // [G]
        public: int                 GetAddr()   const { return m_nAddr; }
        public: int                 GetOffset() const { return m_ofs; }

        // [I]
        public: virtual bool IsCrossing(int nAddr) const
            { return m_nAddr >= nAddr; }
    }; // Span

#if 0
    protected: template<class T, class Base_ = Span>
        class Span_ : public Base_
    {
        protected: typedef Span_<T, Base_> BaseSpan;

        // ctor
        public: Span_(int nAddr, int ofs) :
            Span(nAddr, ofs) {}

        // [G]
        public: override const char* GetKind() const
            { return T::Kind_(); }

        // [I]
        public: static bool Is_(const Span* p)
            { return p->GetKind() == T::Kind_(); }
    }; // Span_
#endif

    protected: class JumpSpan :
        public WithCastable_<JumpSpan, Span>
    {
        public: static const char* Kind_() { return "Jump"; }

        public: bool    m_fLongForm;
        public: uint    m_opLong;
        public: uint    m_opShort;
        public: Label*  m_pLabel;

        public: JumpSpan(
            int nAddr,
            int ofs,
            int opShort,
            int opLong,
            Label* pLabel) :
                m_fLongForm(false),
                m_opLong(opLong),
                m_opShort(opShort),
                m_pLabel(pLabel)
            {
                m_nAddr = nAddr;
                m_ofs   = ofs;
            } // JumpSpan

        // [G]
        public: int GetTarget() const
            { return m_pLabel->m_nAddr; }

        // [I]
        public: override bool IsCrossing(int nAddr) const
        {
            if (m_nAddr < nAddr)
            {
                //      JUMP L1
                //      -- nAddr --
                //  L1: ...
                return m_pLabel->m_nAddr >= nAddr;
            }
            else
            {
                // L1:  ...
                //      -- nAddr --
                //      JUMP L1
                return m_pLabel->m_nAddr < nAddr;
            }
        } // IsCrossing
    }; // JumpSpan

    protected: typedef DoubleLinkedList_<Annot> Annots;
    protected: typedef DoubleLinkedList_<Label> Labels;
    protected: typedef DoubleLinkedList_<Span>  Spans;

    protected: class CodeBuf
    {
        private: static const int m_nMaxCode = 512;

        public:  int    m_nAddr;
        public:  int    m_ofs;
        public:  Annots m_oAnnots;
        public:  Labels m_oLabels;
        public:  Spans  m_oSpans;
        private: uint8* m_prgbCode;
        private: uint8  m_rgbCode[m_nMaxCode];

        // ctor
        public: CodeBuf() :
            m_nAddr(0),
            m_ofs(0),
            m_prgbCode(m_rgbCode) {}

        // [E]
        public: void EmitU32(uint32 u32)
        {
            ASSERT(m_ofs + 4 <= m_nMaxCode);
            int ofs = m_ofs;
            m_ofs += 4;
            m_nAddr += 4;
            PatchU32(ofs, u32);
        } // EmitU8

        public: void EmitU8(uint8 u8)
        {
            ASSERT(m_ofs < m_nMaxCode);
            int ofs = m_ofs;
            m_ofs += 1;
            m_nAddr += 1;
            PatchU8(ofs, u8);
        } // EmitU8

        public: class EnumAnnot : public Annots::Enum
        {
            public: EnumAnnot(CodeBuf* p) :
                Annots::Enum(&p->m_oAnnots) {}
        }; // EnumAnnot

        public: class EnumLabel : public Labels::Enum
        {
            public: EnumLabel(CodeBuf* p) :
                Labels::Enum(&p->m_oLabels) {}
        }; // EnumLabel

        public: class EnumSpan : public Spans::Enum
        {
            public: EnumSpan(CodeBuf* p) :
                Spans::Enum(&p->m_oSpans) {}
        }; // EnumSpan

        // [G]
        public: int GetAddr() const
            { return m_nAddr; }

        public: uint8* GetCode(int ofs)
        {
            ASSERT(ofs <= m_ofs);
            return m_prgbCode + ofs;
        } // GetCode

        // [I]
        public: void Increase(int cb)
        { 
            m_nAddr += cb;
            ASSERT(m_nAddr <= m_ofs);
        } // Increase

        // [P]
        public: void PatchU8(int ofs, uint8 n)
        {
            ASSERT(ofs < m_ofs);
            m_prgbCode[ofs] = n;
        } // PatchU8

        public: void PatchS32(int ofs, int32 n)
        { 
            PatchU32(ofs, n);
        } // PatchS32

        public: void PatchU32(int ofs, uint32 n)
        {
            ASSERT(ofs + 4 <= m_ofs);

            m_prgbCode[ofs+0] = static_cast<uint8>((n >>  0) & 0xff);
            m_prgbCode[ofs+1] = static_cast<uint8>((n >>  8) & 0xff);
            m_prgbCode[ofs+2] = static_cast<uint8>((n >> 16) & 0xff);
            m_prgbCode[ofs+3] = static_cast<uint8>((n >> 24) & 0xff);
        } // PatchU8
    }; // CodeBuf

    struct Arity
    {
        int m_iMin;
        int m_iMax;
        int m_iRest;

        Arity() :
            m_iRest(0), m_iMin(0), m_iMax(0) {}
    }; // Arity

    private:   uint                 m_cbAnnots;
    protected: uint                 m_cbFrame;
    protected: FunObj::FrameKind    m_eFrame;
    protected: bool                 m_fCheckArity;
    protected: Arity                m_oArity;
    protected: CodeBuf              m_oCodeBuf;

    protected: Val  m_classd;
    protected: Val  m_name;

    // ctor
    protected: Asm(Val name) :
        m_cbAnnots(0),
        m_cbFrame(0),
        m_eFrame(FunObj::FrameKind_Fixed),
        m_fCheckArity(true),
        m_classd(CLASSD_native_code_function),
        m_name(name) {}

    // [E]
    private: void embedAnnots(FunObj*);

    protected: void emitAnnot(AnnotKind eAnnot, Val datum)
    {
        Annot* pAnnot = new(this) Annot(
            eAnnot,
            m_oCodeBuf.m_nAddr,
            m_oCodeBuf.m_ofs,
            datum );

        m_oCodeBuf.m_oAnnots.Append(pAnnot);
        m_oCodeBuf.EmitU32(0);
        m_cbAnnots += 4;
    } // emitAnnot

    protected: void emitU32(uint32 u32)
        { m_oCodeBuf.EmitU32(u32); }

    protected: void emitU8(uint8 u8)
        { m_oCodeBuf.EmitU8(u8); }

    // [F]
    protected: Val finish();
    private:   void fixSpans();

    // [I]
    protected: Val install();
    protected: static bool isS8(Int i)
        { return i >= -128 && i <= 127; }

    // [M]
    protected: Val makeFunObj();

    // [S]
    private: void serializeAnnots(FunObj*);
    private: void serializeCode(FunObj*);
}; // Asm

const Val FUNOBJ_self = reinterpret_cast<Val>(FunObj::Tag);

class X86Asm : public Asm
{
    protected: enum Index
    {
        $r0_2   = $r0 | 0x100,
        $r1_2   = $r1 | 0x100,
        $r2_2   = $r2 | 0x100,
        $r3_2   = $r3 | 0x100,
        $r4_2   = $r4 | 0x100,

        $r0_4   = $r0 | 0x200,
        $r1_4   = $r1 | 0x200,
        $r2_4   = $r2 | 0x200,
        $r3_4   = $r3 | 0x200,
        $r4_4   = $r4 | 0x200,

        $r0_8   = $r0 | 0x300,
        $r1_8   = $r1 | 0x300,
        $r2_8   = $r2 | 0x300,
        $r3_8   = $r3 | 0x300,
        $r4_8   = $r4 | 0x300,
    }; // Index

    private: struct Ea
    {
        Mod     m_mod;
        Reg     m_rb;
        uint    m_sib;
        int     m_disp;
        Label*  m_pLabel;
    }; // Ea

    private: enum RegSize
    {
        RegSize_8   = 0x00,
        RegSize_16  = 0x08,
        RegSize_32  = 0x10,
    }; // RegSize

    private: int regSize(Reg rx)
        { return rx & 0xF8; }

    private: Ea     m_oEa;
    private: Label  m_oLabelArity;

    // ctor
    protected: X86Asm(Val name = nil) : Asm(name) {}

    #define define_arith(mp_name, mp_NAME) \
        protected: void mp_name (Ea* pEa, Reg rx) \
            { emitArith(pEa, rx, op_ ## mp_NAME ## _Ev_Gv); } \
        protected: void mp_name (Ea* pEa, Int iz) \
            { emitArith(pEa, opext_ ## mp_NAME ## _Ev_Iz, iz); } \
        protected: void mp_name (Ea* pEa, Val imm) \
        { \
            emitOp(op_ ## mp_NAME ## _Ev_Iz); \
            emitEa(pEa, opext_ ## mp_NAME ## _Ev_Iz); \
            emitIz(imm); \
        } \
        protected: void mp_name (Reg rx, Reg ry) \
        { \
            ASSERT(regSize(rx) == RegSize_32); \
            ASSERT(regSize(ry) == RegSize_32); \
            emitOp(op_ ## mp_NAME ## _Gv_Ev); \
            emitU8(ModRm(Mod_Reg, rx, ry)); \
        } \
        protected: void mp_name (Reg rx, Int iz) \
            { emitArith(opext_ ## mp_NAME ## _Ev_Iz, rx, iz); } \
        protected: void mp_name (Reg rx, Val iz) \
            { emitArith(opext_ ## mp_NAME ## _Ev_Iz, rx, iz); } \
        protected: void mp_name (Reg rd, Ea* pEa) \
            { emitArith(pEa, rd, op_ ## mp_NAME ## _Gv_Ev); } \

    #define define_cmov(mp_t, mp_T) \
        protected: void cmov ## mp_t(Reg rd, Reg rx) \
            { cmovcc(tttn_ ## mp_T, rd, rx); }


    // [A]
    define_arith(adc, ADC)
    define_arith(add, ADD)
    define_arith(and, AND)

    // FIXME 2007-12-02 yosi@msn.com We should use AlignSpan for pseudo
    // asm instruction.
    protected: void align(int n)
    {
        while (0 != m_oCodeBuf.GetAddr() % n)
        {
            emitOp(op_NOP);
        } // while
    } // align

    // [C]
    protected: void call(const char* pszLib, const char* pszProc)
    {
        Val entry = intern_dll_entry(
            make_string(pszLib),
            make_string(pszProc) );

        emitOp(op_CALL_Ev);
        emitU8(ModRm(Mod_Disp0, opext_CALL_Ev, Rm_Disp32));
        emitAnnot(FunObj::Annot::Kind_DllLink, entry);
    } // call

    protected: void call(const Ea* pEa)
    {
        emitOp(op_CALL_Ev);
        emitEa(pEa, opext_CALL_Ev);
    } // call

    protected: void call(Val fname)
    {
        Val fn;
        
        if (SetfCell* p = fname->DynamicCast<SetfCell>())
        {
            fn = p->m_function;
        }
        else
        {
            ASSERT(fboundp(fname));
            emitOp(op_CALL_Jv);
            fn = fdefinition(fname);
        } // if
        emitAnnot(FunObj::Annot::Kind_NamedCallee, fn);
    } // call

    protected: void clc() { emitOp(op_CLC); }

    protected: void cmovcc(Tttn tttn, Reg rd, Reg rx)
    {
        ASSERT(regSize(rd) == RegSize_32);
        emitOp(op_CMOVcc_Gv_Ev + tttn);
        emitU8(ModRm(Mod_Reg, rd, rx));
    } // cmovcc

    define_cmov(a,  A)
    define_cmov(e,  E)
    define_cmov(ne, NE)
    define_cmov(g,  G)
    define_cmov(ge, GE)
    define_cmov(l,  L)
    define_cmov(le, LE)
    define_cmov(nc, NC)

    define_arith(cmp, CMP)

    // [D]
    protected: void dd(Label& rL)
    {
        emitAnnot(FunObj::Annot::Kind_AbsLabel, Fixnum::Encode(&rL));
    } // dd

    // [E]

    //  [00 reg/opext r/m]             disp == 0, r/m != EBP
    //  [00 reg/opext r/m] 24          disp == 0, r/m == EBP
    //  [01 reg/opext r/m] disp8       r/m != ESP
    //  [01 reg/opext r/m] 24 disp8    r/m == ESP
    //  [10 reg/opext r/m] disp32      r/m != ESP
    //  [10 reg/opext r/m] 24 disp32   r/m == ESP
    protected: Ea* ea(Reg rb, int disp = 0)
    {
        ASSERT(regSize(rb) == RegSize_32);

        if (0 == disp && Rm_Disp32 != (rb & 7))
        {
            m_oEa.m_mod = Mod_Disp0;
        }
        else if (isS8(disp))
        {
            m_oEa.m_mod  = Mod_Disp8;
        }
        else
        {
            m_oEa.m_mod = Mod_Disp32;
        }

        if (Rm_Sib == (rb & 7))
        {
            m_oEa.m_sib = 0x24;
        }

        m_oEa.m_rb     = rb;
        m_oEa.m_disp   = disp;
        m_oEa.m_pLabel = NULL;

        return &m_oEa;
    } // ea

    protected: Ea* ea(Reg rb, Label& rL)
    {
        ASSERT(regSize(rb) == RegSize_32);
        m_oEa.m_mod    = Mod_Disp32;
        m_oEa.m_rb     = rb;
        m_oEa.m_pLabel = &rL;
        return &m_oEa;
    } // ea

    protected: Ea* ea(Reg rb, int disp, Reg rx)
    {
        ASSERT(regSize(rb) == RegSize_32);
        ea(static_cast<Reg>(Rm_Sib | RegSize_32), disp);
        m_oEa.m_sib = Scale_1 | ((rx & 7) << 3) | (rb & 7);
        return &m_oEa;
    } // ea

    protected: Ea* ea(int disp, Index rx)
    {
        m_oEa.m_mod  = Mod_Disp0;
        m_oEa.m_disp = disp;
        m_oEa.m_rb   = static_cast<Reg>(Rm_Sib);
        m_oEa.m_sib  = ((rx >> 2) & 0xc0) | ((rx & 7) << 3) | Rm_Disp32;
        return &m_oEa;
    } // ea

    protected: Ea* ea_mv_count(uint k)
        { return ea($tcb, offsetof(Thread, mv_count[k])); }

    protected: Ea* ea_m_fn()
        { return ea($tcb, offsetof(Thread, m_fn)); }

    protected: Ea* ea_m_fp()
        { return ea($tcb, offsetof(Thread, m_fp)); }

    protected: Ea* ea_m_n()
        { return ea($tcb, offsetof(Thread, m_n)); }

    protected: Ea* ea_mv_size(uint k)
        { return ea($tcb, offsetof(Thread, mv_size[k])); }

    protected: Ea* ea_mv_value(uint k)
        { return ea($tcb, offsetof(Thread, mv_value[k])); }

    protected: Ea* ea_mv_value(Reg rx)
        { return ea($tcb, offsetof(Thread, mv_value), rx); }

    private: void emitArith(const Ea* pEa, Opext opext, Int iz)
    {
        if (isS8(iz))
        {
            emitOp(op_ADD_Ev_Ib);
            emitEa(pEa, opext);
            emitU8(static_cast<uint8>(iz));
        }
        else
        {
            emitOp(op_ADD_Ev_Iz);
            emitEa(pEa, opext);
            emitU32(iz);
        }
    } // emitArith

    private: void emitArith(const Ea* pEa, Reg rx, uint opcode)
    {
        emitOp(opcode);
        emitEa(pEa, rx);
    } // emitArith

    private: void emitArith(const Ea* pEa, Opext opext, uint opcode)
    {
        emitOp(opcode);
        emitEa(pEa, opext);
    } // emitArith

    private: void emitArith(Opext opext, Reg rx, Val imm)
    {
        ASSERT(regSize(rx) == RegSize_32);

        Int iz = reinterpret_cast<Int>(imm);
        if (isS8(iz))
        {
            emitOp(op_ADD_Ev_Ib);
            emitU8(ModRm(Mod_Reg, opext, rx));
            emitU8(static_cast<uint8>(iz));
        }
        else if (rx == $EAX)
        {
            emitOp(op_ADD_eAX_Iz + opext * 8);
            emitIz(imm);
        }
        else
        {
            emitOp(op_ADD_Ev_Iz);
            emitU8(ModRm(Mod_Reg, opext, rx));
            emitIz(imm);
        }
    } // emitArith

    private: void emitArith(Opext opext, Reg rx, Int iz)
    {
        ASSERT(regSize(rx) == RegSize_32);

        if (isS8(iz))
        {
            emitOp(op_ADD_Ev_Ib);
            emitU8(ModRm(Mod_Reg, opext, rx));
            emitU8(static_cast<uint8>(iz));
        }
        else
        {
            emitOp(op_ADD_Ev_Iz);
            emitU8(ModRm(Mod_Reg, opext, rx));
            emitU32(iz);
        }
    } // emitArith

    private: void emitEa(const Ea* pEa, Opext opext)
        { emitEa(pEa, static_cast<Reg>(opext)); }

    private: void emitEa(const Ea* pEa, Reg rd)
    {
        emitU8(ModRm(pEa->m_mod, rd, pEa->m_rb));

        if (Mod_Reg == pEa->m_mod) return;

        if (Rm_Sib == (pEa->m_rb & 7))
        {
            emitU8(static_cast<uint8>(pEa->m_sib));
        }

        switch (pEa->m_mod)
        {
        case Mod_Disp0:
            if (static_cast<Reg>(Rm_Sib) == pEa->m_rb &&
                (pEa->m_sib & 7) == Rm_Disp32 )
            {
                goto emitDisp32;
            }
            break;

        case Mod_Disp8:
            emitU8(static_cast<uint8>(pEa->m_disp));
            break;

        case Mod_Disp32:
        emitDisp32:
            if (Label* pLabel = pEa->m_pLabel)
            {
                emitAnnot(
                    FunObj::Annot::Kind_AbsLabel,
                    Fixnum::Encode(pLabel) );
            }
            else
            {
                emitU32(static_cast<uint32>(pEa->m_disp));
            }
            break;
        } // switch mod
    } // emitEa

    protected: void emitEpilogue()
    {
        if (m_fCheckArity)
        {
            label(m_oLabelArity);
            emitOp(op_CALL_Ev);
            emitU8(ModRm(Mod_Disp8, opext_CALL_Ev, $tcb));
            emitU8(static_cast<uint8>(ThreadExtra_ArityError & 0xff));
        }
    } // emitEpilogue

    protected: void emitIz(Val iz)
    {
        emitAnnot(FunObj::Annot::Kind_LispVal, iz);
    } // emitIz

    protected: void emitOp(uint n)
    {
        if (n > 0xffff)
        {
            emitU8(static_cast<uint8>((n >> 16) & 0xff));
        }

        if (n > 0xff)
        {
            emitU8(static_cast<uint8>((n >> 8) & 0xff));
        }
        
        emitU8(static_cast<uint8>(n & 0xff));
    } // emitOp

    protected: void emitPrologue()
    {
        m_fCheckArity = false;
    } // emitPrologue

    protected: void emitPrologue(int iMin, int iMax, int iRest = 0);

    protected: void emitValues(int iVals)
    {
        switch (iVals)
        {
        case 0:
            xor($rn, $rn);
            stc();
            break;

        case 1:
            // CF must be zero.
            break;

        case FunRet_Arbitrary:
            mov($rn, ea_m_n());
            for (int i = 1; i < lengthof(k_rgeRegVal); i++)
            {
                mov(k_rgeRegVal[i],  ea_mv_value(i));
            } // for i
            stc();
            break;

        case FunRet_Bool:
            test($AL, $AL); // CF=0
            mov($r0, nil);
            mov($r1, t);
            cmovne($r0, $r1);
            break;

        case FunRet_NoReturn:
            break;

        default:
            for (int i = 1; i < lengthof(k_rgeRegVal); i++)
            {
                if (i == iVals) break;
                mov(k_rgeRegVal[i],  ea_mv_value(i));
            } // for i

            if (FunRet_Arbitrary == iVals)
            {
                mov($rn, ea_m_n());
            }
            else
            {
                mov($rn, Fixnum::Encode(iVals));
            }
            stc();
            break;
        } // switch iVals
    } // emitValues

    // [F]
    protected: void frame(FunObj::FrameKind eFrame, int cbFrame = 0)
        { m_eFrame = eFrame; m_cbFrame = cbFrame; }

    // [J]
    protected: void jcc(Tttn tttn, Label& rL)
    {
        emitOp(op_Jcc_Jb + tttn);
        emitU8(0);

        // Reserve space for long form
        m_oCodeBuf.m_ofs += 4;

        JumpSpan* pJumpS = new(this) JumpSpan(
            m_oCodeBuf.m_nAddr,
            m_oCodeBuf.m_ofs,
            op_Jcc_Jb + tttn,
            op_Jcc_Jv + tttn,
            &rL );

        m_oCodeBuf.m_oSpans.Append(pJumpS);
    } // jcc

    protected: void ja(Label& rL)  { jcc(tttn_A,  rL); }
    protected: void jae(Label& rL) { jcc(tttn_AE, rL); }
    protected: void je(Label& rL)  { jcc(tttn_E,  rL); }
    protected: void jg(Label& rL)  { jcc(tttn_G,  rL); }
    protected: void jge(Label& rL) { jcc(tttn_GE, rL); }
    protected: void jl(Label& rL)  { jcc(tttn_L,  rL); }
    protected: void jle(Label& rL) { jcc(tttn_LE, rL); }
    protected: void jne(Label& rL) { jcc(tttn_NE, rL); }

    protected: void jmp(const Ea* pEa)
    {
        emitOp(op_JMP_Ev);
        emitEa(pEa, opext_JMP_Ev);
    } // jmp

    protected: void jmp(Label& rL)
    {
        emitOp(op_JMP_Jb);
        emitU8(0);

        // Reserve space for long form
        m_oCodeBuf.m_ofs += 3;

        JumpSpan* pJumpS = new(this) JumpSpan(
            m_oCodeBuf.m_nAddr,
            m_oCodeBuf.m_ofs,
            op_JMP_Jb,
            op_JMP_Jv,
            &rL );

        m_oCodeBuf.m_oSpans.Append(pJumpS);
    } // jmp

    protected: void jmp(Val fname)
    {
        Val fn;
        
        if (SetfCell* p = fname->DynamicCast<SetfCell>())
        {
            fn = p->m_function;
        }
        else
        {
            ASSERT(fboundp(fname));
            emitOp(op_JMP_Jv);
            fn = fdefinition(fname);
        } // if
        emitAnnot(FunObj::Annot::Kind_NamedCallee, fn);
    } // jmp

    // [L]
    protected: void label(Label& rL)
    {
        m_oCodeBuf.m_oLabels.Append(&rL);
        rL.m_nAddr = m_oCodeBuf.m_nAddr;
    } // label

    protected: void lea(Reg rd, const Ea* pEa)
    {
        ASSERT(regSize(rd) == RegSize_32);
        emitOp(op_LEA_Gv_M);
        emitEa(pEa, rd);
    } // lea

    // [M]
    protected: void mov(const Ea* pEa, Reg rx)
    {
        emitOp(op_MOV_Ev_Gv);
        emitEa(pEa, rx);
    } // mov

    protected: void mov(const Ea* pEa, Int iz)
    {
        emitOp(op_MOV_Ev_Iz);
        emitEa(pEa, opext_MOV_Ev_Iz);
        emitU32(static_cast<uint32>(iz));
    } // mov

    protected: void mov(const Ea* pEa, Label& rL)
    {
        emitOp(op_MOV_Ev_Iz);
        emitEa(pEa, opext_MOV_Ev_Iz);
        emitAnnot(FunObj::Annot::Kind_RelLabel, Fixnum::Encode(&rL));
    } // mov

    protected: void mov(const Ea* pEa, Val imm)
    {
        emitOp(op_MOV_Ev_Iz);
        emitEa(pEa, opext_MOV_Ev_Iz);
        emitIz(imm);
    } // mov

    protected: void mov(Reg rd, const Ea* pEa)
    {
        ASSERT(regSize(rd) == RegSize_32);
        emitOp(op_MOV_Gv_Ev);
        emitEa(pEa, rd);
    } // mov

    protected: void mov(Reg rd, Reg rx)
    {
        ASSERT(regSize(rd) == RegSize_32);
        ASSERT(regSize(rx) == RegSize_32);
        emitOp(op_MOV_Gv_Ev);
        emitU8(ModRm(Mod_Reg, rd, rx));
    } // mov

    protected: void mov(Reg rd, Val imm)
    {
        ASSERT(regSize(rd) == RegSize_32);
        emitOp(static_cast<Op>(op_MOV_eAX_Iv + (rd & 7)));
        emitIz(imm);
    } // mov

    protected: void movzxw(Reg rd, const Ea* const pEa)
    {
        emitOp(op_MOVZX_Gv_Ew);
        emitEa(pEa, rd);
    } // movzw

    // [N]
    protected: void not(Reg rd)
    {
        emitOp(op_NOT_Ev);
        emitU8(ModRm(Mod_Reg, opext_NOT_Ev, rd));
    } // not

    // [O]
    define_arith(or, OR)

    // [P]
    protected: void pop(const Ea* pEa)
        { emitOp(op_POP_Ev); emitEa(pEa, opext_POP_Ev); }

    protected: void pop(Reg rx)
    {
        ASSERT(regSize(rx) == RegSize_32);
        emitOp(op_POP_rAX + (rx & 7));
    } // pop

    protected: void push(const Ea* pEa)
        { emitOp(op_PUSH_Ev); emitEa(pEa, opext_PUSH_Ev); }

    protected: void push(Reg rx)
    {
        ASSERT(regSize(rx) == RegSize_32);
        emitOp(op_PUSH_rAX + (rx & 7));
    } // push

    // [R]
    protected: void ret() { emitOp(op_RET); }

    // [S]
    define_arith(sbb, SBB)

    protected: void emitShift(
        Op op_1, Opext opext_1,
        Op op_Ib, Opext opext_Ib,
        Reg rd, int n )
    {
        ASSERT(n >= 1 && n <= 31);
        if (1 == n)
        {
            emitOp(op_1);
            emitU8(ModRm(Mod_Reg, opext_1, rd));
        }
        else
        {
            emitOp(op_Ib);
            emitU8(ModRm(Mod_Reg, opext_Ib, rd));
            emitU8(static_cast<uint8>(n));
        }
    } // s__

    #define define_shift(mp_name, mp_NAME) \
        protected: void mp_name(Reg rd, int n) \
            { \
                ASSERT(regSize(rd) == RegSize_32); \
                emitShift(\
                    op_ ## mp_NAME ## _Ev_1, \
                    opext_ ## mp_NAME ## _Ev_1, \
                    op_ ## mp_NAME ## _Ev_Ib, \
                    opext_ ## mp_NAME ## _Ev_Ib, \
                    rd, n); \
            }

    define_shift(sar, SAR)
    define_shift(shl, SHL)
    define_shift(shr, SHR)
    protected: void stc() { emitOp(op_STC); }
    define_arith(sub, SUB)

    // [T]
    protected: void test(Ea* pEa, int32 iz)
    {
        emitOp(op_TEST_Ev_Iz);
        emitEa(pEa, opext_TEST_Ev_Iz);
        emitU32(iz);
    } // test

    protected: void test(Reg rx, int32 iz)
    {
        ASSERT(regSize(rx) == RegSize_32);
        if (0 == (rx & 7))
        {
            if (iz >= 0 && iz <= 255)
            {
                emitOp(op_TEST_AL_Ib);
                emitU8(static_cast<uint8>(iz));
            }
            else
            {
                emitOp(op_TEST_eAX_Iz);
                emitU32(iz);
            }
        }
        else if (iz >= 0 && iz <= 255 && (rx & 7) <= 3)
        {
            emitOp(op_TEST_Eb_Ib);
            emitU8(ModRm(Mod_Reg, opext_TEST_Eb_Ib, rx));
            emitU8(static_cast<uint8>(iz));
        }
        else
        {
            emitOp(op_TEST_Ev_Iz);
            emitU8(ModRm(Mod_Reg, opext_TEST_Ev_Iz, rx));
            emitU32(iz);
        }
    } // test

    protected: void test(Reg rx, Reg ry)
    {
        if (regSize(rx) == RegSize_8)
        {
            ASSERT(regSize(rx) == RegSize_8);
            emitOp(op_TEST_Eb_Gb);
        }
        else if (regSize(rx) == RegSize_16)
        {
            ASSERT(regSize(rx) == RegSize_16);
            emitOp(op_OPDSIZ);
            emitOp(op_TEST_Ev_Gv);
        }
        else
        {
            ASSERT(regSize(rx) == RegSize_32);
            ASSERT(regSize(ry) == RegSize_32);
            emitOp(op_TEST_Ev_Gv);
        }

        emitU8(ModRm(Mod_Reg, rx, ry));
    } // test

    // [X]
    define_arith(xor, XOR);

    ////////////////////////////////////////////////////////////
    //
    // SSE
    //
    protected: enum XmmReg
    {
        xmm0 = $XMM0, xmm1 = $XMM1, xmm2 = $XMM2, xmm3 = $XMM3,
        xmm4 = $XMM4, xmm5 = $XMM5, xmm6 = $XMM6, xmm7 = $XMM7,
    }; // XmmReg

    private: uint XModRm(Mod eMod, XmmReg x1, XmmReg x2)
        { return ModRm(eMod, static_cast<Reg>(x1), static_cast<Reg>(x2)); }

    private: uint XModRm(Mod eMod, Opext opext, XmmReg x2)
        { return ModRm(eMod, opext, static_cast<Reg>(x2)); }

    #define define_sse_mov(mp_name, mp_NAME, mp_1, mp_2) \
        protected: void mp_name(XmmReg x1, XmmReg x2) \
        { \
            emitOp(op_ ## mp_NAME ## _ ## mp_1 ## _ ## mp_2); \
            emitU8(static_cast<uint8>(XModRm(Mod_Reg, x1, x2))); \
        } \
        protected: void mp_name(XmmReg x1, Ea* ea) \
        { \
            emitOp(op_ ## mp_NAME ## _ ## mp_1 ## _ ## mp_2); \
            emitEa(ea, static_cast<Reg>(x1)); \
        } \
        protected: void mp_name(Ea* ea, XmmReg x2) \
        { \
            emitOp(op_ ## mp_NAME ## _ ## mp_2 ## _ ## mp_1); \
            emitEa(ea, static_cast<Reg>(x2)); \
        }

    define_sse_mov(movsd, MOVSD, Vsd, Wsd)
    define_sse_mov(movss, MOVSS, Vss, Wss)
}; // X86Asm

#define DefAsm(mp_name, mp_arity) \
    { \
        class Asm__ ## mp_name : public X86Asm \
        { \
            private: Asm__ ## mp_name () : \
                X86Asm(Q ## mp_name) {} \
            public: static void Run() \
            { \
                Asm__ ## mp_name oAsm; \
                oAsm.build(); \
            } \
            private: void build() \
            { \
                emitPrologue mp_arity;

#define EndAsm(mp_name) \
                emitEpilogue(); \
                install(); \
            } \
        }; \
        Asm__ ## mp_name::Run(); \
    } // EndAsm

#define label_def(mp_name) Label mp_name; label(mp_name)

#define OffsetOf(mp_class, mp_field) \
    offsetof(mp_class, mp_field) - mp_class::Tag

#define ea_(mp_reg, mp_class, mp_field) \
    ea(mp_reg, offsetof(mp_class, mp_field))

#define thread_svc(mp_field) \
    call(ea($tcb, ThreadExtra_(mp_field)))


#define eax  $EAX
#define ebp  $EBP
#define ebx  $EBX
#define ecx  $ECX
#define edx  $EDX
#define edi  $EDI
#define esi  $ESI
#define esp  $ESP

} // X86
} // TinyCl

#endif //!defined(INCLUDE_tinycl_x86_asm_h)
