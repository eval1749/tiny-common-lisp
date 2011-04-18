#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - x86 Disassembler
// tinycl_x86_decode.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_disasm.cpp#22 $
//
#include "./tinycl_x86.h"

#include "./tinycl_x86_ke_gcmap.h"
#include "../../tinycl_dll_link.h"

namespace TinyCl
{

#define K_(x) intern(make_string(x), PKG_keyword)

namespace
{

using namespace Private;

typedef X86::Mod    Mod;
typedef X86::Reg    Reg;
typedef X86::Rm     Rm;
typedef X86::Scale  Scale;

enum Extend
{
    Extend_ModRm,
    Extend_None,
    Extend_TwoByte,
}; // Extend

enum OpdFmt
{
    OpdFmt_None    = 0,

    OpdFmt_1,  // SHL Ev, 1

    OpdFmt_Ib, // imm8
    OpdFmt_Iw, // imm16
    OpdFmt_Iq, // imm64
    OpdFmt_Is, // imm8 with sign extend
    OpdFmt_Iv, // imm16, imm32 or imm64
    OpdFmt_Iz, // imm32

    OpdFmt_Jb,
    OpdFmt_Jv,

    OpdFmt_Eb,
    OpdFmt_Ew,
    OpdFmt_Ed,
    OpdFmt_Eq,
    OpdFmt_Ev,

    OpdFmt_Gb,
    OpdFmt_Gd,
    OpdFmt_Gq,
    OpdFmt_Gv,

    OpdFmt_Ob,
    OpdFmt_Ov,

    OpdFmt_M,
    OpdFmt_Md,
    OpdFmt_Mp,
    OpdFmt_Mpd,
    OpdFmt_Mps,
    OpdFmt_Mq,

    OpdFmt_Nq,     // mm

    OpdFmt_Pd,     // mm
    OpdFmt_Pq,     // mm

    OpdFmt_Qd,     // mm or Mq
    OpdFmt_Qq,     // mm or Mq
    OpdFmt_Qdq,

    OpdFmt_Udq,    // ModRm:rm = xmm
    OpdFmt_Upd,    // ModRm:rm = xmm
    OpdFmt_Ups,    // ModRm:rm = xmm

    OpdFmt_Vdq,    // ModRm:reg = xmm
    OpdFmt_Vpd,    // ModRm:reg = xmm
    OpdFmt_Vpq,    // ModRm:reg = xmm
    OpdFmt_Vps,    // ModRm:reg = xmm
    OpdFmt_Vq,     // ModRm:reg = xmm
    OpdFmt_Vsd,    // ModRm:reg = xmm
    OpdFmt_Vss,    // ModRm:reg = xmm

    OpdFmt_Wdq,    // xmm or Mdq
    OpdFmt_Wpd,    // xmm or Mdq
    OpdFmt_Wps,    // xmm or Mdq
    OpdFmt_Wq,     // xmm or Mdq
    OpdFmt_Wsd,    // xmm or Mdq
    OpdFmt_Wss,    // xmm or Mdq

    OpdFmt_eAX,    // 0
    OpdFmt_eCX,    // 1
    OpdFmt_eDX,    // 2
    OpdFmt_eBX,    // 3
    OpdFmt_eSP,    // 4
    OpdFmt_eBP,    // 5
    OpdFmt_eSI,    // 6
    OpdFmt_eDI,    // 7

    OpdFmt_rAX,    // 0    PUSH/POP
    OpdFmt_rCX,    // 1    PUSH/POP
    OpdFmt_rDX,    // 2    PUSH/POP
    OpdFmt_rBX,    // 3    PUSH/POP
    OpdFmt_rSP,    // 4    PUSH/POP
    OpdFmt_rBP,    // 5    PUSH/POP
    OpdFmt_rSI,    // 6    PUSH/POP
    OpdFmt_rDI,    // 7    PUSH/POP

    OpdFmt_AL,     // 0
    OpdFmt_CL,     // 1
    OpdFmt_DL,     // 2
    OpdFmt_BL,     // 3
    OpdFmt_AH,     // 4
    OpdFmt_CH,     // 5
    OpdFmt_DH,     // 6
    OpdFmt_BH,     // 7

    OpdFmt_DX, // For IN, OUT

    OpdFmt_Sw, // reg of modrm selects segment register

    OpdFmt_EvF64,  // CALL/JMP Ev
    OpdFmt_EvD64,  // PUSH Ev

    OpdFmt_CS,
    OpdFmt_DS,
    OpdFmt_ES,
    OpdFmt_SS,
}; // Oeprand

enum RegClass
{
    RegClass_Gpr,
    RegClass_Mmx,
    RegClass_Xmm,
}; // RegClass

struct Format
{
    uint        m_cOperands;
    Extend      m_eExtend;
    uint        m_opcode;
    const char* m_psz;
    OpdFmt      m_rgeOpdFmt[3];
}; // Format

static const Format k_rgoFormat[] =
{
    #define DEFFORMAT_0(mp_o, mp_m) \
        { 0, Extend_None, mp_o, # mp_m },

    #define DEFFORMAT_1(mp_o, mp_m, mp_1) \
        { 1, Extend_None, mp_o, # mp_m, OpdFmt_ ## mp_1 },

    #define DEFFORMAT_2(mp_o, mp_m, mp_1, mp_2) \
        { 2, Extend_None, mp_o, # mp_m, OpdFmt_ ## mp_1, OpdFmt_ ## mp_2 },

    #define DEFFORMAT_3(mp_o, mp_m, mp_1, mp_2, mp_3) \
        { 3, Extend_None, mp_o, # mp_m, \
          OpdFmt_ ## mp_1, OpdFmt_ ## mp_2, OpdFmt_ ## mp_3 },

    #define DEFFORMAT_EXT_1(mp_o, mp_e, mp_m, mp_1) \
        { 1, Extend_None, (mp_o << 8) | mp_e, # mp_m, OpdFmt_ ## mp_1 },

    #define DEFFORMAT_EXT_2(mp_o, mp_e, mp_m, mp_1, mp_2) \
        { 2, Extend_None, (mp_o << 8) | mp_e, # mp_m, \
          OpdFmt_ ## mp_1, OpdFmt_ ## mp_2 },

    #define DEFFORMAT_EXT_3(mp_o, mp_e, mp_m, mp_1, mp_2, mp_3) \
        { 3, Extend_None, (mp_o << 8) | mp_e, # mp_m, \
          OpdFmt_ ## mp_1, OpdFmt_ ## mp_2, OpdFmt_ ## mp_3 },

    #define DEFFORMAT_X(mp_opcode, mp_extend) \
        { 0, Extend_ ## mp_extend, mp_opcode, NULL },

    #include "./tinycl_x86_opcode.inc"
}; // k_rgoFormat

const char* const k_rgpszGpr8[8] =
    { "AL", "CL", "DL", "BL", "AH", "CH", "DH", "BH" };

const char* const k_rgpszGpr16[8] =
    { "AX", "CX", "DX", "BX", "SP", "BP", "SI", "DI" };

const char* const k_rgpszGpr32[8] =
    { "EAX", "ECX", "EDX", "EBX", "ESP", "EBP", "ESI", "EDI" };

const char* const k_rgpszMmx[8] =
    { "MM0", "MM1", "MM2", "MM3", "MM4", "MM5", "MM6", "MM7" };

const char* const k_rgpszXmm[8] =
    { "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7" };

const char* const k_rgpszGcDesc[4] =
    { "Jump", "Call", "Call2", "Call3" };

// See tinycl_x86_c_isa.inc for GC eligible register list
const char* const k_rgpszGcReg[6] =
    { "EAX", "EDX", "EBX", "ESI", "EDI", "ECX" };

static int ldb(int iSize, int iPos, int iVal)
{
    return (iVal >> iPos) & ((1 << iSize) - 1);
} // ldb

//////////////////////////////////////////////////////////////////////
//
// Disasm
//
class Disasm
{
    private: struct Context
    {
        uint    m_nAdrSiz;
        uint    m_nModRm;
        uint    m_nOpdSiz;

        Val     m_prefixes;
        Val     mv_operand[3];

        Context() { Reset(); }

        void Reset()
        {
            m_nAdrSiz = 32;
            m_nModRm  = 0x100;
            m_nOpdSiz = 32;
            m_prefixes = nil;
        } // Reset
    }; // Context

    class FormatTab
    {
        private: struct Slot
        {
            const Format*   m_pFormat;
        }; //Slot

        private: int    m_cEntries;
        private: Slot   m_rgoSlot[1033];

        public: FormatTab() : m_cEntries(0) {}

        public: const Format* Get(uint opcode)
        {
            if (0 == m_cEntries)
            {
                populate();
            }

            const Slot* pTop    = m_rgoSlot;
            const Slot* pBtm    = m_rgoSlot + lengthof(m_rgoSlot);
            const Slot* pStart  = pTop + opcode % (pBtm - pTop);
            const Slot* pRunner = pStart;

            for (;;)
            {
                if (NULL == pRunner->m_pFormat)
                {
                    return NULL;
                }

                if (pRunner->m_pFormat->m_opcode == opcode)
                {
                    return pRunner->m_pFormat;
                }

                pRunner++;
                if (pBtm == pRunner) pRunner = pTop;
            } // for
        } // Get

        protected: void populate()
        {
            for (
                const Format* p = k_rgoFormat;
                p < k_rgoFormat + lengthof(k_rgoFormat);
                p++ )
            {
                put(p);
            } // for p
        } // populate

        protected: void put(const Format* pFormat)
        {
            Slot* pTop    = m_rgoSlot;
            Slot* pBtm    = m_rgoSlot + lengthof(m_rgoSlot);
            Slot* pStart  = pTop + pFormat->m_opcode % (pBtm - pTop);
            Slot* pRunner = pStart;

            for (;;)
            {
                if (NULL == pRunner->m_pFormat)
                {
                    pRunner->m_pFormat = pFormat;
                    return;
                }

                if (pRunner->m_pFormat->m_opcode == pFormat->m_opcode)
                {
                    return;
                }

                pRunner++;
                if (pBtm == pRunner) pRunner = pTop;
            } // for
        } // put
    }; // FormatTab

    private: static Format      sm_oFormat_DB;
    private: static Format      sm_oFormat_DD;
    private: static FormatTab   sm_oFormatTab;

    private: Context            m_oContext;
    private: FunObj::EnumAnnot  m_oEnumAnnot;
    private: GcMap::Enum        m_oEnumGcDesc;

    private: const uint8*       m_pbEnd;
    private: const uint8*       m_pbRunner;
    private: const uint8*       m_pbStart;
    private: const FunObj*      m_pFunObj;

    private: Val                m_labels;

    // ctor
    private: Disasm(const FunObj* pFunObj, Val labels = nil) :
        m_labels(labels),
        m_oEnumAnnot(pFunObj),
        m_oEnumGcDesc(pFunObj->GetGcMap(), pFunObj->GetCodeSize()),
        m_pbEnd(pFunObj->GetCodeStart() + pFunObj->GetCodeSize()),
        m_pbRunner(pFunObj->GetCodeStart()),
        m_pbStart(pFunObj->GetCodeStart()),
        m_pFunObj(pFunObj) {}

    // Entry Point
    public: static void Run(Val fn)
    {
        Val labels;
        {
            Disasm oDisasm(fn->StaticCast<FunObj>());
            labels = oDisasm.computeLabels();
        }

        Disasm oDisasm(fn->StaticCast<FunObj>(), labels);
        oDisasm.run();
    } // Run

    // [A]
    private: void addLabel(Val addr)
    {
        if (lt(addr, 0)) return;
        if (m_pbStart + Fixnum::Decode_(addr) >= m_pbEnd) return;

        Val last = nil;
        Val runner = m_labels;
        while (nil != runner)
        {
            if (car(runner) == addr) return;
            if (gt(car(runner), addr)) break;

            last = runner;
            runner = cdr(runner);
        } // while

        if (nil == last)
        {
            m_labels = cons(addr, m_labels);
        }
        else
        {
            setf_cdr(
                cons(addr, cdr(last)),
                last );
        } // if
    } // addLabel

    // [C]
    private: Val computeLabels()
    {
        m_labels = nil;
        do
        {
            const Format* pFormat;
            m_oContext.mv_operand[0] = getAnnot();
            if (nil != m_oContext.mv_operand[0])
            {
                pFormat = &sm_oFormat_DD;
            }
            else
            {
                pFormat = getFormat(decodeOp());
                decodeOperands(pFormat);
            } // if

            for (uint i = 0; i < pFormat->m_cOperands; i++)
            {
                Val operand = m_oContext.mv_operand[i];
                if (consp(operand) && Qgo == car(operand))
                {
                    addLabel(cadr(operand));
                }
                else if (consp(operand) && K_("EA") == car(operand))
                {
                    foreach (List::Enum, oEnum, operand)
                    {
                        Val elt = oEnum.Get();
                        if (consp(elt) && Qgo == car(elt))
                        {
                            addLabel(cadr(elt));
                        }
                    } // for each elt
                }
            } // for i
        } while (m_pbRunner < m_pbEnd);

        return m_labels;
    } // computeLabels

    // [D]
    private: uint decodeOp()
    {
        m_oContext.Reset();

        for (;;)
        {
            uint opcode = readU8();
            switch (opcode)
            {
            case 0xF0:
                push(K_("LOCK"), m_oContext.m_prefixes);
                break;
            case 0xF2:
                push(K_("REPNE"), m_oContext.m_prefixes);
                break;
            case 0xF3:
                push(K_("REP"), m_oContext.m_prefixes);
                break;
            case 0x26:
                push(K_("ES"), m_oContext.m_prefixes);
                break;
            case 0x2E:
                push(K_("CS"), m_oContext.m_prefixes);
                break;
            case 0x36:
                push(K_("SS"), m_oContext.m_prefixes);
                break;
            case 0x3E:
                push(K_("DS"), m_oContext.m_prefixes);
                break;
            case 0x64:
                push(K_("FS"), m_oContext.m_prefixes);
                break;
            case 0x65:
                push(K_("GS"), m_oContext.m_prefixes);
                break;
            case 0x66:
                m_oContext.m_nOpdSiz = 16;
                break;
            case 0x67:
                m_oContext.m_nAdrSiz = 16;
                break;
            default:
                if (0x0F == opcode)
                {
                    if (16 == m_oContext.m_nOpdSiz)
                    {
                        opcode = readU8();
                        if (NULL != getFormat(opcode | 0x660F00))
                        {
                            m_oContext.m_nOpdSiz = 32;
                            opcode |= 0x660F00;
                        }
                        else
                        {
                            opcode |= 0x0F00;
                        }
                    }
                    else if (nil != memq(K_("REPNE"),
                                         m_oContext.m_prefixes ) )
                    {
                        m_oContext.m_prefixes = delq(
                            K_("REPNE"),
                            m_oContext.m_prefixes );

                        opcode = readU8() | 0xF20F00;
                    }
                    else if (nil != memq(K_("REP"), m_oContext.m_prefixes))
                    {
                        m_oContext.m_prefixes = delq(
                            K_("REP"),
                            m_oContext.m_prefixes );

                        opcode = readU8() | 0xF30F00;
                    }
                    else
                    {
                        opcode = readU8() | 0x0F00;
                    }
                } // if

                return opcode;
            } // switch opcode
        } // for
    } // decodeOp

    private: Val decodeRb(int iRb)
        { return decodeReg(RegClass_Gpr, m_oContext.m_nAdrSiz, iRb); }

    private: Val decodeDisp32()
    {
        Val annot = getAnnot();
        when (nil != annot) return list(annot);
        return list(MakeInt(readU32()));
    } // decodeDisp32

    private: Val decodeEa(uint nOpdSiz, RegClass eRegClass = RegClass_Gpr)
    {
        uint nModRm = getModRm();

        Mod  eMod = static_cast<Mod>(nModRm & 0xC0);
        uint nRm  = ldb(3, 0, nModRm);

        switch (eMod)
        {
        case X86::Mod_Disp0:
            switch (nRm)
            {
            case X86::Rm_Sib:       // [base+index]
                return decodeOpdSiz(decodeEaSib(eMod), nOpdSiz);

            case X86::Rm_Disp32:    // [disp32]
                return decodeOpdSiz(decodeDisp32(), nOpdSiz);

            default:
                return decodeOpdSiz(list(decodeRb(nRm)), nOpdSiz);
            } // switch rm

        case X86::Mod_Disp8:
            if (X86::Rm_Sib == nRm)
            {   // [base+index+disp8]
                Val sib  = decodeEaSib(eMod);
                Val disp = decodeS8();
                return decodeOpdSiz(nconc(sib, list(disp)), nOpdSiz);
            }
            else
            {
                // [base+disp8]
                return decodeOpdSiz(list(decodeRb(nRm), decodeS8()), nOpdSiz);
            }

        case X86::Mod_Disp32:
            if (X86::Rm_Sib == nRm)
            {   // [base+index+disp32]
                Val sib  = decodeEaSib(eMod);
                Val disp = decodeIv();
                return decodeOpdSiz(nconc(sib, list(disp)), nOpdSiz);
            }
            else
            {   // [base+disp32]
                Val rb   = decodeRb(nRm);
                Val disp = decodeIv();
                return decodeOpdSiz(list(rb, disp), nOpdSiz);
            }

        case X86::Mod_Reg:
            return decodeReg(eRegClass, nOpdSiz, nRm);

        default:
            CAN_NOT_HAPPEN();
        } // switch
    } // decodeEa

    //   7 6  5 4 3  2 1 0
    //  +----+------+------+
    //  | ss | index| base |
    //  +----+------+------+
    private: Val decodeEaSib(Mod eMod)
    {
        uint nSib    = readU8();
        Scale eScale = static_cast<Scale>(nSib & 0xC0);
        Reg   eIndex = static_cast<Reg>(ldb(3, 3, nSib));
        Reg   eBase  = static_cast<Reg>(ldb(3, 0, nSib));
        Val   rb     = decodeRb(eBase);
        if (X86::Rm_Sib != eIndex)
        {
            Val ri = decodeEaSibIndex(eScale, eIndex);
            if (X86::Mod_Disp0 == eMod && X86::Rm_Disp32 == eBase)
            {
                return nconc(decodeDisp32(), list(ri));
            }
            else
            {
                return list(rb, ri);
            }
        }
        else if (X86::Rm_Disp32 == eBase)
        {
            return nil;
        }
        else
        {
            return list(rb);
        }
    } // decodeEaSib

    private: Val decodeEaSibIndex(Scale eScale, Reg eIndex)
    {
        Val rx = decodeReg(RegClass_Gpr, m_oContext.m_nAdrSiz, eIndex);
        uint nScale;
        switch (eScale)
        {
        case X86::Scale_1:
            return rx;

        case X86::Scale_2:
            nScale = 2;
            break;

        case X86::Scale_4:
            nScale = 4;
            break;

        case X86::Scale_8:
            nScale = 8;
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch eScale

        char16 wsz[10];
        ::wsprintfW(wsz, L"%s*%d",
            rx->StaticCast<Symbol>()->m_name->
                StaticCast<SimpleString>()->GetStart(),
            nScale );
        StackString_<> oName(wsz);
        return intern(oName);
    } // decodeEaSibIndex

    private: Val decodeGx(const char* const* ppsz)
    {
        return decodeReg(ppsz, ldb(3, 3, getModRm()));
    } // decodeGx

    private: Val decodeIv()
    {
        Val operand = getAnnot();
        if (nil == operand) return MakeInt(readS32());
        return operand;
    } // decodeIv

    private: Val decodeLabel(int iRel)
    {
        return list(Qgo, Fixnum::Encode(m_pbRunner - m_pbStart + iRel));
    } // decodeLabel

    private: Val decodeOpdSiz(Val opd, int nOpdSiz)
    {
        switch (nOpdSiz)
        {
        case 8:
            return listA(K_("EA"), K_("BYTE"), opd);
        case 16:
            return listA(K_("EA"), K_("WORD"), opd);
        case 32:
            return cons(K_("EA"), opd);
        case 64:
            return listA(K_("EA"), K_("QWORD"), opd);
        case 80:
            return listA(K_("EA"), K_("TBYTE"), opd);
        case 128:
            return listA(K_("EA"), K_("DQWORD"), opd);
        } // nOpdSize
        CAN_NOT_HAPPEN();
    } // decodeOpdSiz

    private: Val decodeOperand(OpdFmt eOpdFmt)
    {
        switch (eOpdFmt)
        {
        case OpdFmt_1:  // SHL Ev: 1
            return Fixnum::Encode(1);

        case OpdFmt_Ib: // imm8
        case OpdFmt_Is: // imm8 with sign extend
            return decodeS8();

        case OpdFmt_Iw: // imm16
            return Fixnum::Encode(readS16());

        //case OpdFmt_Iq: // imm64
        case OpdFmt_Iv: // imm16: imm32 or imm64
            if (16 == m_oContext.m_nOpdSiz)
            {
                return Fixnum::Encode(readS16());
            }
            else
            {
                return decodeIv();
            }

        case OpdFmt_Iz: // imm32
            return decodeIv();

        case OpdFmt_Jb:
            return decodeLabel(readS8());

        case OpdFmt_Jv:
        {
            Val label = getAnnot();
            if (nil != label) return label;
            return decodeLabel(readS32());
        } // OpdFmt_Jv

        case OpdFmt_Eb:
            return decodeEa(8);

        case OpdFmt_Ew:
            return decodeEa(16);

        case OpdFmt_Ed:
        //case OpdFmt_Eq:
            return decodeEa(32);

        case OpdFmt_Ev:
            return decodeEa(m_oContext.m_nOpdSiz);

        case OpdFmt_Gb:
            return decodeGx(k_rgpszGpr8);

        case OpdFmt_Gd:
            return decodeGx(k_rgpszGpr32);

        case OpdFmt_Gq:
        case OpdFmt_Gv:
            return decodeReg(
                RegClass_Gpr, 
                m_oContext.m_nOpdSiz,
                ldb(3, 3, getModRm()) );

        case OpdFmt_Ob:
            return decodeOpdSiz(decodeDisp32(), 8);

        case OpdFmt_Ov:
            return decodeOpdSiz(decodeDisp32(), 32);

        case OpdFmt_M:
            return decodeEa(m_oContext.m_nOpdSiz);
        case OpdFmt_Md:
            return decodeEa(64, RegClass_Mmx);
        case OpdFmt_Mp:
            return decodeEa(64, RegClass_Mmx);
        case OpdFmt_Mpd:
            return decodeEa(128, RegClass_Xmm);
        case OpdFmt_Mps:
            return decodeEa(128, RegClass_Xmm);
        //case OpdFmt_Mq:

        case OpdFmt_Nq:     // mm
        case OpdFmt_Pd:     // mm
        case OpdFmt_Pq:     // mm
            return decodeReg(RegClass_Mmx, 64, ldb(3, 0, getModRm()));

        case OpdFmt_Qd:     // mm or Mq
        case OpdFmt_Qq:     // mm or Mq
        case OpdFmt_Qdq:
            return decodeEa(64, RegClass_Mmx);

        case OpdFmt_Udq:    // ModRm:rm = xmm
        case OpdFmt_Upd:    // ModRm:rm = xmm
        case OpdFmt_Ups:    // ModRm:rm = xmm
            return decodeReg(RegClass_Xmm, 128, ldb(3, 0, getModRm()));

        case OpdFmt_Vdq:    // ModRm:reg = xmm
        case OpdFmt_Vpd:    // ModRm:reg = xmm
        case OpdFmt_Vpq:    // ModRm:reg = xmm
        case OpdFmt_Vps:    // ModRm:reg = xmm
        case OpdFmt_Vq:     // ModRm:reg = xmm
        case OpdFmt_Vsd:    // ModRm:reg = xmm
        case OpdFmt_Vss:    // ModRm:reg = xmm
            return decodeReg(RegClass_Xmm, 128, ldb(3, 3, getModRm()));

        case OpdFmt_Wdq:    // xmm or Mdq
        case OpdFmt_Wpd:    // xmm or Mdq
        case OpdFmt_Wps:    // xmm or Mdq
        case OpdFmt_Wq:     // xmm or Mdq
            return decodeEa(128, RegClass_Xmm);

        case OpdFmt_Wsd:    // xmm or Mdq
            return decodeEa(64, RegClass_Xmm);

        case OpdFmt_Wss:    // xmm or Mdq
            return decodeEa(32, RegClass_Xmm);

        case OpdFmt_eAX:    // 0
        case OpdFmt_eCX:    // 1
        case OpdFmt_eDX:    // 2
        case OpdFmt_eBX:    // 3
        case OpdFmt_eSP:    // 4
        case OpdFmt_eBP:    // 5
        case OpdFmt_eSI:    // 6
        case OpdFmt_eDI:    // 7
            if (16 == m_oContext.m_nOpdSiz)
            {
                return decodeReg(k_rgpszGpr16, eOpdFmt - OpdFmt_eAX);
            }
            else
            {
                return decodeReg(k_rgpszGpr32, eOpdFmt - OpdFmt_eAX);
            }

        case OpdFmt_rAX:    // 0    PUSH/POP
        case OpdFmt_rCX:    // 1    PUSH/POP
        case OpdFmt_rDX:    // 2    PUSH/POP
        case OpdFmt_rBX:    // 3    PUSH/POP
        case OpdFmt_rSP:    // 4    PUSH/POP
        case OpdFmt_rBP:    // 5    PUSH/POP
        case OpdFmt_rSI:    // 6    PUSH/POP
        case OpdFmt_rDI:    // 7    PUSH/POP
            return decodeReg(k_rgpszGpr32, eOpdFmt - OpdFmt_rAX);

        case OpdFmt_AL:     // 0
        case OpdFmt_CL:     // 1
        case OpdFmt_DL:     // 2
        case OpdFmt_BL:     // 3
        case OpdFmt_AH:     // 4
        case OpdFmt_CH:     // 5
        case OpdFmt_DH:     // 6
        case OpdFmt_BH:     // 7
            return decodeReg(k_rgpszGpr8, eOpdFmt - OpdFmt_AL);

        case OpdFmt_DX: // For IN: OUT
            return decodeReg(k_rgpszGpr16, 2);

        case OpdFmt_Sw: // reg of modrm selects segment register
            return nil;

        case OpdFmt_EvF64:  // CALL/JMP Ev
        case OpdFmt_EvD64:  // PUSH Ev
            return decodeEa(32);

        default:
            CAN_NOT_HAPPEN();
        } // switch eOpdFmt
    } // decodeOperand

    private: void decodeOperands(const Format* pFormat)
    {
        ASSERT(NULL != pFormat);
        for (uint i = 0; i < pFormat->m_cOperands; i++)
        {
            m_oContext.mv_operand[i] = decodeOperand(
                pFormat->m_rgeOpdFmt[i] );
        } // for i
    } // decodeOperands

    private: Val decodeReg(const char* const* ppsz, uint n)
    {
        return intern(make_string(ppsz[n]));
    } // decodeReg

    private: Val decodeReg(RegClass eClass, uint nSize, uint nReg)
    {
        switch (eClass)
        {
        case RegClass_Gpr:
            switch (nSize)
            {
            case 8:
                return decodeReg(k_rgpszGpr8, nReg);
            case 16:
                return decodeReg(k_rgpszGpr16, nReg);
            case 32:
                return decodeReg(k_rgpszGpr32, nReg);
            }
            break;
        case RegClass_Mmx:
            return decodeReg(k_rgpszMmx, nReg);
        case RegClass_Xmm:
            return decodeReg(k_rgpszXmm, nReg);
        } // switch
        CAN_NOT_HAPPEN();
    } // decodeReg

    private: Val decodeS8()
    {
        return Fixnum::Encode(readS8());
    } // decodeS8

    private: void disasmEa(Val ea)
    {
        Val delimiter = Character::FromCode('[');
        foreach (List::Enum, oEnum, cdr(ea))
        {
            Val opd = oEnum.Get();

            if (Character::FromCode('+') == delimiter &&
                fixnump(opd) &&
                lt(opd, zero) )
            {
                // no '+'
            }
            else
            {
                write_char(delimiter);
            }

            delimiter = Character::FromCode('+');

            if (consp(opd) && Qquote == car(opd))
            {
                format(t, "'~S", cadr(opd));
            }
            else if (consp(opd) && Qgo == car(opd))
            {
                format(t, "L~4,'0X", cadr(opd));
            }
            else
            {
                format(t, "~S", opd);
            }
        } // for each elt
        format(t, "]");
    } // disasmEa

    private: void disasmInsn()
    {
        uint const nStart = getAddr();

        const Format* pFormat;

        m_oContext.mv_operand[0] = getAnnot();
        if (nil != m_oContext.mv_operand[0])
        {
            pFormat = &sm_oFormat_DD;
        }
        else
        {
            uint opcode = decodeOp();
            pFormat = getFormat(opcode);
            decodeOperands(pFormat);
        }

        uint const nEnd = getAddr();

        Val labelChar = Character::FromCode(Space);

        if (Fixnum::Encode(nStart) >= car(m_labels))
        {
            if (Fixnum::Encode(nStart) == car(m_labels))
            {
                labelChar = Character::FromCode('L');
            }

            m_labels = cdr(m_labels);
        } // if

        if (! m_oEnumGcDesc.AtEnd() && m_oEnumGcDesc.Get() == nStart)
        {
            const GcMap::Bits* pGcDesc = m_oEnumGcDesc.GetDesc();

            GcMap::Bits nBits = *pGcDesc;

            bool fContinue = 0 != (nBits & 1);
            nBits >>= 1;

            GcMap::Kind const eKind = static_cast<GcMap::Kind>(nBits & 3);
            nBits >>= 2;

            format(t, ";  ____ GC Desc: ~A", K_(k_rgpszGcDesc[eKind]));

            uint cBits = GcMap::BitsInWord - 3;

            switch (eKind)
            {
            case GcMap::StdCallDesc:
                break;

            case GcMap::CallDesc2:
                format(t, " #args=~D", Fixnum::Encode(nBits & 7));
                nBits >>= 3;
                cBits -= 3;
                break;

            case GcMap::CallDesc3:
                format(t, " #args=~D", Fixnum::Encode(nBits & 0xff));
                nBits >>= 8;
                cBits -= 8;
                break;

            case GcMap::JumpDesc:
                for (int i = 0; i < lengthof(k_rgpszGcReg); i++)
                {
                    if (nBits & 1)
                    {
                        format(t, " ~A", K_(k_rgpszGcReg[i]));
                    }
                    
                    nBits >>= 1;
                } // for i
                cBits -= lengthof(k_rgpszGcReg);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch eKind

            int ofs = 0;
            for (;;)
            {
                while (cBits > 0)
                {
                    if (nBits & 1)
                    {
                        format(t, " [ESP+~D]", Fixnum::Encode(ofs));
                    }
                    nBits >>= 1;
                    cBits -= 1;
                    ofs += sizeof(Val);
                } // while

                if (! fContinue)
                {
                    break;
                }

                pGcDesc++;
                nBits = *pGcDesc;
                fContinue = 0 != (nBits & 1);
                nBits >>= 1;
                cBits = GcMap::BitsInWord - 1;
            } // for

            format(t, "~%");
            m_oEnumGcDesc.Next();
        } // if

        format(t, "; ~C~4,'0X",
            labelChar,
            Fixnum::Encode(nStart) );

        uint const k = 6;

        for (uint nRunner = nStart; nRunner < nStart + k; nRunner++)
        {
            if (nRunner < nEnd)
            {
                format(t, " ~2,'0X", Fixnum::Encode(getU8(nRunner)));
            }
            else
            {
                write_string(L"   ");
            }
        } // for nRunner

        {
            StackString_<> oMnemonic(pFormat->m_psz);
            format(t, "  ~A", oMnemonic);
        }

        Val comma = Character::FromCode(Space);
        for (uint i = 0; i < pFormat->m_cOperands; i++)
        {
            format(t, "~C ", comma);
            comma = Character::FromCode(',');

            Val val = m_oContext.mv_operand[i];
            if (consp(val) && Qquote == car(val))
            {
                format(t, "'~S", cadr(val));
            }
            else if (consp(val) && Qgo == car(val))
            {
                format(t, "L~4,'0X", cadr(val));
            }
            else if (consp(val) && K_("EA") == car(val))
            {
                disasmEa(val);
            }
            else
            {
                format(t, "~S", val);
            }
        } // for each operand

        uint nBreak = nStart + k;
        for (uint nRunner = nBreak; nRunner < nEnd; nRunner++)
        {
            if (nRunner == nBreak)
            {
                write_string(L"\n;      ");
                nBreak += k;
            }

            format(t, " ~2,'0X", Fixnum::Encode(getU8(nRunner)));
        } // for nRunner

        write_char(Newline);
    } // disasmInsn

    // [G]
    private: uint getAddr() const
        { return static_cast<uint>(m_pbRunner - m_pbStart); }

    private: Val getAnnot()
    {
        when (m_oEnumAnnot.AtEnd()) return nil;

        FunObj::Annot oAnnot = m_oEnumAnnot.Get();

        when (getPc() < oAnnot.m_ofs) return nil;

        m_oEnumAnnot.Next();

        when (getPc() != oAnnot.m_ofs) return nil;

        m_pbRunner += 4;

        switch (oAnnot.m_eKind)
        {
        case FunObj::Annot::Kind_AbsLabel:
        {
            UInt ofs = m_pFunObj->FetchU32(oAnnot.m_ofs);
            ofs -= reinterpret_cast<UInt>(m_pFunObj->GetCodeStart());

            return list(Qgo, Fixnum::Encode(ofs));
        } // Kind_AbsLabel

        case FunObj::Annot::Kind_ClosedLit:
            return list(K_("CLIT"), m_pFunObj->FetchVal(oAnnot.m_ofs));

        case FunObj::Annot::Kind_ClosedVar:
        {
            UInt const ofs = m_pFunObj->FetchUn(oAnnot.m_ofs);
            if (ofs < 0x10000)
            {
                // In template
                return list(K_("CVAR"), reinterpret_cast<Val>(ofs));
            }

            ClosedCell* p = reinterpret_cast<ClosedCell*>(
                ofs - offsetof(ClosedCell, m_value) );

            return list(K_("CVAR"), p->Encode());
        } // Kind_CloseVar

        case FunObj::Annot::Kind_DllLink:
        {
            DllEntry* pEntry = m_pFunObj->FetchDllEntry(oAnnot.m_ofs);
            Val proc_info = pEntry->m_proc_info;

            Val file_info  = proc_info->StaticCast<DllProcInfo>()->
                m_dll_file_info;

            return  list(K_("DLL"),
                file_info->StaticCast<DllFileInfo>()->m_pathname,
                proc_info->StaticCast<DllProcInfo>()->m_name );
        } // Kind_DllLink

        case FunObj::Annot::Kind_LispVal:
        {
            Val val = m_pFunObj->FetchVal(oAnnot.m_ofs);

            if (listp(val) || symbolp(val) || fixnump(val))
            {
                return list(Qquote, val);
            }

            return val;
        } // Kind_LispVal

        case FunObj::Annot::Kind_LocalCallee:
        case FunObj::Annot::Kind_NamedCallee:
            return m_pFunObj->FetchCallee(oAnnot.m_ofs);

        case FunObj::Annot::Kind_RelLabel:
            return list(
                Qgo,
                Fixnum::Encode(m_pFunObj->FetchU32(oAnnot.m_ofs)) );

        case FunObj::Annot::Kind_SymFun:
        {
            Val cell = m_pFunObj->FetchSymFun(oAnnot.m_ofs);
            return list(Qfunction, cell);
        } // Kind_SymVal

        case FunObj::Annot::Kind_SymSetf:
        {
            Val cell = m_pFunObj->FetchSymSetf(oAnnot.m_ofs);
            return list(
                Qfunction,
                list(Qsetf, cell->StaticCast<SetfCell>()->m_name) );
        } // Kind_SymVal

        case FunObj::Annot::Kind_SymVal:
        {
            Val cell = m_pFunObj->FetchSymVal(oAnnot.m_ofs);
            return list(K_("SYMVAL"), cell->StaticCast<ValueCell>()->m_name);
        } // Kind_SymVal

        case FunObj::Annot::Kind_TlvOfs:
        {
            int ofs = m_pFunObj->FetchU32(oAnnot.m_ofs);
            ofs -= offsetof(Thread, mv_tlv);
            Val tlvrec = svref(
                VAR(Atlv_vectorA),
                Fixnum::Encode(ofs / sizeof(Val)) );
            return list(K_("TLV"), tlvrec->StaticCast<TlvRecord>()->m_name);
        } // Kind_TlvOfs

        default:
        {
            char sz[100];
            ::wsprintfA(sz, "Unknown annot %d", oAnnot.m_eKind);
            return make_string(sz);
        } // default
        } // switch kind
    } // getAnnot

    private: const Format* getFormat(uint opcode)
    {
        const Format* pFormat = sm_oFormatTab.Get(opcode);
        if (NULL == pFormat)
        {
            return &sm_oFormat_DB;
        }

        switch (pFormat->m_eExtend)
        {
        case Extend_ModRm:
            opcode <<= 8;
            opcode |= ldb(3, 3, getModRm());
            return getFormat(opcode);

        case Extend_None:
            return pFormat;

        case Extend_TwoByte:
            opcode <<= 8;
            opcode |= readU8();
            return getFormat(opcode);

        default:
            CAN_NOT_HAPPEN();
        } // switch
    } // getFormat

    private: uint getModRm()
    {
        if (0x100 == m_oContext.m_nModRm)
        {
            m_oContext.m_nModRm = readU8();
        }
        return m_oContext.m_nModRm;
    } // getModRm

    private: uint getPc() const
    {
        return static_cast<uint>(m_pbRunner - m_pbStart);
    } // getPc

    private: uint8 getU8(uint ofs) const
    {
        ASSERT(m_pbStart + ofs < m_pbEnd);
        return m_pbStart[ofs];
    } // getU8

    // [R]
    private: int readS16()
    {
        int s16 = readU8();
        return (readS8() << 8) | s16;
    } // readS16

    private: int readS32()
    {
        int s32 = readU16();
        return (readS16() << 16) | s32;
    } // readS16

    private: int8 readS8()
    {
        when (m_pbRunner >= m_pbEnd) return 0;
        return static_cast<int8>(*m_pbRunner++);
    } // readU8

    private: uint readU32()
    {
        uint u32 = readU16();
        return (readU16() << 16) | u32;
    } // readU32

    private: uint readU16()
    {
        uint u16 = readU8();
        return (readU8() << 8) | u16;
    } // readU16

    private: uint8 readU8()
    {
        when (m_pbRunner >= m_pbEnd) return 0;
        return *m_pbRunner++;
    } // readU8

    private: void run()
    {
        format(t, "; ~S~%", m_pFunObj->Encode());

        format(t, ";  code size       = ~D (#x~X)~%",
            Fixnum::Encode(m_pFunObj->GetCodeSize()),
            Fixnum::Encode(m_pFunObj->GetCodeSize()) );

        uint nFrame = m_pFunObj->GetFunDesc()->m_nFrame;
        format(t, ";  frame           = #x~X (size=~D type=~D)~%",
            Fixnum::Encode(nFrame),
            Fixnum::Encode(nFrame >> 2),
            Fixnum::Encode(nFrame & 3) );

        format(t, ";  annotation size = ~D~%",
            Fixnum::Encode(m_pFunObj->GetAnnotSize()) );

        format(t, ";  gc map size     = ~D~%",
            Fixnum::Encode(m_pFunObj->GetGcMapSize()) );

        format(t, ";~%");

        do
        {
            disasmInsn();
        } while (m_pbRunner < m_pbEnd);
    } // run
}; // Disasm

Format Disasm::sm_oFormat_DB =
{
    1,
    Extend_None,
    0,
    "DB",
    OpdFmt_Ib,
}; // Disasm::sm_oFormat_DB

Format Disasm::sm_oFormat_DD =
{
    1,
    Extend_None,
    0,
    "DD",
    OpdFmt_Iz,
}; // Disasm::sm_oFormat_DB

Disasm::FormatTab Disasm::sm_oFormatTab;

class InnerFunCollector
{
    private: Val m_fns;
    private: Val m_tail;

    private: InnerFunCollector() :
        m_fns(nil),
        m_tail(nil) {}

    public: static Val Run(Val const fn)
    {
        InnerFunCollector oCollector;
        oCollector.walk(fn);
        return cdr(oCollector.m_fns);
    } // Run

    private: static bool isLocalFunction(Val const x)
    {
        if (! functionp(x)) 
        {
            return false;
        }
        Val name = function_name(x);
        return ! function_name_p(name);
    } // isLocalFunction

    private: void walk(Val const fn)
    {
        if (nil != memq(fn, m_fns)) 
        {
            return;
        }
        
        if (nil == m_tail)
        {
            m_fns = m_tail = list(fn);
        }
        else
        {
            m_tail = setf_cdr(list(fn), m_tail);
        }

        FunObj* const pFun = fn->StaticCast<FunObj>();

        foreach (FunObj::EnumAnnot, oEnum, pFun)
        {
            Val datum = nil;
            int ofs = oEnum.Get().m_ofs;
            switch (oEnum.Get().m_eKind)
            {
            case FunObj::Annot::Kind_LispVal:
                datum = pFun->FetchVal(ofs);
                break;

            case FunObj::Annot::Kind_NamedCallee:
            case FunObj::Annot::Kind_LocalCallee:
                datum = pFun->FetchCallee(ofs);
                break;
            } // switch annot

            if (isLocalFunction(datum))
            {
                walk(datum);
            }
        } // for each annot
    } // walk
}; // InnerFunCollector

// fspec ::= simple-fspec
//       |   (flet   simple-fspec fname+)
//       |   (labels simple-fspec fname+)
//
class FunSpecParser
{
    public: static Val Run(Val const fspec)
    {
        Val const fn = parseSimple(fspec);
        if (nil != fn)
        {
            return fn;
        }

        if (! consp(fspec))
        {
            return nil;
        }

        if (car(fspec) == Qflet || car(fspec) == Qlabels)
        {
            Val const outer = parseSimple(cadr(fspec));
            if (nil == outer)
            {
                return nil;
            }

            foreach (List::Enum, oEnum, InnerFunCollector::Run(outer))
            {
                Val const inner = oEnum.Get();
                if (equal(function_name(inner), fspec))
                {
                    return inner;
                }
            } // for each annot
        }
        else if (car(fspec) == Kinternal)
        {
            Val const outer = parseSimple(cadr(fspec));
            if (nil == outer)
            {
                return nil;
            }

            Val index = caddr(fspec);
            if (! fixnump(index))
            {
                return nil;
            }

            foreach (List::Enum, oEnum, InnerFunCollector::Run(outer))
            {
                if (zero == index)
                {
                    return oEnum.Get();
                }

                index = xxsub(index, one);
            } // for each annot
        } // if internal

        return nil;
    } // Run

    private: static Val parseSimple(Val fspec)
    {
        // function object
        if (functionp(fspec))
        {
            return fspec;
        }

        // name
        if (symbolp(fspec))
        {
            return fboundp(fspec) ? symbol_function(fspec) : nil;
        }

        if (! consp(fspec))
        {
            return fspec;
        }

        // (setf name)
        if (car(fspec) == Qsetf)
        {
            if (consp(cdr(fspec)) &&
                symbolp(cadr(fspec)) &&
                nil == cddr(fspec) )
            {
                return fboundp(fspec) ? fdefinition(fspec) : nil;
            }

            return nil;
        } // if setf

        // (:macro name)
        if (car(fspec) == Kmacro)
        {
            if (consp(cdr(fspec)) &&
                symbolp(cadr(fspec)) &&
                nil == cddr(fspec) )
            {
                return macro_function(cadr(fspec));
            }

            return nil;
        } // if macro

        return nil;
    } // parse_fspec_one
}; // FunSpecParser

} // namespace

defun(CommonLisp::disassemble, (Val const x))
{
    Val const fn = FunSpecParser::Run(x);

    if (nil == fn)
    {
        if (! function_name_p(x))
        {
            SignalTypeError(x, Qfunction_name);
        }

        format(t, "No such function ~S.~%", x);
        return nil;
    }

    Disasm::Run(fn);
    if (nil == function_name(fn))
    {
        foreach (List::Enum, oEnum, InnerFunCollector::Run(fn))
        {
            write_string(";\n");
            Disasm::Run(oEnum.Get());
        } // for each fn
    }

    return nil;
} // disassemble

} // TinyCl
