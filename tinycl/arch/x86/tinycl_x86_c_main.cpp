#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Target
// tinycl_x86_compiler.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_main.cpp#8 $
//
#include "./tinycl_x86_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

using namespace X86;

static const RegDesc k_rgoRegDesc[8 * 5] =
{
    { RegClass_Gpr, 0x00,   8, 0, "AL" },
    { RegClass_Gpr, 0x01,   8, 1, "CL" },
    { RegClass_Gpr, 0x02,   8, 2, "DL" },
    { RegClass_Gpr, 0x03,   8, 3, "BL" },
    { RegClass_Gpr, 0x04,   8, 4, "AH" },
    { RegClass_Gpr, 0x05,   8, 5, "CH" },
    { RegClass_Gpr, 0x06,   8, 6, "DH" },
    { RegClass_Gpr, 0x07,   8, 7, "BH" },

    { RegClass_Gpr, 0x08,  16, 0, "AX" },
    { RegClass_Gpr, 0x09,  16, 1, "CX" },
    { RegClass_Gpr, 0x0A,  16, 2, "DX" },
    { RegClass_Gpr, 0x0B,  16, 3, "BX" },
    { RegClass_Gpr, 0x0C,  16, 4, "SP" },
    { RegClass_Gpr, 0x0D,  16, 5, "BP" },
    { RegClass_Gpr, 0x0E,  16, 6, "SI" },
    { RegClass_Gpr, 0x0F,  16, 7, "DI" },

    { RegClass_Gpr, 0x10,  32, 0, "EAX" },
    { RegClass_Gpr, 0x11,  32, 1, "ECX" },
    { RegClass_Gpr, 0x12,  32, 2, "EDX" },
    { RegClass_Gpr, 0x13,  32, 3, "EBX" },
    { RegClass_Gpr, 0x14,  32, 4, "ESP" },
    { RegClass_Gpr, 0x15,  32, 5, "EBP" },
    { RegClass_Gpr, 0x16,  32, 6, "ESI" },
    { RegClass_Gpr, 0x17,  32, 7, "EDI" },

    { RegClass_Fpr, 0x18,  64, 0, "MM0" },
    { RegClass_Fpr, 0x19,  64, 1, "MM1" },
    { RegClass_Fpr, 0x1A,  64, 2, "MM2" },
    { RegClass_Fpr, 0x1B,  64, 3, "MM3" },
    { RegClass_Fpr, 0x1C,  64, 4, "MM4" },
    { RegClass_Fpr, 0x1D,  64, 5, "MM5" },
    { RegClass_Fpr, 0x1E,  64, 6, "MM6" },
    { RegClass_Fpr, 0x1F,  64, 7, "MM7" },

    { RegClass_Fpr, 0x20, 128, 0, "XMM0" },
    { RegClass_Fpr, 0x21, 128, 1, "XMM1" },
    { RegClass_Fpr, 0x22, 128, 2, "XMM2" },
    { RegClass_Fpr, 0x23, 128, 3, "XMM3" },
    { RegClass_Fpr, 0x24, 128, 4, "XMM4" },
    { RegClass_Fpr, 0x25, 128, 5, "XMM5" },
    { RegClass_Fpr, 0x26, 128, 6, "XMM6" },
    { RegClass_Fpr, 0x27, 128, 7, "XMM7" },
}; // k_rgoRegDesc

#define defregset(mp_name, mp_class, mp_n) \
    static const RegDesc* s_rgp ## mp_name [] = {

#define endregset(mp_name, mp_class, mp_n) \
    }; \
    static RegSet s_o ## mp_name = \
        { RegClass_ ## mp_class, mp_n, s_rgp ## mp_name };

#define regentry(mp_reg) \
    &k_rgoRegDesc[$ ## mp_reg],

#include "./tinycl_x86_c_isa.inc"

static RegSet s_oFprEmpty =
{
    RegClass_Fpr, 0, NULL,
}; // s_oFprEmpty

static RegGroup s_oFprGroup =
{
    "FPR",
    RegClass_Fpr,
    8,              // width
    NULL,           // rn
    NULL,           // rsp
    NULL,
    &s_oFprAll,     // all
    &s_oFprAll,     // allocable
    &s_oFprAll,     // free
    &s_oFprAll,     // args
    &s_oFprEmpty,   // callee save
    &s_oFprEmpty,   // caller save
}; // s_oFprGroup

static RegSet s_oGprEmpty =
{
    RegClass_Gpr, 0
}; // s_oGprEmpty

static RegGroup s_oGprGroup =
{
    "GPR",
    RegClass_Gpr,
    4,                      // width
    &k_rgoRegDesc[$rn],     // $rn
    &k_rgoRegDesc[$sp],     // $rsp
    &k_rgoRegDesc[$tcb],    // $tcb
    &s_oGprAll,             // all
    &s_oGprAllocable,       // allocable
    &s_oGprAllocable,       // free
    &s_oGprArgs,            // args
    &s_oGprEmpty,           // callee save
    &s_oGprAllocable,       // caller save
}; // s_oGprGroup

class X86Target : public Target
{
    private: CgOutput* m_rgpArgReg[Arch::MultipleValuesLimit];
    private: Physical* m_rgpPhysical[lengthof(k_rgoRegDesc)];

    public: X86Target() :
        Target(&s_oFprGroup, &s_oGprGroup)
    {
        ::ZeroMemory(m_rgpPhysical, sizeof(m_rgpPhysical));
        ::ZeroMemory(m_rgpArgReg,   sizeof(m_rgpArgReg));
    } // X86Target

    // [C]
    public: override int ComputeFrameSize(FrameReg* pFd) const
    {
        if (nil == pFd->GetFrameKind())
        {
            return 0;
        }

        if (Qblock == pFd->GetFrameKind())
        {
            return sizeof(BlockFrame);
        }

        if (Qcatch == pFd->GetFrameKind())
        {
            return sizeof(CatchFrame);
        }

        if (Kfinally == pFd->GetFrameKind())
        {
            int const cArgs = static_cast<int>(
                Fixnum::Decode_(pFd->GetDefI()->GetLy()) );

            return sizeof(FinallyFrame) + sizeof(Val) * cArgs;
        } // finally

        if (Qhandler_bind == pFd->GetFrameKind())
        {
            int const cbEntries =
                pFd->GetDefI()->CountOperands() *
                sizeof(Val);

            int const cb = sizeof(HandlerFrame) + cbEntries;
            return cb;
        } // handler_bind

        if (Qhandler_case == pFd->GetFrameKind())
        {
            int cb = sizeof(TryCatchFrame);
            foreach (FrameReg::EnumUser, oEnum, pFd)
            {
                if (oEnum.Get()->GetI()->Is<CatchI>())
                {
                    cb += sizeof(TagsFrame::Tag);
                }
            } // for each user
            return cb;
        } // handler_case

        if (Qlet == pFd->GetFrameKind())
        {
            int const cbEntries =
                pFd->GetDefI()->CountOperands() *
                sizeof(BindFrame::Entry);

            int const cb = sizeof(BindFrame) + cbEntries;
            return cb;
        } // bind frame

        if (Qtagbody == pFd->GetFrameKind())
        {
            int cb = sizeof(TagsFrame);
            foreach (FrameReg::EnumUser, oEnum, pFd)
            {
                if (oEnum.Get()->GetI()->Is<TagDefI>())
                {
                    cb += sizeof(TagsFrame::Tag);
                }
            } // for each user
            return cb;
        } // tagbody

        C_INTERNAL_ERROR("Unknown frame type");
        return 0;
    } // ComputeFrameSize

    // [G]
    public: override CgOutput* GetArgReg(uint nNth)
    {
        const RegSet* pArgRegs = s_oGprGroup.m_pArgs;
        if (nNth < static_cast<uint>(pArgRegs->m_c))
        {
            return GetPhysical(pArgRegs->m_prgpReg[nNth]);
        }

        if (nNth >= lengthof(m_rgpArgReg))
        {
            COMPILER_INTERNAL_ERROR();
            return new ThreadSlot(RegClass_Gpr, 0);
        }

        CgOutput* pMx = m_rgpArgReg[nNth];
        if (NULL == pMx)
        {
            pMx = new ThreadSlot(
                RegClass_Gpr,
                offsetof(Thread, mv_value[nNth]) );

            m_rgpArgReg[nNth] = pMx;
        }

        return pMx;
    } // GetArgReg

    public: override Physical* GetPhysical(const RegDesc* pRegDesc)
    {
        uint nIndex = pRegDesc->m_nId;
        Physical* pRx = m_rgpPhysical[nIndex];
        if (NULL == pRx)
        {
            pRx = new Physical(pRegDesc);
            m_rgpPhysical[nIndex] = pRx;
        }
        return pRx;
    } // GetPhysical
}; // X86Target

static Val const
kv_ShortCut[] =
{
    QPtype_error,
    QPundefined_function,
}; // kv_ShortCut

bool NeedArity(const Operand* const pSx)
{
    if (Function* pFun = pSx->DynamicCast<Function>())
    {
        return pFun->NeedArity();
    } // if

    //= <FIXME date="2009-01-25" by="yosi@msn.com">
    //=   We shold tell number of arguments to GC Map consturcotr.
    if (Literal* pLx = pSx->DynamicCast<Literal>())
    {
        for (
            const Val* p = kv_ShortCut;
            p < &kv_ShortCut[lengthof(kv_ShortCut)];
            p++ )
        {
            if (pLx->GetDatum() == *p)
            {
                return false;
            }
        } // for
        return true;
    } // if literal
    //= </FIXME>

    return true;
} // NeedArity

class PassX86Target :
    public Pass_<PassX86Target>
{
    public: static const char* GetName_() { return "X86Target"; }

    public: void Run()
    {
        Context::Get()->SetTarget(new X86Target);
    } // Run
}; /// PassX86Target

DEFPASS(X86Target)

} // Compiler

} // TinyCl
