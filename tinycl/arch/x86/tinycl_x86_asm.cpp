#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - X86 - Built-In Functions
// arch/x86/tinycl_x86_asm.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_asm.cpp#7 $
//
#define DEBUG_ASM 0
#include "./tinycl_x86_asm.h"

namespace TinyCl
{

namespace X86
{

Mm* Mm::sm_pMm;

// [E]
void Asm::embedAnnots(FunObj* pFunObj)
{
    foreach (CodeBuf::EnumAnnot, oEnum, &m_oCodeBuf)
    {
        Annot* pAnnot = oEnum.Get();

        switch (pAnnot->m_eAnnot)
        {
        case FunObj::Annot::Kind_AbsLabel:
        {
            Label* p = reinterpret_cast<Label*>(pAnnot->m_datum);
            uint32 addr = static_cast<uint32>(
                p->m_nAddr +
                reinterpret_cast<UInt>(pFunObj->GetCodeStart()) );
            pFunObj->PatchU32(pAnnot->m_nAddr, addr);
            break;
        } // Kind_AbsLabel

        case FunObj::Annot::Kind_DllLink:
            pFunObj->PatchU32(
                pAnnot->m_nAddr,
                static_cast<uint32>(pAnnot->m_datum->ToInt()) );
            break;

        case FunObj::Annot::Kind_LispVal:
            if (FUNOBJ_self == pAnnot->m_datum)
            {
                pFunObj->PatchVal(pAnnot->m_nAddr, pFunObj->Encode());
            }
            else
            {
                pFunObj->PatchVal(pAnnot->m_nAddr, pAnnot->m_datum);
            }
            break;

        case FunObj::Annot::Kind_NamedCallee:
            pFunObj->PatchCallee(pAnnot->m_nAddr, pAnnot->m_datum);
            break;

        case FunObj::Annot::Kind_RelLabel:
        {
            Label* p = reinterpret_cast<Label*>(pAnnot->m_datum);
            pFunObj->PatchU32(pAnnot->m_nAddr, p->m_nAddr);
            break;
        } // Kind_AbsLabel

        default:
            error("NYI: asm annotation");
            // NOTREACHED
        } // switch kind
    } // for each annot
} // Asm::embedAnnots

void X86Asm::emitPrologue(int iMin, int iMax, int iRest)
{
    m_oArity.m_iMin  = iMin;
    m_oArity.m_iMax  = iMax;
    m_oArity.m_iRest = iRest;

    if (iRest)
    {
        if (0 != iMin)
        {
            cmp($rn, Fixnum::Encode(iMin));
            jl(m_oLabelArity);
        }

        cmp($rn, Fixnum::Encode(Arch::MultipleValuesLimit));
        jg(m_oLabelArity);
    }
    else if (iMin == iMax)
    {
        if (0 == iMin)
        {
            test($rn, $rn);
        }
        else
        {
            cmp($rn, Fixnum::Encode(iMin));
        }

        jne(m_oLabelArity);
    }
    else
    {
        if (0 == iMin)
        {
            test($rn, $rn);
        }
        else
        {
            cmp($rn, Fixnum::Encode(iMin));
        }

        jl(m_oLabelArity);

        cmp($rn, Fixnum::Encode(iMax));
        jg(m_oLabelArity);
    } // if
} // X86Asm::emitPrologue

// [F]
Val Asm::finish()
{
    fixSpans();
    return makeFunObj();
} // Asm::finish

void Asm::fixSpans()
{
    class Resolver
    {
        private: CodeBuf* m_pCodeBuf;

        // ctor
        private: Resolver(CodeBuf* pCodeBuf) :
            m_pCodeBuf(pCodeBuf) {}

        // Entry Point
        public: static void Run(CodeBuf* pCodeBuf)
        {
            Resolver oPass(pCodeBuf);
            oPass.run();
        } // Run

        // [A]
        private: void addCrossing(WorkList_<Span>* pWorkList, int nAddr)
        {
            foreach (CodeBuf::EnumSpan, oEnum, m_pCodeBuf)
            {
                Span* pSpan = oEnum.Get();
                if (pSpan->IsInList()) continue;

                if (pSpan->IsCrossing(nAddr))
                {
                    pWorkList->Push(pSpan);
                }
            } // for each span
        } // addCrossing

        // [C]
        private: void compute()
        {
            WorkList_<Span> oWorkList;
            foreach (CodeBuf::EnumSpan, oEnum, m_pCodeBuf)
            {
                oWorkList.Push(oEnum.Get());
            } // for each span

            while (! oWorkList.IsEmpty())
            {
                Span* pSpan = oWorkList.Pop();
                if (JumpSpan* pJumpS = pSpan->DynamicCast<JumpSpan>())
                {
                    computeJumpSpan(&oWorkList, pJumpS);
                }
            } // while
        } // compute

        private: void computeJumpSpan(
            WorkList_<Span>*    pWorkList,
            JumpSpan*           pJumpS )
        {
            if (pJumpS->m_fLongForm) return;

            int iRel = pJumpS->GetTarget() - pJumpS->m_nAddr;
            if (! isS8(iRel))
            {
                addCrossing(pWorkList, pJumpS->m_nAddr);
                int cb = pJumpS->m_opLong > 0xFF ? 4 : 3;
                pJumpS->m_fLongForm = true;
                updateLabels(pJumpS->m_nAddr, cb);
            }
        } // computeJumpSpan

        // [P]
        private: void processJumpSpan(JumpSpan* pJumpS)
        {
            int iTarget = pJumpS->GetTarget();
            int ofsJump = pJumpS->m_ofs - 5;

            if (pJumpS->m_opLong > 0xff)
            {
                ofsJump -= 1;
            }

            if (! pJumpS->m_fLongForm)
            {
                m_pCodeBuf->PatchU8(
                    ofsJump,
                    static_cast<uint8>(pJumpS->m_opShort & 0xff) );

                m_pCodeBuf->PatchU8(
                    ofsJump + 1,
                    static_cast<uint8>(iTarget - pJumpS->m_nAddr & 0xff) );
            }
            else if (pJumpS->m_opLong <= 0xFF)
            {
                m_pCodeBuf->PatchU8(
                    ofsJump,
                    static_cast<uint8>(pJumpS->m_opLong & 0xff) );

                m_pCodeBuf->PatchS32(
                    ofsJump + 1,
                    iTarget - pJumpS->m_nAddr );
            }
            else
            {
                m_pCodeBuf->PatchU8(
                    ofsJump,
                    static_cast<uint8>((pJumpS->m_opLong >> 8) & 0xff) );

                m_pCodeBuf->PatchU8(
                    ofsJump + 1,
                    static_cast<uint8>(pJumpS->m_opLong & 0xff) );

                m_pCodeBuf->PatchS32(
                    ofsJump + 2,
                    iTarget - pJumpS->m_nAddr );
            }
        } // processJumpSpan

        // [R]
        private: void run()
        {
            compute();

            foreach (CodeBuf::EnumSpan, oEnum, m_pCodeBuf)
            {
                Span* pSpan = oEnum.Get();
                if (JumpSpan* pJumpS = pSpan->DynamicCast<JumpSpan>())
                {
                    processJumpSpan(pJumpS);
                }
            } // for each span
        } // run

        private: void updateLabels(int nAddr, int cb)
        {
            m_pCodeBuf->Increase(cb);

            foreach (CodeBuf::EnumSpan, oEnum, m_pCodeBuf)
            {
                Span* pSpan = oEnum.Get();
                if (pSpan->GetAddr() >= nAddr)
                {
                    pSpan->m_nAddr += cb;
                }
            } // for each span

            foreach (CodeBuf::EnumLabel, oEnum, m_pCodeBuf)
            {
                Label* pLabel = oEnum.Get();
                if (pLabel->m_nAddr >= nAddr)
                {
                    pLabel->m_nAddr += cb;
                }
            } // for each label

            foreach (CodeBuf::EnumAnnot, oEnum, m_pCodeBuf)
            {
                Annot* pAnnot = oEnum.Get();
                if (pAnnot->m_nAddr >= nAddr)
                {
                    pAnnot->m_nAddr += cb;
                }
            } // for each annot
        } // updateLabels
    }; // Resolver

    Resolver::Run(&m_oCodeBuf);
} // Asm::fixSpans

Val Asm::install()
{
    Val fn = finish();
    if (symbolp(m_name))
    {
        m_name->StaticCast<Symbol>()->m_function = fn;
    }
    else
    {
        Val cell = intern_setf_cell(cadr(m_name));
        cell->StaticCast<SetfCell>()->m_function = fn;
    }
    
    return fn;
} // Asm::install


Val Asm::makeFunObj()
{
    size_t cbCode  = m_oCodeBuf.GetAddr();
    size_t cbGcMap = 4;

    size_t cbFunObj = sizeof(FunObj);
    cbFunObj += RoundUp(cbCode, 4);
    cbFunObj += cbGcMap;
    cbFunObj += m_cbAnnots;
    cbFunObj += sizeof(FunObj::FunDesc);
    cbFunObj  = RoundUp(cbFunObj, FunObj::Align);

    Val fn = Thread::Get()->AllocCode(
        m_classd,
        cbFunObj );

    FunObj* pFunObj = fn->StaticCast<FunObj>();

    FunObj::FunDesc* pDesc = pFunObj->GetFunDesc();

    pDesc->m_nFrame = ((m_cbFrame + 4) << 2) | m_eFrame;
    pDesc->m_cbCode = static_cast<uint32>(cbCode);

    pDesc->m_ofsAnnot = static_cast<uint32>(
        sizeof(FunObj) +
        RoundUp(cbCode, 4) );

    pDesc->m_ofsGcMap = pDesc->m_ofsAnnot + m_cbAnnots;

    serializeCode(pFunObj);
    embedAnnots(pFunObj);
    serializeAnnots(pFunObj);

    pFunObj->GetGcMap()[0] = 0;

    pFunObj->m_frob = m_name;

    pFunObj->m_cookie = reinterpret_cast<Val>( 
        static_cast<Int>(FunObj::Cookie) );

    ASSERT(pFunObj->GetAnnotSize() == m_cbAnnots);

    #if DEBUG_ASM
        DEBUG_FORMAT("~S~%", fn);
    #endif

    return fn;
} // Asm::makeFunObj

void Asm::serializeAnnots(FunObj* pFunObj)
{
    FunObj::Annot* pAnnot = pFunObj->GetAnnotStart();
    foreach (CodeBuf::EnumAnnot, oEnum, &m_oCodeBuf)
    {
        pAnnot->m_eKind = oEnum.Get()->m_eAnnot;
        pAnnot->m_ofs   = oEnum.Get()->m_nAddr;
        pAnnot++;
    } // for each annot
} // Asm::serializeAnnots

void Asm::serializeCode(FunObj* pFunObj)
{
    int nAddr = 0;
    int ofs   = 0;

    uint8* pb = pFunObj->GetCodeStart();

    foreach (CodeBuf::EnumSpan, oEnum, &m_oCodeBuf)
    {
        Span* pSpan  = oEnum.Get();
        uint  cbSpan = pSpan->m_nAddr - nAddr;

        ::CopyMemory(pb, m_oCodeBuf.GetCode(ofs), cbSpan);

        pb += cbSpan;
        nAddr = pSpan->GetAddr();
        ofs   = pSpan->GetOffset();
    } // for each span

    ASSERT(m_oCodeBuf.GetAddr() >= nAddr);

    ::CopyMemory(
        pb,
        m_oCodeBuf.GetCode(ofs),
        m_oCodeBuf.GetAddr() - nAddr );
} // Asm::serialzieCode

} // X86
} // TinyCl
