//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Compiler Code Generator
// tinycl/compiler/tinycl_c_cg_asm.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_asm.h#3 $
//
#if !defined(INCLUDE_tinycl_compiler_cg_asm_h)
#define INCLUDE_tinycl_compiler_cg_asm_h

#include "./tinycl_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Assembler
//
class Assembler : protected Mm
{
    protected: struct Annot :
        //public Castable_<Annot>,
        public DoubleLinkedItem_<Annot>,
        public LocalObject
    {
        public: enum Kind
        {
            Kind_None,

            Kind_AbsLabel,
            Kind_ClosedLit,
            Kind_ClosedVar,
            Kind_FunRef,
            Kind_Literal,
            Kind_LocalCallee,
            Kind_NamedCallee,
            Kind_RelLabel,
            Kind_SymSetf,
            Kind_SymFun,
            Kind_SymVal,
            Kind_TlvOfs,

            Kind_Limit,
        }; // AnnotKind

        Kind    m_eKind;
        uint    m_nAddr;
        uint    m_ofs;
        Val     m_value;

        // ctor
        Annot(
            Kind    eKind,
            uint    iAddr,
            uint    ofs,
            Val     val ) :
                m_eKind(eKind),
                m_nAddr(iAddr),
                m_ofs(ofs),
                m_value(val) {}

        // [G]
        uint GetAddr()   const { return m_nAddr; }
        Kind GetKind()   const { return m_eKind; }
        uint GetOffset() const { return m_ofs; }
    }; // Annot

    protected: typedef DoubleLinkedList_<Annot> Annots;

    protected: class BBlockExt : public LocalObject
    {
        public: uint m_nAddr;
        public: uint m_ofs;

        public: uint GetAddr()   const { return m_nAddr; }
        public: uint GetOffset() const { return m_ofs; }
    }; // BBlockExt

    protected: class Span :
        public Castable_<Span>,
        public DoubleLinkedItem_<Span>,
        public LocalObject,
        public WorkListItem_<Span>
    {
        public:  uint m_nAddr;
        public:  uint m_ofs;

        // [G]
        public: uint GetAddr()   const { return m_nAddr; }
        public: uint GetOffset() const { return m_ofs; }

        // [I]
        public: virtual bool IsCrossing(uint nAddr)
            { return GetAddr() >= nAddr; }
    }; // Span

    protected: typedef DoubleLinkedList_<Span> Spans;

    protected: template<class T, class B = Span> class Span_ :
        public WithCastable_<T, B>
    {
        protected: typedef Span_<T, B> Base;

        protected: Span_(int iAddr, int ofs)
        {
            m_nAddr = iAddr;
            m_ofs   = ofs;
        } // Span_
    }; // Span_

    protected: struct FunExt : public LocalObject
    {
        Annots  m_oAnnots;
        uint    m_cbAnnots;

        FunExt() : m_cbAnnots(0) {}
    }; // FunExt

    // Member variables
    protected: Function*    m_pFun;

    protected: static bool isS8(Int i)
    {
        return i >= -128 && i <= 127;
    } // isS8
}; // Assembler

//////////////////////////////////////////////////////////////////////
//
// CicsAssembler
//
class CicsAssembler : public Assembler
{
    protected: class CodeBuf
    {
        enum Limits
        {
            BufferSize = 1024 * 16,
        }; // Limits

        protected: uint8*  m_prgbBuf;
        public:    uint   m_cb;
        public:    uint   m_nAddr;
        protected: Spans  m_oSpans;

        // ctor
        public: CodeBuf()
        {
            m_prgbBuf = reinterpret_cast<uint8*>(
                Context::Get()->Alloc(BufferSize) );

            Reset();
        } // CodeBuf

        // [A]
        public: void AddSpan(Span* pSpan)
            { m_oSpans.Append(pSpan); }

        // [E]
        public: void EmitU32(uint32 u32)
        {
            ASSERT(m_cb + 4 < BufferSize);
            // FIXME yosi@msn.com 2007-10-08 How do we implement
            // bigendian?
            m_prgbBuf[m_cb + 0] = static_cast<uint8>((u32 >>  0) & 0xff);
            m_prgbBuf[m_cb + 1] = static_cast<uint8>((u32 >>  8) & 0xff);
            m_prgbBuf[m_cb + 2] = static_cast<uint8>((u32 >> 16) & 0xff);
            m_prgbBuf[m_cb + 3] = static_cast<uint8>((u32 >> 24) & 0xff);
            m_cb += 4;
            m_nAddr += 4;
        } // EmitU32

        public: void EmitU8(uint8 u8)
        {
            ASSERT(m_cb < BufferSize);
            m_prgbBuf[m_cb] = u8;
            m_cb += 1;
            m_nAddr += 1;
        } // EmitU8

        public: class EnumSpan : public Spans::Enum
        {
            public: EnumSpan(const CodeBuf* p) :
                Spans::Enum(&p->m_oSpans) {}
        }; // EnumSpan

        // [G]
        public: const uint8* GetCode(uint ofs) const
        {
            ASSERT(ofs <= m_cb);
            return m_prgbBuf + ofs;
        } // GetCode

        public: uint GetAddr()   const { return m_nAddr; }
        public: uint GetOffset() const { return m_cb; }

        // [P]
        public: void PatchS32(uint ofs, int32 s32)
        {
            ASSERT(ofs + 4 < m_cb);
            // FIXME yosi@msn.com 2007-10-08 How do we implement
            // bigendian?
            m_prgbBuf[ofs + 0] = static_cast<uint8>((s32 >>  0) & 0xff);
            m_prgbBuf[ofs + 1] = static_cast<uint8>((s32 >>  8) & 0xff);
            m_prgbBuf[ofs + 2] = static_cast<uint8>((s32 >> 16) & 0xff);
            m_prgbBuf[ofs + 3] = static_cast<uint8>((s32 >> 24) & 0xff);
        } // PatchU8

        public: void PatchU8(uint ofs, uint8 u8)
        {
            ASSERT(ofs < m_cb);
            ASSERT(ofs < BufferSize);
            m_prgbBuf[ofs] = u8;
        } // PatchU8

        // [R]
        public: void Reset()
        {
            m_cb    = 0;
            m_nAddr = 0;
            m_oSpans.DeleteAll();
        } // Reset

        public: void Reserve(int n)
            { m_cb += n; }
    }; // CodeBuf

    protected: CodeBuf  m_oCodeBuf;

    protected: struct JumpSpan : public Span_<JumpSpan>
    {
        public: static const char* Kind_() { return "JumpSpan"; }

        public: bool    m_fLongForm;
        public: int     m_opLong;
        public: int     m_opShort;
        public: BBlock* m_pTargetBB;

        // ctor
        public: JumpSpan(
            int     iAddr,
            int     ofs,
            bool    fLongForm,
            int     opShort,
            int     opLong,
            BBlock*     pTargetBB ) :
                m_fLongForm(fLongForm),
                m_opLong(opLong),
                m_opShort(opShort),
                m_pTargetBB(pTargetBB),
                Base(iAddr, ofs) {}

        // [G]
        public: uint GetTarget() const
            { return m_pTargetBB->GetWork<BBlockExt>()->m_nAddr; }

        // [I]
        public: override bool IsCrossing(uint nAddr) const
        {
            if (GetAddr() < nAddr)
            {
                //      JUMP L1
                //      -- iAddr --
                //  L1: ...
                return GetTarget() >= nAddr;
            }
            else
            {
                // L1:  ...
                //      -- iAddr --
                //      JUMP L1
                return GetTarget() < nAddr;
            }
        } // IsCrossing
    }; // JumpSpan

    // [E]
    protected: void emitJump(
        uint    const opShort,
        uint    const opLong,
        BBlock* const pTargetBB )
    {
        bool fLongForm = false;

        if (BBlockExt* const pExt = pTargetBB->GetWork<BBlockExt>())
        {
            if (0 != pExt->GetAddr())
            {
                int const iRel = pExt->GetAddr() - (m_oCodeBuf.GetAddr() + 2);
                fLongForm = ! isS8(iRel);
            }
        }

        // We always allocate 6 byte for jump instruction
        if (fLongForm)
        {
            emitOp(opLong);
            emitU32(0);
            if (opLong <= 0xFF)
            {
                m_oCodeBuf.Reserve(1);
            }
        }
        else
        {
            emitOp(opShort);
            emitU8(0);
            m_oCodeBuf.Reserve(4);
        }

        m_oCodeBuf.AddSpan(
            new(this) JumpSpan(
                m_oCodeBuf.GetAddr(),
                m_oCodeBuf.GetOffset(),
                fLongForm,
                opShort,
                opLong,
                pTargetBB ) );
    } // emitJump

    protected: void emitLit(Literal* pLit, Annot::Kind eKind)
        { return emitLit(pLit->GetDatum(), eKind); }

    protected: void emitLit(Val const lit, Annot::Kind const eKind)
    {
        FunExt* const pFunExt = m_pFun->GetWork<FunExt>();

        pFunExt->m_oAnnots.Append(
            new(this) Annot(
                eKind,
                m_oCodeBuf.GetAddr(),
                m_oCodeBuf.GetOffset(),
                lit ) );
        m_oCodeBuf.EmitU32(0);

        pFunExt->m_cbAnnots += sizeof(uint32);
    } // emitLit

    protected: void emitOp(uint const opcode)
    {
        if (opcode > 0xFFFF)
        {
            emitU8(static_cast<uint8>((opcode >> 16) & 0xff));
        }

        if (opcode > 0xFF)
        {
            emitU8(static_cast<uint8>((opcode >> 8) & 0xff));
        }

        emitU8(static_cast<uint8>(opcode & 0xff));
    } // emitOp

    protected: void emitU32(uint32 u32)
        { m_oCodeBuf.EmitU32(u32); }

    protected: void emitU8(uint8 u8)
        { m_oCodeBuf.EmitU8(u8); }

    // [F]
    protected: void fixSpans()
    {
        class Resolver
        {
            private: CodeBuf*   m_pCodeBuf;
            private: Function*  m_pFun;

            // ctor
            private: Resolver(CodeBuf* pCodeBuf, Function* pFun) :
                m_pCodeBuf(pCodeBuf),
                m_pFun(pFun) {}

            // Entry point
            public: static void Run(CodeBuf* pCodeBuf, Function* pFun)
            {
                Resolver oResolver(pCodeBuf, pFun);
                oResolver.run();
            } // Run

            // [A]
            private: void addCrossing(
                WorkList_<Span>* const pWorkList,
                uint             const nAddr )
            {
                foreach (CodeBuf::EnumSpan, oEnum, m_pCodeBuf)
                {
                    Span* const pSpan = oEnum.Get();
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
                    Span* const pSpan = oWorkList.Pop();
                    if (JumpSpan* pJumpS = pSpan->DynamicCast<JumpSpan>())
                    {
                        computeJumpSpan(&oWorkList, pJumpS);
                    }
                    else
                    {
                        COMPILER_INTERNAL_ERROR();
                        break;
                    }
                } // while
            } // compute

            private: void computeJumpSpan(
                WorkList_<Span>*    const pWorkList,
                JumpSpan*           const pJumpS )
            {
                if (pJumpS->m_fLongForm)
                {
                    return;
                }

                int const iRel = pJumpS->GetTarget() - pJumpS->GetAddr();
                if (! isS8(iRel))
                {
                    addCrossing(pWorkList, pJumpS->GetAddr());
                    uint const cb = pJumpS->m_opLong > 0xFF ? 4 : 3;
                    pJumpS->m_fLongForm = true;
                    updateLabels(pJumpS->GetAddr(), cb);
                }
            } // computeJumpSpan

            // [P]
            private: void processJumpSpan(JumpSpan* const pJumpS)
            {
                uint const nTarget = pJumpS->GetTarget();

                uint const ofsInsn = pJumpS->GetOffset() - 6;

                if (! pJumpS->m_fLongForm)
                {
                    // Short form
                    m_pCodeBuf->PatchU8(
                        ofsInsn,
                        static_cast<uint8>(pJumpS->m_opShort & 0xff) );

                    m_pCodeBuf->PatchU8(
                        ofsInsn + 1,
                        static_cast<uint8>(nTarget - pJumpS->GetAddr()) );
                }
                else if (pJumpS->m_opLong <= 0xFF)
                {
                    // Long form op <= 0xFF
                    m_pCodeBuf->PatchU8(
                        ofsInsn,
                        static_cast<uint8>(pJumpS->m_opLong & 0xff) );

                    m_pCodeBuf->PatchS32(
                        ofsInsn + 1,
                        nTarget - pJumpS->GetAddr() );
                }
                else
                {
                    // Long form op >= 0xFF
                    m_pCodeBuf->PatchU8(
                        ofsInsn,
                        static_cast<uint8>((pJumpS->m_opLong >> 8) & 0xff) );

                    m_pCodeBuf->PatchU8(
                        ofsInsn + 1,
                        static_cast<uint8>(pJumpS->m_opLong & 0xff) );

                    m_pCodeBuf->PatchS32(
                        ofsInsn + 2,
                        nTarget - pJumpS->GetAddr() );
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
                    else
                    {
                        COMPILER_INTERNAL_ERROR();
                    }
                } // for each span
            } // run

            // [U]
            private: void updateLabels(uint nAddr, uint cb)
            {
                m_pCodeBuf->m_nAddr += cb;

                foreach (CodeBuf::EnumSpan, oEnum, m_pCodeBuf)
                {
                    Span* pSpan = oEnum.Get();
                    if (pSpan->GetAddr() >= nAddr)
                    {
                        pSpan->m_nAddr += cb;
                    }
                } // for each span

                foreach (
                    Function::EnumBBlockReverse,
                    oEnum,
                    m_pFun )
                {
                    BBlockExt* pExt = oEnum.Get()->GetWork<BBlockExt>();
                    if (pExt->GetAddr() < nAddr) break;
                    pExt->m_nAddr += cb;
                } // for each bblock

                foreach (
                    Annots::EnumReverse,
                    oEnum,
                    &m_pFun->GetWork<FunExt>()->m_oAnnots )
                {
                    Annot* pAnnot = oEnum.Get();
                    if (pAnnot->GetAddr() < nAddr) break;
                    pAnnot->m_nAddr += cb;
                } // for each annot
            } // updateLabels
        }; // Resolver

        Resolver::Run(&m_oCodeBuf, m_pFun);

        {
            Instruction* pPrologueI = m_pFun->GetPrologueI();
            if (JumpI* pJump = pPrologueI->GetNext()->DynamicCast<JumpI>())
            {
                pJump->SetIndex(
                    pJump->GetBB()->GetNext()->
                        GetWork<BBlockExt>()->GetAddr() );
            }
        }

        foreach (Function::EnumBBlock, oEnum, m_pFun)
        {
            BBlock* const pBB = oEnum.Get();

            int const iDiff =
                pBB->GetWork<BBlockExt>()->GetAddr() -
                pBB->GetFirstI()->GetIndex();

            if (0 == iDiff)
            {
                continue;
            }

            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();
                pI->SetIndex(pI->GetIndex() + iDiff);
            } // for each insn
        } // for each bblock
    } // fixSpans

    // [S]
    protected: void serializeCode(uint8* pbCode)
    {
        uint nAddr   = 0;
        uint ofsCode = 0;

        foreach (CodeBuf::EnumSpan, oEnum, &m_oCodeBuf)
        {
            Span* pSpan = oEnum.Get();

            uint cbSpan = pSpan->m_nAddr - nAddr;

            ::CopyMemory(
                pbCode,
                m_oCodeBuf.GetCode(ofsCode),
                cbSpan );

            pbCode += cbSpan;
            nAddr   = pSpan->GetAddr();
            ofsCode = pSpan->GetOffset();
        } // for each span

        ASSERT(m_oCodeBuf.GetAddr() >= nAddr);

        ::CopyMemory(
            pbCode,
            m_oCodeBuf.GetCode(ofsCode),
            m_oCodeBuf.GetAddr() - nAddr );
    } // serializeCode
}; // CicsAssembler

} // Compiler

} // TinyCl

#endif //!defined(INCLUDE_tinycl_compiler_cg_asm_h)
