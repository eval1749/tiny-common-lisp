#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Assembler
// arch/x86/tinycl_x86_asm.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_asm.cpp#56 $
//
#include "./tinycl_x86_c_cg.h"
#include "../../compiler/cg/tinycl_c_cg_asm.h"
#include "../../compiler/cg/tinycl_c_cg_sink.h"

namespace TinyCl
{

namespace Compiler
{

using namespace X86;

void ComputeGcMap(IMm* const, Function* const, ByteSink* const);

/// <summary>
///   X86 assembler
/// </summary>
class X86Assembler :
    public CicsAssembler
{
    protected: struct Ea
    {
        enum Mode
        {
            Mode_None,

            Mode_Annot,
            Mode_Base,
            Mode_Index,
        }; // Mode

        Annot::Kind m_eAnnot;
        Mode        m_eMode;
        Reg         m_rb;
        Reg         m_rx;
        Int         m_iDisp;
        Val         m_datum;

        Ea(Mode eMode = Mode_Base, Reg rb = $r0, Int ofs = 0) :
            m_eAnnot(Annot::Kind_None),
            m_eMode(eMode),
            m_iDisp(ofs),
            m_rb(rb),
            m_rx($r0),
            m_datum(nil) {}

        Ea(Reg rb, Int ofs) :
            m_eAnnot(Annot::Kind_None),
            m_eMode(Mode_Base),
            m_iDisp(ofs),
            m_rb(rb),
            m_rx($r0),
            m_datum(nil) {}
    }; // Ea

    protected: void asmMov(Reg rd, Reg rx)
    {
        when (rx == rd) return;

        emitOp(op_MOV_Gv_Ev);
        emitU8(ModRm(Mod_Reg, rd, rx));
    } // asmMov

    protected: void asmMov(Reg rd, Operand* pSx)
    {
        if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            Reg rx = mapGpr(pRx);
            if (rd != rx)
            {
                emitOp(op_MOV_Gv_Ev);
                emitU8(ModRm(Mod_Reg, rd, rx));
            }
        }
        else if (StackSlot* pMx = pSx->DynamicCast<StackSlot>())
        {
            asmMov(rd, $sp, pMx->GetLocation());
        }
        else if (ThreadSlot* pMx = pSx->DynamicCast<ThreadSlot>())
        {
            asmMov(rd, $tcb, pMx->GetLocation());
        }
        else
        {
            emitOp(static_cast<Op>(op_MOV_eAX_Iv + (rd & 7)));
            emitIz(pSx);
        } // if
    } // asmMov

    protected: void asmMov(Reg rb, size_t ofs, Operand* pSx)
    {
        if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            emitOp(op_MOV_Ev_Gv);
            emitDisp(mapGpr(pRx), rb, ofs);
        }
        else
        {
            emitOp(op_MOV_Ev_Iz);
            emitDisp(opext_MOV_Ev_Iz, rb, ofs);
            emitIz(pSx);
        }
    } // asmMov

    protected: void asmMov(Reg rb, size_t ofs, Reg rx)
    {
        emitOp(op_MOV_Ev_Gv);
        emitDisp(rx, rb, ofs);
    } // asmMov

    protected: void asmMov(Reg rd, Reg rb, size_t ofs)
    {
        emitOp(op_MOV_Gv_Ev);
        emitDisp(rd, rb, ofs);
    } // asmMov

    protected: void asmXCHG(Reg r1, Reg r2)
    {
        if (r1 == r2) return;
        if (r2 == $r0) swap(r1, r2);

        if (r1 == $r0)
        {
            emitOp(static_cast<Op>(op_XCHG_eAX_eAX + r2));
        }
        else
        {
            emitOp(op_XCHG_Ev_Gv);
            emitU8(ModRm(Mod_Reg, r1, r2));
        }
    } // asmXCHG

    // [C]
    private: static int computeAnnexLocation(
        Function* const pClosure,
        Variable* const pVar )
    {
        ASSERT(pClosure->IsClosure());

        ClosedMarker* pQd = pClosure->FindUpVar(pVar)->
            DynamicCast<ClosedMarker>();
        if (NULL == pQd)
        {
            C_INTERNAL_ERROR("computeAnnexLocation: broken closure");
            return 0;
        }

        foreach (ClosedMarker::EnumUser, oEnum, pQd)
        {
            if (Instruction* const pVarAnnexI =
                    oEnum.GetI()->DynamicCast<VarAnnexI>() )
            {
                return pVarAnnexI->GetOutput()->
                    StaticCast<StackSlot>()->GetLocation();
            }
        } // for each upvar

        C_INTERNAL_ERROR("computeAnnexLocation: broken VarAnnex");
        return 0;
    } // computeAnnexLocation

    protected: static int computeFixedPathOffset(
        Function* const pCallee,
        Function* const pOwner )
    {
        Function* pCaller = pCallee;
        int ofs = 0;
        while (NULL != pCaller)
        {
            if (pCaller == pOwner)
            {
                return ofs;
            } // if owner

            if (pCaller->IsClosure())
            {
                return ofs;
            } // if closure

            ofs += pCaller->m_cbFrame;

            pCaller = pCaller->GetSingleCaller();
        } // while

        // pCallee to variable owner isn't fixed path.
        CLOG(1, "<li class='e'>Can't find path from ~S to ~S</li>~%",
            pCallee, pOwner );

        C_INTERNAL_ERROR("computeFixedPathOffset: not fixed path");
        return 0;
    } // computeFixedPathOffset

    private: static int computeFixedPathOffset(
        Function* const pCallee,
        Variable* const pVar )
    {
        ASSERT(NULL != pCallee);
        ASSERT(NULL != pVar);

        Function* const pOwner  = pVar->GetOwner();
        if (NULL == pOwner)
        {
            C_INTERNAL_ERROR("computeFixedPathOffset: No variable owner");
            return 0;
        }

        Function* pCaller = pCallee;
        int ofs = 0;
        while (NULL != pCaller)
        {
            if (pCaller == pOwner)
            {
                if (! pVar->HasLocation())
                {
                    COMPILER_INTERNAL_ERROR();
                }
                return ofs + pVar->GetLocation();
            } // if owner

            if (pCaller->IsClosure())
            {
                return ofs + computeAnnexLocation(pCaller, pVar);
            } // if closure

            ofs += pCaller->m_cbFrame;

            pCaller = pCaller->GetSingleCaller();
        } // while

        // pCallee to variable owner isn't fixed path.
        CLOG(1, "<li class='e'>Can't find path for ~S from ~S to ~S</li>~%",
            pVar, pCallee, pOwner );

        C_INTERNAL_ERROR("computeFixedPathOffset: not fixed path");
        return 0;
    } // computeFixedPathOffset

    // [E]
    protected: void emitDisp(Opext opext, Reg rb, ptrdiff_t ofs)
    {
        emitDisp(static_cast<Reg>(opext), rb, ofs);
    } // emitDisp

    //  [00 reg/opext r/m]             disp == 0, r/m != EBP
    //  [00 reg/opext r/m] 24          disp == 0, r/m == EBP
    //  [01 reg/opext r/m] disp8       r/m != ESP
    //  [01 reg/opext r/m] 24 disp8    r/m == ESP
    //  [10 reg/opext r/m] disp32      r/m != ESP
    //  [10 reg/opext r/m] 24 disp32   r/m == ESP
    protected: void emitDisp(Reg rd, Reg rb, ptrdiff_t ofs)
    {
        if (0 == ofs && Rm_Disp32 != (rb & 7))
        {
            emitU8(ModRm(Mod_Disp0, rd, rb));
            if (Rm_Sib == (rb & 7)) emitU8(0x24);
        }
        else if (ofs >= -128 && ofs <= 127)
        {
            emitU8(ModRm(Mod_Disp8, rd, rb));
            if (Rm_Sib == (rb & 7)) emitU8(0x24);
            emitU8(static_cast<uint8>(ofs));
        }
        else
        {
            emitU8(ModRm(Mod_Disp32, rd, rb));
            if (Rm_Sib == (rb & 7)) emitU8(0x24);
            emitU32(static_cast<uint32>(ofs));
        }
    } // emitDisp

    protected: void emitEv(Opext opext, Physical* pRy)
        { emitEv(static_cast<Reg>(opext), pRy); }

    protected: void emitEv(Opext opext, Operand* pSy)
        { emitEv(static_cast<Reg>(opext), pSy); }

    protected: void emitEv(Reg rx, Operand* pSy)
    {
        if (Physical* pRy = pSy->DynamicCast<Physical>())
        {
            emitEv(rx, pRy);
        }
        else if (MemSlot* pMy = pSy->DynamicCast<MemSlot>())
        {
            emitEv(rx, pMy);
        }
        else if (Pseudo* pQy = pSy->DynamicCast<Pseudo>())
        {
            Ea oEa = getEa(pQy);
            emitEa(&oEa, rx);
        }
        else
        {
            C_INTERNAL_ERROR("emitEv: unexpected operand");
        }
    } // emitEv

    protected: void emitEa(const Ea* pEa, Reg rx)
    {
        switch (pEa->m_eMode)
        {
        case Ea::Mode_Annot:
            if (Annot::Kind_TlvOfs == pEa->m_eAnnot)
            {
                emitU8(ModRm(Mod_Disp32, rx, $tcb));
            }
            else
            {
                emitU8(ModRm(Mod_Disp0, rx, Rm_Disp32));
            }
            emitLit(pEa->m_datum, pEa->m_eAnnot);
            break;

        case Ea::Mode_Base:
            emitDisp(rx, pEa->m_rb, pEa->m_iDisp);
            break;

        default:
            C_INTERNAL_ERROR("emitEa: unexpecred mode");
            break;
        } // switch mode
    } // emitEa

    protected: void emitEa(const Ea* pEa, Opext opext)
        { emitEa(pEa, static_cast<Reg>(opext)); }

    protected: void emitVsEv(
        X86::Op     const op,
        Operand*    pSx,
        Operand*    pSy )
    {
        if (Physical* const pFx = pSx->DynamicCast<Physical>())
        {
            emitOp(op);
            emitEv(mapFpr(pFx), pSy);
            return;
        }

        C_INTERNAL_ERROR("emitVsEv: expect physical");
    } // emitVsEv

    protected: void emitVsWs(
        X86::Op     const op,
        Operand*    pSx,
        Operand*    pSy )
    {
        if (Physical* const pFx = pSx->DynamicCast<Physical>())
        {
            emitOp(op);
            emitEv(mapFpr(pFx), pSy);
            return;
        }

        C_INTERNAL_ERROR("emitVsWs: expect physical");
    } // emitVsWs

    protected: void emitEv(Reg rx, Physical* pRy)
        { emitU8(ModRm(Mod_Reg, rx, mapGpr(pRy))); }

    protected: void emitEv(Reg rx, MemSlot* pMy)
    {
        if (pMy->Is<StackSlot>())
        {
            emitDisp(rx, $sp, pMy->GetLocation());
        }
        else if (pMy->Is<ThreadSlot>())
        {
            emitDisp(rx, $tcb, pMy->GetLocation());
        }
        else
        {
            C_INTERNAL_ERROR("emitEv: unexpected operand");
        }
    } // emitEv

    protected: void emitIz(Operand* pSx)
    {
        if (Literal* pLx = pSx->DynamicCast<Literal>())
        {
            emitLit(pLx, Annot::Kind_Literal);
            return;
        }

        if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            // NYI: Closed
            // NYI: LoadTimeValue
        }

        if (Function* pFun = pSx->DynamicCast<Function>())
        {
            emitLit(Fixnum::Encode(pFun), Annot::Kind_FunRef);
            return;
        }

        if (FunLit* pFunLit = pSx->DynamicCast<FunLit>())
        {
            emitLit(Fixnum::Encode(pFunLit->GetFun()), Annot::Kind_FunRef);
            return;
        }

        if (Integer* pInt = pSx->DynamicCast<Integer>())
        {
            emitU32(static_cast<uint32>(pInt->GetDatum()));
            return;
        }

        if (ClosedMarker* pCx = pSx->DynamicCast<ClosedMarker>())
        {
            emitLit(Fixnum::Encode(pCx->GetNth()), Annot::Kind_ClosedLit);
            return;
        } // closed marker

        if (Label* pLabel = pSx->DynamicCast<Label>())
        {
            emitLit(Fixnum::Encode(pLabel->GetBB()), Annot::Kind_RelLabel);
            return;
        } // label

        if (TlvOffset* pTlvOfs = pSx->DynamicCast<TlvOffset>())
        {
            emitLit(pTlvOfs->GetTlvRec(), Annot::Kind_TlvOfs);
            return;
        } // tlvofs

        C_INTERNAL_ERROR("emitIz: unexpected operand");
    } // emitIz

    // [G]
    protected: Ea getEa(Instruction* pI)
    {
        if (pI->Is<x86LeaI>())
        {
            if (FrameReg* pFx = pI->GetSx()->DynamicCast<FrameReg>())
            {
                return Ea($sp, pFx->GetLocation() + pI->GetIy());
            }

            return Ea(mapGpr(pI->GetSx()), pI->GetIy());
        } // x86LeaI

        if (pI->Is<SlotI>())
        {
            if (ClosedMarker* pCz = pI->GetSz()->DynamicCast<ClosedMarker>())
            {
                Ea oEa(Ea::Mode_Annot);
                oEa.m_eAnnot = Annot::Kind_ClosedVar;
                oEa.m_datum  = Fixnum::Encode(pCz->GetNth());
                return oEa;
            }

            if (Literal* pLz = pI->GetSz()->DynamicCast<Literal>())
            {
                Val cell = pLz->GetDatum();
                Ea oEa(Ea::Mode_Annot);
                oEa.m_eAnnot = Annot::Kind_SymVal;
                oEa.m_datum  = cell;
                return oEa;
            }

            if (pI->GetLx() == CLASS_stack_cell)
            {
                Ea oEa(mapGpr(pI->GetSz()), 0);
                return oEa;
            }
        } // SlotI

        if (pI->Is<TlvI>())
        {
            Ea oEa(Ea::Mode_Annot);
            oEa.m_eAnnot = Annot::Kind_TlvOfs;
            oEa.m_datum  = pI->GetLx();
            return oEa;
        } // TlvI

        if (pI->Is<UpVarRefI>())
        {
            Variable* pVar = pI->GetSy()->StaticCast<Pseudo>()->
                GetDefI()->GetSx()->StaticCast<Variable>();

            Reg rb = mapGpr(pI->GetSx());
            if (rb == $sp)
            {
                int ofs = computeFixedPathOffset(
                    pI->GetBB()->GetFunction(),
                    pVar );

                return Ea($sp, ofs);
            }

            if (static_cast<Reg>(0) != rb)
            {
                if (! pVar->HasLocation())
                {
                    COMPILER_INTERNAL_ERROR();
                }
                return Ea(rb, pVar->GetLocation());
            }

            C_INTERNAL_ERROR("getEa: UpVarRef base access");
            return Ea();
        } // UpVarRefI

        CLOG(1, "<div class=e>getEa: Unexpected instruction ~S</div>", pI);
        C_INTERNAL_ERROR("getEa: unexpcted instruciton");
        return Ea();
    } // getEa

    protected: Ea getEa(Operand* pSx)
    {
        if (Pseudo* pQx = pSx->DynamicCast<Pseudo>())
        {
            return getEa(pQx->GetDefI());
        }
        else if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            return Ea(Ea::Mode_Base, mapGpr(pRx));
        } // if

        C_INTERNAL_ERROR("getEa: unexpected operand");
        return Ea();
    } // getEa

    protected: Reg mapFpr(Physical* pRx)
    {
        const RegDesc* const pDesc = pRx->GetDesc();
        if (pDesc->m_eRegClass != RegClass_Fpr)
        {
            C_INTERNAL_ERROR("Expect FPR.");
            return static_cast<Reg>(0);
        }

        return static_cast<Reg>(pDesc->m_nId);
    } // mapFpr

    protected: Reg mapFpr(Operand* pSx)
    {
        if (Physical* pRd = pSx->DynamicCast<Physical>())
        {
            return mapFpr(pRd);
        }

        C_INTERNAL_ERROR("Expect FPR.");
        return static_cast<Reg>(0);
    } // mapFpr

    protected: Reg mapGpr(Physical* pRx)
    {
        const RegDesc* const pDesc = pRx->GetDesc();
        #if 0
            if (pDesc->m_eRegClass != RegClass_Gpr)
            {
                C_INTERNAL_ERROR("Expect GPR.");
                return static_cast<Reg>(0);
            }
        #endif
        return static_cast<Reg>(pDesc->m_nId);
    } // mapGpr

    protected: Reg mapGpr(Operand* pSx)
    {
        if (Physical* pRd = pSx->DynamicCast<Physical>())
        {
            return mapGpr(pRd);
        }

        C_INTERNAL_ERROR("Expect GPR.");
        return static_cast<Reg>(0);
    } // mapGpr

    protected: Int getIv(Immediate* pSx)
    {
        if (Literal* pLit = pSx->DynamicCast<Literal>())
        {
            return pLit->GetDatum()->ToInt();
        }

        if (Integer* pInt = pSx->DynamicCast<Integer>())
        {
            return pInt->GetDatum();
        }

        return 0x100;
    } // getIv

    protected: Tttn getTttn(Bool* pBx)
        { return pBx->GetDefI()->StaticCast<x86BoolI>()->GetTttn(); }
}; // X86Assembler

//
/// <summary>
///   X86 assemble pass
/// </summary>
class PassAssemble :
    public    Pass_<PassAssemble, ModulePass>,
    protected InstructionDispatcher,
    protected X86Assembler,
    protected X86Utility
{
    public: static const char* GetName_() { return "Assemble"; }

    protected: virtual void processModule(Module* pM) override
    {
        {
            CLOG_SECTION(1, "<h2>Assemble</h2>~%");
            foreach (Module::EnumFunction, oEnum, pM)
            {
                Function* pFun = oEnum.Get();
                processFunction(pFun);
                realizeFun(pFun);
            } // for each function
        }

        {
            CLOG_SECTION(1, "<h2>Realize</h2>");
            foreach (Module::EnumFunction, oEnum, pM)
            {
                Function* pFun = oEnum.Get();
                finalizeFun(pFun);
            } // for each function
        }
    } // Run

    // [F]
    private: void finalizeFun(Function* pFun)
    {
        FunObj* pFunObj = pFun->m_fn->StaticCast<FunObj>();

        FunObj::EnumAnnot oEnumAnnot(pFunObj);

        foreach (Annots::Enum, oEnum, &pFun->GetWork<FunExt>()->m_oAnnots)
        {
            Annot* pAnnot = oEnum.Get();

            FunObj::Annot::Kind eKind = FunObj::Annot::Kind_LispVal;

            switch (pAnnot->GetKind())
            {
            case Annot::Kind_ClosedVar:
                eKind = FunObj::Annot::Kind_ClosedVar;
                pFunObj->PatchVal(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            case Annot::Kind_ClosedLit:
                eKind = FunObj::Annot::Kind_ClosedLit;
                pFunObj->PatchVal(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            case Annot::Kind_FunRef:
                eKind = FunObj::Annot::Kind_LispVal;
                pFunObj->PatchVal(
                    pAnnot->GetAddr(),
                    Fixnum::To<Function>(pAnnot->m_value)->m_fn );
                break;

            case Annot::Kind_RelLabel:
            {
                BBlock* pBB = reinterpret_cast<BBlock*>(pAnnot->m_value);
                eKind = FunObj::Annot::Kind_RelLabel;
                pFunObj->PatchU32(
                    pAnnot->GetAddr(),
                    pBB->GetWork<BBlockExt>()->GetAddr() );
                break;
            } // Annot::Kind_RelLabel

            case Annot::Kind_Literal:
                eKind = FunObj::Annot::Kind_LispVal;
                pFunObj->PatchVal(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            case Annot::Kind_LocalCallee:
            {
                Function* pCallee = Fixnum::To<Function>(pAnnot->m_value);
                eKind = FunObj::Annot::Kind_LocalCallee;
                pFunObj->PatchCallee(
                    pAnnot->GetAddr(),
                    pCallee->m_fn );
                break;
            } // Annot::Kind_LocalCallee

            case Annot::Kind_NamedCallee:
                eKind = FunObj::Annot::Kind_NamedCallee;
                pFunObj->PatchCallee(
                    pAnnot->GetAddr(),
                    register_caller(pAnnot->m_value, pFun->m_fn) );
                break;

            case Annot::Kind_SymFun:
                eKind = FunObj::Annot::Kind_SymFun;
                pFunObj->PatchSymFun(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            case Annot::Kind_SymSetf:
                eKind = FunObj::Annot::Kind_SymSetf;
                pFunObj->PatchSymSetf(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            case Annot::Kind_SymVal:
                eKind = FunObj::Annot::Kind_SymVal;
                pFunObj->PatchSymVal(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            case Annot::Kind_TlvOfs:
                eKind = FunObj::Annot::Kind_TlvOfs;
                pFunObj->PatchTlvOfs(pAnnot->GetAddr(), pAnnot->m_value);
                break;

            default:
                C_INTERNAL_ERROR("finalizeFun: unexpected annot");
            } // switch kind

            oEnumAnnot.Set(FunObj::Annot(eKind, pAnnot->m_nAddr));
            oEnumAnnot.Next();
        } // for each annot

        pFunObj->m_cookie = Fixnum::FromInt(FunObj::Cookie);
    } // finalizeFun

    // [G]

    // for x86Test
    private: static Int getIb(Operand* pSy)
    {
        ASSERT(NULL != pSy);

        if (Integer* pIy = pSy->DynamicCast<Integer>())
        {
            return pIy->GetDatum();
        }

        if (Literal* pIy = pSy->DynamicCast<Literal>())
        {
            // For KEYSUPPLIED
            Val val = pIy->GetDatum();
            if (fixnump(val))
            {
                return reinterpret_cast<Int>(val);
            }
        }
        return 0x100;
    } // getIb

    // [P]
    private: void  processAdjustFrame(
        Opext   opextIb,
        Opext   opextIz,
        int     cbFrame )
    {
        if (isS8(cbFrame))
        {
            emitOp(op_SUB_Ev_Ib);
            emitU8(ModRm(Mod_Reg, opextIb, $sp));
            emitU8(static_cast<uint8>(cbFrame));
        }
        else
        {
            emitOp(op_SUB_Ev_Iz);
            emitU8(ModRm(Mod_Reg, opextIz, $sp));
            emitU32(cbFrame);
        }
    } // processAdjustFrame

    private: void parseArith(
        Instruction* const pI,
        Opext        const opext,
        X86::Op      const fop32,
        X86::Op      const fop64 )
    {
        if (pI->GetTy() == tyFloat32)
        {
            emitOp(fop32);
            emitEv(mapFpr(pI->GetOutput()), pI->GetSy());
            return;
        } // float32

        if (pI->GetTy() == tyFloat64)
        {
            emitOp(fop64);
            emitEv(mapFpr(pI->GetOutput()), pI->GetSy());
            return;
        } // float32

        parseIntArith(pI, opext);
    } // parseArith

    private: void parseIntArith(
        Instruction* const pI,
        Opext        const opext )
    {
        Operand* const pSx = pI->GetSx();

        if (opext_CMP_Ev_Iz != opext)
        {
            if (pI->GetOutput() != pSx)
            {
                C_INTERNAL_ERROR("parseIntArith: bad instruction");
                return;
            }
        }

        Operand* pSy = pI->GetSy();

        // COPY EDX <= NIL
        // CMP  EAX, NIL
        //  =>
        // CMP EAX, EDX
        if (Instruction* const pPrevI = pI->GetPrev())
        {
            if (pPrevI->Is<CopyInstruction>())
            {
                if (Physical* pPd =
                        pPrevI->GetOutput()->DynamicCast<Physical>() )
                {
                    if (pPrevI->GetSx()->Equal(pSy))
                    {
                        pSy = pPd;
                    }
                }
            }
        } // if prev

        if (Physical* const pRx = pSx->DynamicCast<Physical>())
        {
            if (Physical* const pRy = pSy->DynamicCast<Physical>())
            {
                emitOp(op_ADD_Gv_Ev + opext * 8);
                emitU8(ModRm(Mod_Reg, mapGpr(pRx), mapGpr(pRy)));
                return;
            }

            if (MemSlot* const pMy = pSy->DynamicCast<MemSlot>())
            {
                emitOp(op_ADD_Gv_Ev + opext * 8);
                emitEv(mapGpr(pRx), pMy);
                return;
            }

            if (Immediate* const pIy = pSy->DynamicCast<Immediate>())
            {
                parseIntArith(opext, pSx, pIy);
                return;
            }
        } // if reg

        if (MemSlot* const pMx = pSx->DynamicCast<MemSlot>())
        {
            if (Physical* const pRy = pSy->DynamicCast<Physical>())
            {
                emitOp(op_ADD_Ev_Gv + opext * 8);
                emitEv(mapGpr(pRy), pMx);
                return;
            }

            if (Immediate* const pIy = pSy->DynamicCast<Immediate>())
            {
                parseIntArith(opext, pSx, pIy);
                return;
            }
        } // if mem

        C_INTERNAL_ERROR("parseIntArith: unexpected operand");
    } // parseIntArith

    private: void parseIntArith(
        Opext       const opext,
        Operand*    const pSx,
        Immediate*  const pIy )
    {
        Int const iIv = getIv(pIy);

        // CMP r, 0 => TEST r, 0
        if (Physical* const pRx = pSx->DynamicCast<Physical>())
        {
            if (opext_CMP_Ev_Iz == opext && 0 == iIv)
            {
                Reg rx = mapGpr(pRx);
                emitOp(op_TEST_Ev_Gv);
                emitU8(ModRm(Mod_Reg, rx, rx));
                return;
            }
        } // if

        if (isS8(iIv))
        {
            emitOp(op_ADD_Ev_Ib);
            emitEv(opext, pSx);
            emitU8(static_cast<uint8>(iIv));
            return;
        }

        emitOp(op_ADD_Ev_Iz);
        emitEv(opext, pSx);
        emitIz(pIy);
    } // parseIntArith

    private: void parseShift(
        Instruction* const pI,
        Opext        const opext )
    {
        Operand* const pSx = pI->GetSx();
        Operand* const pSy = pI->GetSy();

        if (Integer* const pIy = pSy->DynamicCast<Integer>())
        {
            uint const nK = static_cast<uint>(pIy->GetDatum());
            if (1 == nK)
            {
                emitOp(op_SHL_Ev_1);
                emitEv(opext, pSx);
            }
            else if (nK < sizeof(Val) * 8)
            {
                emitOp(op_SHL_Ev_Ib);
                emitEv(opext, pSx);
                emitU8(static_cast<uint8>(nK));
            }
            else
            {
                C_INTERNAL_ERROR("Shift: too larget operand");
            }

            return;
        } // integer

        if (mapGpr(pSy) == $ECX)
        {
            emitOp(op_SHL_Ev_CL);
            emitEv(opext, pSx);
            return;
        } // physical

        C_INTERNAL_ERROR("Shift: invalid operand");
    } // parseShift

    private: void processBBlock(BBlock* const pBBlock)
    {
        BBlockExt* const pExt = pBBlock->GetWork<BBlockExt>();
        pExt->m_ofs   = m_oCodeBuf.GetOffset();
        pExt->m_nAddr = m_oCodeBuf.GetAddr();

        foreach (BBlock::EnumI, oEnum, pBBlock)
        {
            Instruction* const pI = oEnum.Get();
            pI->SetIndex(m_oCodeBuf.GetAddr());
            k_rgpfnProcessIFun[pI->GetOp()](this, pI);
        } // for each instruction
    } // processBBlock

    private: void processFunction(Function* const pFun)
    {
        m_pFun = pFun;

        // Allocate stack slot in frame for return address.
        pFun->m_cbFrame += sizeof(Val);

        FunExt* pExt = new(this) FunExt;
        pFun->SetWork(pExt);
        pExt->m_cbAnnots = 0;

        m_oCodeBuf.Reset();

        // Split bblock after PrologueI, since PrologueI is variable
        // length instruction. This is for GC Map consturction.
        if (m_pFun->NeedArity())
        {
            Instruction* const pPrologueI = pFun->GetPrologueI();
            if (! pPrologueI->GetNext()->Is<JumpI>())
            {
                pFun->GetStartBB()->SplitBefore(pPrologueI->GetNext());
            }
        }

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBBlock = oEnum.Get();
            BBlockExt* const pExt = new(this) BBlockExt;
            pBBlock->SetWork(pExt);
        } // for each bblock

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            processBBlock(oEnum.Get());
        } // for each bblock

        fixSpans();
    } // processFunction

    private: void processCopy(Instruction*);

    private: void processEmit(Instruction*, UINT nOp)
        { emitOp(nOp); }

    private: void processUnexpected(Instruction* pI)
    {
        CLOG(1, "<div class=e>Unexpected instruction ~S</div>", pI);
        C_INTERNAL_ERROR(pI->GetMnemonic());
    } // processUnexpected

    // [R]
    protected: void realizeFun(Function* const pFun)
    {
        Mm oMm;
        ByteSink oGcMap(&oMm);
        ComputeGcMap(&oMm, pFun, &oGcMap);

        FunExt* const pExt = pFun->GetWork<FunExt>();

        uint const cbAnnots = pExt->m_cbAnnots;
        uint const cbCode   = m_oCodeBuf.GetAddr();

        size_t const cbFunObjReal =
            sizeof(FunObj) +
            sizeof(FunObj::FunDesc) +
            RoundUp(cbCode, 4) +
            cbAnnots +
            oGcMap.GetPosn();

        size_t const cbFunObj = RoundUp(cbFunObjReal, FunObj::Align);

        Val const fn = Thread::Get()->AllocCode(
            pFun->IsClosure() ?
                CLASSD_native_code_closure :
                CLASSD_native_code_function,
            cbFunObj );

        FunObj* const pFunObj = fn->StaticCast<FunObj>();

        pFunObj->m_frob = pFun->GetName();

        serializeCode(pFunObj->GetCodeStart());

        FunObj::FunDesc* const pDesc = pFunObj->GetFunDesc();

        pDesc->m_cbCode = cbCode;

        pDesc->m_ofsAnnot = static_cast<uint32>(
            sizeof(FunObj) +
            RoundUp(cbCode, 4) );

        pDesc->m_ofsGcMap = pDesc->m_ofsAnnot + cbAnnots;

        pDesc->m_nFrame = pFun->m_cbFrame << 2;

        if (pFun->IsStackRestify())
        {
            pDesc->m_nFrame |= FunObj::FrameKind_Restify;
        }

        oGcMap.Serialize(pFunObj->GetGcMap());

        pFun->m_fn = fn;
    } // realizeFun

    ////////////////////////////////////////////////////////////
    //
    // Dispatch table
    //
    #define DefProcI_Arith(mp_Name, mp_NAME) \
        DefProcI(mp_Name) \
        { \
            parseArith(pI, \
                opext_ ## mp_NAME ## _Ev_Iz, \
                op_ ## mp_NAME ## SS_Vss_Wss, \
                op_ ## mp_NAME ## SD_Vsd_Wsd ); \
        } // DefProcI_Arith

    //= <FIXME date="2008-12-24" by="yosi@msn.com">
    //=  We must have parser for DIV and MUL.
    //= </FIXME>
    #define opext_DIV_Ev_Iz opext_ADC_Ev_Iz
    #define opext_MUL_Ev_Iz opext_ADC_Ev_Iz

    DefProcI(VarAnnex)  { processCopy(pI); }
    DefProcI(Assign)    { processCopy(pI); }
    DefProcI_Arith(Add, ADD)

    DefProcI(Box)       { processUnexpected(pI); } // must be lowered
    DefProcI(Branch);

    DefProcI(Call);
    DefProcI(Convert)   { processUnexpected(pI); }
    DefProcI(Copy)      { processCopy(pI); }

    DefProcI_Arith(Div, DIV)

    //DefProcI(Entry);
    DefProcI(Eq)        { processUnexpected(pI); } // must be lowered
    DefProcI(Exit);

    DefProcI(Ge)        { processUnexpected(pI); } // must be lowered
    DefProcI(Gt)        { processUnexpected(pI); } // must be lowered

    DefProcI(If);

    DefProcI(Jump);

    DefProcI(Le)        { processUnexpected(pI); } // must be lowered
    DefProcI(Load);
    DefProcI(LoadFun);
    DefProcI(LogAnd)    { parseIntArith(pI, opext_AND_Ev_Iz); }
    DefProcI(LogEqv)    { processUnexpected(pI); } // must be lowered
    DefProcI(LogIor)    { parseIntArith(pI, opext_OR_Ev_Iz); }
    DefProcI(LogXor)    { parseIntArith(pI, opext_XOR_Ev_Iz); }
    DefProcI(Lt)        { processUnexpected(pI); } // must be lowered

    DefProcI_Arith(Mul, MUL)

    DefProcI(Phi)       { processUnexpected(pI); }
    DefProcI(PhiCopy)   { processCopy(pI); }
    DefProcI(Prologue);

    DefProcI(Reload);
    DefProcI(Ret);

    DefProcI(Shl)        { parseShift(pI, opext_SHL_Ev_CL); }
    DefProcI(Shr);
    DefProcI(Spill)      { processCopy(pI); }
    DefProcI(StaticCast) { processCopy(pI); }
    DefProcI(Store);
    DefProcI_Arith(Sub, SUB)
    DefProcI(Swap);

    DefProcI(UnBox);
    DefProcI(UpVarBase);

    DefProcI(Values)    { processUnexpected(pI); }
    DefProcI(VarDef);
    DefProcI(VarRef);

    ////////////////////////////////////////////////////////////
    //
    // Dispatch table for x86 instructions
    //
    DefProcI(x86Clc)    { processEmit(pI, op_CLC); }
    DefProcI(x86Cmp)    { parseIntArith(pI, opext_CMP_Ev_Iz); }

    DefProcI(x86CmpF32)
        { emitVsWs(op_COMISS_Vss_Wss, pI->GetSx(), pI->GetSy()); }

    DefProcI(x86CmpF64)
        { emitVsWs(op_COMISD_Vsd_Wsd, pI->GetSx(), pI->GetSy()); }

    DefProcI(x86CvtFloat);
    DefProcI(x86CvtInt);

    DefProcI(x86Encode);

    DefProcI(x86Lea);

    DefProcI(x86Stc)    { processEmit(pI, op_STC); }

    DefProcI(x86Test);

    DefProcI(x86Zero);
}; // PassAssemble

#define THIS_PASS PassAssemble

DefProcI_(Branch)
{
    BranchI* pBranchI = pI->StaticCast<BranchI>();

    BBlock* pFalseBB = pBranchI->GetFalseBB();
    BBlock* pTrueBB  = pBranchI->GetTrueBB();

    Tttn eTttn = getTttn(pBranchI->GetBx());

    BBlock* pNextBB = pI->GetBB()->GetNext();
    if (pNextBB == pFalseBB)
    {
        emitJump(op_Jcc_Jb + eTttn, op_Jcc_Jv + eTttn, pTrueBB);
    }
    else if (pNextBB == pTrueBB)
    {
        eTttn = FlipTttn(eTttn);
        emitJump(op_Jcc_Jb + eTttn, op_Jcc_Jv + eTttn, pFalseBB);
    }
    else
    {
        emitJump(op_Jcc_Jb + eTttn, op_Jcc_Jv + eTttn, pTrueBB);
        emitJump(op_JMP_Jb, op_JMP_Jv, pFalseBB);
    }
} // Branch

struct ThreadSvcEntry
{
    Val m_name;
    int m_ofs;
}; // ThreadSvcEntry

ThreadSvcEntry const
k_rgoThreadSvc[] =
{
    { QPtype_error,             ThreadExtra_(TypeError) },
    { QPundefined_function,     ThreadExtra_(UndefinedFunction) },
}; // k_rgoThreadSvc

DefProcI_(Call)
{
    ASSERT(pI->GetSy() == Void);

    Operand* const pSx = pI->GetSx();

    if (FunName* const pCallee = pSx->DynamicCast<FunName>())
    {
        emitOp(op_CALL_Jv);
        emitLit(pCallee->GetName(), Annot::Kind_NamedCallee);
    }
    else if (Literal* const pCallee = pSx->DynamicCast<Literal>())
    {
        for (
            const ThreadSvcEntry* p = k_rgoThreadSvc;
            p < &k_rgoThreadSvc[lengthof(k_rgoThreadSvc)];
            p++ )
        {
            if (pCallee->GetDatum() == p->m_name)
            {
                emitOp(op_CALL_Ev);
                emitDisp(opext_CALL_Ev, $tcb, p->m_ofs);
                return;
            }
        } // for

        emitOp(op_CALL_Jv);
        emitLit(pCallee->GetDatum(), Annot::Kind_NamedCallee);
    }
    else if (Function* const pCallee = pSx->DynamicCast<Function>())
    {
        emitOp(op_CALL_Jv);
        emitLit(Fixnum::Encode(pCallee), Annot::Kind_LocalCallee);
    }
    else
    {
        C_INTERNAL_ERROR("asm.CALL unsupported callee");
    }
} // Call

DefProcI_(Exit)
{
    ASSERT(NULL != pI);
    if (nil == m_pFun->GetPrologueI()->GetLx()) return;
    if (! m_pFun->NeedArity()) return;

    emitOp(op_CALL_Ev);
    emitDisp(opext_CALL_Ev, $tcb, ThreadExtra_(ArityError));
} // Exit

DefProcI_(If)
{
    if (pI->GetOutput() != pI->GetSy())
    {
        Context::Get()->Error("Invalid IF instruction.");
        return;
    }

    if (pI->GetSy() == pI->GetSz())
    {
        C_INTERNAL_ERROR("asm.IF Bad operands");
        return;
    }

    Tttn const eTttn = FlipTttn(getTttn(pI->GetBx()));
    emitOp(op_CMOVcc_Gv_Ev + eTttn);
    emitEv(mapGpr(pI->GetOutput()), pI->GetSz());
} // If

DefProcI_(Jump)
{
    JumpI* pJumpI = pI->StaticCast<JumpI>();
    BBlock* pTargetBB = pJumpI->GetTargetBB();
    when (pTargetBB == pI->GetBB()->GetNext()) return;
    emitJump(op_JMP_Jb, op_JMP_Jv, pTargetBB);
} // Jump

DefProcI_(Load)
{
    Ea oEa = getEa(pI->GetSx());
    emitOp(op_MOV_Gv_Ev);
    emitEa(&oEa, mapGpr(pI->GetOutput()));
    return;
} // Load

DefProcI_(LoadFun)
{
    Val cell;
    if (Literal* pLx = pI->GetSx()->DynamicCast<Literal>())
    {
        cell = pLx->GetDatum();
    }
    else if (FunName* pFunName = pI->GetSx()->DynamicCast<FunName>())
    {
        cell = pFunName->GetName();
    }
    else
    {
        processUnexpected(pI);
        return;
    }

    Reg const rd = mapGpr(pI->GetOutput());

    if (symbolp(cell))
    {
        emitOp(op_MOV_Gv_Ev);
        emitU8(ModRm(Mod_Disp0, rd, Rm_Disp32));
        emitLit(cell, Annot::Kind_SymFun);
    }
    else if (cell->Is<SetfCell>())
    {
        emitOp(op_MOV_Gv_Ev);
        emitU8(ModRm(Mod_Disp0, rd, Rm_Disp32));
        emitLit(cell, Annot::Kind_SymSetf);
    }
    else
    {
        C_INTERNAL_ERROR("LoadFun: invalid cell");
    }
} // LoadFun

void PassAssemble::processCopy(Instruction* pI)
{
    Output*  const pOd = pI->GetOutput();
    Operand* const pSx = pI->GetSx();

    if (Physical* pRd = pOd->DynamicCast<Physical>())
    {
        Reg const rd = mapGpr(pRd);

        if (Literal* pLx = pSx->DynamicCast<Literal>())
        {
            if (pLx->GetDatum() == zero &&
                ! pI->GetNext()->Is<IfI>() )
            {
                emitOp(op_XOR_Ev_Gv);
                emitU8(ModRm(Mod_Reg, rd, rd));
                return;
            }
        } // if

        asmMov(rd, pSx);
    }
    else if (StackSlot* pMd = pOd->DynamicCast<StackSlot>())
    {
        asmMov($sp, pMd->GetLocation(), pSx);
    }
    else if (ThreadSlot* pMd = pOd->DynamicCast<ThreadSlot>())
    {
        asmMov($tcb, pMd->GetLocation(), pSx);
    }
    else
    {
        C_INTERNAL_ERROR("Copy: invalid operand");
    }
} // processCopy

DefProcI_(Prologue)
{
    Val max = Fixnum::Encode(m_pFun->GetArity()->m_iMax);
    Val min = Fixnum::Encode(m_pFun->GetArity()->m_iMin);

    if (m_pFun->NeedArity())
    {
        BBlock* pExitBB = m_pFun->GetExitBB();

        if (m_pFun->GetArity()->m_fRest)
        {
            if (zero != min)
            {
                parseIntArith(opext_CMP_Ev_Iz, getGpr($rn), Literal::New(min));
                emitJump(op_JL_Jb, op_JL_Jv, pExitBB);
            }
        }
        else if (min == max)
        {
            parseIntArith(opext_CMP_Ev_Iz, getGpr($rn), Literal::New(min));
            emitJump(op_JNE_Jb, op_JNE_Jv, pExitBB);
        }
        else
        {
            parseIntArith(opext_CMP_Ev_Iz, getGpr($rn), Literal::New(min));
            emitJump(op_JL_Jb, op_JL_Jv, pExitBB);

            parseIntArith(opext_CMP_Ev_Iz, getGpr($rn), Literal::New(max));
            emitJump(op_JG_Jb, op_JG_Jv, pExitBB);
        }
    } // if need arity

    // Restify
    Val const restify = pI->GetLy();
    if (nil != restify)
    {
        asmMov($tcb, offsetof(Thread, m_fn), Literal::New(max));
        emitOp(op_CALL_Jv);
        emitLit(restify, Annot::Kind_NamedCallee);
    }

    int const cbFrame = m_pFun->m_cbFrame - 4;
    ASSERT(0 == cbFrame % sizeof(Val));

    if (cbFrame > 0)
    {
        processAdjustFrame(opext_SUB_Ev_Ib, opext_SUB_Ev_Iz, cbFrame);
    } // if
} // Prologue

DefProcI_(Ret)
{
    class Local
    {
        public: static bool NeedCLC(Instruction* const pRefI)
        {
            for (Instruction* pI = pRefI; NULL != pI; pI = pI->GetPrev())
            {
                if (pI->Is<CallI>())
                {
                    if (FunName* const pFunName =
                            pI->GetSx()->DynamicCast<FunName>() )
                    {
                        const TyFunction* const pFunty = pFunName->GetFunty();
                        const Type* const pty = pFunty->GetValueTy();
                        return pty->Is<TyValues>() || pty->Is<TyUndef>();
                    }
                    break;
                }

                if (pI->Is<LogAndI>() ||
                    pI->Is<LogEqvI>() ||
                    pI->Is<LogIorI>() ||
                    pI->Is<LogXorI>() )
                {
                    return false;
                }

                if (pI->Is<x86TestI>())
                {
                    return false;
                }
            } // for pI

            return true;
        } // NeedCLC
    }; // Local

    if (pI->Equal(pI->GetBB()->GetNext()->GetFirstI()))
    {
        // Share RET instruction with next bblock.
        return;
    }

    int cbFrame = m_pFun->m_cbFrame - 4;
    ASSERT(0 == cbFrame % sizeof(Val));

    Val kind = pI->GetSx() == Void ? nil : pI->GetLx();

    if (m_pFun->IsStackRestify())
    {
        if (one == kind || nil == kind)
        {
            if (Local::NeedCLC(pI))
            {
                emitOp(op_CLC);
            }
        }

        asmMov($sp, $sp, cbFrame);
    }
    else
    {
        if (one == kind || nil == kind)
        {
            if (cbFrame > 0)
            {
                processAdjustFrame(opext_ADD_Ev_Ib, opext_ADD_Ev_Iz, cbFrame);
            }
            else
            {
                if (Local::NeedCLC(pI))
                {
                    emitOp(op_CLC);
                }
            }
        }
        else if (Qvalues == kind)
        {
            if (cbFrame > 0)
            {
                emitOp(op_LEA_Gv_M);
                emitDisp($sp, $sp, cbFrame);
            }
        }
        else
        {
            C_INTERNAL_ERROR("Ret: invalid operand");
        }
    } // if

    //= <FIXME date="2008-12-25" by="yosi@msn.com">
    //=   NYI: Tail call optimization:
    //=     CALL+RET = JMP
    //= </FIXME>

    emitOp(op_RET);
} // Ret

DefProcI_(Reload)
{
    StackSlot* pMx = pI->GetSx()->StaticCast<StackSlot>();
    asmMov(
        mapGpr(pI->GetOutput()),
        $sp,
        pMx->GetLocation() );
} // Reload

DefProcI_(Shr)
{
    const Type* pty = pI->GetTy();

    parseShift(
        pI,
        pty == tyUInt || pty == tyUInt32 ?
            opext_SHR_Ev_1 :
            opext_SAR_Ev_1 );
} // Shr

DefProcI_(Store)
{
    Ea oEa = getEa(pI->GetSx());
    if (Physical* pRy = pI->GetSy()->DynamicCast<Physical>())
    {
        Reg ry = mapGpr(pRy);

        if (Ea::Mode_Annot == oEa.m_eMode &&
            Annot::Kind_TlvOfs != oEa.m_eAnnot &&
            $EAX == ry )
        {
            emitOp(op_MOV_Ov_eAX);
            emitLit(oEa.m_datum, oEa.m_eAnnot);
            return;
        }

        emitOp(op_MOV_Ev_Gv);
        emitEa(&oEa, ry);
        return;
    }

    emitOp(op_MOV_Ev_Iz);
    emitEa(&oEa, opext_MOV_Ev_Iz);
    emitIz(pI->GetSy());
} // Store

DefProcI_(Swap)
{
    emitOp(op_XCHG_Ev_Gv);
    emitU8(ModRm(
        Mod_Reg,
        mapGpr(pI->GetOutput()),
        mapGpr(pI->GetSx()->StaticCast<Physical>()) ) );
} // Swap

DefProcI_(UnBox)
{
    if (pI->GetTy() == tyFloat32)
    {
        Reg const rd = mapFpr(pI->GetOutput());
        Reg const rx = mapGpr(pI->GetSx());

        emitOp(op_MOVSS_Vss_Wss);
        emitDisp(rd, rx, offsetof(Layout_single_float, m_flt) - Record::Tag);
        return;
    } // float32

    if (pI->GetTy() == tyFloat64)
    {
        Reg const rd = mapFpr(pI->GetOutput());
        Reg const rx = mapGpr(pI->GetSx());

        emitOp(op_MOVSD_Vsd_Wsd);
        emitDisp(rd, rx, offsetof(Layout_double_float, m_dbl) - Record::Tag);
        return;
    } // float64

    processUnexpected(pI);
} // Unbox

DefProcI_(UpVarBase)
{
    int const ofs = computeFixedPathOffset(
        pI->GetBB()->GetFunction(),
        pI->GetSx()->StaticCast<Function>() );

    emitOp(op_LEA_Gv_M);
    emitDisp(mapGpr(pI->GetOutput()), $sp, ofs);
} // UpVarBaseI

DefProcI_(VarDef)
{
    Variable* const pVar = pI->GetSx()->StaticCast<Variable>();

    switch (pVar->GetStorage())
    {
    case Variable::Storage_Closed:
    case Variable::Storage_Literal:
        break;
        
    case Variable::Storage_Stack:
        if (! pVar->HasLocation())
        {
            C_INTERNAL_ERROR("VarDef: Variable isn't allocated.");
        }
        break;
        
    default:
        COMPILER_INTERNAL_ERROR();
        break;
    } // switch storage
} // VarDef

DefProcI_(VarRef)
{
    VarDefI const* pVarDefI = pI->GetQx()->GetDefI()->StaticCast<VarDefI>();
    Variable* const pVar = pVarDefI->GetSx()->StaticCast<Variable>();
    ASSERT(Variable::Storage_Stack == pVar->GetStorage());

    if (! pVar->HasLocation())
    {
        COMPILER_INTERNAL_ERROR();
        return;
    }

    emitOp(op_LEA_Gv_M);
    Ea oEa($sp, pVar->GetLocation());
    emitEa(&oEa, mapGpr(pI->GetOutput()));
} // VarRef

DefProcI_(x86CvtFloat)
{
    if (pI->GetTy() == tyFloat32)
    {
        // float32 <= float64
        emitVsWs(op_CVTSD2SS_Vss_Wsd, pI->GetOutput(), pI->GetSx());
        return;
    } // float32

    if (pI->GetTy() == tyFloat64)
    {
        // float64 <= float32
        emitVsWs(op_CVTSS2SD_Vsd_Wss, pI->GetOutput(), pI->GetSx());
        return;
    } // float64

    processUnexpected(pI);
} // x86CvtFloat

DefProcI_(x86CvtInt)
{
    if (pI->GetTy() == tyFloat32)
    {
        // float32 <= int
        emitVsEv(op_CVTSI2SS_Vss_Ed, pI->GetOutput(), pI->GetSx());
        return;
    } // float32

    if (pI->GetTy() == tyFloat64)
    {
        // float64 <= int
        emitVsEv(op_CVTSI2SD_Vsd_Ed, pI->GetOutput(), pI->GetSx());
        return;
    } // float64

    processUnexpected(pI);
} // x86CvtInt

DefProcI_(x86Encode)
{
    if (pI->GetTy() == tyFloat32)
    {
        // Copy   uint32  %r1 <= u32
        // Encode float32 %f2 <= %r1
        Reg const rd = mapFpr(pI->GetOutput()->StaticCast<Physical>());
        Reg const rx = mapGpr(pI->GetSx()->StaticCast<Physical>());
        emitOp(op_MOVD_Vdq_Ed);
        emitU8(ModRm(Mod_Reg, rd, rx));
        return;
    } // float32

    if (pI->GetTy() == tyFloat64)
    {
        // Copy   uint32  %r1 <= u32
        // Encode float64 %f2 <= %r1
        Reg const rd = mapFpr(pI->GetOutput()->StaticCast<Physical>());
        Reg const rx = mapGpr(pI->GetSx()->StaticCast<Physical>());

        emitOp(op_MOVD_Vdq_Ed);
        emitU8(ModRm(Mod_Reg, rd, rx));

        emitOp(op_PSLLQ_Udq_Ib);
        emitU8(ModRm(Mod_Reg, opext_PSLLQ_Udq_Ib, rd));
        emitU8(32);
        return;
    } // float64

    processUnexpected(pI);
} // x86Encode

DefProcI_(x86Lea)
{
    if (NULL != pI->GetQd())
    {
        return;
    }

    Ea oEa = getEa(pI);
    emitOp(op_LEA_Gv_M);
    emitEa(&oEa, mapGpr(pI->GetOutput()));
} // x86Lea

DefProcI_(x86Test)
{
    Operand* pSx = pI->GetSx();
    Operand* pSy = pI->GetSy();

    Int iIb = getIb(pSy);

    if (0 == (iIb & ~0xFF))
    {
        if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            int iGpr = mapGpr(pRx) & 7;
            if (0 == iGpr)
            {
                emitOp(op_TEST_AL_Ib);
                emitU8(static_cast<uint8>(iIb));
                return;
            }

            if (iGpr >= 4)
            {
                emitOp(op_TEST_Ev_Iz);
                emitEv(opext_TEST_Ev_Iz, pSx);
                emitU32(static_cast<uint32>(iIb));
                return;
            }
        } // if physical

        emitOp(op_TEST_Eb_Ib);
        emitEv(opext_TEST_Eb_Ib, pSx);
        emitU8(static_cast<uint8>(iIb));
        return;
    } // if Ib

    if (Physical* pRy = pSy->DynamicCast<Physical>())
    {
        emitOp(op_TEST_Ev_Gv);
        emitEv(mapGpr(pRy), pSx);
        return;
    }
    else if (Immediate* pIy = pSy->DynamicCast<Immediate>())
    {
        if (Physical* pRx = pSx->DynamicCast<Physical>())
        {
            if (mapGpr(pRx) == 0)
            {
                emitOp(op_TEST_eAX_Iz);
                emitIz(pIy);
                return;
            }
        }

        emitOp(op_TEST_Ev_Iz);
        emitEv(opext_TEST_Ev_Iz, pSx);
        emitIz(pIy);
        return;
    } // if

    COMPILER_INTERNAL_ERROR();
} // x86Test

DefProcI_(x86Zero)
{
    if (pI->GetTy() == tyFloat32)
    {
        Reg const rd = mapFpr(pI->GetOutput());
        emitOp(op_XORPS_Vps_Wps);
        emitU8(ModRm(Mod_Reg, rd, rd));
        return;
    } // if float32

    if (pI->GetTy() == tyFloat64)
    {
        Reg const rd = mapFpr(pI->GetOutput());
        emitOp(op_XORPD_Vpd_Wpd);
        emitU8(ModRm(Mod_Reg, rd, rd));
        return;
    } // if float64

    processUnexpected(pI);
} // x86Zero

DEFPASS(Assemble)

} // Compiler

} // TinyCl
