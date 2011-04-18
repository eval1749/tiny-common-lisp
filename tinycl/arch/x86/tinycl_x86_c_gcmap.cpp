#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 GC Map Constructor
// tinycl_x86_c_gcmap.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_gcmap.cpp#5 $
//
#include "../../compiler/ir/tinycl_c_ir_dfa.h"
#include "../../compiler/cg/tinycl_c_cg_sink.h"
#include "./tinycl_x86_c_cg.h"
#include "./tinycl_x86_ke_gcmap.h"

namespace TinyCl
{

namespace Compiler
{

namespace GcMapCtor
{

class Constructor
{
    protected: Function*       const m_pFun;
    protected: const RegGroup* const m_pRegGroup;
    protected: const RegSet*   const m_pRegAll;

    // ctor
    protected: Constructor(
        Function*       const pFun,
        const RegGroup* const pRegGroup) :
            m_pFun(pFun),
            m_pRegGroup(pRegGroup),
            m_pRegAll(pRegGroup->m_pAll) {}

    private: Constructor& operator=(Constructor&)
        { CAN_NOT_HAPPEN(); }

    // [D]
    protected: void dumpLiveness(const BitVec* const pLive)
    {
        bool fLive = false;

        foreach (RegSet::Enum, oEnum, m_pRegAll)
        {
            if (pLive->IsOne(oEnum.Get()->m_nIndex + 1))
            {
                CLOG(1, "<li>live ~A</li>", oEnum.Get()->m_pszName);
                fLive = true;
            }
        } // for

        int const cBits = pLive->GetLength();

        int ofs = 0;
        for (int k = m_pRegAll->m_c + 1; k < cBits; k++)
        {
            if (pLive->IsOne(k))
            {
                CLOG(1, "<li>live [$sp+~D]</li>", ofs);
                fLive = true;
            }
            ofs += sizeof(Val);
        } // for k

        if (! fLive)
        {
            CLOG(1, "<li><i>Nothing</i></li>");
        }
    } // dumpLiveness

    // [F]
    private: Instruction* findVarAneex(Variable* const pVar)
    {
        if (Pseudo* const pQd = m_pFun->FindUpVar(pVar))
        {
            foreach (Pseudo::EnumUser, oEnum, pQd)
            {
                if (Instruction* const pVarAneexI =
                        oEnum.GetI()->DynamicCast<VarAnnexI>() )
                {
                    return pVarAneexI;
                }
            } // for
        }
        return NULL;
    } // findVarAneex

    // [M]
    private: int mapOperandToIndex(Operand* const pSx)
    {
        if (Physical* const pPx = pSx->DynamicCast<Physical>())
        {
            const RegDesc* const pDesc = pPx->GetDesc();
            if (pDesc->m_eRegClass == RegClass_Gpr)
            {
                ASSERT(pDesc->m_nIndex + 1 <= (uint) m_pRegAll->m_c);
                return pDesc->m_nIndex + 1;
            }
        }
        else if (StackSlot* pMx = pSx->DynamicCast<StackSlot>())
        {
            return mapStackToIndex(pMx->GetLocation());
        }

        return 0;
    } // mapOperandToIndex

    protected: int mapStackToIndex(int ofs)
    {
        return ofs / sizeof(Val) + m_pRegAll->m_c + 1;
    } // mapStackToIndex

    // [N]
    protected: static bool needGc(Instruction* const pI)
    {
        const Type* pty = pI->GetTy();

        if (pty->IsForeign())
        {
            return false;
        }

        #if 0
            if (pty == tyFixnum)
            {
                return false;
            }

            if (pty == tyCharacter)
            {
                return false;
            }
        #endif

        return true;
    } // needGc

    // [P]
    protected: void processCallee(Function* const pCallee)
    {
        if (m_pFun->IsClosure())
        {
            foreach (Function::EnumUpVar, oEnum, pCallee)
            {
                Variable* const pVar = oEnum.Get();
                if (pVar->GetOwner() == m_pFun)
                {
                    processUseVar(pVar);
                }
                else if (Instruction* const pVarAnnexI = findVarAneex(pVar))
                {
                    StackSlot* const pMd = pVarAnnexI->GetOutput()->
                        StaticCast<StackSlot>();

                    CLOG(1, "<li>use ~S</li>", pVarAnnexI);
                    processUse(mapStackToIndex(pMd->GetLocation()));
                }
                else
                {
                    COMPILER_INTERNAL_ERROR();
                }
            } // for
        }
        else
        {
            foreach (Function::EnumUpVar, oEnum, pCallee)
            {
                Variable* const pVar = oEnum.Get();
                if (pVar->GetOwner() == m_pFun)
                {
                    processUseVar(pVar);
                }
            } // for
        }
    } // processCallee

    protected: virtual void processKill(int const) = 0;
    protected: virtual void processUse(int const) = 0;

    private: void processUseVar(Variable* const pVar)
    {
        CLOG(1, "<li>use ~S</li>", pVar);
        processUse(mapStackToIndex(pVar->GetLocation()));
    } // processUseVar

    protected: void processOperands(Instruction* const pI)
    {
        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            if (int const k = mapOperandToIndex(oEnum.Get()))
            {
                CLOG(1, "<li>use ~S</li>", oEnum.Get());
                processUse(k);
            }
        } // for operand
    } // processOperands

    protected: void processOutput(Instruction* const pI)
    {
        if (int const k = mapOperandToIndex(pI->GetOutput()))
        {
            CLOG(1, "<li>kill ~S</li>", pI->GetOutput());
            processKill(k);
        }
    } // processOutput

    protected: void processUseI(Instruction* const pI)
    {
        if (FrameReg* const pFx = pI->GetSx()->DynamicCast<FrameReg>())
        {
            if (OpenFinallyI* const pOpenI =
                    pFx->GetDefI()->DynamicCast<OpenFinallyI>() )
            {
                processCallee(pOpenI->GetSx()->StaticCast<Function>());
            } // if OpenFinally
        } // if
    } // Use
}; // Constructor

class SubPassCompute :
    public Pass_<SubPassCompute, SubPass>,
    private Constructor,
    private InstructionDispatcher
{
    public: static const char* GetName_() { return "GcMap.Compute"; }

    private: DataFlowBB* m_pBB;
 
    public: SubPassCompute(
        Function*       const pFun,
        const RegGroup* const pRegGroup ) :
            Constructor(pFun, pRegGroup) {}

    // Entry point
    public: static void Run(
        Function*       const pFun,
        const RegGroup* const pRegGroup )
    {
        SubPassCompute oPass(pFun, pRegGroup);
        oPass.run();
    } // Run

    // [C]
    private: int computeArity(Instruction* const pI)
    {
        if (pI->GetNext()->Is<UnreachableI>())
        {
            // We don't care liveness for error report function.
            return 0;
        }

        if (Instruction* pSetArityI = isSetArityI(pI->GetPrev()))
        {
            return min(
                Fixnum::Decode_(pSetArityI->GetLx()),
                m_pRegGroup->m_pArgs->m_c );
        }

        // Assume many arguments
        return m_pRegGroup->m_pArgs->m_c;
    } // computeArity

    private: void computeLocal()
    {
        uint const cSlots = (m_pFun->m_cbFrame - sizeof(Val)) / sizeof(Val);
        uint const cBits  = m_pRegAll->m_c + cSlots + 1;

        foreach (Function::EnumBBlock, oEnum, m_pFun)
        {
            DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
            CLOG_SECTION(1, "compute ~S", pBB);
            pBB->InitDataFlow(cBits);
            m_pBB = pBB;
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();
                CLOG_SECTION(1, "~D ~S", pI->GetIndex(), pI);
                dispatch(pI);
            } // for i

            {
                CLOG_SECTION(1, "LiveIn ~S", pBB);
                dumpLiveness(pBB->GetIn());
            }

            {
                CLOG_SECTION(1, "VarKill ~S", pBB);
                dumpLiveness(pBB->GetKill());
            }
        } // for bb
    } // computeLocal

    public: Instruction* isSetArityI(Instruction* const pI)
    {
        if (NULL == pI)
        {
            return NULL;
        }

        if (! pI->Is<CopyI>())
        {
            return NULL;
        }

        Physical* const pPd =
            pI->GetOutput()->DynamicCast<Physical>();

        if (NULL == pPd)
        {
            return NULL;
        }

        if (pPd->GetDesc() != m_pRegGroup->m_pRn)
        {
            return NULL;
        }

        return pI;
    } // isSetArityI

    // [P]
    protected: override void processKill(int const k)
    {
        m_pBB->SetKill(k);
    } // processKill

    protected: override void processUse(int const k)
    {
        if (! m_pBB->IsKill(k))
        {
            m_pBB->SetIn(k);
        }
    } // processOperand

    // [R]
    private: void run()
    {
        CLOG_SECTION(1, "<b>Compute</b>");
        computeLocal();
        Dfa::SolveBackward(m_pFun);
    } // run

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //
    private: override void processDefault(Instruction* pI)
    {
        if (needGc(pI))
        {
            processOperands(pI);
        }

        processOutput(pI);
    } // processDefault

    //= <FIXME date="2009-01-25" by="yosi@msn.com">
    //=   How do we handle upvar if m_pFun is closure?
    //= </FIXME>
    DefProcI(Call)
    {
        if (Function* const pCallee = pI->GetSx()->DynamicCast<Function>())
        {
            processCallee(pCallee);
        }
        else
        {
            int cArgs = computeArity(pI);
            CLOG(1, "<li>cArgs=~D</li>", cArgs);
            foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pArgs)
            {
                if (0 == cArgs)
                {
                    break;
                }
                cArgs -= 1;
                processUse(oEnum.Get()->m_nIndex + 1);
            } // for
        } // if

        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pCallerSave)
        {
            processKill(oEnum.Get()->m_nIndex + 1);
        } // for
    } // Call

    DefProcI(Prologue)
    {
        ASSERT(NULL != pI);
        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pAll)
        {
            processKill(oEnum.Get()->m_nIndex + 1);
        } // for
    } // Prologue

    DefProcI(Use) { processUseI(pI); }
}; // SubPassCompute

class Sink
{
    private: class GcDesc :
        public DoubleLinkedItem_<GcDesc>,
        public LocalObject
    {
        private: uint           m_cArgs;
        private: GcMap::Kind    m_eKind;
        private: uint           m_ofs;
        private: const BitVec*  m_pBitVec;

        public: GcDesc(
            uint            const ofs,
            GcMap::Kind     const eKind,
            const BitVec*   const pBitVec,
            uint            const cArgs ) :
                m_cArgs(cArgs),
                m_eKind(eKind),
                m_ofs(ofs),
                m_pBitVec(pBitVec) {}

        public: bool Equal(const GcDesc* const that) const
        {
            return
                this->m_cArgs == that->m_cArgs &&
                this->m_eKind == that->m_eKind &&
                this->m_pBitVec->Equal(that->m_pBitVec);
        } // Equal

        public: uint GetOfs() const
            { return m_ofs; }

        public: uint Hash() const
        {
            uint nHashCode = RotateLeft(m_pBitVec->Hash(), 5);
            nHashCode ^= m_eKind;
            nHashCode = RotateLeft(m_pBitVec->Hash(), 8);
            nHashCode ^= m_cArgs;
            return nHashCode;
        } // Hash

        public: void Serialize(
            BitSink_<16>*   const pBits,
            const RegGroup* const pRegGroup) const
        {
            pBits->Write1(0);
            pBits->Write1(m_eKind & 1);
            pBits->Write1((m_eKind >> 1) & 1);

            switch (m_eKind)
            {
            case GcMap::JumpDesc:
                foreach (RegSet::Enum, oEnum, pRegGroup->m_pAllocable)
                {
                    int const k = oEnum.Get()->m_nIndex + 1;
                    pBits->Write1(m_pBitVec->IsOne(k));
                } // for k
                break;

            case GcMap::StdCallDesc:
                break;

            case GcMap::CallDesc2:
                pBits->Write1((m_cArgs >> 0) & 1);
                pBits->Write1((m_cArgs >> 1) & 1);
                pBits->Write1((m_cArgs >> 2) & 1);
                break;

            case GcMap::CallDesc3:
                pBits->Write1((m_cArgs >> 0) & 1);
                pBits->Write1((m_cArgs >> 1) & 1);
                pBits->Write1((m_cArgs >> 2) & 1);
                pBits->Write1((m_cArgs >> 3) & 1);
                pBits->Write1((m_cArgs >> 4) & 1);
                pBits->Write1((m_cArgs >> 5) & 1);
                pBits->Write1((m_cArgs >> 6) & 1);
                pBits->Write1((m_cArgs >> 7) & 1);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch eKind

            int const nEnd = m_pBitVec->FindLastOne();
            for (int k = pRegGroup->m_pAll->m_c + 1; k <= nEnd; k += 1)
            {
                if (pBits->IsStartOfWord())
                {
                    pBits->Logior(1);
                    pBits->Flush();
                    pBits->Write1(0);
                }

                pBits->Write1(m_pBitVec->IsOne(k));
            } // for k

            pBits->Flush();
        } // Serialize
    }; // GcDesc

    private: typedef DoubleLinkedList_<GcDesc> GcDescList;

    private: struct Entry :
        public DoubleLinkedItem_<Entry>,
        public LocalObject
    {
        int     m_ofs;
        GcDesc* m_pDesc;

        Entry(int ofs, GcDesc* pDesc) :
            m_ofs(ofs),
            m_pDesc(pDesc) {}
    }; // Entry

    private: typedef DoubleLinkedList_<Entry> Entries;

    private: struct Segment :
        public DoubleLinkedItem_<Segment>,
        public LocalObject
    {
        enum { Size = 1 << 16 };

        int     m_ofs;
        Entries m_oEntries;

        Segment(int ofs) :
            m_ofs(MapToSegment(ofs)) {}

        int GetEnd()   const { return m_ofs + Size; }
        int GetStart() const { return m_ofs; }

        bool In(const Entry* const pEntry)
        {
            return pEntry->m_ofs >= GetStart() &&
                   pEntry->m_ofs <  GetEnd();
        } // In

        public: static int MapToSegment(int nAddr)
        {
            return (nAddr / Size) * Size;
        } // MapToSegment
    }; // Segment

    private: class GcDescTable
    {
        private: enum { Length = 103 };

        private: struct Slot
        {
            GcDescList  m_oGcDescList;
        };

        private: uint m_nCount;
        private: Slot m_rgoSlot[Length];

        public: GcDescTable() :
            m_nCount(0) {}

        public: GcDesc* Get(
            GcMap::Kind     const eKind,
            const BitVec*   const pBitVec,
            uint            const cArgs )
        {
            GcDesc oGcDesc(0, eKind, pBitVec, cArgs);

            uint const nIndex = oGcDesc.Hash() % Length;
            foreach (
                GcDescList::Enum,
                oEnum,
                &m_rgoSlot[nIndex].m_oGcDescList )
            {
                if (oEnum.Get()->Equal(&oGcDesc))
                {
                    return oEnum.Get();
                }
            } // for

            return NULL;
        } // Get

        public: void Put(GcDesc* const pGcDesc)
        {
            uint const nIndex = pGcDesc->Hash() % Length;
            m_rgoSlot[nIndex].m_oGcDescList.Append(pGcDesc);
            m_nCount += 1;
        } // Put
    }; // GcDescTable

    private: typedef DoubleLinkedList_<Segment> Segments;

    private: BitSink_<16>          m_oBits;
    private: GcDescTable           m_oGcDescTable;
    private: Segments              m_oSegments;
    private: IMm*            const m_pIMm;
    private: const RegGroup* const m_pRegGroup;

    // ctor
    public: Sink(
        IMm*            const pIMm,
        const RegGroup* const pRegGroup ) :
            m_oBits(new(pIMm) ByteSink(pIMm)),
            m_pIMm(pIMm),
            m_pRegGroup(pRegGroup) {}

    private: Sink& operator=(Sink&)
        { CAN_NOT_HAPPEN(); }

    public: void AddEntry(
        GcMap::Kind     const eKind,
        Instruction*    const pI,
        int             const ofsInsn = 0,
        int             const cArgs = 0 )
    {
        uint const nAddr = pI->GetIndex() + ofsInsn;

        static const char* const k_rgpszDesc[] =
        {
            "Jump",
            "Call",
            "Call2",
            "Call3",
        }; // k_rgpszDesc

        CLOG_SECTION(1, "~D ~A ~S", nAddr, k_rgpszDesc[eKind], pI);

        const BitVec* const pLive =
            pI->GetBB()->Extend<DataFlowBB>()->GetOut();

        DumpLiveness(pLive);

        GcDesc* const pGcDesc = internGcDesc(eKind, pI, cArgs);

        Entry*  const pEntry = new(m_pIMm) Entry(
            pI->GetIndex() + ofsInsn,
            pGcDesc );

        Segment* pSegment = NULL;
        foreach (Segments::Enum, oEnum, &m_oSegments)
        {
            if (oEnum.Get()->In(pEntry))
            {
                pSegment = oEnum.Get();
                break;
            }
        } // for

        if (NULL == pSegment)
        {
            pSegment = new(m_pIMm) Segment(Segment::MapToSegment(nAddr));
            m_oSegments.Prepend(pSegment);
        }

        pSegment->m_oEntries.Prepend(pEntry);
    } // AddEntry

    // [D]
    public: void DumpLiveness(const BitVec* const pLive)
    {
        bool fLive = false;

        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pAll)
        {
            if (pLive->IsOne(oEnum.Get()->m_nIndex + 1))
            {
                CLOG(1, "<li>live ~A</li>", oEnum.Get()->m_pszName);
                fLive = true;
            }
        } // for

        int const cBits = pLive->GetLength();

        int ofs = 0;
        for (int k = m_pRegGroup->m_pAll->m_c + 1; k < cBits; k++)
        {
            if (pLive->IsOne(k))
            {
                CLOG(1, "<li>live [$sp+~D]</li>", ofs);
                fLive = true;
            }
            ofs += sizeof(Val);
        } // for k

        if (! fLive)
        {
            CLOG(1, "<li><i>Nothing</i></li>");
        }
    } // DumpLiveness

    // [I]
    private: GcDesc* internGcDesc(
        GcMap::Kind     const eKind,
        Instruction*    const pI,
        int             const cArgs )
    {
        const BitVec* const pLive =
            pI->GetBB()->Extend<DataFlowBB>()->GetOut();

        if (GcDesc* const pPresent = m_oGcDescTable.Get(eKind, pLive, cArgs))
        {
            return pPresent;
        }

        GcDesc* const pGcDesc = new(m_pIMm) GcDesc(
            m_oBits.GetPosn(),
            eKind,
            pLive,
            cArgs );

        m_oGcDescTable.Put(pGcDesc);

        pGcDesc->Serialize(&m_oBits, m_pRegGroup);

        return pGcDesc;
    } // internGcDesc

    // [S]
    public: void Serialize(
        int       const cbCode,
        ByteSink* const pByteSink )
    {
        CLOG_SECTION(2, "<b>Serialize cbCode=~D</b>", cbCode);

        int ofs = Ceiling(cbCode, Segment::Size) * sizeof(uint16);

        {
            CLOG_SECTION(2, "~D - Segment Table", pByteSink->GetPosn());

            Segments::Enum oEnum(&m_oSegments);

            int const ofsCodeEnd = RoundUp(cbCode, Segment::Size);

            for (
                int ofsCode = 0;
                ofsCode < ofsCodeEnd;
                ofsCode += Segment::Size )
            {
                if (oEnum.AtEnd() || ofsCode != oEnum.Get()->m_ofs)
                {
                    CLOG(2, "<li>~D - Segment[~D] 0</li>",
                        pByteSink->GetPosn(),
                        ofsCode / Segment::Size );

                    pByteSink->EmitU16(0);
                }
                else
                {
                    const Segment* const pSegment = oEnum.Get();
                    oEnum.Next();

                    CLOG(2, "<li>~D - Segment[~D] entry=~D</li>",
                        pByteSink->GetPosn(),
                        ofsCode / Segment::Size, ofs );

                    pByteSink->EmitU16(static_cast<uint16>(ofs >> 1));

                    int cEntries = pSegment->m_oEntries.Count();
                    ofs += sizeof(uint16);
                    ofs += cEntries * (sizeof(uint16) + sizeof(uint16));
                } // if
            } // for
        }

        {
            CLOG_SECTION(2, "~D - Entry Table", pByteSink->GetPosn());

            foreach (Segments::Enum, oEnum, &m_oSegments)
            {
                const Segment* const pSegment = oEnum.Get();

                CLOG_SECTION(2, "Entry List");

                int const cEntries = pSegment->m_oEntries.Count();

                CLOG(2, "<li>~D - #entries=~D</li>",
                    pByteSink->GetPosn(),
                    cEntries );

                pByteSink->EmitU16(static_cast<uint16>(cEntries));

                foreach (Entries::Enum, oEnum, &pSegment->m_oEntries)
                {
                    const Entry* const pEntry = oEnum.Get();

                    CLOG(2, "<li>~D - code=~D bits=~D</li>",
                        pByteSink->GetPosn(),
                        pEntry->m_ofs,
                        pEntry->m_pDesc->GetOfs() + ofs );

                    pByteSink->EmitU16(static_cast<uint16>(
                        pEntry->m_ofs - pSegment->m_ofs ) );

                    pByteSink->EmitU16(static_cast<uint16>(
                        (pEntry->m_pDesc->GetOfs() + ofs) /
                        sizeof(GcMap::Bits) ) );
                } // for
            } // for
        }

        CLOG(2, "<li>~D - Bits</li>", pByteSink->GetPosn());

        // Bit vector
        m_oBits.CopyTo(pByteSink);

        // Padding
        while (0 != pByteSink->GetPosn() % 4)
        {
            pByteSink->EmitU16(0);
        }

        CLOG(2, "<li>~D - End</li>", pByteSink->GetPosn());
    } // Serialize
}; // Sink

class SubPassSerialize :
    public Pass_<SubPassSerialize, SubPass>,
    private Constructor,
    private InstructionDispatcher
{
    public: static const char* GetName_() { return "GcMap.Serialize"; }

    private: BitVec*    m_pLive;
    private: Sink       m_oSink;

    // ctor
    protected: SubPassSerialize(
        IMm*            const pIMm,
        Function*       const pFun,
        const RegGroup* const pRegGroup) :
            m_oSink(pIMm, pRegGroup),
            Constructor(pFun, pRegGroup) {}

    // Entry point
    public: static void Run(
        IMm*            const pIMm,
        Function*       const pFun,
        const RegGroup* const pRegGroup,
        ByteSink*       const pByteSink )
    {
        SubPassSerialize oPass(pIMm, pFun, pRegGroup);
        oPass.run();
        oPass.serialize(pByteSink);
    } // Run

    // [P]
    protected: override void processKill(int const k)
    {
        ASSERT(k > 0);
        m_pLive->SetZero(k);
    } // processKill

    protected: override void processUse(int const k)
    {
        ASSERT(k > 0);
        m_pLive->SetOne(k);
    } // processOperand

    // [R]
    private: void run()
    {
        CLOG_SECTION(1, "<b>Serialize</b>");

        foreach (Function::EnumBBlockReverse, oEnum, m_pFun)
        {
            DataFlowBB* const pBB = oEnum.Get()->Extend<DataFlowBB>();
            m_pLive = pBB->GetOut();

            CLOG_SECTION(1, "serialize ~S", pBB);

            {
                CLOG_SECTION(2, "LiveOut");
                m_oSink.DumpLiveness(m_pLive);
            }

            foreach (BBlock::EnumIReverse, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();
                CLOG_SECTION(1, "~D ~S", pI->GetIndex(), pI);
                dispatch(pI);
            } // for insn

            {
                CLOG_SECTION(2, "LiveIn");
                m_oSink.DumpLiveness(pBB->GetIn());
            }
        } // for bb
    } // run

    // [S]
    private: void serialize(
        ByteSink* const pByteSink )
    {
        m_oSink.Serialize(
            m_pFun->GetExitBB()->GetFirstI()->GetIndex(),
            pByteSink );
    } // serialize

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //
    private: override void processDefault(Instruction* pI)
    {
        processOutput(pI);

        if (needGc(pI))
        {
            processOperands(pI);
        }
    } // processDefault

    DefProcI(Branch)
    {
        int const cbInsn =
            pI->GetBB()->GetNext()->GetFirstI()->GetIndex() - pI->GetIndex();

        switch (cbInsn)
        {
        case 0:
            return;

        case 2: // Jcc Jb
            break;

        case 2 + 2: // Jcc Jb + JMP Jb
        case 2 + 5: // Jcc Jb + JMP Jv
            m_oSink.AddEntry(GcMap::JumpDesc, pI, 2);
            break;

        case 6: // Jcc Jv
            break;

        case 6 + 2: // Jcc Jv + JMP Jb
        case 6 + 5: // Jcc Jv + JMP Jv
            m_oSink.AddEntry(GcMap::JumpDesc, pI, 6);
            break;

        default:
            COMPILER_INTERNAL_ERROR();
            break;
        } // switch

        m_oSink.AddEntry(GcMap::JumpDesc, pI);
    } // Branch

    DefProcI(Call)
    {
        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pCallerSave)
        {
            m_pLive->SetZero(oEnum.Get()->m_nIndex);
        } // for

        if (Function* const pCallee = pI->GetSx()->DynamicCast<Function>())
        {
            processCallee(pCallee);
        } // if

        if (NeedArity(pI->GetSx()))
        {
            m_oSink.AddEntry(GcMap::StdCallDesc, pI);
        }
        else
        {
            if (Function* pCallee = pI->GetSx()->DynamicCast<Function>())
            {
                m_oSink.AddEntry(
                    pCallee->GetArity()->m_iMax <= 7 ?
                        GcMap::CallDesc2 :
                        GcMap::CallDesc3,
                    pI,
                    0,
                    pCallee->GetArity()->m_iMax );
            }
            else
            {
                //= <FIXME date="2009-01-25" by="yosi@msn.com">
                //=   We should know number of arguments of callee.
                //= </FIXME>
            }
        } // if
    } // Call

    DefProcI(Jump)
    {
        int const cbInsn =
            pI->GetBB()->GetNext()->GetFirstI()->GetIndex() - pI->GetIndex();

        if (0 != cbInsn)
        {
            m_oSink.AddEntry(GcMap::JumpDesc, pI);
        }
    } // Jump

    DefProcI(Use) { processUseI(pI); }
}; // SubPassSerialize

} // GcMapCtor

using namespace GcMapCtor;

void ComputeGcMap(
    IMm*      const pIMm,
    Function* const pFun,
    ByteSink* const pByteSink )
{
    CLOG_SECTION(1, "<h3>GC Map ~S</h3>", pFun);

    const RegGroup* const pRegGroup =
        Context::Get()->GetTarget()->GetGprGroup();

    SubPassCompute::Run(pFun, pRegGroup);
    SubPassSerialize::Run(pIMm, pFun, pRegGroup, pByteSink);
} // ComputeGcMap

} // Compiler
} // TinyCl
