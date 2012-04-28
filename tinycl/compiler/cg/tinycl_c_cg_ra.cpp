#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Code Generator - Register Allocator
// compiler/cg/tinycl_c_cg_ra.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_ra.cpp#26 $
//
#include "./tinycl_c_cg.h"

#pragma warning(disable: 4355) // 'this' : used in base member initializer list

namespace TinyCl
{

namespace Compiler
{

namespace CgRa
{

namespace Really
{
    enum Value
    {
        MayBe,
        Yes,
    }; // Value
} // Really

/// <summary>
///   Map from physical register or RegDesc to RegMap::Entry.
/// </summary>
class RegMap : public LocalObject
{
    public: struct Entry
    {
        Really::Value   m_eReally;
        int             m_iNextUse;
        Register*       m_pRx;
        Physical*       m_pPx;
    }; // Entry

    private: uint   m_c;
    private: Entry* m_prgoEntry;

    // ctor
    public: RegMap(Mm* pMm, const RegGroup* pRegGroup) :
        m_c(pRegGroup->m_pAll->m_c)
    {
        m_prgoEntry = reinterpret_cast<Entry*>(
            pMm->Alloc(sizeof(Entry) * m_c) );

        Load(pRegGroup);
    } // RegMap

    // [C]
    public: void Copy(const RegMap* pRegMap)
    {
        Entry* p = m_prgoEntry;
        foreach (RegMap::Enum, oEnum, pRegMap)
        {
            *p++ = *oEnum.Get();
        } // for entry
    } // Copy

    // [E]
    public: class Enum
    {
        private: Entry* m_pEnd;
        private: Entry* m_pRunner;

        public: Enum(const RegMap* p) :
            m_pRunner(const_cast<RegMap*>(p)->m_prgoEntry),
            m_pEnd(const_cast<RegMap*>(p)->m_prgoEntry + p->m_c) {}

        public: bool   AtEnd() const { return m_pEnd == m_pRunner; }
        public: Entry* Get() const   { ASSERT(! AtEnd()); return m_pRunner; }
        public: void   Next()        { ASSERT(! AtEnd()); m_pRunner++; }
    }; // Enum

    // [G]
    public: Entry* Get(const Physical* const pPx) const
        { return Get(pPx->GetDesc()); }

    public: Entry* Get(const RegDesc* const pReg) const
        { return &m_prgoEntry[pReg->m_nIndex]; }

    // [L]
    public: void Load(const RegGroup* pRegGroup)
    {
        foreach (RegSet::Enum, oEnum, pRegGroup->m_pFree)
        {
            Entry* const pEntry = Get(oEnum.Get());

            pEntry->m_pPx = Context::Get()->GetTarget()->
                GetPhysical(oEnum.Get());

            pEntry->m_eReally = Really::Yes;
        } // for entry
    } // Load
}; // RegMap


class RegPair :
    public LocalObject,
    public DoubleLinkedItem_<RegPair>
{
    public: Really::Value   const m_eReally;
    public: Physical*       const m_pPx;
    public: Register*       const m_pRx;

    public: RegPair(
        Physical*       const pPx,
        Register*       const pRx,
        Really::Value   const e ) :
        m_eReally(e), m_pPx(pPx), m_pRx(pRx) {}
        
    // Prevent default assignment operator
    private: RegPair& operator=(RegPair&)
        { CAN_NOT_HAPPEN(); }
}; // RegPair

typedef DoubleLinkedList_<RegPair> RegPairs;

struct BBlockExt : LocalObject
{
    RegMap*     m_pRegMap;
    RegPairs    m_oLiveIn;
}; // BBlockExt

static void dumpLiveness(BBlock* pBB, RegList* pRegList)
{
    Val stream = Context::Get()->GetPass()->GetStream();
    if (nil == stream)
    {
        return;
    }

    write_string("<table border='1' cellpadding='3'>\n", stream);

    {
        uint cRegs = 0;
        write_string("<tr><td>LiveIn</td><td>", stream);
        foreach (RegList::Enum, oEnum, pRegList)
        {
            Register* const pRx = oEnum.Get();
            if (pBB->IsLiveIn(pRx->GetIndex()))
            {
                CLOG(1, " ~S", pRx);
                cRegs += 1;
            }
        } // for

        if (0 == cRegs)
        {
            write_string("<i>None</i>", stream);
        }

        write_string("</td></tr>\n", stream);
    }

    {
        uint cRegs = 0;
        write_string("<tr><td>LiveOut</td><td>", stream);
        foreach (RegList::Enum, oEnum, pRegList)
        {
            Register* pRx = oEnum.Get();
            if (pBB->IsLiveOut(pRx->GetIndex()))
            {
                CLOG(1, " ~S", pRx);
                cRegs += 1;
            }
        } // for

        if (0 == cRegs)
        {
            write_string("<i>None</i>", stream);
        }

        write_string("</td></tr>\n", stream);
    }

    write_string("</table>\n", stream);
} // dumpLiveness

/// <summary>
///   Allocate physical register to virtual register. Results are stored into
///   OperandBox.
/// </summary>
class SubPassAllocate :
    public Pass_<SubPassAllocate, SubPass>,
    protected InstructionDispatcher
{
    public: static const char* GetName_() { return "Ra.Allocate"; }

    private: Function*          m_pFun;
    private: const RegGroup*    m_pRegGroup;
    private: RegMap*            m_pRegMap;
    private: RegList*           m_pRegList;

    public: static void Run(
        Function*       pFun,
        const RegGroup* pRegGroup,
        RegList*        pRegList )
    {
        SubPassAllocate oPass(pFun, pRegGroup, pRegList);
        oPass.Prepare();
        oPass.run(pFun);
    } // Run

    // ctor
    private: SubPassAllocate(
        Function*           pFun,
        const RegGroup*     pRegGroup,
        RegList*            pRegList ) :
            m_pFun(pFun),
            m_pRegGroup(pRegGroup),
            m_pRegList(pRegList) {}

    // [A]
    private: Physical* allocate(
        Instruction* const pI,
        Register*    const pRx )
    {
        ASSERT(pRx->GetRegClass() == m_pRegGroup->m_eRegClass);

        if (Physical* pPx = allocateAux(pRx))
        {
            return pPx;
        }

        // No available register
        CLOG_SECTION(4, "spilling");

        RegMap::Entry* pToSpill = m_pRegMap->
            Get(m_pRegGroup->m_pAllocable->m_prgpReg[0]);

        computeNextUse(pToSpill, pI);

        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pAllocable)
        {
            RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());

            if (pToSpill->m_iNextUse < computeNextUse(pEntry, pI))
            {
                pToSpill = pEntry;
            }
        } // for reg

        if (pToSpill->m_iNextUse == pI->GetIndex())
        {
            CLOG(1, "<li class='e'>No available register.</li>");
            COMPILER_INTERNAL_ERROR();
        }

        if (pI->Is<PhiI>() &&
            pI->GetRd() == pRx &&
            pToSpill->m_pRx->GetDefI()->Is<PhiI>() )
        {
            CLOG(3, "<li>make unbound ~S</li>", pToSpill->m_pRx);
            pToSpill->m_pRx->m_pPhysical = NULL;
        }

        spill(pToSpill);

        assign(pToSpill, pRx);

        return pToSpill->m_pPx;
    } // allocate

    private: Physical* allocateAux(Register* const pRx)
    {
        ASSERT(pRx->GetRegClass() == m_pRegGroup->m_eRegClass);
        RegMap::Entry* pFree = NULL;
        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pAllocable)
        {
            RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());

            if (NULL == pEntry->m_pPx)
            {
                continue;
            }

            if (pRx == pEntry->m_pRx)
            {
                CLOG(1, "<li>~S is ~S</li>", pRx, pEntry->m_pPx);
                return pEntry->m_pPx;
            }

            if (NULL == pEntry->m_pRx)
            {
                if (pRx->m_pPhysical == pEntry->m_pPx)
                {
                    pFree = pEntry;
                }
                else if (NULL == pFree)
                {
                    pFree = pEntry;
                }
            }
        } // for reg

        if (NULL != pFree)
        {
            assign(pFree, pRx);
            return pFree->m_pPx;
        }

        return NULL;
    } // allocateAux

    // For PhiI and ValuesI operands
    private: void allocateIfPossible(OperandBox* const pBox)
    {
        if (Register* const pRx = fetchRx(pBox->GetRx()))
        {
            if (Physical* const pPx = findPhysical(pRx))
            {
                CLOG(2, "<li>~S ~S</li>", pRx, pPx);
                pBox->m_pPhysical = pPx;
            }
            else if (NULL == pRx->m_pSpill)
            {
                // Parallel copy requires source operand in register.
                spill(pRx);
            }
        }
    } // allocateIfPossible

    private: Physical* allocateRd(
        Instruction* const pI,
        Register*    const pRx )
    {
        if (AssignI* const pAssignI = pI->GetPrev() ?
                pI->GetPrev()->DynamicCast<AssignI>() : NULL )
        {
            Physical* const pPx = pAssignI->GetRd()->m_pPhysical;

            foreach (RegMap::Enum, oEnum, m_pRegMap)
            {
                RegMap::Entry* pEntry = oEnum.Get();
                if (pEntry->m_pPx == pPx)
                {
                    assign(pEntry, pRx);
                    return pPx;
                }
            } // for

            CLOG(1, "<li class='e'>Failed to assign</li>");
            COMPILER_INTERNAL_ERROR();
        } // assign

        if (pI->Is<CopyI>())
        {
            // Try to allocate %rd to %rx.
            if (Register* const pR2 = pI->GetRx())
            {
                Physical* const pPx = pR2->m_pPhysical;

                foreach (RegMap::Enum, oEnum, m_pRegMap)
                {
                    RegMap::Entry* const pEntry = oEnum.Get();
                    if (pEntry->m_pPx == pPx)
                    {
                        assign(pEntry, pRx);
                        return pPx;
                    }
                } // for
            }
        } // copy

        return allocate(pI, pRx);
    } // allocateRd

    private: Physical* allocateRx(
        Instruction* const pI,
        Register*    const pRx )
    {
        return allocate(pI, pRx);
    } // allocateRx

    private: void assign(RegMap::Entry* const pEntry, Register* const pRx)
    {
        pEntry->m_eReally = Really::Yes;
        pEntry->m_pRx = pRx;
        CLOG(3, "<li>assign ~S to ~S</li>~%", pRx, pEntry->m_pPx);
    } // assign

    // [C]
    private: static int computeNextUse(
        RegMap::Entry*  const pEntry,
        Instruction*    const pRefI )
    {
        Register* const pRx = pEntry->m_pRx;

        BBlock* const pRefBB = pRefI->GetBBlock();

        int iIndex = 0;

        Instruction* pUseI = NULL;

        foreach (Register::EnumUser, oEnum, pRx)
        {
            Instruction* const pI = oEnum.Get()->GetI();
            if (pRefBB->DoesDominate(pI->GetBBlock()))
            {
                if (pI->GetIndex() > pRefI->GetIndex())
                {
                    if (NULL == pUseI || iIndex > pI->GetIndex())
                    {
                        iIndex = pI->GetIndex();
                        pUseI = pI;
                    }
                }
            }
        } // for user

        if (NULL == pUseI)
        {
            CLOG(3, "<li>~S is no more used</li>", pRx);
            iIndex = 0;
        }
        else
        {
            CLOG(3, "<li>next use of ~S is ~D: ~S</li>",
                pRx, pUseI->GetIndex(), pUseI );
        }

        return pEntry->m_iNextUse = iIndex;
    } // computeNextUse

    // [D]
    private: void dumpMap()
    {
        Val const stream = Context::Get()->GetPass()->GetStream();
        if (nil == stream) return;

        CLOG_SECTION(3, "<b>Register Map</b>");

        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pAllocable)
        {
            const RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());

            if (NULL == pEntry->m_pRx)
            {
                continue;
            }

            CLOG(1, "<li>~S ~A ~S</li>",
                pEntry->m_pRx,
                pEntry->m_eReally ? "is" : "maybe",
                pEntry->m_pPx );
        } // for reg
    } // dumpMap

    // [F]
    private: Register* fetchRd(Instruction* const pI)
    {
        return fetchRx(pI->GetRd());
    } // fetchRd

    private: Register* fetchRx(Register* const pRx)
    {
        if (NULL == pRx)
        {
            return NULL;
        }

        if (pRx->GetRegClass() != m_pRegGroup->m_eRegClass)
        {
            return NULL;
        }

        return pRx;
    } // fetchRd

    private: Physical* findPhysical(Register* const pRx)
    {
        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pAllocable)
        {
            RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());

            if (NULL == pEntry->m_pPx)
            {
                continue;
            }

            if (pRx == pEntry->m_pRx)
            {
                return pEntry->m_pPx;
            }
        } // for
        return NULL;
    } // findPhysical

    // [I]
    private: bool isCallerSave(Register* const pRx)
    {
        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pArgs)
        {
            RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());
            if (pEntry->m_pRx == pRx)
            {
                return true;
            }
        } // for
        return false;
    } // isCallerSave

    private: static bool isUsedAfter(
        Instruction* const pI,
        Register*    const pRx )
    {
        BBlock* const pBB = pI->GetBBlock();

        if (pBB->IsLiveOut(pRx->GetIndex()))
        {
            CLOG(2, "<li>~S is live out.</li>~%", pRx);
            return true;
        }

        if (NULL != nextUser(pI, pRx))
        {
            return true;
        }

        CLOG(2, "<li>~S is no more used.</li>~%", pRx);
        return false;
    } // isUsedAfter

    // [N]
    private: static Instruction* nextUser(
        Instruction* const pI,
        Register*    const pRx )
    {
        BBlock* const pBB = pI->GetBBlock();
        foreach (Register::EnumUser, oEnum, pRx)
        {
            Instruction* const pUserI = oEnum.Get()->GetI();
            if (pUserI->GetBBlock() == pBB &&
                pUserI->GetIndex() > pI->GetIndex() )
            {
                CLOG(2, "<li>~S is used at ~S</li>~%", pRx, pUserI);
                return pUserI;
            }
        } // for user
        return NULL;
    } // nextUser

    // [P]
    private: void prepareBBlock(BBlock* const pBB)
    {
        int cPreds = 0;
        foreach (BBlock::EnumInEdge,  oEnum, pBB)
        {
            CfgEdge* const pEdge = oEnum.Get();

            CLOG(2, "<li>in edge ~S</li>", pEdge);

            if (pEdge->GetKind() == CfgEdge::Kind_Nonlocal)
            {
                foreach (RegMap::Enum, oEnum, m_pRegMap)
                {
                    oEnum.Get()->m_pRx = NULL;
                }
                break;
            }

            BBlock* const pPredBB = pEdge->GetFrom();

            if (0 == cPreds)
            {
                m_pRegMap->Copy(pPredBB->GetWork<BBlockExt>()->m_pRegMap);
            }
            else
            {
                RegMap* const pRegMap2 =
                    pPredBB->GetWork<BBlockExt>()->m_pRegMap;

                foreach (RegMap::Enum, oEnum, m_pRegMap)
                {
                    RegMap::Entry* const pEntry1 = oEnum.Get();
                    if (NULL != pEntry1->m_pPx &&
                        NULL != pEntry1->m_pRx )
                    {
                        RegMap::Entry* const pEntry2 = pRegMap2->Get(
                            pEntry1->m_pPx );

                        if (pEntry1->m_pRx != pEntry2->m_pRx)
                        {
                            pEntry1->m_pRx = NULL;
                        }
                        else if (pEntry2->m_eReally == Really::MayBe)
                        {
                            pEntry1->m_eReally = Really::MayBe;
                        }
                    }
                } // for
            } // if

            cPreds += 1;
        } // for pred

        RegPairs* pLiveIn = &pBB->GetWork<BBlockExt>()->m_oLiveIn;

        {
            CLOG_SECTION(2, "Inherited registers~%");
            foreach (RegMap::Enum, oEnum, m_pRegMap)
            {
                RegMap::Entry* const pEntry = oEnum.Get();

                if (NULL == pEntry->m_pRx || NULL == pEntry->m_pPx)
                {
                    continue;
                }

                CLOG(2, "<li>~S ~A ~S</li>~%",
                    pEntry->m_pRx,
                    pEntry->m_eReally == Really::Yes ? "is" : "maybe",
                    pEntry->m_pPx );

                if (pBB->IsLiveIn(pEntry->m_pRx->GetIndex()))
                {
                    pLiveIn->Append(
                        new RegPair(
                            pEntry->m_pPx,
                            pEntry->m_pRx,
                            pEntry->m_eReally) );
                }
                else
                {
                    pEntry->m_pRx = NULL;
                }
            } // for reg
        }

        {
            CLOG_SECTION(2, "LiveIn");
            foreach (RegList::Enum, oEnum, m_pRegList)
            {
                Register* const pRx = oEnum.Get();
                if (! pBB->IsLiveIn(pRx->GetIndex()))
                {
                    continue;
                }

                if (NULL != pRx->m_pSpill)
                {
                    CLOG(2, "<li>spilled ~S</li>", pRx);
                }
                else
                {
                    ASSERT(NULL != pRx->m_pPhysical);

                    RegMap::Entry* pEntry = m_pRegMap->Get(
                        pRx->m_pPhysical );

                    if (pRx == pEntry->m_pRx)
                    {
                        CLOG(2, "<li>~S ~A ~S</li>~%",
                            pEntry->m_pRx,
                            pEntry->m_eReally == Really::Yes ? "is" : "maybe",
                            pEntry->m_pPx );
                    }
                    else if (NULL == pEntry->m_pRx)
                    {
                        CLOG(2, "<li>~S maybe ~S</li>",
                            pRx, pEntry->m_pPx );

                        pEntry->m_eReally = Really::MayBe;
                        pEntry->m_pRx = pRx;

                        pLiveIn->Append(
                            new RegPair(
                                pEntry->m_pPx,
                                pEntry->m_pRx,
                                Really::MayBe ) );
                    }
                    else
                    {
                        COMPILER_INTERNAL_ERROR();
                    }
                }
            } // for reg
        }
    } // prepareBBlock

    private: void processBBlock(BBlock* const pBB)
    {
        {
            CLOG_SECTION(1, "<h2>Process ~S</h2>", pBB);

            dumpLiveness(pBB, m_pRegList);

            m_pRegMap = pBB->GetWork<BBlockExt>()->m_pRegMap;

            prepareBBlock(pBB);

            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();

                CLOG_SECTION(2, "process ~D: ~S", pI->GetIndex(), pI);
                dispatch(pI);
            } // for insn

            dumpMap();

            foreach (BBlock::EnumSucc, oEnum, pBB)
            {
                BBlock* const pSuccBB = oEnum.Get();
                CLOG_SECTION(2, "process succ ~S", pSuccBB);
                foreach (BBlock::EnumI, oEnum, pSuccBB)
                {
                    PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
                    if (NULL == pPhiI)
                    {
                        break;
                    }

                    allocateIfPossible(pPhiI->GetOperandBox(pBB));
                } // for
            } // for succ
        }

        foreach (BBlock::EnumChild, oEnum, pBB)
        {
            BBlock* const pChild = oEnum.Get();
            processBBlock(pChild);
        } // for child
    } // processBBlock

    private: virtual void processDefault(Instruction* pI) override
    {
        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            updateOperand(oEnum.GetBox());
        } // for source

        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            releaseIfNotUsed(oEnum.GetBox());
        } // for source

        processOutput(pI);
    } // processDefault

    private: void processOutput(
        Instruction* const pI )
    {
        Register* const pRd = fetchRd(pI);
        if (NULL == pRd)
        {
            return;
        }

        CLOG_SECTION(3, "output ~S", pRd);

        pRd->m_pPhysical = allocateRd(pI, pRd);

        if (pRd->GetFlag())
        {
            spill(pRd);
        }
    } // processOutput

    // [R]
    private: void releaseIfNotUsed(OperandBox* const pBox)
    {
        releaseIfNotUsed(pBox, pBox->GetI());
    } // releaseIfNotUsed

    private: void releaseIfNotUsed(
        OperandBox*     const pBox,
        Instruction*    const pUserI )
    {
        Register* const pRx = fetchRx(pBox->GetRx());
        if (NULL == pRx) 
        {
            return;
        }

        Instruction* const pNextUserI = nextUser(pUserI, pRx);

        if (NULL == pNextUserI)
        {
            // pRx isn't used in this bblock after pUserI.
            if (! pUserI->GetBBlock()->IsLiveOut(pRx->GetIndex()))
            {
                CLOG(2, "<li><b class='g'>~S is no more used.</b></li>",
                    pRx );

                releasePx(pRx);
                return;
            }

            CLOG(2, "<li>~S is live out.</li>", pRx);
        }

        // If next use point is over Call, we spill it.
        if (isCallerSave(pRx))
        {
            for (
                Instruction* pRunnerI = pUserI->GetNext();
                pNextUserI != pRunnerI;
                pRunnerI = pRunnerI->GetNext() )
            {
                if (pRunnerI->Is<CallI>())
                {
                    CLOG(2, "<li>over Call ~S</li>", pRx);
                    spill(pRx);
                    releasePx(pRx);
                    return;
                }
            } // for
        } // if caller save
    } // releaseIfNotUsed

    private: void releasePx(Register* const pRx)
    {
        foreach (RegMap::Enum, oEnum, m_pRegMap)
        {
            RegMap::Entry* pEntry = oEnum.Get();
            if (pEntry->m_pRx == pRx)
            {
                CLOG(3, "<li>release ~S from ~S</li>",
                    pEntry->m_pPx, pRx );
                pEntry->m_pRx = NULL;
                break;
            }
        } // for
    } // releasePx

    private: void run(Function* const pFun)
    {
        CLOG(1, "<h1>Process/~A ~S</h1>", m_pRegGroup->m_pszName, pFun);
        processBBlock(pFun->GetEntryBB());
    } // run

    // [S]
    private: void spill(RegMap::Entry* const pEntry)
    {
        spill(pEntry->m_pRx);
        pEntry->m_pRx = NULL;
    } // spill

    private: void spill(Register* const pRx)
    {
        CLOG_SECTION(3, "spill ~S", pRx);

        if (NULL != pRx->m_pSpill)
        {
            CLOG(3, "<li>~S is already spilled</li>", pRx);
            return;
        }

        StackSlot* const pSpill = new StackSlot(
            m_pRegGroup->m_eRegClass,
            m_pFun->m_cbFrame );

        if (Float* pFx = pRx->DynamicCast<Float>())
        {
            if (pFx->GetDefI()->GetTy() == tyFloat64)
            {
                m_pFun->m_cbFrame += 8;
            }
            else if (pFx->GetDefI()->GetTy() == tyFloat32)
            {
                m_pFun->m_cbFrame += 4;
            }
            else
            {
                C_INTERNAL_ERROR("Unsupported native float type");
            }
        }
        else
        {
            m_pFun->m_cbFrame += m_pRegGroup->m_cbWidth;
        }

        pRx->m_pSpill = pSpill;

        CLOG(3, "<li><b class='r'>spill</b> ~S to ~S</li>", pRx, pSpill);
    } // spill

    // [U]
    private: void updateOperand(OperandBox* const pBox)
    {
        Register* const pRx = fetchRx(pBox->GetRx());
        if (NULL == pRx)
        {
            return;
        }

        Physical* pPx = findPhysical(pRx);
        if (NULL == pPx)
        {
            pPx = allocateRx(pBox->GetI(), pRx);
        }

        pBox->m_pPhysical = pPx;
    } // updateOperand

    ////////////////////////////////////////////////////////////
    //
    // Instruction handlers
    //
    DefProcI(Call)
    {
        // Register callee is handled specially. It is done by another pass
        // before RA.
        ASSERT(NULL == pI->GetRx());

        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pCallerSave)
        {
            RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());
            if (Register* const pRx = pEntry->m_pRx)
            {
                if (isUsedAfter(pI, pRx))
                {
                    spill(pEntry);
                }
                else
                {
                    CLOG(3, "<li>release ~S from ~S</li>~%",
                        pEntry->m_pPx, pRx );
                }

                pEntry->m_pRx = NULL;
            }
        } // for reg

        processOutput(pI);
    } // Call

    DefProcI(Count)
    {
        if (NULL == m_pRegGroup->m_pRn)
        {
            return;
        }

        Register* const pRd = pI->GetRd();

        RegMap::Entry* const pEntry = m_pRegMap->Get(m_pRegGroup->m_pRn);
        if (NULL == pEntry->m_pRx)
        {
            assign(pEntry, pRd);
            pRd->m_pPhysical = pEntry->m_pPx;
            return;
        }

        processOutput(pI);
    } // Count

    // Block values registers
    DefProcI(MvRestore)
    {
        ASSERT(NULL != pI);
        m_pRegMap->Load(m_pRegGroup);
    } // MvRestore

    // Block values registers
    DefProcI(MvSave)
    {
        ASSERT(NULL != pI);
        uint nNth = 0;
        foreach (RegSet::Enum, oEnum, m_pRegGroup->m_pArgs)
        {
            RegMap::Entry* const pEntry = m_pRegMap->Get(oEnum.Get());
            if (0 != nNth)
            {
                pEntry->m_pPx = NULL;
            }
            nNth += 1;
        } // for
    } // MvSave

    DefProcI(OpenBind)
    {
        ASSERT(NULL != pI);
        // We must ignore all operands since these operands are
        // used after OpenBind.
    } // OpenBind

    DefProcI(Phi)
    {
        // Process all Phis if we are at the last PHI
        if (pI->GetNext()->Is<PhiI>())
        {
            return;
        }

        // Allocates register for Phi outptus
        foreach (BBlock::EnumI, oEnum, pI->GetBBlock())
        {
            PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI)
            {
                break;
            }

            processOutput(pPhiI);
        } // for phi
    } // Phi

    /// <summary>
    ///   Processes VALUES instruction:
    ///   <list>
    ///     <item><description>
    ///       We process Values instruction by parallel copy.
    ///     </description></item>
    ///     <item><description>
    ///       We can't allocate register for VALUES instruction since it can
    ///       exceeds number of allocatable registers.
    ///     </description></item>
    ///   </list>
    /// </summary>
    DefProcI(Values)
    {
        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            allocateIfPossible(oEnum.GetBox());
        } // for operand

        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            OperandBox* const pBox = oEnum.GetBox();
            releaseIfNotUsed(pBox, pI);
        } // for operand
    } // Values
}; // SubPassAllocate

/// <summary>
///  Assign instructions in layout order for checking next use
///  position.
/// </summary>
class SubPassAssign :
    public Pass_<SubPassAssign, SubPass>,
    protected InstructionDispatcher,
    protected Mm
{
    public: static const char* GetName_() { return "Ra.Assign"; }

    private: class LiveMap
    {
        private: class LiveList
        {
            private: uint       m_c;
            private: Register** m_prgp;

            public: LiveList(Mm* pMm, const RegGroup* pRegGroup) :
                m_c(pRegGroup->m_pAll->m_c)
            {
                m_prgp = reinterpret_cast<Register**>(
                    pMm->Alloc(sizeof(Register*) * m_c) );
            } // LiveList

            // [G]
            public: Register* Get(const Physical* const pPx) const
                { return m_prgp[pPx->GetDesc()->m_nIndex]; }

            // [R]
            public: void Reset()
            {
                ::ZeroMemory(m_prgp, sizeof(Register*) * m_c);
            } // Reset

            // [S]
            public: void Set(const Physical* pPx, Register* pRx)
                { m_prgp[pPx->GetDesc()->m_nIndex] = pRx; }
        }; // LiveList

        private: LiveList m_oFprList;
        private: LiveList m_oGprList;

        // ctor
        public: LiveMap(Mm* pMm) :
            m_oFprList(pMm, Context::Get()->GetTarget()->GetFprGroup()),
            m_oGprList(pMm, Context::Get()->GetTarget()->GetGprGroup())
            {}

        // [G]
        public: Register* Get(const Physical* const pPx) const
        {
            switch (pPx->GetDesc()->m_eRegClass)
            {
            case RegClass_Fpr:
                return m_oFprList.Get(pPx);

            case RegClass_Gpr:
                return m_oGprList.Get(pPx);

            default:
                CAN_NOT_HAPPEN();
            } // switch regclass
        } // Get

        // [R]
        public: void Reset()
        {
            m_oFprList.Reset();
            m_oGprList.Reset();
        } // Reset

        // [S]
        public: void Set(const Physical* const pPx, Register* const pRx)
        {
            switch (pPx->GetDesc()->m_eRegClass)
            {
            case RegClass_Fpr:
                m_oFprList.Set(pPx, pRx);
                break;

            case RegClass_Gpr:
                m_oGprList.Set(pPx, pRx);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch regclass
        } // Set
    }; // LiveMap

    private: LiveMap            m_oLiveMap;
    private: const RegGroup*    m_pFprGroup;
    private: const RegGroup*    m_pGprGroup;

    // ctor
    private: SubPassAssign() :
        m_oLiveMap(this),
        m_pFprGroup(Context::Get()->GetTarget()->GetFprGroup()),
        m_pGprGroup(Context::Get()->GetTarget()->GetGprGroup()) {}

    // Entry Point
    public: static void Run(Function* pFun)
    {
        SubPassAssign oSubPass;
        oSubPass.Prepare();
        oSubPass.run(pFun);
    } // Run

    // [P]
    private: void processBBlock(BBlock* const pBB)
    {
        CLOG_SECTION(1, "<h3>Process ~S</h3>", pBB);

        m_oLiveMap.Reset();

        {
            CLOG_SECTION(2, "LiveIn");
            foreach (
                RegPairs::Enum,
                oEnum,
                &pBB->GetWork<BBlockExt>()->m_oLiveIn )
            {
                const RegPair* const pPair = oEnum.Get();
                if (NULL == pPair->m_pRx || Really::Yes == pPair->m_eReally)
                {
                    m_oLiveMap.Set(pPair->m_pPx, pPair->m_pRx);
                    CLOG(2, "<li>~S is ~S</li>", pPair->m_pPx, pPair->m_pRx);
                }
            } // for
        }

        foreach (BBlock::EnumI, oEnum, pBB)
        {
            dispatch(oEnum.Get());
        } // for insn
    } // processBBlock

    private: virtual void processDefault(Instruction* pI) override
    {
        processOperands(pI);
        processOutput(pI);
    } // processDefault

    private: void processOperands(Instruction* const pI)
    {
        BBlock* pBB = pI->GetBBlock();

        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            Register* const pRx = oEnum.GetRx();
            if (NULL == pRx) continue;

            OperandBox* const pBox = oEnum.GetBox();
            if (Physical* const pPx = pBox->m_pPhysical)
            {
                if (StackSlot* const pSpill = pRx->m_pSpill)
                {
                    if (m_oLiveMap.Get(pPx) != pRx)
                    {
                        pBB->InsertBeforeI(
                            new ReloadI(pRx->GetTy(), pPx, pSpill),
                            pI );

                        m_oLiveMap.Set(pPx, pRx);
                    }
                }
                pBox->Replace(pPx);
            }
        } // for box
    } // processOperands

    private: void processOutput(Instruction* const pI)
    {
        if (Register* const pRd = pI->GetRd())
        {
            Physical* const pPd = pRd->m_pPhysical;

            if (StackSlot* const pMd = pRd->m_pSpill)
            {
                pI->GetBBlock()->InsertAfterI(
                    new SpillI(pRd->GetTy(), pMd, pPd),
                    pI );
            }

            m_oLiveMap.Set(pPd, pRd);

            pI->SetOutput(pPd);
        }
    } // processOutput

    // [R]
    private: void resetRegSet(const RegSet* const pRegSet)
    {
        foreach (RegSet::Enum, oEnum, pRegSet)
        {
            const RegDesc* const pReg = oEnum.Get();

            m_oLiveMap.Set(
                Context::Get()->GetTarget()->GetPhysical(pReg),
                NULL );
        } // for reg
    } // resetRegSet

    private: void run(Function* const pFun)
    {
        CLOG(1, "<h1>Process ~S</h1>", pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            processBBlock(oEnum.Get());
        } // for bblock
    } // run

    // Call
    //  Reset register tracker for caller save registers
    DefProcI(Call)
    {
        resetRegSet(m_pFprGroup->m_pCallerSave);
        resetRegSet(m_pGprGroup->m_pCallerSave);
        processOutput(pI);

        if (Physical* pPd = pI->GetOutput()->DynamicCast<Physical>())
        {
            Physical* pP0 = Context::Get()->GetTarget()->GetPhysical(
                m_pGprGroup->m_pArgs->m_prgpReg[0] );
            if (pPd != pP0)
            {
                pI->SetOutput(pP0);
                pI->GetBB()->InsertAfterI(
                    new CopyI(pI->GetTy(), pPd, pP0),
                    pI );
            }
        } // if
    } // Call

    // Parallel copy pass handles spilling.
    DefProcI(Select)
    {
        if (Register* const pRd = pI->GetRd())
        {
            if (StackSlot* const pMd = pRd->m_pSpill)
            {
                pI->SetOutput(pMd);
            }
            else if (Physical* const pPd = pRd->m_pPhysical)
            {
                m_oLiveMap.Set(pPd, pRd);
                pI->SetOutput(pPd);
            }
        }
    } // Select

    DefProcI(Phi)
    {
        if (Register* const pRd = pI->GetRd())
        {
            if (Physical* const pPd = pRd->m_pPhysical)
            {
                m_oLiveMap.Set(pPd, pRd);
                pI->SetOutput(pPd);

                if (StackSlot* const pMd = pRd->m_pSpill)
                {
                    Instruction* pRefI = pI;
                    while (pRefI->Is<PhiI>())
                    {
                        pRefI = pRefI->GetNext();
                    } // while

                    pRefI->GetBBlock()->InsertBeforeI(
                        new SpillI(pI->GetTy(), pMd, pPd),
                        pRefI );
                } // if spill
            }
            else if (StackSlot* const pMd = pRd->m_pSpill)
            {
                pI->SetOutput(pMd);
            } // if
        } // if
    } // Phi

    DefProcI(VarHome)
    {
        class Local
        {
            public: static StackSlot* Optimize(Instruction* const pI)
            {
                if (pI->GetTy() == tyStackCell)
                {
                    return NULL;
                }

                VarHome* pMd = pI->GetOutput()->DynamicCast<VarHome>();
                if (NULL == pMd) return NULL;

                Register* pRy = pI->GetRy();
                if (NULL == pRy) return NULL;

                return pRy->m_pSpill;
            } // Optimize
        }; // Local

        if (StackSlot* pMy = Local::Optimize(pI))
        {
            CLOG(2, "<li>Use ~S instead of ~S</li>", 
                pMy, pI->GetOutput() );
            pI->SetOutput(pMy);
        }

        processDefault(pI);
    } // VarHome
}; // SubPassAssign

/// <summary>
///   Emit instructions for parallel copy.
///   <para>
///     Note: We don't allow Mx and Md are same location.
///   </para>
/// </summary>
class ParallelCopy :
    protected Mm
{
    private: class CopyTask :
        public LocalObject,
        public WorkListItem_<CopyTask>
    {
        public: Physical*   m_pRd;
        public: Physical*   m_pRx;

        // Note: pRd == pRx to avoid using pRx for MemCopy.
        public: CopyTask(Physical* const pRd, Physical* const pRx) :
            m_pRd(pRd), m_pRx(pRx)
        {
            CLOG(3, "<li>CopyTask ~S := ~S</li>~%", pRd, pRx);
            m_pRx->m_cUses += 1;
        } // CopyTask
    }; // CopyTask

    private: typedef WorkList_<CopyTask> CopyTasks;

    private: class LoadTask :
        public LocalObject,
        public WorkListItem_<LoadTask>
    {
        public: Physical*   m_pRd;
        public: MemSlot*    m_pMx;

        public: LoadTask(Physical* const pRd, MemSlot* const pMx) :
            m_pRd(pRd), m_pMx(pMx)
        {
            CLOG(3, "<li>LoadTask ~S := ~S</li>~%", pRd, pMx);
        } // LoadTask
    }; // LoadTask

    private: typedef WorkList_<LoadTask> LoadTasks;

    private: class MemCopyTask :
        public LocalObject,
        public WorkListItem_<MemCopyTask>
    {
        public: MemSlot*   m_pMd;
        public: MemSlot*   m_pMx;

        public: MemCopyTask(MemSlot* const pMd, MemSlot* const pMx) :
            m_pMd(pMd), m_pMx(pMx)
        {
            CLOG(3, "<li>MemCopyTask ~S := ~S</li>~%", pMd, pMx);
        } // MemCopyTask
    }; // MemCopyTask

    private: typedef WorkList_<MemCopyTask> MemCopyTasks;

    private: class MemSetTask :
        public LocalObject,
        public WorkListItem_<MemSetTask>
    {
        public: MemSlot*    m_pMd;
        public: Operand*    m_pSx;

        public: MemSetTask(MemSlot* const pMd, Operand* const pSx) :
            m_pMd(pMd), m_pSx(pSx)
        {
            CLOG(3, "<li>MemSetTask ~S := ~S</li>~%", pMd, pSx);
        } // MemSetTask
    }; // MemSetTask

    private: typedef WorkList_<MemSetTask> MemSetTasks;

    private: class SetTask :
        public LocalObject,
        public WorkListItem_<SetTask>
    {
        public: Physical*   m_pRd;
        public: Operand*    m_pSx;

        public: SetTask(Physical* const pRd, Operand* const pSx) :
            m_pRd(pRd), m_pSx(pSx)
        {
            CLOG(3, "<li>SetTask ~S := ~S</li>~%", pRd, pSx);
        } // SetTask
    }; // SetTask

    private: typedef WorkList_<SetTask> SetTasks;

    private: class StoreTask :
        public LocalObject,
        public WorkListItem_<StoreTask>
    {
        public: MemSlot*    m_pMd;
        public: Physical*   m_pRx;

        public: StoreTask(MemSlot* const pMd, Physical* const pRx) :
            m_pMd(pMd), m_pRx(pRx)
        {
            CLOG(3, "<li>StoreTask ~S := ~S</li>~%", pMd, pRx);
            m_pRx->m_cUses += 1;
        } // StoreTask
    }; // StoreTask

    private: typedef WorkList_<StoreTask> StoreTasks;

    private: typedef WorkList_<CopyTask> CopyTasks;
    private: WorkList_<CopyTask>    m_oCopyTasks;
    private: WorkList_<LoadTask>    m_oLoadTasks;
    private: WorkList_<MemCopyTask> m_oMemCopyTasks;
    private: WorkList_<MemSetTask>  m_oMemSetTasks;
    private: WorkList_<SetTask>     m_oSetTasks;
    private: WorkList_<StoreTask>   m_oStoreTasks;
    private: BBlock*                m_pBB;
    private: Instruction*           m_pRefI;
    private: const RegSet*          m_pRegSet;

    // ctor
    public: ParallelCopy(const RegGroup* pRegGroup) :
        m_pBB(NULL),
        m_pRefI(NULL),
        m_pRegSet(pRegGroup->m_pFree) {}

    // [A]
    public: void AddTask(MemSlot* const pMd, OperandBox* const pBox)
    {
        if (Physical* const pPx = pBox->m_pPhysical)
        {
            AddTask(pMd, pPx);
        }
        else if (Register* const pRx = pBox->GetRx())
        {
            if (StackSlot* const pMx = pRx->m_pSpill)
            {
                AddTask(pMd, pMx);
            }
            else
            {
                CLOG(1, "<li class='e'>Can't add task ~S => ~S</li>",
                    pRx, pMd );

                COMPILER_INTERNAL_ERROR();
            }
        }
        else
        {
            AddTask(pMd, pBox->GetOperand());
        }
    } // AddTask

    public: void AddTask(Physical* const pRd, OperandBox* const pBox)
    {
        if (Physical* const pPx = pBox->m_pPhysical)
        {
            AddTask(pRd, pPx);
        }
        else if (Register* const pRx = pBox->GetRx())
        {
            if (StackSlot* const pMx = pRx->m_pSpill)
            {
                AddTask(pRd, pMx);
            }
            else
            {
                CLOG(1, "<li class='e'>Can't add task ~S => ~S</li>",
                    pRx, pRd );

                COMPILER_INTERNAL_ERROR();
            }
        }
        else
        {
            AddTask(pRd, pBox->GetOperand());
        }
    } // AddTask

    public: void AddTask(Register* const pRd, Physical* const pRx)
    {
        if (StackSlot* const pMd = pRd->m_pSpill)
        {
            AddTask(pMd, pRx);
        }
        else if (Physical* const pPd = pRd->m_pPhysical)
        {
            AddTask(pPd, pRx);
        }
        else
        {
            CLOG(1, "<li class='e'>Can't add task ~S => ~S</li>",
                pRx, pRd );

            COMPILER_INTERNAL_ERROR();
        }
    } // AddTask

    public: void AddTask(Register* const pRd, MemSlot* const pMx)
    {
        if (StackSlot* const pMd = pRd->m_pSpill)
        {
            AddTask(pMd, pMx);
        }
        else if (Physical* const pPd = pRd->m_pPhysical)
        {
            AddTask(pPd, pMx);
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }
    } // AddTask

    public: void AddTask(Physical* const pRd, Operand* const pSx)
    {
        if (Physical* const pRx = pSx->DynamicCast<Physical>())
        {
            m_oCopyTasks.Push(new(this) CopyTask(pRd, pRx));
        }
        else if (MemSlot* const pMx = pSx->DynamicCast<MemSlot>())
        {
            m_oLoadTasks.Push(new(this) LoadTask(pRd, pMx));
        }
        else
        {
            m_oSetTasks.Push(new(this) SetTask(pRd, pSx));
        }
    } // AddTask

    public: void AddTask(MemSlot* const pMd, Operand* const pSx)
    {
        if (Physical* const pRx = pSx->DynamicCast<Physical>())
        {
            m_oStoreTasks.Push(new(this) StoreTask(pMd, pRx));
        }
        else if (MemSlot* const pMx = pSx->DynamicCast<MemSlot>())
        {
            m_oMemCopyTasks.Push(new(this) MemCopyTask(pMd, pMx));
        }
        else
        {
            m_oMemSetTasks.Push(new(this) MemSetTask(pMd, pSx));
        }
    } // AddTask

    // [E]
    private: void emitCopy(CgOutput* pRd, Operand* pSx)
    {
        m_pBB->InsertBeforeI(new PhiCopyI(pRd, pSx), m_pRefI);
    } // emitCopy

    // [G]
    public: RegClass GetRegClass() const
        { return m_pRegSet->m_eRegClass; }

    private: static Physical* getPhysical(const RegDesc* p)
        { return Context::Get()->GetTarget()->GetPhysical(p); }

    // [P]
    private: void processCopyTasks(CopyTasks* pTemps)
    {
        CopyTasks oPendings;
        CopyTasks oReadies;

        CopyTasks* pPendings = &oPendings;
        CopyTasks* pReadies  = &oReadies;

        while (! pTemps->IsEmpty())
        {
            CopyTask* const pTask = pTemps->Pop();

            if (pTask->m_pRd == pTask->m_pRx)
            {
                pTask->m_pRx->m_cUses -= 1;
            }
            else if (pTask->m_pRd->m_cUses)
            {
                pPendings->Push(pTask);
            }
            else
            {
                pReadies->Push(pTask);
            }
        } // while

        for (;;)
        {
            while (! pReadies->IsEmpty())
            {
                CopyTask* const pTask = pReadies->Pop();
                emitCopy(pTask->m_pRd, pTask->m_pRx);
                ASSERT(0 == pTask->m_pRd->m_cUses);
                //ASSERT(pTask->m_pRx->m_cUses >= 1);
                pTask->m_pRx->m_cUses -= 1;

                ASSERT(pTemps->IsEmpty());
                while (! pPendings->IsEmpty())
                {
                    CopyTask* const pTask = pPendings->Pop();
                    if (0 == pTask->m_pRd->m_cUses)
                    {
                        pReadies->Push(pTask);
                    }
                    else
                    {
                        pTemps->Push(pTask);
                    }
                } // while pendings

                swap(pPendings, pTemps);
            } // while ready

            ASSERT(pTemps->IsEmpty());
            if (pPendings->IsEmpty())
            {
                // All tasks are finished or no more pending tasks.
                break;
            }

            // Free %rd since %rd is source of other tasks.
            {
                CopyTask* const pTask = pPendings->Pop();

                ASSERT(pTask->m_pRd->m_cUses >= 1);
                ASSERT(pTask->m_pRx->m_cUses >= 1);

                m_pBB->InsertBeforeI(
                    new SwapI(pTask->m_pRd, pTask->m_pRx),
                    m_pRefI );

                foreach (CopyTasks::Enum, oEnum, pPendings)
                {
                    CopyTask* const pPending = oEnum.Get();
                    if (pPending->m_pRx == pTask->m_pRd)
                    {
                        pPending->m_pRx = pTask->m_pRx;
                        ASSERT(pTask->m_pRd->m_cUses >= 1);
                        pTask->m_pRd->m_cUses -= 1;
                        pTask->m_pRx->m_cUses += 1;
                    }
                } // for pending

                ASSERT(pTemps->IsEmpty());
                while (! pPendings->IsEmpty())
                {
                    CopyTask* const pPending = pPendings->Pop();

                    if (pPending->m_pRd == pPending->m_pRx)
                    {
                        // ignore
                    }
                    else if (0 == pPending->m_pRd->m_cUses)
                    {
                        pReadies->Push(pPending);
                    }
                    else
                    {
                        pTemps->Push(pPending);
                    }
                } // while pending

                swap(pPendings, pTemps);
            } // free %rd
        } // for
    } // processCopyTasks

    private: void processLoadTasks(LoadTasks* pTasks)
    {
        while (! pTasks->IsEmpty())
        {
            LoadTask* pTask = pTasks->Pop();
            emitCopy(pTask->m_pRd, pTask->m_pMx);
        } // while
    } // processLoadTasks

    private: void processMemCopyTasks(MemCopyTasks* pTasks)
    {
        Physical* pRx = NULL;

        foreach (RegSet::Enum, oEnum, m_pRegSet)
        {
            Physical* pPx = getPhysical(oEnum.Get());

            if (0 == pPx->m_cUses)
            {
                pRx = pPx;
                break;
            }
        } // for reg

        ThreadSlot* pSpill = NULL;

        if (NULL == pRx)
        {
            pSpill = new ThreadSlot(
                GetRegClass(),
                offsetof(Thread, mv_value) );

            pRx = getPhysical(m_pRegSet->m_prgpReg[0]);

            emitCopy(pSpill, pRx);
        }

        while (! pTasks->IsEmpty())
        {
            MemCopyTask* pTask = pTasks->Pop();
            emitCopy(pRx, pTask->m_pMx);
            emitCopy(pTask->m_pMd, pRx);
        } // while

        if (NULL != pSpill)
        {
            emitCopy(pRx, pSpill);
        }
    } // processMemCopyTasks

    private: void processMemSetTasks(MemSetTasks* pTasks)
    {
        while (! pTasks->IsEmpty())
        {
            MemSetTask* pTask = pTasks->Pop();

            Physical* pPx = NULL;

            WorkList_<SetTask> oTemp;
            while (m_oSetTasks.IsEmpty())
            {
                SetTask* const pSetTask = m_oSetTasks.Pop();
                if (pSetTask->m_pSx->Equal(pTask->m_pSx))
                {
                    emitCopy(pSetTask->m_pRd, pTask->m_pSx);
                    if (NULL == pPx)
                    {
                        pPx = pSetTask->m_pRd;
                    }
                }
                else
                {
                    oTemp.Push(pSetTask);
                }
            } // while

            while (! oTemp.IsEmpty())
            {
                m_oSetTasks.Push(oTemp.Pop());
            } // while

            if (NULL == pPx)
            {
                emitCopy(pTask->m_pMd, pTask->m_pSx);
            }
            else
            {
                emitCopy(pTask->m_pMd, pPx);
            }
        } // while
    } // processMemSetTasks

    private: void processSetTasks(SetTasks* pTasks)
    {
        WorkList_<SetTask> oSoFar;

        while (! pTasks->IsEmpty())
        {
            SetTask* const pTask = pTasks->Pop();

            Physical* pPx = NULL;
            foreach (WorkList_<SetTask>::Enum, oEnum, &oSoFar)
            {
                if (oEnum.Get()->m_pSx->Equal(pTask->m_pSx))
                {
                    pPx = oEnum.Get()->m_pRd;
                    break;
                }
            } // for

            if (NULL == pPx)
            {
                emitCopy(pTask->m_pRd, pTask->m_pSx);
                oSoFar.Push(pTask);
            }
            else
            {
                emitCopy(pTask->m_pRd, pPx);
            }
        } // while
    } // processSetTasks

    private: void processStoreTasks(StoreTasks* pTasks)
    {
        while (! pTasks->IsEmpty())
        {
            StoreTask* const pTask = pTasks->Pop();
            emitCopy(pTask->m_pMd, pTask->m_pRx);
            pTask->m_pRx->m_cUses -= 1;
        } // while
    } // processStoreTasks

    // [R]
    public: void Reset()
    {
        foreach (RegSet::Enum, oEnum, m_pRegSet)
        {
            Physical* pPx = getPhysical(oEnum.Get());
            pPx->m_cUses = 0;
        } // for reg
    } // ParallelCopy

    public: void Run(Instruction* pRefI)
    {
        m_pRefI = pRefI;
        m_pBB   = pRefI->GetBBlock();

        CLOG(2, "<li>process tasks<ol>~%");

        processStoreTasks(&m_oStoreTasks);      // Md <= Rx
        processMemCopyTasks(&m_oMemCopyTasks);  // Md <= Mx
        processCopyTasks(&m_oCopyTasks);        // Rd <= Rx
        processLoadTasks(&m_oLoadTasks);        // Rd <= Md
        processMemSetTasks(&m_oMemSetTasks);    // Md <= Im
        processSetTasks(&m_oSetTasks);          // Rd <= Im

        CLOG(2, "</ol></li>~%");
    } // Run
}; // ParallelCopy

/// <summary>
///   Invert Phi instructions in layout order for next use position.
/// </summary>
class SubPassInvertPhi :
    public Pass_<SubPassInvertPhi, SubPass>
{
    public: static const char* GetName_() { return "Ra.InvertPhi"; }

    private: ParallelCopy    m_oParallelCopy;
    private: const RegGroup* m_pRegGroup;

    // ctor
    private: SubPassInvertPhi(const RegGroup* const pRegGroup) :
        m_oParallelCopy(pRegGroup),
        m_pRegGroup(pRegGroup) {}

    // Entry Point
    public: static void Run(
        Function*       const pFun,
        const RegGroup* const pRegGroup)
    {
        SubPassInvertPhi oPass(pRegGroup);
        oPass.Prepare();
        oPass.run(pFun);
    } // Run

    // [I]
    private: void invertPhi(BBlock* const pCurr, BBlock* const pSucc)
    {
        CLOG(1, "<h2>invertPhi ~S => ~S</h2><ol>~%", pCurr, pSucc);

        m_oParallelCopy.Reset();

        bool fHasTask = false;

        foreach (BBlock::EnumI, oEnum, pSucc)
        {
            PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI)
            {
                break;
            }

            Register* const pRd = pPhiI->GetRd();
            if (NULL == pRd)
            {
                continue;
            }

            if (pRd->GetRegClass() != m_oParallelCopy.GetRegClass())
            {
                continue;
            }

            CLOG(1, "<li>~S pd=~S spill=~S<ol>",
                pPhiI, pRd->m_pPhysical, pRd->m_pSpill );

            PhiOperandBox* pBox = pPhiI->GetOperandBox(pCurr);

            updatePhiOperand(pBox);

            if (Physical* const pPd = pRd->m_pPhysical)
            {
                m_oParallelCopy.AddTask(pPd, pBox);
                fHasTask = true;
            }
            else if (StackSlot* const pMd = pRd->m_pSpill)
            {
                m_oParallelCopy.AddTask(pMd, pBox);
                fHasTask = true;
            }
            else
            {
                COMPILER_INTERNAL_ERROR();
            }

            CLOG(1, "</ol></li>");
        } // for phi

        if (fHasTask)
        {
            m_oParallelCopy.Run(pCurr->GetLastI());
        }

        CLOG(1, "</ol>~%");
    } // invertPhi

    // [P]
    private: void processBBlock(BBlock* const pCurr)
    {
        foreach (BBlock::EnumSucc, oEnum, pCurr)
        {
            invertPhi(pCurr, oEnum.Get());
        } // for succ
    } // processBBlock

    // [R]
    private: void run(Function* const pFun)
    {
        CLOG(1, "<h1>Process/~A ~S</h1>", m_pRegGroup->m_pszName, pFun);
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            processBBlock(oEnum.Get());
        } // for bblock
    } // run

    // [U]
    private: void updatePhiOperand(
        PhiOperandBox* const pBox )
    {
        Register* const pRx = pBox->GetRx();

        if (NULL == pRx)
        {
            // Literal operand
            return;
        }

        if (NULL != pRx->m_pSpill)
        {
            // Spilled regiser
            return;
        }

        if (NULL == pRx->m_pPhysical)
        {
            COMPILER_INTERNAL_ERROR();
            return;
        }

        pBox->m_pPhysical = pRx->m_pPhysical;
    } // updatePhiOperand
}; // SubPassInvertPhi

/// <summary>
///   ParallelCopy instructions in layout order for checking next use
///   position.
/// </summary>
class SubPassParallelCopy :
    public Pass_<SubPassParallelCopy, SubPass>,
    protected InstructionDispatcher,
    protected Mm
{
    public: static const char* GetName_() { return "Ra.ParallelCopy"; }

    private: ParallelCopy       m_oParallelCopy;
    private: const RegGroup*    m_pRegGroup;
    private: Target*            m_pTarget;

    // ctor
    private: SubPassParallelCopy(const RegGroup* pRegGroup) :
        m_oParallelCopy(pRegGroup),
        m_pRegGroup(pRegGroup),
        m_pTarget(Context::Get()->GetTarget()) {}

    // Entry Point
    public: static void Run(Function* pFun, const RegGroup* pRegGroup)
    {
        SubPassParallelCopy oPass(pRegGroup);
        oPass.Prepare();
        oPass.run(pFun);
    } // Run

    // [G]
    private: static Physical* getPhysical(const RegDesc* p)
        { return Context::Get()->GetTarget()->GetPhysical(p); }

    // [I]
    private: static bool isFirstSelect(Instruction* pI)
    {
        Instruction* pPrevI = pI->GetPrev();
        if (NULL == pPrevI) return true;
        if (pPrevI->Is<SelectI>()) return false;
        if (pPrevI->Is<CountI>()) return false;
        return true;
    } // isFirstSelect

    // [P]
    private: void parallelCopy(Output* pOd, CgOutput* pSx)
    {
        if (Physical* pRd = pOd->DynamicCast<Physical>())
        {
            m_oParallelCopy.AddTask(pRd, pSx);
        }
        else if (MemSlot* pMd = pOd->DynamicCast<MemSlot>())
        {
            m_oParallelCopy.AddTask(pMd, pSx);
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }
    } // parallelCopy

    private: void processBBlock(BBlock* pBB)
    {
        CLOG(1, "<h2>process ~S</h2>", pBB);
        CLOG(2, "<ol>~%");
        foreach (BBlock::EnumI, oEnum, pBB)
        {
            dispatch(oEnum.Get());
        } // for insn
        CLOG(2, "</ol>~%");
    } // processBBlock

    private: void processCountAndSelect(Instruction* const pFirstI)
    {
        if (! isFirstSelect(pFirstI))
        {
            return;
        }

        CLOG(2, "<li>Process ~S<ol>~%", pFirstI);

        m_oParallelCopy.Reset();

        for (
            Instruction* pI = pFirstI;
            NULL != pI;
            pI = pI->GetNext() )
        {
            if (SelectI* const pSelectI = pI->DynamicCast<SelectI>())
            {
                CLOG(2, "<li>process ~S<ol>~%", pI);

                uint const nNth = static_cast<uint>(pI->GetSy()->
                    StaticCast<Integer>()->GetDatum() );

                CgOutput* const pSx = m_pTarget->GetArgReg(nNth);

                if (Physical* const pRx = pSx->DynamicCast<Physical>())
                {
                    parallelCopy(pSelectI->GetOutput(), pRx);
                }
                else if (ThreadSlot* const pMx =
                            pSx->DynamicCast<ThreadSlot>() )
                {
                    parallelCopy(pSelectI->GetOutput(), pMx);
                }
                else
                {
                    COMPILER_INTERNAL_ERROR();
                }

                CLOG(2, "</ol></li>~%");
            }
            else if (CountI* const pCountI = pI->DynamicCast<CountI>())
            {
                CLOG(2, "<li>process ~S<ol>~%", pI);

                parallelCopy(
                    pCountI->GetOutput(),
                    getPhysical(m_pRegGroup->m_pRn) );

                CLOG(2, "</ol></li>~%");
            }
            else
            {
                m_oParallelCopy.Run(pI);
                break;
            }
        } // for

        CLOG(2, "</ol></li>~%");
    } // processCountAndSelect

    // [R]
    private: void run(Function* pFun)
    {
        CLOG(1, "<h1>Process/~A ~S</h1>", m_pRegGroup->m_pszName, pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            processBBlock(oEnum.Get());
        } // for bblock
    } // run

    ////////////////////////////////////////////////////////////
    //
    // Instruction handlers
    //
    DefProcI(Count)
        { processCountAndSelect(pI); }

    DefProcI(Select)
        { processCountAndSelect(pI); }

    DefProcI(Values)
    {
        CLOG(2, "<li>process ~S<ol>~%", pI);

        m_oParallelCopy.Reset();

        uint nNth = 0;
        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            CgOutput* pOd = m_pTarget->GetArgReg(nNth);
            nNth += 1;

            if (Physical* pRd = pOd->DynamicCast<Physical>())
            {
                m_oParallelCopy.AddTask(pRd, oEnum.GetBox());
            }
            else if (ThreadSlot* pMd = pOd->DynamicCast<ThreadSlot>())
            {
                m_oParallelCopy.AddTask(pMd, oEnum.GetBox());
            }
            else
            {
                COMPILER_INTERNAL_ERROR();
            }
        } // for reg

        m_oParallelCopy.Run(pI);

        CLOG(2, "</ol></li>~%");
    } // Values
}; // SubPassParallelCopy

/// <summary>
///   Remove critical edges.
/// </summary>
class SubPassRemoveCriticalEdge :
    public Pass_<SubPassRemoveCriticalEdge, SubPass>
{
    public: static const char* GetName_() { return "Ra.RemoveCriticalEdge"; }

    // Entry Point
    public: static void Run(Function* const pFun)
    {
        SubPassRemoveCriticalEdge oPass;
        oPass.Prepare();
        oPass.run(pFun);
    } // Run

    // [H]
    private: bool hasPhi(BBlock* const pBB)
    {
        foreach (BBlock::EnumI, oEnum, pBB)
        {
            PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
            if (NULL == pPhiI) break;
            if (Register* const pRd = pPhiI->GetRd())
            {
                return true;
            }
        } // for phi
        return false;
    } // hasPhi

    // [R]
    private: void run(Function* const pFun)
    {
        WorkList_<BBlock> oPhiBBs;

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();
            if (hasPhi(pBB))
            {
                oPhiBBs.Push(pBB);
            }
        } // for bblock

        // Note: We don't need to remove critical edge for back edge if
        // phi operands aren't live out in other succs.
        while (! oPhiBBs.IsEmpty())
        {
            BBlock* const pCurr = oPhiBBs.Pop();
            BBlock::EnumPred oEnum(pCurr);
            while (! oEnum.AtEnd())
            {
                BBlock* pPred = oEnum.Get();
                oEnum.Next();
                pPred->RemoveCriticalEdge(pCurr);
            } // while
        } // while
    } // run
}; // SubPassRemoveCriticalEdge

/// <summary>
///  Sub pass for allocating specified register group.
/// </summary>
class SubPassRa :
    public    Pass_<SubPassRa, SubPass>,
    protected Mm
{
    public: static const char* GetName_() { return "Ra.Core"; }

    public: virtual const char* GetName() const override
        { return m_pRegGroup->m_pszName; }

    private: bool               m_fNoCalleeSave;
    private: const RegGroup*    m_pFprGroup;
    private: const RegGroup*    m_pRegGroup;
    private: RegList            m_oRegList;

    // ctor
    private: SubPassRa(
        const RegGroup* const pRegGroup,
        const RegGroup* const pFprGroup ) :
        //= <FIXME date="2009-01-10" by="yosi@msn.com">
        //=   m_fNoCalleeSave must be target specific.
        //= </FIXME>
        m_fNoCalleeSave(true),
        m_pFprGroup(pFprGroup),
        m_pRegGroup(pRegGroup) {}

    // Entry Point
    public: static bool Run(
        Function*       const pFun,
        const RegGroup* const pRegGroup,
        const RegGroup* const pFprGroup )
    {
        SubPassRa oPass(pRegGroup, pFprGroup);
        oPass.Prepare();
        return oPass.run(pFun);
    } // Run

    // [B]
    private: bool buildRegList(Function* const pFun)
    {
        bool fHasFpr = false;

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock*    const pBB  = oEnum.Get();
            BBlockExt* const pExt = new BBlockExt;

            pExt->m_pRegMap = new(this) RegMap(this, m_pRegGroup);
            pBB->SetWork(pExt);
            pBB->SetFlag(0);

            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* const pI = oEnum.Get();
                if (Register* const pRx = pI->GetRd())
                {
                    if (pRx->GetRegClass() == m_pRegGroup->m_eRegClass)
                    {
                        m_oRegList.Append(pRx);
                        pRx->SetFlag(0);
                    }
                    else if (pRx->GetRegClass() == m_pFprGroup->m_eRegClass)
                    {
                        fHasFpr = true;
                    }
                }

                if (pI->Is<CallI>())
                {
                    pBB->SetFlag(1);
                }
            } // for insn
        } // for bb

        return fHasFpr;
    } // buildRegList

    // [C]
    private: void computeSpillHint(Function* const pFun)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();
            if (! pBB->GetFlag())
            {
                // No CALL in this bblock
                continue;
            }

            foreach (RegList::Enum, oEnum, &m_oRegList)
            {
                Register* const pRx = oEnum.Get();
                if (pBB->IsLiveIn(pRx->GetIndex()) &&
                    pBB->IsLiveOut(pRx->GetIndex()) )
                {
                    pRx->SetFlag(1);
                }
            } // for reg
        } // for bb
    } // computeSpillHint

    // [R]
    private: bool run(Function* pFun)
    {
        CLOG(1, "<h1>Process/~A ~S</h1>", m_pRegGroup->m_pszName, pFun);

        bool fHasFpr = buildRegList(pFun);

        if (m_fNoCalleeSave)
        {
            computeSpillHint(pFun);
        }

        SubPassAllocate::Run(pFun, m_pRegGroup, &m_oRegList);
        SubPassInvertPhi::Run(pFun, m_pRegGroup);
        return fHasFpr;
    } // run
}; // SubPassRa

/// <summary>
///   Remove Phi instructions
/// </summary>
class SubPassRemovePhi :
    public Pass_<SubPassRemovePhi, SubPass>
{
    public: static const char* GetName_() { return "Ra.RemovePhi"; }

    // Entry Point
    public: static void Run(Function* const pFun)
    {
        SubPassRemovePhi oPass;
        oPass.Prepare();
        oPass.run(pFun);
    } // Run

    // [R]
    // See (defun foo (x) (multiple-value-call #'bar (if x (baz) (quux))))
    private: void run(Function* const pFun)
    {
        CLOG(1, "<h2>Remove Phi from ~S</h2>", pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();

            CLOG(1, "<h3>Process ~S</h3>", pBB);

            WorkList_<Instruction> oPhiIs;
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
                if (NULL == pPhiI)
                {
                    break;
                }

                if (! pPhiI->GetOutput()->Is<Values>())
                {
                    oPhiIs.Push(pPhiI);
                }
            } // for instruction

            while (! oPhiIs.IsEmpty())
            {
                pBB->RemoveI(oPhiIs.Pop());
            } // while
        } // for bblock
    } // run
}; // SubPassRemovePhi

} // CgRa

using namespace CgRa;

/// <summary>
///   Register allocator pass.
/// </summary>
class PassRa :
    public Pass_<PassRa, FunctionPass>
{
    public: static const char* GetName_() { return "Ra"; }

    private: virtual void processFunction(Function* pFun) override
    {
        SubPassRemoveCriticalEdge::Run(pFun);

        pFun->NumberInstructions();
        pFun->ComputePostDominance();
        pFun->ComputeLiveness();

        if (! Context::Get()->CanContinue())
        {
            return;
        }

        // Put Phi-operands to LiveOut for easy checking of live range.
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* const pBB = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                PhiI* const pPhiI = oEnum.Get()->DynamicCast<PhiI>();
                if (NULL == pPhiI)
                {
                    break;
                }

                if (NULL != pPhiI->GetRd())
                {
                    foreach (BBlock::EnumPred, oEnum, pBB)
                    {
                        BBlock* pPred = oEnum.Get();
                        if (Register* pRx = pPhiI->GetRx(pPred))
                        {
                            pPred->SetLiveOut(pRx->GetIndex());
                        }
                    } // for pred
                }
            } // for insn
        } // for block

        Target* pTarget = Context::Get()->GetTarget();

        bool const fHasFpr = SubPassRa::Run(
            pFun,
            pTarget->GetGprGroup(),
            pTarget->GetFprGroup() );

        if (fHasFpr)
        {
            SubPassRa::Run(
                pFun,
                pTarget->GetFprGroup(),
                pTarget->GetFprGroup() );
        }

        SubPassAssign::Run(pFun);

        SubPassParallelCopy::Run(pFun, pTarget->GetGprGroup());

        // Note: We don't support FPR in values.
        //SubPassParallelCopy::Run(pFun, pTarget->GetFprGroup());

        //= <FIXME date="2007-10-10" by="yosi@msn.com">
        //=   We need to eliminate
        //=     PHICOPY %r1 <= %m2
        //=     PHICOPY %r3 <= %m4
        //=     PHICOPY %m2 <= %r1
        //=     PHICOPY %m4 <= %r3
        //=     e.g. (lambda (a b ... g) (list a b ... g))
        //= </FIXME>

        SubPassRemovePhi::Run(pFun);

        // Remove COPY eax <= eax
        //pFun->Clean();
    } // processFunction
}; // PassRa

DEFPASS(Ra)

} // Compiler

} // TinyCl
