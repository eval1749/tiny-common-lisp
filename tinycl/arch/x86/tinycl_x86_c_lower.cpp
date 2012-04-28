#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Lowering
// tinycl_x86_c_lower.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_lower.cpp#33 $
//
#include "./tinycl_x86_c_cg.h"

#include "../../tinycl_clos.h"

#include "../../rtl/tinycl_float.h"

namespace TinyCl
{

using namespace Internal;

namespace Compiler
{

using namespace X86;


namespace Lower
{

class LowerDbBase
{
    // [G]
    protected: static Operand* getArg(
        Instruction* const pCallI,
        uint         const iNth )
    {
        Values* pVy = pCallI->GetVy();
        if (NULL == pVy)
        {
            return NULL;
        }

        ValuesI* pValuesI = pVy->GetDefI()->DynamicCast<ValuesI>();
        if (NULL == pValuesI)
        {
            return NULL;
        }

        if (iNth >= pValuesI->CountOperands())
        {
            return NULL;
        }

        return pValuesI->GetOperand(iNth);
    } // getArg

    public: struct Entry
    {
        Val m_name;
        void (*m_pfnLower)(Instruction*);
    }; // Entry
}; // LowerDbBase

class LowerDb :
    public FunDb_<LowerDb, LowerDbBase::Entry, 307>,
    public LowerDbBase
{
    public: static LowerDb* Create()
    {
        static Entry s_rgoEntry[] =
        {
            // [C]
            { Qchar_code,   lower_char_code },
            { Qchar_int,    lower_char_code },
        }; // s_rgoEntry

        LowerDb* pLowerDb = new LowerDb;
        pLowerDb->load(s_rgoEntry, lengthof(s_rgoEntry));
        return pLowerDb;
    } // Create

    ////////////////////////////////////////////////////////////
    //
    // Lowering Functions
    //

    // [C]
    private: static void lower_char_code(Instruction* const pCallI)
    {
        Operand* const pSx = pCallI->GetVy()->GetDefI()->
            GetOperandBox(0)->GetOperand();

        Register* const pR1 = new Register;

        pCallI->GetBB()->InsertBeforeI(
            new SubI(tyInt, pR1, pSx, Literal::New(CHAR_u0000)),
            pCallI );

        pCallI->GetBB()->ReplaceI(
            new ShrI(
                pCallI->GetTy(),
                pCallI->GetRd(),
                pR1,
                Integer::New(Character::ShiftCount - Fixnum::TagBits) ),
            pCallI );
    } // lower_char_code
}; // LowerDb;

} // Lower

using namespace Lower;

LowerDb* LowerDb::sm_pFunDb;

/// <summary>
///   Lowering pass for X86 ISA.
/// </summary>
class PassX86Lower :
    public    Pass_<PassX86Lower, FunctionPass>,
    protected InstructionDispatcher,
    protected X86Utility
{
    public: static const char* GetName_() { return "X86Lower"; }

    private: WorkList_<Instruction> m_oParseKeys;

    /// <summary>Entry point</summary>
    protected: virtual void processFunction(Function* pFun) override
    {
        CLOG(1, "<h2>process ~S</h2>~%", pFun);

        Physical* pPTrue = getGpr($true);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            CLOG_SECTION(2, "<h3>process ~S</h3>", oEnum.Get());

            BBlock* const pBB = oEnum.Get();
            BBlock::EnumI oEnumI(pBB);
            while (! oEnumI.AtEnd())
            {
                Instruction* const pI = oEnumI.Get();
                foreach (Instruction::EnumOperand, oEnum, pI)
                {
                    OperandBox* pBox = oEnum.GetBox();
                    if (pBox->GetOperand() == True)
                    {
                        pBox->Replace(pPTrue);
                    }
                } // for operand
                oEnumI.Next();

                CLOG_SECTION(3, "process ~S", pI);
                dispatch(pI);
            } // for each insn
        } // for each bb

        CLOG(2, "<h3>process PARSEKEYS</h3>~%");
        while (! m_oParseKeys.IsEmpty())
        {
            processParseKeys(m_oParseKeys.Pop());
        }

        pFun->Clean();
    } // processFunction

    // [C]
    private: static FrameReg* computeOuterFrame(FrameReg* pFx)
    {
        for (
            FrameReg* pRunner = pFx->GetOuter();
            NULL != pRunner;
            pRunner = pRunner->GetOuter() )
        {
            if (Instruction* pDefI = pRunner->GetDefI())
            {
                if (pDefI->GetBB()->GetFunction() !=
                    pFx->GetDefI()->GetBB()->GetFunction() )
                {
                    // pFx is defined in inner function.
                    return NULL;
                }
                return pRunner;
            }
        } // for
        return NULL;
    } // computeOuterFrame

    // [I]
    /// <summary>
    ///   Inserts activate frame instructions if pOpenI is inner most
    ///   OPENXXX instruction of consecutive OPENXXX instructions.
    /// </summary>
    private: void insertActivateFrame(
        Instruction* const pOpenI,
        Instruction* const pRefI )
    {
        CLOG_SECTION(3, "Activate Frame");

        if (pRefI->GetOutput()->Is<FrameReg>())
        {
            CLOG(3, "<li>middle of consecutive frames</li>");
            return;
        }

        BBlock*   const pBB   = pRefI->GetBB();
        FrameReg* const pFx   = pOpenI->GetOutput()->StaticCast<FrameReg>();

        Register* const pR1 = new Register;
        pBB->InsertBeforeI(
            new x86LeaI(pR1, pFx, offsetof(Frame, m_pOuter)),
            pRefI );

        Pseudo* const pQ2 = new Pseudo;
        pBB->InsertBeforeI(
            new x86LeaI(pQ2, getTcb(), offsetof(Thread, m_fp)),
            pRefI );

        pBB->InsertBeforeI(
            new StoreI(pQ2, pR1),
            pRefI );
    } // insertActivateFrame

    /// <summary>
    ///   Inserts Exit Point frame.
    ///   <list>
    ///     <item><description>
    ///       1 save sp
    ///     </description></item>
    ///     <item><description>
    ///       2 set frame owner
    ///     </description></item>
    ///     <item><description>
    ///       3 set nonlocal exit point (label)
    ///     </description></item>
    ///     <item><description>
    ///         4 set exit point name (block name or catch tag)
    ///     </description></item>
    ///     <item><description>
    ///         5 reset saved values
    ///     </description></item>
    ///   </list>
    /// </summary>
    private: void insertExitPointFrame(
        Instruction* const pOpenI,
        Instruction* const pRefI )
    {
        CLOG_SECTION(3, "Exit Point Frame");

        FrameReg* const pFd = pOpenI->GetOutput()->StaticCast<FrameReg>();

        BBlock* const pBB = pRefI->GetBB();

        // Save SP
        Pseudo* const pQ1 = new Pseudo;
        pBB->InsertBeforeI(
            new x86LeaI(pQ1, pFd, offsetof(XferFrame, m_sp)),
            pRefI );
        pBB->InsertBeforeI(new StoreI(pQ1, getGpr($sp)), pRefI);

        // Set frame owner
        Pseudo* const pQ2 = new Pseudo;
        pBB->InsertBeforeI(
            new x86LeaI(pQ2, pFd, offsetof(XferFrame, m_fn)),
            pRefI );
        pBB->InsertBeforeI(
            new StoreI(pQ2, new FunLit(pBB->GetFunction())),
            pRefI );
    } // insertExitPointFrame

    private: void insertOpenFrame(
        Instruction* const pOpenI,
        Frame::Type  const eType,
        Instruction* const pRefI )
    {
        CLOG_SECTION(3, "Open Frame");

        BBlock*   const pBB = pRefI->GetBB();
        FrameReg* const pFx = pOpenI->GetOutput()->StaticCast<FrameReg>();

        Register* const pR1 = new Register;
        if (FrameReg* const pFy = computeOuterFrame(pFx))
        {
            CLOG_SECTION(3, "Inner frame");
            pBB->InsertBeforeI(
                new x86LeaI(pR1, pFy, offsetof(Frame, m_pOuter)),
                pRefI );
        }
        else
        {
            CLOG_SECTION(3, "Outer most frame");

            Pseudo* const pQ2 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ2, getTcb(), offsetof(Thread, m_fp)),
                pRefI );

            pBB->InsertBeforeI(
                new LoadI(pR1, pQ2),
                pRefI );
        }

        {
            CLOG_SECTION(3, "Set Frame.m_pOuter");

            Pseudo* const pQ3 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ3, pFx, offsetof(Frame, m_pOuter)),
                pRefI );

            pBB->InsertBeforeI(
                new StoreI(pQ3, pR1),
                pRefI );
        }

        {
            CLOG_SECTION(3, "Set frame type");

            Pseudo* const pQ4 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ4, pFx, offsetof(Frame, m_eFrame)),
                pRefI );

            pBB->InsertBeforeI(
                new StoreI(pQ4, new Integer(eType)),
                pRefI );
        }
    } // insertOpenFrame

    /// <summary>
    ///   Inserts instructions for loading address of variable cell.
    /// </summary>
    private: Pseudo* insertVarCell(
        BindOperandBox* const pBox,
        Instruction*    const pRefI )
    {
        Val const cell = pBox->GetVarCell();
        Pseudo* const pQ1 = new Pseudo;
        if (value_cell_p(cell))
        {
            pRefI->GetBB()->InsertBeforeI(
                new SlotI(
                    tyPtrT,
                    pQ1,
                    CLASS_value_cell,
                    Qvalue,
                    Literal::New(cell) ),
                pRefI );
        }
        else if (tlv_record_p(cell))
        {
            pRefI->GetBB()->InsertBeforeI(new TlvI(pQ1, cell), pRefI);
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }

        return pQ1;
    } // insertVarCell

    // [P]
    private: int positionOfKey(
        Values* const pVd,
        Val     const key )
    {
        ParseKeysI* const pParseKeysI =
            pVd->GetDefI()->StaticCast<ParseKeysI>();

        int iPosn = 0;
        Instruction::EnumOperand oEnum(pParseKeysI);
        oEnum.Next();
        oEnum.Next();
        while (! oEnum.AtEnd())
        {
            Val present = oEnum.Get()->StaticCast<Literal>()->GetDatum();
            if (present == key)
            {
                return iPosn;
            }
            iPosn += 1;
            oEnum.Next();
        } // while

        COMPILER_INTERNAL_ERROR();
        return 0;
    } // positionOfKey

    private: void processParseKeys(Instruction* pI)
    {
        Operand* pRx;
        Val parser;
        Val keys;
        {
            Instruction::EnumOperand oEnum(pI);

            pRx = oEnum.Get();
            oEnum.Next();

            Val const kind = oEnum.Get()->StaticCast<Literal>()->GetDatum();
            oEnum.Next();
            if (kind == QAkey)
            {
                parser = QBAkey;
            }
            else if (kind == QAallow_other_keys)
            {
                parser = QBAallow_other_keys;
            }
            else
            {
                COMPILER_INTERNAL_ERROR();
                parser = nil;
            }

            int const cKeys = pI->CountOperands() - 2;
            if (cKeys >= Fixnum::Bits)
            {
                Context::Get()->Error("Too many keys");
            }

            keys = make_vector(Fixnum::Encode(cKeys));

            Val index = zero;
            while (! oEnum.AtEnd())
            {
                setf_svref(
                    oEnum.Get()->StaticCast<Literal>()->GetDatum(),
                    keys,
                    index );
                index = add(index, one);
                oEnum.Next();
            } // while
        }
        Values*  const pVx = new Values;
        ValuesI* pValuesI = new ValuesI(pVx);
        pValuesI->AppendOperand(pRx);
        pValuesI->AppendOperand(Literal::New(keys));

        pI->GetBB()->InsertBeforeI(pValuesI, pI);

        pI->GetBB()->ReplaceI(
            new CallI(tyFixnum, pI->GetVd(), parser, pVx),
            pI );
    } // processParseKeys

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //

    // [B]
    DefProcI(Box)
    {
        Operand* const pSx = pI->GetSx();
        if (Float* pFx = pSx->DynamicCast<Float>())
        {
            if (pFx->GetDefI()->GetTy() == tyFloat32)
            {
                pI->GetBB()->InsertBeforeI(
                    new UseI(pI->GetRx()),
                    pI );

                pI->GetBB()->ReplaceI(
                    new CallI(pI->GetTy(), pI->GetRd(), QBbox_float32),
                    pI );
                return;
            } // if float32

            if (pFx->GetDefI()->GetTy() == tyFloat64)
            {
                pI->GetBB()->InsertBeforeI(
                    new UseI(pI->GetRx()),
                    pI );

                pI->GetBB()->ReplaceI(
                    new CallI(pI->GetTy(), pI->GetRd(), QBbox_float64),
                    pI );
                return;
            } // if float64
        } // if %fx
    } // Box

    // [C]
    /// <summary>
    ///   <list>
    ///     <item><term>1</term>
    ///       <description>
    ///         Signal note for function SLOT-VALUE.
    ///       </description>
    ///     </item>
    ///   </list>
    /// </summary>
    DefProcI(Call)
    {
        FunName* const pFunName = pI->GetSx()->DynamicCast<FunName>();
        if (NULL == pFunName)
        {
            return;
        }

        const LowerDb::Entry* const pEntry =
            LowerDb::Find(pFunName->GetName());

        if (NULL == pEntry)
        {
            return;
        }

        Values* const pVy = pI->GetVy();
        if (NULL == pVy)
        {
            // Note: We set %void in CG pass.
            return;
        }

        ValuesI* const pValuesI = pVy->GetDefI()->DynamicCast<ValuesI>();
        if (NULL == pValuesI)
        {
            return;
        }

        pEntry->m_pfnLower(pI);
    } // Call

    DefProcI(Close)
    {
        FrameReg* const pFx = pI->GetSx()->StaticCast<FrameReg>();

        if (pFx->GetFrameKind() == Qlet)
        {
            int ofs = sizeof(BindFrame);
            foreach (Instruction::EnumOperand, oEnum, pFx->GetDefI())
            {
                BindOperandBox* pBox = oEnum.GetBox()->
                    StaticCast<BindOperandBox>();

                Pseudo*   const pQ1 = new Pseudo;
                Register* const pR2 = new Register;

                pI->GetBB()->InsertBeforeI(
                    new x86LeaI(
                        pQ1,
                        pFx,
                        offsetof(BindFrame::Entry, m_value) + ofs ),
                    pI );

                pI->GetBB()->InsertBeforeI(new LoadI(pR2, pQ1), pI);
                Pseudo* const pQ3 = insertVarCell(pBox, pI);
                pI->GetBB()->InsertBeforeI(new StoreI(pQ3, pR2), pI);

                ofs += sizeof(BindFrame::Entry);
            } // for each operand
        }

        if (pI->GetNext()->Is<CloseI>())
        {
            // We only emit restore $tcb.m_fp if this is end of consecutive
            // close.
            CLOG(3, "<li>This is middle of consecutive CLOSEs.</li>");
            return;
        }

        BBlock* const pBB = pI->GetBBlock();
        Instruction* const pRefI = pI;

        // Restore $tcb.m_fp
        Pseudo* const pQ1 = new Pseudo;
        pBB->InsertBeforeI(
            new x86LeaI(pQ1, pFx, offsetof(Frame, m_pOuter)),
            pRefI );

        Register* const pR2 = new Register;
        pBB->InsertBeforeI(
            new LoadI(pR2, pQ1),
            pRefI );

        Pseudo* const pQ3 = new Pseudo;
        pBB->InsertBeforeI(
            new x86LeaI(pQ3, getTcb(), offsetof(Thread, m_fp)),
            pRefI );

        pBB->InsertBeforeI(
            new StoreI(pQ3, pR2),
            pRefI );
    } // Close

    DefProcI(Convert)
    {
        Operand* const pSx = pI->GetSx();
        const Type* const pTx = pSx->GetTy();
        if (pTx == tyFloat32 || pTx == tyFloat64)
        {
            pI->GetBB()->ReplaceI(
                new x86CvtFloat(pI->GetTy(), pI->GetRd(), pSx),
                pI );
        }
        else
        {
            pI->GetBB()->ReplaceI(
                new x86CvtInt(pI->GetTy(), pI->GetRd(), pSx),
                pI );
        }
    } // Convert

    /// <summary>
    ///   Lower (logeqv x y) = (lognot (logxor x y))
    /// </summary>
    DefProcI(LogEqv)
    {
        Register* const pR1 = new Register;

        pI->GetBB()->InsertBeforeI(
            new LogXorI(pI->GetTy(), pR1, pI->GetSx(), pI->GetSy()),
            pI );

        Operand* const pMinusOne =
            pI->GetTy() == tyInt ?
                static_cast<Operand*>(Integer::New(-1)) :
                static_cast<Operand*>(Literal::New(Fixnum::Encode(-1)));

        pI->GetBB()->ReplaceI(
            new LogXorI(pI->GetTy(), pI->GetOutput(), pR1, pMinusOne),
            pI );
    } // Eqv

    DefProcI(Frame)
    {
        FrameReg* const pFx = pI->GetSx()->StaticCast<FrameReg>();
        pI->GetBB()->ReplaceI(
            new x86LeaI(pI->GetRd(), pFx, 0),
            pI );
    } // Frame

    DefProcI(KeySupplied)
    {
        int const iPosn = positionOfKey(
            pI->GetRx()->GetDefI()->GetVx(),
            pI->GetLy() );

        Pseudo* const pQ1 = new Pseudo;
        pI->GetBB()->InsertBeforeI(
            new x86TestI(pQ1, pI->GetRx(), Fixnum::Encode(1 << iPosn)),
            pI );

        pI->GetBB()->ReplaceI(
            new x86BoolI(pI->GetBd(), pQ1, tttn_NE),
            pI );
    } // KeySupplied

    DefProcI(KeyVal)
    {
        int const iPosn = positionOfKey(pI->GetVx(), pI->GetLy());
        pI->GetBB()->ReplaceI(
            new SelectI(tyT, pI->GetRd(), pI->GetVx(), iPosn + 1),
            pI );
    } // KeyVal

    DefProcI(OpenBind)
    {
        Instruction* const pRefI = pI->GetNext();

        BBlock* const pBB = pRefI->GetBB();
        FrameReg* const pFx = pI->GetOutput()->StaticCast<FrameReg>();

        // BindFrame.m_n
        {
            Pseudo* const pQ1 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ1, pFx, offsetof(BindFrame, m_n)),
                pRefI );

            Val const nbinds = Fixnum::Encode(pI->CountOperands());
            pBB->InsertBeforeI(
                new StoreI(pQ1, Literal::New(nbinds)),
                pRefI );
        }

        // BindFrame.m_rgoEntry
        int ofs = sizeof(BindFrame);
        foreach (Instruction::EnumOperand, oEnum, pI)
        {
            BindOperandBox* const pBox = oEnum.GetBox()->
                StaticCast<BindOperandBox>();

            // Save current value
            {
                Register* const pR1 = new Register;
                Pseudo* const pQ2 = insertVarCell(pBox, pRefI);
                pBB->InsertBeforeI(new LoadI(pR1, pQ2), pRefI);

                Pseudo* const pQ3 = new Pseudo;
                pBB->InsertBeforeI(
                    new x86LeaI(
                        pQ3,
                        pFx,
                        offsetof(BindFrame::Entry, m_value) + ofs ),
                    pRefI );

                pBB->InsertBeforeI(new StoreI(pQ3, pR1), pRefI);
            }

            // Set new value
            {
                Pseudo* const pQ4 = insertVarCell(pBox, pRefI);
                pBB->InsertBeforeI(
                    new StoreI(pQ4, pBox->GetOperand()),
                    pRefI );
            }

            // Set variable name
            {
                Pseudo* const pQ5 = new Pseudo;
                pBB->InsertBeforeI(
                    new x86LeaI(
                        pQ5,
                        pFx,
                        offsetof(BindFrame::Entry, m_name) + ofs ),
                    pRefI );

                Val const cell = pBox->GetVarCell();
                if (value_cell_p(cell))
                {
                    pBB->InsertBeforeI(
                        new StoreI(pQ5, Literal::New(cell)),
                        pRefI );
                }
                else if (tlv_record_p(cell))
                {
                    pBB->InsertBeforeI(
                        new StoreI(pQ5, new TlvOffset(cell)),
                        pRefI );
                }
                else
                {
                    COMPILER_INTERNAL_ERROR();
                }
            }

            ofs += sizeof(BindFrame::Entry);
        } // for each operand

        insertOpenFrame(pI, Frame::Type_Bind, pRefI);
        insertActivateFrame(pI, pRefI);
    } // OpenBind

    private: void processBlockOrCatch(
        Instruction* const pI,
        Frame::Type  const eFrame )
    {
        Instruction* pRefI = pI->GetNext();
        insertOpenFrame(pI, eFrame, pRefI);
        insertExitPointFrame(pI, pRefI);

        BBlock* const pBB = pRefI->GetBB();

        FrameReg* const pFd = pI->GetOutput()->StaticCast<FrameReg>();

        {
            CLOG_SECTION(3, "Set exit point");
            Pseudo* const pQ3 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ3, pFd, offsetof(XferFrame, m_nIp)),
                pRefI );
            pBB->InsertBeforeI(
                new StoreI(pQ3, pI->GetSy()),
                pRefI );
        }

        {
            CLOG_SECTION(3, "Set exit point name");
            Pseudo* const pQ4 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ4, pFd, offsetof(XferFrame, m_name)),
                pRefI );
            pBB->InsertBeforeI(
                new StoreI(pQ4, pI->GetSx()),
                pRefI );
        }

        {
            CLOG_SECTION(3, "Reset saved values");
            Pseudo* const pQ5 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ5, pFd, offsetof(XferFrame, m_n)),
                pRefI );
            pBB->InsertBeforeI(
                new StoreI(pQ5, Literal::New(zero)),
                pRefI );
        }

        insertActivateFrame(pI, pRefI);
    } // processBlockOrCatch

    DefProcI(OpenBlock) {  processBlockOrCatch(pI, Frame::Type_Block); }
    DefProcI(OpenCatch) {  processBlockOrCatch(pI, Frame::Type_Catch); }

    DefProcI(OpenFinally)
    {
        Instruction* const pRefI = pI->GetNext();

        BBlock* const pBB = pRefI->GetBB();
        FrameReg* const pFd = pI->GetOutput()->StaticCast<FrameReg>();

        {
            CLOG_SECTION(3, "FinallyFrame.m_finally");
            Pseudo* const pQ1 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ1, pFd, offsetof(FinallyFrame, m_finally)),
                pRefI );

            Function* const pFinFun = pI->GetSx()->StaticCast<Function>();

            pBB->InsertBeforeI(
                new StoreI(pQ1, new FunLit(pFinFun)),
                pRefI );
        }

        ValuesI* const pValuesI =
            pI->GetVy()->GetDefI()->StaticCast<ValuesI>();

        {
            CLOG_SECTION(3, "FinallyFrame.m_n");
            Pseudo* const pQ2 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ2, pFd, offsetof(FinallyFrame, m_n)),
                pRefI );

            Literal* pNArgs = Literal::New(
                Fixnum::Encode(pValuesI->CountOperands()) );

            pBB->InsertBeforeI(
                new StoreI(pQ2, pNArgs),
                pRefI );

            pI->GetOperandBox(1)->Replace(pNArgs);
        }

        {
            CLOG_SECTION(3, "Set arguments for finally");
            int ofs = sizeof(FinallyFrame);
            foreach (Instruction::EnumOperand, oEnum, pValuesI)
            {
                Pseudo* const pQ3 = new Pseudo;
                pBB->InsertBeforeI(
                    new x86LeaI(pQ3, pFd, ofs),
                    pRefI );

                pBB->InsertBeforeI(
                    new StoreI(pQ3, oEnum.Get()),
                    pRefI );

                ofs += sizeof(Val);
            } // for
        }

        insertOpenFrame(pI, Frame::Type_Finally, pRefI);
        insertActivateFrame(pI, pRefI);
    } // OpenFinally

    DefProcI(OpenHandler)
    {
        Instruction* const pRefI = pI->GetNext();

        BBlock* const pBB = pRefI->GetBB();
        FrameReg* const pFx = pI->GetOutput()->StaticCast<FrameReg>();

        {
            CLOG_SECTION(3, "HanderFrame.m_n");
            Pseudo* const pQ1 = new Pseudo;
            pBB->InsertBeforeI(
                new x86LeaI(pQ1, pFx, offsetof(HandlerFrame, m_cbCatchs)),
                pRefI );

            Val const nbinds = Fixnum::Encode(pI->CountOperands());
            pBB->InsertBeforeI(
                new StoreI(pQ1, Literal::New(nbinds)),
                pRefI );
        }

        {
            CLOG_SECTION(3, "HandlerFrame.m_rgoCatch");

            int ofs = sizeof(HandlerFrame);
            foreach (Instruction::EnumOperand, oEnum, pI)
            {
                Pseudo* const pQ2 = new Pseudo;
                pBB->InsertBeforeI(
                    new x86LeaI(pQ2, pFx, ofs),
                    pRefI );

                pBB->InsertBeforeI(
                    new StoreI(pQ2, oEnum.Get()),
                    pRefI );

                ofs += sizeof(Val);
            } // for operand
        }

        insertOpenFrame(pI, Frame::Type_Handler, pRefI);
        insertActivateFrame(pI, pRefI);
    } // OpenHandler

    DefProcI(OpenTags)
    {
        Instruction* pRefI = pI->GetNext();
        insertOpenFrame(pI, Frame::Type_Tagbody, pRefI);
        insertExitPointFrame(pI, pRefI);

        FrameReg* const pFd = pI->GetOutput()->StaticCast<FrameReg>();

        int ofs = sizeof(TagsFrame);

        {
            CLOG_SECTION(3, "Emit tags");

            foreach (FrameReg::EnumUser, oEnum, pFd)
            {
                if (TagDefI* const pTagDefI = oEnum.Get()->GetI()->
                        DynamicCast<TagDefI>() )
                {
                    Pseudo* const pQ1 = new Pseudo;
                    pRefI->GetBB()->InsertBeforeI(
                        new x86LeaI(pQ1, pFd,
                            ofs + offsetof(TagsFrame::Tag, m_name) ),
                        pRefI );

                    pRefI->GetBB()->InsertBeforeI(
                        new StoreI(pQ1, pTagDefI->GetSz()),
                        pRefI );

                    Pseudo* const pQ2 = new Pseudo;
                    pRefI->GetBB()->InsertBeforeI(
                        new x86LeaI(pQ2, pFd,
                            ofs + offsetof(TagsFrame::Tag, m_nIp) ),
                        pRefI );

                    pRefI->GetBB()->InsertBeforeI(
                        new StoreI(pQ2, pTagDefI->GetSy()),
                        pRefI );

                    Instruction* const pNewI =
                        pTagDefI->GetBB()->InsertBeforeI(
                            new x86LeaI(pTagDefI->GetRd(), pFd, ofs),
                            pTagDefI );

                    pTagDefI->SetOutput(Void);

                    if (pTagDefI == pRefI)
                    {
                        pRefI = pNewI;
                    }

                    ofs += sizeof(TagsFrame::Tag);
                } // if
            } // for each user

            ASSERT(sizeof(TagsFrame) != ofs);
        }

        {
            CLOG_SECTION(3, "Set size of frame");
            Pseudo* const pQ1 = new Pseudo;
            pRefI->GetBB()->InsertBeforeI(
                new x86LeaI(pQ1, pFd, offsetof(TagsFrame, m_cbTags)),
                pRefI );
            pRefI->GetBB()->InsertBeforeI(
                new StoreI(pQ1, Integer::New(ofs - sizeof(TagsFrame))),
                pRefI );
        }

        insertActivateFrame(pI, pRefI);
    } // OpenTags

    DefProcI(OpenTry)
    {
        Instruction* const pRefI = pI->GetNext();
        insertOpenFrame(pI, Frame::Type_TryCatch, pRefI);
        insertExitPointFrame(pI, pRefI);

        FrameReg* pFd = pI->GetOutput()->StaticCast<FrameReg>();

        int ofs = sizeof(TryCatchFrame);

        foreach (FrameReg::EnumUser, oEnum, pFd)
        {
            if (CatchI* const pCatchI = oEnum.Get()->GetI()->
                    DynamicCast<CatchI>() )
            {
                Pseudo* const pQ1 = new Pseudo;
                pRefI->GetBB()->InsertBeforeI(
                    new x86LeaI(pQ1, pFd,
                        ofs + offsetof(TryCatchFrame::Catch, m_type) ),
                    pRefI );
                pRefI->GetBB()->InsertBeforeI(
                    new StoreI(pQ1, pCatchI->GetSz()),
                    pRefI );

                Pseudo* const pQ2 = new Pseudo;
                pRefI->GetBB()->InsertBeforeI(
                    new x86LeaI(pQ2, pFd,
                        ofs + offsetof(TryCatchFrame::Catch, m_nIp) ),
                    pRefI );
                pRefI->GetBB()->InsertBeforeI(
                    new StoreI(pQ2, pCatchI->GetSy()),
                    pRefI );

                ofs += sizeof(TryCatchFrame::Catch);
            } // if
        } // for each user

        ASSERT(sizeof(TryCatchFrame) != ofs);

        // Set size of frames
        {
            Pseudo* const pQ1 = new Pseudo;
            pRefI->GetBB()->InsertBeforeI(
                new x86LeaI(pQ1, pFd, offsetof(TryCatchFrame, m_cbCatchs)),
                pRefI );
            pRefI->GetBB()->InsertBeforeI(
                new StoreI(pQ1, Integer::New(ofs - sizeof(TryCatchFrame))),
                pRefI );
        }

        insertActivateFrame(pI, pRefI);
    } // OpenTry

    DefProcI(ParseKeys)
    {
        m_oParseKeys.Push(pI);
    } // ParseKeys

    DefProcI(Slot)
    {
        Register* pRz = pI->GetRz();
        if (NULL == pRz)
        {
            return;
        }

        Val const klass = pI->GetLx();

        Val const slots = klass->StaticCast<Instance>()->
            GetStorage<Layout_class>()->m_slots;

        if (nil == slots)
        {
            Context::Get()->Error(
                "~S doesn't have effective slto definitions.",
                klass );
            return;
        }

        int ofs;
        if (typep(klass, Qbuilt_in_class))
        {
            if (CLASS_cons == klass)
            {
                ofs = -Cons::Tag2;
            }
            else if (CLASS_symbol == klass)
            {
                Register* const pR1 = new Register;
                pI->GetBB()->InsertBeforeI(
                    new LogAndI(tyInt, pR1, pRz, Integer::New(~Arch32::TagMask)),
                    pI );
                pRz = pR1;
                ofs = 0;
            }
            else if (CLASS_native_code_function == klass ||
                     CLASS_native_code_closure  == klass )
            {
                ofs = -FunObj::Tag;
            }
            else
            {
                ofs = -Record::Tag;
            }
        }
        else if (typep(klass, Qfuncallable_standard_class))
        {
            Pseudo* const pQ1 = new Pseudo;
            pI->GetBB()->InsertBeforeI(
                new x86LeaI(
                    pQ1,
                    pRz,
                    offsetof(
                        Layout_funcallable_instance, m_storage) -
                        FunObj::Tag ),
                pI );

            pRz = new Register;
            pI->GetBB()->InsertBeforeI(new LoadI(pRz, pQ1), pI);

            ofs = sizeof(Layout_storage) - Record::Tag;
        }
        else if (typep(klass, Qstandard_class))
        {
            Pseudo* const pQ1 = new Pseudo;
            pI->GetBB()->InsertBeforeI(
                new x86LeaI(
                    pQ1,
                    pRz,
                    offsetof(Instance, m_storage) - Instance::Tag ),
                pI );

            pRz = new Register;
            pI->GetBB()->InsertBeforeI(new LoadI(pRz, pQ1), pI);

            ofs = sizeof(Layout_storage) - Record::Tag;
        }
        else if (typep(klass, Qstructure_class))
        {
            ofs = -Record::Tag;
        }
        else if (typep(klass, Qpseudo_class))
        {
            // Compiler psuedo class = literal-cell or stack-cell.
            return;
        }
        else
        {
            // Unknown metaclass
            COMPILER_INTERNAL_ERROR();
            return;
        }

        Val const slot_name = pI->GetLy();
        Val const eslotd = find_slot(klass, slot_name);
        if (nil == eslotd)
        {
            COMPILER_INTERNAL_ERROR();
            return;
        }

        Layout_effective_slot_definition* const pESlotD =
            eslotd->StaticCast<Instance>()->
                GetStorage<Layout_effective_slot_definition>();

        Int const iLoc = Fixnum::Decode_(pESlotD->m_location);

        ofs += static_cast<int>(iLoc * sizeof(Val));

        Pseudo* pR1 = new Pseudo;

        pI->GetRd()->ReplaceAll(pR1);

        pI->GetBB()->ReplaceI(
            new x86LeaI(pR1, pRz, ofs),
            pI );
    } // Slot

    /// <summary>
    ///   <list>
    ///     <item><term>float64</term>
    ///       <description>
    ///         UNBOX float64 %f1 &lt;= literal
    ///         =>
    ///         ENCODE float64 %f &lt;= literal[63:32] if literal[31:0] == 0
    ///       </description>
    ///     </item>
    ///   </list>
    /// </summary>
    DefProcI(UnBox)
    {
        if (pI->GetTy() == tyInt || pI->GetTy() == tyInt32)
        {
            pI->GetBB()->ReplaceI(
                new ShrI(pI->GetTy(), pI->GetRd(),
                        pI->GetSx(), Integer::New(Fixnum::TagBits) ),
                pI );
            return;
        } // int

        if (pI->GetTy() == tyFloat32)
        {
            if (Literal* const pLx = pI->GetSx()->DynamicCast<Literal>())
            {
                Arch::Float32* const p = reinterpret_cast<Arch::Float32*>(
                    &pLx->GetDatum()->StaticCast<Float32Impl>()->m_flt );

                if (0 == p->m_i)
                {
                    pI->GetBB()->ReplaceI(
                        new x86ZeroI(tyFloat32, pI->GetRd()),
                        pI );
                }
                else
                {
                    Register* const pR1 =
                        new Register(pI->GetRd()->GetVar());

                    pI->GetBB()->InsertBeforeI(
                        new CopyI(tyUInt32, pR1, Integer::New(p->m_i)),
                        pI );

                    pI->GetBB()->ReplaceI(
                        new x86EncodeI(tyFloat32, pI->GetRd(), pR1),
                        pI );
                } // if not zero
            } // if literal

            return;
        } // float32

        if (pI->GetTy() == tyFloat64)
        {
            if (Literal* const pLx = pI->GetSx()->DynamicCast<Literal>())
            {
                Arch::Float64* const p = reinterpret_cast<Arch::Float64*>(
                    &pLx->GetDatum()->StaticCast<Float64Impl>()->m_dbl );

                if (0 == p->m_hl.l)
                {
                    if (0 == p->m_hl.h)
                    {
                        pI->GetBB()->ReplaceI(
                            new x86ZeroI(tyFloat64, pI->GetRd()),
                            pI );
                    }
                    else
                    {
                        Register* const pR1 =
                            new Register(pI->GetRd()->GetVar());

                        pI->GetBB()->InsertBeforeI(
                            new CopyI(tyUInt32, pR1, Integer::New(p->m_hl.h)),
                            pI );

                        pI->GetBB()->ReplaceI(
                            new x86EncodeI(tyFloat64, pI->GetRd(), pR1),
                            pI );
                    }
                }
            } // if literal
            return;
        } // float64
    } // UnBox

    DefProcI(VarDef)
    {
        Variable* const pVar = pI->GetSx()->StaticCast<Variable>();
        switch (pVar->GetStorage())
        {
        case Variable::Storage_Closed:
        {
            Values* const pVy = new Values;
            pI->GetBB()->InsertBeforeI(
                new ValuesI(pVy, pI->GetSy()),
                pI );


            Register* const pRd = pI->GetRd();
            pI->GetBB()->InsertBeforeI(
                new CallI(tyT, pRd, Qmake_closed_cell, pVy),
                pI );

            if (VarHomeI* pVarHomeI = pI->GetNext()->DynamicCast<VarHomeI>())
            {
                Pseudo* const pQd = new Pseudo;
                pI->SetOutput(pQd);
                pVarHomeI->GetOperandBox(0)->Replace(pQd);
                pVarHomeI->GetOperandBox(1)->Replace(pRd);
            }
            else
            {
                // Keep VarDef for lookup
                pI->SetOutput(Void);
            }
            break;
        } // closed

        case Variable::Storage_Literal:
        {
            Pseudo* pQd = NULL;
            Register::EnumUser oEnum(pI->GetRd());
            while (! oEnum.AtEnd())
            {
                OperandBox* const pBox = oEnum.Get();
                oEnum.Next();

                Instruction* const pUserI = pBox->GetI();
                if (pUserI->Is<SlotI>())
                {
                    // nothing to do
                }
                else if (pUserI->Is<VarHomeI>())
                {
                    ASSERT(NULL == pQd);
                    pQd = new Pseudo;
                    pUserI->GetOperandBox(0)->Replace(pQd);
                }
                else
                {
                    Register* const pRx = new Register;

                    pUserI->GetBB()->InsertBeforeI(
                        new VarRefI(pRx, pI->GetQd()),
                        pUserI );

                    pBox->Replace(pRx);
                }
            } // while

            if (NULL == pQd)
            {
                pI->SetOutput(Void);
            }
            else
            {
                pI->SetOutput(pQd);
            }

            break;
        } // literal

        case Variable::Storage_Stack:
            if (Register* pRd = pI->GetRd())
            {
                Pseudo* pQd = new Pseudo;
                pRd->ReplaceAll(pQd);
                pI->SetOutput(pQd);
            }
            break;

        default:
            COMPILER_INTERNAL_ERROR();
            break;
        } // switch storage

        pI->GetOperandBox(1)->Replace(Void);

        pVar->GetOwner()->GetEntryBB()->MoveBeforeI(
            pI,
            pVar->GetOwner()->GetEntryBB()->GetLastI() );
    } // VarDef
}; /// PassX86Lower

DEFPASS(X86Lower)

} // Compiler

} // TinyCl
