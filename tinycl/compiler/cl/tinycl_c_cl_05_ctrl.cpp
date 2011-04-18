#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse - 03 Evaluation and Compilation
// tinycl_c_cl_05_ctrl.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_05_ctrl.cpp#2 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

Operand* PassParse::parseBlock(
    const Expect*   pExpect,
    Val             form,
    Val             name,
    Val             forms )
{
    FrameReg* pFd = new FrameReg(
        NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
        Qblock, 
        name );

    BlockFrame* pBlockFrame = new(this) BlockFrame(
        pFd,
        name,
        pExpect,
        newBBlock(),
        newBBlock() );

    pFd->SetWork(pBlockFrame);

    Instruction* pOpenI = emitI(new OpenBlockI(
        pFd,
        name,
        pBlockFrame->m_pNonlocalXpBB ) );

    return parseBlockOrCatch(pExpect, form, forms, pBlockFrame, pOpenI);
} // PassParse::parseBlock

Operand* PassParse::parseBlockOrCatch(
    const Expect*   pExpect,
    Val             form,
    Val             forms,
    XferFrame*      pFrame,
    Instruction*    pOpenI )
{
    BBlock* pSucc = setContinue();
    m_pSucc = newBBlock();
    emitI(new JumpI(m_pSucc));
    m_pCurr = m_pSucc;

    PhiI* pPhiI = NULL;
    if (tyVoid != pExpect->m_pty)
    {
        pPhiI = new PhiI(pExpect->m_pty, newOutput(pExpect->m_pty));
        pFrame->m_pLocalXpBB->AppendI(pPhiI);
    }

    Operand* pSx;
    {
        pushFrame(pFrame);
        pSx = parseForms(pExpect, form, forms);
        popFrame();
    }

    // Local Exit Point
    if (Unreachable != pSx)
    {
        emitI(new JumpI(pFrame->m_pLocalXpBB));

        if (NULL != pPhiI)
        {
            setPhiOperand(pPhiI, m_pCurr, pSx);
        }
    }

    // Nonlocal Exit Point
    if (pFrame->IsNonlocal())
    {
        if (NULL == pPhiI)
        {
            pFrame->m_pNonlocalXpBB->AppendI(
                new JumpI(pFrame->m_pLocalXpBB) );
        }
        else
        {
            Output* pSd = newOutput(pExpect->m_pty);

            pFrame->m_pNonlocalXpBB->AppendI(
                new NonlocalI(pExpect->m_pty, pSd) );

            pFrame->m_pNonlocalXpBB->AppendI(
                new JumpI(pFrame->m_pLocalXpBB) );

            setPhiOperand(pPhiI, pFrame->m_pNonlocalXpBB, pSd);
        }
    }
    else
    {
        pOpenI->GetBB()->RemoveI(pOpenI);
        m_pOwner->RemoveBBlock(pFrame->m_pNonlocalXpBB);

        if (! pFrame->m_pLocalXpBB->HasInEdge())
        {
            m_pOwner->RemoveBBlock(pFrame->m_pLocalXpBB);
        }
    }

    // Close block frame
    if (! pFrame->m_pLocalXpBB->HasInEdge())
    {
        return Unreachable;
    }

    setCurrSucc(pFrame->m_pLocalXpBB, pSucc);

    if (NULL == pPhiI)
    {
        emitUnwind(pFrame);
        return emitLinkage(Void);
    }

    {
        Operand* pSx = emitWeakSaveValues(pPhiI->GetOutput());
        emitUnwind(pFrame);
        return emitLinkage(emitWeakRestoreValues(pSx));
    }
} // PassParse::parseBlock

Operand* PassParse::parseReturnFrom(
    const Expect*,
    Val             form,
    Val             name,
    Val             forms )
{
    Operand* pSx = Void;

    enum Save
    {
        Save_None,
        Save_Values,
        Save_WeakValues,
    } eSave = Save_WeakValues;

    BlockFrame* pBlockFrame = NULL;
    foreach (ClFrame::Enum, oEnum, m_pFrame)
    {
        if (BlockFrame* pFrame = oEnum.Get()->DynamicCast<BlockFrame>())
        {
            if (pFrame->GetName() == name)
            {
                pBlockFrame = pFrame;
                break;
            }
        }

        if (Save_WeakValues == eSave)
        {
            if (oEnum.Get()->Is<FinallyFrame>())
            {
                eSave = Save_Values;
            }
        }
    } // for each frame

    if (NULL == pBlockFrame)
    {
        parseError("No such block called ~S.", name);
        return setUnreachable();
    }

    if (tyVoid == pBlockFrame->m_pExpect->m_pty)
    {
        if (nil != forms)
        {
            parseForm1(pBlockFrame->m_pExpect, car(forms));
        }
        eSave = Save_None;
    }
    else
    {
        pSx = parseForm1(pBlockFrame->m_pExpect, car(forms));
        if (Unreachable == pSx) return uselessForm(form);
        if (! pSx->Is<Values>())
        {
            if (Save_Values == eSave)
            {
                eSave = Save_WeakValues;
            }
        }
    }

    if (pBlockFrame->GetOwner() == m_pOwner)
    {
        // Local control transfer
        switch (eSave)
        {
        case Save_WeakValues:
            pSx = emitWeakSaveValues(pSx);
            break;

        case Save_Values:
            pSx = emitSaveValues(pSx);
            break;
        } // switch eSave

        emitUnwinds(pBlockFrame);

        switch (eSave)
        {
        case Save_WeakValues:
            pSx = emitWeakRestoreValues(pSx);
            break;

        case Save_Values:
            pSx = emitRestoreValues(pSx);
            break;
        } // switch eSave

        emitI(new JumpI(pBlockFrame->m_pLocalXpBB));

        if (Void != pSx && NULL != pBlockFrame->m_pLocalXpBB->GetFirstI())
        {
            if (PhiI* pPhiI = pBlockFrame->m_pLocalXpBB->GetFirstI()->
                    DynamicCast<PhiI>() )
            {
                setPhiOperand(pPhiI, m_pCurr, pSx);
            }
        } // if
    }
    else
    {
        // Nonlocal control transfer
        if (NULL == pBlockFrame->m_pVar)
        {
            pBlockFrame->m_pVar = new Variable(
                make_symbol(
                    pBlockFrame->GetName()->StaticCast<Symbol>()->m_name ) );

            Register* pRx = new Register;

            Instruction* pRefI = pBlockFrame->GetRd()->GetDefI()->GetNext();

            pRefI->GetBB()->InsertBeforeI(
                new FrameI(pRx, pBlockFrame->GetRd()),
                pRefI );

            pRefI->GetBB()->InsertBeforeI(
                new VarDefI(new Register, pBlockFrame->m_pVar, pRx),
                pRefI );
        }

        Register* pRd = new Register;
        {
            Pseudo*   pRcell = m_pOwner->InternUpVar(pBlockFrame->m_pVar);
            Register* pRptr  = new Register;

            emitI(
                new SlotI(
                    tyPtrT,
                    pRptr,
                    CLASS_closed_cell,
                    Qvalues,
                    pRcell ) );

            emitI(new LoadI(pRd, pRptr));
        }

        Values* pVx;
        if (Values* pV2 = pSx->DynamicCast<Values>())
        {
            pVx = pV2;
        }
        else if (pSx->Is<VoidOutput>())
        {
            pVx = new Values;
            emitI(new ValuesI(pVx));
        }
        else
        {
            pVx = new Values;
            emitI(new ValuesI(pVx, pSx));
        }

        emitI(new ReturnFromI(pRd, pVx));
        emitUnreachable();
    } // if

    return emitLinkage(Unreachable);
} // PassParse::parseReturnFrom

defparser(block)
{
    CHECK_SYNTAX(2, MaxFormLength, "(block name form...)");

    Val bname = cadr(form);
    unless (symbolp(bname))
    {
        parseError("Block name must be a symbol.");
        return parseLiteral(pExpect, nil);
    }

    return parseBlock(pExpect, form, cadr(form), cddr(form));
} // block

defparser(catch)
{
    CHECK_SYNTAX(2, MaxFormLength, "(catch tag form...)");

    Operand* pTag = parseOperand(Qcatch, tyT, 0, cadr(form));
    if (Unreachable == pTag) return uselessForm(form);

    FrameReg* pFd = new FrameReg(
        NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
        Qcatch );

    CatchFrame* pCatchFrame = new(this) CatchFrame(
        pFd,
        pTag,
        pExpect,
        newBBlock(),
        newBBlock() );

    Instruction* pOpenI = emitI(new OpenCatchI(
        pFd,
        pTag,
        pCatchFrame->m_pNonlocalXpBB ) );

    return parseBlockOrCatch(pExpect, form, cddr(form), pCatchFrame, pOpenI);
} // catch

defparser(go)
{
    CHECK_SYNTAX(2, 2, "(go tag)");

    Val name = cadr(form);
    TagsFrame::Tag* pTag = NULL;
    foreach (ClFrame::Enum, oEnum, m_pFrame)
    {
        if (TagsFrame* pFrame = oEnum.Get()->DynamicCast<TagsFrame>())
        {
            pTag = pFrame->FindTag(name);
            if (NULL != pTag) break;
        }
    } // for each frame

    if (NULL == pTag)
    {
        parseError("No such tag: ~S.", name);
        return setUnreachable();
    }

    pTag->MarkUse();

    if (pTag->GetFrame()->GetOwner() == m_pOwner)
    {
        // Local go
        if (! pTag->IsDefined())
        {
            // Forward jump
            emitUnwinds(pTag->GetFrame());
        }
        else
        {
            // Backward jump can make infinite loop. So, all open loop
            // should be used here
            foreach (ClFrame::Enum, oEnum, m_pFrame)
            {
                ClFrame* pFrame = oEnum.Get();
                if (pFrame->GetOwner() == m_pOwner)
                {
                    emitI(new UseI(pFrame->GetRd()));
                }
            } // for each frame
        }
        emitI(new JumpI(pTag->GetBB()));
    }
    else
    {
        // Nonlocal goto
        FrameReg* pFrameRd = pTag->GetFrame()->GetRd();

        if (NULL == pTag->m_pVar)
        {
            // FIXME 2007-10-18 yosi@msn.com We should use tag name
            // as name of variable.
            pTag->m_pVar = new Variable(Qgo);

            Register*    pRtag  = new Register;
            Instruction* pRefI  = pFrameRd->GetDefI()->GetNext();
            BBlock*      pRefBB = pRefI->GetBB();

            pRefBB->InsertBeforeI(
                new TagDefI(pRtag, pFrameRd, pTag->GetBB(), pTag->GetName()),
                pRefI );

            pRefBB->InsertBeforeI(
                new VarDefI(new Register, pTag->m_pVar, pRtag),
                pRefI );
        } // if

        Register* pRtag = new Register;
        {
            Pseudo*   pRcell = m_pOwner->InternUpVar(pTag->m_pVar);
            Register* pRptr  = new Register;

            emitI(
                new SlotI(
                    tyPtrT,
                    pRptr,
                    CLASS_closed_cell,
                    Qvalue,
                    pRcell ) );

            emitI(new LoadI(pRtag, pRptr));
        } // pRtag

        emitI(new GoI(pRtag));
        emitUnreachable();
    } // if

    return setUnreachable();
} // go

defparser(if)
{
    CHECK_SYNTAX(3, 4, "(if test then [else])");

    Val test_form = cadr(form);
    Val then_form = caddr(form);
    Val else_form = cadddr(form);
    bool fElse    = cdddr(form) != nil;

    Operand* pSx;
    {
        Expect oExpect(Qif, tyT);
        pSx = parseForm1(&oExpect, test_form);
        if (Unreachable == pSx)
        {
            unreachableForm(then_form);
            when (fElse) unreachableForm(else_form);
            return Unreachable;
        }
    } // pSx

    if (pSx->IsTrue())
    {
        Operand* pS2 = parseForm(pExpect, then_form);
        when (fElse) unreachableForm(else_form);
        return pS2;
    }

    if (pSx->IsFalse())
    {
        unreachableForm(then_form);

        if (fElse)
        {
            return parseForm(pExpect, else_form);
        }

        if (tyVoid != pExpect->m_pty)
        {
            return parseForm(pExpect, nil);
        }

        return emitLinkage(Void);
    }

    Bool* pBx = new Bool;

    emitI(new NeI(pBx, pSx, Literal::New(nil)));

    // then only
    if (! fElse && tyVoid == pExpect->m_pty)
    {
        BBlock* pThen = newBBlock();
        BBlock* pSucc = m_pSucc;
        BBlock* pJoin = m_pSucc;

        if (m_pCurr == pJoin)
        {
            pJoin = m_pSucc = newBBlock();
            pSucc = pJoin;
        }
        else if (pJoin->IsExitBB())
        {
            pJoin = m_pSucc = newBBlock();
        }

        emitI(new BranchI(pBx, pThen, pJoin));

        m_pCurr = pThen;
        parseForm(pExpect, then_form);

        setCurrSucc(pJoin, pSucc);
        return emitLinkage(Void);
    } // if then only

    // then and else
    {
        BBlock* pThen = newBBlock();
        BBlock* pElse = newBBlock();
        BBlock* pSucc = startPhi(pExpect->m_pty);
        BBlock* pJoin = m_pSucc;

        emitI(new BranchI(pBx, pThen, pElse));

        setCurrSucc(pThen, pJoin);
        parseForm(pExpect, then_form);

        setCurrSucc(pElse, pJoin);
        parseForm(pExpect, else_form);

        setCurrSucc(pJoin, pSucc);
        
        if (! pJoin->HasInEdge())
        {
            return Unreachable;
        }

        return endPhi(pExpect->m_pty);
    }
} // if

defparser(progn)
{
    return parseForms(pExpect, form, cdr(form));
} // progn

defparser(return)
{
    CHECK_SYNTAX(1, 2, "(return [form])");
    return parseReturnFrom(pExpect, form, nil, cdr(form));
} // return

defparser(return_from)
{
    CHECK_SYNTAX(2, 3, "(return-from name [form])");
    return parseReturnFrom(pExpect, form, cadr(form), cddr(form));
} // return_form

defparser(tagbody)
{
    CHECK_SYNTAX(1, MaxFormLength, "(tagbody {tag|statement}*)");

    FrameReg* pRframe = new FrameReg(
        NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
        Qtagbody );

    Instruction* pOpenI = emitI(new OpenTagsI(pRframe));

    TagsFrame* pFrame = new(this) TagsFrame(pRframe);

    // Collect tags
    foreach (List::Enum, oEnum, cdr(form))
    {
        Val statement = oEnum.Get();
        if (integerp(statement) || symbolp(statement))
        {
            TagsFrame::Tag* pTag = pFrame->FindTag(statement);
            if (NULL == pTag)
            {
                pFrame->AddTag(
                    new TagsFrame::Tag(
                        pFrame,
                        statement,
                        newBBlock() ) );
            }
        }
        else if (consp(statement))
        {
            // nothing to do
        }
        else
        {
            ignoreForm(statement);
        }
    } // for each elt

    BBlock* pSucc = setContinue();

    {
        BBlock* pBB = newBBlock();
        emitI(new JumpI(pBB));
        setCurrSucc(pBB, pBB);
    }

    // Parse statement
    {
        pushFrame(pFrame);

        Expect oExpect(Qtagbody, tyVoid);

        foreach (List::Enum, oEnum, cdr(form))
        {
            Val statement = oEnum.Get();
            if (integerp(statement) || symbolp(statement))
            {
                TagsFrame::Tag* pTag = pFrame->FindTag(statement);
                ASSERT(NULL != pTag);

                pTag->MarkDef();

                BBlock* pTagBB = pTag->GetBB();

                if (tyNil != oExpect.m_pty)
                {
                    emitI(new JumpI(pTagBB));
                }

                setCurrSucc(pTagBB, pTagBB);

                oExpect.m_pty = tyVoid;
            }
            else if (consp(statement))
            {
                Operand* pSx = parseForm1(&oExpect, statement);
                if (Unreachable == pSx)
                {
                    oExpect.m_pty = tyNil;
                }
            }
        } // for each elt

        popFrame();
    }

    // Check tag usage
    {
        foreach (TagsFrame::EnumTag, oEnum, pFrame)
        {
            TagsFrame::Tag* pTag = oEnum.Get();
            if (! pTag->IsUsed())
            {
                Val name = pTag->GetName();
                unless (symbolp(name) && 
                        nil == name->StaticCast<Symbol>()->m_package )
                {
                    styleWarn("Tag ~S isn't used.", name);
                }
            }
        } // for each tag
    }

    if (pOpenI->IsUseless())
    {
        pOpenI->GetBB()->RemoveI(pOpenI);
    }
    else if (NULL != m_pCurr)
    {
        emitUnwind(pFrame);
    }

    restoreSucc(pSucc);

    if (tyVoid == pExpect->m_pty)
    {
        return emitLinkage(Void);
    }
    else
    {
        return emitLinkage(Literal::New(nil));
    }
} // tagbody

defparser(throw)
{
    CHECK_SYNTAX(3, 3, "(throw tag form)");

    Operand* pSx = parseOperand(Qthrow, tyT, 0, cadr(form));
    if (Unreachable == pSx) return uselessForm(form);

    // FIXME 2007-11-25 yosi@msn.com Should we support local catch?

    Operand* pSy = parseOperand(Qcatch, tyValuesRestT, 1, caddr(form));
    if (Unreachable == pSy) return uselessForm(form);

    Values* pVy = pSy->DynamicCast<Values>();
    if (NULL == pVy)
    {
        pVy = new Values;
        emitI(new ValuesI(pVy, pSy));
    }

    emitI(new ThrowI(pSx, pVy));
    emitUnreachable();
    return emitLinkage(Unreachable);
} // throw

defparser(unwind_protect)
{
    CHECK_SYNTAX(1, MaxFormLength, "(unwind-protect protected form*)");

    Val forms = cddr(form);
    if (nil == forms)
    {
        // No cleanup forms
        return parseForm(pExpect, cadr(form));
    }

    Function* pFinFun = new Function(
        Function::Flavor_Finally,
        list(Kfinally) );

    FrameReg* pFrame = new FrameReg(
        NULL == m_pFrame ? NULL : m_pFrame->GetRd(),
        Kfinally );

    FinallyFrame* pFinFrame = new FinallyFrame(pFrame);

    {
        Values* pVx = new Values;
        emitI(new ValuesI(pVx));
        emitI(new OpenFinallyI(pFrame, pFinFun, pVx));
    }

    Operand* pSx;
    {
        pushFrame(pFinFrame);
        pSx = parseForm1(pExpect, cadr(form));
        popFrame();
    }

    // Parse finally function
    {
        OwnerScope oOwner(pFinFun);
        Expect oExpect(tyVoid);
        parseForms(&oExpect, form, forms);
    }

    if (Unreachable != pSx)
    {
        Operand* pSave = emitSaveValues(pSx);
        emitUnwind(pFinFrame);
        pSx = emitRestoreValues(pSave);
    }

    return emitLinkage(pSx);
} // unwind_protect

} // Compiler

} // TinyCl
