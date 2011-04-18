#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - UpVar
// tinycl_c_cg_UpVar.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_split.cpp#1 $
//
#include "./tinycl_c_cg.h"

namespace TinyCl
{

namespace Compiler
{

namespace CgSplit
{


//  entry:
//      1 ENTRY
//        UPVARDEF
//        ...
//      2 JUMP    start
//  start:
//      3 PROLOGUE
//      ...
//      4 RET
//  exit:
//      5 EXIT

enum Limits
{
    MaxClosureInsns = 20 + 5,   // 5 = ENTRY+JUMP+PROLOGUE+RET+EXIT
}; // Limits

class SplitableFun : public Function
{
    // [N]

    // Returns true if
    //  o Closure is called
    //  o Closure has OPENBLOCK, OPENCATCH or OPENTAGBODY = use function
    //    literal.
    public: bool NeedSplit() const
    {
        unless (IsClosure()) return false;

        {
            EnumCall oEnumCall(this);
            if (! oEnumCall.AtEnd()) return true;
        }

        uint cInsns = 0;
        foreach (Function::EnumI, oEnum, this)
        {
            Instruction* pI = oEnum.Get();

            if (pI->Is<UpVarDefI>()) continue;

            cInsns += 1;

            when (cInsns > MaxClosureInsns) return true;

            when (pI->Is<OpenBlockI>()) return true;
            when (pI->Is<OpenCatchI>()) return true;
            when (pI->Is<OpenTagsI>()) return true;
        } // for each insn

        return false;
    } // needSplit

    // [S]
    public: Function* SplitClosure()
    {
        ASSERT(IsClosure());

        CLOG(1, "<ol>~%");

        SplitableFun* pTempl = (new Function(
            Function::Flavor_Template,
            GetName() ))->Extend<SplitableFun>();

        // Update use sites
        {
            EnumUser oEnumUser(this);
            while (! oEnumUser.AtEnd())
            {
                Instruction* pI = oEnumUser.Get()->GetI();
                oEnumUser.Next();

                if (ClosureI* pClosureI = pI->DynamicCast<ClosureI>())
                {
                    CLOG(1, "<li>update ~S by ~S</li>~%", pClosureI, pTempl);
                    pClosureI->GetOperandBox(0)->Replace(pTempl);
                }
                else
                {
                    Context::Get()->Error("Broken closure reference.");
                }
            } // for each user
        }

        // Copy upvars
        {
            BBlock* pBB = pTempl->GetEntryBB();
            foreach (EnumUpVar, oEnum, this)
            {
                Variable* pVar = oEnum.Get();
                pBB->InsertBeforeI(
                    new UpVarDefI(new Pseudo, pVar),
                    pBB->GetLastI() );
            } // for each upvar
        }

        // Restify
        pTempl->m_oArity = m_oArity;

        bool fRestify =
            m_oArity.m_iMin != m_oArity.m_iMax ||
            nil != GetPrologueI()->GetLy();

        if (fRestify)
        {
            pTempl->m_oArity.m_iMax = pTempl->m_oArity.m_iMin;

            pTempl->GetPrologueI()->GetOperandBox(1)->Replace(
                Literal::New(Qdynamic_extent) );
        }

        // Call
        BBlock*  pBB = pTempl->GetStartBB();

        Instruction* pValuesI;
        if (fRestify)
        {
            pValuesI = new ValuesAI(new Values);
        }
        else
        {
            pValuesI = new ValuesI(new Values);
        }

        pBB->AppendI(pValuesI);

        int iMin = m_oArity.m_iMin;
        if (fRestify) iMin += 1;

        Values* pVx = pValuesI->GetVd();

        for (int iNth = 0; iNth < iMin; iNth++)
        {
            Register* pRx = new Register;
            pBB->InsertBeforeI(new SelectI(tyT, pRx, pVx, iNth), pValuesI);
            pValuesI->AppendOperand(pRx);
        } // for iNth

        CallI* pCallI = new CallI(tyValuesRestT, new Values, this, pVx);
        pBB->AppendI(pCallI);
        pBB->AppendI(new RetI(pCallI->GetVd()));

        CLOG(1, "</ol>~%");

        return pTempl;
    } // SplitClosure

    // [U]
    public: bool UpdateRestify()
    {
        PrologueI* pPrologueI = GetPrologueI();
        if (nil == pPrologueI->GetLy()) return false;

        Values* pVx = pPrologueI->GetVd();
        if (NULL == pVx) return false;

        foreach (Values::EnumUser, oEnum, pVx)
        {
            if (SelectI* pSelectI = oEnum.Get()->GetI()->
                DynamicCast<SelectI>() )
            {
                if (pSelectI->GetIy() == m_oArity.m_iMax)
                {
                    return false;
                }
            }
        } // for each user

        // Rest parameter isn't user.
        pPrologueI->GetOperandBox(1)->Replace(Literal::New(nil));
        return true;
    } // UpdatRestify
}; // SplitableFun

} // CgSplit

using namespace CgSplit;

//////////////////////////////////////////////////////////////////////
//
// PassSplitClosure
//
class PassSplitClosure :
    public Pass_<PassSplitClosure, ModulePass>
{
    public: static const char* GetName_() { return "SplitClosure"; }

    private: override void processModule(Module* pM)
    {
        WorkList_<Function> oFuns;
        foreach (Module::EnumFunction, oEnum, pM)
        {
            oFuns.Push(oEnum.Get());
        } // for each fun

        CLOG(1, "<ol>~%");

        while (! oFuns.IsEmpty())
        {
            SplitableFun* pFun = oFuns.Pop()->Extend<SplitableFun>();

            pFun->Extend<SplitableFun>()->UpdateRestify();

            if (pFun->NeedSplit())
            {
                CLOG(1, "<li>Split ~S</li>~%", pFun);
                pFun->SplitClosure();
            }
        } // for each fun

        CLOG(1, "</ol>~%");
    } // processModule
}; // PassSplitClosure

DEFPASS(SplitClosure)

} // Compiler

} // TinyCl
