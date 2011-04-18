#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - x86 Expand Typep
// tinycl_x86_c_typep.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_typep.cpp#6 $
//
#include "./tinycl_x86_c_cg.h"

#include "../../tinycl_clos.h"

namespace TinyCl
{

namespace Compiler
{

using namespace X86;

struct TypepCmd
{
    public: enum Op
    {
        Op_ClassDEq,
        Op_BranchFalse,
        Op_BranchTrue,
        Op_End,
        Op_Eq,
        Op_Generic,
        Op_JoinTrue,
        Op_ReplaceAll,
        Op_TagEq,
    }; // Op

    Op  m_eOp;
    Val m_operand;
}; // TypepCmd

struct TypeDesc
{
    Val         m_klass;
    TypepCmd    m_rgoCmd[16];
}; // TypeDesc

#define TYPEP_EXPANDER_CLASSD_EQ(mp_name) \
{ \
    CLASS_ ## mp_name, \
    { \
        { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Record) }, \
        { TypepCmd::Op_BranchFalse }, \
        { TypepCmd::Op_ClassDEq, CLASSD_ ## mp_name }, \
        { TypepCmd::Op_BranchFalse }, \
        { TypepCmd::Op_JoinTrue }, \
        { TypepCmd::Op_End }, \
    } \
}

#define TYPEP_EXPANDER_CLASSD_OR(mp_name, mp_name1, mp_name2) \
{ \
    CLASS_ ## mp_name, \
    { \
        { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Record) }, \
        { TypepCmd::Op_BranchFalse }, \
        { TypepCmd::Op_ClassDEq, CLASSD_ ## mp_name1 }, \
        { TypepCmd::Op_BranchTrue }, \
        { TypepCmd::Op_ClassDEq, CLASSD_ ## mp_name2 }, \
        { TypepCmd::Op_BranchFalse }, \
        { TypepCmd::Op_JoinTrue }, \
        { TypepCmd::Op_End }, \
    }, \
}


static const TypeDesc
k_rgoTypeDesc[] =
{
    // [A]
    TYPEP_EXPANDER_CLASSD_EQ(array_object),

    // [B]
    TYPEP_EXPANDER_CLASSD_EQ(bignum),
    TYPEP_EXPANDER_CLASSD_OR(bit_vector, simple_bit_vector, bit_vector_object),

    // [C]
    TYPEP_EXPANDER_CLASSD_EQ(character),
    TYPEP_EXPANDER_CLASSD_EQ(charset),
    TYPEP_EXPANDER_CLASSD_EQ(class_description),
    TYPEP_EXPANDER_CLASSD_EQ(closed_cell),

    // FIXME 2008-09-22 yosi@msn.com We should use complex_min and complex_max
    // instead of checking three CLASSDs.
    CLASS_complex,
    {
        { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Record) },
        { TypepCmd::Op_BranchFalse },
        { TypepCmd::Op_ClassDEq, CLASSD_double_float_complex},
        { TypepCmd::Op_BranchTrue },
        { TypepCmd::Op_ClassDEq, CLASSD_single_float_complex },
        { TypepCmd::Op_BranchTrue },
        { TypepCmd::Op_ClassDEq, CLASSD_rational_complex},
        { TypepCmd::Op_BranchFalse },
        { TypepCmd::Op_JoinTrue },
        { TypepCmd::Op_End },
    },

    {
        CLASS_cons,
        {
            { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Cons) },
            { TypepCmd::Op_ReplaceAll },
            { TypepCmd::Op_End },
        }
    },

    // [D]
    TYPEP_EXPANDER_CLASSD_EQ(dll_file_info),
    TYPEP_EXPANDER_CLASSD_EQ(dll_proc_info),
    TYPEP_EXPANDER_CLASSD_EQ(double_float),
    TYPEP_EXPANDER_CLASSD_EQ(double_float_complex),

    // [E]
    TYPEP_EXPANDER_CLASSD_EQ(external_format),
    TYPEP_EXPANDER_CLASSD_EQ(environment),

    // [F]
    {
        CLASS_fixnum,
        {
            { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Fixnum) },
            { TypepCmd::Op_ReplaceAll },
            { TypepCmd::Op_End },
        }
    },

    TYPEP_EXPANDER_CLASSD_OR(float, double_float, single_float),

    {
        CLASS_function,
        {
            { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_FunObj) },
            { TypepCmd::Op_ReplaceAll },
            { TypepCmd::Op_End },
        },
    },

    TYPEP_EXPANDER_CLASSD_EQ(function_information),

    // [H]
    TYPEP_EXPANDER_CLASSD_EQ(hash_table),

    // [L]
    {
        CLASS_list,
        {
            { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Null) },
            { TypepCmd::Op_ReplaceAll },
            { TypepCmd::Op_End },
        },
    },

    // [M]
    TYPEP_EXPANDER_CLASSD_EQ(marker),
    TYPEP_EXPANDER_CLASSD_EQ(method_cache),

    // [N]
    {
        CLASS_null,
        {
            { TypepCmd::Op_Eq, nil },
            { TypepCmd::Op_ReplaceAll },
            { TypepCmd::Op_End },
        },
    },

    // [P]
    TYPEP_EXPANDER_CLASSD_EQ(package),

    // [R]
    TYPEP_EXPANDER_CLASSD_EQ(random_state),
    TYPEP_EXPANDER_CLASSD_EQ(ratio),
    TYPEP_EXPANDER_CLASSD_EQ(rational_complex),
    TYPEP_EXPANDER_CLASSD_EQ(readtable),
    TYPEP_EXPANDER_CLASSD_EQ(regex),
    TYPEP_EXPANDER_CLASSD_EQ(regex_match),

    {
        CLASS_record_object,
        {
            { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Record) },
            { TypepCmd::Op_ReplaceAll },
            { TypepCmd::Op_End },
        },
    },

    TYPEP_EXPANDER_CLASSD_EQ(regex),
    TYPEP_EXPANDER_CLASSD_EQ(regex_match),

    // [S]
    TYPEP_EXPANDER_CLASSD_EQ(setf_cell),
    TYPEP_EXPANDER_CLASSD_EQ(simple_bit_vector),
    TYPEP_EXPANDER_CLASSD_EQ(simple_string),
    TYPEP_EXPANDER_CLASSD_EQ(simple_vector),
    TYPEP_EXPANDER_CLASSD_EQ(single_float),
    TYPEP_EXPANDER_CLASSD_EQ(single_float_complex),
    TYPEP_EXPANDER_CLASSD_EQ(stack_frame),
    TYPEP_EXPANDER_CLASSD_EQ(storage),
    TYPEP_EXPANDER_CLASSD_OR(string, simple_string, string_object),
    TYPEP_EXPANDER_CLASSD_EQ(string_object),
    TYPEP_EXPANDER_CLASSD_EQ(string_range),

    {
        CLASS_symbol,
        {
            { TypepCmd::Op_Eq, nil},
            { TypepCmd::Op_BranchTrue },
            { TypepCmd::Op_TagEq, reinterpret_cast<Val>(Arch::Tag_Record) },
            { TypepCmd::Op_BranchFalse },
            { TypepCmd::Op_ClassDEq, CLASSD_symbol },
            { TypepCmd::Op_BranchFalse },
            { TypepCmd::Op_JoinTrue },
            { TypepCmd::Op_End },
        },
    },

    // [T]
    TYPEP_EXPANDER_CLASSD_EQ(thread),
    TYPEP_EXPANDER_CLASSD_EQ(tlv_record),

    // [V]
    TYPEP_EXPANDER_CLASSD_EQ(value_cell),
    TYPEP_EXPANDER_CLASSD_EQ(vector_object),

    // [W]
    TYPEP_EXPANDER_CLASSD_EQ(weak_pointer),
    TYPEP_EXPANDER_CLASSD_EQ(weak_vector_leader),
}; // k_rgoTypeDesc

/// <summary>
///   Expand TYPEP instruction for X86.
/// </summary>
class PassX86Typep :
    public    Pass_<PassX86Typep, FunctionPass>,
    protected X86Utility
{
    public: static const char* GetName_() { return "X86Typep"; }

    private: struct CmdContext
    {
        Bool*           m_pBd;
        BBlock*         m_pFalseBB;
        BBlock*         m_pJoinBB;
        Instruction*    m_pRefI;
        Register*       m_pRx;
        BBlock*         m_pTrueBB;

        CmdContext(TypepI* pTypepI) :
            m_pBd(NULL),
            m_pFalseBB(NULL),
            m_pJoinBB(NULL),
            m_pRefI(pTypepI),
            m_pRx(pTypepI->GetRx()),
            m_pTrueBB(NULL)
        {
            ASSERT(NULL != pTypepI);

            OperandBox* pBox = pTypepI->GetBd()->GetSingleUser();
            if (NULL == pBox) return;
            if (pBox->GetI() != pTypepI->GetNext()) return;

            if (BranchI* pBranchI = pBox->GetI()->DynamicCast<BranchI>())
            {
                m_pFalseBB = pBranchI->GetFalseBB();
                m_pTrueBB  = pBranchI->GetTrueBB();
            }
        } // CmdContext
    }; // CmdContext

    private: WorkList_<Instruction> m_oTypeps;

    /// <summary>
    ///  Pass entry point.
    /// </summary>
    protected: override void processFunction(Function* pFun)
    {
        CLOG(1, "<h3>Process ~S</h3><ol>~%", pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBB = oEnum.Get();
            foreach (BBlock::EnumI, oEnum, pBB)
            {
                Instruction* pI = oEnum.Get();
                if (pI->Is<TypepI>())
                {
                    m_oTypeps.Push(pI);
                }
            } // for insn
        } // for bb

        while (! m_oTypeps.IsEmpty())
        {
            expandTypep(static_cast<TypepI*>(m_oTypeps.Pop()));
        } // while

        CLOG(1, "</ol>~%");

        pFun->Clean();
    } // processFunction

    /// <summary>
    ///   Make sure we have join basic block.
    /// </summary>
    //      ...
    //      TYPEP %bd <= %r1 typespec
    //      =>
    //    B1
    //      ...
    ///     JUMP B2
    //    B2:
    //      PHI %r2 <= (B1 %r1)
    //      NE %bd <= %r2 nil
    private: void ensureJoinBB(CmdContext* pContext)
    {
        ASSERT(NULL != pContext);

        if (NULL != pContext->m_pJoinBB)
        {
            return;
        }

        // We should not split so far.
        TypepI* pTypepI = pContext->m_pRefI->StaticCast<TypepI>();

        // We should emit something.
        ASSERT(NULL != pContext->m_pRefI->GetPrev());

        BBlock* pBeforeBB = pTypepI->GetBBlock();

        pContext->m_pJoinBB = pBeforeBB->SplitBefore(
            pContext->m_pRefI );

        pContext->m_pRefI = pBeforeBB->GetLastI();
        ASSERT(pContext->m_pRefI->Is<JumpI>());

        if (NULL == pContext->m_pTrueBB)
        {
            Register* pR1 = new Register;
            PhiI* pPhiI = new PhiI(tyT, pR1);
            pContext->m_pJoinBB ->InsertBeforeI(pPhiI, pTypepI);

            pContext->m_pJoinBB->ReplaceI(
                new NeI(pTypepI->GetBd(), pR1, Literal::New(nil)),
                pTypepI );
        }
    } // ensureJoinBB

    private: void expandTypep(TypepI* pTypepI)
    {
        ASSERT(NULL != pTypepI);

        CLOG(1, "<li>expand ~S<ol>~%", pTypepI);

        CmdContext oContext(pTypepI);

        Val typespec = pTypepI->GetSy()->StaticCast<Type>()->Unparse();

        const TypeDesc* pTypeDesc = findTypeDesc(typespec);
        if (NULL == pTypeDesc)
        {
            Values* pVy = new Values;
            pTypepI->GetBB()->InsertBeforeI(
                new ValuesI(pVy, pTypepI->GetRx(), Literal::New(typespec)),
                pTypepI );

            Register* pR1 = new Register;
            pTypepI->GetBB()->InsertBeforeI(
                new CallI(tyT, pR1, Literal::New(Qtypep), pVy),
                pTypepI );

            pTypepI->GetBB()->ReplaceI(
                new NeI(pTypepI->GetBd(), pR1, Literal::New(nil)),
                pTypepI );
        }
        else
        {
            for (
                const TypepCmd* pTypeCmd = pTypeDesc->m_rgoCmd;
                TypepCmd::Op_End != pTypeCmd->m_eOp;
                pTypeCmd++ )
            {
                switch (pTypeCmd->m_eOp)
                {
                case TypepCmd::Op_ClassDEq:
                    processClassDEq(&oContext, pTypeCmd);
                    break;

                case TypepCmd::Op_BranchFalse:
                    processBranchFalse(&oContext, pTypeCmd);
                    break;

                case TypepCmd::Op_BranchTrue:
                    processBranchTrue(&oContext, pTypeCmd);
                    break;

                case TypepCmd::Op_JoinTrue:
                    processJoinTrue(&oContext, pTypeCmd);
                    break;

                case TypepCmd::Op_Eq:
                    processEq(&oContext, pTypeCmd);
                    break;

                case TypepCmd::Op_ReplaceAll:
                    processReplaceAll(&oContext, pTypeCmd);
                    break;

                case TypepCmd::Op_TagEq:
                    processTagEq(&oContext, pTypeCmd);
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // switch op
            } // for typecmd
        } // if

        CLOG(1, "</ol></li>~%");
    } // expandTypep

    /// <summary>
    ///   Find type description for specified type specifier.
    /// </summary>
    /// <param name="typespec">A type specifier</param>
    private: const TypeDesc* findTypeDesc(Val typespec)
    {
        if (symbolp(typespec))
        {
            Val klass = find_class(typespec, nil);
            if (nil != klass) typespec = klass;
        }

        if (! classp(typespec))
        {
            return NULL;
        }

        for (
            const TypeDesc* p = k_rgoTypeDesc;
            p < k_rgoTypeDesc + lengthof(k_rgoTypeDesc);
            p++ )
        {
            if (p->m_klass == typespec)
            {
                return p;
            }
        } // for p

        return NULL;
    } // findTypeDesc

    private: void processBranchFalse(
        CmdContext*     pContext,
        const TypepCmd* pCmd )
    {
        ASSERT(NULL != pContext);
        ASSERT(NULL != pCmd);

        CLOG(2, "<li>Branch False<ol>~%");

        ensureJoinBB(pContext);

        JumpI* pJumpI = pContext->m_pRefI->StaticCast<JumpI>();

        BBlock* pTrueBB = pContext->m_pRefI->GetBBlock()->GetFunction()->
            InsertBefore(new BBlock(), pContext->m_pJoinBB);

        pContext->m_pRefI = pTrueBB->AppendI(
            new JumpI(pContext->m_pJoinBB) );

        BBlock* pCurrBB = pJumpI->GetBBlock();

        if (BBlock* pFalseBB = pContext->m_pFalseBB)
        {
            pCurrBB->ReplaceI(
                new BranchI(
                    pContext->m_pBd,
                    pTrueBB,
                    pFalseBB ),
                pJumpI );
        }
        else
        {
            pCurrBB->ReplaceI(
                new BranchI(
                    pContext->m_pBd,
                    pTrueBB,
                    pContext->m_pJoinBB ),
                pJumpI );

            pContext->m_pJoinBB->GetFirstI()->StaticCast<PhiI>()->
                AddOperand(pCurrBB, Literal::New(nil));
        }

        CLOG(2, "</ol></li>~%");
    } // processBranchFalse

    private: void processBranchTrue(
        CmdContext*     pContext,
        const TypepCmd* pCmd )
    {
        ASSERT(NULL != pContext);
        ASSERT(NULL != pCmd);

        CLOG(2, "<li>Branch True<ol>~%");

        ensureJoinBB(pContext);

        JumpI* pJumpI = pContext->m_pRefI->StaticCast<JumpI>();

        BBlock* pFalseBB = pContext->m_pRefI->GetBBlock()->GetFunction()->
            InsertBefore(new BBlock(), pContext->m_pJoinBB);

        pContext->m_pRefI = pFalseBB->AppendI(
            new JumpI(pContext->m_pJoinBB) );

        BBlock* pCurrBB = pJumpI->GetBBlock();

        if (BBlock* pTrueBB = pContext->m_pTrueBB)
        {
            pCurrBB->ReplaceI(
                new BranchI(
                    pContext->m_pBd,
                    pTrueBB,
                    pFalseBB ),
                pJumpI );
        }
        else
        {
            pCurrBB->ReplaceI(
                new BranchI(
                    pContext->m_pBd,
                    pContext->m_pJoinBB,
                    pFalseBB ),
                pJumpI );

            pContext->m_pJoinBB->GetFirstI()->StaticCast<PhiI>()->
                AddOperand(pCurrBB, Literal::New(t));
        }

        CLOG(2, "</ol></li>~%");
    } // processBranchTrue

    private: void processClassDEq(
        CmdContext*     pContext,
        const TypepCmd* pCmd )
    {
        ASSERT(NULL != pContext);
        ASSERT(NULL != pCmd);

        CLOG(2, "<li>ClassD Eq<ol>~%");

        Pseudo* pQ1 = new Pseudo;
        pContext->m_pRefI->GetBB()->InsertBeforeI(
            new x86LeaI(
                pQ1,
                pContext->m_pRx,
                -Arch::Tag_Record ),
            pContext->m_pRefI );

        Register* pR2 = new Register;
        pContext->m_pRefI->GetBB()->InsertBeforeI(
            new LoadI(pR2, pQ1),
            pContext->m_pRefI );

        Bool* pBd = new Bool;
        pContext->m_pRefI->GetBB()->InsertBeforeI(
            new EqI(pBd, pR2, Literal::New(pCmd->m_operand)),
            pContext->m_pRefI );

        pContext->m_pBd = pBd;

        CLOG(2, "</ol></li>~%");
    } // processClassDEq

    private: void processEq(
        CmdContext*     pContext,
        const TypepCmd* pCmd )
    {
        ASSERT(NULL != pContext);
        ASSERT(NULL != pCmd);

        CLOG(2, "<li>Eq ~W<ol>~%", pCmd->m_operand);

        Bool* pBd = new Bool;
        pContext->m_pRefI->GetBB()->InsertBeforeI(
            new EqI(
                pBd,
                pContext->m_pRx,
                Literal::New(pCmd->m_operand) ),
            pContext->m_pRefI );

        pContext->m_pBd = pBd;

        CLOG(2, "</ol></li>~%");
    } // processEq

    private: void processJoinTrue(
        CmdContext*     pContext,
        const TypepCmd* pCmd )
    {
        ASSERT(NULL != pContext);
        ASSERT(NULL != pCmd);

        CLOG(2, "<li>Join True<ol>~%");

        ASSERT(NULL != pContext->m_pJoinBB);

        if (BBlock* pTrueBB = pContext->m_pTrueBB)
        {
            pContext->m_pRefI->GetBBlock()->ReplaceI(
                new JumpI(pTrueBB),
                pContext->m_pRefI );
        }
        else
        {
            PhiI* pPhiI = pContext->m_pJoinBB->GetFirstI()->
                StaticCast<PhiI>();

            pPhiI->AddOperand(
                pContext->m_pRefI->GetBBlock(),
                True );
        }

        CLOG(2, "</ol></li>~%");
    } // processJoinTrue

    private: void processReplaceAll(
        CmdContext*     pContext,
        const TypepCmd* )
    {
        ASSERT(NULL != pContext);

        CLOG(2, "<li>Replace All<ol>~%");

        TypepI* pTypepI = pContext->m_pRefI->StaticCast<TypepI>();
        pTypepI->GetBd()->ReplaceAll(pContext->m_pBd);
        pTypepI->GetBB()->RemoveI(pTypepI);

        CLOG(2, "</ol></li>~%");
    } // processReplaceAll

    private: void processTagEq(
        CmdContext*     pContext,
        const TypepCmd* pCmd )
    {
        ASSERT(NULL != pContext);
        ASSERT(NULL != pCmd);

        CLOG(2, "<li>Tag Eq<ol>~%");

        Int iTag = reinterpret_cast<Int>(pCmd->m_operand);
        Int iTagMask = Arch::TagMask;

        Register* pR1;
        if (0 == iTag)
        {
            pR1 = pContext->m_pRx;
            iTagMask >>= 1;
        }
        else
        {
            pR1 = new Register;
            pContext->m_pRefI->GetBB()->InsertBeforeI(
                new x86LeaI(
                    pR1,
                    pContext->m_pRx,
                    -iTag ),
                pContext->m_pRefI );

            if (Arch::Tag_Null == iTag)
            {
                iTagMask >>= 1;
            }
        }

        Pseudo* pQ2 = new Pseudo;
        pContext->m_pRefI->GetBB()->InsertBeforeI(
            new x86TestI(
                pQ2,
                pR1,
                Integer::New(iTagMask) ),
            pContext->m_pRefI );

        Bool* pBd = new Bool;
        pContext->m_pRefI->GetBB()->InsertBeforeI(
            new x86BoolI(pBd, pQ2, tttn_Z),
            pContext->m_pRefI );

        pContext->m_pBd = pBd;

        CLOG(2, "</ol></li>~%");
    } // processTagEq
}; // PassX86Typep

DEFPASS(X86Typep)

} // Compiler

} // TinyCl
