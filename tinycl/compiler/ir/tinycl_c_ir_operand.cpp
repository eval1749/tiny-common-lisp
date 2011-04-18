#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler
// compiler/ir/tinycl_c_ir_operand.cpp
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_operand.cpp#10 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

// [B]
void Bool::HtmlPrint(Val stream, bool fDef) const
{
    if (fDef)
    {
        format(stream, "<a class='o' id='o~D'>%b~D</a>",
            Fixnum::Encode(GetName()),
            Fixnum::Encode(GetName()) );
    }
    else
    {
        format(stream, "<a class='o' href='#o~D'>%b~D</a>",
            Fixnum::Encode(GetName()),
            Fixnum::Encode(GetName()) );
    }
} // Bool::HtmlPrint

// [F]

void FrameReg::HtmlPrint(Val stream, bool fDef) const
{
    if (fDef)
    {
        format(stream, "<a class='o' id='o~D'>[frame ~S ~S ~D]</a>",
            Fixnum::Encode(GetName()),
            m_kind,
            m_name,
            Fixnum::Encode(GetName()) );
    }
    else
    {
        format(stream, "<a class='o' href='#o~D'>[frame ~S ~S ~D]</a>",
            Fixnum::Encode(GetName()),
            m_kind,
            m_name,
            Fixnum::Encode(GetName()) );
    }
} // FrameReg::HtmlPrint

void FunName::HtmlPrint(Val const stream, bool const) const
{
    cformat(stream, "#'~W", m_name);
} // FunName::HtmlPrint

FunName* FunName::Intern(Val fname)
{
    Environment* const pEnv =
        VAR(Aruntime_environmentA)->StaticCast<Environment>();

    Val const frob = gethash(fname, pEnv->m_functions);
    Val ftype = cdr(assq(Qtype, cdr(frob)));
    if (nil == frob)
    {
        COMPILER_INTERNAL_ERROR();
        ftype = list(Qfunction, nil, nil);
    }

    return new FunName(fname, nil, TyFunction::Parse(ftype));
} // FunName::Intern

// [I]
void Integer::HtmlPrint(Val stream, bool) const
{
    cformat(stream, "~D", GetDatum());
} // Integer::HtmlPrint

// [L]
bool Label::Equal(const Operand* const p) const
{ 
    if (const Label* const that = p->DynamicCast<const Label>())
    {
        return this->GetBB() == that->GetBB();
    }
    return false;
} // Label::Equal

void Label::HtmlPrint(Val stream, bool) const
{
    GetBB()->HtmlPrint(stream);
} // Label::HtmlPrint

const Type* Literal::GetTy() const
{
    return Type::Parse(type_of(GetDatum()), nil);
} // Literal::GetTy

Literal* Literal::New(Val obj)
{
    return new Literal(obj);
} // Literal::New

void Literal::HtmlPrint(Val stream, bool) const
{
    cformat(stream, "<a class='l'>~W</a>", GetDatum());
} // Literal::HtmlPrint

Operand* Literal::True(const Operand* pSx)
{
    if (pSx->IsTrue())
    {
        return const_cast<Operand*>(pSx);
    }
    return Compiler::True;
} // Literal::True

// [O]
Operand* OperandBox::Replace(Operand* pSx)
{
    m_pOperand->Unrealize(this);
    m_pOperand = pSx;
    m_pOperand->Realize(this);
    return pSx;
} // OperandBox::Replace

Output::Output() :
    m_iName(++Context::Get()->m_cOutputs) {}

void Output::HtmlPrint(Val stream, bool fDef) const
{
    if (fDef)
    {
        html_format(stream, "<a class='o' id='o~D'>%~C~D</a>",
            Fixnum::Encode(GetName()),
            Character::FromCode(getPrefix()),
            Fixnum::Encode(GetName()) );
    }
    else
    {
        html_format(stream, "<a class='o' href='#o~D'>%~C~D</a>",
            Fixnum::Encode(GetName()),
            Character::FromCode(getPrefix()),
            Fixnum::Encode(GetName()) );
    }
} // Output::HtmlPrint

// [S]
/// <summary>
///   Compute value of this SsaOutput.
/// </summary>
Operand* SsaOutput::Compute() const
{
    if (Instruction* pI = GetDefI())
    {
        return pI->Compute();
    }

    // May be this SSaOutput is Bool::True, Bool::False or other constant.
    return NULL;
} // SsaOutput::Compute

/// <summary>
///   Retreive type of SsaOutput.
/// </summary>
const Type* SsaOutput::GetTy() const
{
    return GetDefI()->GetTy();
} // SsaOutput::GetTy

/// <summary>
///   Update Used-Def chain.
/// </summary>
void SsaOutput::Realize(OperandBox* pBox)
{
    ASSERT(NULL != pBox->GetI());
    ASSERT(pBox->GetI()->IsRealized());
    static_cast<Users*>(this)->Append(pBox);
} // SsaOutput::Realize

/// <summary>
///   Replace all use of this SsaOutput by specified operand.
/// </summary>
void SsaOutput::ReplaceAll(Operand* pSx)
{
    ASSERT(this != pSx);

    CLOG(4, "<li>replace  ~S with ~S<ol>~%",
        GetDefI(), pSx );

    for (;;)
    {
        OperandBox* pBox = static_cast<Users*>(this)->GetFirst();
        if (NULL == pBox) break;

        CLOG(4, "<li>~S</li>~%", pBox->GetI());

        ASSERT(pBox->GetOperand() == this);

        Unrealize(pBox);
        pBox->SetOperand(pSx);
        pSx->Realize(pBox);
    } // while

    CLOG(4, "</ol></li>~%");
} // SsaOutput::ReplaceAll

void SsaOutput::Unrealize(OperandBox* pBox)
{
    static_cast<Users*>(this)->Delete(pBox);
} // SsaOutput::Unrealize

// [V]
Variable* Variable::Get(Operand* pSx)
{
    if (Register* pRx = pSx->DynamicCast<Register>())
    {
        return pRx->GetVar();
    }
    return NULL;
} // Variable::Get

Function* Variable::GetOwner() const
{
    BBlock* pBB = GetDefI()->GetBB();
    if (NULL == pBB) return NULL;
    return pBB->GetFunction();
} // Variable::GetOwner

Register* Variable::GetRd() const
    { return GetDefI()->GetRd(); }

void Variable::HtmlPrint(Val stream, bool) const
{
    static const char* k_rgpszStorage[] =
    {
        "closed-cell",
        "literal",
        "register",
        "stack",
    }; // k_rgpszStorage

    cformat(stream, "[var.~D ~W in ~A @ ~D]",
        m_cUpRefs,
        GetName(),
        k_rgpszStorage[GetStorage()],
        GetLocation() );
} // Variable::HtmlPrint

void Variable::Realize(OperandBox* pBox)
{
    Function* pFun = pBox->GetI()->GetBB()->GetFunction();
    if (GetOwner() == pFun)
    {
        pFun->m_oVarDefs.Append(pBox);
    }
    else
    {
        // Note: Up variables are listed in EntryBB
        pFun->m_oUpVarDefs.Append(pBox);
        m_cUpRefs += 1;
    }
} // Variable::Realize

void Variable::Unrealize(OperandBox* pBox)
{
    Function* pFun = pBox->GetI()->GetBB()->GetFunction();
    if (GetOwner() == pFun)
    {
        pFun->m_oVarDefs.Delete(pBox);
    }
    else
    {
        // Note: Up variables are listed in EntryBB
        pFun->m_oUpVarDefs.Delete(pBox);
        m_cUpRefs -= 1;
    }
} // Variable::Unrealize

} // Compiler
} // TinyCl
