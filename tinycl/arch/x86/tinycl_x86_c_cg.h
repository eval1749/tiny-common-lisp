//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Compiler Code Generator
// arch/x86/tinycl_x86_c_cg.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_c_cg.h#12 $
//
#if !defined(INCLUDE_tinycl_x86_c_cg_h)
#define INCLUDE_tinycl_x86_c_cg_h

#include "../../compiler/cg/tinycl_c_cg.h"
#include "./tinycl_x86.h"

namespace TinyCl
{

namespace Compiler
{

/// <summary>
///   Represents x86 TTTN.
/// </summary>
class TttnLit :
    public Operand_<TttnLit>
{
    public: static const char* Kind_() { return "Tttn"; }

    private: X86::Tttn m_e;

    // ctor
    private: TttnLit(X86::Tttn e) :
        m_e(e) {}

    // [G]
    public: X86::Tttn GetDatum() const
        { return m_e; }

    // [H]
    public: virtual void HtmlPrint(Val stream, bool) const override
    {
        static const char* const s_rgpszTttn[16] =
        {
            "O",  "NO",     // 0
            "B",  "NB",     // 2
            "Z",  "NZ",     // 4
            "BE", "NBE",    // 6
            "S",  "NS",     // 8
            "P",  "NP",     // 10
            "L",  "GE",     // 12
            "LE", "G",      // 14
        }; // TttnLit::sm_rgpszTttn

        write_string(s_rgpszTttn[m_e], stream);
    } // HtmlPrint

    // [N]
    public: static TttnLit* New(X86::Tttn e)
        { return new TttnLit(e); }
}; // TttnLit

class X86Utility
{
    // [G]
    protected: static Physical* getGpr(uint eReg)
    {
        Target* pTarget = Context::Get()->GetTarget();
        foreach (RegSet::Enum, oEnum, pTarget->GetGprGroup()->m_pAll)
        {
            const RegDesc* pDesc = oEnum.Get();
            if (pDesc->m_nId == eReg)
            {
                return pTarget->GetPhysical(pDesc);
            }
        } // for each reg
        COMPILER_INTERNAL_ERROR();
        return NULL;
    } // getGpr

    protected: static Physical* getTcb()
    {
        Target* pTarget = Context::Get()->GetTarget();
        return pTarget->GetPhysical(pTarget->GetGprGroup()->m_pRtcb);
    } // getTcb
}; // X86UtilPass

/// <summary>
///     X86 BOOL instruction used for loading floating-point number.
///   <example>
///     x86BOOL bool %bd &lt;= %ix int
///   </example>
/// </summary>
class x86BoolI : public Instruction_<x86BoolI, Op_x86Bool>
{
    public: x86BoolI(Bool* pBd, Operand* pSx, X86::Tttn eTttn) :
        Base(GetOp_(), tyBool, pBd)
    {
        AppendOperand(pSx);
        AppendOperand(TttnLit::New(eTttn));
    } // x86BoolI

    // [G]
    public: X86::Tttn GetTttn() const
        { return GetSy()->StaticCast<TttnLit>()->GetDatum(); }

    // Note: Since x86BoolI may be shared with instructions, we should not
    // modify tttn.
}; // x86BoolI

// X86CLC
class x86ClcI : public Instruction_<x86ClcI, Op_x86Clc>
{
    public: x86ClcI() :
        Base(GetOp_(), tyVoid, Void) {}
}; // x86ClcI

/// <summary>
///     X86 CVTFLOAT instruction.
///   <example>
///     x86CVTFLOAT float %rd &lt;= %rx
///   </example>
/// </summary>
class x86CvtFloat : public Instruction_<x86CvtFloat, Op_x86CvtFloat>
{
    public: x86CvtFloat(const Type* pty, Register* pRd, Operand* pRx) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pRx);
    } // x86CvtFloat
}; // x86CvtFloat

/// <summary>
///     X86 CVTINT instruction.
///   <example>
///     x86CVTINT int %rd &lt;= %rx
///   </example>
/// </summary>
class x86CvtInt : public Instruction_<x86CvtInt, Op_x86CvtInt>
{
    public: x86CvtInt(const Type* pty, Register* pRd, Operand* pRx) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pRx);
    } // x86CvtInt
}; // x86CvtInt

/// <summary>
///     X86 ENCODE instruction used for loading floating-point number.
///   <example>
///     x86ENCODE int %rd &lt;= %rx
///   </example>
/// </summary>
class x86EncodeI : public Instruction_<x86EncodeI, Op_x86Encode>
{
    public: x86EncodeI(const Type* pty, Output* pRd, Output* pRx) :
        Base(GetOp_(), pty, pRd)
    {
        AppendOperand(pRx);
    } // x86EncodeI
}; // x86EncodeI

/// <summary>
///     X86 integer CMP instruction.
///   <example>
///     X86CMP int %id &lt;= %sx %sy
///   </example>
/// </summary>
class x86CmpI : public Instruction_<x86CmpI, Op_x86Cmp>
{
    public: x86CmpI(Pseudo* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyInt32, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // x86CmpI
}; // x86CmpI

/// <summary>
///     X86 integer CmpF32 instruction.
///   <example>
///     X86CmpF32 int %id &lt;= %sx %sy
///   </example>
/// </summary>
class x86CmpF32I : public Instruction_<x86CmpF32I, Op_x86CmpF32>
{
    public: x86CmpF32I(Pseudo* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyInt32, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // x86CmpF32I
}; // x86CmpF32I

/// <summary>
///     X86 integer CmpF64 instruction.
///   <example>
///     X86CmpF64 int %id &lt;= %sx %sy
///   </example>
/// </summary>
class x86CmpF64I : public Instruction_<x86CmpF64I, Op_x86CmpF64>
{
    public: x86CmpF64I(Pseudo* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyInt32, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // x86CmpF64I
}; // x86CmpF64I

// x86LEA int %rd <= %rx iy
class x86LeaI : public Instruction_<x86LeaI, Op_x86Lea>
{
    public: x86LeaI(Output* pRd, Output* pRx, Int iIy) :
        Base(GetOp_(), tyInt32, pRd)
    {
        AppendOperand(pRx);
        AppendOperand(Integer::New(iIy));
    } // x86LeaI
}; // x86LeaI

// X86STC
class x86StcI : public Instruction_<x86StcI, Op_x86Stc>
{
    public: x86StcI() :
        Base(GetOp_(), tyVoid, Void) {}
}; // x86StcI

// X86TEST int %id <= %sx %sy
class x86TestI : public Instruction_<x86TestI, Op_x86Test>
{
    public: x86TestI(Pseudo* pRd, Operand* pSx, Operand* pSy) :
        Base(GetOp_(), tyInt32, pRd)
    {
        AppendOperand(pSx);
        AppendOperand(pSy);
    } // x86TestI

    public: x86TestI(Pseudo* pRd, Operand* pSx, Val y) :
        Base(GetOp_(), tyInt32, pRd)
    {
        ASSERT(fixnump(y));
        AppendOperand(pSx);
        AppendOperand(Literal::New(y));
    } // x86TestI
}; // x86TestI

/// <summary>
///     X86 ZERO instruction used for loading floating-point +0.0.
///   <example>
///     x86ZERO int %rd
///   </example>
/// </summary>
class x86ZeroI : public Instruction_<x86ZeroI, Op_x86Zero>
{
    public: x86ZeroI(const Type* pty, Register* pRd) :
        Base(GetOp_(), pty, pRd) {}
}; // x86ZeroI

bool NeedArity(const Operand* const);

} // Compiler

} // TinyCl

#endif //!defined(INCLUDE_tinycl_x86_c_cg_h)
