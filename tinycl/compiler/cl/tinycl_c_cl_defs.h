// -*- Mode: C++ -*-
// TinyCl - TinyCl Compiler
// tinycl_c_defs.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_defs.h#14 $
//
#if !defined(INCLUDE_tinycl_compiler_parser_h)
#define INCLUDE_tinycl_compiler_parser_h

#include "../tinycl_c_defs.h"

namespace TinyCl
{

namespace Compiler
{

/// <summary>
///  Parser pass.
/// </summary>
class PassParse :
    public    Pass_<PassParse>,
    protected Mm
{
    public: static const char* GetName_() { return "Parse"; }

    private: enum Linkage
    {
        Linkage_Jump,
        Linkage_Next,
        Linkage_Phi,
        Linkage_Return,
        Linkage_Unreachable,
    }; // Linkage

    /// <summary>
    ///   Represents a callee. Used for propagating notinline attribute.
    /// </summary>
    private: struct Callee
    {
        bool                m_fNotinline;
        Operand*            m_pOperand;
        const TyFunction*   m_pFunty;
        Val                 m_name;

        Callee() :
            m_fNotinline(false),
            m_pOperand(Void),
            m_pFunty(tyUnknownFunction),
            m_name(nil) {}
    }; // Callee

    private: class Expect
    {
        public: enum Kind
        {
            Kind_Argument,
            Kind_Value,
        }; // Kind

        Kind        m_eKind;
        int         m_iNth;
        const Type* m_pty;
        Val         m_name;

        public: Expect(const Type* const pty) :
                m_name(nil),
                m_eKind(Kind_Value),
                m_iNth(0),
                m_pty(pty)
            { ASSERT(NULL != pty); }

        public: Expect(Val const name, const Type* const pty) :
                m_name(name),
                m_eKind(Kind_Value),
                m_iNth(0),
                m_pty(pty)
            { ASSERT(NULL != pty); }

        protected: Expect(
            Kind        const eKind,
            Val         const name,
            int         const iNth,
            const Type* const pty ) :
                m_name(name),
                m_eKind(eKind),
                m_iNth(iNth),
                m_pty(pty)
            { ASSERT(NULL != pty); }
    }; // Expect

    private: class ExpectArg :
        public Expect
    {
        public: ExpectArg(
            Val         const name, 
            int         const iNth, 
            const Type* const pty) :
            Expect(Kind_Argument, name, iNth, pty) {}
    }; // ExpectArg

    private: class ClFrame :
        public Castable_<ClFrame>,
        public LocalObject
    {
        public: static const char* Kind_() { return "ClFrame"; }

        private: FrameReg*  m_pRd;
        private: ClFrame*   m_pOuter;
        public:  Function*  m_pOwner;

        protected: ClFrame() :
            m_pOuter(NULL),
            m_pOwner(NULL),
            m_pRd(NULL) {}

        // [E]
        public: class Enum
        {
            private: ClFrame* m_pRunner;

            public: Enum(ClFrame* p) :
                m_pRunner(p) {}

            public: bool AtEnd() const
                { return NULL == m_pRunner; }

            public: ClFrame* Get() const
                { ASSERT(! AtEnd()); return m_pRunner; }

            public: void Next()
                { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pOuter; }
        }; // Enum

        // [G]
        public: ClFrame*  GetOuter() const { return m_pOuter; }
        public: Function* GetOwner() const { return m_pOwner; }
        public: FrameReg* GetRd()    const { return m_pRd; }

        // [I]
        protected: void init(FrameReg* pRd)
        {
            m_pRd = pRd;
        } // init

        // [S]
        public: ClFrame* SetOuter(ClFrame* p) { return m_pOuter = p; }
    }; // ClFrame

    private: class BindFrame :
        public WithCastable_<BindFrame, ClFrame>
    {
        public: static const char* Kind_() { return "Block"; }

        public: BindFrame(
            FrameReg*   pRd )
            { init(pRd); }
    }; // BindFrame

    private: class XferFrame :
        public WithCastable_<XferFrame, ClFrame>
    {
        public: static const char* Kind_() { return "XferFrame"; }

        public: const Expect*   m_pExpect;
        public: BBlock*         m_pLocalXpBB;
        public: BBlock*         m_pNonlocalXpBB;

        // ctor
        protected: XferFrame() {}

        // [I]
        public: virtual bool IsNonlocal() const { return true; }
    }; // XferFrame

    /// <summary>
    ///   Represents frame for BLOCK.
    /// </summary>
    private: class BlockFrame :
        public WithCastable_<BlockFrame, XferFrame>
    {
        public: static const char* Kind_() { return "Block"; }

        public: Variable* m_pVar;

        private: Val m_name;

        // ctor
        public: BlockFrame(
            FrameReg*       pRd,
            Val             name,
            const Expect*   pExpect,
            BBlock*         pLocalXpBB,
            BBlock*         pNonlocalXpBB ) :
                m_name(name)
        {
            init(pRd);
            m_pExpect       = pExpect;
            m_pLocalXpBB    = pLocalXpBB;
            m_pNonlocalXpBB = pNonlocalXpBB;
        } // BlockFrame

        // [G]
        public: Val GetName() const { return m_name; }

        // [I]
        public: bool IsNonlocal() const { return NULL != m_pVar; }
    }; // BlockFrame

    /// <summary>
    ///   Represents frame for CATCH.
    /// </summary>
    private: class CatchFrame :
        public WithCastable_<CatchFrame, XferFrame>
    {
        public: static const char* Kind_() { return "Catch"; }

        public: Operand* m_pTag;

        // ctor
        public: CatchFrame(
            FrameReg*       pRd,
            Operand*        pTag,
            const Expect*   pExpect,
            BBlock*         pLocalXpBB,
            BBlock*         pNonlocalXpBB ) :
                m_pTag(pTag)
        {
            init(pRd);
            m_pExpect       = pExpect;
            m_pLocalXpBB    = pLocalXpBB;
            m_pNonlocalXpBB = pNonlocalXpBB;
        } // XferFrame
    }; // CatchFrame

    /// <summary>
    ///   Represents frame for UNWIND-PROTECT.
    /// </summary>
    private: class FinallyFrame :
        public WithCastable_<FinallyFrame, ClFrame>
    {
        public: static const char* Kind_() { return "Finally"; }

        // ctor
        public: FinallyFrame(FrameReg* pRd)
            { init(pRd); }
    }; // FinallyFrame

    /// <summary>
    ///   Represents frame for HANDLER-BIND.
    /// </summary>
    private: class HandlerFrame :
        public WithCastable_<HandlerFrame, ClFrame>
    {
        public: static const char* Kind_() { return "Handler"; }

        // ctor
        public: HandlerFrame(FrameReg* pRd)
            { init(pRd); }
    }; // HandlerFrame

    /// <summary>
    ///   Represents frame for TAGBODY.
    /// </summary>
    private: class TagsFrame :
        public WithCastable_<TagsFrame, ClFrame>
    {
        public: static const char* Kind_() { return "Tags"; }

        public: class Tag :
            public LocalObject,
            public DoubleLinkedItem_<Tag>
        {
            private: bool       m_fDefined;
            private: bool       m_fUsed;
            private: BBlock*    m_pBBlock;
            private: TagsFrame* m_pFrame;
            public:  Variable*  m_pVar;

            private: Val        m_name;

            // ctor
            public: Tag(
                TagsFrame*  pFrame,
                Val         name,
                BBlock*     pBBlock ) :
                    m_name(name),
                    m_fDefined(false),
                    m_fUsed(false),
                    m_pFrame(pFrame),
                    m_pBBlock(pBBlock),
                    m_pVar(NULL) {}

            // [G]
            public: BBlock*    GetBB()     const { return m_pBBlock; }
            public: TagsFrame* GetFrame()  const { return m_pFrame; }
            public: Val        GetName()   const { return m_name; }

            // [I]
            public: bool IsDefined() const { return m_fDefined; }
            public: bool IsUsed()    const { return m_fUsed; }

            // [M]
            public: void MarkDef() { m_fDefined = true; }
            public: void MarkUse() { m_fUsed    = true; }
        }; // Tag

        private: typedef DoubleLinkedList_<Tag> Tags;

        private: Tags m_oTags;

        // ctor
        public: TagsFrame(FrameReg* pRd)
            { init(pRd); }

        // [A]
        public: Tag* AddTag(Tag* pTag)
            { return m_oTags.Append(pTag); }

        // [E]
        public: class EnumTag : public Tags::Enum
        {
            public: EnumTag(const TagsFrame* p) :
                Tags::Enum(&p->m_oTags) {}
        }; // EnumTag

        // [F]
        public: Tag* FindTag(Val name) const
        {
            foreach (EnumTag, oEnum, this)
            {
                Tag* pTag = oEnum.Get();
                if (eql(pTag->GetName(), name)) return pTag;
            } // for each tag
            return NULL;
        } // FindTag
    }; // TagsFrame

    /// <summary>
    ///   Represents frame for HANDLER-CASE.
    /// </summary>
    private: class TryFrame :
        public WithCastable_<TryFrame, ClFrame>
    {
        public: static const char* Kind_() { return "Try"; }

        public: BBlock* m_pJoinBB;

        // ctor
        public: TryFrame(FrameReg* pRd, BBlock* pJoinBB) :
            m_pJoinBB(pJoinBB)
            { init(pRd); }
    }; // TryFrame

    private: class LexEnv;

    private: class NameRef :
        public Castable_<NameRef>,
        public LocalObject
    {
        public: enum Flag
        {
            Flag_DynamicExtent  = 1 << 0,
            Flag_Ignorable      = 1 << 1,
            Flag_Ignore         = 1 << 2,
            Flag_Inline         = 1 << 3,
            Flag_Notinline      = 1 << 4,
            Flag_Read           = 1 << 5,
            Flag_Write          = 1 << 6,

            Flag_Inherit =
                Flag_DynamicExtent |
                Flag_Ignorable |
                Flag_Ignore |
                Flag_Inline |
                Flag_Notinline,

            Flag_UsageMask  = Flag_Read | Flag_Write | Flag_Ignorable,
            Flag_Used       = Flag_Read,
        }; // Flag

        public: static const char* Kind_() { return "NameRef"; }

        protected: const Type*  m_pty;
        protected: uint         m_rgfFlag;
        protected: Val          m_name;
        protected: Val          m_datum;

        protected: NameRef() :
            m_name(nil),
            m_datum(nil),
            m_pty(NULL),
            m_rgfFlag(0) {}

        // [G]
        public: Val GetDatum() const
            { return m_datum; }

        public: uint GetFlags() const
            { return m_rgfFlag; }

        public: Val GetName() const
            { return m_name; }

        public: const Type* GetTy() const
            { return m_pty; }

        // [I]
        public: bool IsDynamicExtent() const
            { return IsFlag(Flag_DynamicExtent); }

        public: bool IsFlag(uint m) const
            { return 0 != (m_rgfFlag & m); }

        public: bool IsIgnorable() const
            { return IsFlag(Flag_Ignorable); }

        public: bool IsIgnore() const
            { return IsFlag(Flag_Ignore); }

        // [M]
        public: void Mark(Flag e)
            { m_rgfFlag |= e; }

        // [S]
        public: uint SetFlags(uint rgfFlag)
            { return m_rgfFlag = rgfFlag; }

        public: const Type* SetTy(const Type* pty)
            { return m_pty = pty; }

        // [U]
        public: void Unmark(Flag e)
            { m_rgfFlag &= ~e; }
    }; // NameRef

    private: class FunRef :
        public WithCastable_<FunRef, NameRef>,
        public ChildItem_<FunRef, LexEnv>
    {
        private: FunRef* m_pOuter;

        // ctor
        protected: FunRef() :
            m_pOuter(NULL) {}

        // [G]
        /// <summary>Retreive associated lexical environment.</summary>
        /// <returns>LexEnv or null if it is global definfition.</returns>
        public: LexEnv* GetLexEnv() const
        {
            // m_pParent is null if it is global definition.
            return m_pParent;
        } // GetLexEnv

        public: Val GetOpKind() const
            { return car(GetDatum()); }

        public: FunRef* GetOuter() const
            { return m_pOuter; }

        public: const TyFunction* GetFunty() const
            { return GetTy()->StaticCast<TyFunction>(); }

        // [I]
        protected: void init(
            const TyFunction* const pty,
            Val               const name,
            Val               const datum )
        {
            m_pty    = pty;
            m_name   = name;
            m_datum  = datum;
        } // init

        public: bool IsNotinline() const
            { return IsFlag(Flag_Notinline); }

        public: bool IsInline() const
            { return IsFlag(Flag_Inline); }

        // [S]
        public: FunRef* SetOuter(FunRef* p)
            { return m_pOuter = p; }
    }; // FunRef

    private: typedef ChildList_<FunRef, LexEnv> FunRefs;

    // FunDcl - locally/declare
    private: class FunDcl :
        public WithCastable_<FunDcl, FunRef>
    {
        public: static const char* Kind_() { return "FunDcl"; }

        // ctor
        protected: FunDcl() {}

        public: FunDcl(const TyFunction* pty, Val name, Val datum = nil)
            { init(pty, name, datum); }
    }; // FunDcl

    // FunMac - macro function
    private: class FunMac :
        public WithCastable_<FunMac, FunRef>
    {
        public: static const char* Kind_() { return "FunMac"; }

        // ctor
        public: FunMac(Val name, Val datum)
            { init(NULL, name, datum); }

        // [G]
        public: Val GetExpander() const
            { return GetDatum(); }
    }; // FunMac

    // FunPro - procliam'ed function
    private: class FunPro :
        public WithCastable_<FunPro, FunRef>
    {
        public: static const char* Kind_() { return "FunPro"; }

        private: FunName* const m_pFunName;

        // ctor
        public: FunPro(
            const TyFunction* const pFunty,
            Val               const name,
            FunName*          const pFunName) :
                m_pFunName(pFunName)
            { init(pFunty, name, pFunName->GetAlist()); }

        private: FunPro& operator =(const FunPro&)
            { CAN_NOT_HAPPEN(); }

        // [G]
        public: Val GetAlist() const
            { return m_pFunName->GetAlist(); }

        public: FunName* GetFunName() const
            { return m_pFunName; }

        public: TyFunction* GetFunty() const
            { return m_pty->StaticCast<TyFunction>(); }
    }; // FunPro

    // FunSpecial - special operator
    private: class FunSpecial :
        public WithCastable_<FunSpecial, FunRef>
    {
        public: static const char* Kind_() { return "FunSpecial"; }

        // ctor
        public: FunSpecial(Val name)
            { init(NULL, name, nil); }
    }; // FunSpecial

    private: class VarRef :
        public WithCastable_<VarRef, NameRef>,
        public ChildItem_<VarRef, LexEnv>
    {
        public: static const char* Kind_() { return "VarRef"; }

        private: VarRef*    m_pOuter;

        // ctor
        protected: VarRef() :
            m_pOuter(NULL) {}

        // [G]
        /// <summary>Retreive associated lexical environment.</summary>
        /// <returns>LexEnv or null if it is global definfition.</returns>
        public: LexEnv* GetLexEnv() const
        {
            // m_pParent is null if it is global definition.
            return m_pParent;
        } // GetLexEnv

        public: VarRef* GetOuter()  const { return m_pOuter; }

        // [I]
        protected: void init(const Type* pty, Val name)
        {
            m_pty  = pty;
            m_name = name;
        } // init

        // [S]
        public: VarRef* SetOuter(VarRef* p)
            { return m_pOuter = p; }
    }; // VarRef

    private: typedef ChildList_<VarRef, LexEnv> VarRefs;

    class VarEntity :
        public WithCastable_<VarEntity, VarRef>
    {
        public: static const char* Kind_() { return "VarEntity"; }
    }; // VarEntity

    class VarDef :
        public WithCastable_<VarDef, VarEntity>
    {
        public: static const char* Kind_() { return "VarDef"; }

        public:  uint       m_cUpRefs;
        private: Variable*  m_pVar;

        private: Val        m_cell;

        // ctor
        protected: VarDef() :
            m_cUpRefs(0), m_pVar(NULL), m_cell(nil) {}

        // [G]
        public: Val GetCell() const
            { return m_cell; }

        public: Variable* GetVar() const
            { return m_pVar; }

        // [I]
        protected: void init(
            const Type* const pty,
            Val         const name,
            Variable*   const pVar,
            Val         const cell = nil )
        {
            VarRef::init(pty, name);
            m_pVar = pVar;
            m_cell = cell;
        } // init

        // [S]
        public: Val SetCell(Val s) { return m_cell = s; }
    }; // VarDef

    // VarConst - defconstant
    private: class VarConst :
        public WithCastable_<VarConst, VarEntity>
    {
        public: static const char* Kind_() { return "VarConst"; }

        private: Val m_value;

        // ctor
        public: VarConst(Val name, Val val) :
                m_value(val)
            { init(tyT, name); }

        // [G]
        public: Val GetValue() const
            { return m_value; }
    }; // VarConst

    // VarDcl - locally/declare
    private: class VarDcl :
        public WithCastable_<VarDcl, VarRef>
    {
        public: static const char* Kind_() { return "VarDcl"; }

        private: Val m_cell;

        // ctor
        protected: VarDcl() : m_cell(nil) {}

        public: VarDcl(Val name, Val cell = nil) :
                m_cell(cell)
            { init(tyT, name); }

        public: Val GetCell() const { return m_cell; }
    }; // VarDcl

    // VarLet - let, let*
    private: class VarLet :
        public WithCastable_<VarLet, VarDef>
    {
        public: static const char* Kind_() { return "VarLet"; }

        private: Val m_initform;

        // ctor
        protected: VarLet() {}

        public: VarLet(Variable* pVar, Val initform = nil) :
            m_initform(initform)
            { init(tyT, pVar->GetName(), pVar); }

        // [G]
        public: Val GetInitForm() const { return m_initform; }
    }; // VarLet

    // VarMac - symbol-macrolet
    private: class VarMac :
        public WithCastable_<VarMac, VarEntity>
    {
        public: static const char* Kind_() { return "VarMac"; }

        private: Val m_expansion;

        // ctor
        public: VarMac(Val name, Val expansion) :
                m_expansion(expansion)
            { init(tyT, name); }

        // [G]
        public: Val GetExpansion() const
            { return m_expansion; }
    }; // VarMac

    // VarPro - procliam'ed variable
    private: class VarPro :
        public WithCastable_<VarPro, VarEntity>
    {
        public: static const char* Kind_() { return "VarPro"; }

        private: Val m_cell;

        // ctor
        public: VarPro(Val name, Val cell) :
                m_cell(cell)
            { init(tyT, name); }

        // [G]
        public: Val GetCell() const { return m_cell; }
    }; // VarPro

    private: class LambdaList;
    private: class VarParam;
    private: typedef DoubleLinkedList_<VarParam, LambdaList> Params;

    // VarParam - lambda parameter
    private: class VarParam :
        public WithCastable_<VarParam, VarDef>,
        public DoubleLinkedItem_<VarParam, LambdaList>
    {
        public: static const char* Kind_() { return "VarParam"; }

        public: Register*   m_pRx;
        public: VarDef*     m_pSVarDef;

        public: Val         m_initform;
        public: Val         m_key;

        public: VarParam(Variable* pVar) :
            m_pRx(NULL),
            m_pSVarDef(NULL)
            { init(tyT, pVar->GetName(), pVar); }
    }; // VarParam

    private: class LambdaList
    {
        public: Params  m_oReqs;
        public: Params  m_oOpts;
        public: Params  m_oRests;
        public: Params  m_oKeys;
        public: Params  m_oAuxs;
        public: Val     m_key;

        public: LambdaList() :
            m_key(nil) {}
    }; // LamabdaList

    private: class LexEnv :
        public FunRefs,
        public VarRefs
    {
        public:  Val            m_name;
        private: LexEnv*        m_pOuter;
        private: const Type*    m_pty;

        // ctor
        public: LexEnv(const Type* pty, Val name = zero) :
            m_name(name), m_pty(pty)
        {
            PassParse* pPass = Context::Get()->GetPass()->
                StaticCast<PassParse>();

            m_pOuter = pPass->m_pLexEnv;
        } // LexEnv

        // [A]
        public: FunRef* AddFunRef(FunRef* pFunRef)
            { return FunRefs::Append(pFunRef); }

        public: VarRef* AddVarRef(VarRef* pVarRef)
            { return VarRefs::Append(pVarRef); }

        // [E]
        public: class EnumFun : public FunRefs::Enum
        {
            public: EnumFun(const LexEnv* p) :
                FunRefs::Enum(p) {}
        }; // EnumFun

        public: class EnumVar : public VarRefs::Enum
        {
            public: EnumVar(const LexEnv* p) :
                VarRefs::Enum(p) {}
        }; // EnumVar

        // [G]
        public: LexEnv*     GetOuter() const { return m_pOuter; }
        public: const Type* GetTy()    const { return m_pty; }

        // [S]
        public: const Type* SetTy(const Type* pty) { return m_pty = pty; }
    }; // LexEnv

    private: class LexEnvScope
    {
        public: LexEnvScope(LexEnv* pLexEnv)
        {
            PassParse* pPass = Context::Get()->GetPass()->
                StaticCast<PassParse>();

            pPass->m_pLexEnv = pLexEnv;
        } // LexEnvScope

        public: ~LexEnvScope()
        {
            PassParse* pPass = Context::Get()->GetPass()->
                StaticCast<PassParse>();

            pPass->m_pLexEnv = pPass->m_pLexEnv->GetOuter();
        } // ~LexEnvScope
    }; // LexEnvScope

    private: class OwnerScope
    {
        private: BBlock*    m_pCurr;
        private: Function*  m_pSave;
        private: BBlock*    m_pSucc;

        public: OwnerScope(Function* pOwner)
        {
            CLOG(1, "<li><h3>Process ~S</h3><ol>~%", pOwner);

            PassParse* pPass = Context::Get()->GetPass()->
                StaticCast<PassParse>();

            m_pSave = pPass->m_pOwner;
            m_pCurr = pPass->m_pCurr;
            m_pSucc = pPass->m_pSucc;

            pPass->m_pOwner = pOwner;
            pPass->m_pCurr  = pOwner->GetStartBB();
            pPass->m_pSucc  = pOwner->GetExitBB();
        } // OwnerScope

        public: ~OwnerScope()
        {
            PassParse* pPass = Context::Get()->GetPass()->
                StaticCast<PassParse>();

            //pPass->m_pOwner->UpdateValueTy();
            pPass->m_pOwner->Clean();
            pPass->m_pOwner->UpdateValueTy();

            pPass->m_pOwner = m_pSave;
            pPass->m_pCurr  = m_pCurr;
            pPass->m_pSucc  = m_pSucc;

            CLOG(1, "</ol></li>~%");
        } // ~OwnerScope
    }; // OwnerScope

    private: typedef Operand* (*ParserFn)(PassParse*, const Expect*, Val);

    // FunDef - local function definition
    private: class FunDef :
        public WithCastable_<FunDef, FunRef>
    {
        public: static const char* Kind_() { return "FunDef"; }

        public: LambdaList  m_oLambdaList;
        public: LexEnv      m_oLexEnv;

        public: Val m_form;
        public: Val m_forms;

        public: Function* GetFunction() const
            { return reinterpret_cast<Function*>(GetDatum()); }

        // ctor
        public: FunDef(Val const name, Function* const pFun) :
                m_oLexEnv(tyValuesRestT, name),
                m_form(nil),
                m_forms(nil)
            { init(tyUnknownFunction, name, Fixnum::Encode(pFun)); }

        public: void SetTy(const TyFunction* pFunty)
        {
            m_pty = pFunty;
            GetFunction()->SetFunty(pFunty);
        } // SetTy
    }; // FunDef

    #define defparser(mp_name) \
        private: Operand* parse_ ## mp_name( \
            const Expect*   const pExpect, \
            Val             const form ); \
        private: static Operand* parse__ ## mp_name( \
            PassParse*      const pPass, \
            const Expect*   const pExpect, \
            Val             const form ) \
                { return pPass->parse_ ## mp_name(pExpect, form); }

    #define defdeclare(mp_name) \
        private: void parse_declare_ ## mp_name(Val); \
        public: static void parse_declare__ ## mp_name( \
            PassParse*  const pPass, \
            Val         const declspec ) \
                { return pPass->parse_declare_ ## mp_name(declspec); }

    #define defspecial(mp_name) \
        defparser(mp_name)

    #include "../tinycl_compiler.inc"

    private: class ParserTab
    {
        private: struct Slot
        {
            Val         m_name;
            ParserFn    m_pfn;
        }; // Slot

        private: int    m_cEntries;
        private: Slot   m_rgoSlot[101];

        // [G]
        public: ParserFn Get(Val name)
        {
            const Slot* pTop    = m_rgoSlot;
            const Slot* pBtm    = m_rgoSlot + lengthof(m_rgoSlot);
            const Slot* pStart  = pTop + (name->ToInt() % (pBtm - pTop));
            const Slot* pRunner = pStart;
            for (;;)
            {
                if (name == pRunner->m_name) return pRunner->m_pfn;
                if (NULL == pRunner->m_name) return NULL;
                pRunner++;
                if (pRunner == pBtm) pRunner = pTop;
                ASSERT(pRunner != pStart);
            } // for
        } // Get

        // [I]
        public: void InitIfNeeded()
        {
            if (m_cEntries >= 1) return;

            static Slot s_rgoSlot[] =
            {
                #define defparser(mp_name) \
                    { Q ## mp_name, parse__ ## mp_name },

                #define defspecial(mp_name) \
                    defparser(mp_name)

                #include "../tinycl_compiler.inc"
            }; // s_rgoSlot

            for (
                const Slot* p = s_rgoSlot;
                p < s_rgoSlot + lengthof(s_rgoSlot);
                p++ )
            {
                put(p);
            } // for
        } // Init

        // [P]
        private: void put(const Slot* pSlot)
        {
            Slot* pTop    = m_rgoSlot;
            Slot* pBtm    = m_rgoSlot + lengthof(m_rgoSlot);
            Slot* pStart  = pTop + (pSlot->m_name->ToInt() % (pBtm - pTop));
            Slot* pRunner = pStart;
            for (;;)
            {
                if (NULL == pRunner->m_name)
                {
                    m_cEntries++;
                    *pRunner = *pSlot;
                    return;
                }
                pRunner++;
                if (pRunner == pBtm) pRunner = pTop;
                ASSERT(pRunner != pStart);
            } // for
        } // put
    }; // ParserTab

    // Member variables
    private: static ParserTab s_oParserTab;

    private: BBlock*    m_pCurr;
    private: ClFrame*   m_pFrame;
    private: LexEnv*    m_pLexEnv;
    private: Function*  m_pOwner;
    private: BBlock*    m_pSucc;

    private: Val    m_funtab;
    private: Val    m_linenum;
    private: Val    m_vartab;

    // ctor
    public: PassParse() :
        m_linenum(nil),
        m_pCurr(NULL),
        m_pFrame(NULL),
        m_pLexEnv(NULL),
        m_pOwner(NULL),
        m_pSucc(NULL) {}

    // [A]
    private: void activateFreeDcls();
    private: void activateFunDcl(FunRef*);
    private: void activateLexEnv();
    private: void activateVarDcl(VarRef*);
    private: void addFunRef(FunRef*);
    private: void addVarRef(VarRef*);

    // [C]
    private: Val  callMacroExpander(Val, Val);
    private: bool checkSyntax(int, int, const char*, Val);
    private: void closeLexEnv();
    private: const TyFunction* computeFunty(const LambdaList*, const Type*);

    // [D]
    private: void deactivateLexEnv();

    // [E]
    private: Instruction* emitI(Instruction* pI)
    {
        if (NULL != m_pCurr)
        {
            m_pCurr->AppendI(pI);
            CLOG(1, "<li>~S</li>~%", pI);
        }
        return pI;
    } // emitI

    private: OpenBindI* emitBind(VarDef*, Operand*, OpenBindI* = NULL);
    private: Register*  emitBool(Bool*);

    private: void       emitCall(
        const Type*,
        Output*,
        const Callee*,
        Values* );

    private: Operand*   emitCall2(Val, Operand*, Operand*);
    private: Operand*   emitEq(Operand*, Operand*, Operand* = NULL);
    private: Operand*   emitLinkage(const Expect*, Register*);
    private: Operand*   emitLinkage(Operand*);
    private: Register*  emitLoadVar(const Type*, Val);
    private: Operand*   emitRestoreValues(Operand*);

    private: Register* emitCast(
        const Type*,
        Register*,
        Expect::Kind = Expect::Kind_Value );

    private: Operand*   emitSaveValues(Operand*);
    private: void       emitStoreVar(Val, Operand*);
    private: Operand*   emitSucc();
    private: Operand*   emitUnreachable();
    private: void       emitUnwind(ClFrame*);
    private: void       emitUnwinds(ClFrame*);
    private: Operand*   emitWeakRestoreValues(Operand*);
    private: Operand*   emitWeakSaveValues(Operand*);

    private: Operand*   endPhi(const Type*);
    private: Val        ensureVarName(Val);

    // [F]
    private: FunRef*  findFunRef(Val);
    private: Val      findVarCell(Val);
    private: VarRef*  findVarRef(Val);
    private: void     foldCallee(Operand*, Callee*);

    // [G]
    private: Linkage getLinkage() const;

    // [I]
    private: Operand* incompatibleTypes(const Type*, const Type*, Val);
    private: Operand* ignoreForm(Val);
    private: FunRef*  internFunRef(Val);
    private: FunRef*  internFunDcl(Val);
    private: NameRef* internNameDcl(Val);
    private: VarRef*  internVarDcl(Val);
    private: VarRef*  internVarRef(Val);
    private: Output*  internVarCell(VarDef*);

    // [M]
    private: void markFunUse(FunRef*);
    private: void markVarUse(VarRef*);

    // [N]
    private: BBlock*    newBBlock();
    private: Output*    newOutput(const Type*);
    private: VarParam*  newParam(Val);
    private: VarLet*    newVarLet(Val, Val = nil);

    private: Operand* not_yet_implemented(Val form)
    {
        warn("NYI ~S", form);
        return emitLinkage(Literal::New(nil));
    } // not_yet_implemented

    // [O]
    private: Operand* optimizeLexVars(Operand* = NULL);

    // [P]
    private: Operand* parseApply(const Expect*, Val);
    private: Operand* parseBlock(const Expect*, Val, Val, Val);

    private: Operand* parseBlockOrCatch(
        const Expect*,
        Val,
        Val,
        XferFrame*,
        Instruction* );

    private: Operand* parseCall(const Expect*, Val, const Callee*, Val);
    private: Operand* parseCompoundForm(const Expect*, Val);
    private: Operand* parseCons(const Expect*, Val);
    private: void     parseDeclare(Val);
    private: Val      parseDecls(Val, bool = true);
    private: Operand* parseForm(const Expect*, Val);
    private: Operand* parseForm1(const Expect*, Val);
    private: Operand* parseForms(const Expect*, Val, Val);
    private: Operand* parseFunctionForm(const Expect*, Val, const Callee*);
    private: Operand* parseLambdaExpr(const Expect*, Val);
    private: Operand* parseLetAux(const Expect*, Val, Val, OpenBindI*);
    private: Operand* parseLiteral(const Expect*, Val);

    private: void parseMissingValues(
        const Expect*,
        TyValues::Enum*,
        ValuesI* );

    private: Operand* parseOperand(Val, Type*, int, Val);
    private: Operand* parseReturnFrom(const Expect*, Val, Val, Val);
    private: Operand* parseSetfCall(const Expect*, Val, Val, Operand*);
    private: Operand* parseSymbol(const Expect*, Val);
    private: Operand* parseTypePredicate(const Expect*, Val, Val);

    private: Operand* parseForm1(
        Val     fname,
        int     iNth,
        Type*   pty,
        Val     form )
    {
        ExpectArg oExpect(fname, iNth, pty);
        return parseForm1(&oExpect, form);
    } // parseForm1

    private: bool parseForm2(
        const Expect*,
        const Expect*,
        Operand**,
        Operand**,
        Val );

    private: void parseError(const char* psz)
        { parseErrorV(psz, nil); }

    private: void parseError(const char* psz, Val a)
        { parseErrorV(psz, list(a)); }

    private: void parseError(const char* psz, Val a, Val b)
        { parseErrorV(psz, list(a, b)); }

    private: void parseError(const char* psz, Val a, Val b, Val c)
        { parseErrorV(psz, list(a, b, c)); }

    private: void parseError(const char* psz, Val a, Val b, Val c, Val d)
        { parseErrorV(psz, list(a, b, c, d)); }

    private: void parseError(const Expect*, Val, const Type*);

    private: void      parseErrorV(const char*, Val);
    private: void      parseFunBinds(Val);
    private: Function* parseLambda(Val);
    private: Val       parseLambdaList(Val, LambdaList*);
    private: Register* parseSlotValue(Operand*, Operand*);
    private: void      parseVarBinds(Val);

    private: Operand* processCall(const Expect*, Val, const Callee*, Values*);
    private: void     processFunDef(FunDef*);
    private: void     processLambdaExpr(LambdaList*, Val, Val);
    private: void     processLambdaList(LambdaList*, Values*);

    private: void processLambdaListOptional(BBlock*, VarParam*, Bool*);

    private: Register*  processParam(VarParam*, Values*, int);

    private: ClFrame* popFrame();
    private: void     pushFrame(ClFrame*);

    // [R]
    private: void rememberLineNumber(Val);

    public: void restoreSucc(BBlock* pSucc)
    {
        m_pSucc = NULL == pSucc ? m_pCurr : pSucc;
    } // restoreSucc

    public: void Run();

    // [S]
    private: BBlock* setContinue()
    {
        if (m_pCurr == m_pSucc)
        {
            return NULL;
        }
        BBlock* pSucc = m_pSucc;
        m_pSucc = m_pCurr;
        return pSucc;
    } // SetContinue

    private: void setCurrSucc(BBlock* pCurr, BBlock* pSucc)
    {
        m_pCurr = pCurr;
        m_pSucc = NULL == pSucc ? m_pCurr : pSucc;
    } // setCurrSucc

    private: void setPhiOperand(PhiI*, BBlock*, Operand*);

    private: Operand* setUnreachable()
    {
        m_pCurr = NULL;
        return Unreachable;
    } // setUnreachable

    private: BBlock* startPhi(const Type*);

    private: void styleWarn(const char* psz)
        { styleWarnV(psz, nil); }

    private: void styleWarn(const char* psz, Val a)
        { styleWarnV(psz, list(a)); }

    private: void styleWarn(const char* psz, Val a, Val b)
        { styleWarnV(psz, list(a, b)); }

    private: void styleWarnV(const char*, Val);

    // [U]
    private: Operand* unreachableForm(Val form)
    {
        styleWarn("Unreachable form: ~S", form);
        return Unreachable;
    } // unreachableForm

    private: Operand* uselessForm(Val form)
    {
        styleWarn("Useless form: ~S", form);
        return Unreachable;
    } // uselessForm

    // [W]
    private: void warn(const char* psz)
        { warnV(psz, nil); }

    private: void warn(const char* psz, Val a)
        { warnV(psz, list(a)); }

    private: void warn(const char* psz, Val a, Val b)
        { return warnV(psz, list(a, b)); }

    private: void warnV(const char*, Val);

    private: void writePrefix(const char*);
}; // PassParse

#define defparser(mp_name) \
    Operand* PassParse::parse_ ## mp_name(const Expect* pExpect, Val form)

#define defspecial(mp_name) \
    defparser(mp_name)

#define defnyi(mp_name) \
    defparser(mp_name) \
        { ASSERT(NULL != pExpect); return not_yet_implemented(form); }

#define CHECK_SYNTAX(mp_min, mp_max, mp_syntax) \
    if (! checkSyntax(mp_min, mp_max, mp_syntax, form)) \
    { \
        return parseLiteral(pExpect, nil); \
    }

} // Compiler

} // TinyCl

#endif //!defined(INCLUDE_tinycl_compiler_parser_h)
