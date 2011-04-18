//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Definitions for Gen Target
// tinycl_gen.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/generic/tinycl_gen.h#7 $
//
#if !defined(INCLUDE_tinycl_gen_h)
#define INCLUDE_tinycl_gen_h

#include "../../tinycl.h"

namespace TinyCl
{

namespace Generic
{

class ExitPointFrame : public Frame
{
    public: Val     m_sp;           // [2]
    public: Val     m_fn;           // [3]

    // ctor
    protected: ExitPointFrame() {}

    // [I]
    public: bool IsAbandoned() const
        { return Fixnum::Encode(0) == m_sp; }

    // [M]
    public: void MakeAbandoned()
        { m_sp = Fixnum::Encode(0); }
}; // ExitPointFrame

class XferFrame : public ExitPointFrame
{
    public: uint    m_nIp;          // [4]
    public: Val     m_name;         // [5]
    public: Val     m_n;            // [6]
    public: Val     mv_value[3];    // [7]
                                    // [8]
                                    // [9]

    // ctor
    protected: XferFrame() {}

    // [T]

    // Control transfer specified in frame. This method is implemented
    // in arch/${arch}/tinycl_${arch}_${platform}.cpp for setting IP and SP.
    public: void __declspec(noreturn) __fastcall Transfer(Thread*);
}; // XferFrame

class BlockFrame : public XferFrame
{
    public: static Type Type_() { return Type_Block; }
}; // BlockFrame

class CatchFrame : public XferFrame
{
    public: static Type Type_() { return Type_Catch; }
}; // CatchFrame

class FinallyFrame : public Frame
{
    public: static Type Type_() { return Type_Finally; }

    public: Val m_finally;
    public: Val m_n;

    // [U]
    public: void __fastcall Unwind(Thread*);
}; // FinallyFrame

class HandlerFrame : public Frame
{
    public: static Type Type_() { return Type_Handler; }

    public: struct Catch
    {
        Val m_type;
        Val m_fn;
    }; // Catch

    public: UInt m_cbCatchs; // [4]

    // [E]
    public: class EnumCatch
    {
        private: Catch*   m_pEnd;
        private: Catch*   m_pRunner;

        public: EnumCatch(const HandlerFrame* p) :
            m_pEnd(p->getLastCatch()),
            m_pRunner(p->getFirstCatch()) {}

        public: bool   AtEnd() const { return m_pRunner >= m_pEnd; }
        public: Catch* Get()   const { ASSERT(!AtEnd()); return m_pRunner; }
        public: void   Next()        { ASSERT(!AtEnd()); m_pRunner++; }
    }; // EnumCatch

    // [G]
    private: Catch* getFirstCatch() const
        { return reinterpret_cast<Catch*>(reinterpret_cast<Int>(this + 1)); }

    private: Catch* getLastCatch() const
        { return getFirstCatch() + (m_cbCatchs / sizeof(Catch)); }
}; // HandlerFrame

class TagsFrame : public ExitPointFrame
{
    public: static Type Type_() { return Type_Tagbody; }

    public: struct Tag
    {
        Val     m_name;
        uint    m_nIp;
    }; // Tag

    public: UInt m_cbTags; // [4]

    // [E]
    public: class EnumTag
    {
        private: Tag*   m_pEnd;
        private: Tag*   m_pRunner;

        public: EnumTag(const TagsFrame* p) :
            m_pEnd(p->getLastTag()),
            m_pRunner(p->getFirstTag()) {}

        public: bool AtEnd() const { return m_pRunner >= m_pEnd; }
        public: Tag* Get()   const { ASSERT(!AtEnd()); return m_pRunner; }
        public: void Next()        { ASSERT(!AtEnd()); m_pRunner++; }
    }; // EnumTag

    // [G]
    private: Tag* getFirstTag() const
        { return reinterpret_cast<Tag*>(reinterpret_cast<Int>(this + 1)); }

    private: Tag* getLastTag() const
        { return getFirstTag() + (m_cbTags / sizeof(Tag)); }

    // [H]
    public: bool HasTag(const Tag* pTag) const
    {
        foreach (EnumTag, oEnum, this)
        {
            if (oEnum.Get() == pTag) return true;
        }
        return false;
    } // HasTag

    // [I]
    public: bool IsAbandoned() const
        { return Fixnum::Encode(0) == m_sp; }

    // [M]
    public: void MakeAbandoned()
        { m_sp = Fixnum::Encode(0); }

    // [T]

    // Control transfer specified in frame. This method is implemented
    // in arch/${arch}/tinycl_${arch}_${platform}.cpp for setting IP and SP.
    public: void __declspec(noreturn) __fastcall Transfer(Thread*, Tag*);
}; // TagsFrame

CASSERT(sizeof(TagsFrame) == sizeof(Val) * 5);

class TryCatchFrame : public ExitPointFrame
{
    public: static Type Type_() { return Type_TryCatch; }

    public: struct Catch
    {
        Val     m_type;
        uint    m_nIp;
    }; // Catch

    public: UInt m_cbCatchs; // [4]

    // [E]
    public: class EnumCatch
    {
        private: Catch*   m_pEnd;
        private: Catch*   m_pRunner;

        public: EnumCatch(const TryCatchFrame* p) :
            m_pEnd(p->getLastCatch()),
            m_pRunner(p->getFirstCatch()) {}

        public: bool   AtEnd() const { return m_pRunner >= m_pEnd; }
        public: Catch* Get()   const { ASSERT(!AtEnd()); return m_pRunner; }
        public: void   Next()        { ASSERT(!AtEnd()); m_pRunner++; }
    }; // EnumCatch

    // [G]
    private: Catch* getFirstCatch() const
        { return reinterpret_cast<Catch*>(reinterpret_cast<Int>(this + 1)); }

    private: Catch* getLastCatch() const
        { return getFirstCatch() + (m_cbCatchs / sizeof(Catch)); }

    // [T]
    // Control transfer specified in frame. This method is implemented
    // in arch/${arch}/tinycl_${arch}_${platform}.cpp for setting IP and SP.
    public: void __declspec(noreturn) __fastcall
        Transfer(Thread*, Catch*, Val);
}; // TryCatchFrame

CASSERT(sizeof(TryCatchFrame) == sizeof(Val) * 5);

class FromForeignFrame : public Frame
{
    public: static Type Type_() { return Type_FromForeign; }

}; // FromForeignFrame

/// <summary>
///   Represents ToKernel frame used by DLL wrapper to prevent invoking GC.
/// </summary>
class ToForeignFrame : public Frame
{
    public: static Type Type_() { return Type_ToForeign; }

    public: UInt m_cbArgs;
    public: RaSlot* GetRaSlot();
}; // ToForeignFrame

/// <summary>
///   Represents ToKernel frame used by DLL wrapper to allow invoking GC.
/// </summary>
class ToKernelFrame : public ToForeignFrame
{
    public: static Type Type_() { return Type_ToKernel; }
}; // ToKernelFrame

} // Generic

/// <summary>
///   RA address slot in stack.
/// </summary>
class RaSlot : public AsInt
{
    public: UInt m_nIp;
}; // RaSlot

} // TinyCl

#endif // !defined(INCLUDE_tinycl_gen_h)
