//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Sink
// compiler/cg/tinycl_c_cg_sing.h
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cg/tinycl_c_cg_sink.h#1 $
//
#if !defined(INCLUDE_tinycl_c_cg_sink_h)
#define INCLUDE_tinycl_c_cg_sink_h

#include "../tinycl_c_defs.h"

namespace TinyCl
{

namespace Compiler
{

class ByteSink :
    public LocalObject
{
    private: uint   m_cbAlloc;
    private: uint   m_cb;
    private: uint8* m_prgb;
    private: IMm*   m_pIMm;

    // ctor
    public: ByteSink(IMm* pIMm) :
        m_cbAlloc(0),
        m_cb(0),
        m_pIMm(pIMm) {}

    // [C]
    public: void CopyTo(ByteSink* const that) const
    {
        if (m_cb + that->m_cb > that->m_cbAlloc)
        {
            that->enlarge(m_cb);
        }

        ::CopyMemory(that->m_prgb + that->m_cb, m_prgb, m_cb);

        that->m_cb += m_cb;
    } // CopyTo

    // [E]
    public: void EmitU16(uint16 u16)
    {
        EmitU8(static_cast<uint8>(u16 & 0xff));
        EmitU8(static_cast<uint8>((u16 >> 8) & 0xff));
    } // EmitU16

    public: void EmitU32(uint32 u32)
    {
        EmitU16(static_cast<uint16>(u32 & 0xffff));
        EmitU16(static_cast<uint16>((u32 >> 16) & 0xffff));
    } // EmitU16

    public: void EmitU8(uint8 u8)
    {
        if (m_cb + 1 > m_cbAlloc)
        {
            enlarge(1);
        }

        m_prgb[m_cb] = u8;
        m_cb += 1;
    } // EmitU8

    private: void enlarge(uint const cb)
    {
        m_cbAlloc = max((m_cbAlloc + cb) * 130 / 100, 256);
        uint8* prgb = m_prgb;
        m_prgb = reinterpret_cast<uint8*>(m_pIMm->Alloc(m_cbAlloc));
        ::CopyMemory(m_prgb, prgb, m_cb);
    } // enlarge

    public: class Enum
    {
        uint8* m_pEnd;
        uint8* m_p;

        public: Enum(const ByteSink* const p) :
            m_pEnd(p->m_prgb + p->m_cb),
            m_p(p->m_prgb) {}

        public: bool AtEnd() const
            { return m_p >= m_pEnd; }

        public: uint8 Get() const
            { ASSERT(! AtEnd()); return *m_p; }

        public: void Next()
            { ASSERT(! AtEnd()); m_p++; }
    }; // Enum

    // [G]
    public: uint GetPosn() const
        { return m_cb; }

    // [S]
    public: void Serialize(void* pv)
    {
        ::CopyMemory(pv, m_prgb, m_cb);
    } // Serialize
}; // ByteSink

template<uint BitsInWord>
class BitSink_
{
    private: uint32     m_nMask;
    private: int        m_nPosn;
    private: ByteSink*  m_pByteSink;
    private: uint32     m_rgbBit;

    // ctor
    public: BitSink_(ByteSink* const pByteSink) :
        m_nMask(1),
        m_nPosn(0),
        m_pByteSink(pByteSink),
        m_rgbBit(0) {}

    // [C]
    public: void CopyTo(ByteSink* const pByteSink) const
    {
        m_pByteSink->CopyTo(pByteSink);
    } // CopyTo

    // [F]
    public: void Flush()
    {
        switch (BitsInWord)
        {
        case 16:
            m_pByteSink->EmitU16(static_cast<uint16>(m_rgbBit));
            break;

        case 32:
            m_pByteSink->EmitU32(m_rgbBit);
            break;
        } // switch

        m_nMask  = 1;
        m_nPosn  = 0;
        m_rgbBit = 0;
    } // Flush

    // [G]
    public: uint GetPosn() const
        { return m_pByteSink->GetPosn(); }

    // [I]
    public: bool IsStartOfWord() const
        { return 0 == m_nPosn % BitsInWord; }

    // [L]
    public: uint32 Logior(uint32 x)
        { return m_rgbBit |= x; }

    // [W]
    public: void Write1(int nBit)
    {
        if (m_nPosn == BitsInWord)
        {
            Flush();
        }

        if (nBit)
        {
            m_rgbBit |= m_nMask;
        }

        m_nMask <<= 1;
        m_nPosn += 1;
    } // Write
}; // BitSink

} // Compiler
} // TinyCl

#endif //!defined(INCLUDE_tinycl_c_cg_sink_h)
