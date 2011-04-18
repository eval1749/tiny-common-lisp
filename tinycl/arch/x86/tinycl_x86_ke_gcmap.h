//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Definitions for X86 Target
// tinycl_x86.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/arch/x86/tinycl_x86_ke_gcmap.h#2 $
//
#if !defined(INCLUDE_tinycl_x86_ke_gcmap_h)
#define INCLUDE_tinycl_x86_ke_gcmap_h

namespace TinyCl
{

template<uint t_cb, typename t_Bits>
class GcMap_
{
    public: typedef t_Bits Bits;
    public: enum { BitsInWord = sizeof(Bits) * 8 };

    private: typedef GcMap_<t_cb, t_Bits> GcMap;

    private: static const uint km_cbSegment = t_cb * 64 * 1024;

    public: enum Kind
    {
        JumpDesc,       // 0
        StdCallDesc,    // 1
        CallDesc2,      // 2
        CallDesc3,      // 3
    }; // Kind

    private: const uint16* const m_p;
    private: uint          const m_cb;

    // ctor
    protected: GcMap_(const void* const pv, uint const cb) :
        m_cb(cb),
        m_p(reinterpret_cast<const uint16*>(pv)) {}

    private: GcMap& operator=(GcMap&)
        { CAN_NOT_HAPPEN(); }

    // [E]
    public: class Enum
    {
        private: uint m_cEntries;
        private: uint m_nSegment;
        private: uint m_nEntry;
        private: uint m_nSegmentEnd;

        private: const uint16* m_p;

        // ctor
        public: Enum(const GcMap* const p)
            { init(p->m_p, p_>m_cb); }

        public: Enum(const void* const pv, uint const cb)
            { init(pv, cb); }

        // [A]
        private: void advance()
        {
            while (m_nSegment < m_nSegmentEnd)
            {
                if (uint nIndex = m_p[m_nSegment])
                {
                    m_cEntries = m_p[nIndex];
                    m_nEntry   = nIndex + 1;
                    return;
                }
                m_nSegment += 1;
            } // while
        } // advance

        public: bool AtEnd() const
            { return m_nSegment >= m_nSegmentEnd; }

        // [G]
        public: uint Get() const
        {
            ASSERT(! AtEnd());
            return mapToCode(m_nSegment, m_p[m_nEntry]);
        } // Get

        public: const Bits* GetDesc() const
        {
            ASSERT(! AtEnd());

            return
                reinterpret_cast<const Bits*>(m_p) +
                m_p[m_nEntry + 1];
        } // GetDesc

        // [I]
        private: void init(
            const void* const pv,
            uint        const cb )
        {
            m_cEntries  = 0;
            m_nEntry    = 0;
            m_nSegment  = 0;
            m_nSegmentEnd = Ceiling(cb, km_cbSegment);
            m_p           = reinterpret_cast<const uint16*>(pv);
            advance();
        } // init

        // [N]
        public: void Next()
        {
            m_nEntry += 2;
            m_cEntries -= 1;
            if (0 == m_cEntries)
            {
                m_nSegment += 1;
                advance();
            }
        } // Next
    }; // Enum

    // [F]
    // <summary>
    //   Finds next GC point at specified offset or after.
    // </summary>
    public: uint FindAfter(uint const ofsCode) const
    {
        uint const nSegment = ofsCode / km_cbSegment;
        uint const ofs = ofsCode % km_cbSegment / t_cb;

        const uint16* pEntry = m_p + m_p[nSegment];
        uint const cEntries = *pEntry;

        if (cEntries >= 1 && ofs <= pEntry[(cEntries - 1) * 2])
        {
            uint low = 0;
            uint high = cEntries - 1;
            while (low <= high)
            {
                uint const middle = (high + low) / 2;
                uint const nPresent = pEntry[middle * 2];
                if (ofs == nPresent)
                {
                    return mapToCode(nSegment, nPresent);
                }

                if (ofs < nPresent)
                {
                    high = middle - 1;
                }
                else
                {
                    low = middle + 1;
                }
            } // while

            return mapToCode(nSegment, pEntry[low * 2]);
        }
        else
        {
            uint const nEnd = Ceiling(m_cb, km_cbSegment);
            for (uint nRunner = nSegment + 1; nRunner < nEnd; nRunner += 1)
            {
                const uint16* const pEntry = m_p + m_p[nRunner];
                if (0 != *pEntry)
                {
                    return mapToCode(pEntry[1]);
                }
            } // for

            return 0;
        } // if
    } // FindAfter

    public: const Bits* FindAt(UInt ofsInsn) const
    {
        uint const nSegment = ofsInsn / km_cbSegment;

        const uint16* pEntry = m_p + m_p[nSegment];
        uint const cEntries = *pEntry;
        if (0 == cEntries)
        {
            return NULL;
        }

        pEntry++;

        uint const ofs = ofsInsn % km_cbSegment / t_cb;
        if (ofs > pEntry[(cEntries - 1) * 2])
        {
            return NULL;
        }

        uint low = 0;
        uint high = cEntries - 1;
        while (low <= high)
        {
            uint const middle = (high + low) / 2;
            uint const nPresent = pEntry[middle * 2];
            if (ofs == nPresent)
            {
                return mapToGcDesc(pEntry[middle * 2 + 1]);
            }

            if (ofs < nPresent)
            {
                high = middle - 1;
            }
            else
            {
                low = middle + 1;
            }
        } // while

        return NULL;
    } // FindAt

    // [M]
    private: static uint mapToCode(uint const nSegment, uint const ofs)
    {
        return ofs * t_cb + nSegment * km_cbSegment;
    } // mapToCode
    
    private: const Bits* mapToGcDesc(uint const ofs) const
    {
        return reinterpret_cast<const Bits*>(m_p + ofs);
    } // mapToGcDesc
}; // GcMap_

class GcMap :
    public GcMap_<1, uint16>
{
    private: typedef GcMap_<1, uint16> GcMapBase;
    
    public: static uint const cbCall = 5;

    public: GcMap(const void* const pv, uint const cb) :
        GcMapBase(pv, cb) {}
}; // GcMap

} // TinyCl

#endif // !defined(INCLUDE_tinycl_x86_ke_gcmap_h)
