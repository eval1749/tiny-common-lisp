//////////////////////////////////////////////////////////////////////////////
//
// Regex - Utilities
// regex_util.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/regex/regex_util.h#2 $
//
#if !defined(INCLUDE_regex_util_h)
#define INCLUDE_regex_util_h

#include "../tinycl/z_util.h"

namespace Regex
{

using namespace Common;

namespace RegexPrivate
{

//////////////////////////////////////////////////////////////////////
//
// LocalHeap
//
class LocalHeap
{
    private: HANDLE m_hHeap;

    public: LocalHeap() :
        m_hHeap(::HeapCreate(HEAP_NO_SERIALIZE, 0, 0)) {}

    public: ~LocalHeap()
    {
        if (NULL != m_hHeap)
        {
            ::HeapDestroy(m_hHeap);
        }
    } // ~Context

    // [A]
    public: void* Alloc(size_t cb)
        { return ::HeapAlloc(m_hHeap, 0, cb); }

    // [F]
    public: void Free(void* pv)
        { ::HeapFree(m_hHeap, 0, pv); }
}; // LocalHeap

class LocalObject
{
    private: void operator delete(void*) {}
    private: void (operator delete[])(void*) {}

    public: void* operator new(size_t cb, LocalHeap* pHeap)
        { return pHeap->Alloc(cb); }

    public: void* (operator new[])(size_t cb, LocalHeap* pHeap)
        { return pHeap->Alloc(cb); }
}; // LocalObject

//////////////////////////////////////////////////////////////////////
//
// CharSink
//
class CharSink
{
    private: LocalHeap* m_pHeap;
    private: char16*    m_pwch;
    private: char16*    m_pwchEnd;
    private: char16*    m_pwchStart;
    private: char16     m_rgwch[20];

    // ctor
    public: CharSink(LocalHeap* pHeap) :
        m_pHeap(pHeap),
        m_pwch(m_rgwch),
        m_pwchEnd(m_rgwch + lengthof(m_rgwch)),
        m_pwchStart(m_rgwch) {}

    // [A]
    public: void Add(char16 ch)
    {
        if (m_pwch + 2 > m_pwchEnd)
        {
            grow();
        }

        *m_pwch++ = ch;
        *m_pwch = 0;
    } // Add

    // [G]
    public: char16 Get(int iIndex) const
    {
        ASSERT(static_cast<uint>(iIndex) < static_cast<uint>(GetLength()));
        return m_pwchStart[iIndex];
    } // Get

    public: int GetLength() const
        { return static_cast<int>(m_pwch - m_pwchStart); }

    public: const char16* GetStart() const
        { return m_pwchStart; }

    private: void grow()
    {
        int cwch = GetLength();
        int cwchNew = cwch * 130 / 100;

        char16* pwchNew = reinterpret_cast<char16*>(
            m_pHeap->Alloc(sizeof(char16) * cwchNew) );

        CopyMemory(pwchNew, m_pwchStart, sizeof(char16) * cwch);

        m_pwchStart = pwchNew;
        m_pwchEnd   = pwchNew + cwchNew;
        m_pwch      = pwchNew + cwch;
    } // grow

    // [R]
    public: void Reset()
        { m_pwch = m_pwchStart; }

    // [S]
    public: char16* Save(LocalHeap* pHeap) const
    {
        int cwch = GetLength();
        char16* pwsz = reinterpret_cast<char16*>(
            pHeap->Alloc(sizeof(char16) * (cwch + 1)) );
        ::CopyMemory(pwsz, m_pwchStart, sizeof(char16) * cwch);
        pwsz[cwch] = 0;
        return pwsz;
    } // Save

    #if 0
        public: char16* Save() const
        {
            int cwch = GetLength();
            char16* pwsz = new char16[cwch + 1];
            ::CopyMemory(pwsz, m_pwchStart, sizeof(char16) * cwch);
            pwsz[cwch] = 0;
            return pwsz;
        } // Get
    #endif

    public: void xShrink()
    {
        ASSERT(m_pwch > m_pwchStart);
        --m_pwch;
    } // Shrink
}; // CharSink

//////////////////////////////////////////////////////////////////////
//
// Sink
//
template<class T>
class Sink
{
    private: LocalHeap* m_pHeap;
    private: T*         m_pwch;
    private: T*         m_pwchEnd;
    private: T*         m_pwchStart;
    private: T          m_rgwch[20];

    // ctor
    public: Sink(LocalHeap* pHeap) :
        m_pHeap(pHeap),
        m_pwch(m_rgwch),
        m_pwchEnd(m_rgwch + lengthof(m_rgwch)),
        m_pwchStart(m_rgwch) {}

    // [A]
    public: void Add(T ch)
    {
        if (m_pwch + 1 > m_pwchEnd)
        {
            grow();
        }

        *m_pwch++ = ch;
    } // Add

    // [G]
    public: const T* Get() const
        { return m_pwchStart; }

    public: T Get(int iIndex) const
    {
        ASSERT(iIndex < GetLength());
        return m_pwchStart[iIndex];
    } // Get

    public: int GetLength() const
        { return static_cast<int>(m_pwch - m_pwchStart); }

    public: size_t GetSize() const
        { return sizeof(T) * GetLength(); }

    private: void grow()
    {
        int cwch = GetLength();
        int cwchNew = cwch * 130 / 100;

        T* pwchNew = reinterpret_cast<T*>(
            m_pHeap->Alloc(sizeof(T) * cwchNew) );

        ::CopyMemory(pwchNew, m_pwchStart, sizeof(T) * cwch);

        m_pwchStart = pwchNew;
        m_pwchEnd   = pwchNew + cwchNew;
        m_pwch      = pwchNew + cwch;
    } // grow

    // [S]
    public: void Serialize(void* pv) const
    {
        size_t cb = GetLength() * sizeof(T);
        ::CopyMemory(pv, m_pwchStart, cb);
    } // Serialize

    public: void Set(int iIndex, T val)
    {
        ASSERT(iIndex < GetLength());
        m_pwchStart[iIndex] = val;
    } // Set

    public: void Shrink()
    {
        ASSERT(m_pwch > m_pwchStart);
        --m_pwch;
    } // Shrink
}; // Sink


char16* lstrchrW(const char16* pwsz, char16 wch);
bool IsWhitespace(char16 wch);

} // RegexPrivate
} // Regex

#endif // !defined(INCLUDE_regex_util_h)
