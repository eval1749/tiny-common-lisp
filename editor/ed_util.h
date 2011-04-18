//////////////////////////////////////////////////////////////////////////////
//
// Editor - Declarations
// editor/ed_defs.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_util.h#4 $
//
#if !defined(INCLUDE_editor_util_h)
#define INCLUDE_editor_util_h

//////////////////////////////////////////////////////////////////////
//
// Global
//
template<class T>
class Global
{
    HGLOBAL m_h;
    T*      m_p;

    public: Global() : m_h(NULL), m_p(NULL) {}

    public: ~Global()
    {
        if (m_h != NULL)
        {
            if (m_p != NULL) ::GlobalUnlock(m_h);
            ::GlobalFree(m_h);
        }
    } // ~Global

    public: operator HANDLE() 
        { return reinterpret_cast<HANDLE>(m_h); }
    
    public: bool Alloc(size_t cb)
    {
        ASSERT(m_h == NULL);
        m_h = ::GlobalAlloc(GMEM_MOVEABLE, cb);
        return m_h != NULL;
    } // Alloc

    public: void Detach()
    {
        ASSERT(m_p == NULL);
        ASSERT(m_h != NULL);
        m_h = NULL;
    } // Detach

    public: T* Lock()
    {
        if (m_h == NULL) return NULL;
        return m_p = reinterpret_cast<T*>(::GlobalLock(m_h));
    } // Lock

    public: void Unlock()
    {
        if (m_h == NULL) return;
        if (m_p == NULL) return;
        m_p = NULL;
        ::GlobalUnlock(m_h);
    } // Unlock
}; // Global


//////////////////////////////////////////////////////////////////////
//
// Clipboard
//
class Clipboard
{
    BOOL m_fSucceeded;
    mutable HANDLE m_hGlobal;

    public: Clipboard() : m_hGlobal(NULL)
        { m_fSucceeded = ::OpenClipboard(NULL); }

    public: ~Clipboard()
    {
        if (NULL != m_hGlobal) ::GlobalUnlock(m_hGlobal);
        if (m_fSucceeded) ::CloseClipboard();
    } // ~Clipboard

    public: BOOL Empty()
        { return ::EmptyClipboard(); }

    public: char16* GetText() const
    {
        m_hGlobal = ::GetClipboardData(CF_UNICODETEXT);
        if (NULL == m_hGlobal) return NULL;

        return reinterpret_cast<char16*>(::GlobalLock(m_hGlobal));
    } // GetText

    public: BOOL HasFormat(uint uFormat) const
        { return ::IsClipboardFormatAvailable(uFormat); }

    public: BOOL IsOpen() const { return m_fSucceeded; }

    public: bool Set(HANDLE h)
        { return ::SetClipboardData(CF_UNICODETEXT, h) != NULL; }
}; // Clipboard

namespace Editor
{
    bool char_in_set_p(char16, Val);
} // Editor

#endif //!defined(INCLUDE_editor_util_h)
