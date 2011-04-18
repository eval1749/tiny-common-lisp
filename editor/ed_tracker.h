//////////////////////////////////////////////////////////////////////////////
//
// Editor - Tracker
// editor/ed_tracker.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_tracker.h#2 $
//
#if !defined(INCLUDE_editor_tracker_h)
#define INCLUDE_editor_tracker_h

#include "./ed_defs.h"

namespace Editor
{

class Tracker :
    public EditorObject_<Tracker, Layout_tracker>
{
    public: static Val ClassD_() { return CLASSD_tracker; }

    // ctor
    public: Tracker(Val);

    // [G]
    public: Buffer* GetBuffer() const;

    // [R]
    public: void Reset();

    // For DoubleLinkedItem
    // [G]
    public: Val GetNext() const { return m_next; }
    public: Val GetPrev() const { return m_prev; }

    // [S]
    public: Val  SetNext(Val x) { return m_next = x; }
    public: Val  SetPrev(Val x) { return m_prev = x; }

    // [U]
    public: void Unlink() { m_next = m_prev = nil; }
}; // Tracker

CASSERT(sizeof(Tracker) == sizeof(Layout_tracker));

} // Editor

#endif //!defined(INCLUDE_editor_tracker_h)
