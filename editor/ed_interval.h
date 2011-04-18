//////////////////////////////////////////////////////////////////////////////
//
// Editor - Interval
// editor/ed_interval.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_interval.h#6 $
//
#if !defined(INCLUDE_editor_interval_h)
#define INCLUDE_editor_interval_h

#include "./ed_defs.h"

namespace Editor
{

class Interval :
    public EditorObject_<Interval, Layout_interval>
{
    public: static Val ClassD_() { return CLASSD_interval; }

    // ctor
    public: Interval(Val, Val, Val);
    protected: Interval() {}

    // [C]
    public: bool CanMerge(const Interval*) const;

    // [G]
    public: Buffer* GetBuffer() const;
    public: Posn    GetEnd()   const { return m_end; }
    public: Posn    GetStart() const { return m_start; }

    // [I]
    public: bool In(Posn posn) const
        { return xge(posn, m_start) && xlt(posn, m_end); }

    // [S]
    public: void SetStyle(const Interval*);
    public: void SetStyle(Val);

    // For DoubleLinkedItem
    // [G]
    public: Val GetNext() const { return m_next; }
    public: Val GetPrev() const { return m_prev; }

    // [S]
    public: Val  SetNext(Val x) { return m_next = x; }
    public: Val  SetPrev(Val x) { return m_prev = x; }

    // [U]
    public: void Unlink() { m_next = m_prev = nil; }
}; // Interval

CASSERT(sizeof(Interval) == sizeof(Layout_interval));

} // Editor

#endif //!defined(INCLUDE_editor_interval_h)
