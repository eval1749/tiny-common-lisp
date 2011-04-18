//////////////////////////////////////////////////////////////////////////////
//
// Editor - Range
// editor/ed_range.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_range.h#4 $
//
#if !defined(INCLUDE_editor_range_h)
#define INCLUDE_editor_range_h

#include "./ed_defs.h"

namespace Editor
{

class Range :
    public EditorObject_<Range, Layout_range>
{
    public: static Val ClassD_() { return CLASSD_range; }

    public: static bool Is_(const Datum* const x)
    {
        if (Record* p = x->DynamicCast<Record>())
        {
            if (p->m_classd == CLASSD_range) return true;
            if (p->m_classd == CLASSD_selection) return true;
        }
        return false;
    } // Is_

    // ctor
    public: Range(Val, Val = zero, Val = zero);
    protected: Range() {}

    // [C]
    public: void Collapse(Val);
    public: Count Copy() const;
    public: Count Cut();

    // [D]
    public: Count Delete(Unit, Count);

    // [E]
    public: Count EndOf(Unit, Val = nil);

    // [F]
    public: Posn FindCloseParen(char16 = 0, char16 = 0) const;
    public: Posn FindOpenParen(char16 = 0, char16 = 0)  const;

    // [G]
    public: Buffer* GetBuffer() const;
    public: Posn    GetEnd()   const { return m_end; }
    public: Posn    GetStart() const { return m_start; }
    public: Val     GetText()  const;

    // [I]
    public: void Init(Val, Val, Val);

    // [M]
    public: Count Move(Unit, Count);
    public: Count MoveEndWhile(const char*, Count = Kforward);
    public: Count MoveStartWhile(const char*, Count = Kforward);
    public: Count MoveWhile(const char*, Count = Kforward);

    // [P]
    public: Count Paste();

    // [S]
    public: Posn SetEnd(Posn);
    public: void SetRange(Val);
    public: void SetRange(Posn, Posn);
    public: Posn SetStart(Posn);
    public: void SetText(const char16*, int);
    public: void SetText(Val);
    public: Count StartOf(Unit, Val = nil);

    // For DoubleLinkedItem
    // [G]
    public: Val GetNext() const { return m_next; }
    public: Val GetPrev() const { return m_prev; }

    // [S]
    public: Val  SetNext(Val x) { return m_next = x; }
    public: Val  SetPrev(Val x) { return m_prev = x; }

    // [U]
    public: void Unlink() { m_next = m_prev = nil; }
}; // Range

CASSERT(sizeof(Range) == sizeof(Layout_range));

class ALIGN_RECORD StackRange : public Range
{
    public: StackRange(Val);
    public: StackRange(Range*);
    public: StackRange(Selection*);
    public: ~StackRange();
    private: void init(Buffer*, Posn, Posn);
}; // StackRange


defpred(in_range, (Val, Val));

} // Editor

#endif //!defined(INCLUDE_editor_range_h)
