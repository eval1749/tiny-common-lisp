//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer Undo
// editor/ed_buffer_undo.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_undo.h#4 $
//
#if !defined(INCLUDE_editor_undo_h)
#define INCLUDE_editor_undo_h

#include "./ed_buffer_core.h"

namespace Editor
{

class UndoRecord :
    public EditorObject_<UndoRecord, Layout_undo_record>
{
    protected: UndoRecord()
    {
        m_next = m_prev = nil;
    } // UndoRecord
    
    // [G]
    public: Val GetNext() const { return m_next; }
    public: Val GetPrev() const { return m_prev; }

    // [I]
    public: static bool Is_(const Datum* const x)
    {
        return Record::Is_(x);
    } // Is_

    // [S]
    public: Val  SetNext(Val x) { return m_next = x; }
    public: Val  SetPrev(Val x) { return m_prev = x; }

    // [U]
    public: void Unlink() { m_next = m_prev = nil; }
}; // UndoRecord

class BufferUndo : public BufferCore
{
    private: class UndoRecords_ : public Layout_buffer
    {
        public: Val GetHead() const { return m_first_undo; }
        public: Val GetTail() const { return m_last_undo; }

        public: Val  SetHead(Val x) { return m_first_undo = x; }
        public: Val  SetTail(Val x) { return m_last_undo  = x; }
    }; //UndoRecords_

    public: typedef DoubleLinkedList_<UndoRecords_, UndoRecord> UndoRecords;

    // ctor
    protected: BufferUndo();

    // [C]
    public:    bool CanRedo() const;
    public:    bool CanUndo() const;
    protected: bool checkPoint();

    // [E]
    protected: void emptyLog();

    // [I]
    public: bool IsReadOnly() const
        { return nil != m_read_only; }

    public: bool IsUndoDisabled() const
        { return nil == m_undo_state; }

    // [L]
    public: void LogBegin(Val, Val);
    public: void LogDelete(Posn, Posn);
    public: void LogEnd(Val);
    public: void LogInsert(Posn, Posn);

    // [R]
    public: Posn Redo(Posn, Count = one);

    // [T]
    protected: void truncateLog();

    // [U]
    public: Posn Undo(Posn, Count = one);

    public: class UndoBlock
    {
        private: Val            m_name;
        private: BufferUndo*    m_p;

        // ctor
        public: UndoBlock(BufferUndo* p, Val name, Val posn)
            { init(p, name, posn); }

        public: UndoBlock(Range*, Val, Val = nil);
        public: UndoBlock(Selection*, Val, Val = nil);
        public: ~UndoBlock();

        // [I]
        private: void init(BufferUndo*, Val, Val);
    }; // UndoBlock

    friend class UndoBlock;
}; // BufferUndo

CASSERT(sizeof(BufferUndo) == sizeof(Layout_buffer));

} // Editor

#endif //!defined(INCLUDE_editor_undo_h)
