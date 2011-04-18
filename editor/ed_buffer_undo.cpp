#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer Undo
// editor/ed_buffer_undo.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_undo.cpp#8 $
//
//
#define DEBUG_REDO 0
#define DEBUG_UNDO 0
#include "./ed_buffer_undo.h"

namespace Editor
{

using namespace TinyCl;

namespace Private
{

bool g_fTruncate;

class RecordBase;

template<class T, class B>
class UndoRecord_ : public EditorObject_<T, B>
{
    public: RecordBase* GetNext() const
    {
        if (nil == m_next) return NULL;
        return m_next->StaticCast<RecordBase>();
    } // GetNext

    public: RecordBase* GetPrev() const
    {
        if (nil == m_prev) return NULL;
        return m_prev->StaticCast<RecordBase>();
    } // GetPrev
}; // UndoRecord_

class RecordBase :
    public UndoRecord_<RecordBase, Layout_undo_record>
{
    public: static bool Is_(const Datum* const x)
    {
        if (Record* p = x->DynamicCast<Record>())
        {
            if (p->m_classd == CLASSD_undo_begin_record)  return true;
            if (p->m_classd == CLASSD_undo_delete_record) return true;
            if (p->m_classd == CLASSD_undo_end_record)    return true;
            if (p->m_classd == CLASSD_undo_insert_record) return true;
        }
        return false;
    } // Is_

    // [G]
    public: Posn GetAfterRedo() const;
    public: Posn GetAfterUndo() const;

    public: Posn GetBeforeRedo() const;
    public: Posn GetBeforeUndo() const;

    // [I]
    public: template<typename T> bool Is() const
        { return T::ClassD_() == m_classd; }

    // [R]
    public: void Redo(BufferUndo*);

    // [U]
    public: void Undo(BufferUndo*);
}; // Record

class BeginRecord :
    public UndoRecord_<BeginRecord, Layout_undo_begin_record>
{
    public: static Val ClassD_() { return CLASSD_undo_begin_record; }

    // ctor
    public: BeginRecord(Val name)
    {
        m_next = m_prev = nil;
        m_name = name;
    } // BeginRecord

    // [G]
    public: Posn GetAfterRedo()  const { return GetNext()->GetAfterRedo(); }
    public: Posn GetAfterUndo()  const { return GetNext()->GetAfterUndo(); }
    public: Posn GetBeforeRedo() const { return GetNext()->GetBeforeRedo(); }
    public: Posn GetBeforeUndo() const { return GetNext()->GetBeforeUndo(); }

    // [R]
    public: void Redo(BufferCore*) {}

    // [U]
    public: void Undo(BufferUndo* p)
    {
        p->LogEnd(m_name);
    } // Undo
}; // BeginRecord

class DeleteRecord :
    public UndoRecord_<DeleteRecord, Layout_undo_delete_record>
{
    public: static Val ClassD_() { return CLASSD_undo_delete_record; }

    // ctor
    public: DeleteRecord(BufferUndo* pBuffer, Posn start, Posn end)
    {
        m_next = m_prev = nil;

        m_start = start;
        m_end   = end;

        Val string = Thread::Get()->AllocBinVec(
            CLASSD_simple_string, 
            xsub(end, start) );

        m_strings = list(string);

        pBuffer->GetText(
            start,
            end,
            string->StaticCast<SimpleString>()->GetStart() );
    } // DeleteRecord

    // [G]
    public: Posn GetAfterRedo() const
        { return m_end; }

    public: Posn GetAfterUndo() const
        { return m_end; }

    public: Posn GetBeforeRedo() const
        { return m_start; }

    public: Posn GetBeforeUndo() const
        { return m_start; }

    // [I]
    private: void insertStrings(BufferUndo* pBuffer)
    {
        Posn posn = m_start;
        foreach (List::Enum, oEnum, m_strings)
        {
            SimpleString* p = oEnum.Get()->StaticCast<SimpleString>();

            pBuffer->InternalInsert(posn, p->GetStart(), p->GetLength());

            posn = xxadd(posn, p->GetLength());
        } // for each string
    } // insertStrings

    // [R]
    public: void Redo(BufferUndo* pBuffer)
    {
        insertStrings(pBuffer);
        pBuffer->m_char_tick = xxadd(pBuffer->m_char_tick, 1);
    } // Redo

    // [U]
    public: void Undo(BufferUndo* pBuffer)
    {
        insertStrings(pBuffer);
        pBuffer->LogInsert(m_start, m_end);
        pBuffer->m_char_tick = xxsub(pBuffer->m_char_tick, 1);
    } // Undo
}; // DeleteRecord

class EndRecord :
    public UndoRecord_<EndRecord, Layout_undo_end_record>
{
    public: static Val ClassD_() { return CLASSD_undo_end_record; }

    // ctor
    public: EndRecord(Val name)
    {
        m_next = m_prev = nil;
        m_name = name;
    } // EndRecord

    // [C]
    public: bool CanMerge(Val name, Val posn)
    {
        if (nil == posn)    return false;
        if (m_name != name) return false;
        return GetBeforeUndo() == posn;
    } // CanMerge

    // [G]
    public: Posn GetAfterRedo()  const { return GetPrev()->GetAfterRedo(); }
    public: Posn GetAfterUndo()  const { return GetPrev()->GetAfterUndo(); }
    public: Posn GetBeforeRedo() const { return GetPrev()->GetBeforeRedo(); }
    public: Posn GetBeforeUndo() const { return GetPrev()->GetBeforeUndo(); }

    // [R]
    public: void Redo(BufferCore*) {}

    // [U]
    public: void Undo(BufferUndo* pBuffer)
    {
        pBuffer->LogBegin(m_name, GetBeforeUndo());
    } // Undo
}; // EndRecord

class InsertRecord :
    public UndoRecord_<InsertRecord, Layout_undo_insert_record>
{
    public: static Val ClassD_() { return CLASSD_undo_insert_record; }

    // ctor
    public: InsertRecord(Posn start, Posn end)
    {
        m_next = m_prev = nil;

        m_start = start;
        m_end   = end;
    } // InsertRecord

    // [G]
    public: Posn GetAfterRedo()  const { return m_start; }
    public: Posn GetAfterUndo()  const { return m_start; }
    public: Posn GetBeforeRedo() const { return m_end; }
    public: Posn GetBeforeUndo() const { return m_end; }

    // [R]
    public: void Redo(BufferUndo* pBuffer)
    {
        pBuffer->InternalDelete(m_start, m_end);
        pBuffer->m_char_tick = xxadd(pBuffer->m_char_tick, 1);
    } // Redo

    // [U]
    public: void Undo(BufferUndo* pBuffer)
    {
        pBuffer->LogDelete(m_start, m_end);
        pBuffer->InternalDelete(m_start, m_end);
        pBuffer->m_char_tick = xxsub(pBuffer->m_char_tick, 1);
    } // Undo
}; // InsertRecord

Posn RecordBase::GetAfterRedo() const
{
    if (BeginRecord* p = Encode()->DynamicCast<BeginRecord>())
    {
        return p->GetAfterRedo();
    }

    if (DeleteRecord* p = Encode()->DynamicCast<DeleteRecord>())
    {
        return p->GetAfterRedo();
    }

    if (EndRecord* p = Encode()->DynamicCast<EndRecord>())
    {
        return p->GetAfterRedo();
    }

    if (InsertRecord* p = Encode()->DynamicCast<InsertRecord>())
    {
        return p->GetAfterRedo();
    }

    CAN_NOT_HAPPEN();
} // RecordBase::GetAfterUndo

Posn RecordBase::GetAfterUndo() const
{
    if (BeginRecord* p = Encode()->DynamicCast<BeginRecord>())
    {
        return p->GetAfterUndo();
    }

    if (DeleteRecord* p = Encode()->DynamicCast<DeleteRecord>())
    {
        return p->GetAfterUndo();
    }

    if (EndRecord* p = Encode()->DynamicCast<EndRecord>())
    {
        return p->GetAfterUndo();
    }

    if (InsertRecord* p = Encode()->DynamicCast<InsertRecord>())
    {
        return p->GetAfterUndo();
    }

    CAN_NOT_HAPPEN();
} // RecordBase::GetAfterUndo

Posn RecordBase::GetBeforeRedo() const
{
    if (BeginRecord* p = Encode()->DynamicCast<BeginRecord>())
    {
        return p->GetBeforeRedo();
    }

    if (DeleteRecord* p = Encode()->DynamicCast<DeleteRecord>())
    {
        return p->GetBeforeRedo();
    }

    if (EndRecord* p = Encode()->DynamicCast<EndRecord>())
    {
        return p->GetBeforeRedo();
    }

    if (InsertRecord* p = Encode()->DynamicCast<InsertRecord>())
    {
        return p->GetBeforeRedo();
    }

    CAN_NOT_HAPPEN();
} // RecordBase::GetBeforeUndo

Posn RecordBase::GetBeforeUndo() const
{
    if (BeginRecord* p = Encode()->DynamicCast<BeginRecord>())
    {
        return p->GetBeforeUndo();
    }

    if (DeleteRecord* p = Encode()->DynamicCast<DeleteRecord>())
    {
        return p->GetBeforeUndo();
    }

    if (EndRecord* p = Encode()->DynamicCast<EndRecord>())
    {
        return p->GetBeforeUndo();
    }

    if (InsertRecord* p = Encode()->DynamicCast<InsertRecord>())
    {
        return p->GetBeforeUndo();
    }

    CAN_NOT_HAPPEN();
} // RecordBase::GetBeforeUndo

void RecordBase::Redo(BufferUndo* pBuffer)
{
    if (BeginRecord* p = Encode()->DynamicCast<BeginRecord>())
    {
        p->Redo(pBuffer);
        return;
    }

    if (DeleteRecord* p = Encode()->DynamicCast<DeleteRecord>())
    {
        p->Redo(pBuffer);
        return;
    }

    if (EndRecord* p = Encode()->DynamicCast<EndRecord>())
    {
        p->Redo(pBuffer);
        return;
    }

    if (InsertRecord* p = Encode()->DynamicCast<InsertRecord>())
    {
        p->Redo(pBuffer);
        return;
    }

    CAN_NOT_HAPPEN();
} // RecordBase::Undo

void RecordBase::Undo(BufferUndo* pBuffer)
{
    if (BeginRecord* p = Encode()->DynamicCast<BeginRecord>())
    {
        p->Undo(pBuffer);
        return;
    }

    if (DeleteRecord* p = Encode()->DynamicCast<DeleteRecord>())
    {
        p->Undo(pBuffer);
        return;
    }

    if (EndRecord* p = Encode()->DynamicCast<EndRecord>())
    {
        p->Undo(pBuffer);
        return;
    }

    if (InsertRecord* p = Encode()->DynamicCast<InsertRecord>())
    {
        p->Undo(pBuffer);
        return;
    }

    CAN_NOT_HAPPEN();
} // RecordBase::Undo

} // Private

using namespace Private;

// ctor
BufferUndo::BufferUndo()
{
    m_undo_state = Qlog;
    m_undo       = nil;
    m_redo       = nil;
    m_first_undo = nil;
    m_last_undo  = nil;
} // BufferUndo::BufferUndo

// [C]
bool BufferUndo::CanRedo() const
{
    return m_undo != m_redo;
} // BufferUndo::CanRedo

bool BufferUndo::CanUndo() const
{
    if (Qlog  == m_undo_state) return nil != m_last_undo;
    if (Qredo == m_undo_state) return t;
    if (Qundo == m_undo_state) return nil != m_undo;
    return false;
} // BufferUndo::CanUndo

bool BufferUndo::checkPoint()
{
    #if DEBUG_UNDO
        DEBUG_FORMAT("s=~S u=~S r=~S~%", m_undo_state, m_undo, m_redo);
    #endif

    if (nil == m_undo_state)
    {
        return false;
    }

    if (Qundo == m_undo_state)
    {
        if (g_fTruncate)
        {
            truncateLog();
        }
    }
    else if (Qredo == m_undo_state)
    {
        if (g_fTruncate)
        {
            truncateLog();
        }
        else
        {
            // Truncate executed redo records
            //  Discaard m_pRedo->m_pNext ... m_pLast
            if (nil != m_redo)
            {
                Val runner = m_redo->StaticCast<UndoRecord>()->m_next;
                while (nil != runner)
                {
                    #if DEBUG_UNDO
                        DEBUG_FORMAT("discard ~S~%", runner);
                    #endif

                    Val next = runner->StaticCast<UndoRecord>()->m_next;
                    To<UndoRecords>()->Delete(runner);
                    runner = next;
                } // while
                m_redo->StaticCast<UndoRecord>()->m_next = nil;
                m_last_undo = m_redo;
            } // if
        } // if
    } // if

    m_redo       = nil;
    m_undo_state = Qlog;

    return true;
} // BufferUndo::checkPoint

// [E]
void BufferUndo::emptyLog()
{
    m_first_undo = nil;
    m_last_undo  = nil;

    m_redo = nil;
    m_undo = nil;

    if (nil != m_undo_state)
    {
        m_undo_state = Qlog;
    }
} // BufferUndo::emptyLog

// [R]
void BufferUndo::LogBegin(Val name, Val posn)
{
    UndoRecords* pRecords = To<UndoRecords>();

    //  If the last record is generated by same operation, we merge
    //  the last record and new record.
    if (Qlog == m_undo_state)
    {
        if (EndRecord* p = m_last_undo->DynamicCast<EndRecord>())
        {
            if (p->CanMerge(name, posn))
            {
                #if DEBUG_UNDO
                    DEBUG_FORMAT("Merge ~S~%", name);
                #endif

                pRecords->Delete(p->Encode());
                return;
            }
        } // if
    } // if

    BeginRecord* pRecord = new BeginRecord(name);
    Val record = pRecord->Encode();
    pRecords->Append(record);

    #if DEBUG_UNDO
        DEBUG_FORMAT("~S ~S~%", record, name);
    #endif
} // BufferUndo::LogBegin

void BufferUndo::LogDelete(Posn start, Posn end)
{
    Count length = xsub(end, start);
    if (xle(length, zero)) return;

    if (IsUndoDisabled()) return;

    // FIXME 2007-12-24 yosi@msn.com Merge delete records
    DeleteRecord* pRecord = new DeleteRecord(this, start, end);
    Val record = pRecord->Encode();
    To<UndoRecords>()->Append(record);

    #if DEBUG_UNDO
        DEBUG_FORMAT("~S [~D,~D]~%", record, start, end);
    #endif
} // BufferUndo::RecoredDelete

void BufferUndo::LogEnd(Val name)
{
    if (IsUndoDisabled()) return;

    UndoRecords* pRecords = To<UndoRecords>();
    if (BeginRecord* p = m_last_undo->DynamicCast<BeginRecord>())
    {
        #if DEBUG_UNDO
            DEBUG_PRINTF("Ignore empty record ~S~%", p->m_name);
        #endif
        pRecords->Delete(p->Encode());
        return;
    }

    EndRecord* pRecord = new EndRecord(name);
    Val record = pRecord->Encode();
    pRecords->Append(record);

    #if DEBUG_UNDO
        DEBUG_FORMAT("~S ~S~%", record, name);
    #endif
} // BufferUndo::LogEnd

void BufferUndo::LogInsert(Posn start, Posn end)
{
    Count length = xsub(end, start);
    if (xle(length, zero)) return;

    if (IsUndoDisabled()) return;

    // FIXME 2007-12-24 yosi@msn.com Merge insert records
    InsertRecord* pRecord = new InsertRecord(start, end);
    Val record = pRecord->Encode();
    To<UndoRecords>()->Append(record);

    #if DEBUG_UNDO
        DEBUG_FORMAT("~S [~D,~D]~%", record, start, end);
    #endif
} // BufferUndo::RecoredInsert

// [R]
Posn BufferUndo::Redo(Posn posn, Count k)
{
    #if DEBUG_UNDO
        DEBUG_FORMAT("p=~D s=~S~%", posn, m_undo_state);
    #endif

    if (IsReadOnly()) return nil;
    if (IsUndoDisabled()) return nil;
    if (! CanRedo()) return nil;

    m_undo_state = Qredo;

    Int iCount = Fixnum::Decode_(k);

    int iDepth = 0;
    for (;;)
    {
        if (0 == iDepth)
        {
            if (iCount <= 0) break;
            iCount -= 1;
        }

        if (nil == m_redo) break;

        if (m_redo == m_undo)
        {
            #if DEBUG_REDO
                DEBUG_PRINTF("We don't have no redo record.\n");
            #endif
            break;
        }

        #if DEBUG_REDO
            DEBUG_FORMAT("d=~D ~S~%", Fixnum::Encode(iDepth), m_redo);
        #endif

        RecordBase* pRedo = m_redo->StaticCast<RecordBase>();

        // Are we at redo point? If not, we don't execute
        if (0 == iCount && 0 == iDepth)
        {
            Posn redo_posn = pRedo->GetBeforeRedo();
            if (posn != redo_posn)
            {
                #if DEBUG_REDO
                    DEBUG_FORMAT("move to redo posn ~D.~%", redo_posn);
                #endif

                posn = redo_posn;
                break;
            }
        } // if

        if (pRedo->Is<BeginRecord>())
        {
            iDepth -= 1;
        }
        else if (pRedo->Is<EndRecord>())
        {
            iDepth += 1;
        }

        pRedo->Redo(this);

        posn = pRedo->GetAfterRedo();

        m_redo = pRedo->m_prev;

        if (nil == m_undo)
        {
            m_undo = m_first_undo;
        }
        else
        {
            m_undo = m_undo->StaticCast<RecordBase>()->m_next;
        }
    } // for

    #if DEBUG_REDO
        DEBUG_FORMAT("s=~S p=~D~%", m_undo_state, posn);
    #endif

    return posn;
} // BufferUndo::Redo

// [T]

//  Discards records from m_pUndo (exclusive) to end of log.
void BufferUndo::truncateLog()
{
    Val runner;
    if (nil == m_undo)
    {
        runner = m_first_undo;
        m_first_undo = nil;
    }
    else
    {
        runner = m_undo->StaticCast<UndoRecord>()->m_next;
        m_undo->StaticCast<UndoRecord>()->m_next = nil;
    } // if

    m_redo = nil;

    while (runner != m_undo)
    {
        if (nil == runner) break;

        #if DEBUG_UNDO
            DEBUG_FORMAT("delete ~S~%", runner);
        #endif

        Val next = runner->StaticCast<UndoRecord>()->m_next;
        To<UndoRecords>()->Delete(runner);
        runner = next;
    } // while

    m_last_undo = m_undo;
} // BufferUndo::truncateLog

// [U]
Posn BufferUndo::Undo(Posn posn, Count k)
{
    #if DEBUG_UNDO
        DEBUG_FORMAT("p=~D s=~S~%", posn, m_undo_state);
    #endif

    if (IsReadOnly()) return nil;
    if (IsUndoDisabled()) return nil;
    if (! CanUndo()) return nil;

    if (Qredo == m_undo_state)
    {
        // Truncate edit log between redo.next to last
        checkPoint();
    }

    if (Qundo != m_undo_state)
    {
        m_undo = m_last_undo;
    }

    Int iCount = Fixnum::Decode_(k);

    m_undo_state = Qundo;

    int iDepth = 0;
    for (;;)
    {
        if (0 == iDepth)
        {
            if (iCount <= 0) break;
            iCount -= 1;
        }

        if (nil == m_undo)
        {
            ASSERT(0 == iDepth);
            break;
        }

        #if DEBUG_UNDO
            DEBUG_FORMAT("d=~D p=~D ~S~%", 
                Fixnum::Encode(iDepth), posn, m_undo );
        #endif

        RecordBase* pUndo = m_undo->StaticCast<RecordBase>();

        if (0 == iCount && 0 == iDepth)
        {
            Posn undo_posn = pUndo->GetBeforeUndo();
            if (posn != undo_posn)
            {
                #if DEBUG_REDO
                    DEBUG_FORMAT("move to udno posn ~D.~%", undo_posn);
                #endif

                posn = undo_posn;
                break;
            }
        } // if

        if (pUndo->Is<BeginRecord>())
        {
            iDepth -= 1;
        }
        else if (pUndo->Is<EndRecord>())
        {
            iDepth += 1;
        }

        pUndo->Undo(this);

        posn = pUndo->GetAfterUndo();

        m_undo = pUndo->m_prev;
    } // for

    m_redo = m_last_undo;

    return posn;
} // BufferUndo::Undo

BufferUndo::UndoBlock::~UndoBlock()
{
    m_p->checkPoint();
    m_p->LogEnd(m_name);
} // BufferUndo::UndoBlock::~UndoBlock

void BufferUndo::UndoBlock::init(BufferUndo* p, Val name, Val posn)
{
    m_name = name;
    m_p    = p;

    m_p->checkPoint();
    m_p->LogBegin(name, posn);
} // BufferUndo::UndoBlock::~UndoBlock

// FIXME 2008-01-19 yosi@msn.com We should move print-object for undo_*_record
// to rtl_undo.cpp
defmethod(print_object, undo_begin_record, (Val x, Val s))
{
    BeginRecord* p = x->StaticCast<BeginRecord>();
    format(s, "#<Undo-Begin ~S>", p->m_name);
    return x;
} // print_object

defmethod(print_object, undo_delete_record, (Val x, Val s))
{
    DeleteRecord* p = x->StaticCast<DeleteRecord>();
    format(s, "#<Undo-Delete [~D,~D]>", p->m_start, p->m_end);
    return x;
} // print_object

defmethod(print_object, undo_end_record, (Val x, Val s))
{
    EndRecord* p = x->StaticCast<EndRecord>();
    format(s, "#<Undo-End ~S>", p->m_name);
    return x;
} // print_object

defmethod(print_object, undo_insert_record, (Val x, Val s))
{
    InsertRecord* p = x->StaticCast<InsertRecord>();
    format(s, "#<Undo-Insert [~D,~D]>", p->m_start, p->m_end);
    return x;
} // print_object

} // Editor
