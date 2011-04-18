//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer Core
// editor/ed_buffer_core.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_core.h#6 $
//
#if !defined(INCLUDE_editor_core_h)
#define INCLUDE_editor_core_h

#include "./ed_buffer_text.h"

namespace Editor
{

/// <remark>
///   Core buffer functionality
/// </remark>
class BufferCore : public BufferText
{
    /// <remark>
    ///   List of internval
    /// </remark>
    private: class Intervals_ : public Layout_buffer
    {
        public: Val GetHead() const { return m_first_interval; }
        public: Val GetTail() const { return m_last_interval; }

        public: Val  SetHead(Val x) { return m_first_interval = x; }
        public: Val  SetTail(Val x) { return m_last_interval  = x; }
    }; //Intervals_

    public: typedef DoubleLinkedList_<Intervals_, Interval> Intervals;

    /// <remark>
    ///   List of ranges
    /// </remark>
    private: class Ranges_ : public Layout_buffer
    {
        public: Val GetHead() const { return m_first_range; }
        public: Val GetTail() const { return m_last_range; }

        public: Val  SetHead(Val x) { return m_first_range = x; }
        public: Val  SetTail(Val x) { return m_last_range  = x; }
    }; // Ranges

    public: typedef DoubleLinkedList_<Ranges_, Range> Ranges;

    private: class Trackers_ : public Layout_buffer
    {
        public: Val GetHead() const { return m_first_tracker; }
        public: Val GetTail() const { return m_last_tracker; }

        public: Val  SetHead(Val x) { return m_first_tracker = x; }
        public: Val  SetTail(Val x) { return m_last_tracker  = x; }
    }; // Trackers

    public: typedef DoubleLinkedList_<Trackers_, Tracker> Trackers;

    // ctor
    protected: BufferCore();

    // [D]
    protected: void deleteInterval(Val);

    // [E]
    public: class EnumInterval : public Intervals::Enum
    {
        public: EnumInterval(const BufferCore* p) :
            Intervals::Enum(p->To<const Intervals>()) {}
    }; // EnumInterval

    public: class EnumIntervalReverse : public Intervals::EnumReverse
    {
        public: EnumIntervalReverse(const BufferCore* p) :
            Intervals::EnumReverse(p->To<const Intervals>()) {}
    }; // EnumIntervalReverse

    public: class EnumRange : public Ranges::Enum
    {
        public: EnumRange(const BufferCore* p) :
            Ranges::Enum(p->To<const Ranges>()) {}
    }; // EnumRange

    public: class EnumTracker : public Trackers::Enum
    {
        public: EnumTracker(const BufferCore* p) :
            Trackers::Enum(p->To<const Trackers>()) {}
    }; // EnumTracker

    // [G]
    public: Posn GetEnd() const
        { return m_length; }

    public: Val GetIntervalAt(Posn) const;

    public: Posn GetStart() const
        { return Fixnum::Encode(0); }

    // [I]
    public: void InternalDelete(Posn, Posn);
    public: void InternalInsert(Posn, char16, size_t);
    public: void InternalInsert(Posn, const char16*, size_t);

    // [R]
    protected: void relocate(Posn, Val);

    // [S]
    protected: void setStyle(Posn, Posn, Val);

    // [T]
    private: Interval* tryMergeInterval(Interval*);
}; // BufferCore

CASSERT(sizeof(BufferCore) == sizeof(Layout_buffer));

} // Editor

#endif //!defined(INCLUDE_editor_core_h)
