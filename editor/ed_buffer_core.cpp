#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Misc
// editor/ed_rtl_misc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_core.cpp#5 $
//
//
#include "./ed_buffer_core.h"

#include "./ed_interval.h"
#include "./ed_range.h"
#include "./ed_tracker.h"

namespace Editor
{

// ctor
BufferCore::BufferCore()
{
    m_first_interval = nil;
    m_first_range    = nil;
    m_first_tracker  = nil;

    m_last_interval  = nil;
    m_last_range     = nil;
    m_last_tracker   = nil;

    m_tick           = zero;

    Val intv = (new Interval(Encode(), zero, one))->Encode();
    To<Intervals>()->Append(intv);
    m_interval_root = intv;
} // BufferCore::BufferCore

// [I]
void BufferCore::InternalDelete(Posn start, Posn end)
{
    deleteChars(start, end);
    relocate(start, xsub(start, end));
    m_tick  = xadd(m_tick, 1);
} // BufferCore::InternalDelete

void BufferCore::InternalInsert(Posn posn, char16 wch, size_t cwch)
{
    insert(posn, wch, cwch);
    relocate(posn, Fixnum::Encode(cwch));
    m_tick  = xadd(m_tick, 1);
} // BufferCore::InternalInsert

void BufferCore::InternalInsert(Posn posn, const char16* pwch, size_t cwch)
{
    insert(posn, pwch, cwch);
    relocate(posn, Fixnum::Encode(cwch));
    m_tick  = xadd(m_tick, 1);
} // BufferCore::InternalInsert

// [R]
void BufferCore::relocate(Posn posn, Val delta)
{
    // Relocate ranges
    {
        Posn end1 = xadd(m_length, 1);
        foreach (EnumRange, oEnum, this)
        {
            Val range = oEnum.Get();
            Range* pRange = range->StaticCast<Range>();
            Posn start = pRange->m_start;
            Posn end   = pRange->m_end;

            if (xgt(start, posn))
            {
                start = xadd(start, delta);
                if (xlt(start, posn))
                {
                    start = posn;
                }
                else if (xgt(start, end1))
                {
                    start = end1;
                }
                pRange->m_start = start;
            } // if

            if (xgt(end, posn))
            {
                end = xadd(end, delta);
                if (xlt(end, posn))
                {
                    end = posn;
                }
                else if (xgt(end, end1))
                {
                    end = end1;
                }
                pRange->m_end = end;
            } // if

            ASSERT(xle(start, end));
        } // for each range
    }

    Posn end1 = xadd(m_length, 1);

    // Remove empty intervals
    if (xlt(delta, 0))
    {
        EnumIntervalReverse oEnum(this);
        while (! oEnum.AtEnd())
        {
            Val intv = oEnum.Get();
            Interval* pIntv = intv->StaticCast<Interval>();
            oEnum.Next();

            Posn start = pIntv->m_start;
            Posn end   = pIntv->m_end;

            if (xlt(end, posn))
            {
                break;
            }

            if (xgt(start, posn))
            {
                start = xadd(start, delta);
                if (xlt(start, posn))
                {
                    start = posn;
                }
                else if (xgt(start, end1))
                {
                    start = end1;
                }
            } // if

            if (xgt(end, posn))
            {
                end = xadd(end, delta);
                if (xlt(end, posn))
                {
                    end = posn;
                }
                else if (xgt(end, end1))
                {
                    end = end1;
                }
            } // if

            if (start == end)
            {
                deleteInterval(intv);
            }
            else
            {
                ASSERT(xlt(start, end));
            }
        } // while
    } // if

    // Relocate intervals
    {
        foreach (EnumInterval, oEnum, this)
        {
            Val intv = oEnum.Get();
            Interval* pIntv = intv->StaticCast<Interval>();

            Posn start = pIntv->m_start;
            Posn end   = pIntv->m_end;

            if (xgt(start, posn))
            {
                start = xadd(start, delta);
                if (xlt(start, posn))
                {
                    start = posn;
                }
                else if (xgt(start, end1))
                {
                    start = end1;
                }
                pIntv->m_start = start;
            } // if

            if (xgt(end, posn))
            {
                end = xadd(end, delta);
                if (xlt(end, posn))
                {
                    end = posn;
                }
                else if (xgt(end, end1))
                {
                    end = end1;
                }
                pIntv->m_end   = end;
            } // if

            ASSERT(xlt(start, end));
        } // for each interval
    }

    // Update change trackers
    {
        Posn change_start = posn;
        Posn change_end   = posn;

        if (xgt(delta, zero))
        {
            change_end = xxadd(change_end, delta);
        }

        foreach (EnumTracker, oEnum, this)
        {
            Val tracker = oEnum.Get();
            Tracker* pRunner = tracker->StaticCast<Tracker>();

            pRunner->m_start = xmin(pRunner->m_start, change_start);
            pRunner->m_end   = xmax(pRunner->m_end,   change_end);

            pRunner->m_end = xmin(pRunner->m_end, m_length);
        } // for each tracker
    }
} // BufferCore::relocate

} // Editor
