#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Function for Range object
// editor/ed_range.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_range.cpp#6 $
//
//
#include "./rtl_defs.h"

#include "./ed_range.h"
#include "./ed_buffer.h"

namespace Editor
{

// [I]

/// <summary>
///   Check specified position in range.
/// </summary>
/// <param name="posn">A position</param>
/// <param name="range">A range object</param>
/// <returns>True if position in a range.</returns>
defpred(in_range, (Val range, Val posn))
{
    check_type(range, range);
    Range* pRange = range->StaticCast<Range>();
    return pRange->GetBuffer()->IsValidPosn(posn);
} // in_range

// [M]

/// <summary>
///   Make range object of whole buffer.
/// </summary>
/// <param name="buffer">A buffer object</param>
/// <returns>A range object</returns>
Val make_range(Val thing)
{
    if (Range* pRange = thing->DynamicCast<Range>())
    {
        Range* pNewRange = new Range(
            pRange->m_buffer,
            pRange->m_start,
            pRange->m_end );
        return pNewRange->Encode();
    }
    else if (Buffer* pBuffer = thing->DynamicCast<Buffer>())
    {
        Range* pRange = new Range(thing, zero, pBuffer->m_length);
        return pRange->Encode();
    }

    SignalTypeError(thing, list(Qor, Qbuffer, Qrange));
} // make_range

/// <summary>
///   Make range object of at specified position of buffer.
/// </summary>
/// <param name="buffer">A buffer object</param>
/// <param name="posn">A position in buffer</param>
/// <returns>A range object</returns>
Val make_range(Val buffer, Val posn)
{
    Range* pRange = new Range(buffer, posn, posn);
    return pRange->Encode();
} // make_range

/// <summary>
///   Make range object of at specified range of buffer.
/// </summary>
/// <param name="buffer">A buffer object</param>
/// <param name="end">A end position in buffer</param>
/// <param name="start">A start position in buffer</param>
/// <returns>A range object</returns>
Val make_range(Val buffer, Val start, Val end)
{
    Range* pRange = new Range(buffer, start, end);
    return pRange->Encode();
} // make_range

// [P]
/// <summary>
///   Print a range object on stream.
/// </summary>
/// <param name="s">A output-stream object</param>
/// <param name="x">A range object</param>
defmethod(print_object, range, (Val x, Val s))
{
    Range* p = x->StaticCast<Range>();

    if (nil == p->m_buffer)
    {
        format(s, "#<~S ~X~X>",
            class_name(class_of(x)),
            Fixnum::Encode(x->ToInt() >> 4),
            Fixnum::Encode(x->ToInt() & 15) );
    }
    else if (p->GetStart() == p->GetEnd())
    {
        format(s, "#<~S ~D at ~S>",
            class_name(class_of(x)),
            p->GetStart(),
            p->GetBuffer()->m_name );
    }
    else
    {
        format(s, "#<~S [~D,~D] of ~S>",
            class_name(class_of(x)),
            p->GetStart(),
            p->GetEnd(),
            p->GetBuffer()->m_name );
    } // if
    return x;
} // print_object

// [R]
/// <summary>
///  Extract string from range.
/// </summary>
/// <param name="range">A range object</param>
/// <returns>A string object</returns>
defmethod(range_string, range, (Val range))
{
    Range* pRange = range->StaticCast<Range>();
    return pRange->GetText();
} // range_string

/// <summary>
///   Realize range object.
/// </summary>
defmethod(realize_instance, range, (Thread* pth))
{
    Val range = pth->mv_value[0];
    Range* pRange = range->StaticCast<Range>();
    Buffer* pBuffer = pRange->GetBuffer();
    pBuffer->CheckRange(pRange->m_start, pRange->m_end);
    pBuffer->To<Buffer::Ranges>()->Append(range);
    return range;
} // realize_instance

defmethod(unrealize_instance, range, (Val range))
{
    Range* pRange = range->StaticCast<Range>();
    pRange->GetBuffer()->To<Buffer::Ranges>()->Delete(range);
    pRange->m_buffer = nil;
    return range;
} // unrealize_instance

} // Editor
