#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime Range
// editor/ed_range.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_interval.cpp#2 $
//
//
#include "./rtl_defs.h"

#include "./ed_interval.h"
#include "./ed_buffer.h"

namespace Editor
{

// [P]
defmethod(print_object, interval, (Val x, Val s))
{
    Interval* p = x->StaticCast<Interval>();

    if (nil == p->m_buffer)
    {
        format(s, "#<Interval ~X~X>",
            Fixnum::Encode(x->ToInt() >> 4),
            Fixnum::Encode(x->ToInt() & 15) );
    }
    else
    {
        format(s, "#<Interval ~S [~D,~D] of ~S>",
            p->m_state,
            p->GetStart(),
            p->GetEnd(),
            p->GetBuffer()->m_name );
    } // if
    return x;
} // print_object

} // Editor