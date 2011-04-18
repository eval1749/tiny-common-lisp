#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer - Tracker
// editor/ed_tracker.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_tracker.cpp#1 $
//
//
#include "./ed_tracker.h"

#include "./ed_buffer.h"

namespace Editor
{

// ctor
Tracker::Tracker(Val buffer)
{
    m_buffer = buffer;

    // FIXME 2008-02-03 yosi@msn.com Should we use weak-pointer for
    // tracker list?
    Buffer* pBuffer = buffer->StaticCast<Buffer>();
    pBuffer->To<Buffer::Trackers>()->Append(Encode());
} // Tracker::Tracker

// [G]
Buffer* Tracker::GetBuffer() const
{
    return m_buffer->StaticCast<Buffer>();
} // Tracker::GetBuffer

// {R]
void Tracker::Reset()
{
    m_start = Fixnum::Encode(Arch::ArrayDimensionLimit);
    m_end   = zero;
} // Tracker::Reset

} // Editor
