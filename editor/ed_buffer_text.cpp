#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer - Text
// editor/ed_buffer_text.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_text.cpp#1 $
//
//
#define DEBUG_MEMORY 0
#include "./ed_buffer_text.h"

namespace Editor
{

// ctor
BufferText::BufferText()
{
    HANDLE hHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
    if (NULL == hHeap) PlatformError("HeapCreate");

    m_next = nil;
    m_prev = nil;

    const size_t cwch = MinGapLength * 3;

    m_heap = Fixnum::Encode(hHeap);

    void* pv = ::HeapAlloc(hHeap, 0, sizeof(char16) * cwch);
    if (NULL == pv) PlatformError("HeapAlloc");

    #if DEBUG_MEMORY
        DEBUG_PRINTF("heap=%p pv=%p+%d\n", m_heap, pv, cwch);
    #endif

    m_blob = Fixnum::Encode(pv);
    m_gap_end   = Fixnum::Encode(cwch);
    m_gap_start = zero;
    m_length    = zero;
    m_size      = Fixnum::Encode(cwch);
} // BufferText::BufferText

// [C]
void BufferText::CheckPosn(Posn posn) const
{
    if (IsValidPosn(posn)) return;
    error(Qinvalid_position,
        Kbuffer,        Encode(),
        Kdatum,         posn,
        Kexpected_type, list(Qinteger, zero, m_length) );
} // BufferText::CheckPosn

void BufferText::CheckRange(Posn start, Posn end) const
{
    CheckPosn(start);
    CheckPosn(end);
    if (xle(start, end)) return;

    error(Qinvalid_range,
        Kbuffer,        Encode(),
        Kdatum,         end,
        Kexpected_type, list(Qinteger, start, m_length) );
} // BufferText::CheckRange

// [D]
Count BufferText::deleteChars(Posn start, Posn end)
{
    CheckRange(start, end);
    moveGap(start);
    Val n = xsub(end, start);
    m_gap_end = xadd(m_gap_end, n);
    m_length  = xsub(m_length, n);
    return n;
} // BufferText::deleteChars

// [E]
void BufferText::extend(Posn posn, Count extent)
{
    CheckPosn(posn);

    if (xle(extent, 0)) return;

    moveGap(posn);

    if (xge(xsub(m_gap_end, m_gap_start), xadd(extent, MinGapLength))) {
        // We have enough gap
        return;
    }

    Int nExtension = Fixnum::Decode_(extent) + ExtensionLength - 1;
    nExtension = RoundUp(nExtension, ExtensionLength);

    m_size = xadd(m_size, nExtension);

    Int cwch = Fixnum::Decode_(m_size);
    void* pv =  ::HeapReAlloc(
        reinterpret_cast<HANDLE>(m_heap),
        0,
        m_blob->To<char16>(),
        sizeof(char16) * cwch );

    if (NULL == pv) PlatformError("HeapReAlloc");

    #if DEBUG_MEMORY
        DEBUG_PRINTF("heap=%p pv=%p+%d\n", m_heap, pv, cwch);
    #endif

    m_blob = Fixnum::Encode(pv);

    // Extent Gap
    Int nGapEnd = Fixnum::Decode_(m_gap_end);

    ::MoveMemory(
        m_blob->To<char16>() + nGapEnd + nExtension,
        m_blob->To<char16>() + nGapEnd,
        sizeof(char16) * Fixnum::Decode_(xsub(m_length, m_gap_start)) );

    m_gap_end = xadd(m_gap_end, nExtension);

    #if DEBUG_MEMORY
        DEBUG_PRINTF("posn=%d+%d gap=[%d %d]\n",
            Fixnum::Decode_(posn),
            Fixnum::Decode_(extent),
            Fixnum::Decode_(m_gap_start),
            Fixnum::Decode_(m_gap_end) );
    #endif
} // BufferText::extend

// [G]
char16 BufferText::GetCharAt(Posn posn) const
{
    CheckPosn(posn);

    if (xge(posn, m_gap_start))
    {
        posn = xadd(posn, xsub(m_gap_end, m_gap_start));
    }

    char16* pwch = m_blob->To<char16>();
    return pwch[Fixnum::Decode_(posn)];
} // BufferText::GetCharAt

uint BufferText::GetText(Posn start, char16* prgwch, uint cwch) const
{
    CheckPosn(start);

    Posn end = xadd(start, cwch);
    if (xgt(end, m_length)) end = m_length;

    Int lStart = Fixnum::Decode_(start);
    Int lEnd   = Fixnum::Decode_(end);
    Int lGapStart = Fixnum::Decode_(m_gap_start);
    Int lGapEnd   = Fixnum::Decode_(m_gap_end);

    char16* pwch = m_blob->To<char16>();

    if (lStart >= lGapStart)
    {
        // We extract text after gap.
        // gggggg<....>
        ::CopyMemory(
            prgwch,
            pwch + lGapEnd + (lStart - lGapStart),
            sizeof(char16) * (lEnd - lStart) );
    }
    else
    {
        // We extract text before gap.
        // <.....>gggg
        // <...ggg>ggg
        // <...ggg...>
        Int lMiddle = min(lGapStart, lEnd);

        ::CopyMemory(
            prgwch,
            pwch + lStart,
            sizeof(char16) * (lMiddle - lStart) );

        ::CopyMemory(
            prgwch + (lMiddle - lStart),
            pwch + lGapEnd,
            sizeof(char16) * (lEnd - lMiddle) );
    } // if

    return lEnd - lStart;
} // BufferText::GetText

uint BufferText::GetText(Posn start, Posn end, char16* prgwch) const
{
    CheckRange(start, end);

    return GetText(
        start,
        prgwch,
        static_cast<uint>(Fixnum::Decode_(xsub(end, start))) );
} // BufferText::GetText

// [I]
void BufferText::insert(Posn posn, char16 wch, size_t cwch)
{
    if (0 == cwch) return;

    Val n = Fixnum::Encode(cwch);
    extend(posn, n);
    {
        char16* pwchStart = m_blob->To<char16>() + Fixnum::Decode_(posn);
        char16* pwchEnd   = pwchStart + cwch;
        for (char16* pwch = pwchStart; pwch < pwchEnd; pwch++)
        {
            *pwch = wch;
        } // for pwch
    }
    m_gap_start = xadd(m_gap_start, n);
    m_length    = xadd(m_length, n);
} // BufferText::insert

void BufferText::insert(Posn posn, const char16* pwch, size_t cwch)
{
    if (0 == cwch) return;

    Val n = Fixnum::Encode(cwch);
    extend(posn, n);
    ::CopyMemory(
        m_blob->To<char16>() + Fixnum::Decode_(posn),
        pwch,
        sizeof(char16) * cwch );
    m_gap_start = xadd(m_gap_start, n);
    m_length    = xadd(m_length, n);
} // BufferText::insert

bool BufferText::IsValidPosn(Posn posn) const
{
    if (fixnump(posn))
    {
        return xge(posn, zero) && xle(posn, m_length);
    }

    error(Qtype_error,
        Kdatum, posn,
        Kexpected_type, Qsequence_index );
} // BufferText::IsValidPosn

//////////////////////////////////////////////////////////////////////
//
// Move Gap To Specified Position
//
// Called by:
//  CBufferText::delteChars
//  CBufer::insertString
//
// User 1 2 3 4 5 6 7 8           9 A B
//       M i n n e a p o _ _ _ _ _ l i s 
// Gap  1 2 3 4 5 6 7 8 9 A B C D E F 10
//              Posn     Gap
void BufferText::moveGap(Posn new_start)
{
    Posn cur_end   = m_gap_end;
    Posn cur_start = m_gap_start;

    Count diff = xsub(m_gap_start, new_start);

    Posn new_end = xsub(m_gap_end, diff);

    m_gap_end   = new_end;
    m_gap_start = new_start;

    char16* pwch = m_blob->To<char16>();

    if (xgt(diff, 0))
    {
        ////////////////////////////////////////////////////////////
        //
        // Move GAP backward =
        //  Move GAP between new_start and cur_start before cur_end.
        // abcdef....ghijk
        //    ^  s   e
        // abc....defghijk
        //    s   e
        ::MoveMemory(
            pwch + Fixnum::Decode_(new_end),
            pwch + Fixnum::Decode_(new_start),
            sizeof(char16) * Fixnum::Decode_(diff) );
    }
    else if (xlt(diff, 0))
    {
        ////////////////////////////////////////////////////////////
        //
        // Move GAP forward =
        //  Move string between cur_end and m_gap_end after cur_start.
        // abcde...fghijk
        //      s  e   ^
        //         |   |
        //      +--+   |
        //      |      |
        //      |   +--+
        //      V   V
        // abcdefghi...jk
        //          s  e
        ::MoveMemory(
            pwch + Fixnum::Decode_(cur_start),
            pwch + Fixnum::Decode_(cur_end),
            sizeof(char16) * -Fixnum::Decode_(diff) );
    } // if
} // BufferText::moveGap

} // Editor
