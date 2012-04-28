#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Buffer
// editor/rtl_buffer.cpp
//
// This file is part of Evita.
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_buffer.cpp#12 $
//
#include "./rtl_defs.h"

#include "./ed_buffer.h"
#include "./ed_range.h"
#include "./ed_text_window.h"

#include "./peer/peer_file_dialog.h"

#include "../tinycl/rtl/tinycl_regex.h"

namespace Editor
{

namespace Private
{

/// <remark>
///   Regex match context against buffer
/// </remark>
// FIXME 2008-07-12 yosi@msn.com We should implement Regex::IEnvironment.
class BufferMatchContext :
    public TinyCl::MatchContext
{
    private: Val m_range;

    /// <summary>
    ///  Construct <c>BufferMatchContext</c> for <c>regex</c> on
    ///  <c>range</c>.
    /// </summary>
    /// <param name="range">Range object</param>
    /// <param name="regex">Regex object</param>
    public: BufferMatchContext(
        Val const regex,
        Val const range) :
            MatchContext(regex),
            m_range(range)
    {
        check_type(range, range);
    } // BufferMatchContext

    // [B]
    private: virtual bool BackwardFindCharCi(
        char16          wchFind,
        Regex::Posn*    inout_lPosn,
        Regex::Posn     lStop ) const override
    {
        ASSERT(lStop <= *inout_lPosn);

        Buffer::EnumCharRev::Arg oArg(
            m_range->StaticCast<Range>()->GetBuffer(),
            lStop,
            *inout_lPosn );

        foreach (Buffer::EnumCharRev, oEnum, oArg)
        {
            if (charEqCi(oEnum.Get(), wchFind))
            {
                *inout_lPosn = Fixnum::Decode_(oEnum.GetPosn());
                return true;
            }
        } // for
        return false;
    } // BackwardFindCharCi

    private: virtual bool BackwardFindCharCs(
        char16          wchFind,
        Regex::Posn*    inout_lPosn,
        Regex::Posn     lStop ) const override
    {
        ASSERT(lStop <= *inout_lPosn);

        Buffer::EnumCharRev::Arg oArg(
            m_range->StaticCast<Range>()->GetBuffer(),
            lStop,
            *inout_lPosn );

        foreach (Buffer::EnumCharRev, oEnum, oArg)
        {
            if (charEqCs(oEnum.Get(), wchFind))
            {
                *inout_lPosn = Fixnum::Decode_(oEnum.GetPosn());
                return true;
            }
        } // while
        return false;
    } // BackwardFindCharCi

    // [F]
    private: virtual bool ForwardFindCharCi(
        char16          wchFind,
        Regex::Posn*    inout_lPosn,
        Regex::Posn     lStop ) const override
    {
        Buffer::EnumChar::Arg oArg(
            m_range->StaticCast<Range>()->GetBuffer(),
            *inout_lPosn,
            lStop );

        foreach (Buffer::EnumChar, oEnum, oArg)
        {
            if (charEqCi(oEnum.Get(), wchFind))
            {
                *inout_lPosn = Fixnum::Decode_(oEnum.GetPosn());
                return true;
            }
        } // while
        return false;
    } // ForwardFindCharCi

    private: virtual bool ForwardFindCharCs(
        char16          wchFind,
        Regex::Posn*    inout_lPosn,
        Regex::Posn     lStop ) const override
    {
        Buffer::EnumChar::Arg oArg(
            m_range->StaticCast<Range>()->GetBuffer(),
            *inout_lPosn,
            lStop );

        foreach (Buffer::EnumChar, oEnum, oArg)
        {
            if (charEqCs(oEnum.Get(), wchFind))
            {
                *inout_lPosn = Fixnum::Decode_(oEnum.GetPosn());
                return true;
            }
        } // while
        return false;
    } // ForwardFindCharCs

    // [G]
    /// <summary>
    ///  Get a character at speicifed position.
    /// </summary>
    /// <param name="lPosn">A position to get character.</param>
    private: virtual char16 GetChar(Regex::Posn lPosn) const override
    {
        return m_range->StaticCast<Range>()->GetBuffer()->GetCharAt(
            Fixnum::Encode(lPosn) );
    } // GetChar

    /// <summary>
    ///  Get end position of scanning range.
    /// </summary>
    private: virtual Regex::Posn GetEnd() const override
    {
        return Fixnum::Decode_(m_range->StaticCast<Range>()->GetEnd());
    } // GetEnd

    /// <summary>
    ///  Get scanning information.
    /// </summary>
    private: virtual void GetInfo(Regex::SourceInfo* p) const override
    {
        Range* pRange = m_range->StaticCast<Range>();
        p->m_lStart = 0;
        p->m_lEnd   = Fixnum::Decode_(pRange->GetBuffer()->m_length);

        p->m_lScanStart = Fixnum::Decode_(pRange->GetStart());
        p->m_lScanEnd   = Fixnum::Decode_(pRange->GetEnd());
    } // GetInfo

    /// <summary>
    ///  Get start position of scanning range.
    /// </summary>
    private: Regex::Posn GetStart() const
    {
        return Fixnum::Decode_(m_range->StaticCast<Range>()->GetStart());
    } // GetEnd

    // [S]
    private: virtual void SetCapture(
        int         nCapture,
        Regex::Posn lStart,
        Regex::Posn lEnd ) override
    {
        Val range = svref(m_captures, Fixnum::Encode(nCapture));
        if (nil == range)
        {
            range = make_range(
                m_range->StaticCast<Range>()->m_buffer,
                Fixnum::Encode(lStart),
                Fixnum::Encode(lEnd) );
            setf_svref(range, m_captures, Fixnum::Encode(nCapture));
        }
        else
        {
            Range* pRange = range->StaticCast<Range>();
            pRange->m_end    = Fixnum::Encode(lEnd);
            pRange->m_start  = Fixnum::Encode(lStart);
        }
    } // SetCapture

    private: virtual bool StringEqCi(
        const char16*   pwch,
        int             cwch,
        Regex::Posn     lPosn ) const override
    {
        Buffer::EnumChar oEnumBufferChar(
            Buffer::EnumChar::Arg(
                m_range->StaticCast<Range>()->GetBuffer(),
                Fixnum::Encode(lPosn),
                m_range->StaticCast<Range>()->GetEnd() ) );

        foreach (EnumChar, oEnum, EnumChar::Arg(pwch, cwch))
        {
            if (oEnumBufferChar.AtEnd())
            {
                return false;
            }

            if (! charEqCi(oEnum.Get(), oEnumBufferChar.Get()))
            {
                return false;
            }

            oEnumBufferChar.Next();
        } // for

        return true;
    } // StringEqCi

    private: virtual bool StringEqCs(
        const char16*   pwch,
        int             cwch,
        Regex::Posn     lPosn ) const override
    {
        Buffer::EnumChar oEnumBufferChar(
            Buffer::EnumChar::Arg(
                m_range->StaticCast<Range>()->GetBuffer(),
                Fixnum::Encode(lPosn),
                m_range->StaticCast<Range>()->GetEnd() ) );

        foreach (EnumChar, oEnum, EnumChar::Arg(pwch, cwch))
        {
            if (oEnumBufferChar.AtEnd())
            {
                return false;
            }

            if (! charEqCs(oEnum.Get(), oEnumBufferChar.Get()))
            {
                return false;
            }

            oEnumBufferChar.Next();
        } // for

        return true;
    } // StringEqCi
}; // BufferMatchContext

} // Private

using namespace Private;
using namespace TinyCl;

// [A]
#if 0
defun(allocate_buffer, (Val name))
{
    Buffer* pBuffer = new Buffer(name);
    return pBuffer->Encode();
} // allocate_buffer
#endif

// [B]

defun(buffer_match, (Val regex, Val range))
{
    check_type(regex, regex);

    BufferMatchContext oContext(regex, range);

    RegexObj* pRegex = regex->StaticCast<RegexObj>();

    IRegex* pIRegex = reinterpret_cast<IRegex*>(
        pRegex->m_blob->StaticCast<U32Vec>()->GetStart() );

    bool fMatched = Regex::StartMatch(pIRegex, &oContext);

    RegexMatch*  pMatch = new RegexMatch;
    pMatch->m_range = range;
    pMatch->m_regex = regex;

    oContext.SetMatch(pMatch, fMatched);

    return pMatch->Encode();
} // buffer_match

// [C]
defpred(can_close_p, (Val buffer))
{
    check_type(buffer, buffer);

    Buffer* pBuffer = buffer->StaticCast<Buffer>();
    if (! pBuffer->IsModified()) return true;
    if (Knone == pBuffer->m_pathname) return true;

    Val msg = format(nil, "Do you want to save the changes to ~A?",
        pBuffer->m_name );

    // FIXME 2008-01-26 yosi@msn.com We should use portable message box
    // instead of Windows API.
    int iAnswer = ::MessageBoxW(
        NULL,
        msg->StaticCast<SimpleString>()->GetStart(),
        L"Evita",
        MB_ICONWARNING | MB_YESNOCANCEL );

    switch (iAnswer)
    {
    case IDCANCEL:
        return false;

    case IDYES:
    {
        // Get filename for save
        Peer::FileDialog::Param oParam;

        if (0 == pBuffer->GetFileName()[0])
        {
            ::lstrcpyW(
                oParam.m_wsz, 
                pBuffer->m_name->StaticCast<SimpleString>()->GetStart() );

            Peer::FileDialog oFileDialog;
            if (! oFileDialog.GetSaveFileName(&oParam))
            {
                return false;
            }
        } // if

            pBuffer->Save(oParam.m_wsz);
            break;
        } // yes
    } // switch iAnswer

    return true;
} // can_close_p

defun(close_buffer, (Val buffer))
{
    check_type(buffer, buffer);

    VAR(AbuffersA) = delq(buffer, VAR(AbuffersA));

    foreach (Buffer::EnumWindow, oEnum, buffer->StaticCast<Buffer>())
    {
        close_window(oEnum.Get());
    } // for each window

    return buffer;
} // close_buffer

// [F]
defun(find_buffer, (Val name))
{
    foreach (List::Enum, oEnum, VAR(AbuffersA))
    {
        Val buffer = oEnum.Get();
        Buffer* pBuffer = buffer->StaticCast<Buffer>();
        if (Si::string_eq(pBuffer->m_name, name))
        {
            return buffer;
        }
    } // for each buffer
    return nil;
} // find_buffer

// [M]
defun(make_buffer, (Val name))
{
    check_type(name, string);
    Buffer* pBuffer = new Buffer(name);
    Val buffer = pBuffer->Encode();
    push(buffer, VAR(AbuffersA));
    return buffer;
} // make_buffer

// [N]
defmethod(next_match_using_source, range, (Val range, Val match))
{
    ASSERT(range_p(range));

    RegexMatch* pMatch  = match->StaticCast<RegexMatch>();

    BufferMatchContext oContext(
        pMatch->m_regex,
        pMatch->m_range );

    RegexObj* pRegex = pMatch->m_regex->StaticCast<RegexObj>();

    IRegex* pIRegex = reinterpret_cast<IRegex*>(
        pRegex->m_blob->StaticCast<U32Vec>()->GetStart() );

    bool fMatched = Regex::NextMatch(pIRegex, &oContext);
    oContext.SetMatch(pMatch, fMatched);

    return match;
} // next_match_using_source

// [P]
defmethod(print_object, buffer, (Val x, Val s))
{
    format(s, "#<Buffer ~S>", x->StaticCast<Buffer>()->m_name);
    return x;
} // print_object

// [R]

defmethod(realize_instance, buffer, (Thread* pth))
{
    Val buffer = pth->mv_value[0];
    Buffer* p = buffer->StaticCast<Buffer>();
    new(buffer) Buffer(p->m_name);
    return buffer;
} // realize_instance

/// <summary>
///  Rename buffer
/// </summary>
/// <param name="buffer">Buffer object to rename</param>
/// <param name="name">String for new buffer name</param>
/// <returns>Buffer obejct</returns>
defun(rename_buffer, (Val buffer, Val name))
{
    check_type(buffer, buffer);
    check_type(name, string);

    Val present = find_buffer(name);
    if (nil != present)
    {
        Val prefix = name;
        Val counter = one;
        for (;;)
        {
            counter = xadd(counter, one);
            name = format(nil, "(~D)~A", counter, prefix);
            if (nil == find_buffer(name))
            {
                break;
            }
        } // for
    } // if

    buffer->StaticCast<Buffer>()->m_name = name;
    return buffer;
} // rename_buffer

/// <summary>
///  Start laoding file into buffer.
/// </summary>
defun(start_load_file, (Val buffer, Val thing, Val external_format))
{
    check_type(buffer, buffer);

    Val namestr = namestring(thing);

    buffer->StaticCast<Buffer>()->Load(
        namestr->StaticCast<SimpleString>()->GetStart(),
        external_format );

    return buffer;
} // start_load_file

} // Editor
