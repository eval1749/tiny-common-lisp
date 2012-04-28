#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Regex Facility
// tinycl_rtl_regex.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_regex.cpp#3 $
//
#include "./tinycl_regex.h"

namespace TinyCl
{

/// <remark>
///   String Match Context
/// </remark>
class StringMatchContext : public MatchContext
{
    private: Int            m_iOffset;
    private: const char16*  m_pwchStart;
    private: const char16*  m_pwchEnd;

    private: Val m_end;
    private: Val m_match;
    private: Val m_offset;
    private: Val m_regex;
    private: Val m_start;
    private: Val m_string;

    /// <summary>
    ///  Construct StringMatchContext
    /// </summary>
    /// <param name="end">An end position in string</param>
    /// <param name="regex">A regex object</param>
    /// <param name="start">A start position in string</param>
    /// <param name="string">A string object to match</param>
    public: StringMatchContext(Val regex, Val string, Val start, Val end) :
        MatchContext(regex),
        m_end(end),
        m_match(nil),
        m_regex(regex),
        m_start(start),
        m_string(string)
    {
        setup();
    } // StringMatchContext

    // [B]
    private: virtual bool BackwardFindCharCi(
        char16  wchFind,
        Posn*   inout_lPosn,
        Posn    lStop ) const override
    {
        Posn lPosn = *inout_lPosn;
        while (lPosn > lStop)
        {
            lPosn -= 1;
            if (charEqCi(GetChar(lPosn), wchFind))
            {
                *inout_lPosn = lPosn + 1;
                return true;
            }
        } // while
        return false;
    } // BackwardFindCharCi

    private: virtual bool BackwardFindCharCs(
        char16  wchFind,
        Posn*   inout_lPosn,
        Posn    lStop ) const override
    {
        Posn lPosn = *inout_lPosn;
        while (lPosn >= lStop)
        {
            lPosn -= 1;
            if (charEqCs(GetChar(lPosn), wchFind))
            {
                *inout_lPosn = lPosn + 1;
                return true;
            }
        } // while
        return false;
    } // BackwardFindCharCs

    // [F]
    private: virtual bool ForwardFindCharCi(
        char16  wchFind,
        Posn*   inout_lPosn,
        Posn    lStop ) const override
    {
        Posn lPosn = *inout_lPosn;
        while (lPosn < lStop)
        {
            if (charEqCs(GetChar(lPosn), wchFind))
            {
                *inout_lPosn = lPosn;
                return true;
            }
            lPosn += 1;
        } // while
        return false;
    } // ForwardFindCharCs

    private: virtual bool ForwardFindCharCs(
        char16  wchFind,
        Posn*   inout_lPosn,
        Posn    lStop ) const override
    {
        Posn lPosn = *inout_lPosn;
        while (lPosn < lStop)
        {
            if (charEqCs(GetChar(lPosn), wchFind))
            {
                *inout_lPosn = lPosn;
                return true;
            }
            lPosn += 1;
        } // while
        return false;
    } // ForwardFindCharCs

    // [G]
    private: virtual char16 GetChar(Posn lPosn) const override
    {
        lPosn += m_iOffset;
        ASSERT(m_pwchStart + lPosn < m_pwchEnd);
        return m_pwchStart[lPosn];
    } // GetCatpture

    private: virtual Posn GetEnd() const override
    {
        return Fixnum::Decode_(m_end);
    } // GetEnd

    private: virtual void GetInfo(Regex::SourceInfo* p) const override
    {
        p->m_lStart = 0;
        p->m_lEnd   = Fixnum::Decode_(length(m_string));

        p->m_lScanStart = Fixnum::Decode_(m_start);
        p->m_lScanEnd   = Fixnum::Decode_(m_end);
    } // IMatchContext::GetInfo

    private: virtual Posn GetStart() const override
    {
        return Fixnum::Decode_(m_start);
    } // GetStart

    // [S]
    private: virtual void SetCapture(int nCapture, Posn lStart, Posn lEnd) override
    {
        ASSERT(lStart <= lEnd);

        Val range = svref(m_captures, Fixnum::Encode(nCapture));
        if (nil == range)
        {
            range = Thread::Get()->AllocRecord(CLASSD_string_range);
            setf_svref(range, m_captures, Fixnum::Encode(nCapture));
            range->StaticCast<StringRange>()->m_string = m_string;
        }

        StringRange* pRange = range->StaticCast<StringRange>();
        pRange->m_end    = Fixnum::Encode(lEnd);
        pRange->m_start  = Fixnum::Encode(lStart);
    } // SetCapture

    private: void setup()
    {
        Int cwch;
        if (SimpleString* p = m_string->DynamicCast<SimpleString>())
        {
            m_pwchStart = p->GetStart();
            cwch = p->GetLength();
            m_iOffset = 0;
        }
        else if (StringObject* p = m_string->DynamicCast<StringObject>())
        {
            Val ofs;
            Val data = p->GetData(&ofs);
            m_pwchStart = data->StaticCast<SimpleString>()->GetStart();
            m_iOffset = Fixnum::Decode_(ofs);
            cwch = Fixnum::Decode_(p->m_fill_pointer);
        }
        else
        {
            SignalTypeError(m_string, Qstring);
        } // if

        if (nil == m_end)
        {
            m_end = Fixnum::Encode(cwch);
        }

        m_pwchEnd = m_pwchStart + Fixnum::Decode_(m_end);

        // Check bound indexes
        StringData oData(m_string, m_start, m_end);
    } // setup

    private: virtual bool StringEqCi(
        const char16*   pwch,
        int             cwch,
        Posn            lPosn ) const override
    {
        foreach (EnumChar, oEnum, EnumChar::Arg(pwch, cwch))
        {
            if (! charEqCi(oEnum.Get(), GetChar(lPosn)))
            {
                return false;
            }
            lPosn += 1;
        } // for
        return true;
    } // StringEqCi

    private: virtual bool StringEqCs(
        const char16*   pwch,
        int             cwch,
        Posn            lPosn ) const override
    {
        foreach (EnumChar, oEnum, EnumChar::Arg(pwch, cwch))
        {
            if (! charEqCs(oEnum.Get(), GetChar(lPosn)))
            {
                return false;
            }
            lPosn += 1;
        } // for
        return true;
    } // StringEqCi
}; // StringMatchContext

/// <summary>
///  Compile regex string then return regex object.
/// </summary>
/// <param name="pth">Thread object contains parameters</param>
/// <returns>A regex object</returns>
defun(compile_regexV, (Thread* pth))
{
    Val capture         = t;
    Val end             = nil;
    Val extended_syntax = nil;
    Val from_end        = nil;
    Val ignore_case     = nil;
    Val multiple_line   = nil;
    Val single_line     = t;
    Val start           = zero;

    KeyArg rgoKey[] =
    {
        KEYARG(capture),
        KEYARG(end),
        KEYARG(extended_syntax),
        KEYARG(from_end),
        KEYARG(ignore_case),
        KEYARG(multiple_line),
        KEYARG(single_line),
        KEYARG(start),
    }; // rgoKey

    Val string = pth->mv_value[0];

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    uint rgfFlag = 0;

    Val explicit_capture = nil == capture ? t : nil;

    #define SET_FLAG(mp_name, mp_Option) \
        if (nil != mp_name) rgfFlag |= Regex::Option_ ## mp_Option

    SET_FLAG(explicit_capture,  ExplicitCapture);
    SET_FLAG(extended_syntax,   ExtendedSyntax);
    SET_FLAG(from_end,          Backward);
    SET_FLAG(ignore_case,       IgnoreCase);
    SET_FLAG(multiple_line,     Multiline);
    SET_FLAG(single_line,       Singleline);

    class Context : public Regex::ICompileContext
    {
        public:  Val    m_blob;
        public:  Val    m_captures;
        public:  Val    m_error;
        public:  Val    m_posn;

        public: Context() :
            m_blob(nil),
            m_captures(nil),
            m_error(nil),
            m_posn(nil) {}

        private: virtual void* AllocRegex(size_t cb, int cCaptures) override
        {
            m_captures = make_vector(Fixnum::Encode(cCaptures + 1));

            m_blob = Thread::Get()->AllocBinVec(
                CLASSD_unsigned_byte_32_vector,
                Fixnum::Encode(Ceiling(cb, sizeof(uint32))) );

            return m_blob->StaticCast<U32Vec>()->GetStart();
        } // Alloc

        private: virtual bool SetCapture(int iNth, const char16* pwsz) override
        {
            setf_svref(make_string(pwsz), m_captures, Fixnum::Encode(iNth));
            return true;
        } // SetCapture

        private: virtual void SetError(int nPosn, int nError) override
        {
            m_posn  = Fixnum::Encode(nPosn);
            m_error = Fixnum::Encode(nError);
        } // SetError
    } oContext;

    StringData oString(string, start, end);

    IRegex* pIRegex = Regex::Compile(
        &oContext,
        oString.GetStart(),
        oString.GetLength(),
        rgfFlag );

    if (NULL == pIRegex)
    {
        error("Regex compile failed at ~D",
            oContext.m_posn );
    }

    if (simple_string_p(string) &&
        zero == start &&
        (nil == end || length(string) == end) )
    {
        // Simple string
    }
    else
    {
        string = make_string(oString.GetStart(), oString.GetLength());
    }

    RegexObj* pRegex = new RegexObj(
        string,
        oContext.m_blob,
        oContext.m_captures );

    return pRegex->Encode();
} // compile_regexV

/// <summary>
///  Advance regex-match next matched position.
/// </summary>
/// <param name="match">A regex-match object</param>
/// <param name="range">A string-range object</param>
/// <returns>A regex-match object passed by parameter</returns>
defmethod(next_match_using_source, string_range, (Val range, Val match))
{
    RegexMatch* pMatch  = match->StaticCast<RegexMatch>();
    StringRange* pRange = range->StaticCast<StringRange>();

    StringMatchContext oContext(
        pMatch->m_regex,
        pRange->m_string,
        pRange->m_start,
        pRange->m_end );

    RegexObj* pRegex = pMatch->m_regex->StaticCast<RegexObj>();

    IRegex* pIRegex = reinterpret_cast<IRegex*>(
        pRegex->m_blob->StaticCast<U32Vec>()->GetStart() );

    bool fMatched = Regex::NextMatch(pIRegex, &oContext);
    oContext.SetMatch(pMatch, fMatched);

    return match;
} // next_match_using_source

/// <summary>
///  Print regex object on specified stream.
/// </summary>
/// <param name="match">A regex object</param>
/// <param name="s">A output-stream object</param>
/// <returns>A regex object</returns>
defmethod(print_object, regex, (Val re, Val s))
{
    Layout_regex* pRegex = reinterpret_cast<Layout_regex*>(
        re->StaticCast<Record>() );

    format(s, "#<Regex ~S @ ~X~X>",
        pRegex->m_string,
        Fixnum::Encode(re->ToInt() >> 4),
        Fixnum::Encode(re->ToInt() & 15) );

    return re;
} // print_object

/// <summary>
///  Extract string from specified range.
/// </summary>
/// <param name="range">A string-range object</param>
/// <returns>A string object</returns>
defmethod(range_string, string_range, (Val range))
{
    StringRange* pRange = range->StaticCast<StringRange>();
    return subseq(pRange->m_string, pRange->m_start, pRange->m_end);
} // range_string

/// <summary>
///  Print regex-match object on specified stream.
/// </summary>
/// <param name="match">A regex-match object</param>
/// <param name="s">A output-stream object</param>
/// <returns>A regex-match object</returns>
defmethod(print_object, regex_match, (Val match, Val s))
{
    Layout_regex_match* pMatch =
        reinterpret_cast<Layout_regex_match*>(
            match->StaticCast<Record>() );

    if (nil != pMatch->m_matched_p)
    {
        Val v = pMatch->m_captures;
        Val r = svref(v, zero);
        Range* pRange = r->StaticCast<Range>();

        format(s, "#<~S Matched [~D, ~D] @ ~X~X>",
            type_of(match),
            pRange->m_start,
            pRange->m_end,
            Fixnum::Encode(match->ToInt() >> 4),
            Fixnum::Encode(match->ToInt() & 15) );
    }
    else
    {
        format(s, "#<~S Not Matched @~X~X>",
            type_of(match),
            Fixnum::Encode(match->ToInt() >> 4),
            Fixnum::Encode(match->ToInt() & 15) );
    }

    return match;
} // print_object

/// <summary>
///   Execute regex on specified range of string.
/// </summary>
/// <returns>regex-match object</returns>
defun(string_match, (Val regex, Val string, Val start, Val end))
{
    check_type(regex, regex);

    StringMatchContext oContext(regex, string, start, end);

    RegexObj* pRegex = regex->StaticCast<RegexObj>();

    IRegex* pIRegex = reinterpret_cast<IRegex*>(
        pRegex->m_blob->StaticCast<U32Vec>()->GetStart() );

    bool fMatched = Regex::StartMatch(pIRegex, &oContext);

    StringRange* pRange = new StringRange;
    pRange->m_end    = end;
    pRange->m_start  = start;
    pRange->m_string = string;

    RegexMatch*  pMatch = new RegexMatch;
    pMatch->m_range = pRange->Encode();
    pMatch->m_regex = regex;

    oContext.SetMatch(pMatch, fMatched);

    return pMatch->Encode();
} // string_match

} // TinyCl
