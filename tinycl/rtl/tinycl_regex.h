//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Regex Definitions
// tinycl_regex.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_regex.h#2 $
//
#if !defined(INCLUDE_tinycl_regex_h)
#define INCLUDE_tinycl_regex_h

#include "../tinycl.h"
#include "../../regex/IRegex.h"

namespace TinyCl
{

using namespace CommonLisp;

typedef Regex::IRegex IRegex;

/// <remark>
///   Regex Lisp object
/// </remark>
class RegexObj : public Record_<RegexObj, Layout_regex>
{
    public: static Val ClassD_() { return CLASSD_regex; }

    public: RegexObj(Val string, Val blob, Val captures)
    {
        m_blob      = blob;
        m_captures  = captures;
        m_string    = string;
    } // Regex
}; // RegexObj

/// <remark>
///   Regex Match Lisp object
/// </remark>
class RegexMatch :
    public Record_<RegexMatch, Layout_regex_match>
{
    public: static Val ClassD_() { return CLASSD_regex_match; }
}; // RegexMatch

/// <remark>
///  Range Lisp Object
/// </remark>
class Range :
    public Record_<Range, Layout_range>
{
    /// <summary>
    ///  Check x is a range or not.
    /// </summary>
    /// <param name="x">object</param>
    /// <returns>True if x is range object.</returns>
    public: static bool Is_(const Datum* const x)
    {
        Record* p = x->DynamicCast<Record>();
        if (NULL == p)
        {
            return false;
        }
        ClassD* pClassD = p->m_classd->StaticCast<ClassD>();
        return nil != memq(CLASS_range, pClassD->m_class_precedence_list);
    } // Is_
}; // Range

/// <remark>
///   String Range Lisp object
/// </remark>
class StringRange :
    public Record_<StringRange, Layout_string_range>
{
    public: static Val ClassD_() { return CLASSD_string_range; }
}; // StringRange

/// <remark>
///   Match Context
/// </remark>
class MatchContext : public Regex::IMatchContext
{
    protected: typedef Regex::Posn Posn;

    protected: Val m_captures;

    /// <summary>
    ///  Construct MatchContext object for specified regex.
    /// </summary>
    /// <param name="regex">A regex object</param>
    protected: MatchContext(Val regex)
    {
        RegexObj* pRegex = regex->StaticCast<RegexObj>();
        m_captures = make_vector(length(pRegex->m_captures));
    } // MatchContext

    // [C]
    protected: bool charEqCi(char16 wch1, char16 wch2) const
        { return char_equal(wch1, wch2); }

    protected: static bool charEqCs(char16 wch1, char16 wch2)
        { return wch1 == wch2; }

    // [G]
    protected: virtual bool GetCapture(
        int     nCapture,
        Posn*   out_lStart,
        Posn*   out_lEnd ) const override
    {
        Val  range = svref(m_captures, Fixnum::Encode(nCapture));
        if (nil == range)
        {
            return false;
        }

        StringRange* pRange = range->StaticCast<StringRange>();

        *out_lStart = Fixnum::Decode_(pRange->m_start);
        *out_lEnd   = Fixnum::Decode_(pRange->m_end);

        return true;
    } // GetCapture

    // [R]
    private: virtual void ResetCapture(int nCapture) override
    {
        setf_svref(nil, m_captures, Fixnum::Encode(nCapture));
    } // ResetCapture

    private: virtual void ResetCaptures() override
    {
        SimpleVector* p = m_captures->StaticCast<SimpleVector>();
        foreach (SimpleVector::Enum, oEnum, p)
        {
            oEnum.Set(nil);
        } // for each elt
    } // ResetCaptures

    // [S]
    public: void SetMatch(RegexMatch* pMatch, bool fMatched) const
    {
        pMatch->m_captures  = m_captures;
        pMatch->m_matched_p = fMatched ? t : nil;
    } // MakeMatch
}; // MatchContext

inline bool regex_p(Val x)       { return x->Is<RegexObj>(); }
inline bool regex_match_p(Val x) { return x->Is<RegexMatch>(); }

} // TinyCl

#endif //!defined(INCLUDE_tinycl_regex_h)
