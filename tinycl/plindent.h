//////////////////////////////////////////////////////////////////////////////
//
// PlIndnt - Perl Indent
// document.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/plindent.h#1 $
//
// See [1] for syntax of Mason.
//
// References:
//  [1] http://www.masonhq.com/docs/manual/Devel.html
//
#if !defined(INCLUDE_plindent_h)
#define INCLUDE_plindnet_h

#define foreach(mp_enum, mp_var, mp_arg) \
    for (mp_enum mp_var(mp_arg); ! (mp_var).AtEnd(); (mp_var).Next())

#define lengthof(a) ( sizeof(a) / sizeof(*(a)) )
#define override virtual
#define NoReturn __declspec(noreturn)


#if _DEBUG
    #define ASSERT(mp_exp)  Assert(__FILE__, __LINE__, #mp_exp, mp_exp);
    #define DEBUG_PRINTF    DebugPrintf
    void Assert(const char*, int, const char*, bool);
    void DebugPrintf(const char*, ...);
#else
    #define ASSERT(mp_exp)  __assume(mp_exp)
    #define DEBUG_PRINTF    __noop
#endif

class InputStream;
class Document;

#endif // !defined(INCLUDE_plindent_h)
