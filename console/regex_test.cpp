#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Toplevel
// tinycl.cpp
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/console/console.cpp#1 $
//
#include "../tinycl/tinycl.h"

class Source : public Regex::ISource
{
    private: long           m_cwch;
    private: const char16*  m_pwch;

    public: Source(const char16* pwsz) :
        m_cwch(::lstrlenW(pwsz)),
        m_pwch(pwsz) {}

    public: Source(const char16* pwch, long cwch) :
        m_cwch(cwch),
        m_pwch(pwch) {}

    public: virtual Regex::char16 GetChar(long lPosn) const
    {
        ASSERT(lPosn >= 0 && lPosn <= m_cwch);
        return m_pwch[lPosn];
    } // GetChar

    public: virtual void GetInfo(Regex::SourceInfo* p) const
    {
        p->m_lStart     = 0;
        p->m_lEnd       = m_cwch;
        p->m_lScanStart = 0;
        p->m_lScanEnd   = m_cwch;
    } // GetInfo
}; // Source

using namespace TinyCl;

namespace TinyCl
{
    Val make_file_stream(HANDLE, Val);
    void toplevel(Val);
} // TinyCl

Val Kignore_case;
Val Kmultiple_lines;
Val Ksingle_line;

defun(backslash, (Val src))
{
    static const char16 k_rgwchMap[] =
    {
        'a',    0x07,   // alarm
        'b',    0x08,   // backspace
        'e',    0x1B,   // esc
        'f',    0x0C,   // page
        'n',    0x0A,   // lf
        'r',    0x0D,   // cardige return
        't',    0x09,   // tab
        'v',    0x0B,   // vertical tab
    }; // k_rgwchMap

    class Labels
    {
        public: static char16 MapBackslash(char16 wch)
        {
            for (
                int i = 0;
                i < lengthof(k_rgwchMap);
                i += 2 )
            {
                if (k_rgwchMap[i] == wch) return k_rgwchMap[i+1];
            } // for i
            return 0;
        } // MapBackslash
    }; // Labels

    CString oSrc(src);
    CharSink_<> oSink;
    int iNum = 0;
    int cDigits = 0;
    enum State
    {
        State_None,

        State_Backslash,
        State_Unicode,
    } eState = State_None;

    foreach (CString::Enum, oEnum, oSrc)
    {
        char16 wch = oEnum.Get();
        switch (eState)
        {
        case State_None:
            if ('\\' == wch)
            {
                eState = State_Backslash;
            }
            else
            {
                oSink.Add(wch);
            }
            break;

        case State_Backslash:
        {
            char16 wchMap = Labels::MapBackslash(wch);
            if (0 != wchMap)
            {
                oSink.Add(wchMap);
                eState = State_None;
            }
            else if ('u' == wch)
            {
                iNum = 0;
                cDigits = 0;
                eState = State_Unicode;
            }
            else
            {
                oSink.Add(wch);
                eState = State_None;
            }
            break;
        } // State_Backslash

        case State_Unicode:
            iNum *= 16;
            if (wch >= '0' && wch <= '9')
            {
                iNum += wch - '0';
            }
            else if (wch >= 'A' && wch <= 'F')
            {
                iNum += wch - 'A' + 10;
            }
 
            cDigits += 1;
            if (4 == cDigits)
            {
                oSink.Add(static_cast<char16>(iNum));
                eState = State_None;
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // for each char
    return make_string(oSink.GetStart(), oSink.GetLength());
} // backslash


// (test-case id pat src opts expect)
defun(test_case, (Val id, Val pat, Val src, Val opts, Val expect))
{
    check_type(id, simple_string);
    check_type(opts, list);

    unless (listp(expect)) expect = list(expect);

    CString oPat(pat);

    int rgfOption = 0;

    foreach (List::Enum, oEnum, opts)
    {
        Val opt = oEnum.Get();
        oEnum.Next();

        Val val = oEnum.Get();

        if (Kignore_case == opt)
        {
            rgfOption &= ~Regex::Option_IgnoreCase;
            when (nil != val) rgfOption |=  Regex::Option_IgnoreCase;
        }
        else if (Kmultiple_lines == opt)
        {
            rgfOption &= ~Regex::Option_Multiline;
            when (nil != val) rgfOption |=  Regex::Option_Multiline;
        }
        else if (Ksingle_line == opt)
        {
            rgfOption &= ~Regex::Option_Singleline;
            when (nil != val) rgfOption |=  Regex::Option_Singleline;
        }
        else
        {
            error("Unknown Option ~S", opt);
        }
    } // for each opt

    Regex::IContext* pIContext = Regex::CreateContext();

    Regex::IRegex* pIRegex = pIContext->Compile(
        oPat.GetStart(),
        static_cast<int>(oPat.GetLength()),
        rgfOption );

    if (NULL == pIRegex)
    {
        return nil;
    }

    // Set source
    CString oSrc(src);
    Source oSource(oSrc.GetStart(), static_cast<int>(oSrc.GetLength()));
    Regex::IMatch* pIMatch = pIRegex->FirstMatch(&oSource);
    if (NULL == pIMatch)
    {
        return nil;
    }

    Val head = list(0);
    Val tail = head;
    int cCaptures = pIMatch->GetCaptureCount();
    for (int i = 0; i <= cCaptures; i++)
    {
        long lStart, lEnd;
        pIMatch->GetCapture(i, &lStart, &lEnd);
        Val str = make_string(oSrc.GetStart() + lStart, lEnd - lStart);
        tail = setf_cdr(list(str), tail);
    } // for i
    return cdr(head);
} // test_case

void installFunction(
    const char* psz,
    int         iMin,
    int         iMax,
    int         iVals,
    int         fRest,
    Fn          pfn )
{
    Val sym = intern(psz);
    sym->StaticCast<Symbol>()->m_function =
        MakeWrapper(sym, iMin, iMax, iVals, fRest, pfn);
} // installFunction

// Entry Point
int __cdecl main(int, char16**)
{
    #if 0
    FARPROC pfn = ::GetProcAddress(::GetModuleHandle(NULL), "ExeVersion");
    printf("ExeVersion is %p\n", pfn);
    #endif

    TinyCl::Initialize();

    TLV(Aerror_outputA) = make_file_stream(
        ::GetStdHandle(STD_ERROR_HANDLE),
        Fixnum::Encode(StreamFlag_Output) );

    TLV(Astandard_inputA) = make_file_stream(
        ::GetStdHandle(STD_INPUT_HANDLE),
        Fixnum::Encode(StreamFlag_Input) );

    TLV(Astandard_outputA) = make_file_stream(
        ::GetStdHandle(STD_OUTPUT_HANDLE),
        Fixnum::Encode(StreamFlag_Output) );

    // For regex test
    Kignore_case    = intern("IGNORE-CASE", PKG_keyword);
    Kmultiple_lines = intern("MULTIPLE-LINES", PKG_keyword);
    Ksingle_line    = intern("SINGLE-LINE", PKG_keyword);

    installFunction("BACKSLASH", 1, 1, 1, 0, reinterpret_cast<Fn>(backslash));
    installFunction("TEST-CASE", 5, 5, 1, 0, reinterpret_cast<Fn>(test_case));

    installFunction("INVOKE-DEBUGGER", 1, 1, 1, 0, reinterpret_cast<Fn>(toplevel));

    toplevel(nil);

    return 0;
} // main

#if NDEBUG
extern "C" void __cdecl mainCRTStartup()
{
    int iRet = main(0, NULL);
    ::ExitProcess(iRet);
} // WinMainCRTStartup
#endif // NDEBUG
