#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Console Toplevel
// console.cpp
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/console/console.cpp#14 $
//
#include "../tinycl/tinycl.h"
#include "../tinycl/init/tinycl_init.h"
#include "../tinycl/win/tinycl_win.h"

using namespace TinyCl;
using namespace TinyCl::Private;

namespace TinyCl
{
    Val make_file_stream(HANDLE, Val);
    void toplevel(Val);
} // TinyCl

static uint s_nDebuggerDepth;

void __declspec(noreturn)
invoke_debugger(Val cond)
{
    s_nDebuggerDepth += 1;

    if (interactive_stream_p(TLV(Astandard_inputA)))
    {
        toplevel(cond);
    }

    if (1 == s_nDebuggerDepth)
    {
        funcall(Qforce_output, TLV(Aerror_outputA));
        funcall(Qforce_output, TLV(Astandard_outputA));
    }

    ::ExitProcess(1);
} // invoke_debugger

namespace
{

class EnumArg
{
    public: struct Range
    {
        const char16*  m_pwchEnd;
        const char16*  m_pwchStart;

        Range() :
            m_pwchEnd(NULL),
            m_pwchStart(NULL) {}

        bool Equal(const char* const pszStart) const
        {
            const char* pch = pszStart;
            for (
                const char16* pwch = m_pwchStart;
                pwch < m_pwchEnd;
                pwch++ )
            {
                if (0 == *pch)
                {
                    return false;
                }

                if (*pwch != *pch)
                {
                    return false;
                }

                pch++;
            } // for
            return true;
        } // Equal

        bool IsEmpty() const
            { return m_pwchEnd <= m_pwchStart; }
    }; // Range

    private: Range          m_oRange;
    private: const char16*  m_pwsz;

    public: EnumArg(const char16* pwsz) :
        m_pwsz(pwsz)
    {
        if (! AtEnd())
        {
            Next();
        }
    } // EnumArg

    public: bool AtEnd() const
        { return 0 == *m_pwsz && m_oRange.IsEmpty(); }

    public: const Range* Get() const
        { ASSERT(! AtEnd()); return &m_oRange; }

    private: static bool isSpace(char16 wch)
        { return ' ' == wch || '\t' == wch; }

    public: void Next()
    {
        ASSERT(! AtEnd());
        while (isSpace(*m_pwsz))
        {
            m_pwsz++;
        }

        if ('"' != *m_pwsz)
        {
            m_oRange.m_pwchStart = m_pwsz;
            while (0 != *m_pwsz)
            {
                if (isSpace(*m_pwsz))
                {
                    m_oRange.m_pwchEnd = m_pwsz;
                    m_pwsz++;
                    return;
                }
                m_pwsz++;
            } // while
        }
        else
        {
            m_pwsz++;
            m_oRange.m_pwchStart = m_pwsz;
            while (0 != *m_pwsz)
            {
                if ('"' == *m_pwsz)
                {
                    m_oRange.m_pwchEnd = m_pwsz;
                    m_pwsz++;
                    return;
                }
                m_pwsz++;
            } // while
        }
        m_oRange.m_pwchEnd = m_pwsz;
    } // Next
}; // EnumArg

class Initializer
{
    private: static HANDLE findImage()
    {
        char16 wszImage[MAX_PATH + 1];
        if (0 == ::GetModuleFileName(NULL, wszImage, lengthof(wszImage)))
        {
            return INVALID_HANDLE_VALUE;
        }

        {
            char16* pwszDot = ::lstrrchrW(wszImage, '.');
            if (NULL == pwszDot)
            {
                return INVALID_HANDLE_VALUE;
            }
            ::lstrcpyW(pwszDot + 1, L"image");
        }

        char16* pwszSlash = ::lstrrchrW(wszImage, Backslash);
        if (NULL == pwszSlash)
        {
            // Module filename doesn't have slash!
            return INVALID_HANDLE_VALUE;
        }

        char16* pwszName = pwszSlash + 1;

        for (;;)
        {
            #if DEBUG_LOAD
                DEBUG_PRINTF("load %ls\n", wszImage);
            #endif

            HANDLE hImage = ::CreateFileW(
                wszImage,
                GENERIC_READ,                           // dwAccess,
                FILE_SHARE_READ | FILE_SHARE_DELETE,    // dwShare,
                NULL,
                OPEN_EXISTING,                          // dwCreate,
                FILE_FLAG_SEQUENTIAL_SCAN,              // dwFlags
                NULL );
            if (INVALID_HANDLE_VALUE != hImage)
            {
                return hImage;
            }

            for (;;)
            {
                --pwszSlash;
                if (pwszSlash <= wszImage)
                {
                    return INVALID_HANDLE_VALUE;
                }

                if (Backslash == *pwszSlash)
                {
                    break;
                }
            } // for

            ::lstrcpyW(pwszSlash + 1, pwszName);
        } // for
    } // findImage

    private: static void installFunction(
        const char* const psz,
        int         const iMin,
        int         const iMax,
        int         const iVals,
        int         const fRest,
        const char* const pszFn )
    {
        Val sym = InternSymbol(psz);
        sym->StaticCast<Symbol>()->m_function =
            MakeWrapper(sym, iMin, iMax, iVals, fRest, false, pszFn);
    } // installFunction

    public: static void Run()
    {
        FileHandle shImage;

        bool fGenesis = false;
        foreach (EnumArg, oEnum, ::GetCommandLineW())
        {
            if (oEnum.Get()->Equal("-genesis"))
            {
                fGenesis = true;
            }
        } // for

        if (! fGenesis)
        {
            shImage = findImage();
        }

        InitParams oParams;
        oParams.m_hImage      = shImage;
        oParams.m_hSelf       = ::GetModuleHandle(NULL);
        oParams.m_nTotalMb    = 64;
        oParams.m_pvStaticEnd =
            reinterpret_cast<void*>(STATIC_OBJECT_END);

        Thread* pth = Initialize(&oParams);

        if (INVALID_HANDLE_VALUE == shImage.h)
        {
            Mm::FinalizeStatic(pth);
            installFunction("INVOKE-DEBUGGER", 1, 1, 0, 1, "invoke_debugger");
        }

        Thread::Get()->Restart();
    } // Run
}; // Initializer
} // namespace

/// <summary>
///  Entry point of TinyCl Console Command Loop.
/// </summary>
/// <returns>Exit code</returns>
int __cdecl main(int, char16**)
{
    Initializer::Run();

    TLV(Aerror_outputA) = make_file_stream(
        ::GetStdHandle(STD_ERROR_HANDLE),
        Fixnum::Encode(StreamFlag_Output) );

    TLV(Astandard_inputA) = make_file_stream(
        ::GetStdHandle(STD_INPUT_HANDLE),
        Fixnum::Encode(StreamFlag_Input) );

    TLV(Astandard_outputA) = make_file_stream(
        ::GetStdHandle(STD_OUTPUT_HANDLE),
        Fixnum::Encode(StreamFlag_Output) );

    #if _DEBUG
        TLV(ApackageA) = PKG_si;
    #endif

    Val const fn = Qstart_application->StaticCast<Symbol>()->m_function;
    if (nil != fn)
    {
        funcall(fn);
    }
    else
    {
        toplevel(nil);
    }

    return 0;
} // main

#if NDEBUG
/// <summary>
///  Entry of TinyCl Console Command Loop
/// </summary>
extern "C" void __cdecl mainCRTStartup()
{
    int iRet = main(0, NULL);
    ::ExitProcess(iRet);
} // WinMainCRTStartup
#endif // NDEBUG
