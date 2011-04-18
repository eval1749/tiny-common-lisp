#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime for Windows
// tinycl_win_rtl.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/win/tinycl_win_rtl.cpp#20 $
//
#define DEBUG_ALLOC 1
#define DEBUG_GC    1
#include "./tinycl_win.h"

#include "../arch/x86/tinycl_x86.h"
#include "../tinycl_dll_link.h"

namespace TinyCl
{

void PlatformStreamRestart();

namespace Private
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

}; // Private

using namespace Private;

class WindowsHost :
    public Record_<WindowsHost, Layout_windows_host>
{
    public: static Val ClassD_() { return CLASSD_windows_host; }
}; // WindowsHost

class WindowsPathname :
    public Record_<WindowsPathname, Layout_windows_pathname>
{
    public: static Val ClassD_() { return CLASSD_windows_pathname; }
}; // WindowsPathname

inline bool windows_pathname_p(Val x)
    { return x->Is<WindowsPathname>(); }

static DWORD s_dwTlsIndex;

static Val make_windows_host(Val);

void __declspec(noreturn) platform_error(char* psz);

static Val parsePathname(Val, const char16*, Int, Int*);

defmethod(unparse_pathname, windows_pathname, (Val x, Val which, Val s));

// [D]
/// <summary>
///   Resolve DLL function.
/// </summary>
/// <param name="pEntry">A DLL entry.</param>
/// <param name="pThread">A thread call DLL function.</param>
void* __fastcall
DllResolve(
    Thread*     const pThread, 
    DllEntry*   const pEntry)
{
    ASSERT(NULL != pThread);
    ASSERT(NULL != pEntry);

    DllProcInfo* const pProcInfo = pEntry->m_proc_info->
        StaticCast<DllProcInfo>();

    DllFileInfo* const pFileInfo = pProcInfo->m_dll_file_info->
        StaticCast<DllFileInfo>();

    if (NULL == pFileInfo->m_handle)
    {
        HMODULE hModule = ::LoadLibraryW(
            pFileInfo->m_pathname->StaticCast<SimpleString>()->GetStart() );

        if (NULL == hModule)
        {
            goto error;
        } // if

        pFileInfo->m_handle = Fixnum::Encode(hModule);
    } // if not loaded

    // Get Procedure Address
    {
        CHAR szProcName[100];
        {
            SimpleString* pName = pProcInfo->m_name->
                StaticCast<SimpleString>();

            const char16* const pwchStart = 
                pName->GetStart();

            const char16* const pwchEnd = 
                pName->GetStart() + pName->GetLength();

            CHAR* pszRunner = szProcName;
            for (
                LPCWSTR pwchRunner = pwchStart;
                pwchRunner < pwchEnd;
                pwchRunner++ )
            {
                *pszRunner++ = static_cast<CHAR>(
                    *pwchRunner & 0xff );
            } // for

            *pszRunner = 0;
        } // szProcName

        pEntry->m_pfn = ::GetProcAddress(
            reinterpret_cast<HMODULE>(pFileInfo->m_handle),
            szProcName );

        if (NULL == pEntry->m_pfn)
        {
            goto error;
        }

        return pEntry->m_pfn;
    } // GetProcAddress

  error:
    {
        DWORD const dwError = ::GetLastError();
        error(Qdll_link_error,
            Kcode,     MakeUInt(dwError),
            Koperation, make_string("LoadLibrary"),
            Koperands, list(pFileInfo->m_pathname, pProcInfo->m_name) );
    } // error
} // DllResolver

// [G]
defun(get_decoded_time, ())
{
    TIME_ZONE_INFORMATION oInfo;
    DWORD const nInfo = ::GetTimeZoneInformation(&oInfo);

    Val const bias = div(
        Fixnum::Encode(oInfo.Bias), 
        Fixnum::Encode(60) );

    SYSTEMTIME stNow;
    ::GetLocalTime(&stNow);

    Thread* const pth = Thread::Get();

    pth->mv_value[0] = Fixnum::Encode(stNow.wSecond);
    pth->mv_value[1] = Fixnum::Encode(stNow.wMinute);
    pth->mv_value[2] = Fixnum::Encode(stNow.wHour);
    pth->mv_value[3] = Fixnum::Encode(stNow.wDay);
    pth->mv_value[4] = Fixnum::Encode(stNow.wMonth);
    pth->mv_value[5] = Fixnum::Encode(stNow.wYear);
    pth->mv_value[6] = Fixnum::Encode((stNow.wDayOfWeek + 6) % 7);
    pth->mv_value[7] = TIME_ZONE_ID_DAYLIGHT == nInfo ? t : nil;
    pth->mv_value[8] = bias;

    pth->m_n = Fixnum::Encode(9);
    return pth->mv_value[0];
} // get_decoded_time

// [L]
defun(long_site_name, ())
{
    BOOL fSucceeded;
    WCHAR rgwchName[200];
    DWORD cwchName = lengthof(rgwchName);

    fSucceeded = ::GetComputerNameEx(
        ComputerNamePhysicalDnsFullyQualified,
        rgwchName,
        &cwchName );

    if (! fSucceeded)
    {
        platform_error("GetComputerNameEx");
    }

    return make_string(rgwchName);
} // long_site_name

// [M]
defun(machine_instance, ())
{
    BOOL fSucceeded;
    WCHAR rgwchName[200];
    DWORD cwchName = lengthof(rgwchName);

    fSucceeded = ::GetComputerNameEx(
        ComputerNameNetBIOS,
        rgwchName,
        &cwchName );

    if (! fSucceeded)
    {
        platform_error("GetComputerNameEx");
    }

    return make_string(rgwchName);
} // machine_instance

defmethod(make_pathname_using_host, windows_host, (
    Val const host,
    Val const device,
    Val const directory,
    Val const name,
    Val const type, Val) )
{
    Val const path = Thread::Get()->AllocRecord(CLASSD_windows_pathname);
    {
        WindowsPathname* const p = path->StaticCast<WindowsPathname>();
        p->m_host = host;
        p->m_device = device;
        p->m_directory = directory;
        p->m_name = name;
        p->m_type = type;
        p->m_version = Kunspecific;
    }
    return path;
} // make_pathname_using_host

/// <summary>
///  Make a window-host object of specified name.
/// </summary>
/// <param name="name">A windows host name in string.</param>
/// <returns>a newly created window-host object</returns>
defun(make_windows_host, (Val const name))
{
    check_type(name, string);

    Val const host = Thread::Get()->AllocRecord(CLASSD_windows_host);
    {
        WindowsHost* const p = host->StaticCast<WindowsHost>();
        p->m_customary_case = Kdowncase;
        p->m_default_device = nil;
        p->m_local_case     = Kpreserve;
        p->m_name           = name;
    }

    return host;
} // make_windows_host

// [P]

static const char16 k_wszInvalidPathnameChars[] = L"<>:/\"\\|";

static inline bool isValidPathnameChar(char16 const wch)
    { return NULL == ::lstrchrW(k_wszInvalidPathnameChars, wch); }

/// <summary>
///   Parse pathname string.
/// </summary>
static Val parsePathname(
    Val             host,
    const char16*   const pwchStart,
    Int             const cwch,
    Int*            const out_iIndex )
{
    class Lexer
    {
        private: class EnumChar
        {
            private: const char16*  m_pwch;
            private: const char16*  m_pwchEnd;
            private: const char16*  m_pwchStart;

            public: EnumChar(const char16* pwchStart, Int cwch) :
                m_pwch(pwchStart),
                m_pwchEnd(pwchStart + cwch),
                m_pwchStart(pwchStart) {}

            public: bool AtEnd() const
                { return m_pwch >= m_pwchEnd; }

            public: char16 Get() const
                { ASSERT(!AtEnd()); return *m_pwch; }

            public: const char16* GetPtr() const
                { return m_pwch; }

            public: void Next()
            {
                ASSERT(!AtEnd());
                m_pwch++;
            } // Next

            public: void Prev()
            {
                ASSERT(m_pwch > m_pwchStart);
                --m_pwch;
            } // Prev
        }; // EnumChar

        private: EnumChar   m_oEnum;
        private: const char16*      m_pwchComp;
        private: const char16*      m_pwchDot;
        private: const char16*      m_pwchSlash;

        public: Lexer(const char16* const pwchStart, Int const cwch) :
            m_oEnum(pwchStart, cwch),
            m_pwchComp(NULL),
            m_pwchDot(NULL),
            m_pwchSlash(NULL)
        {
            const char16* pwchEnd = pwchStart + cwch;
            for (
                const char16* pwch = pwchStart;
                pwch < pwchEnd;
                pwch++ )
            {
                switch (*pwch)
                {
                case '.':
                    m_pwchDot = pwch;
                    break;

                case '/':
                case '\\':
                    m_pwchSlash = pwch;
                    break;
                } // switch wch
            } // for each char

            if (m_pwchDot == pwchStart)
            {
                // Dot at start is not separator.
                m_pwchDot = NULL;
            }
            else if (m_pwchDot <= m_pwchSlash + 1)
            {
                // Dot just after slash is not separater.
                m_pwchDot = NULL;
            }
        } // Lexer

        public: Val Get()
        {
            if (m_oEnum.AtEnd())
            {
                return nil;
            }

            enum State
            {
                State_Component,
                State_Start,
                State_Slash,
                State_Wild,
            } eState = State_Start;

            m_pwchComp = m_oEnum.GetPtr();

            while (! m_oEnum.AtEnd())
            {
                char16 wch = m_oEnum.Get();

                m_oEnum.Next();

                switch (eState)
                {
                case State_Component:
                    switch (wch)
                    {
                    case '.':
                        if (m_oEnum.GetPtr() - 1 != m_pwchDot)
                        {
                            break;
                        }
                        // FALLTHROUGH

                    case '/':
                    case '\\':
                    case '*':
                    case '?':
                    case ':':
                        m_oEnum.Prev();
                        return Qstring;
                    } // switch wch
                    break;

                case State_Slash:
                    switch (wch)
                    {
                    case '/':
                    case '\\':
                        return Khost;

                    default:
                        m_oEnum.Prev();
                        return Kdirectory;
                    } // switch wch
                    // NOTREACHED

                case State_Start:
                    switch (wch)
                    {
                    case '*':
                        eState = State_Wild;
                        break;

                    case '.':
                        if (m_oEnum.GetPtr() - 1 == m_pwchDot)
                        {
                            return Ktype;
                        }
                        eState = State_Component;
                        break;

                    case '/':
                    case '\\':
                        eState = State_Slash;
                        break;

                    case '?':
                        return Kwild_1;

                    case ':':
                        return Kdevice;

                    default:
                        if (! isValidPathnameChar(wch))
                        {
                            m_oEnum.Prev();
                            return Kinvalid;
                        }

                        eState = State_Component;
                        break;
                    } // swtich wch
                    break;

                case State_Wild:
                    switch (wch)
                    {
                    case '*':
                        if (m_oEnum.GetPtr() > m_pwchSlash)
                        {
                            // "**" appears after the last slash
                            return Kinvalid;
                        }
                        
                        if ('/' == m_oEnum.Get())
                        {
                            return Kwild_inferior;
                        }

                        if ('\\' == m_oEnum.Get())
                        {
                            return Kwild_inferior;
                        }

                        m_oEnum.Prev();
                        return Kinvalid;

                    default:
                        m_oEnum.Prev();
                        return Kwild;
                    }
                } // switch eState
            } // while

            return Qstring;
        } // Get

        public: const char16* GetComp() const
            { return m_pwchComp; }

        public: const char16* GetDot() const
            { return m_pwchDot; }

        public: const char16* GetPtr() const
            { return m_oEnum.GetPtr(); }

        public: Val GetString() const
            { return make_string(GetComp(), GetPtr() - GetComp()); }
    }; // Lexer

    Lexer oLexer(pwchStart, cwch);

    Val comp      = nil;
    Val device    = nil;
    Val dirs      = nil;
    Val last_dirs = nil;
    Val name      = nil;

    enum State
    {
        State_AfterDevice,
        State_AfterShare,
        State_Component,
        State_Host,
        State_Share,
        State_Start,
    } eState = State_Start;

    for (;;)
    {
        Val token = oLexer.Get();

        if (nil == token)
        {
            // At end of pathname
            if (State_Component != eState)
            {
                goto errorUnexpectedEnd;
            }

            Val type = nil;

            if (nil == name)
            {
                name = comp;
            }
            else
            {
                type = comp;
            }

            Val const pathname = Thread::Get()->AllocRecord(
                CLASSD_windows_pathname );
            {
                WindowsPathname* const p =
                    pathname->StaticCast<WindowsPathname>();
                p->m_host      = host;
                p->m_device    = device;
                p->m_directory = dirs;
                p->m_name      = name;
                p->m_type      = type;
                p->m_version   = Kunspecific;
            }
            *out_iIndex = oLexer.GetPtr() - pwchStart;
            return pathname;
        }
        else if (Kinvalid == token)
        {
            goto errorInvalidChar;
        }

        switch (eState)
        {
        case State_AfterDevice:
            eState = State_Component;
            comp = nil;

            if (Kdirectory == token)
            {
                setf_car(Kabsolute, dirs);
            }
            else
            {
                goto addComp;
            }
            break;

        case State_AfterShare:
            if (Kdirectory == token)
            {
                dirs      = list(Kabsolute);
                last_dirs = dirs;

                eState = State_Component;
                comp = nil;
            }
            else
            {
                goto errorExpectSlash;
            }
            break;

        case State_Component:
            if (Kdevice == token)
            {
                if (nil != device)
                {
                    // Device has already been specified
                    goto errorInvalidColon;
                }

                if (Krelative != car(dirs))
                {
                    // Pathname starts with slash(/)
                    goto errorInvalidColon;
                }

                if (SimpleString* p = comp->DynamicCast<SimpleString>())
                {
                    if (1 != p->GetLength())
                    {
                        goto errorInvalidChar;
                    }

                    char16 wchDrive = p->GetStart()[0];

                    if (wchDrive >= 'A' && wchDrive <= 'Z')
                    {
                        device = Character::FromCode(
                            static_cast<char16>(wchDrive - 'A' + 'a') );
                    }
                    else if (wchDrive >= 'a' && wchDrive <= 'z')
                    {
                        device = Character::FromCode(wchDrive);
                    }
                    else
                    {
                        goto errorInvalidDevice;
                    }
                }
                else
                {
                    goto errorInvalidDevice;
                }

                eState = State_AfterDevice;
            }
            else if (Kdirectory == token)
            {
                if (nil == comp)
                {
                    if (nil != dirs)
                    {
                        goto errorEmptyComp;
                    }

                    dirs = list(Kabsolute);
                    last_dirs = dirs;
                }

                if (SimpleString* p = comp->DynamicCast<SimpleString>())
                {
                    if (1 == p->GetLength() && '.' == p->GetStart()[0])
                    {
                        // ignore "./"
                        comp = nil;
                        break;
                    }

                    if (2 == p->GetLength() &&
                        '.' == p->GetStart()[0] &&
                        '.' == p->GetStart()[1] )
                    {
                        comp = Kback;
                    }
                } // if string

                last_dirs = setf_cdr(list(comp), last_dirs);
                comp = nil;
            }
            else if (Ktype == token)
            {
                name = comp;
                comp = nil;
            }
            else
            {
                goto addComp;
            }
            break;

        case State_Host:
            if (Qstring == token)
            {
                Val const name = oLexer.GetString();
                Val another = find_pathname_host(name, nil);
                if (nil == another)
                {
                    another = make_windows_host(name);
                }
                else if (! another->Is<WindowsHost>())
                {
                    goto errorBadHost;
                }

                host = another;
                eState = State_Share;
            }
            else
            {
                goto errorInvalidHost;
            }
            break;

        case State_Share:
            if (Qstring == token)
            {
                device = oLexer.GetString();

                eState = State_AfterShare;
            }
            else
            {
                goto errorInvalidShare;
            }
            break;

        case State_Start:
            if (Khost == token)
            {
                eState = State_Host;
            }
            else if (Kdirectory == token)
            {
                dirs = list(Kabsolute);
                last_dirs = dirs;

                eState = State_Component;
            }
            else
            {
                dirs = list(Krelative);
                last_dirs  = dirs;

                if (Qstring == token)
                {
                    token = oLexer.GetString();
                }

                comp = token;

                eState = State_Component;
            }
            break;

        default:
            CAN_NOT_HAPPEN();

        addComp:
            if (Qstring == token)
            {
                token = oLexer.GetString();
            }
            else if (Ktype == token)
            {
                goto errorInvalidChar;
            }

            if (nil == comp)
            {
                comp = token;
            }
            else if (Kwild_inferior == comp)
            {
                goto errorBadWildInferior;
            }
            else if (! consp(comp))
            {
                comp = list(comp, token);
            }
            else
            {
                comp = nconc(comp, list(token));
            }
            break;

        errorBadHost:
        errorBadWildInferior:
        errorEmptyComp:
        errorExpectSlash:
        errorInvalidChar:
        errorInvalidColon:
        errorInvalidDevice:
        errorInvalidHost:
        errorInvalidShare:
        errorUnexpectedEnd:
            *out_iIndex = oLexer.GetPtr() - pwchStart;
            return nil;
        } // switch state
    } // for
} // parsePathname

defmethod(parse_namestring_using_host, windows_host, (
    Val const host,
    Val const string,
    Val const start,
    Val const end) )
{
    StringData oString(string, start, end);

    Int iIndex;
    Val const pathname = parsePathname(
        host,
        oString.GetStart(),
        oString.GetLength(),
        &iIndex );
    return values(pathname, Fixnum::Encode(iIndex));
} // parse_name_string_using_host

/// <summary>
///  Returns true if specified pathnames are identical.
/// </summary>
defmethod(pathname_equal, windows_pathname, (Val const x, Val const y))
{
    check_type(y, windows_pathname);

    WindowsPathname* const pX = x->StaticCast<WindowsPathname>();
    WindowsPathname* const pY = y->StaticCast<WindowsPathname>();

    if (pX->m_host != pY->m_host)
    {
        return nil;
    }

    if (nil == funcall(Qequalp, pX->m_device, pY->m_device))
    {
        return nil;
    }

    if (nil == funcall(Qequalp, pX->m_directory, pY->m_directory))
    {
        return nil;
    }

    if (nil == funcall(Qequalp, pX->m_name, pY->m_name))
    {
        return nil;
    }

    if (nil == funcall(Qequalp, pX->m_type, pY->m_type))
    {
        return nil;
    }

    return pX->m_version == pY->m_version ? t : nil;
} // pathname_equal

void __declspec(noreturn) platform_error(char* const psz)
{
    DWORD dwError = ::GetLastError();
    error(Qplatform_error,
        Kcode,      MakeUInt(dwError),
        Koperation, make_string(psz) );
} // platform_error

/// <summary>
///   Restart platform.
/// </summary>
void PlatformRestart(Thread* pth)
{
    Val const host = make_windows_host(machine_instance());

    VAR(Apathname_hostsA) = list(host);

    Val const pn = pth->AllocRecord(CLASSD_windows_pathname);
    {
        WindowsPathname* const p = pn->StaticCast<WindowsPathname>();
        p->m_host      = host;
        p->m_device    = nil;
        p->m_directory = nil;
        p->m_name      = nil;
        p->m_type      = nil;
        p->m_version   = nil;
    }

    TLV(Adefault_pathname_defaultsA) = pn;

    {
        Val const tlvrec = svref(
            VAR(Atlv_vectorA),
            Fixnum::Encode(TLV_Adefault_pathname_defaultsA) );

        tlvrec->StaticCast<TlvRecord>()->m_value = pn;
    }

    {
        Val tail = nil;
        foreach (EnumArg, oEnum, ::GetCommandLineW())
        {
            const EnumArg::Range* pRange = oEnum.Get();

            Val arg = make_string(
                pRange->m_pwchStart,
                pRange->m_pwchEnd - pRange->m_pwchStart );

            arg = list(arg);

            if (nil == tail)
            {
                tail = list(arg);
                VAR(Acommand_line_argumentsA) = arg;
                tail = arg;
            }
            else
            {
                tail = setf_cdr(arg, tail);
            }
        } // for
    }

    PlatformStreamRestart();
} // PlatformRestart

defmethod(print_object, windows_pathname, (Val const x, Val const s))
{
    format(s, "#p~S", namestring(x));
    return x;
} // print_object

// [S]
defun(short_site_name, ())
{
    BOOL fSucceeded;
    WCHAR rgwchName[200];
    DWORD cwchName = lengthof(rgwchName);

    fSucceeded = ::GetComputerNameEx(
        ComputerNameDnsHostname,
        rgwchName,
        &cwchName );

    if (! fSucceeded)
    {
        platform_error("GetComputerNameEx");
    }

    return make_string(rgwchName);
} // short_site_name

// [T]
void* Thread::operator new(size_t)
{
    void* const pvArea = ::VirtualAlloc(
        NULL,
        AllocUnit * 4,
        MEM_COMMIT,
        PAGE_READWRITE );
    if (NULL == pvArea)
    {
        platform_error("VirtualAlloc");
    } // if

    return reinterpret_cast<uint8*>(pvArea) + sizeof(X86::ThreadExtra);
} // Thread::operator new

Thread* Thread::Get()
{
    return reinterpret_cast<Thread*>(::TlsGetValue(s_dwTlsIndex));
} // Thread::Get

/// <summary>
///   Start platform
///   <list>
///     <item><description>
///       Allocates memory area for static objects.
///     </description></item>
///     <item><description>
///       Allocates TLS for holding thread object.
///     </description></item>
///   </list>
/// </summary>
/// <param name="pParams">An initialization parameter</param>
Thread* Thread::PlatformStart(const InitParams* pParams)
{
    Mm::Start(pParams);
    s_dwTlsIndex = ::TlsAlloc();
    uint nThreadId = ::GetCurrentThreadId();
    Thread* pThread = new Thread(Fixnum::Encode(nThreadId));
    pThread->Start();
    return pThread;
} // Thread::PlatformStart

/// <summary>
///   Start lisp thread. All thread executes lisp function must be call
///   this method.
/// </summary>
void Thread::Start()
{
    ::TlsSetValue(s_dwTlsIndex, this);
} // Thread::Start

/// <summary>
///   Computes absolute pathname.
/// </summary>
/// <param name="x">A windows-pathame</param>
/// <returns>A windows-pathname</returns>
defmethod(truename, windows_pathname, (Val const x))
{
    WindowsPathname* const p = x->StaticCast<WindowsPathname>();

    if (stringp(p->m_device))
    {
        // "\\host\shared\dir\name"
        return x;
    }

    if (characterp(p->m_device))
    {
        // "x:/dir/name"
        if (consp(p->m_directory) && Kabsolute == car(p->m_directory))
        {
            return x;
        }
    }

    Val const s = namestring(x);

    char16  wsz[MAX_PATH];
    char16* pwszFile;
    DWORD const cwch = ::GetFullPathNameW(
        s->StaticCast<SimpleString>()->GetStart(),
        lengthof(wsz),
        wsz,
        &pwszFile );
    if (0 == cwch)
    {
        platform_error("GetFullPathName");
    }

    Int iIndex;
    return parsePathname(p->m_host, wsz, cwch, &iIndex);
} // truename

// [U]
/// <summary>
///   Make string representation of specified pathname component.
/// </summary>
/// <param name="x">A windows-pathname</param>
/// <param name="which">A component name</param>
/// <param name="s">An output stream to receive result.</param>
defmethod(unparse_pathname, windows_pathname, (
    Val const x,
    Val const which,
    Val const s ))
{
    class Internal
    {
        public: static void PrintComp(Val const x, Val const s)
        {
            if (Kabsolute == x)
            {
                // nothing to print
            }
            else if (Kback == x)
            {
                write_string("..", s);
            }
            else if (Krelative == x)
            {
                write_char('.', s);
            }
            else if (Kwild == x)
            {
                write_char('*', s);
            }
            else if (Kwild_inferior == x)
            {
                write_string("**", s);
            }
            else if (Kwild_1 == x)
            {
                write_char('?', s);
            }
            else if (consp(x))
            {
                foreach (List::Enum, oEnum, x)
                {
                    PrintComp(oEnum.Get(), s);
                } // for each elt
            }
            else if(stringp(x))
            {
                write_string(x, s);
            }
            else if (nil == x)
            {
                // nothing to print
            }
            else
            {
                print_object(x, s);
            }
        } // PrintComp
    }; // Internal

    WindowsPathname* const p = x->StaticCast<WindowsPathname>();

    if (Qnamestring == which)
    {
        if (characterp(p->m_device))
        {
            write_char(p->m_device, s);
            write_char(':', s);
        }
        else if (stringp(p->m_device))
        {
            write_char('\\', s);
            write_string(p->m_device);
        }
    } // if

    if (Qdirectory_namestring == which || Qnamestring == which)
    {
        if (consp(p->m_directory) &&
            car(p->m_directory) == Krelative &&
            cdr(p->m_directory) == nil )
        {
            // We don't print "./"
        }
        else
        {
            foreach (List::Enum, oEnum, p->m_directory)
            {
                Internal::PrintComp(oEnum.Get(), s);
                write_char('\\', s);
            } // for each comp
        }
    } // if

    if (Qfile_namestring == which || Qnamestring == which)
    {
        Internal::PrintComp(p->m_name, s);

        if (nil != p->m_type && Kunspecific != p->m_type)
        {
            write_char('.', s);
            Internal::PrintComp(p->m_type, s);
        }
    }

    return x;
} // unparse_pathname

} // TinyCl
