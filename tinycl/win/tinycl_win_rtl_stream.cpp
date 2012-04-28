#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime for Windows
// tinycl_windows.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/win/tinycl_win_rtl_stream.cpp#7 $
//
#include "./tinycl_win.h"

namespace TinyCl
{

using namespace Private;

//////////////////////////////////////////////////////////////////////
//
// FileStreamImpl
//
class FileStreamImpl : public StreamImpl
{
    protected: HANDLE     m_h;

    // ctor
    public: FileStreamImpl(HANDLE hFile) :
        m_h(hFile) {}

    // [C]
    public: virtual Val Close(Val, Val) override
    {
        if (INVALID_HANDLE_VALUE != m_h)
        {
            ::CloseHandle(m_h);
            return t;
        }
        return nil;
    } // Close
}; // FileStreamImpl

/// <summary>
///   Implemenation of character input file stream for Windows platform.
/// </summary>
class CharInStreamImpl : public FileStreamImpl
{
    enum { BufSize = 4906 };

    private: Val        m_line_column;
    private: Val        m_line_number;
    private: Val        m_unread;

    private: int        m_cb;
    private: int        m_cwch;
    private: char16*    m_pwch;
    private: char       m_rgch[BufSize];
    private: char16     m_rgwch[BufSize];

    public: CharInStreamImpl(HANDLE hFile) :
        FileStreamImpl(hFile),
        m_cb(0),
        m_cwch(0),
        m_pwch(m_rgwch),
        m_line_column(zero),
        m_line_number(zero),
        m_unread(nil) {}

    // [F]
    private: bool fill()
    {
        if (0 != m_cb)
        {
            CopyMemory(m_rgch, m_rgch + sizeof(m_rgch) - m_cb, m_cb);
        }

        DWORD cbRead;
        BOOL fSucceeded = ::ReadFile(
            m_h,
            m_rgch + m_cb,
            static_cast<DWORD>(lengthof(m_rgch) - m_cb),
            &cbRead,
            NULL );
        unless (fSucceeded) return false;
        when (0 == cbRead) return m_cb > 0;
        m_cb += cbRead;
        int cb = m_cb;
        for (;;)
        {
            int cwch = ::MultiByteToWideChar(
                CP_UTF8,
                MB_ERR_INVALID_CHARS,
                m_rgch,
                cb,
                m_rgwch,
                lengthof(m_rgwch) );
            if (cwch >= 1)
            {
                CopyMemory(m_rgch, m_rgch + cb, sizeof(m_rgch) - cb);
                m_cb -= cb;
                m_cwch += cwch;
                break;
            }

            if (1 == cb)
            {
                // Can't translate byte into Unicode
                return false;
            }

            {
                DWORD dwError = ::GetLastError();
                if (ERROR_NO_UNICODE_TRANSLATION != dwError)
                {
                    return false;
                }
            }

            cb -= 1;
        } // for

        m_pwch = m_rgwch;
        return true;
    } // fill

    // [G]
    public: virtual Val GetLineColumn(Val) override
        { return m_line_column; }

    public: virtual Val GetLineNumber(Val) override
        { return m_line_number; }

    // [R]
    public: virtual Val ReadChar(Val stream) override
    {
        if (characterp(m_unread))
        {
            Val ch = m_unread;
            m_unread = nil;
            return ch;
        }

        Val ch = readChar(stream);
        if (CHAR_Return == ch)
        {
            ch = readChar(stream);
            if (CHAR_Newline != ch)
            {
                m_unread = ch;
                ch = CHAR_Return;
            }
        }

        if (CHAR_Newline == ch)
        {
            m_line_number = xxadd(m_line_number, one);
            m_line_column = zero;
        }
        else
        {
            m_line_column = xxadd(m_line_column, one);
        }
        return ch;
    } // ReadChar

    private: Val readChar(Val)
    {
        if (0 == m_cwch)
        {
            unless (fill()) return Keof;
        }

        m_cwch -= 1;
        return Character::FromCode(*m_pwch++);
    } // readChar

    // [U]
    public: virtual void UnreadChar(Val stream, Val ch) override
    {
        ASSERT(characterp(ch));

        when (characterp(m_unread))
        {
            error(make_string(L"Too many unread."), Kstream, stream);
        }
        m_unread = ch;
    } // UnreadChar
}; // CharInStreamImpl

/// <summary>
///   Implemenation of character output file stream for Windows platform.
/// </summary>
class CharOutStreamImpl : public FileStreamImpl
{
    private: enum { BufSize = 4906 };

    private: Val    m_line_column;
    private: Val    m_line_number;

    private: int    m_cch;
    private: char   m_rgch[BufSize];

    // ctor
    public: CharOutStreamImpl(HANDLE hFile) :
        FileStreamImpl(hFile),
        m_line_column(zero),
        m_line_number(zero),
        m_cch(0) {}

    // [C]
    public: Val Close(Val stream, Val abort)
    {
        if (INVALID_HANDLE_VALUE != m_h)
        {
            ForceOutput(stream);
        }

        return FileStreamImpl::Close(stream, abort);
    } // Close

    // [F]
    public: virtual void ForceOutput(Val stream) override
    {
        const char* pch = m_rgch;

        while (0 != m_cch)
        {
            DWORD cbWritten;
            BOOL fSucceeded = ::WriteFile(
                m_h,
                pch,
                m_cch,
                &cbWritten,
                NULL );
            if (! fSucceeded)
            {
                DWORD dwError = ::GetLastError();
                error(Qplatform_error,
                    Kcode,      MakeUInt(dwError),
                    Koperation, Qforce_output,
                    Koperands,  list(stream) );
            }

            if (0 == cbWritten)
            {
                m_cch = 0;
                return;
            }

            m_cch -= cbWritten;
            pch   += cbWritten;
        }  // while
    } // ForceOutput

    // [G]
    public: virtual Val GetLineColumn(Val) override
        { return m_line_column; }

    public: virtual Val GetLineNumber(Val) override
        { return m_line_number; }

    // [W]
    public: virtual void WriteString(
        Val             stream,
        const char16*   pwchIn,
        int             cwchIn ) override
    {
        const char16* pwchStart = pwchIn;
        const char16* pwchEnd   = pwchIn + cwchIn;
        const char16* pwch;
        for (pwch = pwchStart; pwch < pwchEnd; pwch++)
        {
            switch (*pwch)
            {
            case 0x0A:
                writeString(stream, pwchStart, pwch - pwchStart);
                writeString(stream, L"\x0D\x0A", 2);
                pwchStart = pwch + 1;

                m_line_number = xxadd(m_line_number, one);
                m_line_column = zero;
                break;

            case 0x09:
                m_line_column = xxadd(
                    m_line_column,
                    8 - (Fixnum::Decode_(m_line_column) + 1) % 8 );
                break;

            default:
                m_line_column = xxadd(m_line_column, one);
                break;
            } // switch char
        } // for each char

        writeString(stream, pwchStart, pwch - pwchStart);
    } // WriteString

    private: void writeString(
        Val             stream,
        const char16*   pwch,
        size_t          cwchIn )
    {
        while (cwchIn >= 1)
        {
            if (m_cch + 10 > lengthof(m_rgch))
            {
                ForceOutput(stream);
            }

            int cwch = static_cast<int>(cwchIn);

            for (;;)
            {
                int cch = ::WideCharToMultiByte(
                    CP_UTF8,
                    0,
                    pwch,
                    cwch,
                    m_rgch + m_cch,
                    lengthof(m_rgch) - m_cch,
                    NULL,
                    NULL );
                if (cch >= 1)
                {
                    m_cch  += cch;
                    pwch   += cwch;
                    cwchIn -= cwch;
                    break;
                }

                DWORD dwError = ::GetLastError();
                if (ERROR_INSUFFICIENT_BUFFER != dwError)
                {
                    DWORD dwError = ::GetLastError();
                    error(Qplatform_error,
                        Kcode,      MakeUInt(dwError),
                        Koperation, Qwrite_string,
                        Koperands,  list(stream) );
                }

                cwch -= 1;
            } // for
        } // while
    } // write_string
}; // CharOutStreamImpl

class ConsoleStreamImpl : public StreamImpl
{
    private: bool   m_fEol;
    private: HANDLE m_hInput;
    private: HANDLE m_hOutput;

    private: Val m_unread;

    private: enum { MaxWrite = 1024 };

    // ctor
    public: ConsoleStreamImpl() :
        m_fEol(true),
        m_hInput(openConIn()),
        m_hOutput(openConOut()),
        m_unread(nil) {}

    // [F]
    public: virtual void ForceOutput(Val) override {}

    // [G]
    public: virtual Val GetLineColumn(Val) override
    {
        CONSOLE_SCREEN_BUFFER_INFO oInfo;
        if (! ::GetConsoleScreenBufferInfo(m_hOutput, &oInfo)) return nil;
        return Fixnum::Encode(oInfo.dwCursorPosition.X);
    } // GetLineColumn

    public: virtual Val GetLineNumber(Val) override
        { return nil; }

    // [O]
    private: static HANDLE openConIn()
    {
        HANDLE const hInput = ::CreateFileW(
            L"CONIN$",
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_WRITE | FILE_SHARE_READ,
            NULL,
            OPEN_EXISTING,
            0,
            NULL );
        // Note: Win App doesn't have CONIN$ until call AllocConsole.
        ASSERT(INVALID_HANDLE_VALUE != hInput);
        return hInput;
    } // openConIn

    private: static HANDLE openConOut()
    {
        HANDLE const hOutput = ::CreateFileW(
            L"CONOUT$",
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_WRITE | FILE_SHARE_READ,
            NULL,
            OPEN_EXISTING,
            0,
            NULL );
        // Note: Win App doesn't have CONOUT$ until call AllocConsole.
        ASSERT(INVALID_HANDLE_VALUE != hOutput);
        return hOutput;
    } // openConIn

    // [R]
    public: virtual Val ReadChar(Val stream) override
    {
        if (characterp(m_unread))
        {
            Val const ch = m_unread;
            m_unread = nil;
            return ch;
        }

        Val ch = readChar(stream);
        if (CHAR_Return == ch)
        {
            ch = readChar(stream);
            if (CHAR_Newline != ch)
            {
                m_unread = ch;
                ch = CHAR_Return;
            }
        }

        return ch;
    } // ReadChar

    private: Val readChar(Val)
    {
        m_fEol = false;

        for (;;)
        {
            char16 wch;
            DWORD cwch;
            BOOL const fSucceeded = ::ReadConsoleW(
                m_hInput,
                &wch,
                1,
                &cwch,
                NULL );
            if (! fSucceeded)
            {
                continue;
            }

            if (0 == cwch)
            {
                // for Ctrl+C
                continue;
            }

            if (0x1A == wch && 1 == cwch)
            {
                return Keof;
            }

            m_fEol = 0x0A == wch;
            return Character::FromCode(wch);
        } // for
    } // readChar

    // [W]
    public: virtual void WriteString(
        Val,
        const char16* pwchIn,
        int           cwchIn ) override
    {
        const char16* const pwchEnd = pwchIn + cwchIn;

        const char16* pwch = pwchIn;
        while (pwch < pwchEnd)
        {
            DWORD cwchWritten;
            BOOL const fSucceeded = ::WriteConsoleW(
                m_hOutput,
                pwch,
                static_cast<uint>(min(pwchEnd - pwch, MaxWrite)),
                &cwchWritten,
                NULL );
            if (! fSucceeded || 0 == cwchWritten)
            {
                break;
            }
            pwch += cwchWritten;
        } // while
    } // WriteString

    // [U]
    public: virtual void UnreadChar(Val stream, Val ch) override
    {
        ASSERT(characterp(ch));

        when (characterp(m_unread))
        {
            error(make_string(L"Too many unread."), Kstream, stream);
        }
        m_unread = ch;
    } // UnreadChar
}; // ConsoleStreamImpl

class ConsoleStream : 
    public Instance_<ConsoleStream, Layout_console_stream>
{
    public: static Val Class_() { return CLASS_console_stream; }

    // [I]
    private: static Val internCharset(uint nCp)
    {
        {
            Val const charset = gethash(
                Fixnum::Encode(nCp),
                VAR(Acharset_tableA) );

            if (nil != charset)
            {
                return charset;
            }

        }

        {
            Val const cp = Fixnum::Encode(nCp);
            char sz[20];
            ::wsprintfA(sz, "CP%u", nCp);
            Val name = intern(sz, PKG_keyword);
            Val const charset = Thread::Get()->AllocRecord(CLASSD_charset);
            Charset* const p = charset->StaticCast<Charset>();
            p->m_name      = name;
            p->m_code_page = cp;
            p->m_mime_name = nil;

            setf_gethash(charset, cp, VAR(Acharset_tableA));
            
            return charset;
        }
    } // internCharset

    // [M]
    public: static Val Make()
    {
        if (NULL == ::GetConsoleWindow())
        {
            return nil;
        }

        if (437 == ::GetConsoleCP())
        {
            ::SetConsoleCP(1252);
        }

        Val const extfmt =
            Thread::Get()->AllocRecord(CLASSD_external_format);
        {
            ExternalFormat* const p = extfmt->StaticCast<ExternalFormat>();

            p->m_charset = internCharset(::GetConsoleCP());
            p->m_eol = Klf;
        }

        Val const stream =
            Thread::Get()->AllocInstance(CLASSD_console_stream);

        ConsoleStream* const p = stream->StaticCast<ConsoleStream>();

        p->m_blob = Fixnum::Encode(new ConsoleStreamImpl);

        p->m_external_format = extfmt;

        p->m_flags = Fixnum::Encode(
            StreamFlag_Io | StreamFlag_Interactive );

        return stream;
    } // Make
}; // ConsoleStream

// [M]
Val make_file_stream(HANDLE hFile, Val flags)
{
    class Local
    {
        public: static BOOL IsConsoleHandle(HANDLE const hFile)
        {
            DWORD dwMode;
            return ::GetConsoleMode(hFile, &dwMode);
        } // IsConsoleHandle
    }; // Local

    if (Local::IsConsoleHandle(hFile))
    {
        return TLV(Aterminal_ioA);
    }

    StreamImpl* pStreamImpl;

    if (StreamFlag_Output & Fixnum::Decode_(flags))
    {
        pStreamImpl = new CharOutStreamImpl(hFile);
    }
    else
    {
        pStreamImpl = new CharInStreamImpl(hFile);
    }

    Val const stream = Thread::Get()->AllocInstance(CLASSD_file_stream);
    FileStream* const p = stream->StaticCast<FileStream>();
    p->m_flags = flags;
    p->m_blob  = Fixnum::Encode(pStreamImpl);

    return stream;
} // make_file_stream

// [P]
void PlatformStreamRestart()
{
    TLV(Aterminal_ioA) = ConsoleStream::Make();

    {
        Val const tlvrec = svref(
            VAR(Atlv_vectorA),
            Fixnum::Encode(TLV_Aterminal_ioA) );

        tlvrec->StaticCast<TlvRecord>()->m_value = 
            TLV(Aterminal_ioA);
    }
} // PlatformStreamRestart

namespace CommonLisp
{

// [C]
defmethod(close, platform_stream, (Thread* pth))
{
    Val const stream = pth->mv_value[0];
    Val abort  = nil;

    KeyArg rgoKey[] =
    {
        KEYARG(abort),
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    PlatformStream* const pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* const p = pPlatform->GetImpl();
    return p->Close(stream, abort);
} // close

// [O]
defun(openV, (Thread* pth))
{
    Val filething         = pth->mv_value[0];
    Val direction         = Kinput;
    Val external_format   = Kdefault;
    Val if_does_not_exist = MARKER_unbound;
    Val if_exists         = Kerror;

    KeyArg rgoKey[] =
    {
        KEYARG(direction),
        KEYARG(external_format),
        KEYARG(if_does_not_exist),
        KEYARG(if_exists),
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    Val filename = pathname(filething);

    if (Kdefault == external_format)
    {
        external_format = TLV(Aexternal_formatA);
    }

    check_type(external_format, external_format);

    DWORD dwAccess;
    DWORD dwShare;
    Val   flags;

    if (Kinput == direction)
    {
        dwAccess = GENERIC_READ;
        dwShare  = FILE_SHARE_READ;
        flags    = Fixnum::Encode(StreamFlag_Input);

        if (MARKER_unbound == if_does_not_exist)
        {
            if_does_not_exist = Kerror;
        }
    }
    else if (Koutput == direction)
    {
        dwAccess = GENERIC_WRITE;
        dwShare  = FILE_SHARE_READ;
        flags    = Fixnum::Encode(StreamFlag_Output);

        if (MARKER_unbound == if_does_not_exist)
        {
            if (Koverwrite == if_exists || Kappend == if_exists)
            {
                if_does_not_exist = Kerror;
            }
            else
            {
                if_does_not_exist = Kcreate;
            }
        }
    }
    else if (Kio == direction)
    {
        dwAccess = GENERIC_READ | GENERIC_WRITE;
        dwShare  = FILE_SHARE_READ;
        flags    = Fixnum::Encode(StreamFlag_Io);

        if (MARKER_unbound == if_does_not_exist)
        {
            if (Koverwrite == if_exists || Kappend == if_exists)
            {
                if_does_not_exist = Kerror;
            }
            else
            {
                if_does_not_exist = Kcreate;
            }
        }
    }
    else if (Kprobe == direction)
    {
        dwAccess = 0;
        dwShare  = FILE_SHARE_READ;
        flags    = Fixnum::Encode(StreamFlag_Probe);

        if (MARKER_unbound == if_does_not_exist)
        {
            if_does_not_exist = nil;
        }
    }
    else
    {
        goto error_bad_direction;
    }

    DWORD dwCreate;
    {
        if (0 == (dwAccess & GENERIC_WRITE))
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = OPEN_ALWAYS;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Kerror == if_exists)
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = CREATE_NEW;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Knew_version == if_exists)
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = CREATE_NEW;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Koverwrite == if_exists ||
                 Ksupersede == if_exists )
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = CREATE_ALWAYS;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Kappend == if_exists)
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = OPEN_ALWAYS;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Krename == if_exists || Krename_and_delete == if_exists)
        {
            // We've already renamed existing file.
            dwCreate = CREATE_NEW;
        }
        else
        {
            goto error_bad_if_exists;
        }
    } // dwCreate

    HANDLE hFile;
    {
        char16* pwszFilename = namestring(filename)->
            StaticCast<SimpleString>()->GetStart();

        hFile = ::CreateFileW(
            pwszFilename,
            dwAccess,
            dwShare,
            NULL,
            dwCreate,
            0,
            NULL );
        if (INVALID_HANDLE_VALUE == hFile)
        {
            DWORD dwError = ::GetLastError();

            switch (dwError)
            {
            case ERROR_FILE_NOT_FOUND:
                if (nil == if_does_not_exist)
                {
                    return nil;
                }

                error(Qfile_not_found, Kpathname, filename);
                // NOTREACHED

            case ERROR_PATH_NOT_FOUND:
                error(Qpath_not_found, Kpathname, filename);
                // NOTREACHED
            } // switch dwError

            error(Qplatform_error,
                    Kcode,      MakeUInt(dwError),
                    Koperation, Qopen,
                    Koperands,  list(filename) );
            // NOTREACHED
        } // if
    } // hFile

    Val stream = make_file_stream(hFile, flags);
    {
        FileStream* p = stream->StaticCast<FileStream>();
        p->m_external_format = external_format;
        p->m_pathname = filename;
    }

    // FIXME 2007-09-09 yosi@msn.com We must schedule finalization for
    // file stream.
    // schedule_finalization(stream, Qclose);

    if (Kappend == if_exists)
    {
        DWORD dwPos = ::SetFilePointer(
            hFile,
            0,
            NULL,
            FILE_END );
        if (INVALID_SET_FILE_POINTER == dwPos)
        {
            DWORD dwError = ::GetLastError();
            error(Qplatform_error,
                Kcode,      MakeUInt(dwError),
                Koperation, list(Qsetf, Qfile_position),
                Koperands,  list(stream) );
            // NOTREACHED
        }
    }

    return stream;

  error_bad_direction:
    SignalTypeError(
        direction,
        list(Qmember, Kinput, Kio, Koutput, Kprobe, nil) );
    // NOTREACHED

  error_bad_if_exists:
    SignalTypeError(
        if_exists,
        list(Qmember, Kappend, Kerror, Knew_version, Koverwrite,
            Krename, Krename_and_delete, Ksupersede, nil ) );
    // NOTREACHED

  error_bad_if_does_not_exist:
    SignalTypeError(
        if_does_not_exist,
        list(Qmember, Kerror, Kcreate, nil ) );
    // NOTREACHED
} // open

} // CommonLisp
} // TinyCl
