#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - File I/O Peer
// editor/peer/peer_file_io.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_file_io.cpp#7 $
//
#define DEBUG_LOAD 1
#define DEBUG_SAVE 1
#include "./peer_file_io.h"

#include "../ed_buffer.h"

namespace Editor
{

namespace Peer
{

static const DWORD k_cbHugeFile = Fixnum::MostPositive >> 1;

IoManager* IoManager::sm_pThis;

DWORD  IoRequest::sm_nThreadId;
HANDLE IoRequest::sm_hIoCompletionPort;
HANDLE IoRequest::sm_hThread;

namespace
{

static const char* findChar(
    const char* pchStart,
    const char* pchEnd,
    char        ch )
{
    for (const char* pch = pchStart; pch < pchEnd; pch++)
    {
        if (*pch == ch)
        {
            return pch;
        }
    } // for
    return NULL;
} // findChar

} // namespace

// ctor
BufferRequest::BufferRequest(
    Val             buffer,
    const char16*   pwszFile,
    Val             extfmt ) :
    m_buffer(buffer)
{
    ::lstrcpyW(m_wszFileName, pwszFile);

    ExternalFormat* pExtFmt = extfmt->StaticCast<ExternalFormat>();

    m_eNewline =
        Kcr   == pExtFmt->m_eol ? NewlineMode_Cr :
        Kcrlf == pExtFmt->m_eol ? NewlineMode_CrLf :
        Klf   == pExtFmt->m_eol ? NewlineMode_Lf :
                                  NewlineMode_Detect;

    if (Charset* p = pExtFmt->m_charset->DynamicCast<Charset>())
    {
        m_nCodePage = Fixnum::Decode_(p->m_code_page);
    }
    else
    {
        m_nCodePage = ::GetACP();
    }
} // BufferRequest::BufferRequest

CheckRequest::CheckRequest(Val buffer) :
    BufferRequest(
        buffer,
        buffer->StaticCast<Buffer>()->GetFileName(),
        buffer->StaticCast<Buffer>()->m_external_format ) {}

SaveRequest::SaveRequest(Val buffer, const char16* pwszFile, Val extfmt) :
    BufferRequest(buffer, pwszFile, extfmt)
{
    m_end  = buffer->StaticCast<Buffer>()->GetEnd();
    m_posn = zero;
} // SaveRequest::SaveRequest

// [A]
void IoManager::AppendString(Val buffer, const char16* pwch, int cwch)
{
    InsertString(
        buffer,
        buffer->StaticCast<BufferFile>()->GetEnd(),
        pwch,
        cwch );
} // IoManager::AppendString

bool IoRequest::associate(HANDLE hFile)
{
    HANDLE hPort = ::CreateIoCompletionPort(
        hFile,
        sm_hIoCompletionPort,
        reinterpret_cast<ULONG_PTR>(this),
        0 );

    if (hPort != sm_hIoCompletionPort)
    {
        error("CreateIoCompletionPort");
        return false;
    } // if

    return true;
} // IoRequest::associate

// [D]
NewlineMode LoadRequest::detectNewline(
    const char* pchStart,
    const char* pchEnd )
{
    ASSERT(pchStart < pchEnd);

    switch (m_eCrLf)
    {
    case NewlineMode_Cr:
        return 0x0A == *pchStart ? NewlineMode_CrLf : NewlineMode_Cr;

    case NewlineMode_Detect:
        for (const char* pch = pchStart; pch < pchEnd; pch++)
        {
            switch (m_eCrLf)
            {
            case NewlineMode_Cr:
                // Reset m_eCrLf
                m_eCrLf = NewlineMode_CrLf;
                return 0x0A == *pch ? NewlineMode_CrLf :  NewlineMode_Cr;

            case NewlineMode_Detect:
                switch (*pch)
                {
                case 0x0A:
                    return NewlineMode_Lf;

                case 0x0D:
                    m_eCrLf = NewlineMode_Cr;
                    break;
                } // switch
                break;

            default:
                CAN_NOT_HAPPEN();
            }
        } // for
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch m_eCrLf

    return NewlineMode_Detect;
} // detecteNewline

// [E]
void CheckRequest::End()
{
    ASSERT(! isIoThread());

    Buffer* pBuffer = m_buffer->StaticCast<Buffer>();
    Val file_write_date = pBuffer->m_file_write_date;
    if (updateFileState())
    {
        if (! eql(pBuffer->m_file_write_date, file_write_date ))
        {
            pBuffer->m_file_state = Kobsolete;
        }
    } // if
} // CheckRequest::End

void IoManager::InsertRequest::End()
{
    m_buffer->StaticCast<Buffer>()->InternalInsert(m_posn, m_pwch, m_cwch);
} // IoManager::InsertRequest::End

void LoadRequest::End()
{
    ASSERT(! isIoThread());

    Buffer* pBuffer = m_buffer->StaticCast<Buffer>();
    pBuffer->m_file_state = nil;

    if (updateFileState())
    {
        pBuffer->SetFileName(m_wszFileName);

        // Reset modified flag
        pBuffer->m_char_tick = zero;
    } // if
} // LoadRequest::End

void SaveRequest::End()
{
    ASSERT(! isIoThread());

    BufferFile* pBuffer = m_buffer->StaticCast<BufferFile>();
    pBuffer->m_file_state = nil;

    if (updateFileState())
    {
        pBuffer->SetFileName(m_wszFileName);

        // Reset modified flag
        pBuffer->m_char_tick = zero;
    } // if
} // SaveRequest::End

// [F]
void CheckRequest::finishIo()
{
    ASSERT(isIoThread());

    #if DEBUG_LOAD
        DEBUG_PRINTF("%p err=%u %ls\n", this, m_nError, m_wszFileName);
    #endif

    IoManager::Get()->Send(IoManager::Message_CheckRequest, this);

    delete this;
} // CheckRequest::finishIo

void LoadRequest::finishIo()
{
    ASSERT(isIoThread());

    #if DEBUG_LOAD
        DEBUG_PRINTF("%p err=%u %ls\n", this, m_nError, m_wszFileName);
    #endif
    
    if (ERROR_HANDLE_EOF == m_nError)
    {
        m_nError = 0;
    }

    IoManager::Get()->Send(IoManager::Message_LoadRequest, this);

    delete this;
} // LoadRequest::finishIo

void SaveRequest::finishIo()
{
    ASSERT(isIoThread());

    #if DEBUG_LOAD
        DEBUG_PRINTF("%p op=%s err=%u %ls\n",
            this, m_pszOperation, m_nError, m_wszFileName);
    #endif

    if (INVALID_HANDLE_VALUE != m_hFile && 0 == m_nError)
    {
        {
            LARGE_INTEGER li;
            li.HighPart = 0;
            li.LowPart  = m_cbFile;
            if (! ::SetFilePointerEx(m_hFile, li, NULL, FILE_BEGIN))
            {
                error("SetFilePointerEx");
                return;
            } // if

            if (! ::SetEndOfFile(m_hFile))
            {
                error("SetEndOfFile");
                return;
            } // if
        } // if

        ::CloseHandle(m_hFile);
        m_hFile = INVALID_HANDLE_VALUE;

        {
            BOOL fSucceeded = ::MoveFileEx(
                m_wszTempName,
                m_wszFileName,
                MOVEFILE_REPLACE_EXISTING );

            if (! fSucceeded)
            {
                error("MoveFileEx");
                return;
            } // if
        }

        openForLoad();
    } // if

    ::DeleteFile(m_wszTempName);

    IoManager::Get()->Send(IoManager::Message_SaveRequest, this);

    delete this;
} // SaveRequest::finishIo

// [I]
void LoadRequest::insertLine(
    const char* pchBol,
    const char* pchEol )
{
    const char* pch = pchBol;
    const char* pchMid = pchEol;
    while (pch < pchEol)
    {
        int cwch = ::MultiByteToWideChar(
            m_nCodePage,
            MB_ERR_INVALID_CHARS,
            pch,
            static_cast<int>(pchMid - pch),
            m_rgwch,
            lengthof(m_rgwch) );
        if (cwch >= 1)
        {
            IoManager::Get()->AppendString(m_buffer, m_rgwch, cwch);

            if (pchMid == pchEol)
            {
                break;
            } // if

            pch = pchMid;
            char16 wch = *pch & 0xFF;
            pch++;

            IoManager::Get()->AppendString(m_buffer, &wch, 1);

            pchMid = pchEol;
        }
        else
        {
            pchMid -= 1;
            if (pch == pchMid)
            {
                char16 wch = *pch & 0xFF;
                pch++;
                IoManager::Get()->AppendString(m_buffer, &wch, 1);
                pchMid = pchEol;
            } // if
        } // if
    } // while

    char16 wchNewline = 0x0A;
    IoManager::Get()->AppendString(m_buffer, &wchNewline, 1);
} // LoadRequest::isnertLine

// [O]
void LoadRequest::onEvent(uint cbRead)
{
    #if DEBUG_LOAD
        DEBUG_PRINTF("%p cb=%u @%u  %ls\n",
            this, cbRead, m_oOverlapped.Offset, m_wszFileName );
    #endif

    if (INVALID_HANDLE_VALUE == m_hFile)
    {
        if (! openForLoad())
        {
            finishIo();
            return;
        }

        if (associate(m_hFile))
        {
            requestRead(0);
        }

        return;
    } // if

    // Advance to next read position
    m_oOverlapped.Offset += cbRead;

    cbRead += m_cbHead;
    m_cbHead = 0;

    // Do we reach end of file?
    if (0 == cbRead)
    {
        finishIo();
        return;
    }

    const char* pchEnd   = m_rgchIoBuffer + cbRead;
    const char* pchStart = m_rgchIoBuffer;

    if (NewlineMode_Detect == m_eNewline)
    {
        m_eNewline = detectNewline(pchStart, pchEnd);
    }

    switch (m_eNewline)
    {
    case NewlineMode_Detect:
        processPartialLine(pchStart, pchEnd);
        break;

    case NewlineMode_Cr:
        processLine(pchStart, pchEnd, 0x0D);
        break;

    case NewlineMode_CrLf:
        // Does last character of last read is CR?
        if (NewlineMode_Cr == m_eCrLf)
        {
            // Skip first LF continued from last read.
            if (0x0A == *pchStart)
            {
                pchStart++;
            }
        } // if

        processLineCrLf(pchStart, pchEnd);
        break;

    case NewlineMode_Lf:
        processLine(pchStart, pchEnd, 0x0A);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch m_eNewline
} // LoadRequest::onEvent

void SaveRequest::onEvent(uint cbWritten)
{
    #if DEBUG_SAVE
        DEBUG_PRINTF("%p cb=%u @%u %ls\n",
            this, cbWritten, m_oOverlapped.Offset, m_wszFileName );
    #endif // DEBUG_SAVE

    if (INVALID_HANDLE_VALUE == m_hFile)
    {
        {
            char16 wszTempDir[MAX_PATH + 1];
            char16* pwszFile;
            uint cwchFull = ::GetFullPathName(
                m_wszFileName,
                lengthof(wszTempDir),
                wszTempDir,
                &pwszFile );
            if (0 == cwchFull)
            {
                error("GetFullPathName");
                return;
            } // if

            *pwszFile = 0;
            uint cwchTemp = ::GetTempFileName(
                wszTempDir,
                L"ed",
                0,
                m_wszTempName );
            if (0 == cwchTemp)
            {
                error("GetTempName");
                return;
            } // if
        }

        m_hFile = ::CreateFile(
            m_wszTempName,
            GENERIC_WRITE,
            FILE_SHARE_READ | FILE_SHARE_DELETE,
            NULL,
            TRUNCATE_EXISTING,
            FILE_FLAG_OVERLAPPED | FILE_FLAG_SEQUENTIAL_SCAN,
            NULL );
        if (INVALID_HANDLE_VALUE == m_hFile)
        {
            error("CreateFile");
            return;
        } // if

        if (m_posn == m_end)
        {
            finishIo();
        }
        else if (associate(m_hFile))
        {
            retrieve();
        } // if

        return;
    } // if

    // Do we reach EOF?
    if (m_posn == m_end)
    {
        finishIo();
        return;
    } // if

    // Advance to next read position.
    m_oOverlapped.Offset += cbWritten;

    retrieve();
} // SaveRequest::onEvent

LRESULT IoManager::onMessage(uint uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    #define case_(mp_name) \
        case Message_ ## mp_name : \
            reinterpret_cast<mp_name *>(lParam)->End(); \
            return 0;

    case_(CheckRequest)
    case_(InsertRequest)
    case_(LoadRequest)
    case_(SaveRequest)

    #undef case_
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // IoManager::onMessage

bool FileIoRequest::openForLoad()
{
    ASSERT(INVALID_HANDLE_VALUE == m_hFile);

    m_hFile = ::CreateFile(
        m_wszFileName,
        GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_DELETE,
        NULL,
        OPEN_EXISTING,
        FILE_FLAG_OVERLAPPED | FILE_FLAG_SEQUENTIAL_SCAN,
        NULL );
    if (INVALID_HANDLE_VALUE == m_hFile)
    {
        m_nError       = ::GetLastError();
        m_pszOperation = "CreateFile";
        return false;
    } // if

    BY_HANDLE_FILE_INFORMATION oInfo;
    if (! ::GetFileInformationByHandle(m_hFile, &oInfo))
    {
        m_nError = ::GetLastError();
        m_pszOperation = "GetFileInformationByHandle";
        return false;
    }

    if (0 != oInfo.nFileSizeHigh || oInfo.nFileSizeLow > k_cbHugeFile)
    {
        m_nError       = ERROR_NOT_ENOUGH_MEMORY;
        m_pszOperation = "HeapAlloc";
        return false;
    } // if

    m_cbFile      = oInfo.nFileSizeLow;
    m_ftLastWrite = oInfo.ftLastWriteTime;
    m_nFileAttrs  = oInfo.dwFileAttributes;

    return true;
} // FileIoRequest::openForLoad

// [P]
void
LoadRequest::processLine(
    const char* pchStart,
    const char* pchEnd,
    char        chEol )
{
    const char* pch = pchStart;

    while (pch < pchEnd)
    {
        const char* pchEol = findChar(pch, pchEnd, chEol);
        if (NULL == pchEol)
        {
            processPartialLine(pch, pchEnd);
            return;
        }

        insertLine(pch, pchEol);
        pch = pchEol + 1;
    } // while

    requestRead(0);
} // LoadRequest::processLine

void
LoadRequest::processLineCrLf(
    const char* pchStart,
    const char* pchEnd )
{
    const char* pch = pchStart;

    m_eCrLf = NewlineMode_CrLf;

    while (pch < pchEnd)
    {
        const char* pchEol = findChar(pch, pchEnd, 0x0D);
        if (NULL == pchEol)
        {
            // Does it have only LF?
            pchEol = findChar(pch, pchEnd, 0x0A);

            if (NULL == pchEol)
            {
                m_eCrLf = NewlineMode_Detect;
                processPartialLine(pch, pchEnd);
                return;
            }
        }

        insertLine(pch, pchEol);
        pch = pchEol + 1;

        if (pch == pchEnd)
        {
            m_eCrLf = NewlineMode_Cr;
            break;
        }

        if (0x0A == *pch)
        {
            pch++;
        }
    } // while

    requestRead(0);
} // LoadRequest::processLineCrLf

void LoadRequest::processPartialLine(
    const char* pchStart,
    const char* pchEnd )
{
    const char* pch = pchStart;

    // The buffer contains partial line.
    int cbHead = 0;
    while (pch < pchEnd)
    {
        // Shirnk from tail
        const char* pchTail = pchEnd;
        do
        {
            int cwch = ::MultiByteToWideChar(

                m_nCodePage,
                MB_ERR_INVALID_CHARS,
                pch,
                static_cast<int>(pchTail - pch),
                m_rgwch,
                lengthof(m_rgwch) );
            if (cwch >= 1)
            {
                IoManager::Get()->AppendString(m_buffer, m_rgwch, cwch);
                pch = pchTail; 
                break;
            } // if

            --pchTail;
        } while (pch < pchTail);

        // Do we have incomplete MBCS?
        // FIXME 2007-08-06 yosi@msn.com We should use GetCPInfo to
        // determine maximum number of bytes.
        if (pch + 10 >= pchEnd)
        {
            cbHead = static_cast<int>(pchEnd - pch);
            ::MoveMemory(m_rgchIoBuffer, pch, cbHead);
            break;
        } // if

        // Skip one byte.
        char16 wch = *pch & 0xFF;
        pch++;

        IoManager::Get()->AppendString(m_buffer, &wch, 1);
    } // while

    requestRead(cbHead);
} // LoadRequest::processPartialLine

// [R]
void LoadRequest::requestRead(uint cbRest)
{
    ASSERT(isIoThread());

    m_cbHead = cbRest;

    BOOL fSucceeded = ::ReadFile(
        m_hFile,
        m_rgchIoBuffer + cbRest,
        k_cbIoBuffer   - cbRest,
        NULL,
        &m_oOverlapped );
    if (! fSucceeded)
    {
        uint nError = ::GetLastError();
        switch (nError)
        {
        case ERROR_HANDLE_EOF:
            finishIo();
            break;

        case ERROR_IO_PENDING:
            break;

        default:
            error("ReadFile");
            break;
        } // switch
        return;
    } // if
} // LoadRequest::requestRead

void SaveRequest::requestWrite(uint cbWrite)
{
    ASSERT(isIoThread());

    #if DEBUG_SAVE
        DEBUG_PRINTF("%p cb=%u @%u %ls\n",
            this, cbWrite, m_oOverlapped.Offset, m_wszFileName );
    #endif // DEBUG_SAVE

    m_cbFile += cbWrite;

    BOOL fSucceeded = ::WriteFile(
        m_hFile,
        m_rgchIoBuffer,
        cbWrite,
        NULL,
        &m_oOverlapped );
    if (! fSucceeded)
    {
        uint nError = ::GetLastError();
        switch (nError)
        {
        case ERROR_IO_PENDING:
            break;

        default:
            error("WriteFile");
            break;
        } // switch
        return;
    } // if
} // SaveRequest::requestWrite

void SaveRequest::retrieve()
{
    ASSERT(isIoThread());

    int cwch = m_buffer->StaticCast<Buffer>()->GetText(
        m_posn,
        xmin(m_end, xxadd(m_posn, lengthof(m_rgwch))),
        m_rgwch );

    #if _DEBUG
    {
        Buffer* pBuffer = m_buffer->StaticCast<Buffer>();
        for (Int i = 0; i < cwch; i++)
        {
            ASSERT(pBuffer->GetCharAt(xadd(m_posn, i)) == m_rgwch[i]);
        }
    }
    #endif

    const char16* pwchStart = m_rgwch;
    const char16* pwchEnd   = pwchStart + cwch;

    char* pch = m_rgchIoBuffer;
    char* pchEnd = m_rgchIoBuffer + lengthof(m_rgchIoBuffer);

    const char16* pwch;
    for (pwch = pwchStart; pwch < pwchEnd; pwch++)
    {
        // We need to have at least two byte in mbcs buffer for
        // CRLF.
        if (pch + 2 > pchEnd)
        {
            break;
        }

        if (0x0A == *pwch)
        {
            switch (m_eNewline)
            {
            case NewlineMode_Cr:
                *pch++ = 0x0D;
                break;
            

            case NewlineMode_Lf:
                *pch++ = 0x0A;
                break;

            case NewlineMode_CrLf:
                *pch++ = 0x0D;
                *pch++ = 0x0A;
                break;
                
            default:
                CAN_NOT_HAPPEN();
            }
        }
        else if (*pwch < 0x80)
        {
            *pch++ = static_cast<char>(*pwch);
        }
        else
        {
            BOOL fFailed;
            BOOL* pfFailed;
            switch (m_nCodePage)
            {
            case CP_UTF7:
            case CP_UTF8:
                // WideCharToMultiByte requires pfFailed must be NULL.
                // I guess this conversion must not be failed.
                pfFailed = NULL;
                break;

            default:
                pfFailed = &fFailed;
                break;
            } // switch code page

            // FIXME 2007-07-23 yosi@msn.com We should call WC2MB for
            // string instead of char.
            int cch = ::WideCharToMultiByte(
                m_nCodePage,
                0,
                pwch,
                1,
                pch,
                static_cast<int>(pchEnd - pch),
                NULL,
                pfFailed );
            if (0 == cch)
            {
                m_nError = ::GetLastError();
                if (ERROR_INSUFFICIENT_BUFFER == m_nError)
                {
                    m_nError = 0;
                    break;
                } // if

                error("WideCharToMultiByte");
                return;
            } // if

            pch += cch;
        } // if
    } // for

    m_posn = xadd(m_posn, pwch - pwchStart);
    requestWrite(static_cast<int>(pch - m_rgchIoBuffer));
} // SaveRequest::retrieve

// [S]
void IoRequest::Start()
{
    if (NULL == sm_hThread)
    {
        sm_hIoCompletionPort = ::CreateIoCompletionPort(
            INVALID_HANDLE_VALUE,
            NULL,
            0,
            0 );
        if (NULL == sm_hIoCompletionPort)
        {
            PlatformError("CreateIoCompletionPort");
        }

        sm_hThread = ::CreateThread(
            NULL,
            0,
            threadProc,
            NULL,
            0,
            &sm_nThreadId );
        if (NULL == sm_hThread)
        {
            PlatformError("CreateThread");
        }
    } // if

    BOOL fSucceeded = ::PostQueuedCompletionStatus(
        sm_hIoCompletionPort,
        0,
        reinterpret_cast<ULONG_PTR>(this),
        NULL );
    if (! fSucceeded)
    {
        PlatformError("PostQueuedCompletionStatus");
    }
} // IoRequest::Start

// [T]
DWORD WINAPI IoRequest::threadProc(void*)
{
    BOOL fSucceeded;

    for (;;)
    {
        DWORD cbReceived;
        ULONG_PTR ulpKey;
        OVERLAPPED* pOverlapped;
        fSucceeded = ::GetQueuedCompletionStatus(
            sm_hIoCompletionPort,
            &cbReceived,
            &ulpKey,
            &pOverlapped,
            INFINITE );

        IoRequest* pIoRequest = reinterpret_cast<IoRequest*>(ulpKey);

        if (! fSucceeded)
        {
            if (NULL != pOverlapped)
            {
                pIoRequest->error("GetQueuedCompletionStatus");
            }
            continue;
        } // if

        DEBUG_PRINTF("%p cb=%u ov=%p\n", pIoRequest, cbReceived, pOverlapped);

        pIoRequest->onEvent(cbReceived);
    } // for
} // IoRequest::threadProc

// [U]
bool BufferRequest::updateFileState()
{

    Buffer* pBuffer = m_buffer->StaticCast<Buffer>();

    pBuffer->m_state = nil;

    if (0 != m_nError)
    {
        pBuffer->m_file_state = make_platform_error(
            m_pszOperation,
            m_nError );

        return false;
    } // if

    // FIXME 2008-01-19 yosi@msn.com NYI BufferFile::SetNewline
    // FIXME 2008-01-19 yosi@msn.com NYI BufferFile::SetCodePage

    // FILETIME = 100-nanosecond since Janyary 1, 1601
    // 1ns= 10^-9s. 100 nanosecond = 10^-7
    // 1900 014f373b fde04000
    // 1970 019db1de d53e8000
    // 2000 01bf53eb 256d4000
    UInt64 ull = m_ftLastWrite.dwHighDateTime;
    ull <<= 32;
    ull |= m_ftLastWrite.dwLowDateTime;
    ull -= 0x014f373bfde04000ull;   // January 1, 1900
    ull /= 10 * 1000 * 1000;

    pBuffer->m_file_write_date = Si::MakeUInt64(ull);

    pBuffer->m_file_state  = Fixnum::Encode(
        ::GetTickCount() & Fixnum::MostPositive );

    pBuffer->m_read_only = m_nFileAttrs & FILE_ATTRIBUTE_READONLY ?
        Kread_only : nil;

    return true;
} // BufferRequest::updateFileState

} // Peer
} // Editor
