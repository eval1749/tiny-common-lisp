#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - Editor - Application Main
// listener/winapp/ap_main.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_main.cpp#11 $
//
// Example options:
//  -dll vanilla.dll -image evcl3.image -multiple
//
#define DEBUG_IDLE 0

#pragma comment(lib, "comctl32.lib")

#if _WIN64
#pragma comment(linker, "\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
#pragma comment(linker, "\"/manifestdependency:type='Win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='X86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

#include "./peer_application.h"
#include "./peer_file_io.h"
#include "./ctrl_TabBand.h"

const char16* k_pwszTitle = L"Evita Text Editor";

HINSTANCE g_hInstance;
HINSTANCE g_hResource;

namespace Editor
{

void Genesis();
bool Initialize_Runtime();
void Initialize_Editor(bool);

namespace Peer
{

namespace
{

// <summary>
//   Standard output redirector.
//   <para>
//     Redirects standard output to a editor buffer.
//   </para>
// </summary>
class StdOutRedirector
{
    private: static StdOutRedirector* s_pRedirector;

    private: DWORD  m_dwRecieverThreadId;
    private: HANDLE m_hEvent;
    private: HANDLE m_hRead;
    private: HANDLE m_hThread;
    private: HANDLE m_hWrite;

    // ctor
    public: StdOutRedirector() :
        m_dwRecieverThreadId(::GetCurrentThreadId()),
        m_hEvent(NULL),
        m_hRead(INVALID_HANDLE_VALUE),
        m_hThread(NULL),
        m_hWrite(INVALID_HANDLE_VALUE)
    {
        ASSERT(NULL == s_pRedirector);
        s_pRedirector = this;
        ::CreatePipe(&m_hRead, &m_hWrite, NULL, 0);
        ::SetStdHandle(STD_OUTPUT_HANDLE, m_hWrite);
        m_hEvent  = ::CreateEvent(NULL, 0, 0, NULL);
        m_hThread = ::CreateThread(NULL, 0, threadProc_, this, 0, NULL);
    } // StdOutRedirector

    public: ~StdOutRedirector()
    {
        ASSERT(::GetCurrentThreadId() == m_dwRecieverThreadId);

        if (INVALID_HANDLE_VALUE != m_hWrite)
        {
            // Break pipe
            ::CloseHandle(m_hWrite);
        }

        if (INVALID_HANDLE_VALUE != m_hRead)
        {
            ::CloseHandle(m_hRead);
        }

        if (NULL != m_hEvent)
        {
            ::SetEvent(m_hEvent);
            ::CloseHandle(m_hEvent);
        }

        if (NULL != m_hThread)
        {
            ::WaitForSingleObject(m_hThread, INFINITE);
            ::CloseHandle(m_hThread);
        }
    } // ~StdOutRedirector

    // [P]
    public: static void ProcessThreadMessage(MSG* pMsg)
    {
        s_pRedirector->Write(
            reinterpret_cast<const char*>(pMsg->lParam),
            static_cast<uint>(pMsg->wParam) );
    } // ProcessThreadMessage

    // [T]
    private: DWORD threadProc()
    {
        for (;;)
        {
            char rgchBuffer[8 * 1024];

            DWORD cbRead;
            BOOL fSucceeded = ::ReadFile(
                m_hRead,
                rgchBuffer,
                sizeof(rgchBuffer),
                &cbRead,
                NULL );

            if (! fSucceeded) 
            {
                DWORD dwError = ::GetLastError();
                if (ERROR_BROKEN_PIPE == dwError)
                {
                    break;
                }
                DEBUG_PRINTF("ReadFile: %u\n", dwError);
                continue;
            }

            fSucceeded = ::PostThreadMessage(
                m_dwRecieverThreadId,
                THREAD_WM_STDOUT,
                cbRead,
                reinterpret_cast<LPARAM>(rgchBuffer) );
            if (! fSucceeded)
            {
                #if _DEBUG
                {
                    DWORD dwError = ::GetLastError();
                    DEBUG_PRINTF("PostThreadMessage: %u\n", dwError);
                }
                #endif
                continue;
            }

            if (WAIT_FAILED == ::WaitForSingleObject(m_hEvent, INFINITE))
            {
                #if _DEBUG
                {
                    DWORD dwError = ::GetLastError();
                    DEBUG_PRINTF("WaitForSingleObject: %u\n", dwError);
                }
                #endif
                break;
            }
        } // for

        return 0;
    } // threadProc

    private: static DWORD WINAPI threadProc_(void* pv)
        { return reinterpret_cast<StdOutRedirector*>(pv)->threadProc(); }

    // [W]
    public: void Write(
        const char* pchBuffer,
        uint        cchBuffer )
    {
        ASSERT(::GetCurrentThreadId() == m_dwRecieverThreadId);

        const char* pchStart = pchBuffer;
        const char* pchEnd   = pchStart + cchBuffer;
        for (const char* pch = pchStart; pch < pchEnd; pch++)
        {
            write_char(Character::FromCode(*pch));
        } // for pch
        ::SetEvent(m_hEvent);
    } // Continue
}; // StdOutRedirector

} // namespace

HWND g_hwndActiveDialog;
StdOutRedirector* StdOutRedirector::s_pRedirector = NULL;

void DispatchThreadMessage(MSG* pMsg)
{
    switch (pMsg->message)
    {
    case THREAD_WM_GENESIS:
        Genesis();
        break;

    case THREAD_WM_STDOUT:
        StdOutRedirector::ProcessThreadMessage(pMsg);
        break;
    } // switch message
} // DispatchThreadMessage

static int mainLoop()
{
    // FIXME 2008-01-14 yosi@msn.com We should use *image-save-time*
    // instead fo fHasImage.
    bool fHasImage = Initialize_Runtime();

    new Application();
    new IoManager();

    IoManager::Get()->Realize();

    TabBand__Init(g_hInstance);

    Initialize_Editor(fHasImage);

    if (! fHasImage)
    {
        ::PostMessage(NULL, THREAD_WM_GENESIS, 0, 0);
    }

    StdOutRedirector oStdOutRedirector;

    int iIdle = 1;

    for (;;)
    {
        MSG oMsg;
        if (! ::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
        {
            bool fGotMessage = false;

            if (0 != iIdle)
            {
                #if DEBUG_IDLE
                {
                    DEBUG_PRINTF("idle %d msg=0x%x qs=0x%0x\n", 
                        iIdle, oMsg.message, ::GetQueueStatus(QS_ALLEVENTS) );
                }
                #endif // DEBUG_IDLE

                uint nCount = 0;
                for (;;)
                {
                    if (! Application::Get()->OnIdle(nCount))
                    {
                        break;
                    }

                    if (::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
                    {
                        fGotMessage = true;
                        break;
                    }

                    nCount += 1;
                } // for
            }

            iIdle = 0;

            if (! fGotMessage)
            {
                ::GetMessage(&oMsg, NULL, 0, 0);
            }
        } // if

        if (WM_QUIT == oMsg.message)
        {
            return static_cast<int>(oMsg.wParam);
        }

        if (NULL != g_hwndActiveDialog)
        {
            if (::IsDialogMessage(g_hwndActiveDialog, &oMsg))
            {
                continue;
            }
        }

        if (NULL == oMsg.hwnd)
        {
            DispatchThreadMessage(&oMsg);
            continue;
        }

        ::TranslateMessage(&oMsg);
        ::DispatchMessage(&oMsg);

        switch (oMsg.message)
        {
        case WM_PAINT:
        case 0x118: // WM_SYSTIME for bliking caret
            break;

        default:
            iIdle = 1;
            break;
        } // switch messgae

    } // for
} // mainLoop

void HandleMessage()
{
    class Local
    {
        public: static void Dispatch(MSG* const pMsg)
        {
            for (;;)
            {
                if (NULL == pMsg->hwnd)
                {
                    DispatchThreadMessage(pMsg);
                }
                else
                {
                    ::TranslateMessage(pMsg);
                    ::DispatchMessage(pMsg);
                }

                if (! ::PeekMessage(pMsg, NULL, 0, 0, PM_REMOVE))
                {
                    break;
                }
            } // for
        } // Dispatch
    }; // Local

    MSG oMsg;
    if (::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
    {
        Local::Dispatch(&oMsg);
    }
    else
    {
        bool fIdle = true;
        uint nCount = 0;
        while (fIdle)
        {
            if (! Application::Get()->OnIdle(nCount))
            {
                fIdle = false;
            }

            if (::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
            {
                Local::Dispatch(&oMsg);
                return;
            }

            nCount += 1;
        } // while
    } // if
} // HandleMessage

} // Peer
} // Editor

using namespace Editor::Peer;

//////////////////////////////////////////////////////////////////////
//
// WinMain
//
extern "C" int WINAPI WinMain(
    HINSTANCE hInstance,
    HINSTANCE,
    LPSTR,
    int )
{
    g_hInstance = hInstance;
    g_hResource = hInstance;

    int iRet = mainLoop();
    return iRet;
} // WinMain

#if NDEBUG
// See crt/src/crtexe.c
extern "C" void __cdecl WinMainCRTStartup()
{
    HINSTANCE hInstance = ::GetModuleHandle(NULL);
    ASSERT(NULL != hInstance);
    int iRet = WinMain(hInstance, NULL, NULL, 0);
    ::ExitProcess(iRet);
} // WinMainCRTStartup

#endif
