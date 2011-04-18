//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - File I/O
// editor/peer/peer_file_io.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_file_io.h#4 $
//
#if !defined(INCLUDE_editor_peer_file_io_h)
#define INCLUDE_editor_peer_file_io_h

#include "./peer_window.h"

namespace Editor
{

namespace Peer
{

enum NewlineMode
{
    NewlineMode_Detect,
    NewlineMode_Cr,
    NewlineMode_CrLf,
    NewlineMode_Lf,
}; // NewlineMode

class IoManager : public Window_<IoManager>
{
    public: static const char* Kind_() { return "IoManager"; }

    public: enum Message
    {
        Message_First = WM_USER,

        Message_CheckRequest,
        Message_InsertRequest,
        Message_LoadRequest,
        Message_SaveRequest,
    }; // Message

    private: struct InsertRequest
    {
        const char16*   m_pwch;
        int             m_cwch;
        Val             m_buffer;
        Val             m_posn;

        void End();
    }; // InsertRequest

    private: static IoManager*  sm_pThis;

    private: uint m_nThreadId;

    // ctor
    public: IoManager() :
        m_nThreadId(::GetCurrentThreadId())
    {
        ASSERT(NULL == sm_pThis);
        sm_pThis = this;
    } // IoManager

    // [A]
    public: void AppendString(Val, const char16*, int);

    // [G]
    public: static IoManager* Get() { return sm_pThis; }
    public: uint GetThreadId() const { return m_nThreadId; }

    // [I]
    public: void InsertString(
        Val             buffer,
        Val             posn,
        const char16*   pwch,
        int             cwch )
    {
        InsertRequest oParam;
        oParam.m_buffer = buffer;
        oParam.m_posn   = posn;
        oParam.m_pwch   = pwch;
        oParam.m_cwch   = cwch;

        Send(Message_InsertRequest, &oParam);
    } // InsertString

    // [O]
    protected: virtual LRESULT onMessage(uint, WPARAM, LPARAM);

    // [R]
    public: void Realize()
    {
        CreateWindowEx(0, NULL, 0, HWND_MESSAGE);
    } // Realize

    // [S]
    public: void Send(uint nMsg, void* pv)
    {
        ::SendMessage(
            m_hwnd,
            nMsg,
            0,
            reinterpret_cast<LPARAM>(pv) );
    } // Send
}; // IoManager

class IoRequest
{
    private: static DWORD  sm_nThreadId;
    private: static HANDLE sm_hIoCompletionPort;
    private: static HANDLE sm_hThread;

    protected: HANDLE       m_hFile;
    protected: uint         m_nError;
    protected: const char*  m_pszOperation;
    protected: char16       m_wszFileName[MAX_PATH + 1];

    // cotr
    protected: IoRequest() :
        m_hFile(INVALID_HANDLE_VALUE),
        m_nError(0),
        m_pszOperation(NULL)
    {
        m_wszFileName[0] = 0;
    } // IoRequest

    public: ~IoRequest()
    {
        if (INVALID_HANDLE_VALUE != m_hFile)
        {
            ::CloseHandle(m_hFile);
        } // if
    } // ~IoRequest

    // [A]
    protected: bool associate(HANDLE);

    // [E]
    protected: void error(const char* psz)
    {
        m_nError       = ::GetLastError();
        m_pszOperation = psz;
        finishIo();
    } // finishiIo

    // [F]
    protected: virtual void finishIo() = 0;

    // [I]
    protected: bool isIoThread() const
        { return ::GetCurrentThreadId() == sm_nThreadId; }

    // [O]
    protected: virtual void onEvent(uint) = 0;

    // [S]
    public: void Start();

    // [T]
    protected: static DWORD CALLBACK threadProc(void*);
}; // IoRequest

class FileIoRequest : public IoRequest
{
    protected: static const uint k_cbIoBuffer = 4096;

    protected: size_t       m_cbFile;
    protected: NewlineMode  m_eNewline;
    protected: FILETIME     m_ftLastWrite;
    protected: uint         m_nCodePage;
    protected: uint32       m_nFileAttrs;
    protected: OVERLAPPED   m_oOverlapped;
    protected: char         m_rgchIoBuffer[k_cbIoBuffer];

    protected: FileIoRequest() :
        m_cbFile(0),
        m_eNewline(NewlineMode_Detect),
        m_nCodePage(0),
        m_nFileAttrs(0)
    {
        ::ZeroMemory(&m_ftLastWrite, sizeof(m_ftLastWrite));
        ::ZeroMemory(&m_oOverlapped, sizeof(m_oOverlapped));
    } // FileIoIoRequest

    // [O]
    protected: bool openForLoad();
}; // FileIoRequest

class BufferRequest : public FileIoRequest
{
    protected: Val m_buffer;

    // ctor
    protected: BufferRequest(Val buffer, const char16* pwszFile, Val extfmt);

    // [U]
    protected: bool updateFileState();
}; // BufferRequest

class CheckRequest : public BufferRequest
{
    public: CheckRequest(Val);

    // [E]
    public: void End();

    // [F]
    private: override void finishIo();

    // [O]
    private: override void onEvent(uint)
    {
        openForLoad();
        finishIo();
    } // onEvent
}; // CheckRequest

class LoadRequest : public BufferRequest
{
    private: uint           m_cbHead;
    private: NewlineMode    m_eCrLf;
    private: char16         m_rgwch[k_cbIoBuffer];

    // ctor
    public: LoadRequest(Val buffer, const char16* pwszFile, Val extfmt) :
        BufferRequest(buffer, pwszFile, extfmt),
        m_cbHead(0),
        m_eCrLf(NewlineMode_Detect) {}

    // [D]
    private: NewlineMode detectNewline(const char*, const char*);

    // [E]
    public:  void End();

    // [F]
    private: override void finishIo();

    // [I]
    private: void insertLine(const char*, const char*);

    // [O]
    private: override void onEvent(uint);

    // [P]
    private: void processLine(const char*, const char*, char);
    private: void processLineCrLf(const char*, const char*);
    private: void processPartialLine(const char*, const char*);

    // [R]
    private: void requestRead(uint);
}; // LoadRequest

class SaveRequest : public BufferRequest
{
    // Member variables:
    //  m_posn
    //      Current scanning position
    //  m_end
    //      Keeps end of buffer position at start saving
    //  m_rgwch
    //      Used only in method retrieve (for avoiding implicit alloca)
    //  m_wszTempName
    //      Tempoary pathname for saving. finishIo renames m_wszTempName to
    //      Buffer::GetFileName
    private: Posn   m_posn;
    private: Posn   m_end;
    private: char16 m_rgwch[k_cbIoBuffer];
    private: char16 m_wszTempName[MAX_PATH + 1];


    // ctor
    public: SaveRequest(Val buffer, const char16* pwszFile, Val extfmt);

    // [E]
    public: void End();

    // [F]
    private: override void finishIo();

    // [I]
    private: void insertLine(const char*, const char*);

    // [O]
    private: override void onEvent(uint);

    // [R]
    private: void requestWrite(uint);
    private: void retrieve();
}; // SaveRequest

} // Peer

} // Editor

#endif //!defined(INCLUDE_editor_peer_file_io_h)
