//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer - File I/O
// editor/peer/peer_file_io.h
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_file_dialog.h#1 $
//
#if !defined(INCLUDE_editor_peer_file_io_h)
#define INCLUDE_editor_peer_file_io_h

#include "./peer_defs.h"

// For CPP deffinition of GetOpenFileName and GetSaveFileName.
#include <commdlg.h>

namespace Editor
{

namespace Peer
{

class FileDialog
{
    public: struct Param
    {
        bool    m_fReadOnly;
        HWND    m_hwndOwner;
        char16* m_pwszFile;
        char16  m_wsz[MAX_PATH + 1];
        char16  m_wszDir[MAX_PATH + 1];

        Param()
        {
            ::ZeroMemory(this, sizeof(*this));
        } // Param

        void SetDirectory(const char16*);
    }; // Param

    // [G]
    public: bool GetOpenFileName(Param*);
    public: bool GetSaveFileName(Param*);
}; // FileDialog

} // Peer

} // Editor

#endif //!defined(INCLUDE_editor_peer_file_io_h)
