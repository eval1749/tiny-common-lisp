#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - File I/O Peer
// editor/peer/peer_file_io.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_file_dialog.cpp#1 $
//
#include "./peer_file_dialog.h"

namespace Editor
{

namespace Peer
{

// FileDialog::GetOpenFileName
bool FileDialog::GetOpenFileName(Param* pParam)
{
    OPENFILENAME oOfn;
    ::ZeroMemory(&oOfn, sizeof(oOfn));
    oOfn.lStructSize     = sizeof(oOfn);
    oOfn.hwndOwner       = pParam->m_hwndOwner;
    //oOfn.hInstance     = g_hResource;
    oOfn.lpstrFilter     = L"All Files\0*.*\0"
                           L"C/C++ Files\0*.cpp;*.hpp;*.c;*.h;*.cxx;*.hxx\0"
                           L"Lisp Files\0*.lisp;*.l;*.lsp;*.cl\0";
    oOfn.lpstrFile       = pParam->m_wsz;

    oOfn.lpstrInitialDir =
        0 == pParam->m_wszDir[0] ? NULL : pParam->m_wszDir;

    oOfn.nMaxFile        = lengthof(pParam->m_wsz);
    oOfn.Flags           = 0;
    oOfn.Flags           |= OFN_ENABLESIZING;
    oOfn.Flags           |= OFN_EXPLORER;
    oOfn.Flags           |= OFN_FILEMUSTEXIST;
    oOfn.Flags           |= OFN_PATHMUSTEXIST;
    if (! ::GetOpenFileName(&oOfn))
    {
        return false;
    } // if

    pParam->m_fReadOnly = (oOfn.Flags & OFN_READONLY) != 0;
    pParam->m_pwszFile = oOfn.lpstrFile + oOfn.nFileOffset;

    return true;
} // FileDialog::GetOpenFileName


// FileDialog::GetSaveFileNameW
bool FileDialog::GetSaveFileNameW(Param* pParam)
{
    OPENFILENAME oOfn;
    ::ZeroMemory(&oOfn, sizeof(oOfn));
    oOfn.lStructSize = sizeof(oOfn);
    oOfn.hwndOwner   = pParam->m_hwndOwner;
    //oOfn.hInstance   = g_hResource;
    oOfn.lpstrFilter = L"All Files\0*.*\0";
    oOfn.lpstrFile   = pParam->m_wsz;
    oOfn.nMaxFile    = lengthof(pParam->m_wsz);

    oOfn.lpstrInitialDir =
        0 == pParam->m_wszDir[0] ? NULL : pParam->m_wszDir;

    oOfn.Flags       = 0;
    oOfn.Flags       |= OFN_CREATEPROMPT;
    oOfn.Flags       |= OFN_ENABLESIZING;
    oOfn.Flags       |= OFN_EXPLORER;
    oOfn.Flags       |= OFN_OVERWRITEPROMPT;
    oOfn.Flags       |= OFN_PATHMUSTEXIST;
    oOfn.Flags       |= OFN_SHAREAWARE;
    if (! ::GetSaveFileName(&oOfn))
    {
        return false;
    } // if

    pParam->m_pwszFile = oOfn.lpstrFile + oOfn.nFileOffset;

    return true;
} // FileDialog::GetSaveFileNameW


void
FileDialog::Param::SetDirectory(const char16* pwszFile)
{
    if (0 == *pwszFile)
    {
        return;
    }

    char16* pwszFilePart;
    uint cwchFull = ::GetFullPathName(
        pwszFile,
        lengthof(m_wszDir),
        m_wszDir,
        &pwszFilePart );
    if (cwchFull >= 1 && NULL != pwszFilePart)
    {
        *pwszFilePart = 0;
    }
} // FileDialog::Param::SetDirectory

} // Peer
} // Editor
