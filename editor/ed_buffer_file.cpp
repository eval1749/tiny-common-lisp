#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer File
// editor/ed_buffer_file.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_file.cpp#6 $
//
//
#include "./ed_buffer_file.h"

#include "./peer/peer_file_io.h"

namespace Editor
{

using namespace TinyCl;

// ctor
BufferFile::BufferFile(Val name)
{
    Val pathname = nil;
    if ('*' == name->StaticCast<SimpleString>()->GetStart()[0])
    {
        pathname = Knone;
    }

    m_char_tick       = zero;
    m_external_format = TLV(Aexternal_formatA);
    m_file_state      = zero;   // We need to check file state.
    m_file_write_date = zero;
    m_pathname        = pathname;
} // BufferFile::BufferFile

// [C]
void BufferFile::CheckWritable() const
{
    if (IsNotReady())
    {
        error(Qbuffer_not_ready, Kbuffer, Encode());
    }

    if (nil != m_read_only)
    {
        error(Qbuffer_read_only, Kbuffer, Encode());
    }
} // BufferFile::CheckWritable

// [G]
const char16* BufferFile::GetFileName() const
{
    if (pathnamep(m_pathname))
    {
        Val pathname = namestring(m_pathname);
        return pathname->DynamicCast<SimpleString>()->GetStart();
    }

    return L"";
} // BufferFile::GetFileName

Val make_external_format(Val thing)
{
    if (thing->Is<ExternalFormat>())
    {
        return thing;
    }

    if (Kdefault == thing)
    {
        Val extfmt = TLV(Aexternal_formatA);
        return extfmt;
    }

    Val charset = gethash(thing, VAR(Acharset_tableA));
    if (nil == charset)
    {
        error("No such charset ~S.", thing);
    }

    Val extfmt = Thread::Get()->AllocRecord(CLASSD_external_format);
    ExternalFormat* p = extfmt->StaticCast<ExternalFormat>();
    p->m_charset = charset;
    p->m_eol     = TLV(Aexternal_formatA)->StaticCast<ExternalFormat>()->m_eol;

    return extfmt;
} // make_external_format

// [L]
bool BufferFile::Load(const char16* pwszFile, Val extfmt)
{
    extfmt = make_external_format(extfmt);

    if (IsNotReady())
    {
        return false;
    }

    if (NULL == pwszFile || 0 == *pwszFile)
    {
        pwszFile = GetFileName();
        if (0 == *pwszFile)
        {
            return false;
        }
    }

    Peer::LoadRequest* pLoad = new Peer::LoadRequest(
        Encode(),
        pwszFile,
        extfmt );

    m_read_only = nil;

    InternalDelete(zero, GetEnd());
    emptyLog();

    m_file_state = Kchecking;
    m_state      = Qload;

    pLoad->Start();

    return true;
} // BufferFile::Load

// [Q]
Val BufferFile::QueryFileState(bool fForce)
{
    if (! pathnamep(m_pathname))
    {
        return Qignore;
    }

    if (! fixnump(m_file_state))
    {
        return m_file_state;
    }

    if (! fForce)
    {
        // We've checked file state so far.
        Int iNow  = ::GetTickCount() & Fixnum::MostPositive;
        Int iDiff = iNow - Fixnum::Decode_(m_file_state);
        Val intv = VAR(Afile_check_intervalA);
        const Int OneMinute = 1000 * 60;
        UInt iIntv = fixnump(intv) ? Fixnum::Decode_(intv) : OneMinute;

        if (static_cast<UInt>(iDiff) < iIntv)
        {
            // This buffer contains up-to date file at the last check
            return nil;
        }
    } // if

    Peer::CheckRequest* pCheck = new Peer::CheckRequest(Encode());
    m_file_state = Kchecking;
    pCheck->Start();
    return Kchecking;
} // BufferFile::QueryFileState

// [S]
bool BufferFile::Save(const char16* pwszFile)
{
    if (IsNotReady())
    {
        return false;
    }

    if (NULL == pwszFile || 0 == *pwszFile)
    {
        pwszFile = GetFileName();
    }

    Peer::SaveRequest* pSave = new Peer::SaveRequest(
        Encode(),
        pwszFile,
        m_external_format );

    m_file_state = Kchecking;
    m_state      = Qwrite;

    pSave->Start();

    return true;
} // BufferFile::Save

void BufferFile::SetFileName(const char16* pwsz)
{
    StackString oFilename(pwsz);
    m_pathname = funcall(Qpathname, oFilename);

    // FIXME 2008-02-11 yosi@msn.com This mode selector is just stab.
    if (char16* pwszDot = ::lstrrchrW(pwsz, '.'))
    {
        pwszDot++;
        if (0 == ::lstrcmpW(pwszDot, L"cpp"))
        {
            m_mode = VAR(cPP_mode);
            m_lexer = nil;
        }
        else if (0 == ::lstrcmpW(pwszDot, L"h"))
        {
            m_mode = VAR(cPP_mode);
            m_lexer = nil;
        }
    } // if

    run_hooks(Qbuffer_pathname, Encode());
} // BufferFile::SetFileName

} // Editor
