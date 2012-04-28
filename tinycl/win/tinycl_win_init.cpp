#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Windows - Initialization
// tinycl_win_init.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/win/tinycl_win_init.cpp#4 $
//
#define DEBUG_CODEPAGE  1
#include "./tinycl_win.h"

namespace TinyCl
{

inline char16 CharDowncase(char16 wch)
{
    return static_cast<char16>(
        reinterpret_cast<intptr_t>(
            ::CharLower(reinterpret_cast<LPWSTR>(wch))));
} // CharDowncase

inline char16  CharUpcase(char16 wch)
{
    return static_cast<char16>(
        reinterpret_cast<intptr_t>(
            ::CharUpper(reinterpret_cast<LPWSTR>(wch))));
} // CharUpcase

static void installExternalFormats()
{
    class CodePageFactory
    {
        // See WinNls.h for value of code page
        // See http://en.wikipedia.org/wiki/Windows_code_page

        // 50000 x-user-defined
        // 50001 _autodetect_all
        // 50021 _iso-2022-jp$ESC (csISO2022JP)
        // 50022 _iso-2022-jp$SIO (iso-2022-jp)
        // 50932 _autodetect
        // 50949 _autodetect_kr
        // 51932 euc-jp
        public: static void FromRegistry()
        {
            long lError;

            HKEY hDatabase;
            lError = ::RegOpenKeyEx(
                HKEY_CLASSES_ROOT,
                L"MIME\\Database\\Codepage",
                0,
                KEY_READ,
                &hDatabase );
            if (ERROR_SUCCESS != lError)
            {
                return;
            }

            uint dwIndex = 0;
            for (;;)
            {
                char16 wszCodePage[20];

                DWORD cwchCodePage = lengthof(wszCodePage);
                lError = ::RegEnumKeyEx(
                    hDatabase,
                    dwIndex,
                    wszCodePage,
                    &cwchCodePage,
                    NULL,
                    NULL,
                    NULL,
                    NULL );
                if (ERROR_SUCCESS != lError)
                {
                    break;
                }

                dwIndex += 1;

                HKEY hCodePage;
                lError = ::RegOpenKeyEx(
                    hDatabase,
                    wszCodePage,
                    0,
                    KEY_READ,
                    &hCodePage );
                if (ERROR_SUCCESS != lError)
                {
                    continue;
                }

                char16 wszMime[100];
                DWORD  cbMime = sizeof(wszMime);
                lError = ::RegQueryValueEx(
                    hCodePage,
                    L"WebCharset",
                    NULL,
                    NULL,   // lpType
                    reinterpret_cast<uint8*>(wszMime),
                    &cbMime );
                if (ERROR_SUCCESS != lError)
                {
                    lError = ::RegQueryValueEx(
                        hCodePage,
                        L"BodyCharset",
                        NULL,
                        NULL,   // lpType
                        reinterpret_cast<uint8*>(wszMime),
                        &cbMime );
                    if (ERROR_SUCCESS != lError)
                    {
                        continue;
                    }
                } // if

                if ('_' == *wszMime || 'x' == *wszMime)
                {
                    continue;
                }

                int nCp  = aton(wszCodePage);

                switch (nCp)
                {
                case 1200:
                    ::lstrcpyW(wszMime, L"utf-16le");
                    break;

                case 1201:
                    ::lstrcpyW(wszMime, L"utf-16be");
                    break;
                } // switch nCp

                #if DEBUG_CODEPAGE
                    DEBUG_PRINTF("%d %ls\n", nCp, wszMime);
                #endif

                Val cp   = Fixnum::Encode(nCp);
                Val mime = make_string(wszMime);

                Val name = intern(make_string(::CharUpperW(wszMime)), PKG_keyword);

                Val charset = Thread::Get()->AllocRecord(CLASSD_charset);
                Charset* p = charset->StaticCast<Charset>();
                p->m_name      = name;
                p->m_code_page = cp;
                p->m_mime_name = mime;

                setf_gethash(charset, cp, VAR(Acharset_tableA));
                setf_gethash(charset, name, VAR(Acharset_tableA));
            } // for
        } // FromRegistry

        private: static int aton(const char16* pwsz)
        {
            Int nCp = 0;
            while (0 != *pwsz)
            {
                nCp *= 10;
                nCp += *pwsz - '0';
                pwsz++;
            } // while
            return nCp;
        } // aton

    #if 0
        public: static void FromAPI()
        {
            ::EnumSystemCodePages(enumCodePageProc, CP_SUPPORTED);
            setName(932,       Kshift_jis);
            setName(CP_UTF7,   Kutf_7);
            setName(CP_UTF8,   Kutf_8);
        } // FromAPI

        private: static BOOL CALLBACK enumCodePageProc(char16* pwsz)
        {
            Int nCp = aton(pwsz);

            char16 wsz[20];
            ::wsprintfW(wsz, L"CP%u", nCp);

            Val cp   = Fixnum::Encode(nCp);
            Val name = intern(make_string(wsz), PKG_keyword);

            Val charset = Thread::Get()->AllocRecord(CLASSD_charset);
            Charset* p = charset->StaticCast<Charset>();
            p->m_name      = name;
            p->m_aliases   = nil;
            p->m_code_page = cp;

            #if 0
                CPINFOEX oInfo;
                char16* pwszDesc = oInfo.CodePageName;
                if (! ::GetCPInfoEx(nCp, 0, &oInfo))
                {
                    oInfo.CodePageName[0] = 0;
                }
                else
                {
                    // Remove parenthesis
                    int cwch = ::lstrlenW(pwszDesc);
                    if (0x28 == *pwszDesc && 0x29 == pwszDesc[cwch -1])
                    {
                        pwszDesc[cwch - 1] = 0;
                        pwszDesc++;
                    }
                } // if

                p->m_description = make_string(pwszDesc);
            #endif

            setf_gethash(charset, cp, VAR(Acharset_tableA));
            setf_gethash(charset, name, VAR(Acharset_tableA));

            #if DEBUG_CODEPAGE
                DEBUG_PRINTF("%d %ls\n", nCp, oInfo.CodePageName);
            #endif

            return true;
        } // enumCodePageProc

        // [S]
        private: static void setName(uint nCp, const char* psz)
        {
            Val charset = gethash(Fixnum::Encode(nCp), VAR(Acharset_tableA));
            if (nil == charset) return;

            Charset* p = charset->StaticCast<Charset>();
            Val alias = intern(make_string(psz), PKG_keyword);
            push(alias, p->m_aliases);

            setf_gethash(charset, alias, VAR(Acharset_tableA));
        } // SetName

    #endif
    }; // CodePageFactory

    VAR(Acharset_tableA) = make_hash_table(
        Ktest, Qeq,
        Ksize, Fixnum::Encode(601) );

    CodePageFactory::FromRegistry();

    // default external format
    {
        Val extfmt = Thread::Get()->AllocRecord(CLASSD_external_format);
        ExternalFormat* p = extfmt->StaticCast<ExternalFormat>();

        p->m_charset = gethash(
            Fixnum::Encode(CP_UTF8),
            VAR(Acharset_tableA) );

        p->m_eol = Kcrlf;

        VAR(Adefault_external_formatA) = extfmt;

        TLV(Aexternal_formatA) = extfmt;

        if (TlvRecord* p = gethash(QAexternal_formatA, VAR(Avalue_tableA))->
                            DynamicCast<TlvRecord>() )
        {
            p->m_value = extfmt;
        } // if
    }
} // installExternalFormats

//////////////////////////////////////////////////////////////////////
//
// installStaticChars
//  Intialize character objects
//
// See WinNls.h for C1_xxx
//  C1_UPPER    0x001
//  C1_LOWER    0x002
//  C1_DIGIT    0x004
//  C1_SPACE    0x008
//  C1_PUNCT    0x010
//  C1_CTRL     0x020
//  C1_BLANK    0x040
//  C1_XDIGIT   0x080
//  C1_ALPHA    0x100
//  C1_DEFINED  0x200
//
//    Code    Name      Type
//  +-------+---------+-------------------------------
//  | 0x09  | TAB     | C1_SPACE + C1_CTRL + C1_BLANK
//  | 0x0A  | LF      | C1_SPACE + C1_CTRL
//  | 0x0D  | CR      | C1_SPACE + C1_CTRL
//  | 0x20  | SPACE   | C1_SPACE + C1_BLANK
//  +-------+---------+-------------------------------
//
static void installStaticChars()
{
    Layout_character* pStart = reinterpret_cast<Layout_character*>(
        Record::Decode_(CHAR_u0000) );

    Layout_character* pEnd = pStart + Character::Limit;

    for (Layout_character* pChar = pStart; pChar < pEnd; pChar++)
    {
        Int iData = 0;

        char16 wch  = static_cast<char16>(pChar - pStart);

        if ((wch >= 0x20 && wch <= 0x7E) || 0x0A == wch)
        {
            iData |= Character::Attr_Standard;
        }

        uint16 wType = 0;
        ::GetStringTypeW(CT_CTYPE1, &wch, 1, &wType);

        if (C1_ALPHA & wType)
        {
            iData |= Character::Attr_Alpha | Character::Attr_Graphic;
        }

        if (C1_DIGIT & wType)
        {
            iData |= Character::Attr_Numeric | Character::Attr_Graphic;
        }

        if (C1_LOWER & wType)
        {
            iData |= Character::Attr_LowerCase;
            iData |= CharUpcase(wch) << Character::CaseShiftCount;
        }

        if (C1_PUNCT & wType)
        {
            iData |= Character::Attr_Graphic;
        }

        if (C1_SPACE & wType)
        {
            iData |= Character::Attr_Whitespace;
        }

        if (C1_UPPER & wType)
        {
            iData |= Character::Attr_UpperCase;
            iData |= CharDowncase(wch) << Character::CaseShiftCount;
        }

        pChar->m_classd = CLASSD_character;
        pChar->m_data   = Datum::FromInt<Datum>(iData);
    } // for
} // installStaticChars

// [P]
void Platform_InstallStaticObjects()
{
    {
        Val vartab = VAR(Aruntime_environmentA)->StaticCast<Environment>()->
            m_variables;

        Val tlvrec = cdr(assq(Kspecial, cdr(gethash(QAfeaturesA, vartab))));

        TlvRecord* p = tlvrec->StaticCast<TlvRecord>();
        p->m_value = listA(Kx86, Kwin32, K32bit, p->m_value);
        TLV(AfeaturesA) = p->m_value;
    }

    installStaticChars();
    installExternalFormats();
} // Platform_InstallStaticObjects

} // TinyCl
