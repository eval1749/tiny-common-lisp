#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Initialization for Runtime
// editor/rtl_init.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_init.cpp#13 $
//
//
#define DEBUG_LOAD 1
#include "./rtl_defs.h"

#include "./ed_objects.h"
#include "./ed_layout.h"

#include "../tinycl/init/tinycl_init.h"
#include "../tinycl/tinycl_clos.h"
#include "../tinycl/win/tinycl_win.h"

extern HINSTANCE g_hInstance;

namespace Editor
{

using namespace TinyCl::Extension;
using TinyCl::ClassD;

using TinyCl::CLASS_error;
using TinyCl::CLASS_input_stream;
using TinyCl::CLASS_output_stream;
using TinyCl::CLASS_type_error;

using TinyCl::CLASSD_standard_class;
using TinyCl::CLASSD_structure_class;
using TinyCl::CLASS_structure_object;

using TinyCl::PKG_cl;
using TinyCl::PKG_ext;
using TinyCl::PKG_keyword;

#include "../tinycl/init/tinycl_install_arity.h"
#define FUN_VAR(mp_name, mp_min, mp_max, mp_vals) \
    defun_optional(mp_name, mp_min, mp_max)

#define FUN_VAR_SETF(mp_name, mp_min, mp_max) \
    defun_setf_optional(mp_name, mp_min, mp_max)

#include "./ed_rtl.inc"

namespace
{

static const Si::FunEntry k_rgoFunEntry[] =
{
    #include "../tinycl/init/tinycl_init_entry.h"
    #include "./ed_rtl.inc"
}; // k_rgoFunEntry

#define Qnext_match_using_source    Si::Qnext_match_using_source
#define Qrange_string               Si::Qrange_string

static Si::MethodEntry k_rgoMethodEntry[] =
{
    #include "../tinycl/init/tinycl_method_entry.h"
    #include "./ed_rtl.inc"
}; // k_rgoMethodEntry

static HANDLE findImage()
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

static void installCommands()
{
    struct CommandEntry
    {
        Val         m_name;
        const char* m_psz;
    }; // CommandEntry

    static const CommandEntry k_rgoCommandEntry[] =
    {
        #define CommandEntry(mp_key, mp_name) \
            { Q ## mp_name, "Command_" # mp_name },
        #include "./cm_commands.inc"
    }; // k_rgoCommandEntry

    for (
        const CommandEntry* p = k_rgoCommandEntry;
        p < k_rgoCommandEntry + lengthof(k_rgoCommandEntry);
        p++ )
    {
        if (fboundp(p->m_name)) continue;
        DEBUG_FORMAT("~S~%", p->m_name);
        Val fn = MakeWrapper(p->m_name, 0, 0, 0, 1, 0, p->m_psz);
        p->m_name->StaticCast<Si::Symbol>()->m_function = fn;
    } // for p
} // installCommands

static void makeStaticObjects()
{
    #define PRINT_BASE(mp_x) DEBUG_PRINTF(#mp_x "=%x\n", mp_x)

    PRINT_BASE(CLASSD_BASE);
    PRINT_BASE(CLASS_BASE);
    PRINT_BASE(SYMBOL_BASE);
    PRINT_BASE(SETF_BASE);
    PRINT_BASE(TLVREC_BASE);
    PRINT_BASE(VAR_BASE);
    PRINT_BASE(STATIC_OBJECT_END);

    #include "../tinycl/init/tinycl_install_object.h"
    #include "./ed_objects.inc"

    InstallType(Qcolor, t);
    InstallType(Qfont_size, Qfixnum);

    Val Qmember_end_start = list(Qmember, Kend, Kstart);

    Val Qor_double_linked_item_null =
        list(Qor, Qdouble_linked_item, Qnull);

    Val Qor_double_linked_list_null =
        list(Qor, Qdouble_linked_list, Qnull);

    Val Qor_buffer_null      = list(Qor, Qbuffer,       Qnull);
    Val Qor_color_null       = list(Qor, Qcolor,        Qnull);
    Val Qor_font_size_null   = list(Qor, Qfont_size,    Qnull);
    Val Qor_interval_null    = list(Qor, Qinterval,     Qnull);
    Val Qor_hash_table_null  = list(Qor, Qhash_table,   Qnull);
    Val Qor_keymap_null      = list(Qor, Qkeymap,       Qnull);
    Val Qor_range_null       = list(Qor, Qrange,        Qnull);
    Val Qor_string_list      = list(Qor, Qstring,       Qlist);
    Val Qor_tracker_null     = list(Qor, Qtracker,      Qnull);
    Val Qor_undo_record_null = list(Qor, Qundo_record,  Qnull);
    Val Qor_window_null      = list(Qor, Qwindow,       Qnull);

    #include "../tinycl/init/tinycl_install_classd.h"
    #include "./ed_layout.inc"

    #include "../tinycl/init/tinycl_install_class.h"
    #include "./ed_layout.inc"

    #include "../tinycl/init/tinycl_finalize_class.h"
    #include "./ed_layout.inc"

    VAR(Aglobal_keymapA) = make_keymap();


    installCommands();

    InstallStaticFunctions(k_rgoFunEntry, lengthof(k_rgoFunEntry));

    InstallStaticMethods(k_rgoMethodEntry, lengthof(k_rgoMethodEntry));

    {
        #define deflayout(mp_name, mp_meta, mp_layout) \
            PopulateMethodCache(CLASS_ ## mp_name);

        #define deflstorage(mp_name, mp_meta, mp_layout) \
            deflayout(mp_name, mp_meta, mp_layout)

        #include "./ed_layout.inc"
    }
} // makeStaticObjects

} // namespace

/// <summary>
///   Initialize editor runtime environment.
/// </summary>
bool Initialize_Runtime()
{
    FileHandle shImage = findImage();

    Si::InitParams oParams;
    oParams.m_hImage      = shImage;
    oParams.m_hSelf       = g_hInstance;
    oParams.m_nTotalMb    = 16;
    oParams.m_pvStaticEnd = reinterpret_cast<void*>(STATIC_OBJECT_END);

    Thread* pth = Initialize(&oParams);

    if (INVALID_HANDLE_VALUE != shImage.h)
    {
        return true;
    }

    makeStaticObjects();

    InstallPredicate(Qbufferp,      CLASSD_buffer);
    InstallPredicate(Qfontp,        CLASSD_font);
    InstallPredicate(Qframep,       CLASSD_frame);
    InstallPredicate(Qintervalp,    CLASSD_interval);
    InstallPredicate(Qkeymapp,      CLASSD_keymap);
    InstallPredicate(Qrangep,       Qor, CLASSD_range, CLASSD_selection);
    InstallPredicate(Qstylep,       CLASSD_style);

    InstallPredicate(
        Qundo_record_p,
        CLASSD_undo_record_min,
        CLASSD_undo_record_max );

    InstallPredicate(
        Qwindowp,
        CLASSD_window_min,
        CLASSD_window_max );

    INSTALL_STATIC_READER(buffer_length,    buffer,    length);
    INSTALL_STATIC_READER(buffer_name,      buffer,    name);
    INSTALL_STATIC_READER(buffer_pathname,  buffer,    pathname);

    INSTALL_STATIC_READER(selection_active, selection, active);
    INSTALL_STATIC_READER(selection_buffer, selection, buffer);
    INSTALL_STATIC_READER(selection_end,    selection, end);
    INSTALL_STATIC_READER(selection_start,  selection, start);

    INSTALL_STATIC_READER(text_window_selection,    text_window, selection);

    Si::Mm::FinalizeStatic(pth);

    VAR(AhooksA) = make_hash_table(Ktest, Qeq, Ksize, Fixnum::Encode(1021));

    return false;
} // Initialize

} // Editor
