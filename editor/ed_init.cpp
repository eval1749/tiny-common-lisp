#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Initialization
// editor/ed_init.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_init.cpp#17 $
//
#include "./cm_cmdl.h"
#include "./cm_keymap.h"
#include "./cm_mode.h"

#include "./ed_buffer.h"
#include "./ed_range.h"
#include "./ed_selection.h"
#include "./ed_style.h"
#include "./ed_text_window.h"

#include "./rtl_defs.h"

#include "./ed_frame.h"

#include "./peer/peer_application.h"
#include "./peer/peer_frame.h"

namespace Editor
{

namespace Peer
{
    extern void DispatchThreadMessage(MSG*);
} // Peer

namespace
{

// <summary>
//   Populate *commands* and *global-keymap* from "cm_commands.inc".
// </summary>
static void initCommandsAndKeymap()
{
    struct KeymapEntry
    {
        uint    m_nKey;
        Val     m_name;
    }; // KeymapEntry

    #define Key_Control_(x)       (Key_ ## x | 0x200)
    #define Key_Control_Shift_(x) (Key_ ## x | 0x600)
    #define Key_Shift_(x)         (Key_ ## x | 0x400)

    static const KeymapEntry k_rgoEntry[] =
    {
        #define CommandEntry(mp_key, mp_name) \
            { Key_ ## mp_key, Q ## mp_name },

        #include "./cm_commands.inc"
    }; // k_rgoEntry

    Keymap* pKeymap = VAR(Aglobal_keymapA)->StaticCast<Keymap>();

    Val command_table = VAR(Acommand_tableA);

    {
        Val name = Qgraphic_key;
        Val fn = symbol_function(name);
        Val graph_key_command = (new Command(name, fn))->Encode();
        setf_gethash(graph_key_command, name, command_table);

        for (uint ch = 0x20; ch <= 0x7E; ch++)
        {
            pKeymap->Set(
                Character::FromCode(static_cast<char16>(ch)),
                graph_key_command );
        } // for ch
    }

    for (
        const KeymapEntry* p = k_rgoEntry;
        p < k_rgoEntry + lengthof(k_rgoEntry);
        p++ )
    {
        if (Key_UnAssigned != p->m_nKey)
        {
            Val fn  = symbol_function(p->m_name);
            Val cmd = (new Command(p->m_name, fn))->Encode();

            pKeymap->Set(Fixnum::Encode(p->m_nKey), cmd);

            setf_gethash(cmd, p->m_name, command_table);
        }
    } // for p
} // initCommandsAndKeymap

// <summary>
//   Install editing mode.
// </summary>
static void initModes()
{
    Val const cPP_mode = Thread::Get()->AllocRecord(CLASSD_cPP_mode);
    {
        Mode* p = cPP_mode->StaticCast<Mode>();
        p->m_keymap    = make_keymap();
        p->m_string    = make_string("C++");
        p->m_style_map = make_hash_table(Ktest, Qeq);

        p->m_keywords      = nil;
        p->m_syntax_table = nil;

    //RGB(  0,   0,    0),    // TokenType_Normal
    //RGB(  0,  128,   0),    // TokenType_Comment
    //RGB(163,   21,  21),    // TokenType_String
    //RGB(  0,    0,   0),    // TokenType_Word
    //RGB(  0,    0, 255),    // TokenType_WordReserved


        setf_gethash(
            list(Kcolor, Fixnum::Encode(RGB(0, 128, 0)),
                 Kstate, Kblock_comment ),
            Kblock_comment,
            p->m_style_map );

        setf_gethash(
            list(Kcolor, Fixnum::Encode(RGB(0, 128, 0)),
                 Kstate, Kline_comment ),
            Kline_comment,
            p->m_style_map );

        setf_gethash(
            list(Kcolor, Fixnum::Encode(RGB(0, 0, 255)),
                 Kstate, Kkeyword ),
            Kkeyword,
            p->m_style_map );

        setf_gethash(
            list(Kcolor, Fixnum::Encode(RGB(163, 21, 21)),
                 Kstate, Kstring ),
            Kstring,
            p->m_style_map );
    }

    Val const text_mode = Thread::Get()->AllocRecord(CLASSD_text_mode);
    {
        Mode* const p = text_mode->StaticCast<Mode>();
        p->m_keymap    = make_keymap();
        p->m_string    = make_string("Text");
        p->m_style_map = make_hash_table(Ktest, Qeq);

        p->m_keywords      = nil;
        p->m_syntax_table = nil;

        setf_gethash(
            list(
                Kcolor,             Fixnum::Encode(RGB(0, 0, 255)),
                Kstate,             Kuri,
                Ktext_decoration,   Kunderline ),
            Kuri,
            p->m_style_map );
    }

    VAR(cPP_mode)  = cPP_mode;
    VAR(text_mode) = text_mode;
    VAR(AmodesA)   = list(cPP_mode, text_mode);
} // initModes

// <summary>
//   Install styles.
// </summary>
static void initStyles()
{
    Val const style_map = VAR(Astyle_mapA);

    // *style*
    {
        Style* const p = new Style;

        p->m_color       = Fixnum::Encode(0x000000);
        p->m_background  = Fixnum::Encode(0xffffff);
        p->m_marker      = Fixnum::Encode(0x009900);
        p->m_font_family = list(
            list(
                make_string("Consolas"),
                Fixnum::Encode(ANSI_CHARSET) ),
            list(
                make_string("MS Gothic"),
                Fixnum::Encode(SHIFTJIS_CHARSET) ) );
        p->m_font_size   = Fixnum::Encode(10);
        p->m_font_style  = Knormal;
        p->m_font_weight = Knormal;
        p->m_text_decoration = Knone;

        Val styles = nil;

        styles = nconc(styles, list(Kbackground,        p->m_background));
        styles = nconc(styles, list(Kcolor,             p->m_color));
        styles = nconc(styles, list(Kfont_family,       p->m_font_family));
        styles = nconc(styles, list(Kfont_size,         p->m_font_size));
        styles = nconc(styles, list(Kfont_style,        p->m_font_style));
        styles = nconc(styles, list(Kfont_weight,       p->m_font_weight));
        styles = nconc(styles, list(Kmarker,            p->m_marker));
        styles = nconc(styles, list(Ktext_decoration,   p->m_text_decoration));

        setf_gethash(styles, Kdefault, style_map);

        VAR(AstyleA) = p->Encode();
    }

    // *active-selection-style*
    Style* pActiveSelection;
    {
        Style* const p = new Style;

        p->m_background  = Fixnum::Encode(RGB( 51, 153, 255));
        p->m_color       = Fixnum::Encode(RGB(255, 255, 255));
        p->m_marker      = p->m_color;

        pActiveSelection = p;

        setf_gethash(
            list(
                Kbackground,    p->m_background,
                Kcolor,         p->m_color,
                Kmarker,        p->m_marker ),
            Kactive_selection,
            style_map );

        VAR(Aactive_selection_styleA) = p->Encode();
    }

    // *inactive-selection-style*
    {
        Style* const p = new Style;

        p->m_background  = Fixnum::Encode(RGB(191, 205, 219));
        p->m_color       = Fixnum::Encode(RGB(67, 78, 84));
        p->m_marker      = p->m_color;

        setf_gethash(
            list(
                Kbackground,    p->m_background,
                Kcolor,         p->m_color,
                Kmarker,        p->m_marker ),
            Kinactive_selection,
            style_map );

        VAR(Ainactive_selection_styleA) = p->Encode();
    }

    // *dialog-style*
    {
        Style* const p = new Style;

        p->m_color       = Fixnum::Encode(0x000000);
        p->m_background  = Fixnum::Encode(0xffffff);
        p->m_marker      = Fixnum::Encode(0x009900);

        // Note: Get charset by EnumFontFamiliesEx
        p->m_font_family = list(
            make_string("MS Shell Dlg 2"),
            make_string("MS Gothic") );

        #if 0
        p->m_font_family = list(
            list(
                make_string("MS Shell Dlg 2"),
                Fixnum::Encode(ANSI_CHARSET) ),
            list(
                make_string("MS Gothic"),
                Fixnum::Encode(SHIFTJIS_CHARSET) ) );
        #endif

        p->m_font_size   = Fixnum::Encode(10);
        p->m_font_style  = Knormal;
        p->m_font_weight = Knormal;
        p->m_text_decoration = Knone;

        VAR(Adialog_styleA) = p->Encode();
    }

    setf_gethash(
        list(Kstate, Kime, Ktext_decoration, Kime_active),
        Kime_active,
        style_map );

    setf_gethash(
        list(Kstate, Kime, Ktext_decoration, Kime_inactive0),
        Kime_inactive0,
        style_map );

    setf_gethash(
        list(Kstate, Kime, Ktext_decoration, Kime_inactive1),
        Kime_inactive1,
        style_map );

    setf_gethash(
        list(Kstate, Kime, Ktext_decoration, Kime_input),
        Kime_input,
        style_map );

    setf_gethash(
        list(
            Kbackground,    pActiveSelection->m_background,
            Kcolor,         pActiveSelection->m_color,
            Kstate,         Kime ),
        Kime_not_converted,
        style_map );

    setf_gethash(
        list(
            Kbackground,    Fixnum::Encode(RGB(255, 255, 255)),
            Kcolor,         Fixnum::Encode(RGB(255, 0, 0)),
            Kstate,         Kime ),
        Kime,
        style_map );
} // initStyles

#if 0
static void saveImage()
{
    char16 wszImage[MAX_PATH + 1];
    if (0 == ::GetModuleFileName(NULL, wszImage, lengthof(wszImage)))
    {
        return;
    }

    // Compose image file name
    {
        char16* pwszDot = ::lstrrchrW(wszImage, '.');
        if (NULL == pwszDot)
        {
            return;
        }
        ::lstrcpyW(pwszDot + 1, L"image");
    }

    Si::Mm::Save(wszImage);
} // saveImage
#endif

} // namespace

/// <summary>
///   Load genesis files.
/// </summary>
static void loadGenesis(Val const filename)
{
    DEBUG_FORMAT("~S~%", filename);

    Val buffer = make_buffer("*genesis*");
    Val ostream = make_buffer_output_stream(buffer);

    Si::BindFrameScope_<3> oLet;
        oLet.Bind(TLV_Aerror_outputA,       ostream);
        oLet.Bind(TLV_Atrace_outputA,       ostream);
        oLet.Bind(TLV_Astandard_outputA,    ostream);

    Val const frame = car(VAR(AframesA));
    Val const window = (new TextWindow(buffer))->Encode();
    add_window(frame, window);

    values(filename, Kif_does_not_exist, nil);
    Val const in = openV(Thread::Get());
    if (nil == in)
    {
        format(t, "Can't open ~S. You need to set start up directory.~%",
            filename );
        return;
    }

    format(t, "; Load genesis file from ~S~%", filename);

    Val const eof = list(Keof);

    for (;;)
    {
        Val const form = read(in, nil, eof);
        if (form == eof)
        {
            break;
        }

        Val const value = eval(form);
        format(t, "~S~%", value);

        // <FIXME date="2008-08-02" by="yosi@msn.com">
        //  We should move below code to a function and another file.
        {
            // Update window contents
            Peer::Application::Get()->OnIdle(0);

            MSG oMsg;
            while (::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
            {
                if (NULL == oMsg.hwnd)
                {
                    Peer::DispatchThreadMessage(&oMsg);
                }
                else
                {
                    ::TranslateMessage(&oMsg);
                    ::DispatchMessage(&oMsg);
                }
            } // while
        }
        // </FIXME>
    } // for

    close(in);
} // loadGenesis

/// <summary>
///   Genesis command line processor.
///   <p>Note:
///     This function is invoked by THREAD_WM_GENESIS message from message
///     loop after initial frame is created.
///   </p>
///   <p>
///     Please use "-genesis" command line option to specify genesis file.
///     <example>
///         % evita -genesis $ROOT/genesis/genesis.lisp
///     </example>
///   </p>
/// </summary>
void Genesis()
{
    enum State
    {
        State_Normal,

        State_Genesis,
    } eState = State_Normal;

    // Command line option name.
    Si::StackString_<> oGenesis("-genesis");

    Val argv = Si::VAR_Acommand_line_argumentsA->
        StaticCast<Si::ValueCell>()->m_value;

    Val genesis_file = nil;

    foreach (List::Enum, oEnum, argv)
    {
        Val token = oEnum.Get();

        switch (eState)
        {
        case State_Genesis:
            genesis_file = token;
            break;

        case State_Normal:
            if (string_eq(token, oGenesis))
            {
                eState = State_Genesis;
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch eState
    } // for

    if (nil == genesis_file)
    {
        genesis_file = make_string("genesis.lisp");
    }

    loadGenesis(genesis_file);
} // Genesis

/// <summary>
///   Initialize editor application.
///   <list type="number">
///     <item>Creates static objects</item>
///     <item>
///         Create a frame with <c>*scratch*</c> window and
///         <c>*lisp output*</c> windows.
///     </item>
///   </list>
/// </summary>
/// <param name="fHasImage">True if there is an editor image.</param>
void Initialize_Editor(bool const fHasImage)
{
    if (! fHasImage)
    {
        CommandState* const pCmdState = new CommandState;
        VAR(Acommand_stateA) = pCmdState->Encode();

        {
            push(Keditor, TLV(AfeaturesA));

            Val const tlvrec = svref(
                TinyCl::VAR_Atlv_vectorA->StaticCast<ValueCell>()->m_value,
                Fixnum::Encode(TLV_AfeaturesA) );

            tlvrec->StaticCast<TinyCl::TlvRecord>()->m_value =TLV(AfeaturesA);
        }

        Si::internal_package_put(
            PKG_ed->StaticCast<Si::Package>()->m_internal_table,
            Qend );

        Si::internal_package_put(
            PKG_ed->StaticCast<Si::Package>()->m_internal_table,
            Qstart );

        initCommandsAndKeymap();
        initStyles();
        initModes();
    } // if

    Thread::Get()->Restart();

    VAR(AbuffersA) = nil;
    VAR(AframesA)  = nil;

    Val const frame = (new Frame)->Encode();
    add_window(frame, (new TextWindow(make_buffer("*scratch*")))->Encode());

    // Make buffer must be occured after keymap and style intializations.
    {
        Val const buffer = make_buffer("*lisp output*");
        Val const ostream = make_buffer_output_stream(buffer);

        TLV(Aerror_outputA)    = ostream;
        TLV(Atrace_outputA)    = ostream;
        TLV(Astandard_outputA) = ostream;

        // <FIXME date="2008-12-23" by="yosi@msn.com">
        //  We should bind:
        //      *debug-io*
        //      *query-io*
        //      *stndard-input*
        // </FIXME>

        add_window(frame, (new TextWindow(buffer))->Encode());
    }

    realize_instance(frame);

    show_window(frame);
} // Initialize_Commands

} // Editor
