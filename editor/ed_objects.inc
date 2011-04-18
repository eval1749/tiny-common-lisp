// -*- Mode: C++; -*-
// Edtior - Lisp Objcts
// editor/ed_objects.inc
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_defs.h#1 $
//

#include "../tinycl/tinycl_begin_list.h"

defpackage(ed, (PKG_ed, "EDITOR ED", 501, 501, PKG_cl, PKG_ext, nil))

// [A]
defkeyword(active_selection,    "ACTIVE-SELECTION")
// [B]
defkeyword(background,          "BACKGROUND")
defkeyword(backward,            "BACKWARD")
defkeyword(block_comment,       "BLOCK-COMMENT")
defkeyword(bold,                "BOLD")
defkeyword(buffer,              "BUFFER")
// [C]
defkeyword(command,             "COMMAND")
defkeyword(character,           "CHARACTER")
defkeyword(checking,            "CHECKING")
defkeyword(color,               "COLOR")
defkeyword(cpp,                 "CPP")
defkeyword(cpp_word,            "CPP_WORD")
// [E]
defkeyword(editor,              "EDITOR")
defkeyword(extend,              "EXTEND")
// [F]
defkeyword(font_family,         "FONT-FAMILY")
defkeyword(font_size,           "FONT-SIZE")
defkeyword(font_style,          "FONT-STYLE")
defkeyword(font_weight,         "FONT-WEIGHT")
defkeyword(forward,             "FORWARD")
defkeyword(frame,               "FRAME")
// [I]
defkeyword(ime,                 "IME")
defkeyword(ime_active,          "IME-ACTIVE")
defkeyword(ime_inactive0,       "IME-INACTIVE0")
defkeyword(ime_inactive1,       "IME-INACTIVE1")
defkeyword(ime_input,           "IME-INPUT")
defkeyword(ime_not_converted,   "IME-NOT-CONVERTED")
defkeyword(inactive_selection,  "INACTIVE-SELECTION")
defkeyword(italic,              "ITALIC")
// [K]
defkeyword(keyword,             "KEYWORD")
// [L]
defkeyword(line,                "LINE")
defkeyword(line_comment,        "LINE-COMMENT")
// [M]
defkeyword(marker,              "MARKER")
defkeyword(none,                "NONE")
defkeyword(normal,              "NORMAL")
// [O]
defkeyword(obsolete,            "OBSOLETE")
// [P]
defkeyword(paragraph,           "PARAGRAPH")
defkeyword(punctuation,         "PUNCTUATION")
defkeyword(position,            "POSITION")
defkeyword(protocol,            "PROTOCOL")
// [R]
defkeyword(range,               "RANGE")
defkeyword(read_only,           "READ-ONLY")
// [S]
defkeyword(screen,              "SCREEN")
defkeyword(state,               "STATE")
// [T]
defkeyword(text_decoration,     "TEXT-DECORATION")
// [U]
defkeyword(underline,           "UNDERLINE")
defkeyword(uri,                 "URI")
defkeyword(unknown,             "UNKNOWN")
// [W]
defkeyword(window,              "WINDOW")
defkeyword(word,                "WORD")

// %
definternal(ed, Pdefcommand, "%DEFCOMMAND")

// *
definternal(ed, Aactive_selection_styleA,   "*ACTIVE-SELECTION-STYLE*")
definternal(ed, AbuffersA,                  "*BUFFERS*")
definternal(ed, Acommand_stateA,            "*COMMAND-STATE*")
definternal(ed, Acommand_tableA,            "*COMMAND-TABLE*")
definternal(ed, Adialog_styleA,             "*DIALOG-STYLE*")
defexternal(ed, AeventA,                    "*EVENT*")
definternal(ed, Afile_check_intervalA,      "*FILE-CHECK-INTERVAL*")
definternal(ed, AframesA,                   "*FRAMES*")
definternal(ed, Aglobal_keymapA,            "*GLOBAL-KEYMAP*")
definternal(ed, AhooksA,                    "*HOOKS*")
definternal(ed, Ainactive_selection_styleA, "*INACTIVE-SELECTION-STYLE*")
definternal(ed, AmodesA,                    "*MODES*")
defexternal(ed, AprefixA,                   "*PREFIX*")
definternal(ed, AstyleA,                    "*STYLE*")
definternal(ed, Astyle_mapA,                "*STYLE-MAP*")
defexternal(ed, AselectionA,                "*SELECTION*")

// [A]
definternal(ed, active,                     "ACTIVE")
defexternal(ed, active_frame,               "ACTIVE-FRAME")
definternal(ed, active_tick,                "ACTIVE-TICK")
defexternal(ed, active_window,              "ACTIVE-WINDOW")
defexternal(ed, activate_buffer,            "ACTIVATE-BUFFER")
defexternal(ed, activate_frame,             "ACTIVATE-FRAME")
defexternal(ed, activate_window,            "ACTIVATE-WINDOW")
defexternal(ed, add_window,                 "ADD-WINDOW")
defexternal(ed, analyze_buffer,             "ANALYZE-BUFFER")
definternal(ed, arg,                        "ARG")

// [B]
definternal(ed, background,                 "BACKGROUND")
definternal(ed, bindings,                   "BINDINGS")
definternal(ed, blink,                      "BLINK")
definternal(ed, blob,                       "BLOB")
defexternal(ed, buffer,                     "BUFFER")
defexternal(ed, buffer_error,               "BUFFER-ERROR")
defexternal(ed, buffer_input_stream,        "BUFFER-INPUT-STREAM")
defexternal(ed, buffer_length,              "BUFFER-LENGTH")
defexternal(ed, buffer_match,               "BUFFER-MATCH")
defexternal(ed, buffer_name,                "BUFFER-NAME")
defexternal(ed, buffer_not_ready,           "BUFFER-NOT-READY")
defexternal(ed, buffer_output_stream,       "BUFFER-OUTPUT-STREAM")
defexternal(ed, buffer_pathname,            "BUFFER-PATHNAME")
defexternal(ed, buffer_read_only,           "BUFFER-READ-ONLY")
defexternal(ed, bufferp,                    "BUFFERP")

// [C]
definternal(ed, char_tick,                  "CHAR-TICK")
definternal(ed, child_windows,              "CHILD-WINDOWS")
defexternal(ed, close_buffer,               "CLOSE-BUFFER")
defexternal(ed, close_window,               "CLOSE-WINDOW")
defexternal(ed, color,                      "COLOR")
defexternal(ed, command,                    "COMMAND")
definternal(ed, command_state,              "COMMAND-STATE")
defexternal(ed, container,                  "CONTAINER")
definternal(ed, current,                    "CURRENT")

// [D]
defexternal(ed, defcommand,                 "DEFCOMMAND")
definternal(ed, dispatch_event,             "DISPATCH-EVENT")
definternal(ed, double_linked_item,         "DOUBLE-LINKED-ITEM")
definternal(ed, double_linked_list,         "DOUBLE-LINKED-LIST")

// [E]
definternal(ed, events,                     "EVENTS")

// [F]
definternal(ed, family,                     "FAMILY")
definternal(ed, file_state,                 "FILE-STATE")
defexternal(ed, find_buffer,                "FIND-BUFFER")
defexternal(ed, find__file_buffer,          "FIND-FILE-BUFFER")
definternal(ed, first_tracker,              "FIRST-TRACKER")
definternal(ed, first_interval,             "FIRST-INTERVAL")
definternal(ed, first_range,                "FIRST-RANGE")
definternal(ed, first_undo,                 "FIRST-UNDO")
definternal(ed, first_window,               "FIRST-WINDOW")
defexternal(ed, font,                       "FONT")
definternal(ed, font_family,                "FONT-FAMILY")
defexternal(ed, fontp,                      "FONTP")
defexternal(ed, font_size,                  "FONT-SIZE")
defexternal(ed, font_style,                 "FONT-STYLE")
defexternal(ed, font_weight,                "FONT-WEIGHT")
defexternal(ed, frame,                      "FRAME")
defexternal(ed, framep,                     "FRAMEP")

// [G]
definternal(ed, gap_end,                    "GAP-END")
definternal(ed, gap_start,                  "GAP-START")
definternal(ed, goal_x,                     "GOAL-X")
definternal(ed, goal_y,                     "GOAL-Y")
definternal(ed, ime,                        "IME")
defexternal(ed, interval,                   "INTERVAL")
defexternal(ed, intervalp,                  "INTERVALP")
definternal(ed, interval_root,              "INTERVAL-ROOT")

// [H]
definternal(ed, head,                       "HEAD")
definternal(ed, heap,                       "HEAP")

// [I]
defexternal(ed, invalid_position,           "INVALID-POSITION")
defexternal(ed, invalid_range,              "INVALID-RANGE")

// [L]
definternal(ed, last_tracker,               "LAST-TRACKER")
definternal(ed, last_interval,              "LAST-INTERVAL")
definternal(ed, last_range,                 "LAST-RANGE")
definternal(ed, last_undo,                  "LAST-UNDO")
definternal(ed, last_window,                "LAST-WINDOW")
definternal(ed, left,                       "LEFT")
defexternal(ed, lexeme,                     "LEXEME")
defexternal(ed, lexer,                      "LEXER")
definternal(ed, limit,                      "LIMIT")
definternal(ed, line_number,                "LINE-NUMBER")
defexternal(ed, load_buffer,                "LOAD-BUFFER")

// [K]
defexternal(ed, keymap,                     "KEYMAP")
defexternal(ed, keymapp,                    "KEYMAPP")
definternal(ed, keywords,                   "KEYWORDS")

// [M]
defexternal(ed, make_buffer,                "MAKE-BUFFER")
defexternal(ed, make_keymap,                "MAKE-KEYMAP")
defexternal(ed, make_lexer,                 "MAKE-LEXER")
defexternal(ed, make_range,                 "MAKE-RANGE")
defexternal(ed, make_text_window,           "MAKE-TEXT-WINDOW")
definternal(ed, marker,                     "MARKER")
defexternal(ed, mode,                       "MODE")
definternal(ed, name,                       "NAME")
definternal(ed, next,                       "NEXT")

// [O]
definternal(ed, object,                     "OBJECT")

// [P]
definternal(ed, parent,                     "PARENT")
definternal(ed, peer,                       "PEER")
definternal(ed, plist,                      "PLIST")
definternal(ed, prev,                       "PREV")
definternal(ed, prev_window,                "PREV-WINDOW")
definternal(ed, print_name,                 "PRINT-NAME")
definternal(ed, priority,                   "PRIORITY")

// [R]
defexternal(ed, range,                      "RANGE")
defexternal(ed, rangep,                     "RANGEP")
definternal(ed, read_only,                  "READ-ONLY")
definternal(ed, reconvert,                  "RECONVERT")
definternal(ed, redo,                       "REDO")
definternal(ed, right,                      "RIGHT")

// [S]
defexternal(ed, selection,                  "SELECTION")
defexternal(ed, selection_active,           "SELECTION-ACTIVE")
defexternal(ed, selection_buffer,           "SELECTION-BUFFER")
defexternal(ed, selection_end,              "SELECTION-END")
defexternal(ed, selectionp,                 "SELECTIONP")
defexternal(ed, selection_start,            "SELECTION-START")
defexternal(ed, show_window,                "SHOW-WINDOW")
definternal(ed, size,                       "SIZE")
definternal(ed, start_load_file,            "START-LOAD-FILE")
definternal(ed, state,                      "STATE")
definternal(ed, strings,                    "STRINGS")
defexternal(ed, style,                      "STYLE")
definternal(ed, style_map,                  "STYLE-MAP")
defexternal(ed, stylep,                     "STYLEP")
defexternal(ed, syntax_table,               "SYNTAX-TABLE")

// [T]
definternal(ed, table,                      "TABLE")
definternal(ed, tracker,                    "TRACKER")
definternal(ed, tail,                       "TAIL")
defexternal(ed, text_decoration,            "TEXT-DECORATION")
defexternal(ed, text_window,                "TEXT-WINDOW")
defexternal(ed, text_window_selection,      "TEXT-WINDOW-SELECTION")
definternal(ed, tick,                       "TICK")

// [U]
defexternal(ed, undo,                       "UNDO")
definternal(ed, undo_record,                "UNDO-RECORD")
definternal(ed, undo_begin_record,          "UNDO-BEGIN-RECORD")
definternal(ed, undo_delete_record,         "UNDO-DELETE-RECORD")
definternal(ed, undo_end_record,            "UNDO-END-RECORD")
definternal(ed, undo_insert_record,         "UNDO-INSERT-RECORD")
definternal(ed, undo_record_p,              "UNDO-RECORD-P")
definternal(ed, undo_state,                 "UNDO-STATE")

// [V]
definternal(ed, visible,                    "VISIBLE")

// [W]
definternal(ed, weight,                     "WEIGHT")
defexternal(ed, window,                     "WINDOW")
defexternal(ed, windowp,                    "WINDOWP")
definternal(ed, windows,                    "WINDOWS")
defexternal(ed, wrong_buffer,               "WRONG-BUFFER")

// [Command B]
definternal(ed, backward_char,              "BACKWARD-CHAR")
definternal(ed, backward_char_extend,       "BACKWARD-CHAR-EXTEND")
definternal(ed, backward_delete_char,       "BACKWARD-DELETE-CHAR")
definternal(ed, backward_delete_line,       "BACKWARD-DELETE-LINE")
definternal(ed, backward_delete_word,       "BACKWARD-DELETE-WORD")
definternal(ed, backward_line,              "BACKWARD-LINE")
definternal(ed, backward_line_extend,       "BACKWARD-LINE-EXTEND")
definternal(ed, backward_screen,            "BACKWARD-SCREEN")
definternal(ed, backward_screen_extend,     "BACKWARD-SCREEN-EXTEND")
definternal(ed, backward_window,            "BACKWARD-WINDOW")
definternal(ed, backward_window_extend,     "BACKWARD-WINDOW-EXTEND")
definternal(ed, backward_word,              "BACKWARD-WORD")
definternal(ed, backward_word_extend,       "BACKWARD-WORD-EXTEND")

// [Command C]
definternal(ed, close_other_windows,        "CLOSE-OTHER-WINDOWS")
definternal(ed, close_this_window,          "CLOSE-THIS-WINDOW")
definternal(ed, copy_to_clipboard,          "COPY-TO-CLIPBOARD")
definternal(ed, cut_to_clipboard,           "CUT-TO-CLIPBOARD")

// [Command E]
definternal(ed, end_key,                    "END-KEY")
definternal(ed, end_key_extend,             "END-KEY-EXTEND")
definternal(ed, end_of_buffer,              "END-OF-BUFFER")
definternal(ed, end_of_buffer_extend,       "END-OF-BUFFER-EXTEND")
definternal(ed, enter_key,                  "ENTER-KEY")
definternal(ed, eval_last_form,             "EVAL-LAST-FORM")

// [Command F]
definternal(ed, find_or_replace,            "FIND-OR-REPLACE")
definternal(ed, forward_char,               "FORWARD-CHAR")
definternal(ed, forward_char_extend,        "FORWARD-CHAR-EXTEND")
definternal(ed, forward_delete_char,        "FORWARD-DELETE-CHAR")
definternal(ed, forward_delete_line,        "FORWARD-DELETE-LINE")
definternal(ed, forward_delete_word,        "FORWARD-DELETE-WORD")
definternal(ed, forward_line,               "FORWARD-LINE")
definternal(ed, forward_line_extend,        "FORWARD-LINE-EXTEND")
definternal(ed, forward_screen,             "FORWARD-SCREEN")
definternal(ed, forward_screen_extend,      "FORWARD-SCREEN-EXTEND")
definternal(ed, forward_window,             "FORWARD-WINDOW")
definternal(ed, forward_window_extend,      "FORWARD-WINDOW-EXTEND")
definternal(ed, forward_word,               "FORWARD-WORD")
definternal(ed, forward_word_extend,        "FORWARD-WORD-EXTEND")

// [Command G]
definternal(ed, goto_close_paren,           "GOTO-CLOSE-PAREN")
definternal(ed, goto_close_paren_extend,    "GOTO-CLOSE-PAREN-EXTEND")
definternal(ed, goto_line,                  "GOTO-LINE")
definternal(ed, goto_open_paren,            "GOTO-OPEN-PAREN")
definternal(ed, goto_open_paren_extend,     "GOTO-OPEN-PAREN-EXTEND")
definternal(ed, graphic_key,                "GRAPHIC-KEY")

// [Command H]
definternal(ed, home_key,                   "HOME-KEY")
definternal(ed, home_key_extend,            "HOME-KEY-EXTEND")

// [Command L]
definternal(ed, list_buffer,                "LIST-BUFFER")

// [Command N]
definternal(ed, next_window,                "NEXT-WINDOW")
definternal(ed, new_frame,                  "NEW-FRAME")
definternal(ed, new_frame_and_close,        "NEW-FRAME-AND-CLOSE")

// [Command O]
definternal(ed, open_file,                  "OPEN-FILE")

// [Command P]
definternal(ed, paste_from_clipboard,       "PASTE-FROM-CLIPBOARD")
definternal(ed, previous_window,            "PREVIOUS-WINDOW")

// [Command S]
definternal(ed, save_file,                  "SAVE-FILE")
definternal(ed, select_all,                 "SELECT-ALL")
definternal(ed, split_window_horizontally,  "SPLIT-WINDOW-HORIZONTALLY")
definternal(ed, split_window_vertically,    "SPLIT-WINDOW-VERTICALLY")
definternal(ed, start_of_buffer,            "START-OF-BUFFER")
definternal(ed, start_of_buffer_extend,     "START-OF-BUFFER-EXTEND")

// TLV
deftlv(ed, AeventA,     nil)
deftlv(ed, AprefixA,    nil)
deftlv(ed, AselectionA, nil)

// Variables
defvar(ed, Aactive_selection_styleA, nil)
//defvar(ed, AbuffersA,   make_double_linked_list())
defvar(ed, AbuffersA,       nil)

defvar(ed, Acommand_tableA,
    make_hash_table(Ktest, Qeq, Ksize, Fixnum::Encode(503)) )

defvar(ed, Acommand_stateA, nil)
defvar(ed, Adialog_styleA,  nil)

defvar(ed, Afile_check_intervalA,
    Fixnum::Encode(1000 * 60) ) // one minute

defvar(ed, AframesA, nil)

defvar(ed, Aglobal_keymapA, nil)

defvar(ed, AhooksA,         nil)
defvar(ed, Ainactive_selection_styleA, nil)

defvar(ed, AmodesA, nil)

defvar(ed, AstyleA,         nil)

defvar(ed, Astyle_mapA,
    make_hash_table(Ktest, Qeq, Ksize, Fixnum::Encode(1003)) )

// Mode
#define defmode(mp_name, mp_NAME) \
    defexternal(ed, mp_name ## _mode,  mp_NAME "-MODE") \
    definternal(ed, mp_name ## _lexer, mp_NAME "-LEXER") \
    defvar(ed, mp_name ## _mode, nil)

defmode(c,              "C")
defmode(cPP,            "C++")
defmode(common_lisp,    "COMMON-LISP")
defmode(cS,             "C#")
defmode(html,           "HTML")
defmode(java,           "JAVA")
defmode(javascript,     "JAVASCRIPT")
defmode(mason,          "MASON")
defmode(perl,           "PERL")
defmode(scheme,         "SCHEME")
defmode(text,           "TEXT")
defmode(xml,            "XML")
defmode(xsd,            "XSD")
defmode(xslt,           "XSLT")

#undef defmode

#define Qsi_range   Si::Qrange

#define CLASS_si_range      Si::CLASS_range
#define CLASSD_si_range     Si::CLASSD_range

#include "../tinycl/tinycl_end_list.h"
