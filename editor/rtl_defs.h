//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime Functions
// editor/ed_rtl.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_defs.h#8 $
//
#if !defined(INCLUDE_editor_rtl_h)
#define INCLUDE_editor_rtl_h

#include "./ed_defs.h"
#include "./ed_objects.h"

namespace Editor
{

/// <summary>
///   Lisp class syntax-table.
/// </summary>
class Command : public EditorObject_<Command, Layout_command>
{
    public: static Val ClassD_() { return CLASSD_command; }

    public: Command(Val name, Val fn, Val plist = nil)
    {
        m_function = fn;
        m_name     = name;
        m_plist    = plist;
    } // SyntaxTable
}; // Command

/// <summary>
///   Lisp class syntax-table.
/// </summary>
class SyntaxTable : public EditorObject_<SyntaxTable, Layout_syntax_table>
{
    public: static Val ClassD_() { return CLASSD_syntax_table; }

    public: SyntaxTable(Val name)
    {
        m_name   = name;
        m_vector = make_vector(Fixnum::Encode(127));
    } // SyntaxTable
}; // SyntaxTable

// [A]
defun(activate_window, (Val));
defun(add_window, (Val, Val));
//defun(allocate_buffer, (Val));
//defun(append_item, (Val, Val));

// [B]
inline bool bufferp(Val x)  { return typep(x, CLASS_buffer); }
defun(buffer_match, (Val, Val));
inline bool buffer_p(Val x) { return bufferp(x); }

// [C]
defpred(can_close_p, (Val));
defun(char_syntax, (Val, Val));
defun_setf(char_syntax, (Val, Val, Val));
defun(close_buffer, (Val));
defun(close_window, (Val));

// [D]
//defun(delete_item, (Val, Val));

inline defpred(double_linked_item_p, (Val x))
    { return typep(x, CLASS_double_linked_item); }

inline defpred(double_linked_list_p, (Val x))
    { return typep(x, CLASS_double_linked_list); }

// [F]
defun(find_buffer, (Val));
inline bool framep(Val x)  { return typep(x, CLASS_frame); }
inline bool frame_p(Val x) { return framep(x); }

// [H]
defun(hide_window, (Val));

// [K]
inline bool keymapp(Val x)  { return typep(x, CLASS_keymap); }
inline bool keymap_p(Val x) { return keymapp(x); }

// [M]
defun(make_buffer, (Val));
Val make_buffer_input_stream(Val);
Val make_buffer_output_stream(Val, Val = nil);
defun(make_double_linked_list, ());
defun(make_keymap, ());
Val make_range(Val);
Val make_range(Val, Val);
Val make_range(Val, Val, Val);
defun(make_syntax_table, (Val));

inline Val make_buffer(const char* psz)
    { return make_buffer(make_string(psz)); }

// [R]
inline bool rangep(Val x)  { return typep(x, CLASS_range); }
inline bool range_p(Val x) { return rangep(x); }
defun(rename_buffer, (Val, Val));

// [S]
defun(show_window, (Val));

inline bool selectionp(Val x)  { return typep(x, CLASS_selection); }
inline bool selection_p(Val x) { return selectionp(x); }

inline bool syntax_table_p(Val x) { return x->Is<SyntaxTable>(); }

// [W]
inline bool windowp(Val x)  { return typep(x, CLASS_window); }
inline bool window_p(Val x) { return windowp(x); }

} // Editor

#endif //!defined(INCLUDE_editor_rtl_h)
