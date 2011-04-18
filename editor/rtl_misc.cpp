#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Misc
// editor/ed_rtl_misc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_misc.cpp#4 $
//
//
#include "./rtl_defs.h"

#include "./cm_keymap.h"
#include "./cm_mode.h"

#include "./ed_buffer.h"
#include "./ed_range.h"

namespace Editor
{

using namespace TinyCl;

#if 0
// [A]
defun(append_item, (Val dlist, Val item))
{
    check_type(dlist, double_linked_list);
    check_type(item,  double_linked_item);

    DoubleLinkedList* pList = dlist->StaticCast<DoubleLinkedList>();
    return pList->Append(item->StaticCast<DoubleLinkedItem>());
} // append_item
#endif

// [C]
defun(char_syntax, (Val ch, Val syntab))
{
    check_type(ch, character);
    check_type(syntab, syntax_table);

    SyntaxTable* pSynTab  = syntab->StaticCast<SyntaxTable>();

    Int wch = ch->StaticCast<Character>()->ToCode();

    if (SimpleVector* p = pSynTab->m_vector->DynamicCast<SimpleVector>())
    {
        if (wch < p->GetLength())
        {
            return p->GetStart()[wch];
        }
    } // if

    return nil;
} // char_syntax

defun_setf(char_syntax, (Val newval, Val ch, Val syntab))
{
    check_type(ch, character);
    check_type(syntab, syntax_table);

    SyntaxTable* pSynTab  = syntab->StaticCast<SyntaxTable>();

    Int wch = ch->StaticCast<Character>()->ToCode();

    if (SimpleVector* p = pSynTab->m_vector->DynamicCast<SimpleVector>())
    {
        if (wch < p->GetLength())
        {
            return p->GetStart()[wch] = newval;
        }
    } // if

    return newval;
} // char_syntax

#if 0
// [D]
defun(delete_item, (Val dlist, Val item))
{
    check_type(dlist, double_linked_list);
    check_type(item,  double_linked_item);

    DoubleLinkedList* pList = dlist->StaticCast<DoubleLinkedList>();
    return pList->Delete(item->StaticCast<DoubleLinkedItem>());
} // append_item
#endif

// [M]
defun(make_double_linked_list, ())
{
    DoubleLinkedList* pList = new DoubleLinkedList;
    return pList->Encode();
} // make_double_linked_list

defun(make_keymap, ())
{
    Keymap* pKeymap = new Keymap;
    return pKeymap->Encode();
} // make_keymap

defun(make_syntax_table, (Val name))
{
    SyntaxTable* p = new SyntaxTable(name);
    return p->Encode();
} // make_syntax_table

// [P]
defmethod(print_object, command, (Val x, Val s))
{
    Command* p = x->StaticCast<Command>();
    format(s, "#<Command ~S>", p->m_name);
    return x;
} // print_object

} // Editor
