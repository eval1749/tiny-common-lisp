#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Selection
// editor/ed_selection.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_selection.cpp#1 $
//
//
#include "./rtl_defs.h"

#include "./ed_buffer.h"
#include "./ed_range.h"
#include "./ed_selection.h"


namespace Editor
{

// [M]

defun(move_down, (Val sel, Val unit, Val n, Val extendp))
{
    check_type(sel, selection);
    Selection* pSelection = sel->StaticCast<Selection>();
    return pSelection->MoveDown(unit, n, extendp);
} // move_down

//  Unit        | Key Combination
//  ------------+----------------
//  Char        | Left
/// Word        | Ctrl+Left
defun(move_left, (Val sel, Val unit, Val n, Val extendp))
{
    check_type(sel, selection);
    Selection* pSelection = sel->StaticCast<Selection>();
    return pSelection->MoveLeft(unit, xsub(zero, n), extendp);
} // move_left

//  Unit        | Key Combination
//  ------------+----------------
//  Char        | Right
/// Word        | Ctrl+Right
//
// Note: If selection isn't nondegenerate and fExtend is false, selection
// becomes degenerate.
defun(move_right, (Val sel, Val unit, Val n, Val extendp))
{
    check_type(sel, selection);
    Selection* pSelection = sel->StaticCast<Selection>();
    return pSelection->MoveRight(unit, n, extendp);
} // move_right

defun(move_to, (Val sel, Val posn, Val extendp))
{
    check_type(sel, selection);
    Selection* pSelection = sel->StaticCast<Selection>();
    pSelection->MoveTo(posn, extendp);
    return sel;
} // move_to

defun(move_up, (Val sel, Val unit, Val n, Val extendp))
{
    check_type(sel, selection);
    Selection* pSelection = sel->StaticCast<Selection>();
    return pSelection->MoveUp(unit, xsub(zero, n), extendp);
} // move_up

} // Editor
