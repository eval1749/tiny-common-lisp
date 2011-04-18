//////////////////////////////////////////////////////////////////////////////
//
// Editor - Selection
// editor/ed_selection.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_selection.h#6 $
//
#if !defined(INCLUDE_editor_selection_h)
#define INCLUDE_editor_selection_h

#include "./ed_defs.h"

namespace Editor
{

namespace Peer
{
    class TextWindow;
} // Peer

/// <remark>
///  Selection object class.
///  <para>
///    Slots:
///    <list>
///     <item>
///         <term>active</term>
///         <description>(member :end :start)</description>
///     </item>
///     <item>
///         <term>goal-x</term>
///         <description>fixnum</description>
///     </item>
///     <item>
///       <term>goal-y</term>
///       <description>fixnum</description>
///     </item>
///     <item>
///       <term>line-number</term>
///       <description>sequence-index for reloading</description>
///     </item>
///     <item>
///       <term>window</term>
///       <description>(or window null) for associated window</description>
///     </item>
///   </list>
///  </para>
/// </remark>
class Selection : public EditorObject_<Selection, Layout_selection>
{
    public: static Val ClassD_() { return CLASSD_selection; }

    // ctor
    public: Selection(Val, Val, Val = zero, Val = zero);

    // [C]
    public: void Collapse(Val);

    // [D]
    public: Count Delete(Unit, Count);

    // [E]
    public: Count EndKey(Unit, Val = nil);
    public: Count EndOf(Unit, Val = nil);

    // [F]
    private: void forgetGoal();

    // [G]
    public: Posn GetActivePosn() const
        { return IsStartActive() ? GetStart() : GetEnd(); }

    public: Buffer* GetBuffer() const;

    public: Posn GetEnd() const
        { return m_end; }

    public: Posn GetStart() const
        { return m_start; }

    public: Peer::TextWindow* GetWindow() const;

    // [H]
    public: Count HomeKey(Unit, Val = nil);

    // [I]
    private: bool isSingleLine() const;

    public: bool IsStartActive() const
        { return Kstart == m_active; }

    // [M]
    private: Count moveAux(Val, Val, Val);
    public:  Count MoveDown(Unit, Count, Val);
    public:  Count MoveLeft(Unit, Count, Val);
    public:  Count MoveRight(Unit, Count, Val);
    public:  void  MoveTo(Posn, Val = nil);
    public:  Count MoveUp(Unit, Count, Val);

    // [P]
    public: void PrepareForReload();

    // [S]
    public: void  SetActive(Val);
    public: Posn  SetEnd(Posn);
    public: void  SetRange(Posn, Posn);
    public: void  SetRange(Val);
    public: Posn  SetStart(Posn);
    public: void  SetText(const char16* = NULL, int = 0);
    public: Count StartOf(Unit, Val = nil);

    // [T]
    public: void TypeChar(Val, Val);

    // [U]
    private: bool updateGoal();
}; // Selection

#if 0
defun(move_down, (Val, Val, Val, Val));
defun(move_left, (Val, Val, Val, Val));
defun(move_right, (Val, Val, Val, Val));
defun(move_to, (Val, Val, Val));
defun(move_up, (Val, Val, Val, Val));
#endif

} // Editor

#endif //!defined(INCLUDE_editor_selection_h)
