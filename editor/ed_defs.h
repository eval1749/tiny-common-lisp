//////////////////////////////////////////////////////////////////////////////
//
// Editor - Declarations
// editor/ed_defs.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_defs.h#10 $
//
#if !defined(INCLUDE_editor_defs_h)
#define INCLUDE_editor_defs_h

#include "./ed_config.h"
#include "./ed_layout.h"
#include "./ed_objects.h"

namespace Editor
{

namespace Si = TinyCl;
using Si::Character;
using Si::Charset;
using Si::ClassD;
using Si::Datum;
using Si::ExternalFormat;
using Si::Fixnum;
using Si::List;
using Si::Record;
using Si::SimpleString;
using Si::SimpleVector;
using Si::StringObject;
using Si::Thread;
using Si::ValueCell;

using Si::make_platform_error;

using Si::Qend;
using Si::Qrealize_instance;
using Si::Qsequence_index;
using Si::Qstart;
using Si::Qunrealize_instance;

inline Val realize_instance(Val x)
{
    return funcall(Qrealize_instance, x);
} // realize_instance

inline Val unrealize_instance(Val x)
{
    return funcall(Qunrealize_instance, x);
} // unrealize_instance

enum Char
{
    BackQuote       = 0x60,
    Backslash       = 0x5C,
    CloseBrace      = 0x7D,
    CloseBracket    = 0x5D,
    CloseParen      = 0x29,
    CaredgeReturn   = 0x0D,
    DoubleQuote     = 0x22,
    LineFeed        = 0x0A,
    Newline         = LineFeed,
    OpenBrace       = 0x7B,
    OpenBracket     = 0x5B,
    OpenParen       = 0x28,
    Page            = 0x0C,
    SingleQuote     = 0x27,
    Space           = 0x20,
    Tab             = 0x09,
}; // Char

inline Val xadd(Val a, Val b)
{
    check_type(a, fixnum);
    check_type(b, fixnum);
    return Fixnum::Encode(Fixnum::Decode_(a) + Fixnum::Decode_(b));
} // xadd

inline bool xge(Val a, Val b)
{
    check_type(a, fixnum);
    check_type(b, fixnum);
    return Fixnum::Decode_(a) >= Fixnum::Decode_(b);
} // xle

inline bool xgt(Val a, Val b)
{
    check_type(a, fixnum);
    check_type(b, fixnum);
    return Fixnum::Decode_(a) > Fixnum::Decode_(b);
} // xgt

inline bool xle(Val a, Val b)
{
    check_type(a, fixnum);
    check_type(b, fixnum);
    return Fixnum::Decode_(a) <= Fixnum::Decode_(b);
} // xle

inline bool xlt(Val a, Val b)
{
    check_type(a, fixnum);
    check_type(b, fixnum);
    return Fixnum::Decode_(a) < Fixnum::Decode_(b);
} // xlt

inline Val xsub(Val a, Val b)
{
    check_type(a, fixnum);
    check_type(b, fixnum);
    return Fixnum::Encode(Fixnum::Decode_(a) - Fixnum::Decode_(b));
} // xsub

inline Val xadd(Val a, Int i) { return xadd(a, Fixnum::Encode(i)); }
inline Val xsub(Val a, Int i) { return xsub(a, Fixnum::Encode(i)); }

inline Val xmin(Val a, Val b) { return xlt(a, b) ? a : b; }
inline Val xmax(Val a, Val b) { return xgt(a, b) ? a : b; }

inline Int Ceiling(Int a, Int b)
{
    return (a + b - 1) / b;
} // Ceiling

inline Int RoundUp(Int a, Int b)
{
    return Ceiling(a, b) * b;
} // RoundUp

typedef Val Count;
typedef Val Posn;
typedef Val Unit;

template<typename T, typename L>
class EditorObject_ : public Si::Record_<T, L>
{
    public: void* operator new(size_t, Val x)
    {
        return x->StaticCast<Si::Record>();
    } // new

    public: void* operator new(size_t)
    { 
        Val x = Thread::Get()->AllocRecord(T::ClassD_());
        return x->StaticCast<Si::Record>();
    } // new
}; // EdtiorObject_

/// <summary>
///  Represents double linked item.
/// </summary>
class DoubleLinkedItem :
    public Si::Record_<DoubleLinkedItem, Layout_double_linked_item>
{
}; // DoubleLinkedItem

// <summary>
//  Represents double linked list.
// </summary>
// <seealso cref="DoubleLinkedItem"/>
template<class List_, class Element_, class Base_ = List_>
class DoubleLinkedList_ : public Base_
{
    private: typedef DoubleLinkedList_<List_, Element_, Base_> This;

    // [A]
    public: Element_* Append(Element_* p)
        { return Append(p->Encode())->StaticCast<Element_>(); }

    public: Val Append(Val item)
    {
        Element_* pItem = item->StaticCast<Element_>();
        pItem->Unlink();

        if (nil == GetHead())
        {
            SetHead(item);
        }

        Val tail = GetTail();
        if (nil != tail)
        {
            tail->StaticCast<Element_>()->SetNext(item);
        }
        
        pItem->SetPrev(tail);
        return SetTail(item);
    } // Append

    // [D]
    public: Element_* Delete(Element_* p)
        { return Delete(p->Encode())->StaticCast<Element_>(); }

    public: Val Delete(Val item)
    {
        Element_* pItem = item->StaticCast<Element_>();

        Val next = pItem->GetNext();
        Val prev = pItem->GetPrev();

        if (nil == next)
        {
            SetTail(prev);
        }
        else
        {
            next->StaticCast<Element_>()->SetPrev(prev);
        }

        if (nil == prev)
        {
            SetHead(next);
        }
        else
        {
            prev->StaticCast<Element_>()->SetNext(next);
        }

        pItem->Unlink();

        return item;
    } // Delete

    // [E]
    public: class Enum
    {
        private: Val m_runner;

        public: Enum(const This* p) :
            m_runner(p->GetHead()) {}

        public: bool AtEnd() const { return nil == m_runner; }
        public: Val  Get()   const { ASSERT(!AtEnd()); return m_runner; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_runner = m_runner->StaticCast<Element_>()->GetNext();
        } // Next
    }; // Enum

    public: class EnumReverse
    {
        private: Val m_runner;

        public: EnumReverse(const This* p) :
            m_runner(p->GetTail()) {}

        public: bool AtEnd() const { return nil == m_runner; }
        public: Val  Get()   const { ASSERT(!AtEnd()); return m_runner; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_runner = m_runner->StaticCast<Element_>()->GetPrev();
        } // Next
    }; // EnumReverse

    // [I]
    public: bool IsEmpty() const
    {
        if (nil != GetHead()) return false;
        ASSERT(nil == GetTail());
        return true;
    } // IsEmpty

    public: Element_* InsertAfter(Element_* p, Element_* r)
        { return InsertAfter(p->Encode(), r->Encode())->StaticCast<Element_>(); }

    public: Val InsertAfter(Val item, Val ref_item)
    {
        Val next = ref_item->StaticCast<Element_>()->GetNext();
        if (nil == next)
        {
            SetTail(item);
        }
        else
        {
            next->StaticCast<Element_>()->SetPrev(item);
        }

        item->StaticCast<Element_>()->SetPrev(ref_item);
        item->StaticCast<Element_>()->SetNext(next);
        ref_item->StaticCast<Element_>()->SetNext(item);
        return item;
    } // InsertAfter

    public: Element_* InsertBefore(Element_* p, Element_* r)
        { return InsertBefore(p->Encode(), r->Encode())->StaticCast<Element_>(); }

    public: Val InsertBefore(Val item, Val ref_item)
    {
        Val prev = ref_item->StaticCast<Element_>()->GetPrev();
        if (nil == prev)
        {
            SetHead(item);
        }
        else
        {
            prev->StaticCast<Element_>()->SetNext(item);
        }

        item->StaticCast<Element_>()->SetPrev(prev);
        item->StaticCast<Element_>()->SetNext(ref_item);
        ref_item->StaticCast<Element_>()->SetPrev(item);
        return item;
    } // InsertBefore

    // [P]
    public: Element_* Prepend(Element_* p)
        { return Prepend(p->Encode())->StaticCast<Element_>(); }

    public: Val Prepend(Val item)
    {
        Element_* pItem = item->StaticCast<Element_>();
        pItem->Unlink();

        if (nil == GetLast())
        {
            SetTail(item);
        }

        if (nil != GetHead())
        {
            GetHead()->StaticCast<Element_>()->SetPrev(item);
        }

        return SetHead(item);
    } // Prepend
}; // DoubleLinkedList_

/// <summary>
///  Represents double linked list.
/// </summary>
/// <seealso cref="DoubleLinkedItem"/>
class DoubleLinkedList :
    public DoubleLinkedList_<
        DoubleLinkedList,
        DoubleLinkedItem,
        EditorObject_<DoubleLinkedList, Layout_double_linked_list> >
{
    public: static Val ClassD_() { return CLASSD_double_linked_list; }

    public: DoubleLinkedList()
    {
        m_head = nil;
        m_tail = nil;
    } // DoubleLinkedList

    public: Val GetHead() const { return m_head; }
    public: Val GetTail() const { return m_tail; }
    public: Val SetHead(Val x)  { return m_head = x; }
    public: Val SetTail(Val x)  { return m_tail = x; }
}; // DoubleLinkedList

class Application;
class Buffer;
class Frame;
class Interval;
class Range;
class Selection;
class Style;
class TextWindow;
class Tracker;
class Window;

namespace Peer
{
    class Window;
        class Container;
        class Frame;
    class TextWindow;
} // Peer

void __declspec(noreturn) PlatformError(const char*);


// Compute Adler32(RFC1950)
inline UInt ComputeHash(UInt nAcc, UInt nDatum)
{
    const UInt k_nBase = 65521; // largest prime small than 65536
    UInt s1 = nAcc & 0xFFFF;
    UInt s2 = (nAcc >> 16) & 0xFFFF;
    s1 = (s1 + nDatum) % k_nBase;
    s2 = (s2 + s1) % k_nBase;
    return (s2 << 16) | s1;
} // ComputeHash

enum Key
{
    Key_UnAssigned  = 0x000,

    Key_Space       = 0x20,
    Key_Exclamation = 0x21,
    Key_DoubleQuote = 0x22,
    Key_NumberSign  = 0x23,
    Key_Dollar      = 0x24,
    Key_Percent     = 0x25,
    Key_Ampersand   = 0x26,
    Key_SingleQuote = 0x27,
    Key_OpenParen   = 0x28,
    Key_CloseParen  = 0x29,
    Key_Asterisk    = 0x2A,
    Key_Plus        = 0x2B,
    Key_Comma       = 0x2C,
    Key_Minus       = 0x2D,
    Key_Period      = 0x2E,
    Key_Slash       = 0x2F,

    Key_0 = 0x30, Key_1 = 0x31, Key_2 = 0x32, Key_3 = 0x33,
    Key_4 = 0x34, Key_5 = 0x35, Key_6 = 0x36, Key_7 = 0x37,
    Key_8 = 0x38, Key_9 = 0x39,
    Key_Colon       = 0x3A,
    Key_SemiColon   = 0x3B,
    Key_LessThan    = 0x3C,
    Key_Equal       = 0x3D,
    Key_GraterThan  = 0x3E,
    Key_Question    = 0x3F,

    Key_At = 0x40,
                   Key_A = 0x41,  Key_B = 0x42, Key_C = 0x43,
    Key_D = 0x44,  Key_E = 0x045, Key_F = 0x46, Key_G = 0x47,
    Key_H = 0x48,  Key_I = 0x049, Key_J = 0x4A, Key_K = 0x4B,
    Key_L = 0x4C,  Key_M = 0x04D, Key_N = 0x4E, Key_O = 0x4F,
    Key_P = 0x50,  Key_Q = 0x051, Key_R = 0x52, Key_S = 0x53,
    Key_T = 0x54,  Key_U = 0x055, Key_V = 0x56, Key_W = 0x57,
    Key_X = 0x58,  Key_Y = 0x059, Key_Z = 0x5A,

    Key_OpenBracket         = 0x5B,
    Key_Backslash           = 0x5C,
    Key_CloseBracket        = 0x5D,
    Key_CircumflexAccessnt  = 0x5E,
    Key_LowLine             = 0x5F,

    Key_Backspace   = 0x100 | VK_BACK,
    Key_Delete      = 0x100 | VK_DELETE,
    Key_Down        = 0x100 | VK_DOWN,
    Key_End         = 0x100 | VK_END,
    Key_Enter       = 0x100 | VK_RETURN,
    Key_Home        = 0x100 | VK_HOME,
    Key_Insert      = 0x100 | VK_INSERT,
    Key_Left        = 0x100 | VK_LEFT,
    Key_PageDown    = 0x100 | VK_NEXT,
    Key_PageUp      = 0x100 | VK_PRIOR,
    Key_Right       = 0x100 | VK_RIGHT,
    Key_Tab         = 0x100 | VK_TAB,
    Key_Up          = 0x100 | VK_UP,
}; // Key

#if 0
enum Key
{
    Key_First   = 0x100,

    Key_LeftButton,
    Key_MiddleButton,
    Key_RightButton,

    Key_Backspace,
    Key_Delete,
    Key_Down,
    Key_End,
    Key_Enter,
    Key_Escape,
    Key_F1,  Key_F2,  Key_F3,  Key_F4,  Key_F5,
    Key_F6,  Key_F7,  Key_F8,  Key_F9,  Key_F10,
    Key_F11, Key_F12, Key_F13, Key_F14, Key_F15,
    Key_F16, Key_F17, Key_F18, Key_F19, Key_F20,
    Key_F21, Key_F22, Key_F23, Key_F24,
    Key_Home,
    Key_Insert,
    Key_Left,
    Key_PageUp,
    Key_PageDown,
    Key_Right,
    Key_Tab,
    Key_Up,
}; // Key
#endif

enum Modifier
{
    Modifier_Control = 0x200,
    Modifier_Shift   = 0x400,
}; // Modifier

Val run_hooks(Val);
Val run_hooks(Val, Val);

} // Editor

#include "./ed_util.h"

#endif //!defined(INCLUDE_editor_defs_h)
