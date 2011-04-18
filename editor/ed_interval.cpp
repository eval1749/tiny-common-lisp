#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Interval
// editor/ed_interval.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_interval.cpp#9 $
//
//
#define DEBUG_INTERVAL 1
#define DEBUG_STYLE    0
#include "./ed_interval.h"

#include "./ed_buffer.h"

namespace Editor
{

namespace
{

class IntervalNode : public Interval
{
    // ctor
    public: IntervalNode(BufferCore* p, Posn s, Posn e) :
        Interval(p->Encode(), s, e) {}

    // [C]
    public: bool Contains(Val posn) const
    {
        return xge(posn, m_start) && xlt(posn, m_end);
    } // Contains

    // [D]
    public: Val Delete()
    {
        if (nil == m_left)
        {
            return m_right;
        }

        if (nil == m_right)
        {
            return m_left;
        }

        if (xlt(m_left->StaticCast<IntervalNode>()->m_priority,
                m_right->StaticCast<IntervalNode>()->m_priority ) )
        {
            Val temp = RotateRight();
            temp->StaticCast<IntervalNode>()->m_right = Delete();
            return temp;
        }

        {
            Val temp = RotateLeft();
            temp->StaticCast<IntervalNode>()->m_left = Delete();
            return temp;
        }
    } // Delete

    // [I]
    public: bool IsPriority(Val that) const
    {
        return xlt(m_priority, that->StaticCast<IntervalNode>()->m_priority);
    } // IsPriority

    // [L]
    public: bool LessThan(Val that) const
    {
        return xlt(m_start, that->StaticCast<IntervalNode>()->m_start);
    } // LessThan

    // [R]
    public: Val RotateLeft()
    {
        Val temp = m_right;
        m_right  = m_right->StaticCast<IntervalNode>()->m_left;
        temp->StaticCast<IntervalNode>()->m_left = Encode();
        return temp;
    } // RotateLeft

    public: Val RotateRight()
    {
        Val temp = m_left;
        m_left = m_left->StaticCast<IntervalNode>()->m_right;
        temp->StaticCast<IntervalNode>()->m_right = Encode();
        return temp;
    } // RotateLeft
}; // IntervalNode

class IntervalRoot : public Buffer
{
    public: Val GetRoot() const { return m_interval_root; }
    public: Val SetRoot(Val x)  { return m_interval_root = x; }
}; // IntervalRoot

template<class Root_, class Node_>
class BinaryTree_ : public Root_
{
    // [D]
    public: void Delete(Node_* p)
        { Delete(p->Encode()); }

    public: void Delete(Val node)
    {
        SetRoot(deleteAux(GetRoot(), node));
    } // Delete

    private: static Val deleteAux(Val tree, Val node)
    {
        // pNode must be in pTree.
        ASSERT(nil != tree);

        if (tree == node)
        {
            return tree->StaticCast<Node_>()->Delete();
        }

        if (node->StaticCast<Node_>()->LessThan(tree))
        {
            tree->StaticCast<Node_>()->m_left =
                deleteAux(tree->StaticCast<Node_>()->m_left, node);
        }
        else
        {
            tree->StaticCast<Node_>()->m_right =
                deleteAux(tree->StaticCast<Node_>()->m_right, node);
        }

        return tree;
    } // deleteAux

    // [I]
    public: Node_* Insert(Node_* p)
        { return Insert(p->Encode())->StaticCast<Node_>(); }

    public: Val Insert(Val node)
    {
        SetRoot(insertAux(GetRoot(), node));
        return node;
    } // Insert

    private: static Val insertAux(Val tree, Val node)
    {
        if (nil == tree)
        {
            return node;
        }

        Node_* pTree = tree->StaticCast<Node_>();

        if (pTree->LessThan(node))
        {
            Val right = insertAux(pTree->m_right, node);
            pTree->m_right = right;
            if (pTree->IsPriority(right))
            {
                tree = pTree->RotateLeft();
            }
        }
        else
        {
            Val left = insertAux(pTree->m_left, node);
            pTree->m_left = left;
            if (pTree->IsPriority(left))
            {
                tree = pTree->RotateRight();
            }
        } // if

        return tree;
    } // Insert
}; // BinaryTree_

typedef BinaryTree_<IntervalRoot, IntervalNode> IntervalTree;

//  Returns random number for Treap.
//
static int TreapRandom()
{
    static uint32 s_nRandom = ::GetTickCount();
    s_nRandom =  s_nRandom * 1664525 + 1013904223;
    return s_nRandom & ((1<<28)-1);
} // TreapRandom

} // namespace

/// <summary>
///   Construct an Interval object.
/// </summary>
Interval::Interval(Val buffer, Val start, Val end)
{
    //buffer->StaticCast<Buffer>()->CheckRange(start, end);

    m_buffer = buffer;

    m_start = start;
    m_end   = end;

    m_left  = nil;
    m_right = nil;

    m_priority = Fixnum::Encode(TreapRandom() & Fixnum::MostPositive);

    // style
    m_color           = nil;
    m_background      = nil;
    m_marker          = nil;
    m_font_family     = nil;
    m_font_size       = nil;
    m_font_style      = nil;
    m_font_weight     = nil;
    m_state           = nil;
    m_text_decoration = nil;
} // Interval

// [C]
//  Returns true if this interval and pIntv can be merged. If two
//  intervals are mergeable, both intervals have equivalent styles.
//
bool Interval::CanMerge(const Interval* p) const
{
    #define compare(mp_name) \
        if (m_ ## mp_name != p->m_ ## mp_name) return false;

    // FIXME 2008-01-19 yosi@msn.com How do we compare font-family?
    compare(background);
    compare(color);
    compare(font_family);
    compare(font_size);
    compare(font_weight);
    compare(marker);
    compare(state);
    compare(text_decoration);

    return true;
} // Interval::CanMerge

// [D]
void BufferCore::deleteInterval(Val intv)
{
    To<Intervals>()->Delete(intv);
    To<IntervalTree>()->Delete(intv);
} // BufferCore::deleteInterval

// [G]
Buffer* Interval::GetBuffer() const
    { return m_buffer->StaticCast<Buffer>(); }

Val BufferCore::GetIntervalAt(Posn posn) const
{
    posn = xlt(posn, m_length) ? posn : m_length;
    Val runner = m_interval_root;
    while (nil != runner)
    {
        IntervalNode* pIntv = runner->StaticCast<IntervalNode>();
        if (pIntv->Contains(posn))
        {
            break;
        }

        if (xlt(posn, pIntv->m_start))
        {
            runner = pIntv->m_left;
        }
        else
        {
            runner = pIntv->m_right;
        }
    } // while

    #if _DEBUG
        if (nil == runner)
        {
            DEBUG_FORMAT("~D isn't in tree.~%", posn);

            foreach (Intervals::Enum, oEnum, To<Intervals>())
            {
                IntervalNode* pIntv = oEnum.Get()->StaticCast<IntervalNode>();

                DEBUG_FORMAT("list [~D,~D] ~S~%",
                    pIntv->m_start, pIntv->m_end, oEnum.Get() );
            } // for each intv

            runner = m_interval_root;
            while (nil != runner)
            {
                IntervalNode* pIntv = runner->StaticCast<IntervalNode>();

                DEBUG_FORMAT("tree [~D,~D] ~S~%",
                    pIntv->m_start, pIntv->m_end, runner );

                if (xlt(posn, pIntv->m_start))
                {
                    runner = pIntv->m_left;
                }
                else
                {
                    runner = pIntv->m_right;
                }
            } // while

            CAN_NOT_HAPPEN();
        } // if
    #else
        ASSERT(nil != runner);
    #endif

    return runner;
} // BufferCore::GetIntervalAt

// [S]
void BufferCore::setStyle(Posn start, Posn end, Val keyvals)
{
    ASSERT(start != end);

    // To improve perforamnce, we dont' check contents of style.
    // This may be enough for syntax coloring.
    m_tick = xxadd(m_tick, 1);

    // Get interval pHead containing start posn.
    Val head = GetIntervalAt(start);
    Interval* pHead = head->StaticCast<Interval>();

    Posn head_end   = pHead->GetEnd();
    Posn head_start = pHead->GetStart();

    if (head_start == start && head_end == end)
    {
        // pHead: ---s......e---
        // Range: ---s......e---
        pHead->SetStyle(keyvals);
        tryMergeInterval(pHead);
        return;
    }

    if (head_end < end)
    {
        // pHead: --s...e----
        // Range: ----s.....e----
        setStyle(start,    head_end, keyvals);
        setStyle(head_end, end,     keyvals);
        return;
    } // if

    // New style is compatibile with existing one.
    Interval oIntv(Encode(), start, end);
    oIntv.SetStyle(keyvals);

    if (oIntv.CanMerge(pHead))
    {
        return;
    }

    if (head_start == start)
    {
        // pHead: ---s........e---
        // pTail: --------s...e---
        // Range: ---s....e-------
        pHead->m_start = end;

        if (Interval* pPrev = pHead->GetPrev()->DynamicCast<Interval>())
        {
            if (oIntv.CanMerge(pPrev))
            {
                pPrev->m_end = end;
                return;
            }
        }

        IntervalNode* pIntv = new IntervalNode(this, start, end);
        pIntv->SetStyle(keyvals);

        To<Intervals>()->InsertBefore(pIntv, pHead);
        To<IntervalTree>()->Insert(pIntv);

        ASSERT(pHead->GetPrev() == pIntv->Encode());
        ASSERT(pIntv->GetNext() == pHead->Encode());

        #if DEBUG_INTERVAL
            ASSERT(GetIntervalAt(pIntv->GetStart()) == pIntv->Encode());
        #endif

        return;
    } // if

    if (head_end == end)
    {
        // pHead: ---s........e---
        // Range: -------s....e---
        pHead->m_end = start;

        if (Interval* pNext = pHead->GetNext()->DynamicCast<Interval>())
        {
            if (oIntv.CanMerge(pNext))
            {
                pNext->m_start = start;
                return;
            }
        }

        IntervalNode* pIntv = new IntervalNode(this, start, end);
        pIntv->SetStyle(keyvals);

        To<Intervals>()->InsertAfter(pIntv, pHead);
        To<IntervalTree>()->Insert(pIntv);

        ASSERT(pHead->GetNext() == pIntv->Encode());
        ASSERT(pIntv->GetPrev() == pHead->Encode());

        #if DEBUG_INTERVAL
            ASSERT(GetIntervalAt(pIntv->GetStart()) == pIntv->Encode());
        #endif

        return;
    } // if

    {
        // pHead: ---s...........e---
        // pTail: ----------s....e---
        // Range: -----s....e--------
        pHead->m_end = start;

        IntervalNode* pTail = new IntervalNode(this, end, head_end);
        pTail->SetStyle(pHead);

        To<Intervals>()->InsertAfter(pTail, pHead);
        To<IntervalTree>()->Insert(pTail);

        ASSERT(pHead->GetNext() == pTail->Encode());
        ASSERT(pTail->GetPrev() == pHead->Encode());

        #if DEBUG_INTERVAL
            ASSERT(GetIntervalAt(pTail->GetStart()) == pTail->Encode());
        #endif

        IntervalNode* pIntv = new IntervalNode(this, start, end);
        pIntv->SetStyle(keyvals);

        To<Intervals>()->InsertAfter(pIntv, pHead);
        To<IntervalTree>()->Insert(pIntv);

        ASSERT(pHead->GetNext() == pIntv->Encode());
        ASSERT(pIntv->GetPrev() == pHead->Encode());

        #if DEBUG_INTERVAL
            ASSERT(GetIntervalAt(pIntv->GetStart()) == pIntv->Encode());
        #endif

        return;
    }
} // BufferCore::setStyle

void Interval::SetStyle(const Interval* p)
{
    m_background      = p->m_background;
    m_color           = p->m_color;
    m_font_family     = p->m_font_family;
    m_font_size       = p->m_font_size;
    m_font_style      = p->m_font_style;
    m_font_weight     = p->m_font_weight;
    m_marker          = p->m_marker;
    m_state           = p->m_state;
    m_text_decoration = p->m_text_decoration;
} // Interval::SetStyle

void Interval::SetStyle(Val keyvals)
{
    if (! listp(keyvals))
    {
        return;
    }

    m_background      = nil;
    m_color           = nil;
    m_font_family     = nil;
    m_font_weight     = nil;
    m_marker          = nil;
    m_text_decoration = nil;

    foreach (List::Enum, oEnum, keyvals)
    {
        Val key = oEnum.Get();
        oEnum.Next();

        Val val = oEnum.Get();

        #if DEBUG_STYLE
            DEBUG_FORMAT("[~D,~D] ~S=~S~%", m_start, m_end, key, val);
        #endif

        if (Kbackground == key)           { m_background = val; }
        else if (Kcolor == key)           { m_color = val; }
        else if (Kfont_family == key)     { m_font_family = val; }
        else if (Kfont_size   == key)     { m_font_size = val; }
        else if (Kfont_weight == key)     { m_font_weight = val; }
        else if (Kmarker      == key)     { m_marker = val; }
        else if (Kstate     == key)       { m_state = val; }
        else if (Ktext_decoration == key) { m_text_decoration = val; }
        else
        {
            error(Qtype_error,
                Kdatum, key,
                Kexpected_type, list(Qmember,
                    Kbackground,
                    Kcolor,
                    Kfont_family,
                    Kfont_size,
                    Kfont_weight,
                    Kmarker,
                    Ktext_decoration ) );
        } // if
    } // for
} // Interval::SetStyle

// [T]
Interval* BufferCore::tryMergeInterval(Interval* pIntv)
{
    // Merge to previous
    if (Interval* pPrev = pIntv->GetPrev()->DynamicCast<Interval>())
    {
        ASSERT(pPrev->GetEnd()  == pIntv->GetStart());
        ASSERT(pPrev->GetNext() == pIntv->Encode());

        if (pIntv->CanMerge(pPrev))
        {
            Posn end = pIntv->GetEnd();
            To<Intervals>()->Delete(pIntv);
            To<IntervalTree>()->Delete(pIntv->Encode());
            pIntv = pPrev;
            pIntv->m_end = end;

            #if DEBUG_INTERVAL
                DEBUG_FORMAT("merge prev [~D, ~D]~%",
                    pIntv->m_start, pIntv->m_end );
            #endif
        } // if can merge
    } // if prev

    // Absobe next
    if (Interval* pNext = pIntv->GetNext()->DynamicCast<Interval>())
    {
        if (pIntv->CanMerge(pNext))
        {
            Posn end = pNext->GetEnd();
            To<Intervals>()->Delete(pNext);
            To<IntervalTree>()->Delete(pNext->Encode());
            pIntv->m_end = end;

            #if DEBUG_INTERVAL
                DEBUG_FORMAT("absobe next [~D, ~D]~%",
                    pIntv->m_start, pIntv->m_end );
            #endif
        } // if can merge
    } // if next

    return pIntv;
} // BufferCore::tryMergeInterval

} // Editor
