#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Stream
// tinycl_rtl_stream.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_seq.cpp#2 $
//
#include "../tinycl.h"

namespace TinyCl
{

using namespace Private;

static Val unsafe_elt(Val seq, Val index)
{
    Int iIndex = Fixnum::Decode_(index);

    if (listp(seq))
    {
        foreach (List::Enum, oEnum, seq)
        {
            if (0 == iIndex) return oEnum.Get();
            iIndex -= 1;
        } // for each elt

        return nil;
    }
    else if (SimpleVector* p = seq->DynamicCast<SimpleVector>())
    {
        return p->GetStart()[iIndex];
    }
    else if (SimpleString* p = seq->DynamicCast<SimpleString>())
    {
        return Character::FromCode(p->GetStart()[iIndex]);
    }
    else if (StringObject* p = seq->DynamicCast<StringObject>())
    {
        return Character::FromCode(p->GetStart()[iIndex]);
    } // if

    SignalTypeError(seq, Qsequence);
} // unsafe_elt

static Val setf_unsafe_elt(Val newval, Val seq, Val index)
{
    Int iIndex = Fixnum::Decode_(index);

    if (listp(seq))
    {
        foreach (List::Enum, oEnum, seq)
        {
            if (0 == iIndex) return oEnum.Set(newval);
            iIndex -= 1;
        } // for each elt
    }
    else if (SimpleVector* p = seq->DynamicCast<SimpleVector>())
    {
        return p->GetStart()[iIndex] = newval;
    }
    else if (SimpleString* p = seq->DynamicCast<SimpleString>())
    {
        check_type(newval, character);
        p->GetStart()[iIndex] = newval->StaticCast<Character>()->ToCode();
        return newval;
    }
    else if (StringObject* p = seq->DynamicCast<StringObject>())
    {
        check_type(newval, character);
        p->GetStart()[iIndex] = newval->StaticCast<Character>()->ToCode();
        return newval;
    } // if

    SignalTypeError(seq, Qsequence);
} // unsafe_elt

class EnumElt
{
    public: struct Arg
    {
        Val m_end;
        Val m_from_end;
        Val m_seq;
        Val m_start;

        Arg(Val seq, Val start, Val end, Val from_end = nil) :
            m_end(end),
            m_from_end(from_end),
            m_seq(seq),
            m_start(start) {}
    }; // Arg

    private: Val m_seq;
    private: Int m_iDelta;
    private: Int m_iEnd;
    private: Int m_iIndex;
    private: Int m_iStart;

    public: EnumElt(const Arg& r) :
        m_seq(r.m_seq)
    {
        Val end = ensure_bounding_indexes(m_seq, r.m_start, r.m_end);
        m_iStart = Fixnum::Decode_(r.m_start);
        m_iEnd   = Fixnum::Decode_(end);
        if (nil == r.m_from_end)
        {
            m_iIndex = m_iStart;
            m_iDelta = 1;
        }
        else
        {
            m_iIndex = m_iEnd - 1;
            m_iDelta = -1;
        }
    } // EnumElt

    public: bool AtEnd() const
    {
        return m_iDelta > 0 ? m_iIndex >= m_iEnd : m_iIndex < m_iStart;
    } // AtEnd

    public: Val Get() const
    {
        ASSERT(! AtEnd());
        return unsafe_elt(m_seq, Fixnum::Encode(m_iIndex));
    } // Get

    public: Val GetIndex() const
        { return Fixnum::Encode(m_iIndex); }

    public: Val Set(Val newval)
    {
        ASSERT(! AtEnd());
        return setf_unsafe_elt(newval, m_seq, Fixnum::Encode(m_iIndex));
    } // Get

    public: void Next()
    {
        ASSERT(! AtEnd());
        m_iIndex += m_iDelta;
    } // Next
}; // EnumElt

template<class Visitor> Val visitElement(Thread* pth)
{
    Val item1 = pth->mv_value[0];
    Val seq   = pth->mv_value[1];

    Val end      = nil;
    Val from_end = nil;
    Val key      = Qidentity;
    Val start    = zero;
    Val test     = nil;
    Val test_not = nil;

    KeyArg rgoKey[] =
    {
        KEYARG(end),
        KEYARG(from_end),
        KEYARG(key),
        KEYARG(test),
        KEYARG(test_not),
        KEYARG(start),
    }; // rgoKey

    parseKeys(pth, 2, rgoKey, lengthof(rgoKey));

    if (nil != test_not)
    {
        test = complement(test_not);
    }
    else if (nil == test)
    {
        test = Qeql;
    }

    foreach (EnumElt, oEnum, EnumElt::Arg(seq, start, end, from_end))
    {
        Val present = oEnum.Get();
        Val item2   = funcall(key, present);
        if (nil != funcall(test, item1, item2))
        {
            return Visitor::ReturnValue(oEnum.GetIndex(), present);
        }
    } // for each elt

    return nil;
} // visitElement

static void ensureSequenceIndex(Val seq, Val index)
{
    Val len = length(seq);
    if (lt(index, zero) || ge(index, len))
    {
        error(Qunbound_index,
            Kdatum,     index,
            Kexpected_type, list(Qinteger, zero, sub(len, 1)),
            Ksequence,  seq );
    }
} // ensureSequenceIndex

defun(ensure_bounding_indexes, (Val seq, Val start, Val end))
{
    Val len = length(seq);

    if (! fixnump(start) || lt(start, zero) || gt(start, len))
    {
        SignalTypeError(start, Qsequence_index);
    }

    if (nil == end)
    {
        end = len;
    }
    else
    {
        if (! fixnump(end) || lt(end, zero) || gt(end, len))
        {
            SignalTypeError(end, Qsequence_end);
        }
    }

    if (gt(start, end))
    {
        SignalTypeError(end, list(Qinteger, start, len));
    }

    return end;
} // ensure_bounding_indexes

namespace CommonLisp
{

// [C]
defun(copy_list, (Val orig))
{
    Val copy = nil;
    Val* pTail = &copy;
    foreach (List::Enum, oEnum, orig)
    {
        Val kons = list(oEnum.Get());
        *pTail = kons;
        pTail = &kons->StaticCast<Cons>()->m_cdr;
    } // for each elt
    return copy;
} // copy_list

defun(copy_seq, (Val x))
{
    if (nil == x)
    {
        return x;
    }

    if (consp(x))
    {
        return copy_list(x);
    } // if cons

    if (SimpleString* p = x->DynamicCast<SimpleString>())
    {
        Val len = p->m_length;
        Val copy = make_string(len);
        ::CopyMemory(
            copy->StaticCast<SimpleString>()->GetStart(),
            p->GetStart(),
            sizeof(char16) * (Fixnum::Decode_(len) + 1) );
        return copy;
    } // if simple_string

    if (StringObject* p = x->DynamicCast<StringObject>())
    {
        Val ofs;
        Val data = p->GetData(&ofs);
        Val len = p->m_fill_pointer;
        Val copy = make_string(len);
        ::CopyMemory(
            copy->StaticCast<SimpleString>()->GetStart(),
            data->StaticCast<SimpleString>()->GetStart() +
                Fixnum::Decode_(ofs),
            sizeof(char16) * (Fixnum::Decode_(len) + 1) );
        return copy;
    } // if string_object

    SignalTypeError(x, Qsequence);
} // copy_seq

// [E]
defun(elt, (Val seq, Val index))
{
    ensureSequenceIndex(seq, index);
    return unsafe_elt(seq, index);
} // elt

defun_setf(elt, (Val newval, Val seq, Val index))
{
    ensureSequenceIndex(seq, index);
    return setf_unsafe_elt(newval, seq, index);
} // elt

// [F]
defun(fillV, (Thread* pth))
{
    Val seq  = pth->mv_value[0];
    Val item = pth->mv_value[1];

    Val end   = nil;
    Val start = zero;

    KeyArg rgoKey[] =
    {
        KEYARG(end),
        KEYARG(start),
    }; // rgoKey

    parseKeys(pth, 2, rgoKey, lengthof(rgoKey));

    foreach (EnumElt, oEnum, EnumElt::Arg(seq, start, end))
    {
        oEnum.Set(item);
    } // for each elt
    return seq;
} // fillV

defun(findV, (Thread* pth))
{
    class FindVisitor
    {
        public: static Val ReturnValue(Val, Val present)
        {
            return present;
        } // ReturnValue
    }; // FindVisitor

    return visitElement<FindVisitor>(pth);
} // findV

// [M]
defun(make_listV, (Thread* pth))
{
    Val len = pth->mv_value[0];

    Val initial_element = nil;

    KeyArg rgoKey[] =
    {
        KEYARG(initial_element),
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    if (! fixnump(len) || Fixnum::Decode_(len) < 0)
    {
        SignalTypeError(len, Qsequence_index);
    }

    if (zero == len)
    {
        return nil;
    }

    Int iLen = Fixnum::Decode_(len);
    Val head = list(initial_element);
    Val tail = head;
    while (iLen > 0)
    {
        tail = setf_cdr(list(initial_element), tail);
        iLen -= 1;
    } // while

    return head;
} // make_listV

defun(make_sequenceV, (Thread* pth))
{
    Val ty   = pth->mv_value[0];
    Val len  = pth->mv_value[1];

    Val initial_element = MARKER_unbound;

    KeyArg rgoKey[] =
    {
        KEYARG(initial_element),
    }; // rgoKey

    parseKeys(pth, 2, rgoKey, lengthof(rgoKey));

    Val init = initial_element;

    if (Qlist == ty)
    {
        return make_list(len, MARKER_unbound == init ? nil : init);
    }
    else if (Qsimple_vector == ty)
    {
        Val vec = pth->AllocVector(CLASSD_simple_vector, len);
        if (MARKER_unbound != init)
        {
            fill(vec, init);
        }
        return vec;
    }
    else
    {
        Val klass = ty;
        if (! classp(klass))
        {
            klass = find_class(ty);
        }
        Val vec = pth->AllocBinVec(
            klass->StaticCast<Class>()->m_instanced,
            len );
        if (MARKER_unbound != init)
        {
            fill(vec, init);
        }
        return vec;
    } // if
} // make_sequenceV


// [N]
defun(nconc, (Val x, Val y))
{
    if (nil == x) return y;
    Val runner = x;
    for (;;)
    {
        check_type(runner, cons);
        Val next = cdr(runner);
        if (! consp(next))
        {
            setf_cdr(y, runner);
            return x;
        }
        runner = next;
    } // for
} // nconc

defun(nreverse, (Val x))
{
    if (listp(x))
    {
        Val y = nil;
        for (;;)
        {
            if (endp(x)) break;

            // (rotatef (cdr x) y x)
            Val temp = cdr(x);
            setf_cdr(y, x);
            y = x;
            x = temp;
        } // for
        return y;
    }
    else
    {
        Val len = length(x);
        EnumElt oEnum2(EnumElt::Arg(x, zero, len, Kfrom_end));
        foreach (EnumElt, oEnum, EnumElt::Arg(x, zero, len))
        {
            if (oEnum2.GetIndex() == oEnum.GetIndex())
            {
                break;
            }

            Val datum = oEnum.Get();
            oEnum.Set(oEnum2.Get());
            oEnum2.Set(datum);
            oEnum2.Next();
        } // for each elt
        return x;
    }
} // nreverse

// [P]
defun(positionV, (Thread* pth))
{
    class PositionVisitor
    {
        public: static Val ReturnValue(Val index, Val)
        {
            return index;
        } // ReturnValue
    }; // PositionVisitor

    return visitElement<PositionVisitor>(pth);
} // positionV

// [R]
defun(reverse, (Val x))
{
    if (listp(x))
    {
        Val rev = nil;
        foreach (List::Enum, oEnum, x)
        {
            push(oEnum.Get(), rev);
        } // for each elt

        return rev;
    }
    else
    {
        Val len = length(x);
        Val rev = make_sequence(class_of(x), len);
        EnumElt oEnum2(EnumElt::Arg(rev, zero, len, Kfrom_end));
        foreach (EnumElt, oEnum, EnumElt::Arg(x, zero, len))
        {
            oEnum2.Set(oEnum.Get());
            oEnum2.Next();
        } // for each elt
        return rev;
    } // if
} // reverse

// [S]
defun(string, (Val x))
{
    if (x->Is<SimpleString>()) return x;
    if (x->Is<Symbol>()) return x->StaticCast<Symbol>()->m_name;

    if (x->Is<Character>())
    {
        char16 wch = x->StaticCast<Character>()->ToCode();
        return make_string(&wch, 1);
    }

    SignalTypeError(x, Qstring);
} // string

defpred(string_eq, (Val x, Val y))
{
    x = string(x);
    y = string(y);
    SimpleString* px = x->StaticCast<SimpleString>();
    SimpleString* py = y->StaticCast<SimpleString>();
    if (px->m_length != py->m_length)
    {
        return false;
    }
    return 0 == memcmp(
        px->GetStart(),
        py->GetStart(),
        sizeof(char16) * Fixnum::Decode_(px->m_length) );
} // string_eq

defpred(string_equal, (Val x, Val y))
{
    x = string(x);
    y = string(y);

    StringData oX(x);
    StringData oY(y);

    if (oX.GetLength() != oY.GetLength())
    {
        return false;
    }

    StringData::Enum oEnum1(&oX);
    StringData::Enum oEnum2(&oY);

    while (! oEnum1.AtEnd())
    {
        char16 wch1 = oEnum1.Get();
        char16 wch2 = oEnum2.Get();

        if (wch1 != wch2)
        {
            Character* p1 = Character::FromCode(wch1)->
                StaticCast<Character>();

            Character* p2 = Character::FromCode(wch2)->
                StaticCast<Character>();

            if (p1->Upcase() != p2->Upcase())
            {
                return false;
            }
        }

        oEnum1.Next();
        oEnum2.Next();
    } // while

    return true;
} // string_equal

defun(svref, (Val vec, Val idx))
{
    check_type(vec, simple_vector);
    const SimpleVector* p = vec->StaticCast<SimpleVector>();
    if (idx >= p->m_length)
    {
        error(Qunbound_index, Ksequence, vec, Kdatum, idx);
    }
    return p->GetStart()[Fixnum::Decode_(idx)];
} // svref

defun_setf(svref, (Val val, Val vec, Val idx))
{
    check_type(vec, simple_vector);
    const SimpleVector* p = vec->StaticCast<SimpleVector>();
    if (idx >= p->m_length)
    {
        error(Qunbound_index, Ksequence, vec, Kdatum, idx);
    }
    return p->GetStart()[Fixnum::Decode_(idx)] = val;
} // setf_svref

defun(string_downcaseV, (Thread* pth))
{
    Val str = pth->mv_value[0];

    Val end   = nil;
    Val start = zero;

    KeyArg rgoKey[] =
    {
        KEYARG(end),
        KEYARG(start),
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    StringData oString(str, start, end);

    Val newstr = pth->AllocBinVec(
        CLASSD_simple_string, 
        Fixnum::Encode(oString.GetLength()) );

    char16* pwch = newstr->StaticCast<SimpleString>()->GetStart();

    foreach (StringData::Enum, oEnum, &oString)
    {
        Val ch = Character::FromCode(oEnum.Get());
        *pwch++ = ch->StaticCast<Character>()->Downcase()->
            StaticCast<Character>()->ToCode();
    } // for each ch

    return newstr;
} // string_downcaseV

defun(string_upcaseV, (Thread* pth))
{
    Val str = pth->mv_value[0];

    Val end   = nil;
    Val start = zero;

    KeyArg rgoKey[] =
    {
        KEYARG(end),
        KEYARG(start),
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    StringData oString(str, start, end);

    Val newstr = pth->AllocBinVec(
        CLASSD_simple_string, 
        Fixnum::Encode(oString.GetLength()) );

    char16* pwch = newstr->StaticCast<SimpleString>()->GetStart();

    foreach (StringData::Enum, oEnum, &oString)
    {
        Val ch = Character::FromCode(oEnum.Get());
        *pwch++ = ch->StaticCast<Character>()->Upcase()->
            StaticCast<Character>()->ToCode();
    } // for each ch

    return newstr;
} // string_upcaseV

defun(subseq, (Val seq, Val start, Val end))
{
    end = ensure_bounding_indexes(seq, start, end);
    Val sub = make_sequence(class_of(seq), xxsub(end, start));
    Val j = zero;
    for (Val i = start; lt(i, end); i = add(i, one))
    {
        setf_elt(elt(seq, i), sub, j);
        j = xxadd(j, one);
    } // for i
    return sub;
} // subseq

} // CommonLisp

} // TinyCl
