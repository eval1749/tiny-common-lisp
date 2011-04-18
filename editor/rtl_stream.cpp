#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Runtime - Stream
// editor/ed_rtl_stream.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/rtl_stream.cpp#5 $
//
//
#include "./rtl_defs.h"

#include "./ed_buffer.h"
#include "./ed_range.h"

namespace Editor
{

namespace Peer
{
    void HandleMessage();
} // Peer

using namespace TinyCl;

template<class T, class B>
class Instance_ : public AsInt, public B
{
    public: void* operator new(size_t)
    { 
        Val x = Thread::Get()->AllocInstance(T::ClassD_());
        return x->StaticCast<Instance>() + 1;
    } // operator new
        
    public: void operator delete(void*)
        { CAN_NOT_HAPPEN(); }
        
    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        return Storage::Decode_(x->StaticCast<Instance>()->m_storage);
    } // Decode_

    public: Val Encode() const
    { 
        Int iAddr = ToInt() - sizeof(Layout_instance);
        #if _DEBUG
        {
            Layout_instance* p = reinterpret_cast<Layout_instance*>(iAddr);
            ASSERT(p->m_storage->ToInt() == ToInt() + Storage::Tag);
        }
        #endif
        return FromInt<Datum>(iAddr + Instance::Tag);
    } // Encode

    public: static bool Is_(const Datum* const x)
    {
        Instance* p = x->DynamicCast<Instance>();
        when (NULL == p) return false;
        ClassD* pClassD =  p->m_classd->StaticCast<ClassD>();
        return nil != memq(T::Class_(), pClassD->m_class_precedence_list);
    } // Is_
}; // Instance_

class BufferInputStream : 
    public Instance_<BufferInputStream, Layout_buffer_input_stream>
{
    public: static Val Class_()  { return CLASS_buffer_input_stream; }
    public: static Val ClassD_() { return CLASSD_buffer_input_stream; }

    public: BufferInputStream(Val x)
    {
        if (Range* p = x->DynamicCast<Range>())
        {
            m_buffer  = p->m_buffer;
            m_current = make_range(p->m_buffer, p->m_start);
            m_flags   = Fixnum::Encode(StreamFlag_Input);
            m_limit   = x;
            return;
        } // if range

        SignalTypeError(x, list(Qor, Qbuffer, Qrange));
    } // BufferInputStream
}; // BufferInputStream

class BufferOutputStream : 
    public Instance_<BufferOutputStream, Layout_buffer_output_stream>
{
    public: static Val Class_()  { return CLASS_buffer_output_stream; }
    public: static Val ClassD_() { return CLASSD_buffer_output_stream; }

    public: BufferOutputStream(Val x, Val y)
    {
        if (Buffer* p = x->DynamicCast<Buffer>())
        {
            if (fixnump(y))
            {
                y = make_range(x, y);                
            }
            else if (nil == y)
            {
                // nothing to do
            }
            else if (y->Is<Range>())
            {
                // nothing to do
            }
            else
            {
                SignalTypeError(
                    y,
                    list(Qor, Qfixnum, Qnull, Qrange) );
            }

            m_buffer  = x;
            m_current = y;
            m_flags   = Fixnum::Encode(StreamFlag_Output);
            return;
        } // if range

        if (Range* p = x->DynamicCast<Range>())
        {
            m_buffer  = p->m_buffer;
            m_current = make_range(p->m_buffer, p->m_start);
            return;
        } // if range

        SignalTypeError(x, list(Qor, Qbuffer, Qrange));
    } // BufferOutputStream
}; // BufferOutputStream

Val make_buffer_input_stream(Val x)
{
    BufferInputStream* p = new BufferInputStream(x);
    return p->Encode();
} // make_buffer_input_stream

Val make_buffer_output_stream(Val x, Val y)
{
    BufferOutputStream* p = new BufferOutputStream(x, y);
    return p->Encode();
} // make_buffer_output_stream

defmethod(stream_read_char, buffer_input_stream, (Val stream))
{
    BufferInputStream* p = stream->StaticCast<BufferInputStream>();
    Range* pRange = p->m_current->StaticCast<Range>();
    Posn posn = pRange->GetStart();
    Buffer* pBuffer = p->m_buffer->StaticCast<Buffer>();
    Posn lim;
    if (nil == p->m_limit)
    {
        lim = pBuffer->GetEnd();
    }
    else
    {
        lim = p->m_limit->StaticCast<Range>()->GetEnd();
    }

    if (xge(posn, lim)) return Keof;
    Val ch = Character::FromCode(pBuffer->GetCharAt(posn));
    posn = xadd(posn, one);
    pRange->SetRange(posn, posn);
    return ch;
} // stream_read_char

defmethod(stream_unread_char, buffer_input_stream, (Val stream, Val))
{
    BufferInputStream* p = stream->StaticCast<BufferInputStream>();
    Range* pCur = p->m_current->StaticCast<Range>();
    Posn posn = pCur->GetStart();
    Buffer* pBuffer = p->m_buffer->StaticCast<Buffer>();
    Posn lim;
    if (nil == p->m_limit)
    {
        lim = pBuffer->GetStart();
    }
    else if (Range* pRange = p->m_limit->DynamicCast<Range>())
    {
        lim = pRange->GetStart();
    }
    else
    {
        SignalTypeError(p->m_current, Qrange);
    }
    if (xle(posn, lim)) error("Can't unread-char");
    posn = xsub(posn, one);
    pCur->SetRange(posn, posn);
    return nil;
} // stream_unread_char

defmethod(stream_write_char, buffer_output_stream, (Val stream, Val ch))
{
    BufferOutputStream* p = stream->StaticCast<BufferOutputStream>();
    Buffer* pBuffer = p->m_buffer->StaticCast<Buffer>();
    if (nil == p->m_current)
    {
        Posn posn = pBuffer->GetEnd();
        pBuffer->Insert(posn, ch->StaticCast<Character>()->ToCode(), one);
    }
    else if (Range* pRange = p->m_current->DynamicCast<Range>())
    {
        Posn posn = pRange->GetEnd();
        pBuffer->Insert(posn, ch->StaticCast<Character>()->ToCode(), one);
        posn = xadd(posn, 1);
        pRange->SetEnd(posn);
    }
    else
    {
        SignalTypeError(p->m_current, Qrange);
    } // if

    if (Character::FromCode(Newline) == ch)
    {
        Peer::HandleMessage();
    }

    return ch;
} // stream_write_char

} // Editor
