#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - 21 Streams
// tinycl_rtl_stream.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_stream.cpp#6 $
//
#include "../tinycl.h"

namespace TinyCl
{

using namespace Private;

class Stream :
    public Instance_<Stream, Layout_stream>
{
    public: static Val Class_() { return CLASS_stream; }
}; // Stream

class StringInputStream :
    public Instance_<StringInputStream, Layout_string_input_stream>
{
    public: static Val Class_() { return CLASS_string_input_stream; }
}; // StringInputStream

class StringOutputStream :
    public Instance_<StringOutputStream, Layout_string_output_stream>
{
    public: static Val Class_() { return CLASS_string_output_stream; }
}; // StringOutputStream

inline bool string_output_stream_p(Val x)
    { return x->Is<StringOutputStream>(); }

#define define_default_stream_operation(mp_name, mp_dir) \
    defmethod(stream_ ## mp_name , stream, (Val stream)) \
    { \
        if (! mp_dir ## _stream_p(stream)) \
        { \
            unsupported_operation(stream, Q ## mp_name); \
        } \
        return nil; \
    } // define_default_stream_operation

static void NoReturn unsupported_operation(Val stream, Val op)
{
    error(Qunsupported_operation,
        Koperation, op,
        Kstream,    stream );
} // unsupported_operation

// [Stream C]
define_default_stream_operation(clear_input,  input)
define_default_stream_operation(clear_output, output)

// [Stream F]
define_default_stream_operation(finish_output, output)
define_default_stream_operation(force_output, output)

defmethod(stream_force_output, platform_stream, (Val stream))
{
    PlatformStream* pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* p = pPlatform->GetImpl();
    p->ForceOutput(stream);
    return nil;
} // force_output

// [Stream L]
defmethod(stream_line_column, platform_stream, (Val stream))
{
    PlatformStream* pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* p = pPlatform->GetImpl();
    return p->GetLineColumn(stream);
} // stream_line_column

defmethod(stream_line_column, stream, (Val))
{
    return nil;
} // stream_line_column

defmethod(stream_line_number, platform_stream, (Val stream))
{
    PlatformStream* pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* p = pPlatform->GetImpl();
    return p->GetLineNumber(stream);
} // stream_line_number

defmethod(stream_line_number, stream, (Val))
{
    return nil;
} // stream_line_number

// [Stream P]
defmethod(stream_pathname, file_stream, (Val stream))
{
    return stream->StaticCast<FileStream>()->m_pathname;
} // stream_pathname

defmethod(stream_pathname, stream, (Val stream))
{
    unsupported_operation(stream, Qstream_pathname);
} // stream_pathname

// [Stream R]
defmethod(stream_read_char, platform_stream, (Val stream))
{
    PlatformStream* const pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* const p = pPlatform->GetImpl();
    return p->ReadChar(stream);
} // stream_read_char

defmethod(stream_read_char, string_input_stream, (Val stream))
{
    StringInputStream* const p = stream->StaticCast<StringInputStream>();

    StringData oData(p->m_string, p->m_index, p->m_end);
    if (p->m_index >= p->m_end) return Keof;
    Val const ch = Character::FromCode(oData.GetStart()[0]);
    p->m_index = xxadd(p->m_index, 1);
    return ch;
} // stream_read_char string_input_stream

// [Stream U]
defmethod(stream_unread_char, platform_stream, (Val stream, Val ch))
{
    PlatformStream* pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* p = pPlatform->GetImpl();
    p->UnreadChar(stream, ch);
    return nil;
} // stream_unread_char platform_stream

defmethod(stream_unread_char, string_input_stream, (Val stream, Val))
{
    StringInputStream* const p = stream->StaticCast<StringInputStream>();
    if (p->m_index <= p->m_start)
    {
        error("Can't unread-char");
    }
    p->m_index = xxsub(p->m_index, 1);
    return nil;
} // stream_unread_char string_input_stream

// [Stream W]

/// <summary>
///   Encodes HTML meta characters.
/// </summary>
defmethod(stream_write_char, html_encode_stream, (Val stream, Val ch))
{
    HtmlEncodeStream* const p = stream->StaticCast<HtmlEncodeStream>();
    if (nil == p->m_mode)
    {
        write_char(ch, p->m_stream);
    }
    else if (Character::FromCode('&') == ch)
    {
        write_string("&amp;", p->m_stream);
    }
    else if (Character::FromCode('<') == ch)
    {
        write_string("&lt;", p->m_stream);
    }
    else if (Character::FromCode(DoubleQuote) == ch &&
             Kattribute == p->m_mode )
    {
        write_string("&quot;", p->m_stream);
    }
    else
    {
        write_char(ch, p->m_stream);
    }

    return ch;
} // stream_write_char html_encode_stream

defmethod(stream_write_char, platform_stream, (Val stream, Val ch))
{
    PlatformStream* const pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();
    StreamImpl* const p = pPlatform->GetImpl();
    char16 const wch = ch->StaticCast<Character>()->ToCode();
    p->WriteString(stream, &wch, 1);
    return ch;
} // stream_write_char platform_stream

defmethod(stream_write_char, string_output_stream, (Val stream, Val ch))
{
    StringOutputStream* const p = stream->StaticCast<StringOutputStream>();
    if (Character::FromCode(Newline) == ch)
    {
        p->m_column = zero;
    }
    else
    {
        p->m_column = xxadd(p->m_column, 1);
    }

    StringObject* const q = p->m_string->StaticCast<StringObject>();

    Val offset;
    Val curdata = q->GetData(&offset);

    if (q->m_fill_pointer == q->m_total_size)
    {
        Val curlen = q->m_total_size;

        q->m_total_size = xxadd(q->m_total_size, 10);
        Val newdata = Thread::Get()->AllocBinVec(
            CLASSD_simple_string, 
            q->m_total_size );

        ::CopyMemory(
            newdata->StaticCast<SimpleString>()->GetStart(),
            curdata->StaticCast<SimpleString>()->GetStart() +
                Fixnum::Decode_(offset),
            sizeof(char16) * Fixnum::Decode_(curlen) );

        q->m_displaced_to = newdata;
        q->m_offset       = zero;

        curdata = newdata;
        offset  = zero;
    } // if

    curdata->StaticCast<SimpleString>()->GetStart()[
        Fixnum::Decode_(xxadd(offset, q->m_fill_pointer)) ] =
            ch->StaticCast<Character>()->ToCode();

    q->m_fill_pointer = xxadd(q->m_fill_pointer, 1);

    return ch;
} // stream_write_char string_output_stream

defmethod_(stream_write_string, html_encode_stream,
    (Val stream, Val string, Val start, Val end) )
{
    class Sink
    {
        private: uint m_cwch;

        private: Val m_start;
        private: Val m_stream;
        private: Val m_string;

        public: Sink(
            Val const stream,
            Val const string,
            Val const start ) :
                m_cwch(0),
                m_start(start),
                m_stream(stream),
                m_string(string) {}

        public: ~Sink()
            { Flush(); }

        public: void Add()
            { m_cwch += 1; }

        public: void Flush()
        {
            if (m_cwch > 0)
            {
                Val const next = Fixnum::Encode(
                    Fixnum::Decode_(m_start) + m_cwch );

                funcall(
                    Qstream_write_string,
                    m_stream,
                    m_string,
                    m_start,
                    next );
                    
                m_start = next;
                m_cwch = 0;
            }
        } // Flush

        public:void Skip()
            { m_start = Fixnum::Encode(Fixnum::Decode_(m_start) + 1); }
    }; // Sink

    HtmlEncodeStream* const p = stream->StaticCast<HtmlEncodeStream>();
    if (nil == p->m_mode)
    {
        funcall(Qstream_write_string, p->m_stream, string, start, end);
    }
    else
    {
        StringData oString(string, start, end);
        Sink oSink(p->m_stream, string, start);

        foreach (StringData::Enum, oEnum, &oString)
        {
            switch (oEnum.Get())
            {
            case '&':
                oSink.Flush();
                oSink.Skip();
                write_string("&amp;", p->m_stream);
                break;

            case '<':
                oSink.Flush();
                oSink.Skip();
                write_string("&lt;", p->m_stream);
                break;

            case DoubleQuote:
                if (Kattribute == p->m_mode)
                {
                    oSink.Flush();
                    oSink.Skip();
                    write_string("&quot;", p->m_stream);
                }
                else
                {
                    oSink.Add();
                }
                break;

            default:
                oSink.Add();
                break;
            } // switch wch
        } // for
    } // if

    return string;
} // stream_write_string html_encode_stream

defmethod_(stream_write_string, output_stream,
    (Val stream, Val string, Val start, Val end) )
{
    StringData oString(string, start, end);
    foreach (StringData::Enum, oEnum, &oString)
    {
        funcall(Qstream_write_char, stream, Character::FromCode(oEnum.Get()));
    } // for pwch
    return string;
} // stream_write_string

defmethod_(stream_write_string, platform_stream,
    (Val stream, Val string, Val start, Val end) )
{
    StringData oString(string, start, end);

    PlatformStream* const pPlatform = stream->StaticCast<Instance>()->
        GetStorage<PlatformStream>();

    StreamImpl* const p = pPlatform->GetImpl();

    p->WriteString(
        stream,
        oString.GetStart(),
        oString.GetLength() );

    return string;
} // stream_write_string

namespace CommonLisp
{

using namespace TinyCl;
using namespace TinyCl::Private;

// [C]
defmethod(close, stream, (Thread*))
    { return nil; }

defmethod(close, html_encode_stream, (Thread* pth))
{
    Val const stream = pth->mv_value[0];
    HtmlEncodeStream* const p = stream->StaticCast<HtmlEncodeStream>();
    return funcall(Qclose, p->m_stream);
} // close html_encode_stream

// [G]
defun(get_output_stream_string, (Val stream))
{
    check_type(stream, string_output_stream);
    StringOutputStream* p = stream->StaticCast<StringOutputStream>();
    Val string = copy_seq(p->m_string);
    StringObject* q = p->m_string->StaticCast<StringObject>();
    q->m_fill_pointer = zero;
    p->m_column = zero;
    return string;
} // get_output_stream_string

// [I]
defpred(input_stream_p, (Val x))
{
    if (typep(x, CLASS_input_stream)) return true;
    if (typep(x, CLASS_stream)) return false;
    SignalTypeError(x, Qstream);
} // input_stream_p

defpred(interactive_stream_p, (Val stream))
{
    check_type(stream, stream);
    Stream* p = stream->StaticCast<Stream>();
    return 0 != (Fixnum::Decode_(p->m_flags) & StreamFlag_Interactive);
} // interactive_stream_p

// [M]
Val make_string_input_stream(Val string, Val start, Val end)
{
    // Check bound indexes
    StringData oData(string, start, end);

    Val const stream =
        Thread::Get()->AllocInstance(CLASSD_string_input_stream);
    {
        StringInputStream* const p = stream->StaticCast<StringInputStream>();
        p->m_flags  = Fixnum::Encode(StreamFlag_Input);
        p->m_string = string;
        p->m_start  = start;
        p->m_index  = start;
        p->m_end    = nil == end ? length(string) : end;
    }
    return stream;
} // make_start_input_stream

defun(make_string_output_streamV, (Thread* pth))
{ 
    // We don't support element-type parameter.

    Val const strbuf = pth->AllocRecord(CLASSD_string_object);
    {
        const Val datalen = Fixnum::Encode(24);
        Val const data = pth->AllocBinVec(CLASSD_simple_string, datalen);
        StringObject* const p = strbuf->StaticCast<StringObject>();
        p->m_fill_pointer = zero;
        p->m_flags        = Carray_flag_fill_pointer;
        p->m_displaced_to = data;
        p->m_offset       = zero;
        p->m_total_size   = datalen;
    }

    Val stream = pth->AllocInstance(CLASSD_string_output_stream);
    {
        StringOutputStream* p = stream->StaticCast<StringOutputStream>();
        p->m_flags  = Fixnum::Encode(StreamFlag_Output);
        p->m_column = zero;
        p->m_string = strbuf;
    }
    return stream;
} // make_output_string_streamV

// [O]
defpred(output_stream_p, (Val x))
{
    if (typep(x, CLASS_output_stream)) return true;
    if (typep(x, CLASS_stream)) return false;
    SignalTypeError(x, Qstream);
} // output_stream_p

defpred(streamp, (Val x))
    { return x->Is<Stream>(); }

} // CommonLisp
} // TinyCl
