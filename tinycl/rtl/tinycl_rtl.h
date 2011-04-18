//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions
// tinycl_rtl.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl.h#5 $
//
#if !defined(INCLUDE_tinycl_rtl_h)
#define INCLUDE_tinycl_rtl_h


#define defun(mp_fname, mp_params) \
    Val __fastcall mp_fname mp_params

#define defun_setf(mp_fname, mp_params) \
    Val __fastcall setf_ ## mp_fname mp_params

#define defpred(mp_fname, mp_params) \
    bool __fastcall mp_fname mp_params

#define defun(mp_fname, mp_params) \
    Val __fastcall mp_fname mp_params

#define defmethod(mp_fname, mp_class, mp_params) \
    extern "C" Val __fastcall mp_fname ## __ ## mp_class mp_params

#define defmethod_setf(mp_fname, mp_class, mp_params) \
    extern "C" Val __fastcall setf_ ## mp_fname ## __ ## mp_class mp_params

#define defmethod_(mp_fname, mp_class, mp_params) \
    Val __fastcall mp_fname ## __ ## mp_class  mp_params

#define defpred(mp_fname, mp_params) \
    bool __fastcall mp_fname mp_params

namespace TinyCl
{

enum Signs
{
    Signs_Pos_Pos   = 0,
    Signs_Neg_Pos   = 1,
    Signs_Pos_Neg   = 2,
    Signs_Neg_Neg   = 3,
}; // Signs

// For variable arity lisp callable function.
class Thread;

namespace CommonLisp
{

using namespace TinyCl::Keyword;
using TinyCl::Val;

// [A]
defun(add, (Val, Val));
defun(appendS2, (Val, Val));

inline Val append(Val a, Val b) { return appendS2(a, b); }

defpred(array_has_fill_pointer_p, (Val));
defpred(arrayp, (Val));
defun(ash, (Val, Val));

// [B]
defpred(boundp,   (Val));


// [C]
defun(car,  (Val));
defun(cdr,  (Val));

defun(caar, (Val));
defun(cadr, (Val));
defun(cdar, (Val));
defun(cddr, (Val));

defun(caaar, (Val));
defun(caadr, (Val));
defun(cadar, (Val));
defun(caddr, (Val));
defun(cdaar, (Val));
defun(cdadr, (Val));
defun(cddar, (Val));
defun(cdddr, (Val));

defun(caaaar, (Val));
defun(caaadr, (Val));
defun(caadar, (Val));
defun(caaddr, (Val));
defun(cadaar, (Val));
defun(cadadr, (Val));
defun(caddar, (Val));
defun(cadddr, (Val));
defun(cdaaar, (Val));
defun(cdaadr, (Val));
defun(cdadar, (Val));
defun(cdaddr, (Val));
defun(cddaar, (Val));
defun(cddadr, (Val));
defun(cdddar, (Val));
defun(cddddr, (Val));

defun_setf(car, (Val, Val));
defun_setf(cdr, (Val, Val));

defun_setf(caar, (Val, Val));
defun_setf(cadr, (Val, Val));
defun_setf(cdar, (Val, Val));
defun_setf(cddr, (Val, Val));

defun(char_name, (Val));
defun(class_of, (Val));
defun(clear_input, (Val));
defun(clear_output, (Val));
defun(closeV, (Thread*));
defun(cons, (Val, Val));
defun(constantly, (Val));
defpred(consp, (Val));
defun(copy_list, (Val));
defun(copy_seq, (Val));

// [D]
defun(describe, (Val));
defun(div, (Val, Val));

// [E]
defpred(endp, (Val));
defun(enough_namestring, (Val, Val));
bool eq(Val, Val);
defpred(eql,    (Val, Val));
defpred(equal,  (Val, Val));
defpred(equalp, (Val, Val));
void NoReturn errorV(Thread*);
void NoReturn error(Val);
void NoReturn error(Val, Val);
void NoReturn error(Val, Val, Val);
void NoReturn error(Val, Val, Val, Val);
void NoReturn error(Val, Val, Val, Val, Val);
void NoReturn error(Val, Val, Val, Val, Val, Val);
void NoReturn error(Val, Val, Val, Val, Val, Val, Val);
void NoReturn error(const char*);
void NoReturn error(const char*, Val);
void NoReturn error(const char*, Val, Val);

// [F]
defpred(fboundp, (Val));
defun(fdefinition, (Val));
defun_setf(fill_pointer, (Val, Val));
defun(findV, (Thread*));
defun(find_class, (Val, Val = t, Val = nil));
//defun(find_class_, (Val, Val, Val, Val));
Val find_symbol(Val, Val);
Val find_symbol(Val);
defun(finish_output, (Val));
defun(force_output, (Val = nil));
Val format(Val, const char*);
Val format(Val, const char*, Val);
Val format(Val, const char*, Val, Val);
Val format(Val, const char*, Val, Val, Val);
Val format(Val, const char*, Val, Val, Val, Val);
Val format(Val, const char*, Val, Val, Val, Val, Val);
Val format(Val, const char*, Val, Val, Val, Val, Val, Val);
Val format(Val, const char*, Val, Val, Val, Val, Val, Val, Val);
defun(formatV, (Thread*));
Val funcall(Val);
Val funcall(Val, Val);
Val funcall(Val, Val, Val);
Val funcall(Val, Val, Val, Val);
Val funcall(Val, Val, Val, Val, Val);
Val funcall(Val, Val, Val, Val, Val, Val);
Val funcall(Val, Val, Val, Val, Val, Val, Val);
Val funcall(Val, Val, Val, Val, Val, Val, Val, Val);
Val funcall(Val, Val, Val, Val, Val, Val, Val, Val, Val);
bool functionp(Val);

// [G]
bool ge(Val, Val);
Val getf(Val, Val, Val = nil);
Val gethash(Val, Val, Val, Val*);
Val setf_gethash(Val, Val, Val);
//defun(gethash_, (Val, Val, Val, Val));
defun(get_output_stream_string, (Val));
bool gt(Val, Val);

// [I]
defpred(input_stream_p, (Val));
defpred(interactive_stream_p, (Val));

// [K]
defpred(keywordp, (Val));

// [L]
bool le(Val, Val);
defun(long_site_name, ());
bool lt(Val, Val);

// [M]
defun(macine_instance, ());
defun(make_listV, (Thread*));
Val make_hash_table(Val, Val);
Val make_hash_table(Val, Val, Val, Val);
defun(make_hash_tableV, (Thread*));
defun(make_sequenceV, (Thread*));
Val make_string(Val);
Val make_string_input_stream(Val, Val = zero, Val = nil);
defun(make_string_output_streamV, (Thread*));
defun(make_symbol, (Val));
Val merge_pathnames(Val, Val, Val = Knewest);
defun(mul, (Val, Val));

// [N]
defun(name_char, (Val));
defun(namestring, (Val));
bool ne(Val, Val);
defpred(numberp, (Val));

// [O]
defun(openV, (Thread*));
defpred(output_stream_p, (Val));

// [P]
defun(parse_namestringV, (Thread*));
defun(pathname, (Val));
defun(pathname_device, (Val));
defun(pathname_directory, (Val));
defun(pathname_name, (Val));
defun(pathname_type, (Val));
defun(pathname_version, (Val));
defpred(pathnamep, (Val));
defun(positionV, (Thread*));
defun(print, (Val, Val = nil));

// [R]
defun(rem, (Val, Val));
defun(reverse, (Val));
Val read(Val = nil, Val = t, Val = nil, Val = nil);
Val read_char(Val = nil, Val = t, Val = nil);
Val room(Val = Kdefault);

// [S]
defun(short_site_name, ());
defun(signalV, (Thread*));
defpred(streamp, (Val));
defun(string_downcaseV, (Thread*));
defun(string_upcaseV, (Thread*));
Val subseq(Val, Val, Val = nil);
defun(svref, (Val, Val));
defun_setf(svref, (Val, Val, Val));
Val  sub(Val, Val);
defun(sub_1, (Val));
defun(sxhash, (Val));
defun(symbol_function, (Val));
defun(symbol_value, (Val));
defpred(symbolp,  (Val));

// [T]
Val  truncate(Val, Val = one);
void truncate(Val, Val, Val*, Val*);
defun(type_of, (Val));
defpred(typep, (Val, Val, Val = nil));

// [U]
Val unread_char(Val, Val = nil);

// [V]
defun(values, ());
defun(values, (Val));
defun(values, (Val, Val));
defun(values, (Val, Val, Val));
defun(values, (Val, Val, Val, Val));
defun(values, (Val, Val, Val, Val, Val));
defun(values, (Val, Val, Val, Val, Val, Val));
defun(values, (Val, Val, Val, Val, Val, Val, Val));
defun(values, (Val, Val, Val, Val, Val, Val, Val, Val));
defun(values, (Val, Val, Val, Val, Val, Val, Val, Val, Val));
defun(values_list, (Val));

// [W]
Val write_char(Val, Val = nil);
void write_string(Val, Val = nil);
void write_string(const char16*, Int, Val = nil);
void write_string(const char*, Int, Val = nil);
defun(write_stringV, (Thread*));

// 11 Package
defun(find_package, (Val));
Val find_symbol(Val, Val, Val*);
Val intern(const char*);
Val intern(const char*, Val);
Val intern(Val);
defun(intern, (Val, Val));
defun(make_packageV, (Thread*));
defun(use_package, (Val, Val));

// 13 Characters
defpred(characterp, (Val));
defun(char_downcase, (Val));
defun(char_upcase, (Val));
defun(code_char, (Val));
defun(digit_char, (Val, Val = Fixnum::Encode(10)));
defun(digit_char_p, (Val, Val = Fixnum::Encode(10)));

defun(listV, (Thread*));
defun(listAV, (Thread*));

defun(nreverse, (Val));
defun(nconc,    (Val, Val));

// 16 Strings
Val make_string(const char16*, size_t);
Val make_string(const char16*);
Val make_string(const char*, size_t);
Val make_string(const char*);
defun(string, (Val));
defpred(string_eq, (Val, Val));
defpred(string_equal, (Val, Val));

// 17 Sequences
defun(elt, (Val, Val));
defun_setf(elt, (Val, Val, Val));
defun(fillV, (Thread*));
//defun(length, (Val));

} // CommonLisp

using namespace CommonLisp;

// [A]
defun(allocate_funcallable_instance, (Val));
defun(assq, (Val, Val));

// [C]
defun(classd_of, (Val));
defpred(classp, (Val));
defun(cl_length, (Val));
Val coerce_to_condition(Thread*);
defun(collect_garbageV, (Thread*));
defun(compile_form, (Val));
defun(compile_regexV, (Thread*));

// [D]
defun(delq, (Val, Val));

// [E]
defun(ensure_bounding_indexes, (Val, Val, Val));

// [F]
Val find_pathname_host(Val, Val = t);
defun(find_setf_cell, (Val));
defun(find_value_cell, (Val));
defun(funcallable_instance_function, (Val));
defpred(funcallable_standard_object_p, (Val));
defun(function_name, (Val));          // arch
defpred(function_name_p, (Val));

// [G]
defun(gcdS2, (Val, Val));

// [I]
defun(intern_callee, (Val));
defun(intern_dll_entry, (Val, Val));
defun(intern_setf_cell, (Val));
defun(intern_value_cell, (Val, Val));

// [M]
defun(make_caller_set, (Val));
defun(make_closed_cell, (Val));
defun(make_not_function_function, (Val));
Val make_platform_error(const char*, uint);
defun(make_undefined_function_function, (Val));
defun(make_vector, (Val));
defun(memq, (Val, Val));
defun(memv, (Val, Val));

// [R]
Val read_char_(Val = nil, Val = t, Val = nil, Val = nil);
defun(register_caller, (Val, Val));

// [S]
defun(safe_list_length, (Val));
defun(set_funcallable_instance_function, (Val, Val));
Val string_match(Val, Val, Val = zero, Val = nil);
defpred(subclassp, (Val, Val));
defun(sxhash_eq, (Val));
defun(sxhash_eql, (Val));
defun(sxhash_equalp, (Val));

// [U]
defun(update_callers, (Val, Val));    // arch

// [%]
defun(Pdeftlv, (Val, Val = nil, Val = nil, Val = nil));
defun(Pdefvar, (Val, Val, Val = nil));

struct KeyArg
{
    Val*    m_pval;
    Val     m_key;
}; // KeyArg

#define KEYARG(mp_name) KEYARG2(mp_name, mp_name)

#define KEYARG2(mp_key, mp_name) { &mp_name, K ## mp_key }

void parseKeys(Thread*, Int, const KeyArg*, uint);

#define check_type(mp_var, mp_ty) \
    unless (mp_ty ## _p(mp_var)) SignalTypeError(mp_var, Q ## mp_ty);


// 11 Package
Val internal_package_put(Val pkg, Val sym);

// 12 Numbers
Int  GetInt(Val);
UInt GetUInt(Val);
Val  MakeInt(Int);
Val  MakeInt64(Int64);
Val  MakeUInt(UInt);
Val  MakeUInt64(UInt64);

// 13 Character
Val const CHAR_u0000   = internal_make_char(0);
Val const CHAR_uFFFF   = internal_make_char(0xFFFF);
Val const CHAR_Newline = internal_make_char(Private::Newline);
Val const CHAR_Return  = internal_make_char(Private::Return);

#undef internal_make_char

inline Val Character::FromCode(char16 wch)
{
    return FromInt<Datum>(
        CHAR_u0000->ToInt() +
        sizeof(Layout_character) * wch );
} // Character::Encode

inline char16 Character::ToCode() const
{
    return static_cast<char16>(
        (ToInt() -CHAR_BASE) / sizeof(Layout_character) );
} // Character::ToCode

// 21 Streams
enum StreamFlag
{
    StreamFlag_Input        = 1 << 0,
    StreamFlag_Output       = 1 << 1,
    StreamFlag_Io           = StreamFlag_Input | StreamFlag_Output,
    StreamFlag_Probe        = 1 << 2,
    StreamFlag_Interactive  = 1 << 3,
}; // StreamFlag

class StreamImpl
{
    public: virtual Val  Close(Val, Val)
        { return nil; }

    // [F]
    public: virtual void ForceOutput(Val s)
        { unsupported(s, Qforce_output); }

    // [G]
    public: virtual Val GetLineColumn(Val)
        { return nil; }

    public: virtual Val GetLineNumber(Val)
        { return nil; }

    // [R]
    public: virtual Val  ReadChar(Val s)
        { unsupported(s, Qread_char); }

    // [U]
    public: virtual void  UnreadChar(Val s, Val)
        { unsupported(s, Qunread_char); }

    protected: void NoReturn unsupported(Val s, Val op)
        { error(Qunsupported_operation, Koperation, op, Kstream, s); }

    // [W]
    public: void WriteString(Val s, const char* pchStart, Int cch)
    {
        const char* pchEnd = pchStart + cch;
        for (const char* pch = pchStart; pch < pchEnd; pch++)
        {
            char16 wch = *pch;
            WriteString(s, &wch, 1);
        } // for
    } // WriteString

    public: virtual void WriteString(Val s, const char16*, Int)
        { unsupported(s, Qwrite_string); }
}; // StreamImpl

class PlatformError : public
    Instance_<PlatformError, Layout_platform_error>
{
    public: static Val Class_() { return CLASS_platform_error; }
}; // PlatformError

class PlatformStream : public
    Instance_<PlatformStream, Layout_platform_stream>
{
    public: static Val Class_() { return CLASS_platform_stream; }

    public: StreamImpl* GetImpl()
    {
        return Fixnum::To<StreamImpl>(m_blob);
    } // StreamImpl
}; // PlatformStream

class FileStream :
    public Instance_<FileStream, Layout_file_stream>
{
    public: static Val Class_() { return CLASS_file_stream; }

    public: StreamImpl* GetImpl()
    {
        return Fixnum::To<StreamImpl>(m_blob);
    } // StreamImpl
}; // FileStream

// 23 Reader
enum CharType
{
    CharType_Invalid    = 0,
    CharType_Cons       = 1,    // constituent
    CharType_Nmacro     = 2,    // non-terminating macro character
    CharType_Tmacro     = 3,    // terminating macro character
    CharType_Space      = 4,    // whitespace
    CharType_Sescape    = 5,    // single escape
    CharType_Mescape    = 6,    // multiple escape
    CharType_Mask       = 7
}; // Type

enum CharTrait
{
    CharTrait_Invalid     = 0x000000,
    CharTrait_Alphabetic  = 0x000100,   // bit 8
    CharTrait_Digit       = 0x000200,   // bit 9
    CharTrait_Package     = 0x000400,   // bit 10
    CharTrait_Dot         = 0x000800,   // bit 11
    CharTrait_Decimal     = 0x001000,   // bit 12
    CharTrait_Plus        = 0x002000,   // bit 13
    CharTrait_Minus       = 0x004000,   // bit 14
    CharTrait_Ratio       = 0x008000,   // bit 15
    CharTrait_Dmarker     = 0x010000,   // bit 16
    CharTrait_Emarker     = 0x020000,   // bit 17
    CharTrait_Fmarker     = 0x040000,   // bit 18
    CharTrait_Lmarker     = 0x080000,   // bit 19
    CharTrait_Smarker     = 0x100000,   // bit 20

    CharTrait_Mask        = 0x1FFF00,

    CharTrait_Alphadigit  = CharTrait_Alphabetic | CharTrait_Digit,
    CharTrait_Sign        = CharTrait_Plus | CharTrait_Minus,

    CharTrait_FloatMarker =
        CharTrait_Dmarker |
        CharTrait_Emarker |
        CharTrait_Fmarker |
        CharTrait_Lmarker |
        CharTrait_Smarker,
}; // CharTrait

class Readtable : public Record_<Readtable, Layout_readtable>
{
    public: static Val ClassD_() { return CLASSD_readtable; }
}; // Readtable

inline bool setf_cell_p(Val x)  { return x->Is<SetfCell>(); }
inline bool tlv_record_p(Val x) { return x->Is<TlvRecord>(); }

namespace CommonLisp
{

// [A]
inline Val add(Val x, Int i) { return add(x, Fixnum::Encode(i)); }
inline bool atom(Val x) { return !consp(x); }

// [B]
inline bool bignump(Val x) { return x->Is<Bignum>(); }

// [C]
inline bool char_equal(char16 wch1, char16 wch2)
{
    if (wch1 == wch2)
    {
        return true;
    }

    return Character::UpcaseFromCode(wch1) ==
           Character::UpcaseFromCode(wch2);
} // char_equal

inline Val close(Val stream)
    { return funcall(Qclose, stream); }

inline Val complement(Val fn)
    { return funcall(Qcomplement, fn); }

// [E]
inline Val enough_namestring(Val x)
    { return enough_namestring(x, TLV(Adefault_pathname_defaultsA)); }

inline bool environment_p(Val x)     { return x->Is<Environment>(); }
inline bool external_format_p(Val x) { return x->Is<ExternalFormat>(); }

// [F]
inline Val fill(Val seq, Val item)
    { values(seq, item); return fillV(Thread::Get()); }

inline bool fixnump(Val x)   { return x->Is<Fixnum>(); }
inline bool functionp(Val x) { return x->Is<Function>(); }

// [G]
inline Val gethash(Val key, Val htb, Val def = nil)
{
    Val found;
    Val val = gethash(key, htb, def, &found);
    return values(val, found);
} // gethash

inline Val setf_gethash(Val a, Val b, Val c, Val)
    { return setf_gethash(a, b, c); }

inline bool hash_table_p(Val x) { return x->Is<HashTable>(); }
inline Val mul(Val x, Int i) { return mul(x, Fixnum::Encode(i)); }
inline Val rem(Val x, Int i) { return rem(x, Fixnum::Encode(i)); }
inline Val sub(Val x, Int i) { return sub(x, Fixnum::Encode(i)); }
inline Val truncate(Val x, Int i) { return truncate(x, Fixnum::Encode(i)); }
inline bool le(Val x, Int i) { return le(x, Fixnum::Encode(i)); }
inline bool lt(Val x, Int i) { return lt(x, Fixnum::Encode(i)); }
inline bool ge(Val x, Int i) { return ge(x, Fixnum::Encode(i)); }
inline bool gt(Val x, Int i) { return gt(x, Fixnum::Encode(i)); }
inline bool integerp(Val x) { return fixnump(x) || bignump(x); }

inline bool consp(Val x) { return x->Is<Cons>(); }

inline Val length(Val x) { return cl_length(x); }

inline bool listp(Val x) { return x->Is<List>(); }

inline Val list() { return nil; }
inline Val list(Val a) { return cons(a, nil); }
inline Val list(Val a, Val b) { return cons(a, list(b)); }
inline Val list(Val a, Val b, Val c) { return cons(a, list(b, c)); }

inline Val list(Val a, Val b, Val c, Val d)
    { return cons(a, list(b, c, d)); }

inline Val list(Val a, Val b, Val c, Val d, Val e)
    { return cons(a, list(b, c, d, e)); }

inline Val list(Val a, Val b, Val c, Val d, Val e, Val f)
    { return cons(a, list(b, c, d, e, f)); }

inline Val list(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
    { return cons(a, list(b, c, d, e, f, g)); }

inline Val list(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h)
    { return cons(a, list(b, c, d, e, f, g, h)); }

inline Val list(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h, Val i)
    { return cons(a, list(b, c, d, e, f, g, h, i)); }

inline Val list(Val a, Val b, Val c, Val d, Val e,
                Val f, Val g, Val h, Val i, Val j)
    { return cons(a, list(b, c, d, e, f, g, h, i, j)); }

inline Val listA(Val a) { return a; }
inline Val listA(Val a, Val b) { return cons(a, b); }
inline Val listA(Val a, Val b, Val c) { return cons(a, listA(b, c)); }

inline Val listA(Val a, Val b, Val c, Val d)
    { return cons(a, listA(b, c, d)); }

inline Val listA(Val a, Val b, Val c, Val d, Val e)
    { return cons(a, listA(b, c, d, e)); }

inline Val listA(Val a, Val b, Val c, Val d, Val e, Val f)
    { return cons(a, listA(b, c, d, e, f)); }

inline Val listA(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
    { return cons(a, listA(b, c, d, e, f, g)); }


inline bool simple_string_p(Val x) { return x->Is<SimpleString>(); }
inline bool simple_vector_p(Val x) { return x->Is<SimpleVector>(); }
inline bool stringp(Val x)         { return simple_string_p(x); }
inline bool symbolp(Val x)         { return x->Is<Symbol>(); }
inline bool vectorp(Val x)         { return x->Is<Vector>(); }

// [M]
inline Val make_list(Val siz, Val init = nil)
    { values(siz, Kinitial_element, init); return make_listV(Thread::Get()); }

inline Val make_sequence(Val ty, Val siz)
    { values(ty, siz); return make_sequenceV(Thread::Get()); }

inline Val merge_pathnames(Val a)
    { return merge_pathnames(a, TLV(Adefault_pathname_defaultsA)); }

inline Val make_string_output_stream()
    { return make_string_output_streamV(Thread::Get()); }

// Note: We must make make_symbol(Val) unqiue for DEF file.
inline Val make_symbol_(const char* psz)
    { return make_symbol(make_string(psz)); }

// [O]
inline Val open(Val filename, Val a, Val b, Val c, Val d)
{
    values(filename, a, b, c, d);
    return openV(Thread::Get());
} // open

// [P]
inline Val parse_namestring(Val a, Val b = nil)
    { values(a, b); return parse_namestringV(Thread::Get()); }

inline Val parse_namestring(Val a, Val b, Val c)
    { values(a, b, c); return parse_namestringV(Thread::Get()); }

inline Val position(Val item, Val seq)
    { values(item, seq); return positionV(Thread::Get()); }

inline Val position(Val item, Val seq, Val k1, Val v1)
    { values(item, seq, k1, v1); return positionV(Thread::Get()); }

inline Val position(Val item, Val seq, Val k1, Val v1, Val k2, Val v2)
    { values(item, seq, k1, v1, k2, v2); return positionV(Thread::Get()); }

inline Val pop(Val& r)
    { Val x = car(r); r = cdr(r); return x; }

inline Val print_object(Val x, Val s)
    { return funcall(Qprint_object, x, s); }

inline void push(Val x, Val& r)
    { r = cons(x, r); }

// [S]
inline Val stream_pathname(Val x)
    { return funcall(Qstream_pathname, x); }

inline Val string_downcase(Val a)
    { values(a); return string_downcaseV(Thread::Get()); }

inline Val string_upcase(Val a)
    { values(a); return string_upcaseV(Thread::Get()); }

inline Val symbol_name(Val a)
    { return a->StaticCast<Symbol>()->m_name; }

// [T]
inline Val truename(Val x)
    { return funcall(Qtruename, x); }

// [W]
inline void write_char(char16 wch, Val s = nil)
    { write_char(Character::FromCode(wch), s); }

inline void write_string(const char* psz, Val s = nil)
    { write_string(psz, ::lstrlenA(psz), s); }

inline void write_string(const char16* pwsz, Val s = nil)
    { write_string(pwsz, ::lstrlenW(pwsz), s); }

inline bool stream_p(Val x) { return streamp(x); }

// for check_type macro
inline bool character_p(Val x)  { return characterp(x); }
inline bool class_p(Val x)      { return classp(x); }
inline bool cons_p(Val x)       { return consp(x); }
inline bool fixnum_p(Val x)     { return fixnump(x); }
inline bool list_p(Val x)       { return listp(x); }
inline bool function_p(Val x)   { return functionp(x); }
inline bool package_p(Val x)    { return x->Is<Package>(); }
inline bool string_p(Val x)     { return stringp(x); }
inline bool symbol_p(Val x)     { return symbolp(x); }
inline bool value_cell_p(Val x) { return x->Is<ValueCell>(); }

// Fast arithmetic
inline Val xxadd(Val a, Val b)
    { return Fixnum::Encode(Fixnum::Decode_(a) + Fixnum::Decode_(b)); }

inline Val xxadd(Val a, Int b)
    { return xxadd(a, Fixnum::Encode(b)); }

inline Val xxsub(Val a, Val b)
    { return Fixnum::Encode(Fixnum::Decode_(a) - Fixnum::Decode_(b)); }

inline Val xxsub(Val a, Int b)
    { return xxsub(a, Fixnum::Encode(b)); }

} // CommonLisp

#define VAR(x) (VAR_ ## x->StaticCast<ValueCell>()->m_value)

void __declspec(noreturn) SignalTypeError(Val, Val);

class BasicHost : public Record_<BasicHost, Layout_basic_host>
{
    public: static bool Is_(const Datum* const x)
        { return subclassp(class_of(const_cast<Val>(x)), CLASS_basic_host); }
}; // BasicHost

class Class : public Instance_<Class, Layout_class>
{
    public: static Int Decode_(const Datum* const x)
        { return Storage::Decode_(x->StaticCast<Instance>()->m_storage); }

    public: static bool Is_(const Datum* const x)
        { return classp(const_cast<Val>(x)); }
}; // Class

class Pathname : public Record_<Pathname, Layout_pathname>
{
    public: static bool Is_(const Datum* const x)
        { return pathnamep(const_cast<Val>(x)); }
}; // Pathname

class Collector
{
    private: Val m_head;
    private: Val m_tail;

    public: Collector() :
        m_head(nil),
        m_tail(nil) {}

    public: Val Add(Val x)
    {
        if (nil == m_head)
        {
            m_head = list(x);
            m_tail = m_head;
        }
        else
        {
            m_tail = setf_cdr(list(x), m_tail);
        }
        
        return x;
    } // Add

    public: Val Get() const
        { return m_head; }
}; // Collector

class StringData
{
    private: const char16*  m_pwchEnd;
    private: const char16*  m_pwchStart;

    // ctor
    public: StringData(Val string, Val start = zero, Val end = nil)
    {
        Int cwch;

        if (SimpleString* p = string->DynamicCast<SimpleString>())
        {
            m_pwchStart = p->GetStart();
            cwch        = p->GetLength();
        }
        else if (StringObject* p = string->DynamicCast<StringObject>())
        {
            Val ofs;
            Val data = p->GetData(&ofs);
            m_pwchStart = data->StaticCast<SimpleString>()->GetStart();
            m_pwchStart += Fixnum::Decode_(ofs);
            cwch = Fixnum::Decode_(p->m_fill_pointer);
        }
        else
        {
            SignalTypeError(string, Qstring);
        }

        Int iStart = fixnump(start) ? Fixnum::Decode_(start) : -1;

        Int iEnd   =
            nil == end   ? cwch :
            fixnump(end) ? Fixnum::Decode_(end) : -1;

        if (iStart < 0 || iStart > cwch)
        {
            error(Qunbound_index,
                Kdatum,    start,
                Kexpected_type, list(Qinteger, zero, Fixnum::Encode(cwch)),
                Ksequence, string );
        }

        if (iEnd < 0 || iEnd > cwch)
        {
            error(Qunbound_index,
                Kdatum,    end,
                Kexpected_type, list(Qinteger, zero, Fixnum::Encode(cwch)),
                Ksequence, string );
        }

        if (iStart > iEnd)
        {
            error(Qunbound_index,
                Kdatum,    end,
                Kexpected_type, list(Qinteger, start, Fixnum::Encode(cwch)),
                Ksequence, string );
        }

        m_pwchEnd    = m_pwchStart + iEnd;
        m_pwchStart += iStart;
    } // StringData

    // [E]
    public: class Enum
    {
        protected: const char16*  m_pwch;
        protected: const char16*  m_pwchEnd;

        public: Enum(const StringData* p) :
            m_pwchEnd(p->m_pwchEnd),
            m_pwch(p->m_pwchStart) {}

        public: bool   AtEnd() const { return m_pwch >= m_pwchEnd; }
        public: char16 Get()   const { ASSERT(!AtEnd()); return *m_pwch; }
        public: const char16* GetPtr() const { return m_pwch; }
        public: void   Next()        { ASSERT(!AtEnd()); m_pwch++; }
    }; // Enum

    // [G]
    public: const char16* GetEnd() const
        { return m_pwchEnd; }

    public: Int  GetLength() const
        { return m_pwchEnd - m_pwchStart; }

    public: const char16* GetStart() const
        { return m_pwchStart; }
}; // StringData

class StackStringBase
{
    public: Val m_string;
    public: Val m_not_used;
    public: Val m_classd;
    public: Val m_length;

    public: static bool Is_(const Datum* const x)
    {
        SimpleString* p = x->DynamicCast<SimpleString>();
        when (NULL == p) return false;

        StackStringBase* q = reinterpret_cast<StackStringBase*>(
            reinterpret_cast<Val*>(p) - 2 );
        ASSERT(NULL != q);

        return q->m_string == x;
    } // Is_
}; // StackString

// StackString_
template<int t_cwch = 100>
class ALIGN_BINVEC StackString_ : public StackStringBase
{
    private: char16 m_rgwch[t_cwch];

    public: StackString_(const char* psz)
        { init(psz, ::lstrlenA(psz)); }

    public: StackString_(const char* pch, size_t cch)
        { init(pch, cch); }

    public: StackString_(const char16* pwsz)
        { init(pwsz, ::lstrlenW(pwsz)); }

    public: StackString_(const char16* pwch, size_t cwch)
        { init(pwch, cwch); }

    public: operator Val() const { return m_string; }

    private: void init(const char* pchStart, size_t cch)
    {
        if (cch + 1 > t_cwch)
        {
            m_string = make_string(pchStart, cch);
            return;
        }

        m_classd = CLASSD_simple_string;
        m_length = Fixnum::Encode(cch);
        m_string = reinterpret_cast<SimpleString*>(&m_classd)->Encode();
        const char* pchEnd = pchStart + cch;
        for (const char* pch = pchStart; pch < pchEnd; pch++)
        {
            m_rgwch[pch - pchStart] = *pch;
        } // for pch
        m_rgwch[cch] = 0;
    } // init

    private: void init(const char16* pwch, size_t cwch)
    {
        if (cwch + 1 > t_cwch)
        {
            m_string = make_string(pwch, cwch);
            return;
        }

        m_classd = CLASSD_simple_string;
        m_length = Fixnum::Encode(cwch);
        m_string = reinterpret_cast<SimpleString*>(&m_classd)->Encode();
        CopyMemory(m_rgwch, pwch, sizeof(char16) * cwch);
        m_rgwch[cwch] = 0;
    } // init
}; // StatckString_

typedef StackString_<> StackString;

} // TinyCl

#endif //!defined(INCLUDE_tinycl_rtl_h)
