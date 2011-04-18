//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Object Layout
// tinycl_layout.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_layout.h#21 $
//
#if !defined(INCLUDE_tinycl_layout_h)
#define INCLUDE_tinycl_layout_h

namespace TinyCl
{

class Datum : public AsInt
{
    public: template<typename T> T* DynamicCast() const
    {
        unless (Is<T>()) return NULL;
        return FromInt<T>(T::Decode_(this));
    } // DyamicCast

    public: int GetTag() const 
        { return static_cast<int>(ToInt()) & Arch::TagMask; }

    public: template<typename T> bool Is() const
        { return T::Is_(this); }

    public: template<typename T> T* StaticCast() const
    {
        Int iAddr = T::Decode_(const_cast<Datum*>(this));
        return FromInt<T>(iAddr);
    } // StaticCast

    public: template<typename T> T* To() const
    {
        return FromInt<T>(ToInt());
    } // To
}; // Datum

/// <summary>
///   Represents fixnum object.
/// </summary>
class Fixnum : public Datum
{
    public: enum Constant
    {
        Tag     = Arch::Tag_Fixnum,
        TagBits = Arch::TagBits - 1,
        TagMask = (1 << TagBits) - 1,
        Bits    = Arch::Bits - TagBits,

        Zero    = 0,
        One     = 1 << TagBits,
    }; // Constant

    public: static const Int MostPositive =  
                (static_cast<Int>(1) << (Bits - 1)) - 1;

    public: static const Int MostNegative =  
                static_cast<Int>(-1) << (Bits - 1);

    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        return x->ToInt() >> TagBits;
    } // Decode_

    public: static Val Encode(Int iVal)
    {
        ASSERT(iVal >= MostNegative);
        ASSERT(iVal <= MostPositive);
        return FromInt(iVal << TagBits);
    } // Encode

    public: static Val Encode(const void* pv)
    {
        Int iVal = reinterpret_cast<Int>(pv);
        return FromInt(iVal);
    } // Encode

    public: static Val FromInt(Int iVal)
    {
        ASSERT(0 == (iVal & TagMask));
        return reinterpret_cast<Val>(iVal);
    } // FromInt

    public: static bool Is_(const Datum* const x)
        { return Tag == (x->GetTag() & TagMask); }

    public: template<typename T> static T* To(const Datum* const x)
    {
        ASSERT(Is_(x));
        return reinterpret_cast<T*>(x->ToInt());
    } // To
}; // Fixnum

namespace CommonLisp
{

Val const zero = reinterpret_cast<Val>(0);
Val const one  = reinterpret_cast<Val>(1 << Fixnum::TagBits);
Val const two  = reinterpret_cast<Val>(2 << Fixnum::TagBits);
Val const minus_one = reinterpret_cast<Val>(-1 << Fixnum::TagBits);

} // CommonLisp

template<class T, int Tag_>
class Datum_ : public AsInt
{
    public: enum Constant
    {
        Tag     = Tag_,
        TagMask = Arch::TagMask,
    }; // Constant

    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Tag == x->GetTag());
        ASSERT(T::Is_(x));
        return x->ToInt() - Tag;
    } // Decode_

    public: Val Encode() const
    {
        return FromInt<Datum>(ToInt() + Tag);
    } // Encode

    public: static bool Is_(const Datum* const x)
    {
        return Tag == x->GetTag();
    } // Is_

    public: template<typename T> T* To() const
    {
        return FromInt<T>(ToInt());
    } // To
}; // Datum_

//////////////////////////////////////////////////////////////////////
//
// Declare CLASSD_xxx
//
struct  Layout_class_description;
typedef Layout_class_description Layout_classd;
struct  LayoutX_class;

//////////////////////////////////////////////////////////////////////
//
// Define Layout
//
#define direct_slot__(mp_cty, mp_field, mp_cname, mp_ty) \
    mp_cty mp_field;

#define inherited_slot_(mp_name) \
    Val m_ ## mp_name;

#define deflayout(mp_name, mp_meta, mp_layout) \
    struct Layout_ ## mp_name  mp_layout {

#define endlayout(mp_name) };

#include "./tinycl_layout.inc"
typedef Layout_class_description Layout_classd;

CASSERT(sizeof(Layout_class)    == sizeof(Layout_built_in_class));
CASSERT(sizeof(Layout_class)    == sizeof(Layout_standard_class));
CASSERT(sizeof(Layout_class)    == sizeof(Layout_structure_class));
CASSERT(sizeof(Layout_cons)     == sizeof(Val) * 2);
CASSERT(sizeof(Layout_instance) == sizeof(Val) * 2);
CASSERT(sizeof(Layout_storage)  == sizeof(Val) * 2);
CASSERT(sizeof(Layout_symbol)   == sizeof(Val) * 6);

// For static allocations
CASSERT(sizeof(Layout_classd)     % Arch::Align_Record == 0);
CASSERT(sizeof(Layout_setf_cell)  % Arch::Align_Record == 0);
CASSERT(sizeof(Layout_tlv_record) % Arch::Align_Record == 0);
CASSERT(sizeof(Layout_value_cell) % Arch::Align_Record == 0);

CASSERT(sizeof(Layout_class)      % Arch::Align_Instance == 0);

namespace CommonLisp
{
    using TinyCl::Layout_error;
    using TinyCl::Layout_input_stream;
    using TinyCl::Layout_output_stream;
    using TinyCl::Layout_package;
    using TinyCl::Layout_structure_object;
} // CommonLisp

class ClassD;

class CodeObject :
    public Datum_<CodeObject, Arch::Tag_FunObj>,
    public Layout_native_code_function
{
    // [G]
    public: ClassD* GetClassD() const
        { return m_classd->StaticCast<ClassD>(); }

    public: uint8* GetCodeStart() const
    { 
        CodeObject* p = const_cast<CodeObject*>(this + 1);
        return reinterpret_cast<uint8*>(p);
    } // GetCodeStart

    public: size_t GetSize() const
        { return m_length->ToInt(); }
}; // CodeObject

class Record :
    public Datum_<Record, Arch::Tag_Record>,
    public Layout_record_object
{
    // [G]
    public: ClassD* GetClassD() const
        { return m_classd->StaticCast<ClassD>(); }

    public: size_t GetSize() const;
}; // Record

//////////////////////////////////////////////////////////////////////
//
// Null
//
class Null : public Datum
{
    public: Val             m_classd;       // +20 +20
    public: Val             m_length;       // +24 +28
    public: Val             m_not_used_28;  // +28 +30  [0]
    public: Val             m_not_used_2C;  // +2C +38  [1]
    public: Val             m_not_used_30;  // +30 +40  [2]
    public: Layout_cons     m_Cons;         // +34 +48  [3]
                                            // +38 +50  [4]
    public: Val             m_not_used_3C;  // +3C +58  [5]
    public: Layout_symbol   m_Symbol;       // +40 +60  [6] m_classd
                                            // +44 +68  [7] m_hash_code
                                            // +48 +70  [8] m_name
                                            // +4C +78  [9] m_package
                                            // +50 +80  [10] m_function
                                            // +54 +88  [11] m_plist
                                            // +58 +90  [12]

    public: enum Constant
    {
        Tag = Arch::Tag_Null,
    }; // Constant

    public: static Int Decode_(const Datum* const val)
    {
        return  val->ToInt() -
                offsetof(Null, m_Symbol) -
                Tag;
    } //  Decode_

    public: Val Encode() const
    {
        return FromInt<Datum>(
            ToInt() +
            offsetof(Null, m_Symbol) +
            Tag );
    } // Encode
}; // Null

struct StaticReco
{
    Mm::Area    m_oArea;
    Null        m_Nil;
}; // StaticReco

struct StaticBino
{
    Mm::Area            m_oArea;
    Layout_marker       m_Unbound;
    Layout_marker       m_Free;
    Layout_marker       m_Removed;
    Layout_double_float m_zero64;
    Layout_double_float m_pi64;

    #define defloat32(mp_pkg, mp_cname, mp_s, mp_e, mp_i) \
        Layout_single_float m_ ## mp_cname;

    #define defloat64(mp_pkg, mp_cname, mp_s, mp_e, mp_h, mp_l) \
        Layout_double_float m_ ## mp_cname;

    #include "./tinycl_float.inc"

    Layout_character    mv_char[Arch::CharCodeLimit];

    // most-positive-single-float = 3.4028235f38
    //  (integer-length (ash #xffffff 26)) = 128
    struct
    {
        Val         m_classd;
        Val         m_length;

        // +1 for sign
        // +1 for alignment padding
        Arch::Bigit m_rgBigit[128 / Arch::BigitBits + 2];
    } m_bignum_most_positive_float32;

    // most-positive-double-float = 1.7976931348623157d308
    //  (integer-length (ash #xfffffffffffff8 968)) = 1024
    struct
    {
        Val         m_classd;
        Val         m_length;

        // +1 for sign
        // +1 for alignment padding
        Arch::Bigit m_rgBigit[1024 / Arch::BigitBits + 2];
    } m_bignum_most_positive_float64;
}; // StaticBino

#define ROUNDUP(x, y) \
    ( (x + y - 1) / y * y )

const Int BINO_BASE = LISP_BASE;

const Int CHAR_BASE = BINO_BASE + offsetof(StaticBino, mv_char);

const Int BINO_AREA_SIZE = ROUNDUP(sizeof(StaticBino), Arch::AllocUnit);

const Int RECO_BASE = BINO_BASE + BINO_AREA_SIZE;

#define BIGNUM_most_positive_float32 \
    reinterpret_cast<Val>( \
        BINO_BASE + \
        offsetof(StaticBino, m_bignum_most_positive_float32) + \
        Arch::Tag_Record )

#define BIGNUM_most_positive_float64 \
    reinterpret_cast<Val>( \
        BINO_BASE + \
        offsetof(StaticBino, m_bignum_most_positive_float64) + \
        Arch::Tag_Record )

namespace CommonLisp
{

defstatic_(nil,
    RECO_BASE + Arch::Tag_Null + offsetof(StaticReco, m_Nil) +
    offsetof(Null, m_Symbol) );

} // CommonLisp

using namespace CommonLisp;

defstatic_(MARKER_unbound,
    BINO_BASE + Arch::Tag_Record + offsetof(StaticBino, m_Unbound) );

defstatic_(MARKER_free,
    BINO_BASE + Arch::Tag_Record + offsetof(StaticBino, m_Free) );

defstatic_(MARKER_removed,
    BINO_BASE + Arch::Tag_Record + offsetof(StaticBino, m_Removed) );

#define internal_make_char(c) reinterpret_cast<Val>( \
    BINO_BASE + Character::Tag + offsetof(StaticBino, mv_char[c]) )

enum ClassIndex
{
    #define deflayout(mp_cname, mp_meta, mp_layout) \
        ClassIndex_ ## mp_cname,

    #include "./tinycl_layout.inc"

    ClassIndex_Limit,
}; // ClassIndex

enum ClassDIndex
{
    #define defabstract(mp_cname, mp_meta, mp_layout)

    #define deflayout(mp_cname, mp_meta, mp_layout) \
        ClassDIndex_ ## mp_cname,

    #include "./tinycl_layout.inc"

    ClassDIndex_Limit,
}; // ClassDIndex

enum SymbolIndex
{
    #define definternal(mp_pkg, mp_cname, mp_lname) \
        SymbolIndex_Q ## mp_cname,

    #include "./tinycl_syms_cl.inc"

    #define defkeyword(mp_cname, mp_lname) \
        SymbolIndex_K ## mp_cname,

    #include "./tinycl_keys.inc"

    #define definternal(mp_pkg, mp_cname, mp_lname) \
        SymbolIndex_Q ## mp_cname,

    #include "./tinycl_syms.inc"

    SymbolIndex_Limit
}; // SymbolIndex

enum SetfCellIndex
{
    #define defsetf(mp_cname) \
        SetfCellIndex_ ## mp_cname,

    #include "./rtl/tinycl_setfs.inc"

    SetfCellIndex_Limit,
}; // SetfCellIndex

enum Tlv
{
    #define deftlv(mp_pkg, mp_cname, mp_lname) TLV_ ## mp_cname,

    TLV_Zero,

    #include "./tinycl_tlvs.inc"

    TLV_Limit,
}; // Tlv

namespace CommonLisp
{

using TinyCl::TLV_Aerror_outputA;
using TinyCl::TLV_AfeaturesA;
using TinyCl::TLV_Aquery_ioA;
using TinyCl::TLV_Astandard_inputA;
using TinyCl::TLV_Astandard_outputA;
using TinyCl::TLV_Atrace_outputA;
using TinyCl::TLV_Aterminal_ioA;

} // CommonLisp

enum ValueCellIndex
{
    #define defloat32(mp_pkg, mp_name, mp_s, mp_e, mp_i) \
        ValueCellIndex_ ## mp_name,

    #define defloat64(mp_pkg, mp_name, mp_s, mp_e, mp_h, mp_l) \
        ValueCellIndex_ ## mp_name,

    #include "./tinycl_float.inc"

    #define defvar(mp_pkg, mp_cname, mp_init) \
        ValueCellIndex_ ## mp_cname,

    #include "./tinycl_vars.inc"

    ValueCellIndex_Limit,
}; // ValueCellIndex

enum PackageIndex
{
    #define defpackage(mp_cname, mp_params) \
        PackageIndex_ ## mp_cname,

    #include "./tinycl_pkgs.inc"

    PackageIndex_Limit,
}; // PackageIndex

const Int STATIC_OBJECT_BASE = RECO_BASE + sizeof(StaticReco);

#define defstatic_symbol(mp_name) \
    defstatic_(mp_name, \
        SYMBOL_BASE + \
        sizeof(Layout_symbol) * SymbolIndex_ ## mp_name + \
        Arch::Tag_Record );

const Int TLV_Start = 1;

#define TINYCL_LIST
    #include "./init/tinycl_defstatic.h"
    #include "./tinycl_layout.inc"

    #include "./tinycl_syms.inc"

    #include "./tinycl_float.inc"

    namespace Keyword
    {
        #include "./tinycl_keys.inc"
    } // Keyword

    namespace CommonLisp
    {
        #include "./tinycl_syms_cl.inc"
        Val const t = Qt;
    } // CommonLisp

    #include "./tinycl_pkgs.inc"
    #include "./rtl/tinycl_setfs.inc"
    #include "./tinycl_tlvs.inc"
    #include "./tinycl_vars.inc"
#undef TINYCL_LIST
#include "./tinycl_end_list.h"

#define Qnil nil

defstatic_(CLASSD_classd, CLASSD_class_description);
defstatic_(CLASS_classd,  CLASS_class_description);

namespace Extension
{
    using TinyCl::Qexternal_format;
    using TinyCl::Qstream_read_char;
    using TinyCl::Qstream_unread_char;
    using TinyCl::Qstream_write_char;
    using TinyCl::Qtype_specifier;
} // Extension

template<typename T, typename L>
class Record_ : public Datum_<T, Arch::Tag_Record>, public L
{
    public: void* operator new(size_t)
    {
        Val regex = Thread::Get()->AllocRecord(T::ClassD_());
        return regex->StaticCast<T>();
    } // new

    public: ClassD* GetClassD() const
        { return m_classd->StaticCast<ClassD>(); }

    public: static bool Is_(const Datum* const x)
    {
        unless (Record::Is_(x)) return false;
        Val classd = FromInt<T>(Record::Decode_(x))->m_classd;
        return T::ClassD_() == classd;
    } // Is_
}; // Record_

template<typename T, typename L>
class BinObj_ : public Datum_<T, Arch::Tag_Record>, public L
    {};

template<typename T, typename L, typename E>
class DataVector_ : public Record_<T, L>
{
    // [E]
    public: class Enum
    {
        private: E* m_pEnd;
        private: E* m_pRunner;

        public: Enum(Val x) { init(x->StaticCast<T>()); }
        public: Enum(T* p) { init(p); }

        public: bool AtEnd() const
            { return m_pRunner >= m_pEnd; }

        public: E Get() const
            { ASSERT(!AtEnd()); return *m_pRunner; }

        private: void init(T* p)
        {
            m_pRunner = p->GetStart();
            m_pEnd    = p->GetStart() + p->GetLength();
        } // init

        public: void Next()
            { ASSERT(!AtEnd()); m_pRunner++; }

        public: E Set(E x)
            { ASSERT(!AtEnd()); return *m_pRunner = x; }
    }; // Enum

    // [G]
    public: E* GetStart() const
    {
        return const_cast<E*>(reinterpret_cast<const E*>(this + 1));
    } // Get

    public: Int GetLength() const
    {
        return Fixnum::Decode_(m_length);
    } // GetLength
}; // DataVector_

class ClassD : public Record_<ClassD, Layout_classd>
{
    public: static Val ClassD_() { return CLASSD_classd; }

    public: enum Format
    {
        Format_None         = Fixnum::Zero,
        Format_Record       = Fixnum::One * 1,
        Format_Instance     = Fixnum::One * 2,
        Format_Vector       = Fixnum::One * 3,
        Format_BinVec       = Fixnum::One * 4,
        Format_Immediate    = Fixnum::One * 5,
        Format_Cons         = Fixnum::One * 6,
        Format_Storage      = Fixnum::One * 7,
    }; // Format

    public: size_t GetElementSize() const
        { return Fixnum::Decode_(m_element_size); }

    public: size_t GetExtraLength() const
        { return Fixnum::Decode_(m_extra_length); }

    public: size_t GetFixedSize() const
        { return Fixnum::Decode_(m_fixed_size); }

    public: Format GetFormat() const
        { return static_cast<Format>(m_format->ToInt()); }
}; // ClassD

class Instance : public Record_<Instance, Layout_instance>
{
    public: template<typename T> T* GetStorage() const
    {
        return reinterpret_cast<T*>(m_storage->StaticCast<Storage>());
    } // GetStorage

    public: static bool Is_(const Datum* const x)
    {
        unless (x->Is<Record>()) return false;
        Instance* p = FromInt<Instance>(x->ToInt() - Tag);
        return p->m_classd->StaticCast<ClassD>()->m_format->ToInt() ==
                ClassD::Format_Instance;
    } // Is_
}; // Instance

CASSERT(sizeof(Instance) == sizeof(Layout_instance));

class Storage : public Record_<Storage, Layout_storage>
{
    public: static Val ClassD_() { return CLASSD_storage; }

    public: Val* GetStart() const
        { return const_cast<Val*>(reinterpret_cast<const Val*>(this + 1)); }
}; // Storage


// FIXME yosi@msn.com 2007-09-02 Should we use Instance_?
template<typename T, typename S>
class Instance_ : public S
{
    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        return Storage::Decode_(x->StaticCast<Instance>()->m_storage);
    } // Decode_

    public: static bool Is_(const Datum* const x)
    {
        Instance* p = x->DynamicCast<Instance>();
        when (NULL == p) return false;
        ClassD* pClassD =  p->m_classd->StaticCast<ClassD>();
        return nil != memq(T::Class_(), pClassD->m_class_precedence_list);
    } // Is_
}; // Instance_

typedef Arch::Bigit Bigit;

class Bignum : public DataVector_<Bignum, Layout_bignum, Bigit>
{
    public: static Val ClassD_() { return CLASSD_bignum; }
}; // Bignum

/// <remark>
///   Lisp character object.
/// </remark>
class Character : public Record_<Character, Layout_character>
{
    public: static Val ClassD_() { return CLASSD_character; }

    public: enum Constant
    {
        Tag         = Arch::Tag_Record,
        Limit       = 1 << 16,
        Max         = Limit - 1,

        OpenParen   = 0x28,
        CloseParen  = 0x29,

        #if SIZEOF_VAL == 4
            ShiftCount      = 3,
            CaseShiftCount  = 8 + 5 + 2,
        #endif // SIZEOF_VAL == 4

        #if SIZEOF_VAL == 8
            ShiftCount      = 4,
            LowerShiftCount = 8 + 5 + 3,
            UpperShiftCount = LowerShiftCount + 16,
            Mask            = 0xFFFF,
        #endif // SIZEOF_VAL == 8
    }; // Constant

    public: enum Attr
    {
        Attr_CategoryMask   = Fixnum::One * 31,
        Attr_Alpha          = Fixnum::One * 32,
        Attr_Numeric        = Fixnum::One * 64,
        Attr_Graphic        = Fixnum::One * 128,
        Attr_Whitespace     = Fixnum::One * 256,
        Attr_LowerCase      = Fixnum::One * 512,
        Attr_UpperCase      = Fixnum::One * 1024,
        Attr_Standard       = Fixnum::One * 2048,

        Attr_AlphaNumeric = Attr_Alpha | Attr_Numeric,
        Attr_BothCase     = Attr_LowerCase | Attr_UpperCase,
    }; // Attr

    // Unicode Category
    public: enum Category
    {
        // General Category Constants
        Category_ControlMin             = 0,
        Category_Unassigned             = 0,     // Cn
        Category_Control                = 1,     // Cc
        Category_Format                 = 2,     // Cf
        Category_PrivateUse             = 3,     // Co
        Category_Surrogate              = 4,     // Cs
        Category_ControlMax             = 4,

        // Note: For \p{L&}, we assign consecutive numbers for Ll, Lt, Lu.
        Category_LetterMin              = 5,
        Category_LowercaseLetter        = 5,     // Ll
        Category_TitlecaseLetter        = 6,     // Lt
        Category_UppercaseLetter        = 7,     // Lu
        Category_ModifierLetter         = 8,     // Lm
        Category_OtherLetter            = 9,     // Lo
        Category_LetterMax              = 9,

        Category_MarkMin                = 10,
        Category_CombiningSpacingMark   = 10,     // Mc
        Category_EnclosingMark          = 11,     // Me
        Category_NonSpacingMark         = 12,     // Mn
        Category_MarkMax                = 12,

        Category_NumberMin              = 13,
        Category_DecimalDigitNumber     = 13,     // Nd
        Category_LetterNumber           = 14,     // Nl
        Category_OtherNumber            = 15,     // No
        Category_NumberMax              = 15,

        Category_PunctuationMin         = 16,
        Category_ConnectorPunctuation   = 16,     // Pc
        Category_DashPunctuation        = 17,     // Pd
        Category_ClosePunctuation       = 18,     // Pe
        Category_FinalPunctuation       = 19,     // Pf
        Category_InitialPunctuation     = 20,     // Pi
        Category_OtherPunctuation       = 21,     // Po
        Category_OpenPunctuation        = 22,     // Ps
        Category_PunctuationMax         = 22,

        Category_SymbolMin              = 23,
        Category_CurrencySymbol         = 23,     // Sc
        Category_ModifierSymbol         = 24,     // Sk
        Category_MathSymbol             = 25,     // Sm
        Category_OtherSymbol            = 26,     // So
        Category_SymbolMax              = 26,

        Category_SeparatorMin           = 27,
        Category_LineSeparator          = 27,     // Zl
        Category_ParagraphSeparator     = 28,     // Zp
        Category_SpaceSeparator         = 29,     // Zs
        Category_SeparatorMax           = 29,
    }; // Category

    // Downcase
    public: Val Downcase() const
    {
        unless (IsUpperCase()) return Encode();
        return Character::FromCode(static_cast<char16>(
            m_data->ToInt() >> CaseShiftCount ) );
    } // Downcase

    // [E]
    public: static Val FromCode(char16);

    // [I]
    public: bool IsLowerCase() const
        { return 0 != (m_data->ToInt() & Attr_LowerCase); }

    public: bool IsUpperCase() const
        { return 0 != (m_data->ToInt() & Attr_UpperCase); }

    // [T]
    public: char16 ToCode() const;

    // [U]

    /// <summary>
    ///   Returns upcase character.
    /// </summary>
    /// <returns>Upcase character object</returns>
    public: Val Upcase() const
    {
        unless (IsLowerCase()) return Encode();
        return Character::FromCode(static_cast<char16>(
            m_data->ToInt() >> CaseShiftCount ) );
    } // Upcase

    /// <summary>
    ///   Returns upcase character.
    /// </summary>
    /// <returns>Upcase character object</returns>
    public: static Val UpcaseFromCode(char16 wch)
        { return FromCode(wch)->StaticCast<Character>()->Upcase(); }
}; // Character

class Charset : 
    public Record_<Charset, Layout_charset>
{
    public: static Val ClassD_() { return CLASSD_charset; }
}; // Charset

class ClosedCell : public Record_<ClosedCell, Layout_closed_cell>
{
    public: static Val ClassD_() { return CLASSD_closed_cell; }
}; // ClosedCell

class Cons : public Datum, public Layout_cons
{
    public: enum Constant
    {
        Tag  = Arch::Tag_Cons,

        // FIXME 2008-01-03 yosi@msn.com We should use more meaningful name
        // instead of Cons::Tag2.
        Tag2 = Tag + sizeof(Layout_cons),
    }; // Constatn

    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        return x->ToInt() - Tag2;
    } // Decode_

    public: Val Encode() const
    {
        return FromInt<Datum>(ToInt() + Tag2);
    } // Encode

    public: static bool Is_(const Datum* const x)
        { return Tag == x->GetTag (); }
}; // Cons

CASSERT(sizeof(Layout_cons) == sizeof(Val) * 2);

class DoubleFloat :
    public BinObj_<DoubleFloat, Layout_double_float>
{
    public: static Val ClassD_() { return CLASSD_double_float; }
}; // DoubleFloat

class ExternalFormat : 
    public Record_<ExternalFormat, Layout_external_format>
{
    public: static Val ClassD_() { return CLASSD_external_format; }
}; // ExternalFormat

class FunctionInformation :
    public Record_<FunctionInformation, Layout_function_information>
{
    public: static Val ClassD_() { return CLASSD_function_information; }
}; // FunctionInformation

/// <summary>
///   Represents simple-vector object.
/// </summary>
class SimpleVector :
    public DataVector_<SimpleVector, Layout_simple_vector, Val>
{
    public: static Val ClassD_() { return CLASSD_simple_vector; }
}; // SimpleVector

/// <summary>
///   Represents hash-table object.
/// </summary>
class HashTable : public Record_<HashTable, Layout_hash_table>
{
    public: static Val ClassD_() { return CLASSD_hash_table; }

    /// <summary>
    ///   Represents entry in hash-table.
    /// </summary>
    public: struct Entry
    {
        Val m_key;
        Val m_val;

        Val GetKey() const { return m_key; }
        Val GetVal() const { return m_val; }

        bool IsEmpty() const
            { return MARKER_free == m_key; }

        bool IsRemoved() const
            { return MARKER_removed == m_key; }

        void MarkEmpty()
        {
            m_key = MARKER_free;
            m_val = 0;
        } // MarkEmpty

        void MarkRemoved()
        {
            m_key = MARKER_removed;
            m_val = 0;
        } // MarkRemoved

        void Set(Val key, Val val)
        {
            m_key = key;
            m_val = val;
        } // Set

        Val SetVal(Val val)
        {
            return m_val = val;
        } // Set
    }; // Entry

    // [C]
    /// <summary>
    ///   Computes next access entry.
    /// </summary>
    /// <param name="pEntry">A hash-table entry.</param>
    /// <returns>Next entry of specified entry.</returns>
    private: Entry* computeNextEntry(Entry* pEntry) const
    {
        pEntry++;
        if (pEntry >= GetBtm())
        {
            pEntry = GetTop();
        }
        return pEntry;
    } // computeNextEntry

    /// <summary>
    ///   Computes previous access entry.
    /// </summary>
    /// <param name="pEntry">A hash-table entry.</param>
    /// <returns>Previous entry of specified entry.</returns>
    private: Entry* computePrevEntry(Entry* pEntry) const
    {
        if (pEntry <= GetTop())
        {
            pEntry = GetBtm();
        }
        --pEntry;
        return pEntry;
    } // computePrevEntry

    // [E]
    private: template<class T> class Enum_
    {
        public: struct Arg
        {
            Val m_hash_code;
            Val m_vector;

            Arg(Val vec, Val hc) :
                m_hash_code(hc),
                m_vector(vec) {}
        }; // Arg

        public: typedef Entry Entry;

        protected: Entry* m_pBtm;
        protected: Entry* m_pRunner;
        protected: Entry* m_pStart;
        protected: Entry* m_pTop;

        protected: Enum_(Val vector, Val hash_code)
            { init(vector, hash_code); }

        protected: Enum_(Arg oArg)
            { init(oArg.m_vector, oArg.m_hash_code); }

        public: void Add()
            { m_pTop[-1].m_key = add(m_pTop[-1].m_key, 1); }
            
        private: bool atEnd() const
            { return static_cast<const T*>(this)->AtEnd(); }

        public: Entry* Get() const
            { ASSERT(!atEnd()); return m_pRunner; }

        public: Val GetKey() const
            { ASSERT(!atEnd()); return  m_pRunner->m_key; }

        public: Val GetVal() const
            { ASSERT(!atEnd()); return  m_pRunner->m_val; }

        protected: void init(Val vector, Val hash_code)
        {
            SimpleVector* p = vector->StaticCast<SimpleVector>();
            m_pTop   = reinterpret_cast<Entry*>(p->GetStart() + 2);
            m_pBtm   = reinterpret_cast<Entry*>(
                p->GetStart() + p->GetLength() );

            Int iHashCode = Fixnum::Decode_(hash_code);

            m_pStart  = m_pTop + iHashCode % (m_pBtm - m_pTop);
            m_pRunner = m_pStart;
        } // init

        public: Val SetKey(Val key)
            { ASSERT(!atEnd()); return  m_pRunner->m_key = key; }

        public: Val SetVal(Val val)
            { ASSERT(!atEnd()); return  m_pRunner->m_val = val; }
    }; // Enum_

    public: class EnumVec : public Enum_<EnumVec>
    {
        private: Int m_nRest;

        public: EnumVec(Val vector) :
            Enum_<EnumVec>(vector, zero)
        {
            m_nRest = Fixnum::Decode_(m_pTop[-1].m_key);
            --m_pRunner;
            next();
        } // Enum

        public: bool AtEnd() const
            { return 0 == m_nRest; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            next();
            m_nRest -= 1;
        } // Next

        private: void next()
        {
            while (! AtEnd())
            {
                m_pRunner++;
                if (MARKER_free    != GetKey() &&
                    MARKER_removed != GetKey() )
                {
                    return;
                }
            } // while
        } // next
    }; // EnumVec

    public: class Enum : public EnumVec
    {
        public: Enum(Val htb) :
            EnumVec(htb->StaticCast<HashTable>()->m_vector) {}
    }; // Enum

    public: class EnumAll : public Enum_<EnumAll>
    {
        private: bool  m_fEnd;

        public: EnumAll(Arg oArg) :
            m_fEnd(false),
            Enum_<EnumAll>(oArg) {}

        public: bool AtEnd() const
            { return m_fEnd; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_pRunner++;
            if (m_pRunner == m_pBtm) m_pRunner = m_pTop;
            m_fEnd = m_pRunner == m_pStart;
        } // Next
    }; // EnumAll

    // [G]
    /// <summary>
    ///   Retrieve bottom of hash-table entry vector.
    /// </summary>
    public: Entry* GetBtm() const
    {
        SimpleVector* p = m_vector->StaticCast<SimpleVector>();
        return reinterpret_cast<Entry*>(p->GetStart() + p->GetLength());
    } // GetBtm

    /// <summary>
    ///   Retrieve number of entries in hash-table.
    /// </summary>
    public: Val GetCount() const
    {
        SimpleVector* p = m_vector->StaticCast<SimpleVector>();
        return p->GetStart()[0];
    } // GetCount

    /// <summary>
    ///   Retrieve top of hash-table entry vector.
    /// </summary>
    public: Entry* GetTop() const
    {
        SimpleVector* p = m_vector->StaticCast<SimpleVector>();
        return reinterpret_cast<Entry*>(p->GetStart() + 2);
    } // GetTop

    // [P]
    public: void Put(Entry*, Val, Val);

    // [R]
    public: void Remove(Entry*);
}; // HashTable

/// <summary>
///   Helper class for list.
/// </summary>
class List : public Cons
{
    // [D]
    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        return x->ToInt() - Tag - sizeof(Layout_cons);
    } // Decode_

    // [E]
    public: class Enum
    {
        private: Val m_runner;
        public: Enum(Val list) : m_runner(list) {}

        public: bool AtEnd() const
        {
            if (nil == m_runner)
            {
                return true;
            }

            ASSERT(m_runner->Is<Cons>());
            return false;
        } // AtEnd

        public: Val  Get() const
        {
            ASSERT(!AtEnd());
            return m_runner->StaticCast<Cons>()->m_car;
        } // Get

        public: Val  GetCons() const { return m_runner; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_runner = m_runner->StaticCast<Cons>()->m_cdr;
        } // Next

        public: Val Set(Val x)
        {
            ASSERT(!AtEnd());
            return m_runner->StaticCast<Cons>()->m_car = x;
        } // Val
    }; // Enum

    // [I]
    public: static bool Is_(const Datum* const x)
    {
        int iTag = (x->GetTag () & 3);
        return Null::Tag == iTag;
    } // Is_
}; // List

class SetfCell : public Record_<SetfCell, Layout_setf_cell>
{
    public: static Val ClassD_() { return CLASSD_setf_cell; }
}; // SetfCell

class SimpleString :
    public DataVector_<SimpleString, Layout_simple_string, char16>
{
    public: static Val ClassD_() { return CLASSD_simple_string; }

    public: Val Hash() const
    {
        Int iHashCode = 0;
        const char16* pwchStart = GetStart();
        const char16* pwchEnd   = pwchStart + Fixnum::Decode_(m_length);
        for (const char16* pwch = pwchStart; pwch < pwchEnd; pwch++)
        {
            iHashCode ^= *pwch;
            iHashCode  = ((iHashCode & 0xFFFF) << 8) | (iHashCode >> 16);
        } // for

        iHashCode |= 1;
        iHashCode &= Fixnum::MostPositive;

        return Fixnum::Encode(iHashCode);
    } // Hash
}; // SimpleString

class SingleFloat :
    public BinObj_<SingleFloat, Layout_single_float>
{
    public: static Val ClassD_() { return CLASSD_single_float; }
}; // SingleFloat


class U32Vec :
    public DataVector_<U32Vec, Layout_unsigned_byte_32_vector, uint32>
{
    public: static Val ClassD_() { return CLASSD_unsigned_byte_32_vector; }
}; // U32Vec

const Val Carray_flag_fill_pointer = Fixnum::Encode(1 << 0);
const Val Carray_flag_displaced    = Fixnum::Encode(1 << 1);

class StringObject : public Record_<StringObject, Layout_string_object>
{
    public: static Val ClassD_() { return CLASSD_string_object; }

    public: Val GetData(Val* out_offset);

    public: Int GetLength() const
        { return Fixnum::Decode_(m_fill_pointer); }

    public: char16* GetStart() const;
}; // StringObject

class TlvRecord : public Record_<TlvRecord, Layout_tlv_record>
{
    public: static Val ClassD_() { return CLASSD_tlv_record; }
}; // TlvRecord

class ValueCell : public Record_<ValueCell, Layout_value_cell>
{
    public: static Val ClassD_() { return CLASSD_value_cell; }
}; // ValueCell

class Vector : 
    public Datum_<Vector, Record::Tag>, 
    public Layout_data_vector
{
    public: static bool Is_(const Datum* const x)
    {
        unless (x->Is<Record>()) return false;
        Vector* p = FromInt<Vector>(Record::Decode_(x));
        return p->m_classd >= CLASSD_vector_min &&
               p->m_classd <= CLASSD_vector_max;
    } // Is_
}; // Vector

CASSERT(sizeof(Vector) == sizeof(Layout_data_vector));


class VectorObject : public Record_<VectorObject, Layout_vector_object>
{
    public: static bool Is_(const Datum* const x)
    {
        if (Record* p = x->DynamicCast<Record>())
        {
            return p->m_classd >  CLASSD_data_vector_max &&
                   p->m_classd <= CLASSD_vector_max;
        }
        return false;
    } // Symbol::Is_
}; // VectorObject

class Symbol : public Record_<Symbol, Layout_symbol>
{
    public: static Val ClassD_() { return CLASSD_symbol; }

    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(x->Is<Symbol>());
        return x->ToInt() & ~TagMask;
    } // Decode_

    public: static bool Is_(const Datum* const x)
    {
        if (nil == x) return true;
        return Record_<Symbol, Layout_symbol>::Is_(x);
    } // Symbol::Is_
}; // Symbol

class Environment : public Record_<Environment, Layout_environment>
{
    public: static Val ClassD_() { return CLASSD_environment; }
}; // Environment

class Function : 
    public Datum,
    public Layout_native_code_object
{
    public: enum Contant
    {
        Tag = Arch::Tag_FunObj,
    }; // Constant

    public: static Int Decode_(const Datum* const x)
    {
        ASSERT(Is_(x));
        return x->ToInt() - Tag;
    } // Decode_

    public: Val Encode() const
    {
        return FromInt<Datum>(ToInt() + Tag);
    } // Encode

    // [I]
    public: static bool Is_(const Datum* const x)
        { return Tag == x->GetTag(); }
}; // Function

class HtmlEncodeStream :
    public Instance_<HtmlEncodeStream, Layout_html_encode_stream>
{
    public: static Val Class_() { return CLASS_html_encode_stream; }
}; // HtmlEncodeStream

class StructureObject :
    public Record_<StructureObject, Layout_structure_object> {};

class Package : public Record_<Package, Layout_package>
{
    public: static Val ClassD_() { return CLASSD_package; }
}; // Package

} // TinyCl

#endif //!defined(INCLUDE_tinycl_layout_h)
