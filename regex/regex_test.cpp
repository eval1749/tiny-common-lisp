#include "precomp.h"
// Regex Unit Test
//
// Copyright (C) 2012 by Project Vogue.
//
#undef override

#pragma warning(disable: 4365)
#pragma warning(disable: 4481)
#pragma warning(disable: 4514)
#pragma warning(disable: 4625)
#pragma warning(disable: 4626)
#pragma warning(disable: 4668)
#pragma warning(disable: 4710)
#pragma warning(disable: 4820)
#pragma warning(disable: 4826)
#pragma warning(disable: 4986)

#include <assert.h>
#include <ostream>

#include "IRegex.h"
#include "gtest/gtest.h"

namespace RegexTest {

typedef Regex::IRegex IRegex;
typedef Regex::char16 char16;

typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned int uint;

typedef Regex::Posn Posn;

template<typename T>
class scoped_ptr {
  private: T* pointer_;
  public: scoped_ptr(T& pointer) : pointer_(&pointer) {}
  public: scoped_ptr(T* pointer) : pointer_(pointer) {}
  public: ~scoped_ptr() { delete pointer_; }
  public: T& operator *() { return *pointer_; }
  public: T* operator ->() { return pointer_; }
  DISALLOW_COPY_AND_ASSIGN(scoped_ptr);
};

template<class BaseIterator>
class ConstIterator_ : public BaseIterator {
  public: typedef typename BaseIterator::ValueType ValueType;
  public: ConstIterator_(const BaseIterator& it) : BaseIterator(it) {}
  public: const ValueType& operator *() const { return Iterator::operator *(); }
  public: const ValueType* operator ->() const { return Iterator::operator ->(); }
};

template<typename T>
class Vector {
  private: T* elements_;
  private: int size_;

  public: class Iterator {
    public: typedef T& ReferenceType;
    public: typedef T* PointerType;
    public: typedef T ValueType;

    private: int index_;
    private: Vector& vector_;

    public: Iterator(Vector& vector, int index)
        : index_(index), vector_(vector) {}

    public: Iterator(const Iterator& other)
        : index_(other.index_), vector_(other.vector_) {}

    public: Iterator& operator=(const Iterator other) {
      assert(vector_ == other.vector_);
      index_= other.index_;
      return *this;
    }

    public: T& operator *() { return vector_[index_]; }
    public: const T& operator *() const { return vector_[index_]; }
    public: T* operator ->() { return &vector_[index_]; }
    public: const T* operator ->() const { return &vector_[index_]; }
    public: Iterator& operator ++() { ++index_; return *this; }

    public: bool operator ==(const Iterator& other) const {
      assert(vector_ == other.vector_);
      return index_ == other.index_;
    }

    public: bool operator !=(const Iterator& other) const {
      return !operator ==(other);
    }
  };

  public: typedef ConstIterator_<Iterator> ConstIterator;

  public: Vector(int size)
      : elements_(new T[size]),
        size_(size) {}

  public: Vector(const Vector& other)
      : elements_(new T[other.size_]),
        size_(other.size_) {
    auto i = 0;
    for (auto it = other.begin(); it != other.end(); ++it) {
      elements_[i++] = *it;
    }
  }

  public: Vector(Vector&& other)
      : elements_(other.elements_)
        size_(other.size_) {
    other.elements_ = nullptr;
    other.size_ = 0;
  }

  public: Vector& operator =(const Vector& other) {
    delete elements_;
    size_ = other.size_;
    elements_ = new T[size_];
    auto i = 0;
    for (auto it = other.begin(); it != other.end(); ++it) {
      elements_[i++] = *it;
    }
    return *this;
  }

  public: Vector& operator =(Vector&& other) {
    delete elements_;
    elements_ = other.elements_;
    size_ = other.size_;
    other.elements_ = nullptr;
    other.size_ = 0;
    return *this;
  }

  public: T& operator [](int index) {
    assert(index >= 0);
    assert(index < size_);
    return elements_[index];
  }

  public: const T& operator [](int index) const {
    assert(index >= 0);
    assert(index < size_);
    return elements_[index];
  }

  public: bool operator ==(const Vector& other) const {
    if (this == &other) {
      return true;
    }

    if (size() != other.size()) {
      return false;
    }

    auto other_it = other.begin();
    for (auto it = begin(); it != end(); ++it) {
      if (*it != *other_it) {
        return false;
      }
      ++other_it;
    }

    return true;
  }

  public: bool operator !=(const Vector& other) const {
    return !operator ==(other);
  }

  public: Iterator begin() { return Iterator(*this, 0); }

  public: ConstIterator begin() const {
    return ConstIterator(const_cast<Vector*>(this)->begin());
  }

  public: Iterator end() { return Iterator(*this, size()); }

  public: ConstIterator end() const {
    return ConstIterator(const_cast<Vector*>(this)->end());
  }

  public: int size() const { return size_; }
};

class String {
  private: char16* chars_;
  private: int size_;

  public: class Iterator {
    public: typedef char16& ReferenceType;
    public: typedef char16* PointerType;
    public: typedef char16 ValueType;

    private: int index_;
    private: String& string_;

    public: Iterator(String& string, int index)
        : index_(index), string_(string) {}

    public: Iterator(const Iterator& other)
        : index_(other.index_), string_(other.string_) {}

    public: Iterator& operator=(const Iterator other) {
      assert(string_ == other.string_);
      index_= other.index_;
      return *this;
    }

    public: bool operator ==(const Iterator& other) const {
      assert(string_ == other.string_);
      return index_ == other.index_;
    }

    public: bool operator !=(const Iterator& other) const {
      return !operator ==(other);
    }

    public: const char16& operator *() const { return string_[index_]; }
    public: Iterator& operator ++() { ++index_; return *this; }

    public: Iterator operator +(int n) const {
      return Iterator(string_, index_ + n);
    }

    public: Iterator operator -(int n) const {
      return Iterator(string_, index_ - n);
    }

    public: int operator -(const Iterator& other) const {
      return index_ - other.index_;
    }
  };

  public: typedef ConstIterator_<Iterator> ConstIterator;

  public: String() : chars_(nullptr), size_(0) {}

  public: String(const char* string8)
      : size_(::lstrlenA(string8)) {
    chars_ = new char16[size_ + 1];
    auto const end = string8 +  size_;
    auto dest = chars_;
    for (auto s = string8; s < end; ++s) {
      *dest++ = *s;
    }
    *dest = 0;
  }

  public: String(const char16* string16)
      : size_(::lstrlenW(string16)) {
    chars_ = new char16[size_ + 1];
    ::CopyMemory(chars_, string16, (size_ + 1) * sizeof(char16));
  }

  public: String(const String& other)
    : chars_(new char16[other.size() + 1]),
      size_(other.size_) {
    ::CopyMemory(chars_, other.chars_, (size_ + 1) *sizeof(char16));
  }

  public: String(String&& other)
    : chars_(other.chars_),
      size_(other.size_) {
    other.chars_ = nullptr;
    other.size_ = 0;
  }

  public: String(const ConstIterator& begin, const ConstIterator& end)
      : size_(end - begin) {
    chars_ = new char16[size_ + 1];
    auto dest = chars_;
    for (auto it = begin; it != end; ++it) {
      *dest++ = *it;
    }
    *dest = 0;
  }

  public: ~String() {
    delete chars_;
  }

  public: String& operator=(const String& other) {
    delete chars_;
    size_ = other.size_;
    chars_ = new char16[size_ + 1];
    ::CopyMemory(chars_, other.chars_, (size_ + 1) * sizeof(char16));
    return *this;
  }

  public: String& operator=(String&& other) {
    delete chars_;
    chars_ = other.chars_;
    size_ = other.size_;
    other.chars_ = nullptr;
    other.size_ = 0;
    return *this;
  }

  public: const char16& operator[](int index) const {
    assert(index >= 0);
    assert(index < size_);
    return chars_[index];
  }

  public: operator const char16*() const { return chars_; }

  public: bool operator ==(const String& other) const {
    return this == &other
        || !::memcmp(chars_, other.chars_, size_ * sizeof(char16));
  }

  public: bool operator !=(const String& other) const {
    return !operator ==(other);
  }

  public: Iterator begin() { return Iterator(*this, 0); }

  public: ConstIterator begin() const {
    return ConstIterator(const_cast<String*>(this)->begin());
  }
  public: Iterator end() { return Iterator(*this, size()); }


  public: ConstIterator end() const {
    return ConstIterator(const_cast<String*>(this)->end());
  }

  public: int size() const { return size_; }
  public: const char16* value() const { return chars_; }

  public: static String Format(const char16* format, ...) {
    char16 buffer[100];
    va_list args;
    va_start(args, format);
    ::wvsprintfW(buffer, format, args);
    va_end(args);
    return String(buffer);
  }
};

class Range {
  private: bool bound_;
  private: Posn end_;
  private: Posn start_;
  private: String text_;

  public: Range() : bound_(false), end_(0), start_(0) {}

  public: bool operator ==(const Range& other) const {
    if (!bound_) {
      return !other.bound_;
    }
    return end_ == other.end_
        && start_ == other.start_
        && text_ == other.text_;
  }

  public: bool operator !=(const Range& other) const {
    return !operator ==(other);
  }

  public: int end() const { return end_; }
  public: int start() const { return start_; }

  public: String text() const {
    return String(text_.begin() + start_, text_.begin() + end_);
  }

  public:void Bind(const String& text, int start, int end) {
    bound_ = true;
    end_ = end;
    start_ = start;
    text_= text;
  }

  public:bool IsEmpty() const { return !bound_; }

  public:void Reset() {
    bound_ = false;
    end_ = start_ = 0;
    text_ = "";
  }
};

class MatchContext : public Regex::IMatchContext {
  private: Vector<Range> captures_;
  private: bool matched_;
  private: IRegex* regex_;
  private: const String source_;

  public: MatchContext(IRegex* regex, int num_captures, const String& source)
      : captures_(num_captures + 1),
        matched_(false),
        regex_(regex),
        source_(source) {}

  public: virtual ~MatchContext() {}

  public: const Vector<Range>& captures() const { return captures_; }
  public: bool matched() const { return matched_; }
  public: void set_matched(bool matched) { matched_ = matched; }

  // [B]
  public: virtual bool BackwardFindCharCi(
      char16 pattern,
      Posn* inout_posn,
      Posn stop) const override {
    auto posn = *inout_posn;
    while (posn >= stop) {
      --posn;
      if (CharEqCi(GetChar(posn), pattern)) {
        *inout_posn = posn + 1;
        return true;
      }
    }
    return false;
  }

  public: virtual bool BackwardFindCharCs(
      char16 pattern,
      Posn* inout_posn,
      Posn stop) const override {
    auto posn = *inout_posn;
    while (posn >= stop) {
      --posn;
      if (CharEqCs(GetChar(posn), pattern)) {
        *inout_posn = posn + 1;
        return true;
      }
    }
    return false;
  }

  // [C]
  private: bool CharEqCi(char16 a, char16 b) const {
    return CharDowncase(a) == CharDowncase(b);
  }

  private: bool CharEqCs(char16 a, char16 b) const {
    return a == b;
  }

  // [F]
  public: virtual bool ForwardFindCharCi(
      char16 pattern,
      Posn* inout_posn,
      Posn stop) const override {
    auto posn = *inout_posn;
    while (posn < stop) {
      if (CharEqCi(GetChar(posn), pattern)) {
        *inout_posn = posn;
        return true;
      }
      ++posn;
    }
    return false;
  }

  public: virtual bool ForwardFindCharCs(
      char16 pattern,
      Posn* inout_posn,
      Posn stop) const override {
    auto posn = *inout_posn;
    while (posn < stop) {
      if (CharEqCs(GetChar(posn), pattern)) {
        *inout_posn = posn;
        return true;
      }
      ++posn;
    }
    return false;
  }

  // [G]
  public: virtual bool GetCapture(
      int index,
      Posn* out_start,
      Posn* out_end) const override {
    if (index >= captures_.size()) {
      return false;
    }
    if (captures_[index].IsEmpty()) {
      return false;
    }
    *out_start = captures_[index].start();
    *out_end = captures_[index].end();
    return true;
  }

  public: virtual char16 GetChar(Posn posn) const override {
    return source_[posn];
  }

  public: virtual Posn GetEnd() const override {
    return source_.size();
  }

  public: virtual void GetInfo(Regex::SourceInfo* info) const override {
    info->m_lStart = GetStart();
    info->m_lEnd = GetEnd();
    info->m_lScanStart = info->m_lStart;
    info->m_lScanEnd = info->m_lEnd;
  }

  public: virtual Posn GetStart() const override { return 0; }

  // [R]
  public: virtual void ResetCapture(int index) override {
    captures_[index].Reset();
  }

  public: virtual void ResetCaptures() override {
    for (auto it = captures_.begin(); it != captures_.end(); ++it) {
      it->Reset();
    }
  }

  // [S]
  public: virtual void SetCapture(int index, Posn start, Posn end) override {
    captures_[index].Bind(source_, start, end);
  }

  public: virtual bool StringEqCi(
      const char16* pattern_start,
      int size,
      Posn posn) const override {
    auto const pattern_end = pattern_start + size;
    for (auto pattern = pattern_start; pattern < pattern_end; ++pattern) {
      if (posn >= GetEnd()) {
        return false;
      }
      if (!CharEqCi(*pattern, GetChar(posn))) {
        return false;
      }
      ++posn;
    }
    return true;
  }

  public: virtual bool StringEqCs(
      const char16* pattern_start,
      int size,
      Posn posn) const override {
    auto const pattern_end = pattern_start + size;
    for (auto pattern = pattern_start; pattern < pattern_end; ++pattern) {
      if (posn >= GetEnd()) {
        return false;
      }
      if (!CharEqCi(*pattern, GetChar(posn))) {
        return false;
      }
      ++posn;
    }
    return true;
  }

  DISALLOW_COPY_AND_ASSIGN(MatchContext);
};

class Pattern {
  private: class Context : public Regex::ICompileContext {
    private: int error_code_;
    private: int error_posn_;
    private: int num_captures_;

    public: Context() : error_code_(0), error_posn_(0), num_captures_(0) {}

    public: virtual void* AllocRegex(size_t size, int num_captures) override {
      num_captures_ = num_captures;
      return new char[size];
    }

    public: int error_code() const { return error_code_; }
    public: int error_posn() const { return error_posn_; }
    public: int num_captures() const { return num_captures_; }

    public: virtual bool SetCapture(int, const char16*) { return true; }

    public: virtual void SetError(int error_posn, int error_code) {
      error_code_ = error_code;
      error_posn_ = error_posn;
    }
  };

  private: int const error_code_;
  private: int const error_posn_;
  private: int const num_captures_;
  private: IRegex* const regex_;

  private: Pattern(IRegex* regex, int num_captures)
      : error_code_(0),
        error_posn_(0),
        num_captures_(num_captures),
        regex_(regex) {}

  private: Pattern(int error_code, int error_posn)
      : error_code_(error_code),
        error_posn_(error_posn),
        num_captures_(0),
        regex_(nullptr) {}

  public: virtual ~Pattern() {}

  public: int error_code() const { return error_code_; }
  public: int error_posn() const { return error_posn_; }

  public: static Pattern& Compile(const String& source, int flags) {
    Context context;
    auto const regex = Regex::Compile(&context, source, source.size(), flags);
    if (regex) {
      return *new Pattern(regex, context.num_captures());
    }

    return *new Pattern(context.error_code(), context.error_posn());
  }

  public: MatchContext& Match(const String& source) {
    auto& context = *new MatchContext(regex_, num_captures_, source);
    context.set_matched(StartMatch(regex_, &context));
    return context;
  }

  DISALLOW_COPY_AND_ASSIGN(Pattern);
};

class Result {
  private: Vector<String> strings_;

  public: Result() : strings_(0) {}

  public: Result(const String& p1) : strings_(1) {
    strings_[0] = p1;
  }

  public: Result(const String& p1, const String& p2) : strings_(2) {
    strings_[0] = p1;
    strings_[1] = p2;
  }

  public: Result(const String& p1, const String& p2, const String& p3)
      : strings_(3) {
    strings_[0] = p1;
    strings_[1] = p2;
    strings_[2] = p3;
  }

  public: Result(const MatchContext& match)
      : strings_(match.captures().size()) {
    auto index = 0;
    for (auto it = match.captures().begin();
         it != match.captures().end();
         ++it) {
      strings_[index] = it->text();
      ++index;
    }
  }

  public: bool operator ==(const Result& other) const {
    return strings_ == other.strings_;
  }

  public: Vector<String>::ConstIterator begin() const {
    return strings_.begin();
  }

  public: Vector<String>::ConstIterator end() const {
    return strings_.end();
  }
};

::std::ostream& operator <<(::std::ostream& os, const String& string) {
  for (auto it = string.begin(); it != string.end(); ++it) {
    auto const ch = *it;
    if (ch == '\\') {
      os << "\\\\";
    } else if (ch >= ' ' && ch < 0x7F) {
      os << char(ch);
    } else {
      char buf[20];
      ::wsprintfA(buf, "\\u%04X", ch);
      os << buf;
    }
  }
  return os;
}

::std::ostream& operator <<(::std::ostream& os, const Result& result) {
  os << "Result(";
  const char* comma = "";
  for (auto it = result.begin(); it != result.end(); ++it) {
    os << comma;
    comma = ", ";
    os << "\"" << *it << "\"";
  }
  os << ")";
  return os;
}

class RegexTest : public ::testing::Test {
  protected: Result Execute(
      const String& pattern_source, 
      const String& source, 
      int flags = 0) {
    scoped_ptr<Pattern> pattern(Pattern::Compile(pattern_source, flags));
    if (pattern->error_code()) {
      return Result(
          String::Format(String("Regex compile failed at %d"),
              pattern->error_posn()));
    }

    scoped_ptr<MatchContext> match(pattern->Match(source));
    return match->matched() ? Result(*match) : Result();
  }
};

TEST_F(RegexTest, Basic) {
  EXPECT_EQ(Result("foo"), Execute("foo", "foobar")); // fixed-001

  EXPECT_EQ(Result("foo"), Execute("foo.*", "foo")); // quant-001
  EXPECT_EQ(Result("foobar"), Execute("foo.*", "foobar")); // quant-002
  EXPECT_EQ(Result("foobarb"), Execute("foo.*b", "foobarbaz")); // quant-003
  EXPECT_EQ(Result("foobarb", "bar"), Execute("foo(.*)b", "foobarbaz")); // capture-001
  EXPECT_EQ(Result("Regex compile failed at 1"), Execute("{foo}", "foo")); // syntax-error-001
  EXPECT_EQ(Result("Regex compile failed at 1"), Execute("*foo*", "foo")); // syntax-error-002
  EXPECT_EQ(Result("Regex compile failed at 1"), Execute("+foo+", "foo")); // syntax-error-003
  EXPECT_EQ(Result("Regex compile failed at 1"), Execute("?foo?", "foo")); // syntax-error-004
  EXPECT_EQ(Result("Regex compile failed at 5"), Execute("foo{}", "foo")); // syntax-error-005
  EXPECT_EQ(Result("Regex compile failed at 5"), Execute("foo{bar}", "foo")); // syntax-error-006
  EXPECT_EQ(Result("Regex compile failed at 5"), Execute("foo**", "foo")); // syntax-error-007
  EXPECT_EQ(Result("Regex compile failed at 0"), Execute("(foo", "foo")); // syntax-error-008
  EXPECT_EQ(Result("Regex compile failed at 4"), Execute("foo)", "foo")); // syntax-error-009
}

TEST_F(RegexTest, Smoke00) {
  EXPECT_EQ(Result(""), Execute("", "")); // smoke/0001
  EXPECT_EQ(Result(""), Execute("", "a")); // smoke/0002
  EXPECT_EQ(Result("a"), Execute("a", "a")); // smoke/0003
  EXPECT_EQ(Result(), Execute("a", "bcd")); // smoke/0004
  EXPECT_EQ(Result("a"), Execute("a", "--a")); // smoke/0006
  EXPECT_EQ(Result(), Execute("a", "")); // smoke/0007
  EXPECT_EQ(Result("ab"), Execute("ab", "ab")); // smoke/0008
  EXPECT_EQ(Result("ab"), Execute("ab", "--ab--")); // smoke/0009
  EXPECT_EQ(Result("ab"), Execute("ab", "--ab")); // smoke/0010
  EXPECT_EQ(Result(), Execute("ab", "")); // smoke/0011
  EXPECT_EQ(Result(), Execute("ab", "bcd")); // smoke/0012
}

TEST_F(RegexTest, CharacterShorthands) {
  EXPECT_EQ(Result("\x07"), Execute("\a", "\x07")); // smoke/0021
  EXPECT_EQ(Result("\x08"), Execute("[\b]", "\x08")); // smoke/0022
  EXPECT_EQ(Result("\x1B"), Execute("\\e", "\x1B")); // smoke/0023
  EXPECT_EQ(Result("\x0C"), Execute("\\f", "\x0C")); // smoke/0024
  EXPECT_EQ(Result("\n"), Execute("\\n", "\n")); // smoke/0025
  EXPECT_EQ(Result("\x0D"), Execute("\\r", "\x0D")); // smoke/0026
  EXPECT_EQ(Result("\x09"), Execute("\\t", "\x09")); // smoke/0027
  EXPECT_EQ(Result("\x0B"), Execute("\\v", "\x0B")); // smoke/0028
  EXPECT_EQ(Result("="), Execute("\\075", "=")); // smoke/0029
  EXPECT_EQ(Result("a"), Execute("\\u0061", "a")); // smoke/0030
  EXPECT_EQ(Result("\x18"), Execute("\\cX", "\x18")); // smoke/0031
}

TEST_F(RegexTest, AnchorsAndZeroWidth) {
  EXPECT_EQ(Result(""), Execute("^", "abc")); // smoke/0040
  EXPECT_EQ(Result("a"), Execute("^a", "abc")); // smoke/0041
  EXPECT_EQ(Result(), Execute("^a", "bc")); // smoke/0042
  EXPECT_EQ(Result("b"), Execute("^b", "a\nbc", Regex::Option_Multiline)); // smoke/0043
  EXPECT_EQ(Result(""), Execute("\\A", "a\nbc", Regex::Option_Multiline)); // smoke/0044
  EXPECT_EQ(Result("a"), Execute("\\Aa", "a\nbc", Regex::Option_Multiline)); // smoke/0045
  EXPECT_EQ(Result(), Execute("\\Ab", "a\nbc",  Regex::Option_Multiline)); // smoke/0045
  EXPECT_EQ(Result(""), Execute("$", "")); // smoke/0046
  EXPECT_EQ(Result("abc"), Execute("abc$", "abc")); // smoke/0047
  EXPECT_EQ(Result(), Execute("abc$", "abcd")); // smoke/0048
  EXPECT_EQ(Result(""), Execute("$", "", Regex::Option_Multiline)); // smoke/0049
  EXPECT_EQ(Result("abc"), Execute("abc$", "abc", Regex::Option_Multiline)); // smoke/0050
  EXPECT_EQ(Result("abc"), Execute("abc$", "abc\n", Regex::Option_Multiline)); // smoke/0051
  EXPECT_EQ(Result(), Execute("abc$", "abcd", Regex::Option_Multiline)); // smoke/0052
  EXPECT_EQ(Result("abc"), Execute("abc$", "abc\n", Regex::Option_Multiline)); // smoke/0053
  EXPECT_EQ(Result("abc"), Execute("abc$", "abc\ndef", Regex::Option_Multiline)); // smoke/0055
  EXPECT_EQ(Result("abc"), Execute("abc\\Z", "abc\n", Regex::Option_Multiline)); // smoke/0056
  EXPECT_EQ(Result(), Execute("abc\\Z", "abcd", Regex::Option_Multiline)); // smoke/0057
  EXPECT_EQ(Result("abc"), Execute("abc\\Z", "abc\n", Regex::Option_Multiline)); // smoke/0058
  EXPECT_EQ(Result(), Execute("abc\\Z", "abc\ndef", Regex::Option_Multiline)); // smoke/0059
  EXPECT_EQ(Result(""), Execute("\\z", "abc")); // smoke/0060
  EXPECT_EQ(Result("abc"), Execute("abc\\z", "abc")); // smoke/0061
  EXPECT_EQ(Result(), Execute("abc\\z", "abcn")); // smoke/0062
  EXPECT_EQ(Result(""), Execute("\\G", "abc")); // smoke/0063
  EXPECT_EQ(Result(""), Execute("\\b", "abc")); // smoke/0064
  EXPECT_EQ(Result("abc"), Execute("\\babc", "Aabc+abc")); // smoke/0065
  EXPECT_EQ(Result(""), Execute("\\B", "abc ")); // smoke/0066
  EXPECT_EQ(Result("abc"), Execute("\\Babc", "Aabc+abc")); // smoke/0067
  EXPECT_EQ(Result("abc"), Execute("(?#comment1)abc(?#comment2)", "abc")); // smoke/0068
}

TEST_F(RegexTest, CharSet) {
  EXPECT_EQ(Result("1"), Execute("\\d", "1")); // smoke/0070
  EXPECT_EQ(Result(), Execute("\\d", "x")); // smoke/0071
  EXPECT_EQ(Result("1"), Execute("[\\d]", "1")); // smoke/0072
  EXPECT_EQ(Result(), Execute("[\\d]", "x")); // smoke/0073
  EXPECT_EQ(Result(), Execute("\\D", "1")); // smoke/0080
  EXPECT_EQ(Result("x"), Execute("\\D", "x")); // smoke/0081
  EXPECT_EQ(Result(), Execute("[\\D]", "1")); // smoke/0082
  EXPECT_EQ(Result("x"), Execute("[\\D]", "x")); // smoke/0083
  EXPECT_EQ(Result("a"), Execute("[abc]", "a")); // smoke/0100
  EXPECT_EQ(Result(), Execute("[abc]", "x")); // smoke/0101
  EXPECT_EQ(Result(), Execute("[^abc]", "a")); // smoke/0102
  EXPECT_EQ(Result("x"), Execute("[^abc]", "x")); // smoke/0103
  EXPECT_EQ(Result("a"), Execute("[a-f]", "a")); // smoke/0104
  EXPECT_EQ(Result(), Execute("[a-f]", "x")); // smoke/0105
  EXPECT_EQ(Result(), Execute("[^a-f]", "a")); // smoke/0106
  EXPECT_EQ(Result("x"), Execute("[^a-f]", "x")); // smoke/0107
  EXPECT_EQ(Result("-"), Execute("[-]", "-")); // smoke/0110
  EXPECT_EQ(Result("a"), Execute("[-a]", "a")); // smoke/0111
  EXPECT_EQ(Result("="), Execute("[--A]", "=")); // smoke/0112
  EXPECT_EQ(Result("-"), Execute("[*-\\-]", "-")); // smoke/0114
  EXPECT_EQ(Result(), Execute("[\\a\\cH\\e\\f\\n\\r\\t\\v]", "-")); // smoke/0115
}

TEST_F(RegexTest, Smoke200) {
  EXPECT_EQ(Result("abccfoo"), Execute("abc*foo", "abccfoo")); // smoke/0200
  EXPECT_EQ(Result("abcfoo"), Execute("abc*foo", "abcfoo")); // smoke/0201
  EXPECT_EQ(Result("abfoo"), Execute("abc*foo", "abfoo")); // smoke/0202
  EXPECT_EQ(Result("abc"), Execute("[abc]+", "abcfoo")); // smoke/0204
  EXPECT_EQ(Result("abccfoo"), Execute("abc+foo", "abccfoo")); // smoke/0210
  EXPECT_EQ(Result("abcfoo"), Execute("abc+foo", "abcfoo")); // smoke/0211
  EXPECT_EQ(Result(), Execute("abc+foo", "abfoo")); // smoke/0212
  EXPECT_EQ(Result(), Execute("abc?foo", "abccfoo")); // smoke/0220
  EXPECT_EQ(Result("abcfoo"), Execute("abc?foo", "abcfoo")); // smoke/0221
  EXPECT_EQ(Result("abfoo"), Execute("abc?foo", "abfoo")); // smoke/0222
  EXPECT_EQ(Result("abccfoo"), Execute("abc{1,3}foo", "abccfoo")); // smoke/0230
  EXPECT_EQ(Result("abcfoo"), Execute("abc{1,3}foo", "abcfoo")); // smoke/0231
  EXPECT_EQ(Result(), Execute("abc{1,3}foo", "abfoox")); // smoke/0232
  EXPECT_EQ(Result("abcccfoo"), Execute("abc{1,3}foo", "abcccfoo")); // smoke/0233
  EXPECT_EQ(Result(), Execute("abc{1,3}foo", "abccccfoo")); // smoke/0234
  EXPECT_EQ(Result("0123456789", ""), Execute("(\\d*)+", "0123456789")); // smoke/0235
  EXPECT_EQ(Result("", ""), Execute("(d*)+", "0123456789")); // smoke/0236
  EXPECT_EQ(Result("aaaaaaa"), Execute("a{2,}", "aaaaaaa")); // smoke/0237
  EXPECT_EQ(Result("abbxyz"), Execute("ab{2,}xyz", "abbxyz")); // smoke/0238
  EXPECT_EQ(Result("abbxyz"), Execute("ab{2}xyz", "abbxyz")); // smoke/0239
  EXPECT_EQ(Result("FoO"), Execute("(?i)foo", "FoO")); // smoke/0240
  EXPECT_EQ(Result("FoObar"), Execute("(?i:foo)bar", "FoObar")); // smoke/0241
  EXPECT_EQ(Result("$100", "100"), Execute("[$](?<foo>\\d+)", "$100")); // smoke/0250
  EXPECT_EQ(Result("123123", "123"), Execute("(?<foo>\\d+)\\k<foo>", "123123")); // smoke/0251
  EXPECT_EQ(Result("bar"), Execute("(?<=foo)bar", "foobar")); // smoke/0260
  EXPECT_EQ(Result(), Execute("(?<=foo)bar", "fxxbar")); // smoke/0261
}

TEST_F(RegexTest, Lookahead) {
  // Matches newline in TEXTAREA tag using lookbehing and lookahead
  // Note: second match is faield. Since, [^\\n<*] is faield at the first
  // newline.
  EXPECT_EQ(Result("\n"), Execute("(?<=<textarea[^>]*>[^\\n<]*)[\\n](?=[^<]*</textarea>)", "<textarea>\nfoo\nbar\n</textarea>")); // smoke/0262
  EXPECT_EQ(Result("bar"), Execute("(?(?<=foo)bar|baz)", "foobar")); // smoke/0270
  EXPECT_EQ(Result(), Execute("(?(?<=foo)bar|baz)", "bar")); // smoke/0271
  EXPECT_EQ(Result("baz"), Execute("(?(?<=foo)bar|baz)", "baz")); // smoke/0272
  EXPECT_EQ(Result("foobar", "foo", ""), Execute("(?:(foo)|(FOO))(?(1)bar|baz)", "foobar")); // smoke/0280
  EXPECT_EQ(Result("FOObaz", "", "FOO"), Execute("(?:(foo)|(FOO))(?(1)bar|baz)", "FOObaz")); // smoke/0281
  EXPECT_EQ(Result("<small>"), Execute("<small.*?>", "<small>")); // smoke/0300
  EXPECT_EQ(Result("<small color='red'>"), Execute("<small.*?>", "<small color='red'>")); // smoke/0301
  EXPECT_EQ(Result("<small color='red'>", "color='red'"), Execute("<small\\s*(.*?)>", "<small color='red'>")); // smoke/0302
  EXPECT_EQ(Result(), Execute("<small.+?>", "<small>")); // smoke/0310
  EXPECT_EQ(Result("<small color='red'>"), Execute("<small.+?>", "<small color='red'>")); // smoke/0311
  EXPECT_EQ(Result("<small color='red'>", "color='red'"), Execute("<small\\s*(.+?)>", "<small color='red'>")); // smoke/0312
}

TEST_F(RegexTest, WordBoundary) {
  EXPECT_EQ(Result(), Execute("\\bfoo\\b", "foobar")); // smoke/0320
  EXPECT_EQ(Result("foo"), Execute("\\bfoo\\b", "foo+bar")); // smoke/0321
  EXPECT_EQ(Result("foo"), Execute("\\bfoo\\b", "this is foo.")); // smoke/0322
  EXPECT_EQ(Result(), Execute("\\bfoo\\b", "this is foobar.")); // smoke/0323
}

TEST_F(RegexTest, Alternative) {
  EXPECT_EQ(Result("foobaz", "foo"), Execute("(foo|bar)baz", "foobaz")); // smoke/0400
  EXPECT_EQ(Result("barbaz", "bar"), Execute("(foo|bar)baz", "barbaz")); // smoke/0401
  EXPECT_EQ(Result("foo", "foo", ""), Execute("(foo)|(bar)baz", "foobaz")); // smoke/0402
  EXPECT_EQ(Result("barbaz", "", "bar"), Execute("(foo)|(bar)baz", "barbaz")); // smoke/0403
}

TEST_F(RegexTest, BackwardSearch) {
  EXPECT_EQ(Result("foo"), Execute("foo", "foo", Regex::Option_Backward)); // smoke/1000
  EXPECT_EQ(Result("foo"), Execute("foo", "foo bar", Regex::Option_Backward)); // smoke/1001
  EXPECT_EQ(Result("foobaz"), Execute("foo\\S+", "foobar foobaz", Regex::Option_Backward)); // smoke/1002
  EXPECT_EQ(Result("foobaz", "baz"), Execute("foo(\\S+)", "foobar foobaz", Regex::Option_Backward)); // smoke/1003
  EXPECT_EQ(Result("<small><b>foo</b></small>"), Execute("<small.*?>", "<small><b>foo</b></small>", Regex::Option_Backward)); // smoke/1004
}

} // RegexTest
