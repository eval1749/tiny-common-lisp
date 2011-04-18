<?xml version="1.0"?><doc>
<members>
<member name="T:Regex.IEnvironment" decl="false" source="d:\proj\evedit2\regex\iregex.h" line="93">
<remark>
 Interface provides chracter tests and overridable implementation.
 <para>
    Implementation of methods use Windows API for character case
    related tests.
 </para>
</remark>
</member>
<member name="T:Regex.ICompileContext" decl="false" source="d:\proj\evedit2\regex\iregex.h" line="125">
<remark>
 Interface of regex compilation context.
</remark>
</member>
<member name="T:Regex.IMatchContext" decl="false" source="d:\proj\evedit2\regex\iregex.h" line="135">
<remark>
 Interface of regex match context.
</remark>
</member>
<member name="T:Common.EnumChar" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="318">
<remark>
  C-String enumerator.
</remark>
</member>
<member name="T:Common.EnumChar.Arg" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="323">
<summary>
  Argument for EnumChar
</summary>
</member>
<member name="M:Common.EnumChar.#ctor(Common.EnumChar.Arg)" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="341">
<summary>
  Construct C-String enumerator.
</summary>
</member>
<member name="M:Common.EnumChar.AtEnd" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="348">
<summary>
 Check enumereator at end.
</summary>
</member>
<member name="M:Common.EnumChar.Get" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="356">
<summary>
 Returns current character
</summary>
</member>
<member name="M:Common.EnumChar.Next" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="365">
<summary>
 Advance current position
</summary>
</member>
<member name="T:Regex.RegexPrivate.LengthInfo" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="25">
<remark>
  Regex length information
</remark>
</member>
<member name="M:Regex.RegexPrivate.LengthInfo.IsFixed" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="34">
<summary>
  Returns fixed match information.
</summary>
<returns>Value of m_fFixed</returns>
</member>
<member name="M:Regex.RegexPrivate.LengthInfo.#ctor(System.Boolean,System.Int32,System.Int32)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="40">
<summary>
 Constructs LengthInfo
</summary>
<param name="fFixed">
  True if regex matches fixed length of source
</param>
<param name="iMax">
 Maximum number of characters to be matched.
</param>
<param name="iMin">
 Minimum number of characters to be matched.
</param>
</member>
<member name="T:Regex.RegexPrivate.MinMax" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="58">
<remark>
  For specifiying min-max parameter.
</remark>
</member>
<member name="T:Regex.RegexPrivate.Node" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="73">
<remark>
  Base class of parse tree node
</remark>
</member>
<member name="T:Regex.RegexPrivate.WithCase" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="131">
WithCase
</member>
<member name="T:Regex.RegexPrivate.WithDirection" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="148">
WithDirection
</member>
<member name="T:Regex.RegexPrivate.NodeCaptureBase" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="194">
<remark>
  Base class for capture reference.
</remark>
</member>
<member name="T:Regex.RegexPrivate.NodeAnd" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="352">
<remark>
  Parse tree node "And".
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeAnd.#ctor" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="360">
<remark>
  Constructs empty And node.
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeAnd.#ctor(Regex.RegexPrivate.Node*)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="365">
<remark>
  Constructs And node with one sub node.
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeAnd.#ctor(Regex.RegexPrivate.Node*,Regex.RegexPrivate.Node*)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="371">
<remark>
  Constructs And node with two sub nodes.
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeAnd.#ctor(Regex.RegexPrivate.Node*,Regex.RegexPrivate.Node*,Regex.RegexPrivate.Node*)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="380">
<remark>
  Constructs And node with three sub nodes.
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeAnd.ComputeMinLength" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="391">
<summary>
  Sum of value of all subnodes.
</summary>
</member>
<member name="T:Regex.RegexPrivate.NodeAny" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="412">
<remark>
  Parse tree node for dot(.)
</remark>
</member>
<member name="T:Regex.RegexPrivate.NodeAtom" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="436">
<remark>
  Parse tree node for atomic group <c>(?&gt;...)</c>
</remark>
</member>
<member name="T:Regex.RegexPrivate.NodeCapture" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="453">
<remark>
  Parse tree node capture. Regex syntax is <c>(...)</c>.
</remark>
</member>
<member name="F:Regex.RegexPrivate.NodeCapture.m_iNth" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="462">
<summary>
 An index number of this capture.
</summary>
</member>
<member name="M:Regex.RegexPrivate.NodeCapture.#ctor(Regex.RegexPrivate.Node.Direction,Regex.RegexPrivate.Node*,System.Int32)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="467">
<summary>
  Construct NodeCapture object.
</summary>
<param name="eDir">A direction of capturing</param>
<param name="iNth">Capture index number</param>
<param name="pNode">A node will be captured</param>
</member>
<member name="M:Regex.RegexPrivate.NodeCapture.GetOp" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="485">
<summary>
  Opcode of this node.
</summary>
<returns>
  Op_Capture_B for backward capturing, Op_Capture_F for
  forward capturing.
</returns>
</member>
<member name="M:Regex.RegexPrivate.NodeCapture.NeedStack" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="496">
<summary>
  Alwasy true for NodeCapture.
</summary>
</member>
<member name="T:Regex.RegexPrivate.NodeChar" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="563">
<remark>
  Parse tree node for character comparison.
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeChar.#ctor(Regex.RegexPrivate.Node.Direction,System.Char,Regex.RegexPrivate.Node.Case,System.Boolean)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="573">
<summary>
 Construct NodeChar.
</summary>
</member>
<member name="T:Regex.RegexPrivate.NodeString" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="1068">
<remark>
  Parse tree node for string comparison.
</remark>
</member>
<member name="M:Regex.RegexPrivate.NodeString.#ctor(Regex.RegexPrivate.Node.Direction,System.Char!System.Runtime.CompilerServices.IsConst*,System.Int32,Regex.RegexPrivate.Node.Case,System.Boolean)" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="1079">
<summary>
 Construct NodeString.
</summary>
</member>
<member name="T:Regex.RegexPrivate.NodeVoid" decl="false" source="d:\proj\evedit2\regex\regex_node.h" line="1140">
<summary>
 Parse tree node for void. This is used for empty capture.
</summary>
</member>
<member name="T:Regex.RegexPrivate.Scanner" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="20">
<remark>
  Scanner base class
</remark>
</member>
<member name="T:Regex.RegexPrivate.CharScanner" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="58">
<remark>
 Character scanner
</remark>
</member>
<member name="M:Regex.RegexPrivate.CharScanner.#ctor" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="69">
<summary>
  For CharScanner_ template
</summary>
</member>
<member name="M:Regex.RegexPrivate.CharScanner.computeMethod(System.Boolean,System.Boolean)" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="75">
<summary>
 Compute scanner method code.
</summary>
<param name="fBackwrad">Ture if backward scanner</param>
<param name="fIgnoreCase">Ture if case-insensitive scanner</param>
</member>
<member name="T:Regex.RegexPrivate.StringScannerCompiler" decl="false" source="d:\proj\evedit2\regex\regex_compile.cpp" line="356">
<remark>
  String scanner compiler
</remark>
</member>
</members>
</doc>