<!DOCTYPE html>
<!-- saved from url=(0084)https://www.gnu.org/software/emacs/manual/html_node/elisp/General-Escape-Syntax.html -->
<html><!-- Created by GNU Texinfo 7.0.3, https://www.gnu.org/software/texinfo/ --><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">

<title>General Escape Syntax (GNU Emacs Lisp Reference Manual)</title>

<meta name="description" content="General Escape Syntax (GNU Emacs Lisp Reference Manual)">
<meta name="keywords" content="General Escape Syntax (GNU Emacs Lisp Reference Manual)">
<meta name="resource-type" content="document">
<meta name="distribution" content="global">
<meta name="Generator" content="makeinfo">
<meta name="viewport" content="width=device-width,initial-scale=1">

<link rev="made" href="mailto:bug-gnu-emacs@gnu.org">
<link rel="icon" type="image/png" href="https://www.gnu.org/graphics/gnu-head-mini.png">
<meta name="ICBM" content="42.256233,-71.006581">
<meta name="DC.title" content="gnu.org">
<style type="text/css">
@import url('/software/emacs/manual.css');
</style>
</head>

<body lang="en">
<div class="subsubsection-level-extent" id="General-Escape-Syntax">
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Ctl_002dChar-Syntax.html" accesskey="n" rel="next">Control-Character Syntax</a>, Previous: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html" accesskey="p" rel="prev">Basic Char Syntax</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html" accesskey="u" rel="up">Character Type</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>
<hr>
<h4 class="subsubsection" id="General-Escape-Syntax-1">2.4.3.2 General Escape Syntax</h4>

<p>In addition to the specific escape sequences for special important
control characters, Emacs provides several types of escape syntax that
you can use to specify non-<abbr class="acronym">ASCII</abbr> text characters.
</p>
<ol class="enumerate">
<li> <a class="index-entry-id" id="index-_005c-in-character-constant"></a>
<a class="index-entry-id" id="index-backslash-in-character-constants"></a>
<a class="index-entry-id" id="index-unicode-character-escape"></a>
You can specify characters by their Unicode names, if any.
<code class="code">?\N{<var class="var">NAME</var>}</code> represents the Unicode character named
<var class="var">NAME</var>.  Thus, ‘<samp class="samp">?\N{LATIN SMALL LETTER A WITH GRAVE}</samp>’ is
equivalent to <code class="code">?à</code> and denotes the Unicode character U+00E0.  To
simplify entering multi-line strings, you can replace spaces in the
names by non-empty sequences of whitespace (e.g., newlines).

</li><li> You can specify characters by their Unicode values.
<code class="code">?\N{U+<var class="var">X</var>}</code> represents a character with Unicode code point
<var class="var">X</var>, where <var class="var">X</var> is a hexadecimal number.  Also,
<code class="code">?\u<var class="var">xxxx</var></code> and <code class="code">?\U<var class="var">xxxxxxxx</var></code> represent code
points <var class="var">xxxx</var> and <var class="var">xxxxxxxx</var>, respectively, where each <var class="var">x</var>
is a single hexadecimal digit.  For example, <code class="code">?\N{U+E0}</code>,
<code class="code">?\u00e0</code> and <code class="code">?\U000000E0</code> are all equivalent to
<code class="code">?à</code> and to ‘<samp class="samp">?\N{LATIN SMALL LETTER A WITH GRAVE}</samp>’.  The
Unicode Standard defines code points only up to ‘<samp class="samp">U+<var class="var">10ffff</var></samp>’,
so if you specify a code point higher than that, Emacs signals an
error.

</li><li> You can specify characters by their hexadecimal character
codes.  A hexadecimal escape sequence consists of a backslash,
‘<samp class="samp">x</samp>’, and the hexadecimal character code.  Thus, ‘<samp class="samp">?\x41</samp>’ is
the character <kbd class="kbd">A</kbd>, ‘<samp class="samp">?\x1</samp>’ is the character <kbd class="kbd">C-a</kbd>, and
<code class="code">?\xe0</code> is the character <kbd class="kbd">à</kbd> (<kbd class="kbd">a</kbd> with grave accent).
You can use one or more hex digits after ‘<samp class="samp">x</samp>’, so you can
represent any character code in this way.

</li><li> <a class="index-entry-id" id="index-octal-character-code"></a>
You can specify characters by their character code in
octal.  An octal escape sequence consists of a backslash followed by
up to three octal digits; thus, ‘<samp class="samp">?\101</samp>’ for the character
<kbd class="kbd">A</kbd>, ‘<samp class="samp">?\001</samp>’ for the character <kbd class="kbd">C-a</kbd>, and <code class="code">?\002</code>
for the character <kbd class="kbd">C-b</kbd>.  Only characters up to octal code 777 can
be specified this way.

</li></ol>

<p>These escape sequences may also be used in strings.  See <a class="xref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html">Non-<abbr class="acronym">ASCII</abbr> Characters in Strings</a>.
</p>
</div>
<hr>
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Ctl_002dChar-Syntax.html">Control-Character Syntax</a>, Previous: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html">Basic Char Syntax</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html">Character Type</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>





</body></html>