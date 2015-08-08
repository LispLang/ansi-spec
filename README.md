# ANSI Common Lisp Specification

The ANSI Common Lisp draft specification, parsed from TeX sources, available as
a Common Lisp library.

# Notes

The draft does not differ in any significant (ie non-editorial) way from the
official ANSI Common Lisp specification, so it's perfectly fine to use
it. Thankfully, the TeX sources for the draft are available, so we don't have to
extract the text and markup from Postcript files or PDF files.

The TeX sources were obtained from the [CMU AI Archive][cmu]. Specifically, the
version used was `dpans/dpans3.tgz`. To save space, the `.dvi.Z` files were
removed.

# Implementation

## Parser

The first stage of the parser is preprocess, which does the following:

1. Strip comments (otherwise these would be interpreted as text by the parser).
2. Include files. Some files have an `\input` directive, which is used to
include the contents of others.

## Traversal

After preprocessing, the files are parsed using [plump-tex][plump], and we
recursively go through the Plump nodes.

This part of the process can be thought of as filtering the semantics of the
spec from the noise of TeX. For some nodes (those that declare formatting, or
references, etc.), we emit a corresponding XML to the output file. For some TeX
directives like `\defineSection`, we only produce an opening XML tag, and the
`\endSection\ directive adds the closing tag.

The end result is a file, `spec/output.xml`, which has a simpler XML
representation of the spec.

XML was chosen because:

1. It's easy to write from a context where you don't know the structure around
   the node you're on.
2. "muh s-expressions" isn't an argument.

# Tex Sources

## Structure

The sources in the `tex` folder are essentially divided into two categories:

- `chapter` files: All of these structured the same way. These just import their
  content from the corresponding `concept` files.
- `concept` files: These contain the actual structure of chapters.

We first parse the `chapter` files, extracting chapter numbers, titles, and the
text from the corresponding `concept` file. Since the specification doesn't
really change I just went through the `chapter` files and hardcoded them.

Since I didn't want to waste my time writing a TeX parser I looked at
[this article][tex2xml] and [LaTeXML][latexml]. Eventually, however, I managed
to [trick Shinmera][shin] into writing a TeX parser I could use.

## Setup Files

Included by the text are various files whose names are prefixed with
`setup-`. In TeX, these define the choice of font and what have you. For our
purposes, they are mostly noise.

We are, however, interested in the following:

- `setup-document.tex`: Contains macros for formatting, references, section
style, characters, BNF notation, and some other things.

- `setup-figures.tex`: Short directives that expand to figure names.

- `setup-sections.tex`: Short directives that expand to section names.

- `setup-tables.tex`: Table-defining macros.

- `setup-terms.tex`: Tons of abbreviations and macros for text.

# License

Copyright (c) 2014-2015 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.

[cmu]: ftp://ftp.cs.cmu.edu/usr/ai/lang/lisp/doc/standard/ansi/0.html
[plump]: https://github.com/Shinmera/plump-tex
[tex2xml]: http://jblevins.org/log/xml-tools
[latexml]: http://dlmf.nist.gov/LaTeXML/
[shin]: https://twitter.com/eudxa/status/512783348398186496
