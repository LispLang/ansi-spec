# ANSI Common Lisp Specification

[![Build Status](https://travis-ci.org/LispLang/ansi-spec.svg?branch=master)](https://travis-ci.org/LispLang/ansi-spec)

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
[this article][tex2xml] and [LaTeXML][latexml].

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

# Implementation

## Preprocessing

The first stage of the process is preprocessing, implemented in
`preprocess.lisp`. This includes:

1. Stripping TeX comments, that is, lines that start with `%`. Otherwise these
   would be interpreted as text by the parser.

2. Include files. Some files have an `\input` directive, which is used to
   include the contents of other files.

3. Strip some unwanted character sequences that comment stripping doesn't delete
   for some reason.

4. Turn the ampersand character into a directive.

5. Simplify some complex directives, like chapter definitions.

## Explicit body transformation

TeX has a lot of constructs like this:

```tex
{\tag some more text}
```

Because it's easier to deal with, we want this:

```tex
\tag{some more text}
```

So the transformation in `explicit-body.lisp` does just that.

## Traversal

After preprocessing, the files are parsed using [plump-tex][plump], and we go through the document nodes.

### Modes

The parser is based on *modes*. A mode is triggered by a specific tag
(e.g. `\displaytwo`, `\it`) and has a certain arity, which is the number of
siblings it consumes. For instance, the `\displaytwo` macro is used like this:

```tex
\displaytwo{Title of the table}{
contents & of \cr
the & table \cr
}
```

So, it's corresponding mode will be triggered by the string `"displaytwo"`, and
will have an arity of 2, since it needs to use two bodies: the one with the
title and the one with the table contents.

Each mode has a list of callbacks for each argument, with as many callbacks as
the mode's arity. When a mode is triggered, the first *n* siblings of the node
that triggered the mode (where *n* = the arity of the mode) are added to an `eq`
hash table, which associates each child with the callback from the mode. Then,
when traversal reaches a node that exists in the table, the callback is called.

### Output

The parts of the spec we want are written to an output file,
`spec/output.xml`. XML was chosen because:

1. It's easy to write from a context where you don't know the structure around
   the node you're on.
2. "muh s-expressions" isn't an argument.

### Stripping

Text nodes that are written to the output file must first go through a filter,
where backslash characters and italic corrections (`\/`, an ungoogleable TeX
leftover from the eighties) are removed.

### Parsing Abbreviations

The specification makes liberal use of the TeX directive to define macros. Since
transcribing those macros to some kind of Lisp for for automatic compilation
would be quite boring, we use part of the traversal machinery to take define
macros found in the text and take care of macroexpansion.

# XML Output

This section will document the format of the XML output file.

# License

Copyright (c) 2014-2019 Fernando Borretti (fernando@borretti.me)

Licensed under the MIT License.

[cmu]: ftp://ftp.cs.cmu.edu/usr/ai/lang/lisp/doc/standard/ansi/0.html
[plump]: https://github.com/Shinmera/plump-tex
[tex2xml]: http://jblevins.org/log/xml-tools
[latexml]: http://dlmf.nist.gov/LaTeXML/
