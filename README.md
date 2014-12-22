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

# Internals

The parser has two stages: Preprocessing and transformation. In the
preprocessing stage, regular expressions are used to:

1. Make some expressions simpler. For example, `beginSection` and `endSection`
   pairs are turned into a single `section{...}` expression, which makes it
   easier to handle.
2. Expand abbreviations and other macros found in the `setup-*.tex` files.

After preprocessing, the files are parsed using [plump-tex][plump], and we
recursively go through the Plump nodes transforming them into S-expressions.

For example, this:

```tex
Deprecated language features are not expected to appear in future \clisp\
standards, but are required to be implemented for conformance with this
standard; \seesection\ReqLangFeatures.

\term{Conforming programs} can use deprecated features;
however, it is considered good programming style to avoid them.
It is permissible for the compiler to produce \term{style warnings} 
about the use of such features at compile time, 
but there should be no such warnings at program execution time.
```

Becomes this:

```lisp
(:section (:title "Deprecated" :ref "DeprecatedFeatures")
   ("Deprecated language features are not expected to appear in future"
    (:roman "Common Lisp") "\\
standards, but are required to be implemented for conformance with this
standard; See"
    (:clref (:type :secref) "ReqLangFeatures") "."
    (:term "Conforming programs") "can use deprecated features;
however, it is considered good programming style to avoid them.
It is permissible for the compiler to produce"
    (:term "style warnings") "about the use of such features at compile time, 
but there should be no such warnings at program execution time."
```

## Implementation

Now, for the details. The sources in the `tex` folder are essentially divided
into two categories:

- `chapter` files: All of these structured the same way. These just import their
  content from the corresponding `concept` files.
- `concept` files: These contain the actual structure of chapters.

We first parse the `chapter` files, extracting chapter numbers, titles, and the
text from the corresponding `concept` file. Since the specification doesn't
really change I just went through the `chapter` files and hardcoded them.

Then there interesting part: Parsing the `concept` files, which contain the
actual content. Since I didn't want to waste my time writing a TeX parser I
looked at [this article][tex2xml] and [LaTeXML][latexml]. Eventually, however, I
managed to [trick Shinmera][shin] into writing a TeX parser I could use.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.

[cmu]: ftp://ftp.cs.cmu.edu/usr/ai/lang/lisp/doc/standard/ansi/0.html
[plump]: https://github.com/Shinmera/plump-tex
[tex2xml]: http://jblevins.org/log/xml-tools
[latexml]: http://dlmf.nist.gov/LaTeXML/
[shin]: https://twitter.com/eudxa/status/512783348398186496
