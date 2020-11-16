# SRFI nnn: Scheme metadata files

by Firstname Lastname, Another Person, Third Person

# Status

Early Draft

# Abstract

??? abstract, preferably shorter than 200 words. Please outline the
need for, and design of, the proposal.

# Issues

??? Optional section that may point out things to be resolved. This
will not appear in the final SRFI.

# Rationale

??? detailed rationale. This should be 200-500 words long. Please
explain why the proposal should be incorporated as a standard feature
in Scheme implementations. List related standards and SRFIs, including
dependencies, conflicts, and replacements. If there are other
standards which this proposal will replace or with which it will
compete, please explain why the present proposal is a substantial
improvement.

## Survey of prior art

GitHub's version of Markdown can make tables. For example:

| System        | Procedure | Signature                 |
| ------------- |:---------:| ------------------------- |
| System A      | `jumble`  | _list_ _elem_             |
| System B      | `bungle`  | _elem_ _list_             |
| System C      | `frob`    | _list_ _elem_ _predicate_ |

# Specification

## Package metadata

```
(package

  (version <string>)
  (author <name> <email>)       ; allow multiple
  (maintainer <name> <email>)   ; allow multiple
  (license <license-expression>)
  (homepage <uri>)
  (manual <uri>)                ; can be a relative filename in snow-fort
  (category <category>)         ; allow multiple
  (synopsis <string>)
  (long-description <string>)

  (provides-srfi <nonnegative-integer>)  ; allow multiple

  (depends ...)
  (test-depends ...)
  (test-script <filename>))
```

## License expression sublanguage

License information is given in a [domain-specific language for SPDX license expressions](https://gitlab.com/weinholt/spdx).

 - (**or** *expr0* *expr1*) — A choice between license expressions.
 - (**and** *expr0* *expr1*) — A requirement to comply with both
   license expressions.
 - (**with** *license* *exception*) — A license identifier with a
   license exception identifier (both strings).
 - (**user-defined** *document-ref* *license-ref*) — This is a
   user-defined license reference. The document is optional and `#f`
   if omitted, otherwise both are strings.
 - (**+** *license-identifier*) — Represents "(or later)". Example:
   `(+ "GPL-3.0")` represents "GNU GPL version 3 (or later)".
 - *license-identifier* — A string identifying a license. Example:
   `"GPL-3.0"`.

Allowed characters in strings are A-Z, a-z, 0-9 and hyphen (-). No
check is done to verify that license identifiers and license exception
identifiers are valid.


# Implementation

??? explanation of how it meets the sample implementation requirement
(see process), and the code, if possible, or a link to it Source for
the sample implementation.

# Acknowledgements

??? Give credits where credits is due.

# References

??? Optional section with links to web pages, books and papers that
helped design the SRFI.

# Copyright

Copyright (C) Firstname Lastname (20XY).

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
