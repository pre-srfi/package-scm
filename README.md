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

### Akku (R6RS, R7RS)

There are no metadata files inside the packages themselves.

Each package has a form in the global `akku-index.scm` file. The
following fields exist.

    (name "spdx")
    (versions
      ((version "1.0.0")
       (synopsis "SPDX license expressions and such")
       (authors "Author Name <author@example.com>")
       (license "MIT")
       (lock
         (location
          (url "https://archive.akkuscm.org/archive/pkg/s/spdx_1.0.0_repack.tar.xz"))
         (content
          (sha256
           "5e621cb4447c9fb5136002aa01f5600de84f44cd321786262af69de4c6379c66")))
       (depends
         ("chez-srfi" "~0.0.0-akku")
         ("packrat" "~0.1.0-akku"))
       (depends/dev)
       (conflicts)))

### Snow-Fort (R7RS)

    (version "0.7")
    (library
      (name (chibi sqlite3))
      (path "chibi/sqlite3.sld")
      (depends
        (chibi)
        (scheme base)
        (scheme write)
        (srfi 130)))

### ScmPkg

Each package has an `etc/meta` file. The following fields exist.

    (description "GTK+ 2.x binding for STklos -- GTklos level")
    (version "1.0.0")
    (author "Author Name <author@example.com>")
    (maintainer "Author Name <author@example.com>")
    (stklos-doc (html))
    (license gpl-2.1)

### Gauche modules

Each package has a `package.scm` file. The following fields exist.

    (define-gauche-package "Gauche-tar"
      :version "1.1.2"
      :description "Multi-line. The first line is a summary."
      :require (("Gauche" (>= "0.9.5")) ("Gauche-gl" "0.6"))
      :authors ("Author Name <author@example.com>")
      :maintainers ("Author Name <author@example.com>")
      :licenses ("GPL")
      :homepage "http://example.com/Gauche-tar/"
      :repository "http://example.com/Gauche-tar.git")

### Dorodango (R6RS) and Guildhall (Guile)

[Dorodango](https://gitlab.com/rotty/dorodango) is an R6RS package
manager by Andreas Rottmann, and Guildhall is a derivative for Guile.

### Guix (Guile)

Guix [package
recipes](https://guix.gnu.org/manual/en/html_node/Defining-Packages.html).
The following fields exist.

    (name "hello")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-silent-rules")))
    (inputs `(("gawk" ,gawk)))
    (synopsis "Hello, GNU world: An example GNU package")
    (description "Guess what GNU Hello prints!")
    (home-page "https://www.gnu.org/software/hello/")
    (license gpl3+)

### Eggs (Chicken)

Each Chicken 4 egg `foo` has a `foo.meta` file. The following fields
exist.

    (synopsis "SRFI 193: Command line")
    (category os)
    (license "ISC")
    (author "Author Name")
    (test-depends test)

Each Chicken 5 egg `foo` has a `foo.egg` file. The following fields
exist.

    (synopsis "SRFI 193: Command line")
    (category os)
    (version "0.1.3")
    (license "ISC")
    (author "Author Name")
    (test-dependencies test)
    (components (extension srfi-193))

The Chicken package index uses a `foo.release-info` file for each egg
`foo`:

    (repo git "git://github.com/lassik/chicken-{egg-name}.git")
    (uri targz "https://github.com/lassik/chicken-{egg-name}/tarball/{egg-release}")
    (release "0.1")
    (release "0.1.1")
    (release "0.1.2")
    (release "0.1.3")

### Winds (Cyclone)

Each package has an `package.scm` file. The following fields exist.

    (name postgresql)
    (version 0.1)
    (license "BSD")
    (authors "Author Name")
    (maintainers "Author Name")
    (description "PostgreSQL socket frontend interface library written in pure R7RS Scheme.")
    (tags "database" "sql" "networking")
    (docs "https://github.com/cyclone-scheme/cyclone-winds/wiki/postgresql")
    (test "test.scm")
    (dependencies (bytevector md5))
    (test-dependencies ())
    (foreign-dependencies ())
    (library
     (name (cyclone postgresql))
     (description "Wrap library"))

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
