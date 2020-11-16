(import (scheme base) (scheme file) (scheme read) (scheme write))

(cond-expand
  (gauche
   (import (rename (only (gauche base) pprint) (pprint pretty-print))))
  (else
   (define (pretty-print x) (write x) (newline))))

(define first car)

(define (filter f xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs) (reverse acc)
        (loop (cdr xs) (if (f (car xs)) (cons (car xs) acc) acc)))))

(define (for-each/between visit between xs)
  (unless (null? xs)
    (visit (car xs))
    (let loop ((xs (cdr xs)))
      (unless (null? xs) (between) (visit (car xs)) (loop (cdr xs))))))

(define (identity x) x)

(define (string-prefix? fix string)
  (and (<= (string-length fix) (string-length string))
       (string=? fix (substring string 0 (string-length fix)))))

(define (read-all)
  (let loop ((forms '()))
    (let ((form (read)))
      (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

(define known-spdx-license-identifiers
  '("0BSD" "AAL" "ADSL" "AFL-1.1" "AFL-1.2" "AFL-2.0" "AFL-2.1"
    "AFL-3.0" "AGPL-1.0" "AGPL-1.0-only" "AGPL-1.0-or-later"
    "AGPL-3.0" "AGPL-3.0-only" "AGPL-3.0-or-later" "AMDPLPA"
    "AML" "AMPAS" "ANTLR-PD" "APAFML" "APL-1.0" "APSL-1.0"
    "APSL-1.1" "APSL-1.2" "APSL-2.0" "Abstyles" "Adobe-2006"
    "Adobe-Glyph" "Afmparse" "Aladdin" "Apache-1.0" "Apache-1.1"
    "Apache-2.0" "Artistic-1.0" "Artistic-1.0-Perl"
    "Artistic-1.0-cl8" "Artistic-2.0" "BSD-1-Clause"
    "BSD-2-Clause" "BSD-2-Clause-FreeBSD" "BSD-2-Clause-NetBSD"
    "BSD-2-Clause-Patent" "BSD-2-Clause-Views" "BSD-3-Clause"
    "BSD-3-Clause-Attribution" "BSD-3-Clause-Clear"
    "BSD-3-Clause-LBNL" "BSD-3-Clause-No-Nuclear-License"
    "BSD-3-Clause-No-Nuclear-License-2014"
    "BSD-3-Clause-No-Nuclear-Warranty" "BSD-3-Clause-Open-MPI"
    "BSD-4-Clause" "BSD-4-Clause-UC" "BSD-Protection"
    "BSD-Source-Code" "BSL-1.0" "Bahyph" "Barr" "Beerware"
    "BitTorrent-1.0" "BitTorrent-1.1" "BlueOak-1.0.0" "Borceux"
    "CAL-1.0" "CAL-1.0-Combined-Work-Exception" "CATOSL-1.1"
    "CC-BY-1.0" "CC-BY-2.0" "CC-BY-2.5" "CC-BY-3.0"
    "CC-BY-3.0-AT" "CC-BY-3.0-US" "CC-BY-4.0" "CC-BY-NC-1.0"
    "CC-BY-NC-2.0" "CC-BY-NC-2.5" "CC-BY-NC-3.0" "CC-BY-NC-4.0"
    "CC-BY-NC-ND-1.0" "CC-BY-NC-ND-2.0" "CC-BY-NC-ND-2.5"
    "CC-BY-NC-ND-3.0" "CC-BY-NC-ND-3.0-IGO" "CC-BY-NC-ND-4.0"
    "CC-BY-NC-SA-1.0" "CC-BY-NC-SA-2.0" "CC-BY-NC-SA-2.5"
    "CC-BY-NC-SA-3.0" "CC-BY-NC-SA-4.0" "CC-BY-ND-1.0"
    "CC-BY-ND-2.0" "CC-BY-ND-2.5" "CC-BY-ND-3.0" "CC-BY-ND-4.0"
    "CC-BY-SA-1.0" "CC-BY-SA-2.0" "CC-BY-SA-2.0-UK"
    "CC-BY-SA-2.5" "CC-BY-SA-3.0" "CC-BY-SA-3.0-AT"
    "CC-BY-SA-4.0" "CC-PDDC" "CC0-1.0" "CDDL-1.0" "CDDL-1.1"
    "CDLA-Permissive-1.0" "CDLA-Sharing-1.0" "CECILL-1.0"
    "CECILL-1.1" "CECILL-2.0" "CECILL-2.1" "CECILL-B" "CECILL-C"
    "CERN-OHL-1.1" "CERN-OHL-1.2" "CERN-OHL-P-2.0"
    "CERN-OHL-S-2.0" "CERN-OHL-W-2.0" "CNRI-Jython"
    "CNRI-Python" "CNRI-Python-GPL-Compatible" "CPAL-1.0"
    "CPL-1.0" "CPOL-1.02" "CUA-OPL-1.0" "Caldera" "ClArtistic"
    "Condor-1.1" "Crossword" "CrystalStacker" "Cube" "D-FSL-1.0"
    "DOC" "DSDP" "Dotseqn" "ECL-1.0" "ECL-2.0" "EFL-1.0"
    "EFL-2.0" "EPICS" "EPL-1.0" "EPL-2.0" "EUDatagrid"
    "EUPL-1.0" "EUPL-1.1" "EUPL-1.2" "Entessa" "ErlPL-1.1"
    "Eurosym" "FSFAP" "FSFUL" "FSFULLR" "FTL" "Fair"
    "Frameworx-1.0" "FreeImage" "GFDL-1.1"
    "GFDL-1.1-invariants-only" "GFDL-1.1-invariants-or-later"
    "GFDL-1.1-no-invariants-only"
    "GFDL-1.1-no-invariants-or-later" "GFDL-1.1-only"
    "GFDL-1.1-or-later" "GFDL-1.2" "GFDL-1.2-invariants-only"
    "GFDL-1.2-invariants-or-later" "GFDL-1.2-no-invariants-only"
    "GFDL-1.2-no-invariants-or-later" "GFDL-1.2-only"
    "GFDL-1.2-or-later" "GFDL-1.3" "GFDL-1.3-invariants-only"
    "GFDL-1.3-invariants-or-later" "GFDL-1.3-no-invariants-only"
    "GFDL-1.3-no-invariants-or-later" "GFDL-1.3-only"
    "GFDL-1.3-or-later" "GL2PS" "GLWTPL" "GPL-1.0" "GPL-1.0+"
    "GPL-1.0-only" "GPL-1.0-or-later" "GPL-2.0" "GPL-2.0+"
    "GPL-2.0-only" "GPL-2.0-or-later"
    "GPL-2.0-with-GCC-exception"
    "GPL-2.0-with-autoconf-exception"
    "GPL-2.0-with-bison-exception"
    "GPL-2.0-with-classpath-exception"
    "GPL-2.0-with-font-exception" "GPL-3.0" "GPL-3.0+"
    "GPL-3.0-only" "GPL-3.0-or-later"
    "GPL-3.0-with-GCC-exception"
    "GPL-3.0-with-autoconf-exception" "Giftware" "Glide"
    "Glulxe" "HPND" "HPND-sell-variant" "HaskellReport"
    "Hippocratic-2.1" "IBM-pibs" "ICU" "IJG" "IPA" "IPL-1.0"
    "ISC" "ImageMagick" "Imlib2" "Info-ZIP" "Intel" "Intel-ACPI"
    "Interbase-1.0" "JPNIC" "JSON" "JasPer-2.0" "LAL-1.2"
    "LAL-1.3" "LGPL-2.0" "LGPL-2.0+" "LGPL-2.0-only"
    "LGPL-2.0-or-later" "LGPL-2.1" "LGPL-2.1+" "LGPL-2.1-only"
    "LGPL-2.1-or-later" "LGPL-3.0" "LGPL-3.0+" "LGPL-3.0-only"
    "LGPL-3.0-or-later" "LGPLLR" "LPL-1.0" "LPL-1.02" "LPPL-1.0"
    "LPPL-1.1" "LPPL-1.2" "LPPL-1.3a" "LPPL-1.3c" "Latex2e"
    "Leptonica" "LiLiQ-P-1.1" "LiLiQ-R-1.1" "LiLiQ-Rplus-1.1"
    "Libpng" "Linux-OpenIB" "MIT" "MIT-0" "MIT-CMU"
    "MIT-advertising" "MIT-enna" "MIT-feh" "MITNFA" "MPL-1.0"
    "MPL-1.1" "MPL-2.0" "MPL-2.0-no-copyleft-exception" "MS-PL"
    "MS-RL" "MTLL" "MakeIndex" "MirOS" "Motosoto" "MulanPSL-1.0"
    "MulanPSL-2.0" "Multics" "Mup" "NASA-1.3" "NBPL-1.0"
    "NCGL-UK-2.0" "NCSA" "NGPL" "NIST-PD" "NIST-PD-fallback"
    "NLOD-1.0" "NLPL" "NOSL" "NPL-1.0" "NPL-1.1" "NPOSL-3.0"
    "NRL" "NTP" "NTP-0" "Naumen" "Net-SNMP" "NetCDF" "Newsletr"
    "Nokia" "Noweb" "Nunit" "O-UDA-1.0" "OCCT-PL" "OCLC-2.0"
    "ODC-By-1.0" "ODbL-1.0" "OFL-1.0" "OFL-1.0-RFN"
    "OFL-1.0-no-RFN" "OFL-1.1" "OFL-1.1-RFN" "OFL-1.1-no-RFN"
    "OGC-1.0" "OGL-Canada-2.0" "OGL-UK-1.0" "OGL-UK-2.0"
    "OGL-UK-3.0" "OGTSL" "OLDAP-1.1" "OLDAP-1.2" "OLDAP-1.3"
    "OLDAP-1.4" "OLDAP-2.0" "OLDAP-2.0.1" "OLDAP-2.1"
    "OLDAP-2.2" "OLDAP-2.2.1" "OLDAP-2.2.2" "OLDAP-2.3"
    "OLDAP-2.4" "OLDAP-2.5" "OLDAP-2.6" "OLDAP-2.7" "OLDAP-2.8"
    "OML" "OPL-1.0" "OSET-PL-2.1" "OSL-1.0" "OSL-1.1" "OSL-2.0"
    "OSL-2.1" "OSL-3.0" "OpenSSL" "PDDL-1.0" "PHP-3.0"
    "PHP-3.01" "PSF-2.0" "Parity-6.0.0" "Parity-7.0.0" "Plexus"
    "PolyForm-Noncommercial-1.0.0"
    "PolyForm-Small-Business-1.0.0" "PostgreSQL" "Python-2.0"
    "QPL-1.0" "Qhull" "RHeCos-1.1" "RPL-1.1" "RPL-1.5"
    "RPSL-1.0" "RSA-MD" "RSCPL" "Rdisc" "Ruby" "SAX-PD" "SCEA"
    "SGI-B-1.0" "SGI-B-1.1" "SGI-B-2.0" "SHL-0.5" "SHL-0.51"
    "SISSL" "SISSL-1.2" "SMLNJ" "SMPPL" "SNIA" "SPL-1.0"
    "SSH-OpenSSH" "SSH-short" "SSPL-1.0" "SWL" "Saxpath"
    "Sendmail" "Sendmail-8.23" "SimPL-2.0" "Sleepycat"
    "Spencer-86" "Spencer-94" "Spencer-99" "StandardML-NJ"
    "SugarCRM-1.1.3" "TAPR-OHL-1.0" "TCL" "TCP-wrappers" "TMate"
    "TORQUE-1.1" "TOSL" "TU-Berlin-1.0" "TU-Berlin-2.0"
    "UCL-1.0" "UPL-1.0" "Unicode-DFS-2015" "Unicode-DFS-2016"
    "Unicode-TOU" "Unlicense" "VOSTROM" "VSL-1.0" "Vim" "W3C"
    "W3C-19980720" "W3C-20150513" "WTFPL" "Watcom-1.0" "Wsuipa"
    "X11" "XFree86-1.1" "XSkat" "Xerox" "Xnet" "YPL-1.0"
    "YPL-1.1" "ZPL-1.1" "ZPL-2.0" "ZPL-2.1" "Zed" "Zend-2.0"
    "Zimbra-1.3" "Zimbra-1.4" "Zlib" "blessing" "bzip2-1.0.5"
    "bzip2-1.0.6" "copyleft-next-0.3.0" "copyleft-next-0.3.1"
    "curl" "diffmark" "dvipdfm" "eCos-2.0" "eGenix" "etalab-2.0"
    "gSOAP-1.3b" "gnuplot" "iMatix" "libpng-2.0"
    "libselinux-1.0" "libtiff" "mpich2" "psfrag" "psutils"
    "wxWindows" "xinetd" "xpp" "zlib-acknowledgement"))

(define (sloppy-stringify x)
  (cond ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((pair? x)   (cons (sloppy-stringify (car x))
                           (sloppy-stringify (cdr x))))
        (else        x)))

(define (sloppy-assoc-cdr property obj)
  (cond ((not (pair? obj)) '())
        ((and (pair? (car obj)) (equal? property (caar obj))) (cdar obj))
        (else (sloppy-assoc-cdr property (cdr obj)))))

(define (convert-all valid? convert all)
  (map (lambda (one) (if (valid? one) (convert one) (error "Not valid:" one)))
       all))

(define (replicate-all obj property valid? convert)
  (let ((all (sloppy-assoc-cdr property obj)))
    (if (null? all) '()
        (list (cons property (convert-all valid? convert all))))))

(define (replicate-one obj property valid? convert)
  (let ((all (sloppy-assoc-cdr property obj)))
    (cond ((null? all)
           '())
          ((= 1 (length all))
           (list (cons property (convert-all valid? convert all))))
          (else
           (error "Expected only one value:" property)))))

(define (dependency? x)
  (or (symbol? x)
      (and (list? x)
           (= 2 (length x))
           (symbol? (list-ref x 0))
           (let ((version (list-ref x 1)))
             (or (string? version)
                 (symbol? version)
                 (real? version))))))

(define (valid-license? x) (or (string? x) (symbol? x)))
(define (valid-package-name? x) (or (string? x) (list? x)))

(define (convert-license license)
  (let ((license (sloppy-stringify license)))
    (if (member license known-spdx-license-identifiers)
        license
        (string-append "LicenseRef-" license))))

(define (convert-akku-package pkg)
  (append (replicate-one pkg 'name valid-package-name? identity)
          (let ((pkg (first (sloppy-assoc-cdr 'versions pkg))))
            (append
             (replicate-one pkg 'synopsis string? identity)
             (replicate-all pkg 'author string? identity)
             (replicate-one pkg 'license valid-license? convert-license)
             (replicate-all pkg 'category symbol? sloppy-stringify)
             (replicate-all pkg 'dependencies dependency? sloppy-stringify)
             (replicate-all pkg 'test-dependencies dependency? sloppy-stringify)))))

(define (chicken-egg-name->srfi-number egg-name)
  (cond ((string-prefix? "srfi-" egg-name)
         (string->number (substring egg-name
                                    (string-length "srfi-")
                                    (string-length egg-name))))
        ((string=? egg-name "vector-lib") 43)
        ((string=? egg-name "box") 111)
        (else #f)))

(define (convert-c4-dot-meta)
  #f)

(define (convert-c5-dot-egg egg)
  (let ((egg-name (car egg)))
    (append
     `((name ,egg-name))
     (replicate-one egg 'synopsis string? identity)
     (replicate-all egg 'author string? identity)
     (replicate-one egg 'license valid-license? convert-license)
     (replicate-all egg 'category symbol? sloppy-stringify)
     (replicate-all egg 'dependencies dependency? sloppy-stringify)
     (replicate-all egg 'test-dependencies dependency? sloppy-stringify)
     (let ((srfi (chicken-egg-name->srfi-number egg-name)))
       (if (not srfi) '() `((provides-srfi ,srfi)))))))

(define akku-index
  (with-input-from-file "convert-sources/akku-index.scm"
    (lambda ()
      (read-line)  ; Skip #!r6rs line.
      (filter (lambda (x) (and (pair? x) (eq? 'package (car x))))
              (read-all)))))

(define eggs-5-latest
  (with-input-from-file "convert-sources/eggs-5-latest.scm" read-all))

(define all-packages
  (append (map convert-akku-package akku-index)
          (map convert-c5-dot-egg eggs-5-latest)))

(for-each/between pretty-print newline all-packages)
