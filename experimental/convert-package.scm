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

(define spdx-license-identifiers
  (with-input-from-file "convert-sources/spdx-license-identifiers.scm"
    read-all))

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
    (if (member license spdx-license-identifiers) license
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

(define (convert-chicken-egg egg)
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

(define eggs-4-latest
  (with-input-from-file "convert-sources/eggs-4-latest.scm" read-all))

(define eggs-5-latest
  (with-input-from-file "convert-sources/eggs-5-latest.scm" read-all))

(define all-packages
  (append (map convert-akku-package akku-index)
          (map convert-chicken-egg eggs-4-latest)
          (map convert-chicken-egg eggs-5-latest)))

(for-each/between pretty-print newline all-packages)
