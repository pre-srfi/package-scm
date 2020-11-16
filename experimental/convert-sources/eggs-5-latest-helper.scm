#! /usr/bin/env gosh

(import (scheme base) (scheme file) (scheme read) (scheme write))
(import (gauche base) (file util))

(define (egg-file->package egg-file)
  (let ((egg-name (path-sans-extension (sys-basename egg-file))))
    (cons egg-name (with-input-from-file egg-file read))))

(for-each (lambda (package) (write package) (newline) (newline))
          (glob-fold "eggs-5-latest/*/*/*.egg"
                     (lambda (egg-file packages)
                       (let ((package (egg-file->package egg-file)))
                         (append packages (list package))))
                     '()))
