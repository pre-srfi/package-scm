#! /usr/bin/env gosh

(import (scheme base) (scheme file) (scheme read) (scheme write))
(import (gauche base) (file util))

(define (command-args) (cdr (command-line)))

(define (egg-file->package egg-file)
  (let ((egg-name (path-sans-extension (sys-basename egg-file))))
    (cons egg-name (with-input-from-file egg-file read))))

(unless (= 1 (length (command-args))) (error "Usage"))
(let* ((dir (car (command-args)))
       (ext (cond ((string=? dir "eggs-4-latest") "meta")
                  ((string=? dir "eggs-5-latest") "egg")
                  (else (error "Huh?")))))
  (for-each (lambda (package) (write package) (newline) (newline))
            (glob-fold (string-append dir "/*/*/*." ext)
                       (lambda (egg-file packages)
                         (let ((package (egg-file->package egg-file)))
                           (append packages (list package))))
                       '())))
