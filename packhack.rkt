#! /usr/bin/env racket

#lang racket

(require sxml)

(require html-parsing)

;;

(define (wrap-up package-repo-title package-names)
  (map (λ (package-name) (list package-repo-title package-name))
       (sort (map string-trim package-names) string-ci<?)))

;;

(define (gauche-packages)
  (let ((document (html->xexp (file->string ".cache/gauche-packages.html"))))
    ((sxpath "//a/text()")
     document)
    ((sxpath "//td[contains(@class, 'inbody')]/text()")
     document)
    (wrap-up "Gauche" ((sxpath "//h3/text()") document))))

;;

(for-each writeln (gauche-packages))
