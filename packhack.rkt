#! /usr/bin/env racket

#lang racket

(require sxml)

(require html-parsing)

;;

(define (gauche-packages)
  (let ((document (html->xexp (file->string ".cache/gauche-packages.html"))))
    ((sxpath "//a/text()")
     document)
    ((sxpath "//td[contains(@class, 'inbody')]/text()")
     document)
    (map (λ (package-name)
           (list "Gauche" package-name))
         (sort (map string-trim ((sxpath "//h3/text()") document))
               string-ci<?))))

;;


(for-each writeln (gauche-packages))
