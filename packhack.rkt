#! /usr/bin/env racket

#lang racket

(require sxml)

(require html-parsing)

;;

(define (sort-packages package-list)
  (sort package-list string-ci<? #:key first))

(define (packages-for package-repo-title package-list)
  (map (λ (pkg) (append pkg (list package-repo-title)))
       package-list))


;;

(define (gauche-packages)
  (let ((document (html->xexp (file->string ".cache/gauche-packages.html"))))
    ;; ((sxpath "//a/text()") document)
    ;; ((sxpath "//td[contains(@class, 'inbody')]/text()") document)
    (packages-for "Gauche"
                  (map (compose (λ (package-name) (list package-name "" ""))
                                string-trim)
                       ((sxpath "//h3/text()") document)))))

;;

(for-each (lambda (pkg)
            (newline)
            (for-each writeln pkg))
          (sort-packages (gauche-packages)))
