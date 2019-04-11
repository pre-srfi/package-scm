#! /usr/bin/env racket

#lang racket

(require sxml)

(require css-expr)
(require html-parsing)
(require txexpr)

;;

(define (not-null? x)
  (not (null? x)))

(define (packages-for package-repo-title package-list)
  (map (λ (pkg) (append pkg (list package-repo-title)))
       package-list))

(define (sort-package-table package-table)
  (sort (hash->list package-table) string-ci<? #:key first))

(define (tabulate-packages-by-name package-list)
  (sort-package-table
   (foldl (λ (pkg table)
            (hash-update table
                         (first pkg)
                         (λ (packages) (append packages (list (rest pkg))))
                         '()))
          (hash)
          package-list)))

;;

(define (chicken-eggref first-thing . more-things)
  (match (string-split first-thing "/")
    ((list "" "wiki.call-cc.org" "eggref" chicken-major-version package-name)
     (let ((url (string-append "https:" first-thing)))
       (list* package-name url more-things)))
    (else '())))

(define (chicken-grovel-td td)
  (let ((a-text ((sxpath "//a/text()") td))
        (a-href ((sxpath "//a/@href/text()") td))
        (text   ((sxpath "./text()") td)))
    (map first (filter not-null? (list a-href a-text text)))))

(define (chicken-tr-texts tr)
  (map first (filter not-null? (map chicken-grovel-td tr))))

(define (chicken-grovel-tr tr)
  (match (chicken-tr-texts tr)
    ((list-rest name desc _)
     (chicken-eggref name desc))))

(define (chicken-egg-index-n package-repo-title html-filename)
  (let ((document (html->xexp (file->string html-filename))))
    (packages-for package-repo-title
                  (filter not-null?
                          (map chicken-grovel-tr
                               (rest ((sxpath "//table/tr") document)))))))

(define (chicken-egg-index-4)
  (chicken-egg-index-n "Chicken 4" ".cache/egg-index-4.html"))

(define (chicken-egg-index-5)
  (chicken-egg-index-n "Chicken 5" ".cache/egg-index-5.html"))

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

(define (packages-list-from-all-repos)
  (append (chicken-egg-index-4)
          (chicken-egg-index-5)
          (gauche-packages)))

(define (package-list->html package-list)
  (xexpr->html
   `(html
     (head
      (title "Scheme packages")
      (style
          ,(css-expr->css
            (css-expr
             [html #:font-family sans-serif]
             [table #:border-collapse collapse]
             [table td th
                    #:border (1px solid black)
                    #:padding 5px])))
      (body
       (h1 "Scheme packages")
       (table
        (tr
         (th "Package name")
         (th "Description"))
        ,@(append-map
           (λ (pkg)
             (match-let (((list-rest pkg-name pkg-implementations) pkg))
               (map (λ (pkg-impl)
                      `(tr (td ,pkg-name)
                           (td ,(second pkg-impl))))
                    pkg-implementations)))
           (tabulate-packages-by-name package-list))))))))

(define (string->file string file)
  (call-with-atomic-output-file
   file (λ (out . _) (write-string string out))))

(string->file (package-list->html (packages-list-from-all-repos))
              "packhack.html")
