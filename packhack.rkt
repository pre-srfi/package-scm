#! /usr/bin/env racket

#lang racket

(require sxml)

(require css-expr)
(require html-parsing)
(require txexpr)

;;

(define (not-null? x)
  (not (null? x)))

(define (first= k)
  (λ (x) (and (pair? x) (equal? k (first x)))))

(define (read-all)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (write->string x)
  (with-output-to-string (curry write x)))

(define (lines->string lines)
  (string-join lines "\n"))

;;

(define (package-for package-repo-title package)
  (append package (list package-repo-title)))

(define (packages-for package-repo-title package-list)
  (map (curry package-for package-repo-title) package-list))

(define (package-name-for-sorting package-name)
  (string-trim package-name #rx"[()]"))

(define (sort-package-table package-table)
  (sort (hash->list package-table)
        string-ci<? #:key (compose package-name-for-sorting first)))

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

(define (akku-package-name package-form)
  (let ((pkg-name (cadr (or (assoc 'name (rest package-form))
                            (error "No name")))))
    (if (string? pkg-name) pkg-name (write->string pkg-name))))

(define (akku-package-first-version-form package-form)
  (first (rest (assoc 'versions (rest package-form)))))

(define (akku-package-first-synopsis package-form)
  (cadr (or (assoc 'synopsis (akku-package-first-version-form package-form))
            '(synopsis ""))))

(define (akku-package-first-description package-form)
  (cadr (or (assoc 'description
                   (akku-package-first-version-form package-form))
            '(description ""))))

(define (akku-package-from-snow-fort? package-form)
  (string-prefix?
   (akku-package-first-description package-form)
   "This Snow package is federated from http://snow-fort.org/"))

(define (akku-and-snow-fort-packages)
  (append-map (λ (package-form)
                (let* ((pkg-name (akku-package-name package-form))
                       (pkg-desc (akku-package-first-synopsis package-form))
                       (package  (list pkg-name "" pkg-desc)))
                  (cons (package-for "Akku" package)
                        (if (akku-package-from-snow-fort? package-form)
                            (list (package-for "Snow-Fort" package))
                            (list)))))
              (filter (first= 'package)
                      (let ((lines (file->lines ".cache/akku-index.scm")))
                        (with-input-from-string (lines->string (drop lines 1))
                          read-all)))))

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

(define (gauche-iterate-relevant-tags)
  (let ((document (html->xexp (file->string ".cache/gauche-packages.html"))))
    (append-map
     (λ (tag)
       (case (first tag)
         ((h3)
          (list (list 'h3 (string-trim (first ((sxpath "text()") tag))))))
         ((table)
          (map (compose (curry list 'td) string-trim)
               ((sxpath "//td/text()") tag)))
         (else '())))
     ((sxpath "//body/*") document))))

(define (gauche-append-package all cur)
  (if (not cur)
      all
      (append all (list (list (first cur) "" (third cur))))))

(define (gauche-packages)
  (packages-for
   "Gauche"
   (let loop ((tags (gauche-iterate-relevant-tags)) (cur #f) (all '()))
     (match tags
       ((list)
        (gauche-append-package all cur))
       ((list-rest (list 'h3 text) tags)
        (loop tags (list text) (gauche-append-package all cur)))
       ((list-rest (list 'td text) tags)
        (loop tags (append cur (list text)) all))))))

;;

(define (packages-list-from-all-repos)
  (append (akku-and-snow-fort-packages)
          (chicken-egg-index-4)
          (chicken-egg-index-5)
          (gauche-packages)))

(define (package-impl-tds package-impl)
  `((td ,(second package-impl))
    (td ,(third package-impl))))

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
         (th "Description")
         (th "Repository"))
        ,@(append-map
           (λ (pkg)
             (match-let (((list-rest pkg-name first-impl more-impls) pkg))
               (let ((n (number->string (+ 1 (length more-impls)))))
                 (cons `(tr (td ((rowspan ,n)) ,pkg-name)
                            ,@(package-impl-tds first-impl))
                       (map (compose (curry cons 'tr) package-impl-tds)
                            more-impls)))))
           (tabulate-packages-by-name package-list))))))))

(define (string->file string file)
  (call-with-atomic-output-file
   file (λ (out . _) (write-string string out))))

(string->file (package-list->html (packages-list-from-all-repos))
              "packhack.html")
