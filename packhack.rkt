#! /usr/bin/env racket

#lang errortrace racket

(require json)                          ; read-json
(require net/http-client)               ; http-sendrecv
(require net/uri-codec)                 ; alist->form-urlencoded
(require net/url)                       ; call/input-url
(require srfi/1)                        ; list-index
(require srfi/26)                       ; cut, cute

(require css-expr)
(require html-parsing)
(require sxml)
(require txexpr)

;;

(define cache-dir (build-path (current-directory) ".cache"))

(define max-description-length 100)

;;

(define (not-null? x)
  (not (null? x)))

(define (first-equal? k x)
  (and (pair? x) (equal? k (first x))))

(define (read-all)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (write->string x)
  (with-output-to-string (curry write x)))

(define (lines->string lines)
  (string-join lines "\n"))

(define (string->file string file)
  (call-with-atomic-output-file
   file (λ (out . _) (write-string string out))))

(define (condense-whitespace s)
  (string-trim (string-replace s #px"\\s+" " ")))

(define (cap-string-length max-length s)
  (condense-whitespace (substring s 0 (min max-length (string-length s)))))

(define (alist->hash alist)
  (foldl (λ (pair h) (hash-set h (car pair) (cdr pair)))
         (hash) alist))

(define (hash-map-key-val h keyfun valfun)
  (alist->hash (hash-map h (λ (key val) (cons (keyfun key) (valfun val))))))

(define (call-with-url-input url proc)
  (call/input-url
   (string->url url)
   (lambda (url) (get-pure-port url '() #:redirections 1))
   proc))

(define call-with-http-sendrecv-port
  (make-keyword-procedure
   (lambda (kw-syms kw-args proc . normal-args)
     (define-values (status headers port)
       (keyword-apply http-sendrecv kw-syms kw-args normal-args))
     (unless (bytes=? status #"HTTP/1.1 200 OK")
       (error "HTTP request did not return 200 OK"))
     (with-input-from-bytes (port->bytes port)
       proc))))

(define (read-json-predicate predicate port)
  (let ((json-data (read-json port)))
    (cond ((eof-object? json-data)
           (error "No data"))
          ((not (predicate json-data))
           (error "Bad data" json-data))
          (else json-data))))

(define read-json-list (curry read-json-predicate list?))

(define read-json-hash (curry read-json-predicate hash-eq?))

;;

(define (fetch-into-cache cache-filename call-with-in-port)
  (call-with-atomic-output-file
   cache-filename
   (λ (out _) (write-bytes (call-with-in-port port->bytes)
                           out))))

(define (cache-get-filename cache-basename get-input-port)
  (let ((cache-filename (build-path cache-dir cache-basename)))
    (make-directory* cache-dir)
    (unless (file-exists? cache-filename)
      (fprintf (current-error-port) "Fetching <~a>...~%" cache-basename)
      (fetch-into-cache cache-filename get-input-port))
    cache-filename))

(define (cache-get-proc proc cache-basename get-input-port)
  (call-with-input-file (cache-get-filename cache-basename get-input-port)
    proc))

(define (cache-get-proc-from-url proc cache-basename url)
  (cache-get-proc proc cache-basename (curry call-with-url-input url)))

(define (cache-get-html-xexp-from-url cache-basename url)
  (cache-get-proc-from-url (compose html->xexp port->string)
                           cache-basename url))
;;

(define (package-for package-repo-title package)
  (append package (list package-repo-title)))

(define (packages-for package-repo-title package-list)
  (map (curry package-for package-repo-title) package-list))

(define (package-name-for-sorting package-name)
  (string-trim package-name #rx"[()]"))

(define (packages-by-name->hash package-list)
  (foldl (λ (pkg table)
           (hash-update table
                        (first pkg)
                        (λ (packages) (append packages (list (rest pkg))))
                        '()))
         (hash)
         package-list))

(define (packages-by-name->alist package-list)
  (sort (hash->list (packages-by-name->hash package-list))
        string-ci<? #:key (compose package-name-for-sorting first)))

;;

(define (akku-cache-index-forms)
  (cache-get-proc-from-url
   (λ (port)
     (let ((lines (port->lines port)))
       (with-input-from-string (lines->string (drop lines 1))
         read-all)))
   "akku-index.scm"
   "https://archive.akkuscm.org/archive/Akku-index.scm"))

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

(define (akku-package-url pkg-name)
  (string-append "https://akkuscm.org/packages/"
                 (uri-path-segment-encode pkg-name) "/"))

(define (akku-package-from-snow-fort? package-form)
  (string-prefix?
   (akku-package-first-description package-form)
   "This Snow package is federated from http://snow-fort.org/"))

(define (akku-and-snow-fort-packages)
  (append-map
   (λ (package-form)
     (let* ((pkg-name (akku-package-name package-form))
            (pkg-desc (akku-package-first-synopsis package-form))
            (akku-pkg (package-for "Akku"
                                   (list pkg-name
                                         (akku-package-url pkg-name)
                                         pkg-desc)))
            (snow-pkg (if (akku-package-from-snow-fort? package-form)
                          (package-for "Snow-Fort"
                                       (list pkg-name "" pkg-desc))
                          #f)))
       (cons akku-pkg (if snow-pkg (list snow-pkg) '()))))
   (filter (curry first-equal? 'package)
           (akku-cache-index-forms))))

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

(define (chicken-egg-index-n package-repo-title cache-basename url)
  (let ((document (cache-get-html-xexp-from-url cache-basename url)))
    (packages-for package-repo-title
                  (filter not-null?
                          (map chicken-grovel-tr
                               (rest ((sxpath "//table/tr") document)))))))

(define (chicken-egg-index-4)
  (chicken-egg-index-n
   "Chicken 4" "egg-index-4.html" "https://eggs.call-cc.org/4/"))

(define (chicken-egg-index-5)
  (chicken-egg-index-n
   "Chicken 5" "egg-index-5.html" "https://eggs.call-cc.org/5/"))

;;

(define gauche-packages-url
  "https://practical-scheme.net/wiliki/wiliki.cgi/Gauche:Packages")

(define (gauche-packages-xexp)
  (cache-get-html-xexp-from-url "gauche-packages.html" gauche-packages-url))

(define (gauche-iterate-relevant-tags)
  (let ((document (gauche-packages-xexp)))
    (append-map
     (λ (tag)
       (case (first tag)
         ((h3)
          (list (list 'h3
                      (string-trim (first ((sxpath "text()") tag)))
                      (car ((sxpath "@id/text()") tag)))))
         ((table)
          (map (compose (curry list 'td) string-trim)
               ((sxpath "//td/text()") tag)))
         (else '())))
     ((sxpath "//body/*") document))))

(define (gauche-append-package all cur)
  (if (not cur)
      all
      (match-let (((list-rest pkg-name heading-id _ description _) cur))
        (let ((url (string-append gauche-packages-url "#"
                                  (uri-path-segment-encode heading-id))))
          (append all (list (list pkg-name url description)))))))

(define (gauche-packages)
  (packages-for
   "Gauche"
   (let loop ((tags (gauche-iterate-relevant-tags)) (cur #f) (all '()))
     (match tags
       ((list)
        (gauche-append-package all cur))
       ((list-rest (list 'h3 text heading-id) tags)
        (loop tags (list text heading-id) (gauche-append-package all cur)))
       ((list-rest (list 'td text) tags)
        (loop tags (append cur (list text)) all))))))

;;

(define (guile-packages-xexp)
  (cache-get-html-xexp-from-url
   "guile-packages.html"
   "https://www.gnu.org/software/guile/libraries/"))

(define (guile-package-descriptions document)
  (filter
   (λ (s) (and (>= (string-length s) 20)
               (not (memf (curry string-contains? s)
                          '("This website is powered by GNU Guile and")))))
   (map (λ (div)
          (condense-whitespace
           (string-join ((sxpath "./descendant-or-self::*/text()")
                         (first ((sxpath "./p") div))))))
        ((sxpath "//body//div") document))))

(define (guile-packages)
  (let* ((document (guile-packages-xexp)))
    (packages-for
     "Guile"
     (map (λ (package description)
            (append package (list description)))
          ;; Another hack. A sxpath bug puts the first section last, so
          ;; we sort it ourselves.
          (sort (map (λ (section)
                       (list (car ((sxpath "./h3/a/text()") section))
                             (car ((sxpath "./h3/a/@href/text()") section))))
                     ((sxpath "//section[contains(@class, 'lib')]") document))
                string-ci<? #:key first)
          (guile-package-descriptions document)))))

;;

(define (raven-cache-readme-lines)
  (cache-get-proc-from-url
   port->lines
   "ravensc-readme.md"
   "https://raw.githubusercontent.com/guenchi/Raven/master/README.md"))

(define (raven-cache-packages-json)
  (cache-get-proc
   read-json-list
   "ravensc-packages.json"
   (λ (proc)
     (call-with-http-sendrecv-port
      proc
      "ravensc.com" "/list"
      #:ssl? #f
      #:method "POST"
      #:headers (list "Accept: application/json")))))

(define (raven-cache-package-info-json pkg-name)
  (cache-get-proc
   read-json-hash
   (let ((safe-pkg-name (regexp-replace* #rx"[^A-Za-z0-9-]" pkg-name "_")))
     (string-append "ravensc-info-" safe-pkg-name ".json"))
   (λ (proc)
     (call-with-http-sendrecv-port
      proc
      "ravensc.com" "/info"
      #:ssl? #f
      #:method "POST"
      #:headers '("Content-Type: application/x-www-form-urlencoded")
      #:data (alist->form-urlencoded `((name . ,pkg-name)))))))

(define (make-pkg-entry pkg-name [pkg-desc #f])

  (define (dflt-desc pkg-desc [sanatize identity] [dflt-text "(no description)"])
    (if (and pkg-desc (positive? (string-length (string-trim pkg-desc))))
        (sanatize pkg-desc)
        dflt-text))

  (define (capitalize-first pkg-desc)
    (if (and pkg-desc (not (zero? (string-length pkg-desc))))
        (begin (string-set! pkg-desc 0 (char-upcase (string-ref pkg-desc 0)))
               pkg-desc)
        pkg-desc))

  (define (strip-trailing-period pkg-desc)
    (string-trim pkg-desc #px"[\\.\\s]+" #:left? #f))

  `(,pkg-name "" ,(dflt-desc pkg-desc (compose capitalize-first strip-trailing-period))))

(define (raven-packages)

  (define (try-json-pkg-info pkg-name dflt-pkg-info)
    (let* ((json-obj (raven-cache-package-info-json pkg-name))
           (dscrp (hash-ref json-obj 'dscrp)))
      ;; Not all JSON responses will have the description set, so
      ;; default.
      (if (eq? dscrp (json-null))
          dflt-pkg-info
          (make-pkg-entry pkg-name dscrp))))

  (define (gh-pkg-info pkg-name)

    ;; "package" must be the first column name.
    (define header-cols '((gh-package       . "package")
                          (gh-chez-only?    . "only for Chez")
                          (gh-description   . "description")
                          (gh-r6rs?         . "r6rs common")
                          (gh-pure-scheme?  . "pure Scheme")
                          (gh-c-dependency? . "C lib depenced")))

    (define (split-table-cols line)
      (map string-trim (string-split line "|")))

    (define (split-pkg-info line header-idxs)

      (define (safe-list-ref cols idx)
        ;; Empty trailing columns won't be filled up in the markdown
        ;; table, so fetch columns safely:
        (if (< idx (length cols)) (list-ref cols idx) ""))

      (let ((cols (split-table-cols line)))
        (if (null? (cdr cols))
            #f
            (map (cut safe-list-ref cols <>) header-idxs))))

    (define (split-header-cols line)

      (define (col-idx cols col-name)
        (list-index (cut string=? <> col-name) cols))

      (define (col-idxs cols col-names)
        (let ((idxs (map (cut col-idx cols <>) col-names)))
          (if (every identity idxs) idxs #f)))

      (let ((cols (split-table-cols line)))
        (if (null? (cdr cols)) #f (col-idxs cols (map cdr header-cols)))))

    (let* ((raven-gh-lines (raven-cache-readme-lines))
           (props (let loop ((lines (filter (compose positive? string-length)
                                            raven-gh-lines))
                             (header-idxs #f))
                    (cond
                      ((null? lines) #f)
                      ((split-header-cols (car lines))
                       => (lambda (col-idxs)
                            (loop (cdr lines) col-idxs)))
                      ((and header-idxs
                            (split-pkg-info (car lines) header-idxs))
                       => (lambda (pkg-values)
                            (if (and pkg-values
                                     (string=? (car pkg-values) pkg-name))
                                (map cons (map car header-cols) pkg-values)
                                (loop (cdr lines) header-idxs))))
                      (else
                       (loop (cdr lines) header-idxs))))))
      (if props
          (make-pkg-entry pkg-name (cdr (assoc 'gh-description props)))
          ;; Not all entries in the JSON package list will have an
          ;; entry in the README, so allow a minimal default:
          (make-pkg-entry pkg-name))))

  (define (all-pkg-info pkg-obj)
    (let ((pkg-name (hash-ref pkg-obj 'name)))
      (try-json-pkg-info pkg-name (gh-pkg-info pkg-name))))

  (packages-for "Raven" (map all-pkg-info (raven-cache-packages-json))))

;;

(define (slib-packages)
  ;; Note: We won't use any caching helpers here, we just assume that
  ;; the whole repo has been cloned to local folder "~/.cache/slib".

  (define local-repo-path-name ".cache/slib")

  (define (pathname->text-lines pathname)
    (string-split (file->string pathname) "\n"))

  (define (slib-file-ref pkg-name)
    (format "`~a'" pkg-name))

  (define (all-readme-infos)

    (define (maybe-continuation-line line)
      ;; Some package descriptions in README span multiple lines. This procedure helps to join them.
      (and (positive? (string-length line))
           (char=? (string-ref line 0) #\tab)
           (substring line 1)))

    (define (maybe-pkg-line line)
      ;; Expected format of a package description's first line in README:
      ;; "`array.scm' has multi-dimensional arrays."
      (regexp-match #px"`((\\S+)\\.scm)'\\s+(.*)" line))

    (define (cleanup-desc line)
      (cond
       ((regexp-match #px"(has|is)(\\s+a)?\\s+(.*)" line)
        => (match-lambda ([list _ _ _ desc] desc)))
       (else line)))

    (sort
     (let loop ((lines (pathname->text-lines ".cache/slib/README"))
                (state 'start)
                (pkg-name-curr #f)
                (pkg-desc-curr "")
                (pkg-entries '()))
       (if (null? lines)
           pkg-entries
           (let* ((full-line (car lines))
                  (line (string-trim full-line)))
             (cond
              ((and (eq? state 'start) (string-prefix? line (slib-file-ref "slib.sh")))
               ;; start collecting package descriptions after this line.
               (loop (cdr lines) 'in-pkg-list pkg-name-curr pkg-desc-curr pkg-entries))
              ((and (eq? state 'in-pkg-list) (string=? line "INSTALLATION INSTRUCTIONS"))
               ;; stop collecting package descriptions after this line and return all collected entries.
               (cons (make-pkg-entry pkg-name-curr pkg-desc-curr) pkg-entries))
              ((and (eq? state 'in-pkg-list) (maybe-continuation-line full-line))
               => (lambda (pkg-desc-cont)
                    (loop (cdr lines) state
                          pkg-name-curr
                          (string-append pkg-desc-curr " " pkg-desc-cont)
                          pkg-entries)))
              ((and (eq? state 'in-pkg-list) (maybe-pkg-line line))
               => (match-lambda [(list _ pkg-basename pkg-name pkg-readme-desc)
                                 (loop (cdr lines) state
                                       pkg-name
                                       (cleanup-desc pkg-readme-desc)
                                       (if pkg-name-curr
                                           (cons (make-pkg-entry pkg-name-curr pkg-desc-curr) pkg-entries)
                                           pkg-entries))]))
              (else
               (loop (cdr lines) state pkg-name-curr pkg-desc-curr pkg-entries))))))
     string<? #:key car))

  (define (all-files.scm-infos)
    ;; Some figures on collecting package descriptions from source files and README:
    ;; - total number of .scm files in ~/.cache/slib: 154 (where all files are in the root folder)
    ;; - number of packages returned by `all-files.scm-infos': 144 (with number of packages named srfi*.scm: 7)
    ;; - number of packages returned by `all-files.scm-infos' without resorting to `all-readme-infos': 136

    (define (make-try-get-readme-info)
      (define readme-infos #f)
      (lambda (pkg-name)
        (unless readme-infos
          (set! readme-infos (all-readme-infos)))
        (find (lambda (info) (string=? (car info) pkg-name)) readme-infos)))

    (define try-get-readme-info (make-try-get-readme-info))

    (define (try-get-file-info scm.path)

      (define (strip-trailing-emacs-hint pkg-desc)
        ;; Someone having a better regexp-brain than me will be able
        ;; to squeeze that into the regexp for matching the
        ;; package description; I won't bother doing so...
        (cond
         ((regexp-match #px"(.*)-\\*-[sS]cheme-\\*-" pkg-desc)
          => (match-lambda ([list _ stripped-pkg-desc] stripped-pkg-desc)))
         (else pkg-desc)))

      (let* ((scm.file-name (file-name-from-path scm.path))
             (pkg-name (path->string (path-replace-extension scm.file-name #"")))
             (pkg-basename (path->string scm.file-name)))
        (cond
         ((regexp-match #px";+\\s*\"(\\S+)\"[\\.,:]?\\s+(.*)" (car (file->lines scm.path)))
          => (match-lambda [(list _ l-basename l-desc)
                            (if (string=? l-basename pkg-basename)
                                (make-pkg-entry pkg-name (strip-trailing-emacs-hint l-desc))
                                (try-get-readme-info pkg-name))]))
         ((try-get-readme-info pkg-name) => identity)
         (else #f))))

    (filter identity
            (map try-get-file-info
                 (find-files            ; this will already return an alphabetically sorted list
                  (lambda (scm.path) (bytes=? (or (path-get-extension scm.path) #".NEVER") #".scm"))
                  local-repo-path-name))))

  (packages-for "slib" (all-files.scm-infos)))

(define (packages-list-from-all-repos)
  (append (akku-and-snow-fort-packages)
          (chicken-egg-index-4)
          (chicken-egg-index-5)
          (gauche-packages)
          (guile-packages)
          (raven-packages)
          (slib-packages)))

(define (package-impl-tds pkg)
  (match-define (list url description repository) pkg)
  `((td ,(cap-string-length max-description-length description))
    (td ,(if (non-empty-string? url)
             `(a ((href ,url)) ,repository)
             repository))))

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
           (packages-by-name->alist package-list))))))))

(define (package-list->json package-list)
  (jsexpr->string
   (hash-map-key-val
    (packages-by-name->hash package-list)
    string->symbol
    (λ (pkgs-with-this-name)
      (foldl (λ (pkg repo->info)
               (match-define (list url description repository) pkg)
               (hash-set repo->info
                         (string->symbol repository)
                         (hash 'description description
                               'url (or url ""))))
             (hash) pkgs-with-this-name)))))

(define (main)
  (let ((package-list (packages-list-from-all-repos)))
    (string->file (package-list->html package-list) "packhack.html")
    (string->file (package-list->json package-list) "packhack.json")))

(main)
