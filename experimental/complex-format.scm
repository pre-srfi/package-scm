(import (scheme base) (scheme file) (scheme read) (scheme write)
        (scheme process-context))

;;; Settings

(define trace? #t)

(define initial-features '(chicken))

(define metadata-features (make-parameter initial-features))

;;; Utilities

(define (append-map f xs) (apply append (map f xs)))

(define (writeln x) (write x) (newline))

(define (read-all)
  (let loop ((forms '()))
    (let ((form (read)))
      (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

(define (evaluate-boolean-expression true-symbols expr)
  (let recurse ((expr expr))
    (cond ((symbol? expr)
           (not (not (memq expr true-symbols))))
          ((and (pair? expr) (eq? (car expr) 'not))
           (if (= 2 (length expr))
               (not (recurse (cadr expr)))
               (error "Malformed (not ...)")))
          ((and (pair? expr) (eq? (car expr) 'and))
           (let loop ((expr (cdr expr)))
             (or (null? expr)
                 (and (recurse (car expr))
                      (loop (cdr expr))))))
          ((and (pair? expr) (eq? (car expr) 'or))
           (let loop ((expr (cdr expr)))
             (and (not (null? expr))
                  (or (recurse (car expr))
                      (loop (cdr expr))))))
          (else
           (error "Unknown boolean expression:" expr)))))

;;; The stuff that is specific to this SRFI

(define (trace-expanding form)
  (parameterize ((current-output-port (current-error-port)))
    (display "Expanding ")
    (writeln form)))

(define (expand-cond-expand-into-many clauses)
  (if (null? clauses) (error "No matching cond-expand clause")
      (let* ((clause  (car clauses))
             (c-guard (car clause))
             (c-body  (cdr clause)))
        (if (evaluate-boolean-expression (metadata-features) c-guard)
            (expand-many-forms c-body)
            (expand-cond-expand-into-many (cdr clauses))))))

(define (expand-one-form-into-many form)
  (when trace? (trace-expanding form))
  (cond ((not (pair? form))
         (list form))
        ((eq? (car form) 'cond-expand)
         (expand-cond-expand-into-many (cdr form)))
        ((eq? (car form) 'include)
         (if (and (= 2 (length form)) (string? (cadr form)))
             (read-scheme-metadata-file (cadr form))
             (error "Malformed (include ...)")))
        (else
         (list (expand-many-forms form)))))

(define (expand-many-forms forms)
  (append-map expand-one-form-into-many forms))

(define (read-scheme-metadata)
  (expand-many-forms (read-all)))

(define (read-scheme-metadata-file filename)
  (with-input-from-file filename read-scheme-metadata))

;;; Main program

(define (main)
  (let ((args (cdr (command-line))))
    (if (= 1 (length args))
        (for-each writeln (read-scheme-metadata-file (car args)))
        (error "Usage: complex-format.scm filename"))))

(main)
