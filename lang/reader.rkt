#lang br/quicklang


(provide (rename-out (read-syntax2 read-syntax))
         (except-out (all-from-out racket/base) read-syntax))

(require racket/base)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-spaces line)
  (length
    (takef (string->list line)
           (lambda (x) (equal? x #\ )))))

(define (begins/tilde? line)
  (equal? #\~
    (first
      (dropf (string->list line)
             (lambda (x) (equal? x #\ ))))))

(define (begins/semicolon? line)
  (equal? #\;
    (first
      (dropf (string->list line)
             (lambda (x) (equal? x #\ ))))))

(define (remove-tilde line)
  (list->string
    (rest
      (dropf (string->list line)
             (lambda (x) (equal? x #\ ))))))

(define (empty-string? str)
  (not (non-empty-string? str)))

;; 1. Consume lines until at least 1 non-empty line has been. Stop at an empty line
;; 2. Run the lines through (read-syntax) [including all empty lines], as a single string
;;    We now have a list of syntax objects
;; 3. Each stx-obj has a line count. Use this to find out the indentation
(provide consume pparse)
(define (consume port)
  (port-count-lines! port)
  (define-values (row col pos) (port-next-location port))
  (let start ([seen-non-empty-line? #f]
              [seen-lines           empty])
    (let ([line (read-line port)])
      (cond
        ([and (empty-string? line) seen-non-empty-line?]
         (values
           (reverse (cons (if (eof-object? line) "" line) seen-lines))
           row))
        (else (start (or (non-empty-string? line) seen-non-empty-line?)
                     (cons line seen-lines)))))))

;; Map a block to a list of syntaxes
(define (pparse path lines start-line-count)
  (define input
    (open-input-string
      (string-join lines
                   "\n"
                   #:after-last "\n")))
  (port-count-lines! input)
  (set-port-next-location! input start-line-count 0 1)
  (let loop ([stxs empty])
    (let ([stx (read-syntax path input)])
      (if (eof-object? stx)
        (reverse stxs)
        (loop (cons stx stxs))))))


(define (is-last^4-list? lst)
  (if (empty? lst)
    #f
    (if (list? (last lst))
      (is-last^3-list? (last lst))
      #f)))

(define (is-last^3-list? lst)
  (if (empty? lst)
    #f
    (if (list? (last lst))
      (is-last-last-list? (last lst))
      #f)))

(define (is-last-last-list? lst)
  (if (empty? lst)
    #f
    (if (list? (last lst))
      (is-last-list? (last lst))
      #f
      )))

(define (is-last-list? lst)
  (if (empty? lst)
    #f
    (list? (last lst))))

(define (drop-last lst)
  (if (empty? lst)
    empty
    (drop-right lst 1)))

;; Syntax entry on the same line
(define (same-line build stx)
  (if (empty? build)
    (list stx)
    (let ([l (last build)])
      (if (list? l)
        (append (drop-last build) (list (same-line l stx)))
        (append build             (list stx))))))

(define (indent build stx)
  (same-line build (list stx)))

;; Second deepest append
;; ((x y)) -> ((x y) (z))
(define (nodent build stx)
  (if (is-last-last-list? build)
    ;; yes? update last recursively
    (append (drop-last build) (list (nodent (last build) stx)))
    ;; no? append syntax to this list
    (append build (list (list stx))))
  )

(define (nodent-escape build stx)
  (if (is-last-last-list? build)
    ;; yes? update last recursively
    (append (drop-last build) (list (nodent (last build) stx)))
    ;; no? append syntax to this list
    (append build (list stx)))
  )

(define (dedent build stx)
  (if (is-last^3-list? build)
    ;; Yes? Update last recursively
    (append (drop-last build) (list (dedent (last build) stx)))
    ;; No? Append syntax to this list
    (begin
      (displayln `(on list ,build))
      (append build (list (list stx)))))
  )

(require racket/bool)

(provide parse*)
(define (parse* stxs)
  (let loop ([stxs* stxs]
             [prv   0]
             [pind  0]
             [build '()])
    (if (empty? stxs*)
      build
      (let* ([stx (first stxs*)]
             [lin (syntax-line stx)]
             [ind (if (= prv lin) pind (syntax-column stx))])
        ; (if (and (not (= prv lin))
        ;          (symbol=? '~ (syntax-e stx)))
        ;   (loop 
          (loop (rest stxs*) lin ind
            ((cond
              ([= prv lin]  same-line)
              ([> ind pind] indent)
              ([= ind pind] nodent)
              ([< ind pind] dedent))
             build stx
             ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse port)
  (let loop ([str "(void"]
             [prev-indent 0]
             [last-b-tilde? #f])
    ;(writeln str)
    (define line (read-line port))
    (if (eof-object? line)
      (string-append str (make-string (add1 (/ prev-indent 2)) (integer->char 41)))
      (if (not (non-empty-string? line))
        (loop str prev-indent last-b-tilde?)
        (let* ([curr-indent (count-spaces line)]
               [btilde      (begins/tilde? line)]
               [diff        (- curr-indent prev-indent)]
               [is-empty?   (empty? (string->list line))])
          ;(writeln line)
          (cond
            ([positive? diff] ;(writeln "pos diff")
                              (if btilde
                                (loop (string-append str (remove-tilde line)) curr-indent #t)
                                (loop
                                  (string-append
                                    str
                                    (make-string (/ diff 2) (integer->char 40))
                                    line)
                                  curr-indent #f)))
            ([zero?     diff] ;(writeln `("zer diff" ,btilde))
                              (if btilde
                                (loop (string-append str (if last-b-tilde? "" (string (integer->char 41))) (remove-tilde line)) curr-indent #t)
                                (loop
                                  (string-append
                                    str
                                    (if last-b-tilde? "" (string (integer->char 41)))
                                    (string (integer->char 40))
                                    line)
                                  prev-indent #f)))
            ([negative? diff] ;(writeln `("neg diff" ,diff ,btilde))
                              (if btilde
                                (loop
                                  (string-append
                                    str
                                    (make-string ((if last-b-tilde? identity add1) (/ (- diff) 2)) (integer->char 41))
                                    (remove-tilde line))
                                  curr-indent #t)
                                (loop
                                  (string-append
                                    str
                                    (make-string ((if last-b-tilde? identity add1) (/ (- diff) 2)) (integer->char 41))
                                    (make-string 1 (integer->char 40))
                                    line)
                                  curr-indent #f)))
        ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-syntax for racket's #lang functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-syntax2 path port)
  (define src-lines (list (parse port))) ; (port->lines port))
  (writeln src-lines)
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module rython "main.rkt"
                          (begin ,@(first src-datums))))
  ; (writeln module-datum)
  (datum->syntax #f module-datum))
