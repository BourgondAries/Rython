#lang br/quicklang


(provide read-syntax (all-from-out racket/base))

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
(define (read-syntax path port)
  (define src-lines (list (parse port))) ; (port->lines port))
  (writeln src-lines)
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module rython "main.rkt"
                          (begin ,@(first src-datums))))
  ; (writeln module-datum)
  (datum->syntax #f module-datum))
