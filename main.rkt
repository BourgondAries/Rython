#lang br/quicklang

(require fancy-app)

(provide require define-values begin void define)


(define (count-spaces line)
  (length
    (takef (string->list line)
           (equal? _ #\ ))))

; Skip completely empty lines (could filter these)
; On the first line, emit '('. No problem.
; If the next line is the same height, emit )

(define (parse port)
  (let loop ([str "(void"]
             [prev-indent 0])
    (define line (read-line port))
    (if (eof-object? line)
      (string-append str (make-string (add1 (/ prev-indent 2)) (integer->char 41)))
      (if (not (non-empty-string? line))
        (loop str prev-indent)
        (let* ([curr-indent (count-spaces line)]
               [diff        (- curr-indent prev-indent)]
               [is-empty?   (empty? (string->list line))])
          ; (writeln str)
          (cond
            ([positive? diff] (writeln "pos diff")
                              (loop
                                (string-append
                                  str
                                  (make-string (/ diff 2) (integer->char 40))
                                  line)
                                curr-indent))
            ([zero?     diff] (writeln "zer diff")
                              (loop
                                (string-append
                                  str
                                  (string (integer->char 41))
                                  (string (integer->char 40))
                                  line)
                                prev-indent))
            ([negative? diff] (writeln "neg diff")
                              (loop
                                (string-append
                                  str
                                  (make-string (add1 (/ (- diff) 2)) (integer->char 41))
                                  (make-string 1 (integer->char 40))
                                  line)
                                curr-indent))))
        ))))

(define (read-syntax path port)
  ; (writeln (parse port))
  (define src-lines (list (parse port))) ; (port->lines port))
  (writeln src-lines)
  (define src-datums (format-datums '~a src-lines))
  ; (writeln (first src-datums))
  (define module-datum `(module funstacker-mod "main.rkt"
                          (begin ,@(first src-datums))
                          ))
  (writeln module-datum)
  (datum->syntax #f module-datum))
(provide read-syntax)

(provide #%module-begin)
