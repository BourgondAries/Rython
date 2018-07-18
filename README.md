# Rython - A Python-like Racket #

A lot of people seem to complain about the parentheses in lisps and sadly never truly come to appreciate the sheer power lisp brings them. With this I'd like to introduce those people to a Lisp that doesn't use too many parentheses but keeps the full functionality of lisp (in this case Racket).

# Lisps? #

If you're unfamiliar with lisps: there are multiple. Lisp is not a single language, but a family of languages. Racket is one such language. Rython is implemented in Racket.

# Example #

```
#lang rython

~; Factorial function

define (fact x [acc 1])
  if (> x 0)
    fact (sub1 x) (* acc x)
    ~ acc

fact 10
```

# Short Guide #

Rython uses indentation to deduce where to put ( and ). A positive indentation after some line indicates a body:
Indents are currently hardcoded as 2 spaces.
```
define x
  + 1 2 3
```

```
(define x     ; No ) here
  (+ 1 2 3))  ; Extra ) here to close the define
```

Same-level lines are simply enclosed by ().
```
define x
  displayln "hello world"
  + 1 2 3
```

```
(define x
  (displayln "hello-world")
  (+ 1 2 3))
```

# Surround Avoidance #

To prevent a line from being surrounded by (), make ~ the first character after whitespace.

# Installing #

1. Download and instal [Racket](https://racket-lang.org/download/).
2. Run `raco pkg install rython`
3. Start a file with `#lang rython`, add some code
4. Run `racket <your-file>`
