#! /usr/bin/env racket
#lang reader "main.rkt"

require racket/base

+ 10 20

define (x y)
  println 1337
  + y 10

println (x 3)

define cool "this is a cool variable"
string-append cool " and expressions work"

define
  nice meme z #:pref [pref 0]
  println pref
  - meme z

+ 1
nice -321 90 #:pref 6
+ 2

define (z)
  define u 10
  define i 30
  let ([m (+ u i)])
    - m i u

+ 3
displayln (z)

require syntax/parse/define
  for-syntax racket

define-syntax-parser test
  [_ control (~or (1 2 3) (x))]
    identity #'control

test (+ 1 2 3 4 5) (1 2 3)
