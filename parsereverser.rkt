#lang racket

(require (prefix-in stx: "syntax.rkt"))


(define (parse-reverse tree)
  (cond
    ((not (list? tree)) (display '()))
    (else (parse-reverse (car tree)))
    )
  )


(define (test tree)
  (cond
    ((stx:for-stmt? tree) 
     (list (stx:for-stmt-initial tree) (stx:while-stmt (stx:for-stmt-test tree) (list (stx:for-stmt-body tree) (stx:for-stmt-repeat tree))(stx:for-stmt-pos tree))))
    (else ('()))))