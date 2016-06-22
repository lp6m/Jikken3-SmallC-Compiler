#lang racket
(provide (all-defined-out))
(require (prefix-in parser: "parser.rkt")
         (prefix-in semantic: "semantic.rkt")
         (prefix-in stx: "parser-syntax.rkt")
         (prefix-in ir-stx:    "ir-syntax.rkt")
         (prefix-in ir: "ir.rkt")
         (prefix-in addr: "addr.rkt")
         (prefix-in gen: "gen.rkt")
         (prefix-in optimize: "optimize/optimize.rkt"))

(define (compile filename  #:optimize (flag #f))
  (let ((ir (if flag
                (optimize:optimize (addr:assign-addr (ir:ir-main filename)))
                (addr:assign-addr (ir:ir-main filename)))))
    (display (gen:code->string (gen:gen-code ir)))))
