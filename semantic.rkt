#lang racket
(require (prefix-in parser: "parser.rkt")
         (prefix-in stx:    "syntax.rkt"))
;オブジェクト情報をもつ構造体
(struct obj (name lev kind type) #:transparent)
;kind = var parm func proto 
;type = (int), (pointer t), (array t n), (fun int/void a1 a2 ..)

;オブジェクト情報を収集 ; objのリストを返す
(define (collect-object ast)
  (define (var-declist-tolist decl)
           ;(int), (pointer t), (array t n)
           (stx:var-declist 
  (define (collect-object-main ast lev)
       (cond
         ((list? ast) (map (lambda (x) (collect-object-main x lev)) ast))
         ((stx:declaration? ast) 
          (let ((declist (stx:declaration-declist ast)))
            (map (lambda (x) (obj (stx:var-decl-id x) lev  'var (var-decl-tolist x))) declist)))
         
                       