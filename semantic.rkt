#lang racket
(require (prefix-in parser: "parser.rkt")
         (prefix-in stx:    "syntax.rkt"))
;オブジェクト情報をもつ構造体
(struct obj (name lev kind type) #:transparent)
;kind = var parm fun proto 
;type = (int), (pointer t), (array t n), (fun int/void a1 a2 ..)

;オブジェクト情報を収集 ; 
(define (collect-object ast)
  (define (var-decl-totype decl) ;(int), (pointer int), (array (pointer int) 10), (array int 10)のいずれかの形式をかえす.
           (if (stx:var-decl-isarray decl)
              (if (stx:var-decl-ispointer decl)
                 (list 'array (list 'pointer (stx:var-decl-type decl)) (stx:var-decl-num decl))
                 (list 'array (stx:var-decl-type decl) (stx:var-decl-num decl)))
              (if (stx:var-decl-ispointer decl)
                 (list 'pointer (stx:var-decl-type decl))
                 (list (stx:var-decl-type decl)))))
  (define (var-decl-toobj decl lev kind) ;var-declからlev,kindを指定したobj構造体をかえす
    (obj (stx:var-decl-id decl) lev kind (var-decl-totype decl)))
  
  (define (collect-object-main ast lev)
       (cond
         ((list? ast) (map (lambda (x) (collect-object-main x lev)) ast))
         ((stx:declaration? ast) 
          (let ((declist (stx:declaration-declist ast)))
            (map (lambda (x) (var-decl-toobj x lev 'var)) declist)))
         ((stx:func-prototype? ast)
          (if (null? (stx:func-prototype-declarator ast))
             `(,(var-decl-toobj (stx:func-prototype-var-decl ast) lev 'proto)) ;パラメータがないとき
             `(,(var-decl-toobj (stx:func-prototype-var-decl ast) lev 'proto) ,(map (lambda (x) (var-decl-toobj x (+ lev 1) 'parn)) (stx:func-prototype-declarator ast)))));パラメータがあるとき
         ((stx:func-definition? ast)
          (if (null? (stx:func-definition-declarator ast))
             `(,(var-decl-toobj (stx:func-definition-var-decl ast) lev 'fun))
             `(,(var-decl-toobj (stx:func-definition-var-decl ast) lev 'fun) ,(map (lambda (x) (var-decl-toobj x (+ lev 1) 'parm)) (stx:func-prototype-declarator ast)))))
         (else "unko")))
  (collect-object-main ast 0))
         
                       