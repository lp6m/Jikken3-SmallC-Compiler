 #lang racket
(require (prefix-in parser: "parser.rkt")
         (prefix-in stx:    "syntax.rkt"))
;オブジェクト情報をもつ構造体
(struct obj (name lev kind type) #:transparent)
;kind = var parm fun proto 
;type = (int), (pointer t), (array t n), (fun int/void a1 a2 ..)

;parser.rkt内のparse-fileやparse-string関数で返る抽象構文木を受け取り,
;オブジェクト情報を収集して,2重宣言があれば即時エラーを吐く.
;返り値は、引数の抽象構文木内のstx:var-decl構造体を、レベル情報や宣言種類を含んだobj構造体に変換した抽象構文木.
(define (collect-object ast)
  
  ;(int), (pointer int), (array (pointer int) 10), (array int 10)のいずれかの形式をかえす.
  (define (var-decl-totype decl) 
    (if (stx:var-decl-isarray decl)
       (if (stx:var-decl-ispointer decl)
          (list 'array (list 'pointer (stx:var-decl-type decl)) (stx:var-decl-num decl))
          (list 'array (stx:var-decl-type decl) (stx:var-decl-num decl)))
       (if (stx:var-decl-ispointer decl)
          (list 'pointer (stx:var-decl-type decl))
          (list (stx:var-decl-type decl)))))

   ;var-declからlev,kindを指定したobj構造体をかえす
  (define (var-decl-toobj decl lev kind)
    (obj (stx:var-decl-id decl) lev kind (var-decl-totype decl)))
  ;メイン部分
  (define (collect-object-main ast lev)
    (cond
      ((list? ast) (map (lambda (x) (collect-object-main x lev)) ast))
      ;stx:declaration
      ((stx:declaration? ast) 
       (stx:declaration
        (let ((declist (stx:declaration-declist ast))) (map (lambda (x) (var-decl-toobj x lev 'var)) declist))
        (stx:declaration-pos ast)))
      ;stx:func-prototype
      ((stx:func-prototype? ast)
       (stx:func-prototype
        (var-decl-toobj (stx:func-prototype-var-decl ast) lev 'proto)
        (if (null? (stx:func-prototype-declarator ast))
           `()
          (map (lambda (x) (var-decl-toobj x (+ lev 1) 'parm)) (stx:func-prototype-declarator ast)))
        (stx:func-prototype-pos ast)))
      ;stx:func-definition
      ((stx:func-definition? ast)
       (stx:func-definition
        (var-decl-toobj (stx:func-definition-var-decl ast) lev 'fun)
        (if (null? (stx:func-definition-declarator ast))
         `()
         (map (lambda (x) (var-decl-toobj x (+ lev 1) 'parm)) (stx:func-definition-declarator ast)))
        (collect-object-main (stx:func-definition-statement ast) (+ lev 1))
        (stx:func-definition-pos ast)))
      ;stx:compound-stmt
      ((stx:compound-stmt? ast)
       (stx:compound-stmt
        (if (null? (stx:compound-stmt-declaration-list-opt ast))
           `()
           (map (lambda (x) (collect-object-main x (+ lev 1))) (stx:compound-stmt-declaration-list-opt ast)))
        (if (null? (stx:compound-stmt-statement-list-opt ast))
           `()
           (map (lambda (x) (collect-object-main x (+ lev 1))) (stx:compound-stmt-statement-list-opt ast)))
        (stx:compound-stmt-pos ast)))
      ;stx:expression
      ((stx:expression? ast)
       (stx:expression
        (stx:expression-iskakko ast)
        (map (lambda (x) (collect-object-main x lev)) (stx:expression-explist ast))
        (stx:expression-pos ast)))
      ;stx:func-call
      ((stx:funccall-exp? ast)
       (stx:funccall-exp
        (stx:funccall-exp-tgt ast)
        (collect-object-main (stx:funccall-exp-paramlist ast) lev)
        (stx:funccall-exp-pos ast)))
      ;stx:aop-exp
      ((stx:aop-exp? ast)
       (stx:aop-exp
        (stx:aop-exp-op ast)
        (collect-object-main (stx:aop-exp-left ast) lev)
        (collect-object-main (stx:aop-exp-right ast) lev)
        (stx:aop-exp-pos ast)))
      ;stx:rop-exp
      ((stx:rop-exp? ast)
       (stx:rop-exp
        (stx:rop-exp-op ast)
        (collect-object-main (stx:rop-exp-left ast) lev)
        (collect-object-main (stx:rop-exp-right ast) lev)
        (stx:rop-exp-pos ast)))
      ;stx:assign-stmt
      ((stx:assign-stmt? ast)
       (stx:assign-stmt
        (collect-object-main (stx:assign-stmt-var ast) lev)
        (collect-object-main (stx:assign-stmt-src ast) lev)
        (stx:assign-stmt-pos ast)))
      ;stx;while-stmt
      ((stx:while-stmt? ast)
       (stx:while-stmt
        (collect-object-main (stx:while-stmt-test ast) lev)
        (collect-object-main (stx:while-stmt-body ast) lev)
        (stx:while-stmt-pos ast)))
      ;stx:if-else-stmt
      ((stx:if-else-stmt? ast)
       (stx:if-else-stmt
        (collect-object-main (stx:if-else-stmt-test ast) lev)
        (collect-object-main (stx:if-else-stmt-tbody ast) lev)
        (collect-object-main (stx:if-else-stmt-ebody ast) lev)
        (stx:if-else-stmt-pos ast)))
      ;stx:return-stmt
      ((stx:return-stmt? ast)
       (stx:return-stmt
        (collect-object-main (stx:return-stmt-var ast) lev)
        (stx:return-stmt-pos ast)))
      ;stx:logical-and-or-stmt
      ((stx:logical-and-or-expr? ast)
       (stx:logical-and-or-expr
        (stx:logical-and-or-expr-op ast)
        (collect-object-main (stx:logical-and-or-expr-log1 ast) lev)
        (collect-object-main (stx:logical-and-or-expr-log2 ast) lev)
        (stx:logical-and-or-expr-pos ast)))
      ;stx:addr-exp
      ((stx:addr-exp? ast)
       (stx:addr-exp
        (collect-object-main (stx:addr-exp-var ast) lev)
        (stx:addr-exp-pos ast)))
      ;stx:deref-exp
      ((stx:deref-exp? ast)
       (stx:deref-exp
        (collect-object-main (stx:deref-exp-arg ast) lev)
        (stx:deref-exp-pos ast)))
      ;stx:var-decl
      ((stx:var-decl? ast) "var-declになることはないはずなのでエラーです")
      ;stx:lit-exp
      ((stx:lit-exp? ast) ast)
      ((stx:var-exp? ast) ast)
      (else ast)))
  
  (collect-object-main ast 0))

(define initial-env (list (lambda (x) #f)))
(define (search-env env var);this env is envlist ;return var or #f
    (if (= (length env) 1)
        ((car env) var) ;return from here
        (if ((car env) var)
            (car ((car env) var)) ;return from here
            (search-env (cdr env) var))))
(define (add-ref-env env var) 
  (define (add-ref-env-main env var)
    (let ((search-rst (search-env env var)))
    (if search-rst
        (cons var env) 
        (let ((top-env (lambda (x) (if (equal? x var) var ((car env) var)))))
          (if (null? (cdr env))
              (cons var `(,top-env))
              (cons var `(,top-env ,@(cdr env))))))))
  (add-ref-env-main env var))
        
(define (env-test mylist)
  (define (env-test-main env mylist)
    (if (= 1 (length mylist))
        `(,(car (add-ref-env env (car mylist))))
        (let ((rst (add-ref-env env (car mylist))))
          `(,(car rst) ,@(env-test-main (cdr rst) (cdr mylist))))))
  (env-test-main initial-env mylist))

(define (add-newenv env)
  `(,@initial-env ,env))

(define (pop-topenv env)
  (cdr env))

                       