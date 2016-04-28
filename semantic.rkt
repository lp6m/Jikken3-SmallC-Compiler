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


(define initial-obj-collection (lambda (x) #f))
(define now-step 1)
(define (fresh-step)
  (let([oldstep now-step]) (set! now-step (+ 1 now-step)) oldstep))
(define (add-obj collection var);cons
  (if (collection var)
      (cons (collection var) collection)
      (let ((new-step (fresh-step)))
        (cons new-step
             (lambda (x) 
               (if (equal? x var) 
                      new-step
                      (collection x)))))))

(define (add-obj-list-get-result objlist)
  (define (add-obj-list-get-result-main collection objlist)
    (if (= 1 (length objlist))
      `(,(car (add-obj collection (car objlist))))
      (let ((rst (add-obj collection (car objlist))))
        `(,(car rst) ,@(add-obj-list-get-result-main (cdr rst) (cdr objlist))))))
  (add-obj-list-get-result-main initial-obj-collection objlist))

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


(struct a-st (x y) #:transparent)

                       