#lang racket
(require (prefix-in parser: "parser.rkt")
        (prefix-in semantic: "semantic.rkt")
        (prefix-in stx: "parser-syntax.rkt")
         (prefix-in ir-stx:    "ir-syntax.rkt"))

;リストの入れ子append
;(list-nest-append `(`(1 2 3 4) `(5 6 7 8))) => `(1 2 3 4 5 6 7 8)
(define (list-nest-append lists)
  (if (null? lists)
     `()
     (append (car lists) (list-nest-append (cdr lists)))))
;色々な値を初期化する関数
(define (initialize-parms)
  (begin
    (set! now-symbol-num 0)
    (set! now-tmpvar-num 0)))

;現時点でのラベルの個数を示す
(define now-symbol-num 0)
;呼び出すごとに新しいラベルをかえす関数 L0,L1,L2..
(define (fresh-label)
  (let ([old-symbol-num now-symbol-num])
    (set! now-symbol-num (+ 1 now-symbol-num))
    (ir-stx:label-stmt (string->symbol (string-append "L" (number->string old-symbol-num))))))
;現時点での一時用の変数の個数を示す
(define now-tmpvar-num 0)
;呼び出すごとに新しい一時用変数(obj構造体）をかえす関数 t0 t1 t2...
(define (fresh-tmpvar)
  (let ([old-tmpvar-num now-tmpvar-num])
    (set! now-tmpvar-num (+ 1 now-tmpvar-num))
         (semantic:obj (string->symbol (string-append "L" (number->string old-tmpvar-num))) 0 0 'tmp)))

;抽象構文木を中間表現に変換する関数.
;appendしていくのでast->irはかならずlistをかえすようにする
(define (ast->ir ast)
  (cond
    ((null? ast) `())
    ;空リストはけすためにmapではなくappendでまわす
    ((list? ast) (append (ast->ir (car ast)) (ast->ir (cdr ast))))
    ((stx:func-prototype? ast) `()) 
    ((stx:func-definition? ast)
     `(,(ir-stx:fun-def
         (stx:func-definition-var-decl ast) ;obj構造体
         (map (lambda (x) (ir-stx:var-decl x)) (stx:func-definition-declarator ast)) ;パラメータのリストをそれぞれir-stx:var-declで包む.
         (ast->ir (stx:func-definition-statement ast)))))
    ((stx:declaration? ast)
     ;stx:declarationは複数のobjをもつかir-stx:var-declは1つしかもてないので包む.
     (map (lambda (x) (ir-stx:var-decl x)) (stx:declaration-declist ast)))

    ((stx:compound-stmt? ast)
      `(,(ir-stx:cmpd-stmt
        ;宣言部
        (if (null? (stx:compound-stmt-declaration-list-opt ast))
                  `()
                  (list-nest-append (map (lambda (x) (ast->ir x)) (stx:compound-stmt-declaration-list-opt ast))))
        ;body部
        (if (null? (stx:compound-stmt-statement-list-opt ast))
           `()
           (list-nest-append (map (lambda (x) (ast->ir x)) (stx:compound-stmt-statement-list-opt ast)))))))
    ((stx:if-else-stmt? ast)
     (let ((if-test-var (fresh-tmpvar)) (L1 (fresh-label)) (L2 (fresh-label)) (L3 (fresh-label)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl if-test-var))
           ;cmpd-stmt-stmts
           `(,@(exp->ir if-test-var (stx:if-else-stmt-test ast))
              ,(ir-stx:if-stmt if-test-var L1 L2)
              ,L1
              ,@(ast->ir (stx:if-else-stmt-tbody ast))
              ,(ir-stx:goto-stmt L3)
              ,L2
              ,@(ast->ir (stx:if-else-stmt-ebody ast))
              ,L3)))))
    ((stx:while-stmt? ast)
     (let ((while-test-var (fresh-tmpvar)) (L1 (fresh-label)) (L2 (fresh-label)) (L3 (fresh-label)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl while-test-var))
           ;cmpd-stmt-stmts
           `(,L1
             ,@(exp->ir while-test-var (stx:while-stmt-test ast))
             ,(ir-stx:if-stmt while-test-var L2 L3)
             ,L2
             ,@(ast->ir (stx:while-stmt-body ast))
             ,(ir-stx:goto-stmt L1)
             ,L3)))))
    ((stx:return-stmt? ast)
     (let ((return-var (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl return-var))
           ;cmpd-stmt-stmts
           `(,@(exp->ir return-var (stx:return-stmt-var ast))
             ,(ir-stx:ret-stmt return-var))))))
    
              
    (else `())))

(define (exp->ir dest exp)
  `(,(ir-stx:assign-stmt dest (ir-stx:lit-exp 0))))
;parse-fileしてsemantic-analysisして中間表現にする関数
(define (ir-main filename)
  (begin
    (initialize-parms)    
    (ast->ir (semantic:semantic-analysis (parser:parse-file filename)))))