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
         (semantic:obj (string->symbol (string-append "t" (number->string old-tmpvar-num))) 0 0 'tmp)))

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
      (list (ir-stx:cmpd-stmt
          ;宣言部
          (if (null? (stx:compound-stmt-declaration-list-opt ast))
              `()
              (let ((rst (map (lambda (x) (ast->ir x)) (stx:compound-stmt-declaration-list-opt ast))))
                (list-nest-append rst)))
          ;body部
          (if (null? (stx:compound-stmt-statement-list-opt ast))
              `()
              (let ((rst (map (lambda (x) (ast->ir x)) (stx:compound-stmt-statement-list-opt ast))))
                (list-nest-append rst))))));list-nest-append
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
     (if (null? (stx:return-stmt-var ast))
         ;void型のときはvarはnull
         `(,(ir-stx:ret-stmt `()))
         ;int型のとき
         (let ((return-var ((fresh-tmpvar))))
           `(,(ir-stx:cmpd-stmt
               ;cmpd-stmt-decls
               (list (ir-stx:var-decl return-var))
               ;cmpd-stmt-stmts
               `(,@(exp->ir return-var (stx:return-stmt-var ast))
                 ,(ir-stx:ret-stmt return-var)))))))
    
    ((stx:expression? ast) (exp->ir `() ast))
              
    (else `())))

(define (exp->ir dest exp)
  ;destがnull禁止
  (cond
    
    ((stx:expression? exp)
     (cond
       ((null? (stx:expression-explist exp)) `())
       ((= 1 (length (stx:expression-explist exp)))
        `(,@(exp->ir dest (car (stx:expression-explist exp)))))
       (else
         ;最後以外のexplistはcmpd-stmtが並んだリストとなる
         `(,@(map (lambda (x)
                    (let ((t0 (fresh-tmpvar)))
                      (ir-stx:cmpd-stmt
                       ;cmpd-stmt-decls
                       (list (ir-stx:var-decl t0))
                       ;cmpd-stmt-stmts
                       (exp->ir t0 x))))
                  (reverse (cdr (reverse (stx:expression-explist exp))))) ;最後以外
           ,@(exp->ir dest (car (reverse (stx:expression-explist exp))))))))
    ((stx:aop-exp? exp)
     (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
        ;cmpd-stmt-decls
        (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
        ;cmpd-stmt-stmts
        `(,@(exp->ir t0 (stx:aop-exp-left exp))  ;t0 = left
         ,@(exp->ir t1 (stx:aop-exp-right exp)) ;t1 = right
         ,(ir-stx:assign-stmt dest (ir-stx:aop-exp (stx:aop-exp-op exp) t0 t1)))))))
    ((stx:rop-exp? exp)
     (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
        ;cmpd-stmt-decls
        (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
        ;cmpd-stmt-stmts
        `(,@(exp->ir t0 (stx:rop-exp-left exp))
          ,@(exp->ir t1 (stx:rop-exp-right exp))
          ,(ir-stx:assign-stmt dest (ir-stx:rop-exp (stx:rop-exp-op exp) t0 t1)))))))
   
    ((stx:lit-exp? exp) `(,(ir-stx:assign-stmt dest (ir-stx:lit-exp (stx:lit-exp-val exp)))))
    ((semantic:obj? exp) `(,(ir-stx:assign-stmt dest exp)))
    ((stx:assign-stmt? exp)
     (let ((t0 (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl t0))
           ;cmpd-stmt-stmts
           `(,@(exp->ir t0 (stx:assign-stmt-src exp))
             ,(ir-stx:assign-stmt (stx:assign-stmt-var exp) t0))))))
    ((stx:deref-exp? exp)
     (let ((t0 (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl t0))
           ;cmpd-stmt-stmts
           `(,@(exp->ir t0 (stx:deref-exp-arg exp))
             ,(ir-stx:read-stmt dest t0))))))
    ((stx:addr-exp? exp) 
     ;addr-expのvarがすでにobjならすぐ返す
     (if (semantic:obj? (stx:addr-exp-var exp))
         (ir-stx:assign-stmt dest (ir-stx:addr-exp (stx:addr-exp-var exp)))
         ;expressionなどであれば分解する
         ((let ((t0 (fresh-tmpvar)))
            `(,(ir-stx:cmpd-stmt
                ;cmpd-stmt-decls
                (list (ir-stx:var-decl t0))
                ;cmpd-stmt-stmts
                `(,@(exp->ir t0 (stx:addr-exp-var exp))
                  ,(ir-stx:assign-stmt dest (ir-stx:addr-exp-var t0)))))))))
         
    ((stx:logical-and-or-expr? exp)
     (cond
       ((equal? 'and (stx:logical-and-or-expr-op exp))
            ((let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)) (L0 (fresh-label)) (L1 (fresh-label)) (L2 (fresh-label)) (L3 (fresh-label)))
               `(,(ir-stx:cmpd-stmt
                   ;cmpd-stmt-decls
                   (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
                   ;cmpd-stmt-stmts
                   `(,@(exp->ir t0 (stx:logical-and-or-expr-log1 exp)) ;t0 = log1;
                      ,@(exp->ir t1 (stx:logical-and-or-expr-log2 exp)) ;t1 = log2;
                      ,(ir-stx:if-stmt t0 L0 L1)                        ;if(t0) then goto L0 else goto L1;
                      ,L0                                               ;label L0;
                      ,(ir-stx:if-stmt t1 L2 L1)                        ;if(t1) then goto L2 else goto L1;
                      ,L2                                               ;label L2;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 1))     ;dest = 1;
                      ,(ir-stx:goto-stmt L3)                            ;goto L3;
                      ,L1                                               ;label L1;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 0))     ;dest = 0;
                      ,L3))))))                                         ;label L3;
                                                     
           ((equal? 'or (stx:logical-and-or-expr-op exp))
            ((let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)) (L0 (fresh-label)) (L1 (fresh-label)) (L2 (fresh-label)) (L3 (fresh-label)))
               `(,(ir-stx:cmpd-stmt
                   ;cmpd-stmt-decls
                   (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
                   ;cmpd-stmt-stmts
                   `(,@(exp->ir t0 (stx:logical-and-or-expr-log1 exp)) ;t0 = log1;
                      ,@(exp->ir t1 (stx:logical-and-or-expr-log2 exp)) ;t1 = log2;
                      ,(ir-stx:if-stmt t0 L1 L0)                        ;if(t0) then goto L1 else goto L0;
                      ,L0                                               ;label L0;
                      ,(ir-stx:if-stmt t1 L1 L2)                        ;if(t1) then goto L1 else goto L2;
                      ,L2                                               ;label L2;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 0))     ;t2 = 0;
                      ,(ir-stx:goto-stmt L3)                            ;goto L3;
                      ,L1                                               ;label L1;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 1))     ;t2 = 1;
                      ,L3))))))                                         ;label L3;
           (else (error "unknown logical op"))))
    (else (error "kinojunkai"))))

;parse-fileしてsemantic-analysisして中間表現にする関数
(define (ir-main filename)
  (begin
    (initialize-parms)    
    (ast->ir (semantic:semantic-analysis (parser:parse-file filename)))))