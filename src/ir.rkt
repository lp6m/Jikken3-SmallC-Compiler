#lang racket
(provide (all-defined-out))
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
;呼び出すごとに新しい一時用変数(obj構造体）をかえす関数 _x0 _x1 _x2...
(define (fresh-tmpvar)
  (let ([old-tmpvar-num now-tmpvar-num])
    (set! now-tmpvar-num (+ 1 now-tmpvar-num))
         (semantic:obj (string->symbol (string-append "_x" (number->string old-tmpvar-num))) 0 0 'tmp 0)))

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
         (let ((return-var (fresh-tmpvar)))
           `(,(ir-stx:cmpd-stmt
               ;cmpd-stmt-decls
               (list (ir-stx:var-decl return-var))
               ;cmpd-stmt-stmts
               `(,@(exp->ir return-var (stx:return-stmt-var ast))
                 ,(ir-stx:ret-stmt return-var)))))))
    
    ((stx:expression? ast)
     (let ((t0 (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl t0))
           ;cmpd-stmt-stmts
           (exp->ir t0 ast)))))
              
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
    ((stx:funccall-exp? exp)
     (if (equal? 'print (semantic:obj-name (stx:funccall-exp-tgt exp)))
         ;print文
         (let ((t0 (fresh-tmpvar)))
           `(,(ir-stx:cmpd-stmt
             ;cmpd-stmt-decls
             (list (ir-stx:var-decl t0))
             ;cmpd-stmt-stmts
             `(,@(exp->ir t0 (car (stx:funccall-exp-paramlist exp)))
               ,(ir-stx:print-stmt t0)))))
         ;print以外
         (let ((tlist (map (lambda (x) (fresh-tmpvar)) (stx:funccall-exp-paramlist exp))))
           `(,(ir-stx:cmpd-stmt
               ;cmpd-stmt-decls
               (map (lambda (x) (ir-stx:var-decl x)) tlist)
               ;cmpd-stmt-stmts
               `(,@(list-nest-append (map (lambda (x y) (exp->ir x y)) tlist (stx:funccall-exp-paramlist exp)))
                 ,(if (equal? 'void (caar (semantic:obj-type (stx:funccall-exp-tgt exp))))
                      ;void
                      (ir-stx:call-stmt `() (stx:funccall-exp-tgt exp) tlist)
                      (ir-stx:call-stmt dest (stx:funccall-exp-tgt exp) tlist))))))))
    ((stx:aop-exp? exp)
     (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)))
       `(,(ir-stx:cmpd-stmt
           ;cmpd-stmt-decls
           (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
           ;cmpd-stmt-stmts
           ;opが+または-のとき,ポインタ演算の場合がある.整数値及び整数型の変数は4倍する必要がある
           ;semantic.rktのtype-check関数を利用して型を調べる
           (let
               ((left-type (semantic:type-check-exp (stx:aop-exp-left exp)))
                (right-type (semantic:type-check-exp (stx:aop-exp-right exp)))
                (left-ast (stx:aop-exp-left exp))
                (right-ast (stx:aop-exp-right exp))
                (type-rst 0)) ;0でなにもなし 1で左辺を4倍 2で右辺を4倍
             (begin
               ;4倍する必要あるか調べてtype-rstを書き換える ;型検査で既にエラーは弾いているのでエラー処理は考えない
               (cond
                 ((and (semantic:isint left-type) (semantic:isint-pointer right-type)) (set! type-rst 1))
                 ((and (semantic:isint left-type) (semantic:isint-pointerpointer right-type)) (set! type-rst 1))
                 ((and (semantic:isint right-type) (semantic:isint-pointer left-type)) (set! type-rst 2))
                 ((and (semantic:isint right-type) (semantic:isint-pointerpointer left-type)) (set! type-rst 2))
                 (else (set! type-rst 0)))
               ;type-rstに基づいて構文木の書き換えを行う/行わない
               (cond
                 ((= type-rst 1) 
                  (cond 
                    ((stx:lit-exp? left-ast) (set! left-ast (stx:lit-exp (* 4 (stx:lit-exp-val left-ast)) `())))
                    ((and (stx:expression? left-ast) (equal? 1 (length (stx:expression-explist left-ast))) (stx:lit-exp? (car (stx:expression-explist left-ast))))
                     (set! left-ast (stx:lit-exp (* 4 (stx:lit-exp-val (car (stx:expression-explist left-ast)))) `())))
                    (else (set! left-ast (stx:aop-exp '* (stx:lit-exp 4 `()) right-ast `())))))
                 ((= type-rst 2)
                  (cond 
                    ((stx:lit-exp? right-ast) (set! right-ast (stx:lit-exp (* 4 (stx:lit-exp-val right-ast)) `())))
                    ((and (stx:expression? right-ast) (equal? 1 (length (stx:expression-explist right-ast))) (stx:lit-exp? (car (stx:expression-explist right-ast))))
                     (set! right-ast (stx:lit-exp (* 4 (stx:lit-exp-val (car (stx:expression-explist right-ast)))) `())))
                    (else (set! right-ast (stx:aop-exp '* (stx:lit-exp 4 `()) right-ast `()))))))
               ;返り値
               `(,@(exp->ir t0 left-ast)  ;t0 = left
                 ,@(exp->ir t1 right-ast) ;t1 = right
                 ,(ir-stx:assign-stmt dest (ir-stx:aop-exp (stx:aop-exp-op exp) t0 t1)))))))))
                 
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
    ((semantic:obj? exp) `(,(ir-stx:assign-stmt dest (ir-stx:var-exp exp))))
    ((stx:assign-stmt? exp)
     (cond
        ;assign-stmtの左辺がobjの場合exp->irすると a = 3; の左辺aがexp->irされて変数参照かと思われて tmp = aみたいになって希望通りの動作をしない
       ((semantic:obj? (stx:assign-stmt-var exp))
         ;assign-stmtの左辺がobjなら左辺はexp->irしない
         (let ((t0 (fresh-tmpvar)))
           `(,(ir-stx:cmpd-stmt
               ;cmpd-stmt-decls
               (list (ir-stx:var-decl t0))
               ;cmpd-stmt-stmts
               `(,@(exp->ir t0 (stx:assign-stmt-src exp))
                 ,(ir-stx:assign-stmt (stx:assign-stmt-var exp) (ir-stx:var-exp t0))
                 ,(ir-stx:assign-stmt dest (ir-stx:var-exp t0)))))))
       ;assign-stmtの左辺がderef-expのときメモリへの書き込み:write-stmt
       ;int *a; *a = 3;      => t0 = a; t1 = 3; write-stmt t0 t1
       ;int a[10]; a[2] = 3; => t0 = a; t1 = 2; t2 = t0 + t1; t3 = 3; write-stmt t2 t3
         ((stx:deref-exp? (stx:assign-stmt-var exp)) 
          (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)))
            `(,(ir-stx:cmpd-stmt
                ;cmpd-stmt-decls
                (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
                ;cmpd-stmt-stmts
                `(,@(exp->ir t0 (stx:deref-exp-arg (stx:assign-stmt-var exp))) ;deref-expの中身をt0に入れる.(exp->ir t0 (stx:assign-stmt-var exp))ではないことに注意.
                  ,@(exp->ir t1 (stx:assign-stmt-src exp)) 
                  ,(ir-stx:write-stmt t0 t1)
                  ,(ir-stx:assign-stmt dest (ir-stx:var-exp t1)))))))
         ;assign-stmtの左辺がobjでもderefでもないとき:こんなときある？
         (else
          (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)))
           `(,(ir-stx:cmpd-stmt
               ;cmpd-stmt-decls
               (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
               ;cmpd-stmt-stmts
               `(,@(exp->ir t0 (stx:assign-stmt-var exp))　;a *(a+2)
                 ,@(exp->ir t1 (stx:assign-stmt-src exp))
                 ,(ir-stx:assign-stmt t0 (ir-stx:var-exp t1))
                 ,(ir-stx:assign-stmt dest (ir-stx:var-exp t1)))))))))
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
         `(,(ir-stx:assign-stmt dest (ir-stx:addr-exp (stx:addr-exp-var exp)))) ;fix
         ;expressionなどであれば分解する
         ((let ((t0 (fresh-tmpvar)))
            `(,(ir-stx:cmpd-stmt
                ;cmpd-stmt-decls
                (list (ir-stx:var-decl t0))
                ;cmpd-stmt-stmts
                `(,@(exp->ir t0 (stx:addr-exp-var exp))
                  ,(ir-stx:assign-stmt dest (ir-stx:addr-exp t0)))))))))
         
    ((stx:logical-and-or-expr? exp)
     (cond
       ((equal? 'and (stx:logical-and-or-expr-op exp))
            (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)) (L0 (fresh-label)) (L1 (fresh-label)) (L2 (fresh-label)) (L3 (fresh-label)))
               `(,(ir-stx:cmpd-stmt
                   ;cmpd-stmt-decls
                   (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
                   ;cmpd-stmt-stmts
                   `(,@(exp->ir t0 (stx:logical-and-or-expr-log1 exp)) ;t0 = log1;
                      ,(ir-stx:if-stmt t0 L0 L1)                        ;if(t0) then goto L0 else goto L1;
                      ,L0                                               ;label L0;
                      ,@(exp->ir t1 (stx:logical-and-or-expr-log2 exp)) ;t1 = log2;
                      ,(ir-stx:if-stmt t1 L2 L1)                        ;if(t1) then goto L2 else goto L1;
                      ,L2                                               ;label L2;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 1))     ;dest = 1;
                      ,(ir-stx:goto-stmt L3)                            ;goto L3;
                      ,L1                                               ;label L1;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 0))     ;dest = 0;
                      ,L3)))))                                         ;label L3;
                                                     
           ((equal? 'or (stx:logical-and-or-expr-op exp))
            (let ((t0 (fresh-tmpvar)) (t1 (fresh-tmpvar)) (L0 (fresh-label)) (L1 (fresh-label)) (L2 (fresh-label)) (L3 (fresh-label)))
               `(,(ir-stx:cmpd-stmt
                   ;cmpd-stmt-decls
                   (list (ir-stx:var-decl t0) (ir-stx:var-decl t1))
                   ;cmpd-stmt-stmts
                   `(,@(exp->ir t0 (stx:logical-and-or-expr-log1 exp)) ;t0 = log1;
                      ,(ir-stx:if-stmt t0 L1 L0)                        ;if(t0) then goto L1 else goto L0;
                      ,L0                                               ;label L0;
                      ,@(exp->ir t1 (stx:logical-and-or-expr-log2 exp)) ;t1 = log2;
                      ,(ir-stx:if-stmt t1 L1 L2)                        ;if(t1) then goto L1 else goto L2;
                      ,L2                                               ;label L2;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 0))     ;t2 = 0;
                      ,(ir-stx:goto-stmt L3)                            ;goto L3;
                      ,L1                                               ;label L1;
                      ,(ir-stx:assign-stmt dest (ir-stx:lit-exp 1))     ;t2 = 1;
                      ,L3)))))                                          ;label L3;
           (else (error "unknown logical op"))))
    (else (error "kinojunkai"))))

;中間表現見やすく表示する関数
;cmpd-stmtのカッコや,var-declの宣言はすべて表示しない
(define (ir-simple-display ir)
  (cond
    ((list? ir) (for-each (lambda (x) (ir-simple-display x)) ir))
    ((ir-stx:var-decl? ir) (display ""))
    ((ir-stx:fun-def? ir)
     (begin
       (display "fun-def: ")
       (display (semantic:obj-name (ir-stx:fun-def-var ir)))
       (display " ")
       (display (semantic:obj-type (ir-stx:fun-def-var ir)))
       (display "(")
       (for-each (lambda (x) (begin (display (semantic:obj-name (ir-stx:var-decl-var x))) (display " ") (display (semantic:obj-type (ir-stx:var-decl-var x))) (display " "))) (ir-stx:fun-def-parms ir))
       (display ")")
       (newline)
       (for-each (lambda (x) (ir-simple-display x)) (ir-stx:fun-def-body ir)) 
       (newline)))
    ((ir-stx:assign-stmt? ir)
     (begin
       (display (semantic:obj-name (ir-stx:assign-stmt-var ir)))
       (display " = ")
       (ir-simple-display (ir-stx:assign-stmt-exp ir))
       (newline)))
    ((ir-stx:write-stmt? ir)
     (begin
       (display "*")
       (display (semantic:obj-name (ir-stx:write-stmt-dest ir)))
       (display " = ")
       (display (semantic:obj-name (ir-stx:write-stmt-src ir)))
       (newline)))
    ((ir-stx:read-stmt? ir)
     (begin
       (display (semantic:obj-name (ir-stx:read-stmt-dest ir)))
       (display " = *")
       (display (semantic:obj-name (ir-stx:read-stmt-src ir)))
       (newline)))
    ((ir-stx:label-stmt? ir)
     (begin
       (display "label:")
       (display (ir-stx:label-stmt-name ir))
       (newline)))
    ((ir-stx:if-stmt? ir)
     (begin
       (display "if ")
       (display (semantic:obj-name (ir-stx:if-stmt-var ir)))
       (display " then goto ")
       (display (ir-stx:label-stmt-name (ir-stx:if-stmt-tlabel ir)))
       (display " else goto ")
       (display (ir-stx:label-stmt-name (ir-stx:if-stmt-elabel ir)))
       (newline)))
    ((ir-stx:goto-stmt? ir)
     (begin
       (display "goto ")
       (display (ir-stx:label-stmt-name (ir-stx:goto-stmt-label ir)))
       (newline)))
    ((ir-stx:call-stmt? ir)
     (if (null? (ir-stx:call-stmt-dest ir))
         (begin
           (display (semantic:obj-name (ir-stx:call-stmt-tgt ir)))
           (display "(")
           (for-each (lambda (x) (begin (display (semantic:obj-name x)) (display ","))) (ir-stx:call-stmt-vars ir))
           (display ")")
           (newline))
         (begin
           (display (semantic:obj-name (ir-stx:call-stmt-dest ir)))
           (display " = ")
           (display (semantic:obj-name (ir-stx:call-stmt-tgt ir)))
           (display "(")
           (for-each (lambda (x) (begin (display (semantic:obj-name x)) (display ","))) (ir-stx:call-stmt-vars ir))
           (display ")")
           (newline))))
    ((ir-stx:ret-stmt? ir)
     (begin
       (display "return ")
       (if (null? (ir-stx:ret-stmt-var ir))
           (display "")
           (display (semantic:obj-name (ir-stx:ret-stmt-var ir))))
       (newline)))
    ((ir-stx:print-stmt? ir)
     (begin
       (display "print(")
       (display (semantic:obj-name (ir-stx:print-stmt-var ir)))
       (display ")")
       (newline)))
    ((ir-stx:cmpd-stmt? ir)
     (for-each (lambda (x) (ir-simple-display x)) (ir-stx:cmpd-stmt-stmts ir)))
    ((ir-stx:var-exp? ir)
     (display (semantic:obj-name (ir-stx:var-exp-var ir))))
    ((ir-stx:lit-exp? ir)
     (display (ir-stx:lit-exp-val ir)))
    ((ir-stx:aop-exp? ir)
     (begin
       (display "(")
       (ir-simple-display (ir-stx:aop-exp-left ir))
       (display " ")
       (display (ir-stx:aop-exp-op ir))
       (display " ")
       (ir-simple-display (ir-stx:aop-exp-right ir))
       (display ")")))
    ((ir-stx:rop-exp? ir)
     (begin
       (display "(")
       (ir-simple-display (ir-stx:rop-exp-left ir))
       (display " ")
       (display (ir-stx:rop-exp-op ir))
       (display " ")
       (ir-simple-display (ir-stx:rop-exp-right ir))
       (display ")")))
    ((ir-stx:addr-exp? ir)
     (begin
       (display "&")
       (display (semantic:obj-name (ir-stx:addr-exp-var ir)))))
    ((semantic:obj? ir)
     (display (semantic:obj-name ir)))))
;parse-fileしてsemantic-analysisして中間表現にする関数
(define (ir-main filename)
  (begin
    (initialize-parms)    
    (ast->ir (semantic:semantic-analysis-file filename))))