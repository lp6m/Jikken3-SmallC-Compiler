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
      ;stx:func-prototype　'protoのobj構造体のtypeはconsセルで,carは関数自体の型情報,cdrは引数の型情報のリストが入っている（consというか結局はリスト）
      ((stx:func-prototype? ast)
       (let* ((proto-declarator-objlist　　　　　　　;引数のobj構造体のリスト
               (if (null? (stx:func-prototype-declarator ast))
                  `()
                  (map (lambda (x) (var-decl-toobj x (+ lev 1) 'parm)) (stx:func-prototype-declarator ast))))
               (proto-declarator-obj-typelist       ;引数のobjのtypeのリスト
                (if (null? proto-declarator-objlist)
                   `()
                   (map (lambda (x) (obj-type x)) proto-declarator-objlist))))
         ;protoのobj構造体のtypeに引数のtypeリストをappend
         (stx:func-prototype
          (let ((fp-obj (var-decl-toobj (stx:func-prototype-var-decl ast) lev 'proto)))
            (obj (obj-name fp-obj) (obj-lev fp-obj) (obj-kind fp-obj) (append (list (obj-type fp-obj)) proto-declarator-obj-typelist)))
          proto-declarator-objlist
          (stx:func-prototype-pos ast))))
      ;stx:func-definition　'funのobj構造体のtypeはconsセルで,carは関数自体の型情報,cdrは引数の型情報のリストが入っている（consというか結局はリスト）
      ((stx:func-definition? ast)
       (let* ((fun-declarator-objlist                ;引数のobj構造体のリスト
               (if (null? (stx:func-definition-declarator ast))
                  `()
                  (map (lambda (x) (var-decl-toobj x (+ lev 1) 'parm)) (stx:func-definition-declarator ast))))
              (fun-declarator-obj-typelist          ;引数のobjのtypeのリスト
               (if (null? fun-declarator-objlist)
                  `()
                  (map (lambda (x) (obj-type x)) fun-declarator-objlist))))
         ;funのobj構造体のtypeに引数のtypeリストをappend
         (stx:func-definition
          (let ((fd-obj (var-decl-toobj (stx:func-definition-var-decl ast) lev 'fun)))
            (obj (obj-name fd-obj) (obj-lev fd-obj) (obj-kind fd-obj) (append (list (obj-type fd-obj)) fun-declarator-obj-typelist)))
          fun-declarator-objlist
          (collect-object-main (stx:func-definition-statement ast) (+ lev 1))
          (stx:func-definition-pos ast))))
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
      ;stx:var-exp
      ((stx:var-exp? ast) ast)
      ;else
      (else ast)))
  
  (collect-object-main ast 0))

;以下は環境のスタックを処理するための関数
;環境の実装方法は、レベル1つにつき1つのlambda式を作り、そのlambda式のリストが環境である。
;パラメータ定義や関数
(define initial-env (list (lambda (x) #f)))
;lambda式のリストである環境からobj-nameが重複するものを検索をかける
;返り値はみつかったobjまたは#f
(define (search-env-by-obj-name env tgt-obj)
    (if (= (length env) 1)
        ((car env) tgt-obj) 
        (if ((car env) tgt-obj)
            (car ((car env) tgt-obj)) 
            (search-env-by-obj-name (cdr env) tgt-obj))))

;環境からobjを参照し,なければ環境の先頭にobjを追加
(define (add-ref-env env newobj) 
  (define (add-ref-env-main env newobj)
    ;newobjとnameが一致するobjを環境から検索
    (let ((search-rst (search-env-by-obj-name env newobj)))
      ;環境に新しく追加するオブジェクトの種類によって変更
      (cond
        ;新しいobjが変数宣言のとき
        ((equal? (obj-kind newobj) 'var)
         (if search-rst
            ;既にnameが一致するオブジェクトが存在するとき
            (let ((error-flg
                   ;検索されたオブジェクトの種類によって処理を分岐
                   (cond
                     ;関数宣言で既にあるとき,その関数宣言のレベルが0であればエラー
                     ((equal? (obj-kind search-rst) 'fun)
                      (if (equal? 0 (obj-lev newobj))
                         (error "functionとvarで同名の宣言は不可")
                         #f))
                     ;変数宣言で既にあるとき,新しいオブジェクトとレベルが同じならエラー
                     ((equal? (obj-kind search-rst) 'var)
                      (if (equal? (obj-lev newobj) (obj-lev search-rst))
                         (error "既に同じレベルでの変数宣言があります")
                         #f))
                     ;関数パラメータで既にあるとき,警告を表示して登録
                     ((equal? (obj-kind search-rst) 'parm)
                      (begin (display "警告======!!") #f))
                     (else (error "unknown obj-kind")))))
              (if error-flg
                 (error "ここにはこない")
                 (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) newobj)))))
                   (if (null? (cdr env))
                      (cons newobj `(,top-env))
                      (cons newobj `(,top-env ,@(cdr env)))))))
            ;既にnameが一致するオブジェクトが存在しないとき→登録
            (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) newobj)))))
              (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
        ;新しいobjが関数パラメータ宣言のとき
        ((equal? (obj-kind newobj) 'parm)
         (if search-rst
            ;既にnameが一致するオブジェクトが存在するとき
            ;パラメータ二重宣言ならエラー
            (if (equal? (obj-kind search-rst) 'parm)
               (error "パラメータで二重宣言")
               ;登録
               (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) newobj)))))
                 (if (null? (cdr env))
                    (cons newobj `(,top-env))
                    (cons newobj `(,top-env ,@(cdr env))))))
            ;既にnameが一致するオブジェクトが存在しないとき→登録
            (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) newobj)))))
              (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
        ;新しいobjが関数プロトタイプ宣言のとき type(関数の型と引数の型情報）が一致してるかを調べ、一致しないならエラー 一致なら登録もせず元の環境をかえす
        ((equal? (obj-kind newobj) 'proto)
         (if (equal? (obj-type newobj) (obj-type search-rst))
            (cons newobj env)
            (error "型情報の違う二重プロトタイプ宣言はエラー")))
        ;新しいobjが関数定義のとき→即エラー
        ((equal? (obj-kind newobj) 'fun)
         (error "関数の二重定義はエラー"))
        (else (error "unknown obj-kind ")))))
    
  (add-ref-env-main env newobj))
        
(define (env-test mylist)
  (define (env-test-main env mylist)
    (if (= 1 (length mylist))
        `(,(car (add-ref-env env (car mylist))))
        (let ((rst (add-ref-env env (car mylist))))
          `(,(car rst) ,@(env-test-main (cdr rst) (cdr mylist))))))
  (env-test-main initial-env mylist))

;新しく環境のリストの先頭にlambda式を追加する
(define (add-newenv env)
  `(,@initial-env ,env))

;先頭の環境をpopする
(define (pop-topenv env)
  (cdr env))

                       