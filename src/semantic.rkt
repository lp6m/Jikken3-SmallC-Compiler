#lang racket
(provide (all-defined-out))
(require 
  parser-tools/lex
  (prefix-in : parser-tools/lex-sre)
  (prefix-in parser: "parser.rkt")
  (prefix-in stx:    "parser-syntax.rkt"))
;オブジェクト情報をもつ構造体
(struct obj (name lev kind type ofs) #:transparent)
;kind = var parm fun proto 
(define fun 'fun)
(define var 'var)
(define proto 'proto)
(define parm 'parm)
;type = (int), (pointer t), (array t n), (fun int/void a1 a2 ..)

(define initial-env (list (lambda (x) #f)))
;collect-objectで使う環境
(define obj-env initial-env)
;型検査用.関数の名前と型と引数の型をすべてリストで表したものを要素とするリストをつくる
;collect-object関数で木を巡回しているときに行う. print関数だけもともと追加しておく
(define initial-func-def-list (list (list 'print (list 'void) (list 'int))))
(define func-def-list initial-func-def-list)
;型検査用.func-def-listからnameが一致するものを取り出す
(define (search-func-by-name name)
  (define (search-func-by-name-main name tgt-list)
    (if (null? tgt-list)
        #f
        (if (equal? (caar tgt-list) name)
            (car tgt-list)
            (search-func-by-name-main name (cdr tgt-list)))))
  (search-func-by-name-main name func-def-list))


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
    (obj (stx:var-decl-id decl) lev kind (var-decl-totype decl) 0))
  
  ;メイン部分
  (define (collect-object-main ast lev)
    (cond
      ((list? ast) (map (lambda (x) (collect-object-main x lev)) ast))
      ;stx:declaration
      ((stx:declaration? ast) 
       (stx:declaration
        (let ((declist (stx:declaration-declist ast)))
          (map
           (lambda (x)
             (let* ((newobj (var-decl-toobj x lev var))
                    (rst (add-ref-env obj-env newobj (stx:declaration-pos ast))))　;環境にオブジェクトを追加した結果をrstに入れる
               (begin (set! obj-env (cdr rst)) (car rst)))) declist))　　          ;環境を更新
        (stx:declaration-pos ast)))
      ;stx:func-prototype　protoのobj構造体のtypeはconsセルで,carは関数自体の型情報,cdrは引数の型情報のリストが入っている（consというか結局はリスト）
      ((stx:func-prototype? ast)
       (let* ((proto-declarator-objlist　　　　　　　;引数のobj構造体のリスト
               (if (null? (stx:func-prototype-declarator ast))
                   `()
                   (map (lambda (x) (var-decl-toobj x (+ lev 1) parm)) (stx:func-prototype-declarator ast))))
              (proto-declarator-obj-typelist       ;引数のobjのtypeのリスト
               (if (null? proto-declarator-objlist)
                   `()
                   (map (lambda (x) (obj-type x)) proto-declarator-objlist)))
              (proto-obj  ;protoのobj構造体.typeに引数の型情報が必要なので追加.具体的にはprotoのobj構造体のtypeに引数のtypeリストをappendする.
               (let ((fp-obj (var-decl-toobj (stx:func-prototype-var-decl ast) lev proto)))
                 (obj (obj-name fp-obj) (obj-lev fp-obj) (obj-kind fp-obj) (append (list (obj-type fp-obj)) proto-declarator-obj-typelist) 0))))
         ;返り値はここ
         (stx:func-prototype
          (let* ((rst (add-ref-env  obj-env proto-obj (stx:func-prototype-pos ast))));プロトタイプ宣言のobjを環境に追加した結果をrstに入れる
            (set! obj-env (cdr rst))　　　　　　　　　　         ;環境を更新
            (car rst))　　　　　　　　　　　　　　　　　　　　　　　;(car rst)がプロトタイプ宣言のobj
          
          (begin                                              ;プロトタイプ宣言のパラメータのobjリスト
            (set! obj-env (add-new-level-to-env obj-env))     ;新しいレベルを環境に追加
            (map (lambda (x)                                  ;パラメータを環境に追加していく
                   (begin (let ((rst (add-ref-env obj-env x (stx:func-prototype-pos ast))))
                            (set! obj-env (cdr rst)))))
                 proto-declarator-objlist)
            (set! obj-env (pop-top-env obj-env))              ;最後に新しいレベルの環境をpop
            proto-declarator-objlist)
          (stx:func-prototype-pos ast))))
      
      ;stx:func-definition　funのobj構造体のtypeはconsセルで,carは関数自体の型情報,cdrは引数の型情報のリストが入っている（consというか結局はリスト）
      ((stx:func-definition? ast)
       (let* ((fun-declarator-objlist                ;引数のobj構造体のリスト
               (if (null? (stx:func-definition-declarator ast))
                   `()
                   (map (lambda (x) (var-decl-toobj x (+ lev 1) parm)) (stx:func-definition-declarator ast))))
              (fun-declarator-obj-typelist           ;引数のobjのtypeのリスト
               (if (null? fun-declarator-objlist)
                   `()
                   (map (lambda (x) (obj-type x)) fun-declarator-objlist)))
              (def-obj　　　　　　　　　　　　　　　　　　　;funのobj構造体.typeに引数の型情報が必要なので追加.具体的にはfunのobj構造体のtypeに引数のtypeリストをappendする.
                (let ((fd-obj (var-decl-toobj (stx:func-definition-var-decl ast) lev fun)))
                  (obj (obj-name fd-obj) (obj-lev fd-obj) (obj-kind fd-obj) (append (list (obj-type fd-obj)) fun-declarator-obj-typelist) 0)))
              
              
              (func-def-rst
               (stx:func-definition
                (let* ((rst (add-ref-env  obj-env def-obj (stx:func-definition-pos ast)))) ;関数定義のobjを環境に追加した結果をrstに入れる
                  (begin
                    (set! obj-env (cdr rst))　　　　　　　　　　        ;環境を更新
                    (set! func-def-list                              ;型検査で使うfunc-def-listを更新
                          (append
                           (list (append
                                  (list (obj-name (car rst)))
                                  (obj-type (car rst)))) func-def-list))
                    (car rst)))                                      ;(car rst)が関数定義のobj
                
                (begin                                               ;関数定義のパラメータのobjリスト
                  (set! obj-env (add-new-level-to-env obj-env))      ;新しいレベルを環境に追加
                  (map (lambda (x)                                   ;パラメータを環境に追加していく
                         (begin (let ((rst (add-ref-env obj-env x (stx:func-definition-pos ast))))
                                  (set! obj-env (cdr rst)))))
                       fun-declarator-objlist)
                  fun-declarator-objlist)
                (collect-object-main (stx:func-definition-statement ast) (+ lev 1));compound-stmtの中身を展開（オブジェクト情報を収集）
                (stx:func-definition-pos ast))))
         (begin
           (set! obj-env (pop-top-env obj-env))                      ;最後に新しいレベルの環境をpop
           func-def-rst)))
      
      ;stx:compound-stmt
      ((stx:compound-stmt? ast)
       (begin
         (set! obj-env (add-new-level-to-env obj-env))        　　　　;新しいレベルを環境に追加
         (let ((rst　　　　　　　　　　　　　　　　　　　　　　　　　　　　 ;rstが評価する値 compound-stmtの中をチェック
                (stx:compound-stmt
                 (if (null? (stx:compound-stmt-declaration-list-opt ast))
                   `()
                  (map (lambda (x) (collect-object-main x (+ lev 1))) (stx:compound-stmt-declaration-list-opt ast)))
               (if (null? (stx:compound-stmt-statement-list-opt ast))
                   `()
                  (map (lambda (x) (collect-object-main x (+ lev 1))) (stx:compound-stmt-statement-list-opt ast)))
               (stx:compound-stmt-pos ast))))
           (set! obj-env (pop-top-env obj-env))　　　　　　　　　　　　 ;最後に新しいレベルの環境をpop
           rst)))
      ;stx:expression
      ((stx:expression? ast)
       (stx:expression
        (stx:expression-iskakko ast)
        (map (lambda (x) (collect-object-main x lev)) (stx:expression-explist ast))
        (stx:expression-pos ast)))
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
      ((stx:var-decl? ast) (error "tree-walk-error:var-decl"))
      ;stx:lit-exp
      ((stx:lit-exp? ast) ast)
      ;stx:var-exp
      ;環境からnameが一致するものを探す
      ((stx:var-exp? ast)
       (let* ((dummy-obj (obj (stx:var-exp-tgt ast) 0 var `() 0)) ;seach-envはobj構造体との一致を調べるので検索用にvar-expをobj構造体に変換 name以外のメンバはダミー
              (search-rst (search-env-by-obj-name obj-env dummy-obj)))         ;環境から検索
         (if search-rst                                                                
             (if (or (equal? var (obj-kind search-rst)) (equal? parm (obj-kind search-rst))) ;見つかった場合,varかparmとして宣言されているか確認
                search-rst                                                                     ;varかparmであれば見つかったobjをかえす
               (error (format
                              "~a:~a: stx:var-exp: ~a is defined as function-definition or function-prototype not as variable."
                              (position-line (stx:var-exp-pos ast))
                              (position-col (stx:var-exp-pos ast))
                              (stx:var-exp-tgt ast))))                                            ;funやprotoとして定義されている
            (error (format
                    "~a:~a: stx:var-exp: ~a is undefined."
                    (position-line (stx:var-exp-pos ast))
                    (position-col (stx:var-exp-pos ast))
                    (stx:var-exp-tgt ast))))))                                          ;見つからなかった場合,未定義
      ;stx:func-call-exp
      ((stx:funccall-exp? ast)
       (stx:funccall-exp
        ;stx:funccall-exp-tgt
        (let* ((dummy-obj (obj (stx:funccall-exp-tgt ast) 0 fun `() 0)) ;seach-envはobj構造体との一致を調べるので検索用にobj構造体をつくる name以外のメンバはダミー
               (search-rst (search-env-by-obj-name obj-env dummy-obj)))
          (if search-rst
              (if (or (equal? fun (obj-kind search-rst)) (equal? proto (obj-kind search-rst)))  ;見つかった場合,funまたはprotoとして宣言されているか確認
                  search-rst                                                                      ;funまたはobjであれば見つかったobjをかえす
                  (error (format
                          "~a:~a: stx:funccall-exp ~a is defined as variable not as function-definition or function-prototype."
                          (position-line (stx:funccall-exp-pos ast))
                          (position-col (stx:funccall-exp-pos ast))
                          (stx:funccall-exp-tgt ast))))                                          ;varして定義されている
              (error (format
                      "~a:~a: stx:funccall-exp ~a is undefined."
                      (position-line (stx:funccall-exp-pos ast))
                      (position-col (stx:funccall-exp-pos ast))
                      (stx:funccall-exp-tgt ast)))))                                            ;見つからなかった場合,未定義
        ;stx:funccall-exp-paramlist
        (collect-object-main (stx:funccall-exp-paramlist ast) lev)
        (stx:funccall-exp-pos ast)))
       ;else
      (else ast)))
  
  ;ここから開始
  (begin
    ;暗黙のprint関数のプロトタイプ宣言を追加.
    (let ((print-proto-added-ast (append `(,(stx:func-prototype (stx:var-decl 'void 'print #f #f `()) (list (stx:var-decl 'int 'n #f #f `())) `())) ast)))
      (set! func-def-list initial-func-def-list)
      (set! obj-env initial-env)
      (collect-object-main print-proto-added-ast 0))))

;環境のスタックを処理するための関数
;環境の実装方法は、レベル1つにつき1つのlambda式を作り、そのlambda式のリストが環境である。
;パラメータ定義や関数

;lambda式のリストである環境からobj-nameが重複するものを検索をかける
;返り値はみつかったobjまたは#f
(define (search-env-by-obj-name env tgt-obj)
    (if (= (length env) 1)
       ((car env) tgt-obj) 
       (if ((car env) tgt-obj)
          ((car env) tgt-obj)
          (search-env-by-obj-name (cdr env) tgt-obj))))

;環境からobjを参照し,なければ環境の先頭にobjを追加
(define (add-ref-env env newobj pgpos) 
  ;newobjとnameが一致するobjを環境から検索
  (let* ((search-rst (search-env-by-obj-name env newobj)))
    ;環境に新しく追加するオブジェクトの種類によって変更
    (cond
      ;新しいobjが変数宣言のとき
      ((equal? (obj-kind newobj) var)
       (if search-rst
           ;既にnameが一致するオブジェクトが存在するとき
           (let ((error-flg
                  ;検索されたオブジェクトの種類によって処理を分岐
                  (cond
                    ;関数宣言または関数プロトタイプ宣言で既にあるとき,その関数宣言のレベルが0であればエラー
                    ((or (equal? (obj-kind search-rst) fun) (equal? (obj-kind search-rst) proto))
                     (if (equal? 0 (obj-lev newobj))
                         (error (format 
                                 "~a:~a: stx:declaration ~a is already defined as function-definition."
                                 (position-line pgpos)
                                 (position-col pgpos)
                                 (obj-name newobj)))
                         #f))
                    ;変数宣言で既にあるとき,新しいオブジェクトとレベルが同じならエラー
                    ((equal? (obj-kind search-rst) var)
                     (if (equal? (obj-lev newobj) (obj-lev search-rst))
                         (error (format
                                 "~a:~a: stx:declaration ~a is already defined as variable in the same level."
                                 (position-line pgpos)
                                 (position-col pgpos)
                                 (obj-name newobj)))
                         #f))
                    ;関数パラメータで既にあるとき,警告を表示して登録
                    ((equal? (obj-kind search-rst) parm)
                     (begin (eprintf "warning: ~a:~a: stx:declaration ~a is already defined as function-param."
                                     (position-line pgpos)
                                     (position-col pgpos)
                                     (obj-name newobj))
                            #f))
                    (else (begin (display search-rst) (error "unknown obj-kind1"))))))
             (if error-flg
                 (error "ここにはこない")
                 (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
                   (if (null? (cdr env))
                       (cons newobj `(,top-env))
                       (cons newobj `(,top-env ,@(cdr env)))))))
           ;既にnameが一致するオブジェクトが存在しないとき→登録
           (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
             (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
      ;新しいobjが関数パラメータ宣言のとき
      ((equal? (obj-kind newobj) parm)
       (if search-rst
           ;パラメータ二重宣言ならエラー
           (if (equal? (obj-kind search-rst) parm)
               (error (format
                       "~a:~a: stx:function-prototype-parm ~a is already defined as the function-param in the same function."
                       (position-line pgpos)
                       (position-col pgpos)
                       (obj-name newobj)))
               ;登録
               (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
                 (if (null? (cdr env))
                     (cons newobj `(,top-env))
                     (cons newobj `(,top-env ,@(cdr env))))))
           ;既にnameが一致するオブジェクトが存在しないとき→登録
           (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
             (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
      ;新しいobjが関数プロトタイプ宣言のとき type(関数の型と引数の型情報）が一致してるかを調べ、一致しないならエラー 一致なら登録もせず元の環境をかえす
      ((equal? (obj-kind newobj) proto)
       (if search-rst
           ;既にnameが一致するオブジェクトが存在するとき
           (cond
             ;既に変数として存在するとき
             ((equal? (obj-type search-rst) var)
              (error (format
                      "~a:~a stx:function-prototype: ~a is already defined as var."
                      (position-line pgpos)
                      (position-col pgpos)
                      (obj-name newobj))))
             ;既に関数として存在するとき
             ((equal? (obj-type search-rst) fun)
              (if (equal? (obj-type newobj) (obj-type search-rst))
                  (cons newobj env)                    ;型情報が既に存在するものと一致する場合、更新せずにかえす
                  (error (format
                          "~a:~a stx:function-prototype: ~a: different-type-parameter-function is already defined."
                          (position-line pgpos)
                          (position-col pgpos)
                          (obj-name newobj)))))
             ;既にプロトタイプ宣言として存在するとき
             ((equal? (obj-type search-rst) proto)
              (if (equal? (obj-type newobj) (obj-type search-rst))
                  (cons newobj env)                    ;型情報が既に存在するものと一致する場合、更新せずにかえす
                  (error (format
                          "~a:~a stx:function-prototype: ~a: different-type-parameter-prototype is already defined."
                          (position-line pgpos)
                          (position-col pgpos)
                          (obj-name newobj)))))
             ;既にパラメータとして存在しているとき
             ((equal? (obj-kind search-rst) parm);たぶんここにはこない
              (error (format
                      "~a:~a: stx:function-prototype: ~a is already defined as function-param."
                      (position-line pgpos)
                      (position-col pgpos)
                      (obj-name newobj))))
             (else (error "unknown-obj-kind2")))
           ;既にnameが一致するオブジェクトが存在しないとき→登録
           (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
             (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
      
      ;新しいobjが関数定義のとき
      ((equal? (obj-kind newobj) fun)
       (if search-rst
           ;既にnameが一致するオブジェクトが存在するとき
           (cond
             ((equal? (obj-kind search-rst) var)
              (error (format
                      "~a:~a: stx:function-definition: ~a is already defined as variable."
                      (position-line pgpos)
                      (position-col pgpos)
                      (obj-name newobj))))
             ((equal? (obj-kind search-rst) parm);たぶんここにはこない
              (error (format
                      "~a:~a: stx:function-definition: ~a is already defined as function-param."
                      (position-line pgpos)
                      (position-col pgpos)
                      (obj-name newobj))))
             ((equal? (obj-kind search-rst) fun)
              (error (format
                      "~a:~a: stx:function-deinition: ~a is already defined as function. function-definitin is duplicate."
                      (position-line pgpos)
                      (position-col pgpos)
                      (obj-name newobj))))
             ((equal? (obj-kind search-rst)  proto)
              ;型情報が一致しているかどうかチェック
              (if (equal? (obj-type newobj) (obj-type search-rst))
                  ;登録
                  (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
                    (if (null? (cdr env))
                        (cons newobj `(,top-env))
                        (cons newobj `(,top-env ,@(cdr env)))))
                  ;エラー
                  (error (format
                          "~a:~a: stx:function-definition: ~a: The types of parameters does not match with function-prototype which is already defined."
                          (position-line pgpos)
                          (position-col pgpos)
                          (obj-name newobj)))))
                          
             (else (error "unknown obj-kind3")))
           
           ;既にnameが一致するオブジェクトが存在しないとき→登録
           (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
             (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
      (else (error "unknown obj-kind4")))))

;新しく環境のリストの先頭にlambda式を追加する
(define (add-new-level-to-env env)
  (append initial-env env))

;先頭の環境をpopする
(define (pop-top-env env)
  (cdr env))

;型検査用.型情報を扱う構造体 こっちのほうが扱いやすい
(struct type-struct (type isarray ispointer ispointerpointer) #:transparent)
;型検査用.return文にて、return文がある関数の型と返り値の型の整合性をしらべるためのもの.中身はtype-struct構造体.
(define now-func-type-struct 'dummy)
;型検査用便利関数
;int型かどうかチェックする関数 isarrayは関係ない
  (define (isint tgt-struct)
    (and (equal? 'int (type-struct-type tgt-struct)) (equal? #f (type-struct-ispointer tgt-struct)) (equal? #f (type-struct-ispointerpointer tgt-struct))))
  ;int*型かどうかチェックする関数 isarrayは関係ない
  (define (isint-pointer tgt-struct)
    (and (equal? 'int (type-struct-type tgt-struct)) (equal? #t (type-struct-ispointer tgt-struct)) (equal? #f (type-struct-ispointerpointer tgt-struct))))
  ;int**型かどうかチェックする関数 isarrayは関係ない
  (define (isint-pointerpointer tgt-struct)
    (and (equal? 'int (type-struct-type tgt-struct)) (equal? #t (type-struct-ispointer tgt-struct)) (equal? #t (type-struct-ispointerpointer tgt-struct))))
  ;等しい型かどうか調べる関数 isarrayは関係ない
  (define (isequal-type type1 type2)
    (and (equal? (type-struct-type type1) (type-struct-type type2)) (equal? (type-struct-ispointer type1) (type-struct-ispointer type2)) (equal? (type-struct-ispointerpointer type1) (type-struct-ispointerpointer type2))))
  ;objをうけとり型に問題がないか調べる
  (define (obj-check tgt-obj)
    (let* ((tgt-obj-fixed ;obj-kindがfunまたはprotoのときは,typeに引数情報を含んでいるので,carで一番はじめのをとりだす
            (if (or (equal? fun (obj-kind tgt-obj)) (equal? proto (obj-kind tgt-obj)))
                (obj (obj-name tgt-obj) (obj-lev tgt-obj) (obj-kind tgt-obj) (car (obj-type tgt-obj)) 0)
                tgt-obj))
           (obj-type-struct (conv-typelist-to-struct (obj-type tgt-obj-fixed)))) 
           ;(hoge (display obj-type-struct));for debug
          
      (if (or (equal? var(obj-kind tgt-obj-fixed)) (equal? parm (obj-kind tgt-obj-fixed)))
          ;パラメータまたは変数のときはvoidはダメ.
          (if (equal? 'void (type-struct-type obj-type-struct))
              (begin (eprintf"obj-error : ~a: the type \"void\" is allowed only as function."
                             (obj-name tgt-obj))
                     (error ""))
              #t)
          ;プロトタイプ宣言または関数定義のとき,typeがvoidであればポインタでもなく配列でもなければならない
          (if (equal? 'void (type-struct-type obj-type-struct))
              (if (and (equal? #f (type-struct-isarray obj-type-struct))
                       (equal? #f (type-struct-ispointer obj-type-struct)))
                  #t
                  (begin (eprintf "obj-error : ~a: the type \"void-array\", \"void-pointer\",and  \"void-pointer-array\" is not allowd as function type."
                                  (obj-name tgt-obj))
                         (error "")))
              #t))))
;obj-typeのリストから型情報の構造体に変換する便利関数
(define (conv-typelist-to-struct typelist)
  (let* ((rst (type-struct 'dummy #f #f #f)))
    (if (equal? 'array (car typelist))
        (begin
          (set! rst (type-struct (type-struct-type rst) #t (type-struct-ispointer rst) (type-struct-ispointerpointer rst)))
          (let ((arraytype (cadr typelist)))
            (if (list? arraytype)
                ;int*の配列->int**
                (set! rst (type-struct (cadr arraytype) (type-struct-isarray rst) #t #t))
                ;intの配列-> int*
                (set! rst (type-struct arraytype (type-struct-isarray rst) #t #f)))
            (set! rst (type-struct (type-struct-type rst) (type-struct-isarray rst) (type-struct-ispointer rst) (type-struct-ispointerpointer rst)))
            rst))
        (if (equal? (car typelist) 'pointer)
            (begin
              (set! rst (type-struct (cadr typelist) (type-struct-isarray rst) #t (type-struct-ispointerpointer rst)))
              rst)
            (begin
              (set! rst (type-struct (car typelist) (type-struct-isarray rst) (type-struct-ispointer rst) (type-struct-ispointerpointer rst)))
              rst)))))
;型検査をする関数 collect-object関数によりobj構造体をうめこまれたあとの抽象構文木をうけとって型検査をする
;返り値は#tまたは型のリスト 型違反があれば即時エラーを出力する.
;#tはwell-typedを意味する.
(define (type-check ast) 
  ;抽象構文木の型検査をする.expression構造体の子になるものに関しては別のtype-check-exp関数を利用する.
  ;返り値は#tまたは即時エラー
  (define (type-check-main ast)
    (cond
      ;空の文
      ((null? ast) #t)
      ;obj
      ((obj? ast) (obj-check ast))
      ;すべてエラーなく通過した場合すべて#tを返すので返り値は#tになる
      ((list? ast) (if (= 1 (length ast)) (type-check-main (car ast)) (and (type-check-main (car ast) ) (type-check-main (cdr ast)))))
      ;stx:declaration
        ;declistの中のobjがすべて正しいか調べる
      ((stx:declaration? ast)
         (type-check-main (stx:declaration-declist ast)))
      ;stx:func-prototype
      ;プロトタイプ宣言自身のobjとパラメータリストがすべて#tならOK
      ((stx:func-prototype? ast)
       (begin
          (type-check-main (stx:func-prototype-var-decl ast))
          (type-check-main (stx:func-prototype-declarator ast))
           #t))
      ;stx:func-definition
      ;関数定義自身のobjとパラメータリストと関数定義の複文本体がすべて#tならOK
      ((stx:func-definition? ast)
       (begin
         (let ((rst (type-check-main (stx:func-definition-var-decl ast)))) ;関数定義自身のobjがwell-typedがチェック.結果をrstにいれる
           (begin
             (set! now-func-type-struct
                   (conv-typelist-to-struct
                    (obj-type (let ((tgt-obj (stx:func-definition-var-decl ast)))
                                (obj (obj-name tgt-obj) (obj-lev tgt-obj) (obj-kind tgt-obj) (car (obj-type tgt-obj)) 0)))))　;現在の関数の型をnow-func-type-structに入れる.return文でいまの関数の型を調べるため.
             rst))
         (type-check-main (stx:func-definition-declarator ast))
         (type-check-main (stx:func-definition-statement ast))
       #t))
         
      
      ;stx:compound-stmt
      ((stx:compound-stmt? ast)
       (if (and
            (type-check-main (stx:compound-stmt-declaration-list-opt ast))
            (type-check-main (stx:compound-stmt-statement-list-opt ast)))
           #t
           #f))
      ;stx:expression
      ;ここにくるのは文としての式のときのみのはず. type-check-expでエラーがでなければOK
      ((stx:expression? ast) (let ((exp-type (type-check-exp ast))) #t)) 
      ;stx:while-stmt
      ((stx:while-stmt? ast)
       ;まずtestの型がintであるかチェック
       (if (isint (type-check-exp (stx:while-stmt-test ast)))
           ;testの型がintであればbodyがwell-typedか調べる
           (if (type-check-main (stx:while-stmt-body ast))
               #t
               #f)
           ;testの型がintでなければエラー
           (error (format "~a:~a: stx:while-stmt: the type of while-test or for-test must be int"
                          (position-line (stx:while-stmt-pos ast))
                          (position-col (stx:while-stmt-pos ast))))))
      
      ;stx:if-else-stmt
      ((stx:if-else-stmt? ast)
       ;まずtestの型がintであるかチェック
       (if (isint (type-check-exp (stx:if-else-stmt-test ast)))
           ;testの型がintであれば2つのbodyがwell-typedか調べる
           (if (and
                (type-check-main (stx:if-else-stmt-tbody ast))
                (type-check-main (stx:if-else-stmt-ebody ast)))
               #t
               #f)
           ;testの型がintでなければエラー
           (error (format "~a:~a: stx:if-else-stmt the type of if-test must be int"
                          (position-line (stx:if-else-stmt-pos ast))
                          (position-col (stx:if-else-stmt-pos ast))))))
      ;stx:return-stmt
      ;現在いる関数の型と一致しなければエラー
      ((stx:return-stmt? ast)
       (if (equal? 'void (type-struct-type now-func-type-struct))
           ;void型のときはnullでなければエラー
           (if (null? (stx:return-stmt-var ast))
               #t
               (error (format "~a:~a: stx:return-stmt: the function whose type is void must not have return-val.")
                      (position-line (stx:return-stmt-pos ast))
                      (position-col (stx:return-stmt-pos ast))))
           ;他の型のときは型が一致していればOK
           (if (isequal-type now-func-type-struct (type-check-exp (stx:return-stmt-var ast)))
               #t
               (error (format
                       "~a:~a: stx:return-stmt: the return-val-list-types doesn't match the function-definition."
                       (position-line (stx:return-stmt-pos ast))
                       (position-col (stx:return-stmt-pos ast)))))))
      
      (else (begin (display ast) (error "tree-walk-error:type-check")))))
  
  
  (type-check-main ast))

;式の型検査をする関数.返り値はtype-struct構造体を用いる.
(define (type-check-exp exp)
  (cond
    ;stx:expression
    ;explistの式を順番に評価していき,最後の式の型をかえす. e1,e2..enの型はenの型と同じ
    ((stx:expression? exp)
     (let ((exp-type-list (map (lambda (x) (type-check-exp x)) (stx:expression-explist exp))))
       (car (reverse exp-type-list))))
    ;stx:assign-stmt
    ;左辺と右辺に同じ型があればそれらと同じ型がつく
    ((stx:assign-stmt? exp)
     (let ((left-type (type-check-exp (stx:assign-stmt-var exp)))
           (right-type (type-check-exp (stx:assign-stmt-src exp))))
       (if (not (or (stx:deref-exp? (stx:assign-stmt-var exp)) (obj? (stx:assign-stmt-var exp))))
           (error (format "~a:~a: the left operand type of assign-statement is incorrect."
                          (position-line (stx:assign-stmt-pos exp))
                          (position-col (stx:assign-stmt-pos exp))))
           (if (isequal-type left-type right-type)
               left-type
               (error (format "the left operand type and the right operand type of assign-statement be same."))))))
    ;stx:funccall-exp
    ;collec-objectで木を巡回したときに収集した関数の情報から,引数の個数と型が一致しているかをしらべる
    ((stx:funccall-exp? exp)
     (let ((search-rst (search-func-by-name (obj-name (stx:funccall-exp-tgt exp)))))
       (if (equal? #f search-rst)
           (error (format "~a:~a stx:funccall-exp: call undefined function \"~a\""
                                  (position-line (stx:funccall-exp-pos exp))
                                  (position-col (stx:funccall-exp-pos exp))
                                  (stx:funccall-exp-tgt exp)))
           (let ((funccall-exptypelist  ;関数呼び出しで使用されている式の型リスト
                  (if (null? (stx:funccall-exp-paramlist exp))
                      `()
                      (map (lambda (x) (type-check-exp x)) (stx:funccall-exp-paramlist exp))))
                 (funcdef-exptypelist    ;関数定義で定められている式の型リスト
                  (if (equal? 2 (length search-rst))
                      `()                         ;nameと関数本体の型しかsearch-rstにない,つまり引数が0個のときはnullリスト
                      (map (lambda (x) (conv-typelist-to-struct x)) (cddr search-rst)))))
             ;引数の個数が一致しているかしらべる
             (if (equal? (length funccall-exptypelist) (length funcdef-exptypelist))
                 ;引数の型が全て等しいかしらべる
                 (if (let ((isok #t))
                       (begin
                         (map (lambda (x y) (set! isok (and isok (isequal-type x y)))) funccall-exptypelist funcdef-exptypelist)
                         isok))
                     ;関数呼び出しでの引数の型と関数定義された引数の型が全て等しいのでOK.かえす型は関数自体の型.search-rstの2個目の要素
                     (conv-typelist-to-struct (cadr search-rst))
                     ;エラー
                     (error (format "~a:~a: stx:funccall-exp: the type-list of funccall-parmlist doesn't match with the type-list of function-definition"
                             (position-line (stx:funccall-exp-pos exp))
                             (position-col (stx:funccall-exp-pos exp)))))
                 ;引数の個数が一致しなかった
                 (error (format "~a:~a: stx:funccall-exp: the number of type-lists of funccall-parmlist doesn't match with the number of type-lists of function-definition"
                             (position-line (stx:funccall-exp-pos exp))
                             (position-col (stx:funccall-exp-pos exp)))))))))
    ;stx:aop-exp
    ((stx:aop-exp? exp)
     (let
         ((left-type (type-check-exp (stx:aop-exp-left exp)))
          (right-type (type-check-exp (stx:aop-exp-right exp))))
       (cond
         ;+演算
         ((equal? '+ (stx:aop-exp-op exp))
          (cond
            ;int + int
            ((and (isint left-type) (isint right-type)) (type-struct 'int #f #f #f))
            ;int* + int または int + int*
            ((or (and (isint-pointer left-type) (isint right-type))
                 (and (isint-pointer right-type) (isint left-type)))
             (type-struct 'int #f #t #f))
            ;int** + int または int + int**
            ((or (and (isint-pointerpointer left-type) (isint right-type))
                 (and (isint-pointerpointer right-type) (isint left-type)))
             (type-struct 'int #f #t #t))
            ;エラー
            (else (error (format "~a:~a: stx:aop-exp: the type of operand of \"+\" operation is incorrect."
                             (position-line (stx:aop-exp-pos exp))
                             (position-col (stx:aop-exp-pos exp)))))))
         ;-演算
         ((equal? '- (stx:aop-exp-op exp))
          (cond
            ;int - int
            ((and (isint left-type) (isint right-type)) (type-struct 'int #f #f #f))
            ;int* - int
            ((and (isint-pointer left-type) (isint right-type)) (type-struct 'int #f #t #f))
            ;int** - int
            ((and (isint-pointerpointer left-type) (isint right-type)) (type-struct 'int #f #t #t))
            ;エラー
            (else (error (format "~a:~a: stx:aop-exp: the type of operand of \"-\" operation is incorrect."
                             (position-line (stx:aop-exp-pos exp))
                             (position-col (stx:aop-exp-pos exp)))))))
         ;*演算
         ((equal? '* (stx:aop-exp-op exp))
          (if (and (isint left-type) (isint right-type))
              (type-struct 'int #f #f #f)
              (error (format "~a:~a: stx:aop-exp: the type of operand of \"*\" operation is incorrect."
                             (position-line (stx:aop-exp-pos exp))
                             (position-col (stx:aop-exp-pos exp))))))
         ;/演算
         ((equal? '/ (stx:aop-exp-op exp))
          (if (and (isint left-type) (isint right-type))
              (type-struct 'int #f #f #f)
              (error (format "~a:~a: stx:aop-exp: the type of operand of \"/\" operation is incorrect."
                             (position-line (stx:aop-exp-pos exp))
                             (position-col (stx:aop-exp-pos exp))))))
         (else (error "unknown operand")))))
    ;stx;rop-exp
    ((stx:rop-exp? exp)
     (let ((left-type (type-check-exp (stx:rop-exp-left exp)))
           (right-type (type-check-exp (stx:rop-exp-right exp))))
       (if (isequal-type left-type right-type)
           (type-struct 'int #f #f #f)
           (error (format "~a:~a: stx:rop-exp: the types of left-operand and right-operand must be same."
                             (position-line (stx:rop-exp-pos exp))
                             (position-col (stx:rop-exp-pos exp)))))))
    ;stx:logical-and-or-exp
    ((stx:logical-and-or-expr? exp)
     (if (and (isint (type-check-exp (stx:logical-and-or-expr-log1 exp))) (isint (type-check-exp (stx:logical-and-or-expr-log2 exp))))
         (type-struct 'int #f #f #f)
         (error (format "~a:~a: stx:logical-and-or-exp: the type of operand of \"&&\" or \"||\" expression is incorrect."
                             (position-line (stx:logical-and-or-expr-pos exp))
                             (position-col (stx:logical-and-or-expr-pos exp))))))
    ;stx:addr-exp &
    ((stx:addr-exp? exp)
     (if (isint (type-check-exp (stx:addr-exp-var exp)))
         (type-struct 'int #f #t #f)
         (error (format "~a:~a: stx:addr-exp: the type of operand of addr-exp must be int"
                             (position-line (stx:addr-exp-pos exp))
                             (position-col (stx:addr-exp-pos exp))))))
    ;stx:deref-exp *
    ((stx:deref-exp? exp)
     ;*e eがint*ならintつける
     (cond
       ((isint-pointer (type-check-exp (stx:deref-exp-arg exp)))
        (type-struct 'int #f #f #f))
       ;*e eがint**ならint*つける
       ((isint-pointerpointer (type-check-exp (stx:deref-exp-arg exp)))
        (type-struct 'int #f #t #f))
       (else (error (format "~a:~a: stx:deref-exp: the type of operand of deref-exp is incorrect."
                             (position-line (stx:deref-exp-pos exp))
                             (position-col (stx:deref-exp-pos exp)))))))
    ;stx:lit-exp
    ((stx:lit-exp? exp) (type-struct 'int #f #f #f))
    ;obj
    ((obj? exp) (begin (obj-check exp) (conv-typelist-to-struct (obj-type exp))))
    (else (begin (display exp) (error "tree-walk-error:type-check-exp")))))
;意味解析のメイン関数 抽象構文木をうけとり,オブジェクト情報の収集をしてオブジェクト情報を埋め込んだ抽象構文木をかえす。.
;同時に,二重定義や未定義変数,関数の使用してないかのチェックと型検査を行う.
(define (semantic-analysis-ast ast)
  (let ((object-collected-ast (collect-object ast)))
    (begin
      (type-check object-collected-ast)
      object-collected-ast)))

(define (semantic-analysis-file filename)
  (let* ((ast (parser:parse-file filename))
         (object-collected-ast (collect-object ast)))
    (begin
      (type-check object-collected-ast)
      object-collected-ast)))
                                