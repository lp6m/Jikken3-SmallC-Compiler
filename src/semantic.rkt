#lang racket
(provide (all-defined-out))
(require (prefix-in parser: "parser.rkt")
         (prefix-in stx:    "parser-syntax.rkt"))
;オブジェクト情報をもつ構造体
(struct obj (name lev kind type) #:transparent)
;kind = var parm fun proto 
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
    (obj (stx:var-decl-id decl) lev kind (var-decl-totype decl)))
  
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
             (let* ((newobj (var-decl-toobj x lev 'var))
                    (rst (add-ref-env obj-env newobj)))　　　　　　　　　　　;環境にオブジェクトを追加した結果をrstに入れる
               (begin (set! obj-env (cdr rst)) (car rst)))) declist))　　 ;環境を更新
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
                   (map (lambda (x) (obj-type x)) proto-declarator-objlist)))
               (proto-obj                                  ;protoのobj構造体.typeに引数の型情報が必要なので追加.具体的にはprotoのobj構造体のtypeに引数のtypeリストをappendする.
                (let ((fp-obj (var-decl-toobj (stx:func-prototype-var-decl ast) lev 'proto)))
                  (obj (obj-name fp-obj) (obj-lev fp-obj) (obj-kind fp-obj) (append (list (obj-type fp-obj)) proto-declarator-obj-typelist)))))
         ;返り値はここ
         (stx:func-prototype
            (let* ((rst (add-ref-env  obj-env proto-obj)))　    ;プロトタイプ宣言のobjを環境に追加した結果をrstに入れる
              (set! obj-env (cdr rst))　　　　　　　　　　                 ;環境を更新
              (car rst))　　　　　　　　　　　　　　　　　　　　　　　　　　　　 ;(car rst)がプロトタイプ宣言のobj
            
            (begin                                                               ;プロトタイプ宣言のパラメータのobjリスト
              (set! obj-env (add-new-level-to-env obj-env))  ;新しいレベルを環境に追加
              (map (lambda (x)                                            ;パラメータを環境に追加していく
                     (begin (let ((rst (add-ref-env obj-env x)))
                              (set! obj-env (cdr rst)))))
                  proto-declarator-objlist)
              (set! obj-env (pop-top-env obj-env))                ;最後に新しいレベルの環境をpop
              proto-declarator-objlist)
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
                  (map (lambda (x) (obj-type x)) fun-declarator-objlist)))
              (def-obj　　　　　　　　　　　　　　　　　　　;funのobj構造体.typeに引数の型情報が必要なので追加.具体的にはfunのobj構造体のtypeに引数のtypeリストをappendする.
                (let ((fd-obj (var-decl-toobj (stx:func-definition-var-decl ast) lev 'fun)))
                  (obj (obj-name fd-obj) (obj-lev fd-obj) (obj-kind fd-obj) (append (list (obj-type fd-obj)) fun-declarator-obj-typelist))))
                
         
         (func-def-rst
          (stx:func-definition
           (let* ((rst (add-ref-env  obj-env def-obj)))       　;関数定義のobjを環境に追加した結果をrstに入れる
             (begin
               (set! obj-env (cdr rst))　　　　　　　　　　               ;環境を更新
               (set! func-def-list                                           ;型検査で使うfunc-def-listを更新
                    (append
                     (list (append
                            (list (obj-name (car rst)))
                            (obj-type (car rst)))) func-def-list))
             (car rst)))                                                         ;(car rst)が関数定義のobj
           
           (begin                                                               ;関数定義のパラメータのobjリスト
             (set! obj-env (add-new-level-to-env obj-env))  ;新しいレベルを環境に追加
             (map (lambda (x)                                            ;パラメータを環境に追加していく
                    (begin (let ((rst (add-ref-env obj-env x)))
                             (set! obj-env (cdr rst)))))
                  fun-declarator-objlist)
             
             fun-declarator-objlist)
           (collect-object-main (stx:func-definition-statement ast) (+ lev 1));compound-stmtの中身を展開（オブジェクト情報を収集）
           (stx:func-definition-pos ast))))
         (begin
           (set! obj-env (pop-top-env obj-env))                ;最後に新しいレベルの環境をpop
           func-def-rst)))
           
      ;stx:compound-stmt
      ((stx:compound-stmt? ast)
       (begin
         (set! obj-env (add-new-level-to-env obj-env))        　　　　　　　　;新しいレベルを環境に追加
         (let ((rst　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　;rstが評価する値 compound-stmtの中をチェック
                (stx:compound-stmt
               (if (null? (stx:compound-stmt-declaration-list-opt ast))
                  `()
                  (map (lambda (x) (collect-object-main x (+ lev 1))) (stx:compound-stmt-declaration-list-opt ast)))
               (if (null? (stx:compound-stmt-statement-list-opt ast))
                  `()
                  (map (lambda (x) (collect-object-main x (+ lev 1))) (stx:compound-stmt-statement-list-opt ast)))
               (stx:compound-stmt-pos ast))))
           (set! obj-env (pop-top-env obj-env))　　　　　　　　　　　　　　　　　　;最後に新しいレベルの環境をpop
           rst)))
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
      ((stx:var-decl? ast) (error "var-declになることはないはずなので木の巡回エラーです"))
      ;stx:lit-exp
      ((stx:lit-exp? ast) ast)
      ;stx:var-exp
      ;環境からnameが一致するものを探す
      ((stx:var-exp? ast)
       (let* ((dummy-obj (obj (stx:var-exp-tgt ast) 0 'var `())) ;seach-envはobj構造体との一致を調べるので検索用にvar-expをobj構造体に変換 name以外のメンバはダミー
              (search-rst (search-env-by-obj-name obj-env dummy-obj)))         ;環境から検索
         (if search-rst                                                                
            (if (or (equal? 'var (obj-kind search-rst)) (equal? 'parm (obj-kind search-rst)))                          ;見つかった場合,varかparmとして宣言されているか確認
               search-rst                                                                ;varかparmであれば見つかったobjをかえす
               (begin (display ast) (error "その変数は変数またはパラメータ以外で宣言されています")))　　　　　　　 ;funやprotoとして定義されている
            (begin (display ast) (error "その変数は未定義です")))))                                   ;見つからなかった場合,未定義
      ;stx:func-call-exp
      ((stx:funccall-exp? ast)
       (let* ((dummy-obj (obj (stx:funccall-exp-tgt ast) 0 'fun `())) ;seach-envはobj構造体との一致を調べるので検索用にobj構造体をつくる name以外のメンバはダミー
              (search-rst (search-env-by-obj-name obj-env dummy-obj)))
         (if search-rst
              (if (equal? 'fun (obj-kind search-rst))                               ;見つかった場合,funとして宣言されているか確認 （ちなみに,protoである可能性はない。BNF的に,かならずプロトタイプ宣言のあとに関数定義がある為。）
                 search-rst                                                                    ;funであれば見つかったobjをかえす
                 (error "関数以外で宣言されています"))                                 ;varやprotoとして定義されている
              (error "その関数は未定義です"))))                                        ;見つからなかった場合,未定義
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
(define (add-ref-env env newobj) 
  (define (add-ref-env-main env newobj)
    ;newobjとnameが一致するobjを環境から検索
    (let* ((search-rst (search-env-by-obj-name env newobj))); (hoge (display search-rst)))
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
        ((equal? (obj-kind newobj) 'parm)
         (if search-rst
            ;既にnameが一致するオブジェクトが存在するとき
            ;パラメータ二重宣言ならエラー
            (if (equal? (obj-kind search-rst) 'parm)
               (error "パラメータで二重宣言")
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
        ((equal? (obj-kind newobj) 'proto)
         (if search-rst
            ;既にnameが一致するオブジェクトが存在するとき
            (if (equal? (obj-type newobj) (obj-type search-rst))
               (cons newobj env) 　　　　　　　　　　　　　　　　　　　　;型情報が既に存在するものと一致する場合、更新せずにかえす
               (error "型情報の違う二重プロトタイプ宣言はエラー"))
            ;既にnameが一致するオブジェクトが存在しないとき→登録
            (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
              (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
            
        ;新しいobjが関数定義のとき
        ((equal? (obj-kind newobj) 'fun)
         (if search-rst
            ;既にnameが一致するオブジェクトが存在するとき
            (cond
              ((equal? (obj-kind search-rst) 'var)
               (error "既に変数として宣言されている"))
              ((equal? (obj-kind search-rst) 'parm)
               (error "パラメータとして既に宣言されている"));たぶんここにはこない
              ((equal? (obj-kind search-rst) 'fun)
               (error "関数の二重定義はエラー"))
              ((equal? (obj-kind search-rst)  'proto)
               ;型情報が一致しているかどうかチェック
               (if (equal? (obj-type newobj) (obj-type search-rst))
                  ;登録
                  (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
                    (if (null? (cdr env))
                       (cons newobj `(,top-env))
                       (cons newobj `(,top-env ,@(cdr env)))))
                  ;エラー
                  (error "プロトタイプ宣言と関数定義で型が違う")))
              (else (error "unknown obj-kind")))
            
            ;既にnameが一致するオブジェクトが存在しないとき→登録
            (let ((top-env (lambda  (x)  (if (equal? (obj-name x) (obj-name newobj)) newobj ((car env) x)))))
              (if (null? (cdr env))
                 (cons newobj `(,top-env))
                 (cons newobj `(,top-env ,@(cdr env)))))))
        (else (error "unknown obj-kind ")))))
    
  (add-ref-env-main env newobj))
        
(define (env-test mylist)
  (define (env-test-main env mylist)
    (if (= 1 (length mylist))
        `(,(car (add-ref-env env (car mylist))))
        (begin
          (set! env (add-new-level-to-env env))
          (let ((rst (add-ref-env env (car mylist))))
            `(,(car rst) ,@(env-test-main (cdr rst) (cdr mylist)))))))
  (env-test-main initial-env mylist))

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

;型検査をする関数 collect-object関数によりobj構造体をうめこまれたあとの抽象構文木をうけとって型検査をする
;返り値は#tまたは型のリスト 型違反があれば即時エラーを出力する.
;#tはwell-typedを意味する.
(define (type-check ast)
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
            (if (or (equal? 'fun (obj-kind tgt-obj)) (equal? 'proto (obj-kind tgt-obj)))
               (obj (obj-name tgt-obj) (obj-lev tgt-obj) (obj-kind tgt-obj) (car (obj-type tgt-obj)))
                             tgt-obj))
           (obj-type-struct (conv-typelist-to-struct (obj-type tgt-obj-fixed)))
           ;(hoge (display obj-type-struct));for debug
           ) 
      (if (or (equal? 'var (obj-kind tgt-obj-fixed)) (equal? 'parm (obj-kind tgt-obj-fixed)))
         ;パラメータまたは変数のときはvoidはダメ.
         (if (equal? 'void (type-struct-type obj-type-struct))
            (error "void型が現れるのは関数の返り値型としてのみです")
            #t)
         ;プロトタイプ宣言または関数定義のとき,typeがvoidであればポインタでもなく配列でもなければならない
         (if (equal? 'void (type-struct-type obj-type-struct))
            (if (and (equal? #f (type-struct-isarray obj-type-struct))
                    (equal? #f (type-struct-ispointer obj-type-struct)))
               #t
               (error "voidの配列,voidのポインタ型,voidのポインタ型の配列は許可されません!!"))
            #t))))
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
      ((stx:declaration? ast) (type-check-main (stx:declaration-declist ast)))
      ;stx:func-prototype
      ;プロトタイプ宣言自身のobjとパラメータリストがすべて#tならOK
      ((stx:func-prototype? ast)
       (if (and
            (type-check-main (stx:func-prototype-var-decl ast))
            (type-check-main (stx:func-prototype-declarator ast)))
          #t
          #f))
      ;stx:func-definition
      ;関数定義自身のobjとパラメータリストと関数定義の複文本体がすべて#tならOK
      ((stx:func-definition? ast)
       (if (and
            (let ((rst (type-check-main (stx:func-definition-var-decl ast)))) ;関数定義自身のobjがwell-typedがチェック.結果をrstにいれる
              (begin
                (set! now-func-type-struct
                     (conv-typelist-to-struct
                      (obj-type (let ((tgt-obj (stx:func-definition-var-decl ast)))
                                       (obj (obj-name tgt-obj) (obj-lev tgt-obj) (obj-kind tgt-obj) (car (obj-type tgt-obj)))))))　;現在の関数の型をnow-func-type-structに入れる.return文でいまの関数の型を調べるため.
                rst))
            (type-check-main (stx:func-definition-declarator ast))
            (type-check-main (stx:func-definition-statement ast)))
          #t
          #f))
               
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
          (error "while文またはfor文のテストの式の型はintでなければなりません.")))
                
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
          (error "if文のテストの式の型はintでなければなりません.")))
      ;stx:return-stmt
      ;現在いる関数の型と一致しなければエラー
      ((stx:return-stmt? ast)
       (if (equal? 'void (type-struct-type now-func-type-struct))
          ;void型のときはnullでなければエラー
          (if (null? (stx:return-stmt-var ast))
             #t
             (error "void型関数内のreturn文はnullでなければなりません"))
          ;他の型のときは型が一致していればOK
          (if (isequal-type now-func-type-struct (type-check-exp (stx:return-stmt-var ast)))
             #t
             (begin (display now-func-type-struct) (display (type-check-exp (stx:return-stmt-var ast))) (error "return文の型の整合性がとれていません,")))))
     
      (else (begin (display ast) (error "stmt 木の巡回エラー")))))

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
         (if (isequal-type left-type right-type)
            left-type
            (begin (display (stx:assign-stmt-var exp)) (newline) (display left-type) (newline) (display (stx:assign-stmt-src exp)) (newline) (display right-type) (error "代入式の左辺と右辺は同じ型である必要があります")))))
      ;stx:funccall-exp
      ;collec-objectで木を巡回したときに収集した関数の情報から,引数の個数と型が一致しているかをしらべる
      ((stx:funccall-exp? exp)
       (let ((search-rst (search-func-by-name (stx:funccall-exp-tgt exp))))
         (if (equal? #f search-rst)
            (begin  (error "error: funccall-exp: call undefined function"))
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
                    (error "関数呼び出しにおいて定義された引数の型と実際に呼び出しに使われた引数の型が一致しない"))
                 ;引数の個数が一致しなかった
                 (error "関数呼び出しにおいて定義された引数の個数と実際に呼び出しに使われた引数の個数が一致しない"))))))
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
              (else (error "+演算において型違反"))))
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
              (else (error "-演算において型違反"))))
           ;*演算
           ((equal? '* (stx:aop-exp-op exp))
            (if (and (isint left-type) (isint right-type))
               (type-struct 'int #f #f #f)
               (error "*演算において型違反")))
           ;/演算
           ((equal? '/ (stx:aop-exp-op exp))
            (if (and (isint left-type) (isint right-type))
               (type-struct 'int #f #f #f)
               (error "/演算において型違反")))
           (else (error "unknown operand")))))
      ;stx;rop-exp
      ((stx:rop-exp? exp)
       (let ((left-type (type-check-exp (stx:rop-exp-left exp)))
             (right-type (type-check-exp (stx:rop-exp-right exp))))
       (if (isequal-type left-type right-type)
          (type-struct 'int #f #f #f)
          (begin (display left-type) (display right-type) (error "比較演算の左辺と右辺には同じ型がこないといけない!")))))
      ;stx:logical-and-or-exp
      ((stx:logical-and-or-expr? exp)
       (if (and (isint (type-check-exp (stx:logical-and-or-expr-log1 exp))) (isint (type-check-exp (stx:logical-and-or-expr-log2 exp))))
          (type-struct 'int #f #f #f)
          (error "&&または||の右辺と左辺にはint型のみOK")))
      ;stx:addr-exp &
      ((stx:addr-exp? exp)
       (if (isint (type-check-exp (stx:addr-exp-var exp)))
          (type-struct 'int #f #t #f)
          (error "addr-exp &の後ろはint型のみOK")))
      ;stx:deref-exp *
      ((stx:deref-exp? exp)
       ;*e eがint*ならintつける
       (cond
         ((isint-pointer (type-check-exp (stx:deref-exp-arg exp)))
          (type-struct 'int #f #f #f))
         ;*e eがint**ならint*つける
         ((isint-pointerpointer (type-check-exp (stx:deref-exp-arg exp)))
          (type-struct 'int #f #t #f))
       (else (begin 
               (display (stx:deref-exp-arg exp))
               (newline)
               (display (type-check-exp (stx:deref-exp-arg exp)))
               (newline)
               (error "deref-expで型違反")))))
      ;stx:lit-exp
      ((stx:lit-exp? exp) (type-struct 'int #f #f #f))
      ;obj
      ((obj? exp)
       (let ((conv-rst (conv-typelist-to-struct (obj-type exp))))
       ;(if (equal? #t (type-struct-isarray conv-rst))
          ;変数参照式なので intならint*に, int*ならint**にする
        ;  (cond
         ;   ((isint conv-rst) (type-struct 'int #t #t #f))
          ;  ((isint-pointer conv-rst) (type-struct 'int #t #t #t))
           ; (else "変数参照で配列、要素がintかint*以外になっている"))
          ;そのままかえす
          conv-rst));)
      (else (begin (display exp) (error "exp 木の巡回エラー")))))
  
  (display (type-check-main ast)))

;意味解析のメイン関数 抽象構文木をうけとり,オブジェクト情報の収集をしてオブジェクト情報を埋め込んだ抽象構文木をかえす。.
;同時に,二重定義や未定義変数,関数の使用してないかのチェックと型検査を行う.
(define (semantic-analysis ast)
  (let ((object-collected-ast (collect-object ast)))
    (begin
      (type-check object-collected-ast)
      object-collected-ast)))