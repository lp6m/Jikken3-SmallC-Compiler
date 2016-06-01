#lang racket
(provide (all-defined-out))
(require (prefix-in parser: "parser.rkt")
         (prefix-in semantic: "semantic.rkt")
         (prefix-in stx: "parser-syntax.rkt")
         (prefix-in ir-stx:    "ir-syntax.rkt")
         (prefix-in ir: "ir.rkt"))
;offsetの値0に戻す
(define (reset-addr-parm)
  (set! min-addr-size 0)
  (set! now-addr-size 0))
;関数でのアドレス(負の値)の最小値を保存
(define min-addr-size 0)
;現時点でのアドレスのサイズを示す
(define now-addr-size 0)
;呼び出すごとに新しいの位置(4刻み)
(define (fresh-addr-normal)
  (let ([old-addr-size now-addr-size])
    (set! now-addr-size (- now-addr-size 4))
    old-addr-size))

(define (fresh-addr-array num)
  (let ([old-addr-size now-addr-size])
    (set! now-addr-size (- now-addr-size (* 4 num)))
    (+ 4 now-addr-size)))
;offset用の環境
(define initial-env (list (lambda (x) #f)))
(define obj-env initial-env)
;objをaddしてobjと環境のconsセルかえす
(define (add-obj-to-env env obj)
  (begin
    (set! min-addr-size (min min-addr-size (semantic:obj-ofs obj)))
    (let ((top-env (lambda (x) (if (equal? (semantic:obj-name obj) (semantic:obj-name x)) obj ((car env) x)))))
      (if (null? (cdr env))
          (cons obj `(,top-env))
          (cons obj `(,top-env ,@(cdr env)))))))
     
;obj-envに新しいレベルの環境を足す
(define (add-newlevel-to-env env)
  (append initial-env env))
;obj-envからpopする
(define (pop-env env)
  (cdr env))
;objからenvを探す
;返り値はみつかったobjまたは#f
(define (search-env-by-obj-name env tgt-obj)
    (if (= (length env) 1)
       ((car env) tgt-obj) 
       (if ((car env) tgt-obj)
          ((car env) tgt-obj)
          (search-env-by-obj-name (cdr env) tgt-obj))))
;中間表現のobj構造体にoffsetを割り当てる関数
;lev = 0の大域変数は無視
;offsetをつけるのはir-stx:fun-def-parmsとir-stx:cmpd-stmt-declsのみ.
;
(define (assign-addr ir-ast)
  ;assign-addr関数の初期化
  (define (initial-addr-parm)
  (begin
    (set! obj-env initial-env)
    (reset-addr-parm)))
  ;objのofsをセットしたobjを返す
  (define (set-offset-to-obj tgt-obj addr)
    (semantic:obj (semantic:obj-name tgt-obj)
                  (semantic:obj-lev tgt-obj)
                  (semantic:obj-kind tgt-obj)
                  (semantic:obj-type tgt-obj)
                  addr))
  (define (assign-addr-main ir-ast)
    (cond
      ((list? ir-ast) (map assign-addr-main ir-ast))
      ((ir-stx:var-decl? ir-ast)
       (ir-stx:var-decl
        (let* ((tgt-obj
                 (if (and (equal? 0 (semantic:obj-lev (ir-stx:var-decl-var ir-ast)))
                          (not (equal? 'tmp (semantic:obj-type (ir-stx:var-decl-var ir-ast)))))
                     (ir-stx:var-decl-var ir-ast) ;level0は大域なのでofsはそのまま
                     ;ofsを割り当てる
                     (if (and (not (equal? 'tmp (semantic:obj-type (ir-stx:var-decl-var ir-ast))))
                              (equal? 'array (car (semantic:obj-type (ir-stx:var-decl-var ir-ast)))))
                         ;配列のときはfresh-addr-array
                         (let ((arraynum (car (reverse (semantic:obj-type (ir-stx:var-decl-var ir-ast))))))
                           (set-offset-to-obj (ir-stx:var-decl-var ir-ast) (fresh-addr-array arraynum)))
                         ;それ以外はfresh-addr-normal
                         (set-offset-to-obj (ir-stx:var-decl-var ir-ast) (fresh-addr-normal)))))
               (rst (add-obj-to-env obj-env tgt-obj)))
          (begin
            (set! obj-env (cdr rst))
            (car rst)))))
               
            
      ((ir-stx:fun-def? ir-ast)
       (begin
         (reset-addr-parm)
         (let ((fun-def-rst
                (ir-stx:fun-def
                 ;ir-stx:fun-def-var
                 (ir-stx:fun-def-var ir-ast)
                 ;ir-stx:fun-def-parms 4個目までは無視 5個目からは4,8,12,...
                 (let ((params
                        (if (<= (length (ir-stx:fun-def-parms ir-ast)) 4)
                            (ir-stx:fun-def-parms ir-ast)
                            (let ((parmindex 0))
                              (map (lambda (x) 
                                     (ir-stx:var-decl (set-offset-to-obj (ir-stx:var-decl-var x)
                                                                         (begin
                                                                           (set! parmindex (+ 1 parmindex))
                                                                           (if (<= parmindex 4)
                                                                               0
                                                                               (* 4 (- parmindex 4)))))))
                                   (ir-stx:fun-def-parms ir-ast))))))
                   ;bodyで引数をいじる時はコピーしたメモリ位置にする
                   (for-each (lambda (x) (let ((rst (add-obj-to-env obj-env (ir-stx:var-decl-var x)))) (begin (set! obj-env (cdr rst)) (ir-stx:var-decl (car rst)))))
                             (map (lambda (x) (ir-stx:var-decl (set-offset-to-obj (ir-stx:var-decl-var x) (fresh-addr-normal)))) params))
                   params)
                 ;ir-stx:fun-def-body
                 (assign-addr-main (ir-stx:fun-def-body ir-ast)))))
           ;fun-def-varのofsにローカルメモリの最小値入れる
           (ir-stx:fun-def
            (set-offset-to-obj (ir-stx:fun-def-var fun-def-rst) min-addr-size)
            (ir-stx:fun-def-parms fun-def-rst)
            (ir-stx:fun-def-body fun-def-rst)))))
      ((ir-stx:cmpd-stmt? ir-ast)
       (let ((old-addr now-addr-size)
               (rst (ir-stx:cmpd-stmt
                     (begin
                       (set! obj-env (add-newlevel-to-env obj-env))
                       (assign-addr-main (ir-stx:cmpd-stmt-decls ir-ast)))
                     (assign-addr-main (ir-stx:cmpd-stmt-stmts ir-ast)))))
           (begin
             (set! now-addr-size old-addr)
             (set! obj-env (pop-env obj-env))
             rst)))
      
      ((semantic:obj? ir-ast)
       (if (search-env-by-obj-name obj-env ir-ast)
           (search-env-by-obj-name obj-env ir-ast)
           (begin (display ir-ast) (error "something wrong in addr.rkt!"))));見つからなかったらバグってる
      ((ir-stx:assign-stmt? ir-ast)
       (ir-stx:assign-stmt (assign-addr-main (ir-stx:assign-stmt-var ir-ast))
                           (assign-addr-main (ir-stx:assign-stmt-exp ir-ast))))
      ((ir-stx:write-stmt? ir-ast)
       (ir-stx:write-stmt (assign-addr-main (ir-stx:write-stmt-dest ir-ast))
                          (assign-addr-main (ir-stx:write-stmt-src ir-ast))))
      ((ir-stx:read-stmt? ir-ast)
       (ir-stx:read-stmt (assign-addr-main (ir-stx:read-stmt-dest ir-ast))
                         (assign-addr-main (ir-stx:read-stmt-src ir-ast))))
      ((ir-stx:label-stmt? ir-ast) ir-ast)
      ((ir-stx:if-stmt? ir-ast)
       (ir-stx:if-stmt (assign-addr-main (ir-stx:if-stmt-var ir-ast))
                       (ir-stx:if-stmt-tlabel ir-ast)
                       (ir-stx:if-stmt-elabel ir-ast)))
      ((ir-stx:goto-stmt? ir-ast) ir-ast)
      ((ir-stx:call-stmt? ir-ast)
       (ir-stx:call-stmt (assign-addr-main (ir-stx:call-stmt-dest ir-ast))
                         (ir-stx:call-stmt-tgt ir-ast)
                         (assign-addr-main (ir-stx:call-stmt-vars ir-ast))))
      ((ir-stx:ret-stmt? ir-ast)
       (ir-stx:ret-stmt (assign-addr-main (ir-stx:ret-stmt-var ir-ast))))
      ((ir-stx:print-stmt? ir-ast)
       (ir-stx:print-stmt (assign-addr-main (ir-stx:print-stmt-var ir-ast))))
      ((ir-stx:var-exp? ir-ast) (ir-stx:var-exp (assign-addr-main (ir-stx:var-exp-var ir-ast))))
           
      ((ir-stx:lit-exp? ir-ast) ir-ast)
      ((ir-stx:aop-exp? ir-ast)
       (ir-stx:aop-exp (ir-stx:aop-exp-op ir-ast)
                       (assign-addr-main (ir-stx:aop-exp-left ir-ast))
                       (assign-addr-main (ir-stx:aop-exp-right ir-ast))))
      ((ir-stx:rop-exp? ir-ast)
       (ir-stx:rop-exp (ir-stx:rop-exp-op ir-ast)
                       (assign-addr-main (ir-stx:rop-exp-left ir-ast))
                       (assign-addr-main (ir-stx:rop-exp-right ir-ast))))
      ((ir-stx:addr-exp? ir-ast)
       (ir-stx:addr-exp (assign-addr-main (ir-stx:addr-exp-var ir-ast))))
      (else ir-ast)))
  (begin (initial-addr-parm) (assign-addr-main ir-ast)))