#lang racket
(require 
  (prefix-in semantic: "../semantic.rkt")
  (prefix-in ir: "../ir.rkt")
  (prefix-in ir-stx: "../ir-syntax.rkt")
   (prefix-in addr: "../addr.rkt")
   (prefix-in cfg:"cfg.rkt")
  (prefix-in dfa: "dfa.rkt")
  (prefix-in sgn: "sign.rkt")
  (prefix-in reach: "reachdef.rkt"))
(provide (all-defined-out))
;;;; テスト実行用関数

(define tmp-dot-file "./cfg.dot")
(define tmp-pdf-file "./cfg.pdf")

(define (emit-dot cfg . opts)
  (with-output-to-file tmp-dot-file
    (lambda ()
      (display (apply cfg:cfg->dot cfg opts)))
    #:mode 'text #:exists 'replace)
  (system (format ;"/usr/bin/dot -Tpdf -o ~a ~a"       ; for Linux
                  "/usr/local/bin/dot -Tpdf -o ~a ~a" ; for Mac (homebrew)
                  tmp-pdf-file tmp-dot-file))
  ;(process (format "evince ~a" tmp-pdf-file)) ; for Linux
  (system (format "open ~a" tmp-pdf-file))    ; for Mac
  (void))

(define (get-cfg filename)
  (cfg:ir->cfg (cfg:get-main-def (addr:assign-addr (ir:ir-main filename)))))

(define (display-cfg2 filename #:analyzer (analyzer 'sign) #:gui (gui #t))
  (let ((ir (cfg:get-main-def (addr:assign-addr (ir:ir-main filename))))
        (cfg (get-cfg filename)))
    (if analyzer
        ;anlysはdfa.rktで定義されている構造体 sign.rktで定義されている解析用の関数がセットになったもの
        (let* ((anlys (case analyzer
                        ((reachdef) (reach:analysis
                                    (apply set-union (map fv-stmt ir))))
                        ((sign) (sgn:analysis
                                 ;中間表現ででてくる自由変数(プログラム中に出てくる変数)の集合を作成
                                 (apply set-union (map fv-stmt ir))))
                        (else (error "unknown analyzer:" analyzer))))
               ;実際に符号解析行い,結果をsolutionに入れる
               (solution (dfa:solve anlys cfg)))
          ;以下は表示用
          ;dfa:get-propertyが解析結果から特定のプロパティを取得する関数.
          ;それを用いて取得したデータを読みやすく文字化したものをsign-prop-strに入れる.
          (let ((prop-str (case analyzer
                            ((reachdef) (lambda (stmt kind)
                                          (reach:store->string
                                           (dfa:get-property solution stmt #:kind kind))))
                            ((sign) (lambda (stmt kind)
                                      (sgn:store->string
                                       (dfa:get-property solution stmt #:kind kind)))))))
            (if gui
                (emit-dot cfg prop-str)
                (displayln (cfg:cfg->string cfg prop-str)))))
        (if gui
            (emit-dot cfg)
            (displayln (cfg:cfg->string cfg))))))

(define (optimize ir)
  (define (remove-initial v)
    (cond
      ((null? v) `())
      ((equal? (car v) 'initial) (remove-initial (cdr v)))
      (else (cons (car v) (remove-initial (cdr v))))))
  ;辞書dictからvarの定数が埋め込めるなら(ir-stx:lit-exp )を変えす.不可能なら(ir-stx:var-exp)
  (define (find-lit var dict)
    (let* ((var-name (semantic:obj-name var))
          (res (remove-initial (dict-ref dict var-name))))
      (if (equal? (length res) 1)
          (cond
            ((ir-stx:var-exp? (car res)) (find-lit (ir-stx:var-exp-var (car res)) dict))
            ((semantic:obj?   (car res)) (find-lit (car res) dict))
            ((ir-stx:lit-exp? (car res)) (car res))
            (else var))
          var)))
  ;
  (let* ((main-ir (cfg:get-main-def ir))
         (main-cfg  (cfg:ir->cfg main-ir))
         (anlys (reach:analysis (apply set-union (map fv-stmt main-ir))))
         (solution (dfa:solve anlys main-cfg)) ;解析結果
         (in-dict (car solution))             ;各文をキー,各文にはいる前の到達定義集合が値となる辞書
         ;到達定義に基づいて定数畳み込みを各文について行う
         (main-ir-optimized
           (map (lambda (x) (if (ir-stx:assign-stmt? x)
                                (let* ((def-dict (dict-ref in-dict x))
                                      (exp (ir-stx:assign-stmt-exp x)))
                                  (ir-stx:assign-stmt 
                                   (ir-stx:assign-stmt-var x)
                                   (cond ((ir-stx:var-exp? exp) (let ((rst (find-lit (ir-stx:var-exp-var exp) def-dict)))
                                                                  (if (ir-stx:lit-exp? rst)
                                                                      rst
                                                                      (ir-stx:var-exp rst))))
                                         ;((ir-stx:aop-exp? exp) (ir-stx:aop-exp (ir-stx:aop-exp-op exp)
                                         ;                                       (find-lit (ir-stx:aop-exp-left exp) def-dict)
                                         ;                                       (find-lit (ir-stx:aop-exp-right exp) def-dict)))
                                         ;((ir-stx:rop-exp? exp) (ir-stx:rop-exp (ir-stx:rop-exp-op exp)
                                         ;                                       (find-lit (ir-stx:rop-exp-left exp) def-dict)
                                         ;                                       (find-lit (ir-stx:rop-exp-right exp) def-dict)))
                                         (else exp))))
                                x))
                main-ir)))
    ;main関数を最適化したものに差し替える
    (map (lambda (x) (if (and (ir-stx:fun-def? x) (equal? (semantic:obj-name (ir-stx:fun-def-var x)) 'main))
                         (ir-stx:fun-def (ir-stx:fun-def-var x)
                                         (ir-stx:fun-def-parms x)
                                         main-ir-optimized)
                         x))
         ir)))
;;;; 自由変数(free variable)集合
;;;;   この中間表現では,自由変数とはプログラム中に出現する変数のことと思えばよい
(define (fv-exp exp)
  (cond
   ((semantic:obj? exp) (list (semantic:obj-name exp)))
   ((ir-stx:var-exp? exp) (list (semantic:obj-name (ir-stx:var-exp-var exp))))
   ((ir-stx:lit-exp? exp) '())
   ((ir-stx:aop-exp? exp) (set-union (fv-exp (ir-stx:aop-exp-left exp))
                              (fv-exp (ir-stx:aop-exp-right exp))))
   ((ir-stx:rop-exp? exp) (set-union (fv-exp (ir-stx:rop-exp-left exp))
                              (fv-exp (ir-stx:rop-exp-right exp))))
   (else '())))
   ;(else (error "unknown exp:" exp))))

(define (fv-stmt stmt)
  (cond
   ((ir-stx:assign-stmt? stmt) (set-add (fv-exp (ir-stx:assign-stmt-exp stmt))
                                 (semantic:obj-name (ir-stx:assign-stmt-var stmt))))
   ((ir-stx:label-stmt? stmt) '())
   ((ir-stx:if-stmt? stmt) (list (semantic:obj-name (ir-stx:if-stmt-var stmt))))
   ((ir-stx:goto-stmt? stmt) '())
   ((ir-stx:ret-stmt? stmt) (list (semantic:obj-name (ir-stx:ret-stmt-var stmt))))
   (else '())))
   ;(else (error "unknown stmt:" stmt))))

;(display-cfg2 "../sample.c" #:analyzer 'sign)
;(display-cfg2 "../sample.c" #:analyzer 'reachdef)


