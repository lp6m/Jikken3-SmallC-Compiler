#lang racket
(require 
  (prefix-in semantic: "../semantic.rkt")
  (prefix-in ir: "../ir.rkt")
  (prefix-in ir-stx: "../ir-syntax.rkt")
   (prefix-in addr: "../addr.rkt")
   (prefix-in cfg:"cfg.rkt")
  (prefix-in dfa: "dfa.rkt")
  (prefix-in sgn: "sign.rkt"))
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

(define (get-ir filename)
  (addr:assign-addr (ir:ir-main filename)))

(define (get-cfg filename)
  (cfg:ir->cfg (cfg:get-main-def (addr:assign-addr (ir:ir-main filename)))))
         
(define (my-optimize assigned-ir)
  (let* 
       ((cfg (cfg:ir->cfg (cfg:get-main-def assigned-ir))))
  (begin (pretty-print cfg) (emit-dot cfg))))

(define (my-optimize-file filename)
  (my-optimize (addr:assign-addr (ir:ir-main filename))))

(define (display-cfg2 filename #:analyzer (analyzer 'sign) #:gui (gui #t))
  (let ((ir (cfg:get-main-def (addr:assign-addr (ir:ir-main filename))))
        (cfg (get-cfg filename)))
    (if analyzer
        ;anlysはdfa.rktで定義されている構造体 sign.rktで定義されている解析用の関数がセットになったもの
        (let* ((anlys (case analyzer
                        ((sign) (sgn:analysis
                                 ;中間表現ででてくる自由変数(プログラム中に出てくる変数)の集合を作成
                                 (apply set-union (map fv-stmt ir))))
                        (else (error "unknown analyzer:" analyzer))))
               ;実際に符号解析行い,結果をsolutionに入れる
               (solution (dfa:solve anlys cfg)))
          ;以下は表示用
          ;dfa:get-propertyが解析結果から特定のプロパティを取得する関数.
          ;それを用いて取得したデータを読みやすく文字化したものをsign-prop-strに入れる.
          (let ((sign-prop-str
                 (lambda (stmt kind)
                   (sgn:store->string
                    (dfa:get-property solution stmt #:kind kind)))))
            (if gui
                (emit-dot cfg sign-prop-str)
                (displayln (cfg:cfg->string cfg sign-prop-str)))))
        (if gui
            (emit-dot cfg)
            (displayln (cfg:cfg->string cfg))))))

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
