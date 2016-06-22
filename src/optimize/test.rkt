#lang racket
(require 
  (prefix-in cfg:"cfg.rkt")
  (prefix-in ir: "ir.rkt")
  (prefix-in dfa: "dfa.rkt")
  (prefix-in sgn: "sign.rkt"))
(provide (all-defined-out))
;;;; テスト実行用関数

(define tmp-dot-file "/tmp/cfg.dot")
(define tmp-pdf-file "/tmp/cfg.pdf")

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

;
(define (display-cfg ir #:analyzer (analyzer 'sign) #:gui (gui #t))
  (let ((cfg (cfg:ir->cfg ir)))
    (if analyzer
        ;anlysはdfa.rktで定義されている構造体 sign.rktで定義されている解析用の関数がセットになったもの
        (let* ((anlys (case analyzer
                        ((sign) (sgn:analysis
                                 ;中間表現ででてくる自由変数（プログラム中に出てくる変数）の集合を作成
                                 (apply set-union (map ir:fv-stmt ir))))
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

;;;; test programs

;;; 階乗計算, 入力: x
;;   t = (x < 0);
;;   if(t) L1 L2;
;; L1:
;;   y = -1;
;;   return y;
;;   goto L3;
;; L2:
;; L3: 
;;   y = 1;
;;   goto L5;
;; L4:
;;   y = y * x;
;;   x = x - 1;
;; L5:
;;   t = (x > 1);
;;   if(t) L4 L6;
;; L6:
;;   t = 0;
;;   return y;
(define pgm1
  `(,(ir:assign-stmt 't (ir:rop-exp '< (ir:var-exp 'x) (ir:lit-exp 0)))
    ,(ir:if-stmt 't 'L1 'L2)
    ,(ir:label-stmt 'L1)
    ,(ir:assign-stmt 'y (ir:lit-exp -1))
    ,(ir:ret-stmt 'y)
    ,(ir:goto-stmt 'L3)
    ,(ir:label-stmt 'L2)
    ,(ir:label-stmt 'L3)
    ,(ir:assign-stmt 'y (ir:lit-exp 1))
    ,(ir:goto-stmt 'L5)
    ,(ir:label-stmt 'L4)
    ,(ir:assign-stmt 'y (ir:aop-exp '* (ir:var-exp 'y) (ir:var-exp 'x)))
    ,(ir:assign-stmt 'x (ir:aop-exp '- (ir:var-exp 'x) (ir:lit-exp 1)))
    ,(ir:label-stmt 'L5)
    ,(ir:assign-stmt 't (ir:rop-exp '> (ir:var-exp 'x) (ir:lit-exp 1)))
    ,(ir:if-stmt 't 'L4 'L6)
    ,(ir:label-stmt 'L6)
    ,(ir:assign-stmt 't (ir:lit-exp 0))
    ,(ir:ret-stmt 'y)))

;;; 絶対値，入力: x
;;   t = (x >= 0);
;;   if(t) L1 L2;
;; L1:
;;   return x;
;;   goto L3;
;; L2:               
;;   x = 0 - x;
;; L3:
;;   return x;
(define pgm2
  `(,(ir:assign-stmt 't (ir:rop-exp '>=
                              (ir:var-exp 'x)
                              (ir:lit-exp 0)))
    ,(ir:if-stmt 't 'L1 'L2)
    ,(ir:label-stmt 'L1)
    ,(ir:label-stmt 'L4) ;; dummy
    ,(ir:assign-stmt 'y (ir:var-exp 'x))
    ,(ir:label-stmt 'L5) ;; dummy
    ,(ir:goto-stmt 'L3)
    ,(ir:label-stmt 'L2)
    ,(ir:assign-stmt 'y (ir:aop-exp '-
                              (ir:lit-exp 0)
                              (ir:var-exp 'x)))
    ,(ir:label-stmt 'L3)))
