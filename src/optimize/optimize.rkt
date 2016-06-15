#lang racket
(require 
   (prefix-in ir: "../ir.rkt")
   (prefix-in addr: "../addr.rkt")
   (prefix-in cfg:"cfg.rkt"))
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

(define (test filename)
  (let* 
      ((ir (addr:assign-addr (ir:ir-main filename)))
       (cfg (cfg:ir->cfg (cfg:get-main-def ir))))
  (emit-dot cfg)))