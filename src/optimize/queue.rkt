;;;; キュー
#lang racket
(provide (all-defined-out))
;queueはconsセルになっている
;carは結果を表し,
;cdrはキューのデータを表す.
(define (queue-test)
  (let ((q (make-queue)))
    (begin
      (display q)
      (display "\n insert a b c\n")
      (set! q (enqueue q 'a))
      (set! q (enqueue q 'b))
      (set! q (enqueue q 'c))
      (display q)
      (display "\n dequeue\n")
      (set! q (dequeue q))
      (display "\n dequeue\n")
      (set! q (dequeue q))
      (display q)
      (display "\n dequeue\n")
      (set! q (dequeue q)))))


;; -> queue
(define (make-queue)
  (cons '() '()))

;; queue, v -> queue
;queue qに新しい要素vを入れる
(define (enqueue q v)
  (let ((f (car q)) (r (cdr q)))
    (cons f (cons v r))))

;; queue -> (v . queue)
;;   returns e if empty 
(define (dequeue q #:if-empty (e #f))
  (let ((f (car q)) (r (cdr q)))
    (cond
     ((and (empty? f) (empty? r)) (cons e q))
     ((empty? f) (let ((nf (reverse r)))
                   (cons (car nf)
                         (cons (cdr nf) '()))))
     (else (cons (car f)
                 (cons (cdr f) r))))))
