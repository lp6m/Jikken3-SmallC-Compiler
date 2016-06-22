;;;; 到達定義解析
#lang racket
(require (prefix-in ir-stx: "../ir-syntax.rkt")
         (prefix-in dfa: "dfa.rkt")
         (prefix-in cfg: "cfg.rkt")
         (prefix-in semantic: "../semantic.rkt"))
(provide (all-defined-out))

;; sign: 符号(+,-,0)の集合
;;   符号: pos (+),neg (-),zero (0)
;; abst: 符号の集合に対する名前づけ(シンボル)
;;   none: {}
;;   pos: {+}, neg: {-},zero: {0}
;;   non-pos: {-,0}, non-neg: {+,0}, non-zero: {-,+}
;;   any: {-,0,+}

(define (sign->abst ss)
  (cond ((empty? ss) 'none)
        ((= (length ss) 1) (first ss))
        ((set=? ss '(neg zero)) 'non-pos)
        ((set=? ss '(pos zero)) 'non-neg)
        ((set=? ss '(neg pos)) 'non-zero)
        ((set=? ss '(neg zero pos)) 'any)
        (else (error "illegal sign:" ss))))

(define (abst->sign as)
  (case as
    ((none)     '())
    ((neg)      '(neg))
    ((zero)     '(zero))
    ((pos)      '(pos))
    ((non-neg)  '(zero pos))
    ((non-zero) '(neg pos))
    ((non-pos)  '(neg zero))
    ((any)      '(neg zero pos))
    (else (error "illegal abst sign:" as))))

;; signの順序関係
(define sign-leq subset?)

;; signのleast upper bound演算
(define sign-lub set-union)
    
;;;; store: 変数名 -> sign なハッシュテーブル(hasheq)

;; -> store
(define (empty-store) (hasheq))

;; store -> 変数名のリスト
(define store-vars dict-keys)

(define bot-store (empty-store))

;; store, 変数名 -> sign
(define (store-lookup s v)
  (dict-ref s v '()))

;; store, 変数名,sign -> store
(define store-update dict-set)

;; store, 変数名 -> store
(define store-remove dict-remove)

;; 変数名のリスト -> store
(define (top-store vs)
  (foldl (lambda (v s)
           (store-update s v '(initial)))
         bot-store
         vs))

;; dfa.rktで指定されている仕様に沿った比較結果(-1/0/1/#f)を返す
(define (store-compare left right)
  (define (leq l r)
    (andmap (lambda (v)
              ;sign-leqはsubset? subset? x yはxがyの部分集合かを返す
              (sign-leq (store-lookup l v)
                        (store-lookup r v)))
            (store-vars l))) ;lのキーすべて
  (let ((leq-l (leq left right))
        (leq-r (leq right left)))
    (cond ((and leq-l leq-r) 0)
          (leq-l -1)
          (leq-r 1)
          (else #f))))

;; storeのleast upper bound演算
;;foldlの内部実装
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;ssは辞書が要素にあるリスト
(define (store-lub . ss)
  (foldl (lambda (ps ns) ;nsがssの要素
           (foldl (lambda (v ns)
                    (let ((p-sgn (store-lookup ps v))
                          (n-sgn (store-lookup ns v)))
                      (store-update
                       ns v (sign-lub p-sgn n-sgn))))
                  ns
                  (store-vars ps)))  
         bot-store
         ss))

;; 読みやすく文字列化
(define (store->string s)
  (define (remove-initial v)
    (cond
      ((null? v) `())
      ((equal? (car v) 'initial) (remove-initial (cdr v)))
      (else (cons (car v) (remove-initial (cdr v))))))
  (define (binding->string v s)
    (if (equal? s "")
        ""
        (format "~a = ~a " v s)))
  (string-join
   (map (lambda (v)
          (binding->string v (string-join (map cfg:exp->string (remove-initial (store-lookup s v))) ", ")))
        (store-vars s))
   ""
   #:before-first "{" #:after-last "}"))

;; 伝達関数,forward store update
;; stmt,store -> store
;この伝達関数は1つのstmtを受け取って,
;
(define (transfer stmt entry-store)
  (define (gen s)
    (if (ir-stx:assign-stmt? stmt)
        ;もし代入文であれば左辺にある変数の定義が更新されるので右辺の式の符号を計算して更新した辞書を返す
        (store-update s
                      (semantic:obj-name (ir-stx:assign-stmt-var stmt))
                      (list (ir-stx:assign-stmt-exp stmt)))
                      ;(exp->sign (ir-stx:assign-stmt-exp stmt) entry-store))
        s))
  ;もし代入文であれば定義が抹消されるので取り除く
  (define (kill s)
    (if (ir-stx:assign-stmt? stmt)
        (store-remove s (semantic:obj-name (ir-stx:assign-stmt-var stmt)))
        s))
  (gen (kill entry-store)))

;; 変数名リスト -> analysis
;;   引数の変数名リストは,対象プログラムに出現する全ての変数名
(define (analysis vs)
  ;dfaに関数を渡している dfaで定義されているanalysis構造体となる.
  (dfa:analysis 'forward
                transfer        ;伝達関数
                store-compare   ;leftとrightを受け取って-1/0/1/#fを返す関数
                store-lub       ;
                bot-store       ;hasheq
                (top-store vs)));中間表現に出てくる変数について (変数,(hasheq))という辞書集合(辞書はhasheqテーブル)
