;;;; 符号解析
#lang racket
(require (prefix-in ir-stx: "../ir-syntax.rkt")
         (prefix-in dfa: "dfa.rkt")
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


;;;; abst値に対する算術演算,関係演算の計算.結果もabst値

;; 算術演算
;; (注: 書いた人間は算数が苦手なため,バグが含まれている可能性大)
(define (abst-aop-calc op left right)
  (let ((rands (list left right)))
    (define (lr-is l r)
      (and (eq? left l) (eq? right r)))
    (define (rands-is ss)
      (set=? rands ss))
    (cond
     ((or (eq? left 'none) (eq? right 'none))                'none)
     ((or (eq? left  'any) (eq? right  'any))                 'any)
     (else 
      (case op
        ((+) (cond ((or (rands-is '(pos  pos))
                        (rands-is '(pos  zero))
                        (rands-is '(pos  non-neg)))           'pos)
                   ((or (rands-is '(neg  neg))
                        (rands-is '(neg  zero))
                        (rands-is '(neg  non-pos)))           'neg)
                   (    (rands-is '(zero zero))              'zero)

                   ((or (rands-is '(non-pos  non-pos))
                        (rands-is '(non-pos  zero)))      'non-pos)
                   ((or (rands-is '(non-neg  non-neg))
                        (rands-is '(non-neg  zero)))      'non-neg)
                   (    (rands-is '(non-zero zero))      'non-zero)
                   (else                                      'any)))

        ((-) (cond ((or (lr-is 'pos      'neg)
                        (lr-is 'pos      'zero)
                        (lr-is 'zero     'neg)
                        (lr-is 'non-neg  'neg))               'pos)
                   ((or (lr-is 'neg      'pos)
                        (lr-is 'neg      'zero)
                        (lr-is 'zero     'pos)
                        (lr-is 'non-pos  'pos))               'neg)
                   (    (lr-is 'zero     'zero)              'zero)
                   ((or (lr-is 'neg      'non-neg)
                        (lr-is 'zero     'non-neg)
                        (lr-is 'non-pos  'zero)
                        (lr-is 'non-pos  'non-neg))       'non-pos)
                   ((or (lr-is 'pos      'non-pos)
                        (lr-is 'zero     'non-pos)
                        (lr-is 'non-neg  'zero)
                        (lr-is 'non-neg  'non-pos))       'non-neg)
                   ((or (lr-is 'zero     'non-zero)
                        (lr-is 'non-zero 'zero))         'non-zero)
                   (else                                      'any)))
                
        ((*) (cond ((or (rands-is '(pos      pos))
                        (rands-is '(neg      neg)))           'pos)
                   (    (rands-is '(pos      neg))            'neg)
                   (    (lr-is    'zero     'zero)           'zero)
                   ((or (rands-is '(pos      non-pos))
                        (rands-is '(neg      non-neg))
                        (rands-is '(non-pos  non-neg)))   'non-pos)
                   ((or (rands-is '(pos      non-neg))
                        (rands-is '(neg      non-pos))
                        (rands-is '(non-pos  non-pos))
                        (rands-is '(non-neg  non-neg)))   'non-neg)
                   ((or (rands-is '(pos      non-zero))
                        (rands-is '(neg      non-zero))
                        (rands-is '(non-zero non-zero))) 'non-zero)
                   (else                                      'any)))

        ((/) (cond ((or (rands-is '(pos      pos))
                        (rands-is '(neg      neg)))           'pos)
                   (    (rands-is '(pos      neg))            'neg)
                   (    (eq? left 'zero)                     'zero)
                   ((or (lr-is 'non-pos     'pos)
                        (lr-is 'non-neg     'neg))        'non-pos)
                   ((or (lr-is 'non-neg     'pos)
                        (lr-is 'non-pos     'neg))        'non-neg)
                   ((or (rands-is '(pos      non-zero))
                        (rands-is '(neg      non-zero))
                        (rands-is '(non-zero non-zero))) 'non-zero)
                   (else                                      'any)))
        (else (error "unknown aop:" op)))))))

;; 関係演算.C言語では真が1,偽が0であることに注意.
;; (注: 書いた人間は算数が苦手なため,バグが含まれている可能性大)
(define (abst-rop-calc op left right)
  (let ((rands (list left right)))
    (define (lr-is l r)
      (and (eq? left l) (eq? right r)))
    (define (rands-is ss)
      (set=? rands ss))
    (cond
     ((or (eq? left 'none) (eq? right 'none))       'none)
     ((or (eq? left  'any) (eq? right  'any))    'non-neg)
     (else
      (case op
        ((==) (cond
               (    (rands-is '(zero zero))          'pos)
               ((or (rands-is '(pos  neg))
                    (rands-is '(pos  zero))
                    (rands-is '(pos  non-pos))
                    (rands-is '(neg  zero))
                    (rands-is '(neg  non-neg))
                    (rands-is '(zero non-zero)))    'zero)
               (else                             'non-neg)))
        ((!=) (cond
               ((or (rands-is '(pos  neg))
                    (rands-is '(pos  zero))
                    (rands-is '(pos  non-pos))
                    (rands-is '(neg  zero))
                    (rands-is '(neg  non-neg))
                    (rands-is '(zero non-zero)))     'pos)
               (    (rands-is '(zero zero))         'zero)
               (else                             'non-neg)))
        ((<)  (cond
               ((or (lr-is 'neg     'pos)
                    (lr-is 'neg     'zero)
                    (lr-is 'neg     'non-neg)
                    (lr-is 'zero    'pos)
                    (lr-is 'non-pos 'pos))           'pos)
               ((or (lr-is 'pos     'neg)
                    (lr-is 'pos     'zero)
                    (lr-is 'pos     'non-pos)
                    (lr-is 'zero    'neg)
                    (lr-is 'zero    'non-pos)
                    (lr-is 'non-neg 'neg)
                    (lr-is 'non-neg 'non-pos))      'zero)
               (else                             'non-neg)))
        ((<=) (cond
               ((or (lr-is 'neg     'pos)
                    (lr-is 'neg     'zero)
                    (lr-is 'neg     'non-neg)
                    (lr-is 'zero    'pos)
                    (lr-is 'zero    'zero)
                    (lr-is 'zero    'non-neg)
                    (lr-is 'non-pos 'pos)
                    (lr-is 'non-pos 'zero)
                    (lr-is 'non-pos 'non-neg))       'pos)
               ((or (lr-is 'pos     'neg)
                    (lr-is 'pos     'zero)
                    (lr-is 'pos     'non-pos)
                    (lr-is 'zero    'neg)
                    (lr-is 'non-neg 'neg))          'zero)
               (else                             'non-neg)))
        ((>)  (cond
               ((or (lr-is 'pos     'neg)
                    (lr-is 'pos     'zero)
                    (lr-is 'pos     'non-pos)
                    (lr-is 'zero    'neg)
                    (lr-is 'non-neg 'neg))           'pos)
               ((or (lr-is 'neg     'pos)
                    (lr-is 'neg     'zero)
                    (lr-is 'neg     'non-neg)
                    (lr-is 'zero    'pos)
                    (lr-is 'zero    'zero)
                    (lr-is 'zero    'non-neg)
                    (lr-is 'non-pos 'pos)
                    (lr-is 'non-pos 'zero)
                    (lr-is 'non-pos 'non-neg))      'zero)
               (else                             'non-neg)))
        ((>=) (cond
               ((or (lr-is 'pos     'neg)
                    (lr-is 'pos     'zero)
                    (lr-is 'pos     'non-pos)
                    (lr-is 'zero    'neg)
                    (lr-is 'zero    'non-pos)
                    (lr-is 'non-neg 'neg)
                    (lr-is 'non-neg 'non-pos))       'pos)
               ((or (lr-is 'neg     'pos)
                    (lr-is 'neg     'zero)
                    (lr-is 'neg     'non-neg)
                    (lr-is 'zero    'pos)
                    (lr-is 'non-pos 'pos))          'zero)
               (else                             'non-neg)))
        (else (error "unknown rop:" op)))))))
    
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
           (store-update s v '(neg zero pos)))
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
  (define (binding->string v s)
    (format "~a -> ~a" v s))
  (string-join
   (map (lambda (v)
          (binding->string v (sign->abst (store-lookup s v))))
        (store-vars s))
   ", "
   #:before-first "{" #:after-last "}"))

;;; 式の符号を計算
(define (exp->sign exp store)
  (cond
   ((semantic:obj? exp) (store-lookup store (semantic:obj-name exp)))
   ((ir-stx:var-exp? exp) (store-lookup store (semantic:obj-name (ir-stx:var-exp-var exp))))
   ((ir-stx:lit-exp? exp) (let ((n (ir-stx:lit-exp-val exp)))
                     (cond ((zero? n) '(zero))
                           ((positive? n) '(pos))
                           ((negative? n) '(neg)))))
   ((ir-stx:aop-exp? exp) (let ((ls (exp->sign (ir-stx:aop-exp-left exp) store))
                         (rs (exp->sign (ir-stx:aop-exp-right exp) store)))
                     (abst->sign
                      (abst-aop-calc (ir-stx:aop-exp-op exp)
                                     (sign->abst ls)
                                     (sign->abst rs)))))
   ((ir-stx:rop-exp? exp) (let ((ls (exp->sign (ir-stx:rop-exp-left exp) store))
                         (rs (exp->sign (ir-stx:rop-exp-right exp) store)))
                     (abst->sign
                      (abst-rop-calc (ir-stx:rop-exp-op exp)
                                     (sign->abst ls)
                                     (sign->abst rs)))))
   (else (error "unknown exp:" exp))))

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
                      (exp->sign (ir-stx:assign-stmt-exp stmt) entry-store))
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
                (top-store vs)));中間表現に出てくる変数について (変数,'(neg zero pos))という辞書集合(辞書はhasheqテーブル)
