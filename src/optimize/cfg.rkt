;;;; 制御フローグラフ(control flow graph)
#lang racket
(require (prefix-in ir: "../ir.rkt")
         (prefix-in addr: "../addr.rkt")
         (prefix-in ir-stx: "../ir-syntax.rkt")
         (prefix-in semantic: "../semantic.rkt"))
(provide (all-defined-out))

;;;; CFG (control flow graph) は基本ブロックのベクタ

;; 基本ブロック (basic block)
(struct bblock
  ((labels #:mutable) ; 先頭ラベル集合
   stmts              ; ブロック内stmtのベクタ
   (preds #:mutable)  ; predecessors (CFG中のインデックスの集合)
   (succs #:mutable)) ; successors (CFG中のインデックスの集合)
  #:transparent)

;;;; CFGアクセス関数

;; CFGに含まれる全ての文をリストにして返す
(define (all-stmts cfg)
  (append-map (lambda (bb)
                (vector->list (bblock-stmts bb)))
              (vector->list cfg)))

;; CFG中のtgt-stmtの位置(インデックスの組)を返す
;;   CFG, stmt -> (bblockインデックス . stmtインデックス)
(define (find-stmt cfg tgt-stmt)
  ;; 基本ブロックbb中のtgt-stmtの位置(stmtインデックス)のリストを返す
  (define (find-in-bb bb)
    (let* ((stmts (bblock-stmts bb))
           (jss (map (lambda (j stmt)
                       (cons j stmt))
                     (range 0 (vector-length stmts))
                     (vector->list stmts))))
      (map (lambda (js) (car js))
           (filter (lambda (js)
                     (let ((j (car js)) (stmt (cdr js)))
                       (eq? stmt tgt-stmt)))
                   jss))))
  (let ((rs (append-map (lambda (i bb)
                          (map (lambda (j) (cons i j))
                               (find-in-bb bb)))
                        (range 0 (vector-length cfg))
                        (vector->list cfg))))
    (unless (= (length rs) 1)
      (error "multiple or no stmts found:" rs))
    (first rs)))
           
;; stmtのpredecessorsを返す
;;   CFG, stmt -> stmtのリスト
(define (preds cfg stmt)
  (let* ((idx (find-stmt cfg stmt))
         (b-idx (car idx))
         (bb (vector-ref cfg b-idx))
         (stmts (bblock-stmts bb))
         (s-idx (cdr idx)))
    (cond
     ((and (zero? b-idx) (zero? s-idx)) ;; BEGIN
      '())
     ((zero? s-idx) ;; 基本ブロックの先頭の文
      (map (lambda (pb-idx)
             (let* ((pb (vector-ref cfg pb-idx))
                    (stmts (bblock-stmts pb)))
               (vector-ref stmts (- (vector-length stmts) 1))))
           (bblock-preds bb)))
     (else ;; 基本ブロックの先頭以外の文
      (list (vector-ref stmts (- s-idx 1)))))))

;; stmtのsuccessorsを返す
;;   CFG, stmt -> stmtのリスト
(define (succs cfg stmt)
  (let* ((idx (find-stmt cfg stmt))
         (b-idx (car idx))
         (bb (vector-ref cfg b-idx))
         (stmts (bblock-stmts bb))
         (s-idx (cdr idx)))
    (cond
     ((and (= b-idx (- (vector-length cfg) 1))
           (= s-idx (- (vector-length stmts) 1))) ;; END
      '())
     ((= s-idx (- (vector-length stmts) 1)) ;; 基本ブロックの末尾の文
      (map (lambda (sb-idx)
             (let* ((sb (vector-ref cfg sb-idx))
                    (stmts (bblock-stmts sb)))
               (vector-ref stmts 0)))
           (bblock-succs bb)))
     (else ;; 基本ブロックの末尾以外の文
      (list (vector-ref stmts (+ s-idx 1)))))))


;;;; CFG生成

;; ir中の各ラベルを直後の文へくっつける．
;; くっつけて出来るコンスペア (ラベル集合 . stmt) を以降「ラベル付き文」と呼ぶ．
;;   stmtリスト -> ラベル付き文のリスト
(define (coalesce-label ir)
  ((compose1 reverse (lambda (l-ir)
                       (map (lambda (l-stmt)
                              (cons (reverse (car l-stmt)) ; for readability
                                    (cdr l-stmt)))
                            l-ir)))
   (foldl (lambda (stmt l-ir)
            (if (null? l-ir)
                (if (ir-stx:label-stmt? stmt)
                    (cons (cons (list (ir-stx:label-stmt-name stmt))
                                #f)
                          l-ir)
                    (cons (cons '() stmt) l-ir))
                (let ((i (first l-ir))
                      (is (rest l-ir)))
                  (cond
                   ((and (ir-stx:label-stmt? stmt)
                         (not (cdr i)))
                    (cons (cons (cons (ir-stx:label-stmt-name stmt) (car i))
                                (cdr i))
                          is))
                   ((ir-stx:label-stmt? stmt)
                    (cons (cons (list (ir-stx:label-stmt-name stmt))
                                #f)
                          l-ir))
                   ((not (cdr i))
                    (cons (cons (car i) stmt)
                          is))
                   (else (cons (cons '() stmt) l-ir))))))
          '()
          ir)))

;; 基本ブロックの先頭にあたるラベル付き文をleaderと呼ぶ．
;; leader集合を見つけて返す
;;   ラベル付き文のリスト -> leader集合
(define (find-leaders l-ir)
  ;find-target関数:label-stmtを受け取って,そのラベルが入っている文を返す
  (define (find-target lbl)
    (first (memf (lambda (l-stmt)
                   (set-member? (car l-stmt) (ir-stx:label-stmt-name lbl)))
                 l-ir)))
  ;ここからが本体
  (if (null? l-ir)
      '()
      ((compose1 reverse cdr)           ;(compose f g x) := f(g(x))
       (foldl
        (lambda (l-stmt acc)
          (let ((stmt (cdr l-stmt))
                (is-leader (car acc))
                (leaders (cdr acc)))
            (let ((leaders2 (if is-leader
                                (set-add leaders l-stmt)
                                leaders)))
              ;ここの#t,#fはその文の次がleaderかどうかを表す.よく考えるとわかる.
              (cond ((eq? stmt 'BEGIN) (cons #t leaders2))
                    ((eq? stmt 'END) (cons #f (set-add leaders2 l-stmt)))
                    ((ir-stx:assign-stmt? stmt) (cons #f leaders2))
                    ((ir-stx:write-stmt? stmt) (cons #f leaders2))
                    ((ir-stx:read-stmt? stmt) (cons #f leaders2))
                    ((ir-stx:call-stmt? stmt) (cons #f leaders2))
                    ((ir-stx:print-stmt? stmt) (cons #f leaders2))
                    ((ir-stx:if-stmt? stmt)
                     (cons #t
                           (set-union
                            leaders2
                            `(,(find-target (ir-stx:if-stmt-tlabel stmt))
                              ,(find-target (ir-stx:if-stmt-elabel stmt))))))
                    ((ir-stx:goto-stmt? stmt)
                     (cons #t
                           (set-add
                            leaders2
                            (find-target (ir-stx:goto-stmt-label stmt)))))
                    ((ir-stx:ret-stmt? stmt)
                     (cons #t leaders2))
                    (else (error "unknown stmt:" stmt))))))
        (cons #t '())
        l-ir))))

;; 基本ブロックのコンストラクタ
;;   ラベル付き文のリスト -> エッジ情報が未設定のbblock
(define (make-bblock l-stmts)
  (bblock (car (first l-stmts))
          (list->vector (map cdr l-stmts))
          '() '()))

;; leader情報をつかってラベル付き文のリストを基本ブロックに分割
;;   ラベル付き文のリスト，leader集合 -> bblockのリスト
(define (split l-ir leaders)
  (reverse
   (map (compose1 make-bblock reverse)
        (foldl (lambda (l-stmt bbs)
                 (if (set-member? leaders l-stmt)
                     (cons (list l-stmt) bbs)
                     (cons (cons l-stmt (first bbs))
                           (rest bbs))))
               '()
               l-ir))))

;; 制御フローに従い，基本ブロック間にエッジを張る
;;   bblockのリスト, u-lbls -> エッジの張られたbblockのベクタ(つまりCFG)
;;     u-lbls: 実際に使用されたラベルを記録するためのmutable set
(define (set-edges bbs u-lbls)
  (define (add-pred bb p) ;; bbのpredsにpを追加
    (set-bblock-preds! bb (set-add (bblock-preds bb) p)))
  (define (add-succ bb s) ;; bbのsuccsにsを追加
    (set-bblock-succs! bb (set-add (bblock-succs bb) s)))
  (let* ((idx (range 0 (length bbs)))
         (end-idx (- (length bbs) 1))
         (labels->idx (map cons
                           (map bblock-labels bbs)
                           idx))
         (bbv (list->vector bbs)))
    ;; bbvのi,j番目の基本ブロック間に双方向のエッジを張る
    (define (add-edge i j)
      (add-succ (vector-ref bbv i) j)
      (add-pred (vector-ref bbv j) i))
    ;; 先頭ラベルにlblを含む基本ブロックを探し，そのインデックスを返す
    ;; また，見つかれば使用ラベル集合u-lblsにそのラベルを追加
    (define (find-target-idx lbl)
      (let ((idx (cdr (assf (lambda (lbls) (set-member? lbls lbl))
                            labels->idx))))
        (unless idx (error "target label not found:" lbl))
        (set-add! u-lbls lbl)
        idx))
    (for-each ;; 基本ブロックリスト(とそのインデックス)を先頭から順に処理
     (lambda (bb i)
       (let* ((stmts (bblock-stmts bb))
              (last-stmt (vector-ref stmts (- (vector-length stmts) 1))))
         (cond ;; 基本ブロックの末尾の文で場合分けしながら，適切なエッジを張る
          ((eq? last-stmt 'BEGIN) (add-edge i (+ i 1)))
          ((eq? last-stmt 'END) (void))
          ((ir-stx:assign-stmt? last-stmt) (add-edge i (+ i 1)))
          ((ir-stx:if-stmt? last-stmt)
           (let* ((ti (find-target-idx (ir-stx:label-stmt-name (ir-stx:if-stmt-tlabel last-stmt))))
                  (ei (find-target-idx (ir-stx:label-stmt-name (ir-stx:if-stmt-elabel last-stmt)))))
             (add-edge i ti)
             (add-edge i ei)))
          ((ir-stx:goto-stmt? last-stmt)
           (let* ((ti (find-target-idx (ir-stx:label-stmt-name (ir-stx:goto-stmt-label last-stmt)))))
             (add-edge i ti)))
          ((ir-stx:ret-stmt? last-stmt) (add-edge i end-idx))
          (else (error "unknown stmt:" last-stmt)))))
     bbs idx)
    bbv))

;; 各基本ブロックの先頭ラベル集合から未使用ラベルを取り除く
(define (gc-label cfg u-lbls)
  (for/vector ((bb cfg))
    (set-bblock-labels!
     bb
     (filter (lambda (lbl) (set-member? u-lbls lbl))
             (bblock-labels bb))))
  cfg)

;; 中間表現(stmtリスト) -> CFG
(define (ir->cfg ir)
  (let ((used-labels (mutable-seteq)))
    (let* ((l-ir (coalesce-label `(BEGIN ,@ir END)))
           (leaders (find-leaders l-ir))
           (bbs (split l-ir leaders)))
      (gc-label (set-edges bbs used-labels) used-labels))))


;;;; 各種表示

(define (labels->string lbls #:sep (sep ": ") #:after-last (lsep ":"))
  (string-join (map symbol->string lbls)
               sep #:after-last lsep))

(define (stmt->string stmt)
  (cond ((eq? stmt 'BEGIN) "<BEGIN>")
        ((eq? stmt 'END) "<END>")
        (else (ir-stmt->string stmt))))

;; cfg [, prop->str] -> 人間が読みやすい表現のstring
;;   cfg: control flow graph
;;   prop-str: stmt, 'before or 'after  -> string
(define (cfg->string cfg . opts)
  (unless (< (length opts) 2)
    (error "bad arguments:" opts))
  (let ((prop-str (if (empty? opts) #f (first opts))))
    (define (fmt-stmt s) (format "~a\n" (stmt->string s)))
    (define (fmt-prop s k) (format "[~a]\n" (prop-str s k)))
    (string-join
     (map (lambda (bb)
            (string-append
             (let ((s (labels->string (bblock-labels bb) #:after-last "")))
               (if (string=? s "")
                   ""
                   (string-append s ":\n")))
             (let ((stmts (vector->list (bblock-stmts bb))))
               (string-join
                (cons
                 (if prop-str (fmt-prop (first stmts) 'before) "")
                 (map (lambda (stmt)
                        (string-join
                         `(,(fmt-stmt stmt)
                           ,(if prop-str
                                (fmt-prop stmt 'after)
                                ""))
                         "  "))
                      stmts))
                "  " #:before-first "  "))))
          (vector->list cfg))
     "\n")))

;; cfg [, prop-str] -> DOT(グラフ描画言語)プログラムのstring
;;   cfg: control flow graph
;;   prop-str: stmt, 'before or 'after  -> string
(define (cfg->dot cfg . opts)
  (unless (< (length opts) 2)
    (error "bad arguments:" opts))
  (let ((prop-str (if (empty? opts) #f (first opts))))
    (define (escape s)
      (apply string-append
             (map (lambda (c)
                    (case c
                      ((#\> #\< #\{ #\}) (format "\\~a" c))
                      (else (format "~a" c))))
                  (string->list s))))
    (define (fmt-stmt s)
      (escape (format "~a\\l" (stmt->string s))))
    (define (fmt-prop s k)
      (escape (format "[~a]\\l" (prop-str s k))))
    (define (bblock->node bb i)
      (let ((stmts (vector->list (bblock-stmts bb))))
        (apply string-append
               `(,(format "bb~a [label=\"{" i)
                 ,(let ((s (labels->string (bblock-labels bb)
                                           #:sep "," #:after-last "")))
                    (if (string=? s "")
                        ""
                        (string-append s "\\l|")))
                 ,(if prop-str (fmt-prop (first stmts) 'before) "")
                 ,@(map (lambda (stmt)
                          (string-append
                           (fmt-stmt stmt)
                           (if prop-str
                               (fmt-prop stmt 'after)
                               "")))
                        stmts)
                 "}\"];\n"))))
    (define (bblock->edge bb i)
      (string-join (map (lambda (j)
                          (format "bb~a -> bb~a\n" i j))
                        (bblock-succs bb))
                   "  " #:after-last ""))
    (let* ((bbs (vector->list cfg))
           (idxs (range 0 (length bbs))))
      (string-append
       "digraph CFG {\n"
       "  node [shape=record fontname=\"courier\"]\n"
       (string-join (map bblock->node bbs idxs)
                    "  " #:before-first "  ")
       (string-join (map bblock->edge bbs idxs)
                    "  " #:before-first "  ")
       "}\n"))))

;main関数のcompound-stmtのリストのみを取り出す関数
(define (get-main-def ir)
  (if (and (ir-stx:fun-def? (car ir)) (equal? 'main (semantic:obj-name (ir-stx:fun-def-var (car ir)))))
      (ir-stx:fun-def-body (car ir))
      (get-main-def (cdr ir))))

;ir:ir-simple-displayとは別に作成

(define (exp->string exp)
  (cond
   ((semantic:obj? exp) (obj->string exp))
   ((ir-stx:var-exp? exp) (exp->string (ir-stx:var-exp-var exp)))
   ((ir-stx:lit-exp? exp) (format "~a" (ir-stx:lit-exp-val exp)))
   ((ir-stx:aop-exp? exp) (format "(~a~a~a)"
                           (exp->string (ir-stx:aop-exp-left exp))
                           (ir-stx:aop-exp-op exp)
                           (exp->string (ir-stx:aop-exp-right exp))))
   ((ir-stx:rop-exp? exp) (format "(~a~a~a)"
                           (exp->string (ir-stx:rop-exp-left exp))
                           (ir-stx:rop-exp-op exp)
                           (exp->string (ir-stx:rop-exp-right exp))))
   ((ir-stx:addr-exp? exp) (format "&~a"
                                   (exp->string (ir-stx:addr-exp-var exp))))
   (else (error "unknown exp:" exp))))

(define (obj->string obj)
  (if (null? obj)
      ""
      (symbol->string (semantic:obj-name obj))))

(define (ir-stmt->string stmt)
  (cond
    ((ir-stx:assign-stmt? stmt) (format "~a = ~a;"
                                       (obj->string (ir-stx:assign-stmt-var stmt))
                                       (exp->string (ir-stx:assign-stmt-exp stmt))))
    ((ir-stx:write-stmt? stmt) (format "*~a = ~a;"
                                      (obj->string (ir-stx:write-stmt-dest stmt))
                                      (obj->string (ir-stx:write-stmt-src stmt))))
    ((ir-stx:read-stmt? stmt) (format "~a = *~a;"
                                     (obj->string (ir-stx:read-stmt-dest stmt))
                                     (obj->string (ir-stx:read-stmt-src stmt))))
   
    ((ir-stx:label-stmt? stmt) (format "~a"
                                       (ir-stx:label-stmt-name stmt)))
    ((ir-stx:if-stmt? stmt) (format "if(~a) ~a ~a;"
                                    (obj->string (ir-stx:if-stmt-var stmt))
                                    (ir-stmt->string (ir-stx:if-stmt-tlabel stmt))
                                    (ir-stmt->string (ir-stx:if-stmt-elabel stmt))))
    ((ir-stx:goto-stmt? stmt) (format "goto ~a;"
                                      (ir-stmt->string (ir-stx:goto-stmt-label stmt))))
   ((ir-stx:ret-stmt? stmt) (if (null? (ir-stx:ret-stmt-var stmt))
                                "return;"
                                (format "return ~a;" (obj->string (ir-stx:ret-stmt-var stmt)))))
   ((ir-stx:call-stmt? stmt) 
    (if (null? (ir-stx:call-stmt-dest stmt))
        (format "~a(~a);"
                (obj->string (ir-stx:call-stmt-tgt stmt))
                (string-join (map obj->string (ir-stx:call-stmt-vars stmt)) ","))
        (format "~a = ~a(~a);"
                (obj->string (ir-stx:call-stmt-dest stmt))
                (obj->string (ir-stx:call-stmt-tgt stmt))
                (string-join (map obj->string (ir-stx:call-stmt-vars stmt)) ","))))
   ((ir-stx:print-stmt? stmt) (format "print(~a);"
                                      (obj->string (ir-stx:print-stmt-var stmt))))
   (else (error "unknown stmt:" stmt))))
