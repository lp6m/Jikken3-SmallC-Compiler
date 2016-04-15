#lang racket
(provide (all-defined-out))

;; 整数即値: <num>
(struct lit-exp    (val pos)           #:transparent)
;; 変数: <var>
(struct var-exp    (tgt pos)           #:transparent)
;; 符号反転: -<exp>
(struct neg-exp    (arg pos)           #:transparent)
;; メモリ参照: *<exp>
(struct deref-exp   (arg pos)           #:transparent)
;; アドレスを取る: &<var>
(struct addr-exp  (var pos)           #:transparent)
;;配列参照
(struct array-exp (tgt index pos) #:transparent)
;;関数呼び出し
(struct funccall-exp (tgt paramlist pos) #:transparent)
;;変数宣言文
(struct declaration (declist pos) #:transparent)
;;変数宣言 変数部 - not pointer
;(struct declarator-notpointer (id pos) #:transparent)
;;変数宣言 変数部 - pointer
;(struct declarator-pointer (id pos) #:transparent)
;;
;;(struct direct-declarator-var (id pos) #:transparent)
;;
;;(struct direct-declarator-array(id num pos) #:transparent)
;;
(struct func-prototype (type id declarator pos) #:transparent)
(struct func-definition (type declarator statement pos) #:transparent)
;;
(struct param-declaration (type declarator pos) #:transparent)

;; 算術演算: <left-exp> <op> <right-exp>
(struct aop-exp    (op left right pos) #:transparent)
;; 比較演算: <left-exp> <op> <right-exp>
(struct rop-exp    (op left right pos) #:transparent)

;; 変数への代入: <var> = <exp>;
(struct assign-stmt  (var src pos)              #:transparent)
;; 条件分岐: if(<exp>) <cmpd-stmt> else <cmpd-stmt>
(struct if-stmt      (test tbody ebody pos)     #:transparent)
;; 繰り返し: while(<exp>) <cmpd-stmt>
(struct while-stmt   (test body pos)            #:transparent)
;; for(initial; test; repeat) body
(struct for-stmt (initial test repeat body pos) #:transparent) 
;;RETURN
(struct return-stmt (var pos) #:transparent)
;;int,void
(struct int-id (pos) #:transparent)
(struct void-id (pos) #:transparent)
;;
(struct logical-and-or-expr (op log1 log2 pos) #:transparent)
;;
(struct expression (exp assign-exp pos) #:transparent)