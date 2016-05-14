#lang racket
(provide (all-defined-out))
; ; プログラムは var-decl と fun-def のリスト
; 変数宣言
(struct var-decl (var) #:transparent)
; 関数定義
(struct fun-def (var parms body) #:transparent) ; parms は var-decl のリスト
; ; 文
; 変数への代入: <var> = <exp>;
(struct assign-stmt (var exp) #:transparent)
; メモリへの書き込み: *<dest> = <src>;
(struct write-stmt (dest src) #:transparent)
; メモリ参照: <dest> = *<src>;
(struct read-stmt (dest src) #:transparent)
; ラベル: <name>:
(struct label-stmt (name) #:transparent)
; 条件分岐: if(<var>){ goto <tlabel>; } else { goto <elabel>; }
(struct if-stmt (var tlabel elabel) #:transparent)
; 無条件分岐: goto <label>;
(struct goto-stmt (label) #:transparent)
; 関数呼出し: <dest> = <tgt>(<var1>, <var2>, <var3>, ...);
(struct call-stmt (dest tgt vars) #:transparent) ; vars は var のリスト
; リターン: return <var>;
(struct ret-stmt (var) #:transparent)
; 値の出力: print(<var>);
(struct print-stmt (var) #:transparent)
; 複文: {<decls> <stmts>}
(struct cmpd-stmt (decls stmts) #:transparent) ; decls は var-decl のリスト，stmts は文のリスト
 
; ; 式
; 変数参照
(struct var-exp (var) #:transparent)
; 整数即値
(struct lit-exp (val) #:transparent)
; 算術演算
(struct aop-exp (op left right) #:transparent)
; 比較演算
(struct rop-exp (op left right) #:transparent)
; アドレス取得: &<var>
(struct addr-exp (var) #:transparent)

