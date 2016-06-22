;;;; Generic Data Flow Analysis (a.k.a Monotone Framework)
#lang racket
(require "queue.rkt"
         (prefix-in ir: "../ir.rkt")
         (prefix-in cfg: "cfg.rkt"))
(provide (all-defined-out))
(provide analysis solve get-property)

(struct analysis
  (direction     ;; 'forward or 'backward
   transfer      ;; 伝達関数: stmt, プロパティ -> プロパティ
   prop-compare  ;; プロパティの比較関数:
                 ;;   順序がつくなら，-1, 0, 1のいずれかを返す
                 ;;   順序がつかないなら，#fを返す
   lub           ;; プロパティのleast upper bound関数
   bot           ;; 最小プロパティ
   init          ;; 初期プロパティ
   )
  #:transparent)

;; データフロー解析を実行
;; analysis, CFG -> 解析結果
;;   解析結果: (辞書(stmt -> 直前のプロパティ) . 辞書(stmt -> 直後のプロパティ))
(define (solve anlys cfg)
  ;引数のanlys構造体からそれぞれのメンバを取り出し
  (let ((direction    (analysis-direction    anlys));解析方向
        (transfer     (analysis-transfer     anlys));伝達関数
        (prop-compare (analysis-prop-compare anlys));leftとrightを受け取って-1/0/1/#fを返す関数
        (lub          (analysis-lub          anlys))
        (bot          (analysis-bot          anlys));hasheq
        (init         (analysis-init         anlys)));中間表現に出てくる変数についての (変数,'(neg zero pos))という辞書集合(辞書はhasheqテーブル)
    ;; プロパティ辞書esからstmtに対応するプロパティを取得．なければbotを返す
    (define (get-prop es stmt)
      (dict-ref es stmt bot))
    ;; ワークリストwlが空になるまでプロパティ辞書entries, exitsの更新を繰り返す．
    ;; directionがforwardの場合，entriesは直前のプロパティの辞書，exitsは直後の
    ;; プロパティの辞書．backwardの場合は逆．
    ;; ワークリストが空になった時点のentries, exitsが不動点，すなわち解析結果．
    ;; ワークリストは (stmt . プロパティ)のキュー．stmtの直前のプロパティへの
    ;; 追加を意味する．
    (define (fixed-point wl entries exits)
      (let ((w      (car (dequeue wl))) ;キューから取り出したの先頭にあった値.
            (new-wl (cdr (dequeue wl))));キューから先頭の要素をpopしたのキュー
        (if w
            ;キューが空ではなかった場合
            ;(car w)はプログラムの1文,(cdr w)はそのプログラム持っている変数についての情報
            (let* ((stmt (car w))
                   (old-entry (get-prop entries stmt))  ;直前の辞書[現在のプログラム文] 以前の変数のプロパティ辞書をold-entryに入れる
                   (new-entry (lub old-entry (cdr w)))) ;(cdr w)は辞書; 追加を考慮し再計算
              (case (prop-compare new-entry old-entry) ;; 差分の有無で場合分け
                ((0) (fixed-point new-wl entries exits))
                ((1) (let* ((old-exit (get-prop exits stmt))
                           (new-exit (transfer stmt new-entry)))
                       (case (prop-compare new-exit old-exit);;差分の有無で場合分け
                         ((0) (fixed-point
                               new-wl
                               (dict-set entries stmt new-entry)
                               exits))
                         ((1) (fixed-point
                               (foldl (lambda (succ wl) ;; wlにsuccsを追加
                                        (enqueue wl (cons succ new-exit)))
                                      new-wl
                                      ((if (eq? direction 'forward)
                                           cfg:succs
                                           cfg:preds) cfg stmt))
                               (dict-set entries stmt new-entry)
                               (dict-set exits stmt new-exit)))
                         (else (error "transfer not monotone")))))
                (else (error "lub operation not adequate"))))
            ;キューが空だった場合
            (if (eq? direction 'forward)
                (cons entries exits) ;; 直前=entries，直後=exits
                (cons exits entries) ;; 直前=exits，直後=entries
                ))))
    ;ここよりしたが関数本体
    
    ;stmtsはすべてのプログラムのリスト(cfg:all-stmtsがcfg内のプログラムをすべてリストに戻す関数
    ;解析方向directionがforward以外であれば反対方向とするのでreverseする
    (let* ((stmts ((compose1 (if (eq? direction 'forward)
                                 identity ;identityは受け取ったものをそのまま返す関数
                                 reverse)
                             cfg:all-stmts)
                   cfg))
           ;; entries, exitsの初期値(すべての文のプロパティがbot)
           ;; entries, exits自体がhasheqテーブル(eqによって要素の区別をする辞書)である.
           ;; entries, exitsの初期値init-propは,プログラムののリストすべてをKeyとして持っており,Valueに空の辞書(空のhasheqテーブル)をもつ
           (init-prop (foldl (lambda (stmt prop)
                               (dict-set prop stmt bot))
                             (hasheq)
                             stmts)))
      ;上で定義したfixed-point関数を実際に呼び出し,解析を開始
      ;キュー初期値はの(cons 一番はじめの文 データ)
      ; データとはhasheqテーブルの辞書で,キーが変数でvalueがneg zero posもの
      (fixed-point (enqueue (make-queue)
                            (cons (first stmts) init))
                   init-prop init-prop))))

;; 解析結果から特定のプロパティを取得
;;   kind: before(直前), after(直後), both(両方)
(define (get-property solution stmt #:kind (kind 'both))
  (let ((b-prop (dict-ref (car solution) stmt))
        (a-prop (dict-ref (cdr solution) stmt)))
    (case kind
      ((both) (cons b-prop a-prop))
      ((before) b-prop)
      ((after) a-prop)
      (else (error "unknown kind:" kind)))))
