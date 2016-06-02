#lang racket
(provide (all-defined-out))
(require (prefix-in parser: "parser.rkt")
         (prefix-in semantic: "semantic.rkt")
         (prefix-in stx: "parser-syntax.rkt")
         (prefix-in ir-stx:    "ir-syntax.rkt")
         (prefix-in ir: "ir.rkt")
         (prefix-in addr: "addr.rkt"))

;; registers

(define zero 'zero) ; constant 0
(define at 'at) ; reserved for assembler

(define v0 'v0) ; expression evaluation and results of a function
(define v1 'v1) ; expression evaluation and results of a function

(define a0 'a0) ; argument 1
(define a1 'a1) ; argument 2
(define a2 'a2) ; argument 3
(define a3 'a3) ; argument 4

(define t0 't0) ; temporary (not preserved accross call)
(define t1 't1) ; temporary (not preserved accross call)
(define t2 't2) ; temporary (not preserved accross call)
(define t3 't3) ; temporary (not preserved accross call)
(define t4 't4) ; temporary (not preserved accross call)
(define t5 't5) ; temporary (not preserved accross call)
(define t6 't6) ; temporary (not preserved accross call)
(define t7 't7) ; temporary (not preserved accross call)
(define t8 't8) ; temporary (not preserved accross call)
(define t9 't9) ; temporary (not preserved accross call)

(define s0 's0) ; saved temporary (preserved accross call)
(define s1 's1) ; saved temporary (preserved accross call)
(define s2 's2) ; saved temporary (preserved accross call)
(define s3 's3) ; saved temporary (preserved accross call)
(define s4 's4) ; saved temporary (preserved accross call)
(define s5 's5) ; saved temporary (preserved accross call)
(define s6 's6) ; saved temporary (preserved accross call)
(define s7 's7) ; saved temporary (preserved accross call)

(define k0 'k0) ; reserved for OS kernel
(define k1 'k1) ; reserved for OS kernel

(define gp 'gp) ; pointer to global area
(define sp 'sp) ; stack pointer
(define fp 'fp) ; frame pointer
(define ra 'ra) ; return address (used by function call)

;; opcode

(define abs     'abs) ; abs rdest, rsrc

(define add     'add)   ; add   rd, rs, rt
(define addi    'addi)  ; addi  rt, rs, imm
(define addiu   'addiu) ; addiu rt, rs, imm
(define addu    'addu)  ; addu  rd, rs, rt

(define op-and  'and)  ; and  rd, rs, rt
(define andi    'andi) ; andi rt, rs, imm

(define b       'b)      ; b      label
(define bclf    'bclf)   ; bclf   cc lael
(define bclt    'bclt)   ; bclt   cc lael
(define beq     'beq)    ; beq    rs, rt, label
(define beqz    'beqz)   ; beqz   rsrc, label
(define bge     'bge)    ; bge    rsrc1, rsrc2, label
(define bgeu    'bgeu)   ; bgeu   rsrc1, rsrc2, label
(define bgez    'bgez)   ; bgez   rs, label
(define bgezal  'bgezal) ; bgezal rs, label
(define bgt     'bgt)    ; bgt    rsrc1, src2, label
(define bgtu    'bgtu)   ; bgtu   rsrc1, src2, label
(define bgtz    'bgtz)   ; bgtz   rs, label
(define ble     'ble)    ; ble    rsrc1, src2, label
(define bleu    'bleu)   ; bleu   rsrc1, src2, label
(define blez    'blez)   ; blez   rs, label
(define blt     'blt)    ; blt    rsrc1, rsrc2, label
(define bltu    'bltu)   ; bltu   rsrc1, rsrc2, label
(define bltz    'bltz)   ; bltz   rs, label
(define bltzal  'bltzal) ; bltzal rs, label
(define bne     'bne)    ; bne    rs, rt, label
(define bnez    'bnez)   ; bnez   rsrc, label


(define clo     'clo) ; clo rd, rs
(define clz     'clz) ; clz rd, rs

(define div     'div)  ; div  rs, rt / div  rdest, rsrc1, src2
(define divu    'divu) ; divu rs, rt / divu rdest, rsrc1, src2

(define j       'j)    ; j    target
(define jal     'jal)  ; jal  target
(define jalr    'jalr) ; jalr rs, rd
(define jr      'jr)   ; jr   rs

(define li      'li)  ; li  rdest, imm
(define lui     'lui) ; lui rt, imm

(define la      'la)   ; la   rdest, address
(define lb      'lb)   ; lb   rt, address
(define lbu     'lbu)  ; lbu  rt, address
(define ld      'ld)   ; ld   rdest, address
(define lh      'lh)   ; lh   rt, address
(define lhu     'lhu)  ; lhu  rt, address
(define ll      'll)   ; ll   rt, address
(define lw      'lw)   ; lw   rt, address
(define lwc1    'lwc1) ; lwc1 ft, address
(define lwl     'lwl)  ; lwl  rt, address
(define lwr     'lwr)  ; lwr  rt, address
(define ulh     'ulh)  ; ulh  rdest, address
(define ulhu    'ulhu) ; ulhu rdest, address
(define ulw     'ulw)  ; ulw  rdest, address

(define move    'move) ; move rdest rsrc
(define movf    'movf) ; movf rd, rs, cc
(define movn    'movn) ; movn rd, rs, rt
(define movt    'movt) ; movt rd, rs, cc
(define movz    'movz) ; movz rd, rs, rt
(define mfc0    'mfc0) ; mfc0 rt, rd
(define mfc1    'mfc1) ; mfc1 rt, fs
(define mfhi    'mfhi) ; mfhi rd
(define mflo    'mflo) ; mflo rd
(define mthi    'mthi) ; mthi rs
(define mtlo    'mtlo) ; mtlo rs
(define mtc0    'mtc0) ; mtc0 rd, rt
(define mtc1    'mtc1) ; mtc1 rd, fs

(define madd    'madd)  ; madd  rs, rt
(define maddu   'maddu) ; maddu rs, rt

(define msub    'msub)  ; msub  rs, rt
(define msubu   'msubu) ; msubu rs, rt

(define mul     'mul)   ; mul   rd, rs, rt
(define mulo    'mulo)  ; mulo  rdest, rsrc1, src2
(define mulou   'mulou) ; mulou rdest, rsrc1, src2

(define mult    'mult)  ; mult  rs, rt
(define multu   'multu) ; multu rs, rt

(define neg     'neg)  ; neg  rdest, rsrc
(define negu    'negu) ; negu rdest, rsrc

(define nop     'nop) ; nop

(define nor     'nor) ; nor rd, rs, rt

(define op-not     'not) ; not rdest, rsrc

(define op-or   'or)  ; or  rd, rs, rt
(define ori     'ori) ; ori rt, rs, imm

(define rem     'rem)  ; rem rdest, rsrc1, rsrc2
(define remu    'remu) ; rem rdest, rsrc1, rsrc2

(define rol     'rol)  ; rol rdest, rsrc1, rsrc2
(define ror     'ror)  ; ror rdest, rsrc1, rsrc2

(define sb      'sb)   ; sb   rt, address
(define sc      'sc)   ; sc   rt, address
(define sd      'sd)   ; sd   rsrc, address
(define sh      'sh)   ; sh   rt, address
(define sw      'sw)   ; sw   rt, address
(define swc1    'swc1) ; swc1 ft, address
(define sdc1    'sdc1) ; sdc1 ft, address
(define swl     'swl)  ; swl  rt, address
(define swr     'swr)  ; swr  rt, address
(define ush     'ush)  ; ush  rsrc, address
(define usw     'usw)  ; usw  rsrc, address

(define seq     'seq)   ; seq   rdest, rsrc1, rsrc2
(define sge     'sge)   ; sge   rdest, rsrc1, rsrc2
(define sgeu    'sgeu)  ; sgeu  rdest, rsrc1, rsrc2
(define sgt     'sgt)   ; sgt   rdest, rsrc1, rsrc2
(define sgtu    'sgtu)  ; sgtu  rdest, rsrc1, rsrc2
(define sle     'sle)   ; sle   rdest, rsrc1, rsrc2
(define sleu    'sleu)  ; sleu  rdest, rsrc1, rsrc2
(define slt     'slt)   ; slt   rd, rs, rt
(define slti    'slti)  ; sltu  rt, rs, imm
(define sltiu   'sltiu) ; sltiu rt, rs, imm
(define sltu    'sltu)  ; sltu  rd, rs, rt
(define sne     'sne)   ; sne   rdest, rsrc1, rsrc2

(define sll     'sll)  ; sll  rd, rt, shamt
(define sllv    'sllv) ; sllv rd, rt, rs
(define sra     'sra)  ; sra  rd, rt, shamt
(define srav    'srav) ; srav rd, rt, rs
(define srl     'sra)  ; srl  rd, rt, shamt
(define srlv    'srav) ; srlv rd, rt, rs

(define sub     'sub)  ; sub  rd, rs, rt
(define subu    'subu) ; subu rd, rs, rt

(define syscall 'syscall) ; syscall

(define xor     'xor)  ; xor  rd, rs, rt
(define xori    'xori) ; xori rt, rs, imm

;; directives
(define .align  '.align)
(define .ascii  '.ascii)
(define .asciiz '.asciiz)
(define .byte   '.byte)
(define .data   '.data)
(define .double '.double)
(define .extern '.extern)
(define .float  '.float)
(define .globl  '.globl)
(define .half   '.half)
(define .kdata  '.kdata)
(define .ktext  '.ktext)
(define .set    '.set)
(define .space  '.space)
(define .text   '.text)
(define .word   '.word)


;; assembly emitter

;; $r
(define ($ r)
  `($ ,r))

;; i($p)
(define (-> p i)
  `(-> ,p ,i))

;; lbl:
(define (emit-label lbl)
  `(#:label ,lbl))

;; emit opcode
(define (emit op . args)
  `(,op ,@args))

;; header of executable code
(define asm-header `(,(emit .data)
                     ,(emit-label "newline")
                     ,(emit .asciiz "\"\\n\"")))
                     
;; stringify assembly code

(define (arg->string arg)
  (cond ((string? arg) arg)
        ((symbol? arg) (symbol->string arg))
        ((number? arg) (number->string arg))
        ((eq? (first arg) '$)
         (format "$~a" (second arg)))
        ((eq? (first arg) '->)
         (format "~a(~a)" (third arg) (arg->string (second arg))))
        (else (format "gen: unknown argument: ~a" arg))))

(define (instr->string instr)
  (case (first instr)
    ((#:label) (format "~a:" (second instr)))
    (("gb") (format "~a" (second instr)))
    (else (format "\t~a\t~a"
                  (symbol->string (first instr))
                  (string-join (map arg->string (rest instr))
                               ", ")))))

;; generated spim code -> string
(define (code->string code)
  (string-join (map instr->string code) "\n"))

;; ofs(n) -> "n($fp)"
;;$fp+vofsの位置のメモリを指す表記を返す
(define (ofs-addr vofs)
  (-> ($ fp) vofs))

;destレジスタにobjを読み込む命令のリストを返す
(define (emit-load-obj dest tmp tgt-obj)
  (if (isglobal tgt-obj)
      `(,(emit la ($ tmp) (semantic:obj-name tgt-obj))
        ,(emit lw ($ dest) (-> ($ tmp) 0)))
      `(,(emit-load-ofs dest (semantic:obj-ofs tgt-obj)))))
      
;; register -> ofs(n) -> code of the form "lw $dest n($fp)"
;destレジスタに$fp+vofsの位置のメモリの値を格納する表記を返す
(define (emit-load-ofs dest vofs)
  (emit lw ($ dest) (ofs-addr vofs)))

;srcレジスタの値をobjに格納するのリスト返す
(define (emit-store-obj src tmp tgt-obj)
  (if (isglobal tgt-obj)
      `(,(emit la ($ tmp) (semantic:obj-name tgt-obj))
        ,(emit sw ($ src) (-> ($ tmp) 0)))
      `(,(emit-store-ofs src (semantic:obj-ofs tgt-obj)))))
;; register -> ofs(n) -> code of the form "sw $src n($fp)"
;;srcレジスタの値をメモリの$fp+vofsの位置に格納する表記を返す
(define (emit-store-ofs src vofs)
  (emit sw ($ src) (ofs-addr vofs)))

;; stmt list -> asm code list
(define (gen-stmts stmts)
  (append-map (lambda (stmt)
                (gen-stmt stmt))
              stmts))

(define now-func-stack-size 0)
(define now-stack-pop-size 0)
;objがグローバルかどうか調べる関数
(define (isglobal tgt-obj)
  (let ((type (semantic:obj-type tgt-obj))
        (lev (semantic:obj-lev tgt-obj)))
    (if (equal? 'tmp type)
         #f
         (if (equal? 0 lev)
             #t
             #f))))
;global変数を探す関数
(define (gen-stmt-global stmt)
  (cond ((list? stmt)
         (ir:list-nest-append (map gen-stmt-global stmt)))
        ((ir-stx:var-decl? stmt)
         (let* ((tgt-obj (ir-stx:var-decl-var stmt))
                (type (semantic:obj-type tgt-obj)))
           (if (equal? (car type) 'array)
               `(,(emit "gb" (string-append (symbol->string (semantic:obj-name tgt-obj)) ": .space " (number->string (* 4 (car (reverse type)))))))
               `(,(emit "gb" (string-append (symbol->string (semantic:obj-name tgt-obj)) ": .word 0" ))))))
        (else `())))
;; stmt -> asm code list
(define (gen-stmt stmt)
  (cond ((null? stmt) `())
        ((list? stmt) 
         (ir:list-nest-append (map gen-stmt stmt)))
        ((ir-stx:var-decl? stmt) `())
        ((ir-stx:fun-def? stmt)
         ;fun-def-varのobj構造体のofsにはその関数で使っている変数のofs(負)の最小値が書かれている
         (let* ((variable_size (semantic:obj-ofs (ir-stx:fun-def-var stmt)))
                (stack_size (- variable_size 8)) ;$fp,$ra退避する分追加
                (memory_param_num (max 0 (- (length (ir-stx:fun-def-parms stmt)) 4))) ;メモリに入っている($fpより上にある引数の数)
                (hoge (set! now-func-stack-size stack_size))
                (fuga (set! now-stack-pop-size (* -1 (- (- stack_size 4) (* 4 memory_param_num ))))))
           `(;header
             ,(emit-label (symbol->string (semantic:obj-name (ir-stx:fun-def-var stmt))))
             ,(emit addiu ($ sp) ($ sp) (* -1 now-stack-pop-size)) ;$spさげる
             ,(emit sw ($ fp) (-> ($ sp) 4))              ;$fp退避
             ,(emit sw ($ ra) (-> ($ sp) 0))              ;$ra退避
             ,(emit addiu ($ fp) ($ sp) (* -1 stack_size));$fp設定
             ;関数に引数がある時は引数をメモリに入れる
             ,@(append
                (let ((paramindex 0))
                  (ir:list-nest-append
                   (map
                    (lambda (x)
                      (begin (let  
                                 ((rst (cond
                                         ((equal? paramindex 0) `(,(emit sw ($ a0) (-> ($ fp) 0))))
                                         ((equal? paramindex 1) `(,(emit sw ($ a1) (-> ($ fp) -4))))
                                         ((equal? paramindex 2) `(,(emit sw ($ a2) (-> ($ fp) -8))))
                                         ((equal? paramindex 3) `(,(emit sw ($ a3) (-> ($ fp) -12))))
                                         (else `(,(emit lw ($ t0) (-> ($ fp) (* 4 (- paramindex 3))))
                                                 ,(emit sw ($ t0) (-> ($ fp) (* -4 paramindex))))))))
                               (set! paramindex (+ paramindex 1))
                               rst)))
                    (ir-stx:fun-def-parms stmt))))
                  ;関数本体
                  (gen-stmt (ir-stx:fun-def-body stmt)))
             ;footer
             ,(emit lw ($ fp) (-> ($ sp) 4)) 　　　　　　　        ;fp復元
             ,(emit lw ($ ra) (-> ($ sp) 0))                     ;ra復元
             ,(emit addiu ($ sp) ($ sp) now-stack-pop-size) ;spをpop
             ,(emit jr ($ ra)))))                                ;呼び出し元に戻る
        ((ir-stx:assign-stmt? stmt)
         (let ((dest (ir-stx:assign-stmt-var stmt)))
           `(,@(gen-exp t0 (ir-stx:assign-stmt-exp stmt))
             ,@(emit-store-obj t0 t1 dest))))
        ((ir-stx:write-stmt? stmt)
         (let ((dest (ir-stx:write-stmt-dest stmt))
               (src (ir-stx:write-stmt-src stmt)))
           `(,@(emit-load-obj t0 t1 dest)
             ,@(emit-load-obj t1 t2 src)
             ,(emit sw ($ t1) (-> ($ t0) 0)))))
        ((ir-stx:read-stmt? stmt)                 ;dest = *src          
         (let ((dest (ir-stx:read-stmt-dest stmt))           
               (src (ir-stx:read-stmt-src stmt)))
           `(,@(emit-load-obj t0 t1 src)
             ,(emit lw ($ t0) (-> ($ t0) 0))
             ,@(emit-store-obj t0 t1 dest))))
        ((ir-stx:label-stmt? stmt)
         (list (emit-label (ir-stx:label-stmt-name stmt))))
        ((ir-stx:if-stmt? stmt)
         (let ((test (ir-stx:if-stmt-var stmt))
               (tlabel (ir-stx:if-stmt-tlabel stmt))
               (elabel (ir-stx:if-stmt-elabel stmt)))
           `(,@(emit-load-obj t0 t1 test)
             ,(emit beqz ($ t0) (ir-stx:label-stmt-name elabel))
             ,(emit j (ir-stx:label-stmt-name tlabel)))))
        ((ir-stx:goto-stmt? stmt)
         (list (emit j (ir-stx:label-stmt-name (ir-stx:goto-stmt-label stmt)))))
        ((ir-stx:print-stmt? stmt)
         `(,@(emit-load-obj a0 t0 (ir-stx:print-stmt-var stmt))
           ,(emit li ($ v0) 1)
           ,(emit syscall)
           ,(emit li ($ v0) 4)
           ,(emit la ($ a0) "newline")
           ,(emit syscall)))
        ((ir-stx:ret-stmt? stmt)
         (append
          ;結果の代入
          (if (null? (ir-stx:ret-stmt-var stmt))
              `()
              (emit-load-obj v0 t0 (ir-stx:ret-stmt-var stmt)))
          ;fp,ra,sp戻す
          `(,(emit lw ($ fp) (-> ($ sp) 4))                     ;fp復元
            ,(emit lw ($ ra) (-> ($ sp) 0))                     ;ra復元
            ,(emit addiu ($ sp) ($ sp) now-stack-pop-size) ;spをpop
            ,(emit jr ($ ra)))))                                ;呼び出し元に戻る
        ((ir-stx:call-stmt? stmt)
         ;呼び出し前の準備
         ;4つめまでは$a0~$a4に,それ以降は$fp+4,8,12...に値を入れる
         (append
          (ir:list-nest-append
           (let ((paramindex 0))
             (map (lambda (x)
                    (let ((rst
                           (cond
                             ((equal? 0 paramindex) `(,(emit-load-ofs a0 (semantic:obj-ofs x))))
                             ((equal? 1 paramindex) `(,(emit-load-ofs a1 (semantic:obj-ofs x))))
                             ((equal? 2 paramindex) `(,(emit-load-ofs a2 (semantic:obj-ofs x))))
                             ((equal? 3 paramindex) `(,(emit-load-ofs a3 (semantic:obj-ofs x))))
                             (else `(,(emit-load-ofs t0 (semantic:obj-ofs x))
                                     ,(emit-store-ofs t0 (+ now-func-stack-size (* -4 (- (+ 1 (length (ir-stx:call-stmt-vars stmt))) 4 (- paramindex 3))))))))))
                      (begin
                        (set! paramindex (+ 1 paramindex))
                        rst)))
                  (ir-stx:call-stmt-vars stmt))))
          ;jump
          `(,(emit jal (semantic:obj-name (ir-stx:call-stmt-tgt stmt))))
          ;結果を代入
          (if (null? (ir-stx:call-stmt-dest stmt))
              `()
              (emit-store-obj v0 t0 (ir-stx:call-stmt-dest stmt)))))
           
        ((ir-stx:cmpd-stmt? stmt)
         (ir:list-nest-append (map gen-stmt (ir-stx:cmpd-stmt-stmts stmt))))
        (else '())))

;; register -> exp -> asm code list
;; destレジスタにir-stx:expの値を入れる命令リストを返す
(define (gen-exp dest exp)
  (cond ((ir-stx:var-exp? exp)
         ;globalの時はlalw
         
         ;arrayである時はt0にofsを入れるを返す
         (let ((var (ir-stx:var-exp-var exp)))
           (if (and (not (equal? (semantic:obj-type var) 'tmp))
                    (equal? (car (semantic:obj-type var)) 'array))
               (list (emit addiu ($ t0) ($ fp) (semantic:obj-ofs var)))
               (list (emit-load-ofs  t0 (semantic:obj-ofs var))))))
        ((ir-stx:lit-exp? exp)
         (list (emit li ($ dest) (ir-stx:lit-exp-val exp))))
        ((ir-stx:aop-exp? exp)
         `(,(emit-load-ofs t0 (semantic:obj-ofs (ir-stx:aop-exp-left exp)))
           ,(emit-load-ofs t1 (semantic:obj-ofs (ir-stx:aop-exp-right exp)))
           ,(emit (case (ir-stx:aop-exp-op exp)
                    ((+) add)
                    ((-) sub)
                    ((*) mul)
                    ((/) div))
                  ($ dest) ($ t0) ($ t1))))
        ((ir-stx:rop-exp? exp)
         `(,(emit-load-ofs t0 (semantic:obj-ofs (ir-stx:rop-exp-left exp)))
           ,(emit-load-ofs t1 (semantic:obj-ofs (ir-stx:rop-exp-right exp)))
           ,(emit (case (ir-stx:rop-exp-op exp)
                    ((==) seq)
                    ((!=) sne)
                    ((<)  slt)
                    ((<=) sle)
                    ((>)  sgt)
                    ((>=) sge))
                  ($ dest) ($ t0) ($ t1))))
        ((ir-stx:addr-exp? exp)
         (let ((src (ir-stx:addr-exp-var exp)))
           (list (emit addiu ($ t0) ($ fp) (semantic:obj-ofs src)))))
        (else (begin (display exp) (error "compiler internal error.")))))

;; addr-assigned-ir -> asm code list
(define (gen-code addr-ir)
  (begin
    (set! now-func-stack-size 0)
    (set! now-stack-pop-size 0)
    `(,@asm-header
      ,@(gen-stmt-global addr-ir)
      ,(emit .text)
      ,(emit .globl "main")
      ,@(gen-stmts addr-ir))))

(define (test filename)
  (display (code->string  (gen-code (addr:assign-addr (ir:ir-main filename))))))