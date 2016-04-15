#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         (prefix-in stx: "syntax.rkt")
         )
(define ... (void)) ;; indicates a part to be implemented

 
(define-tokens tokens-with-value
  (NUM ID))

(define-empty-tokens tokens-without-value
  (+ - * /
   < <= > >= == !=
   & && || =
   SEMI LPAR RPAR COMMA RETURN
   LBRA RBRA LBBRA RBBRA
   INT VOID
   IF ELSE WHILE FOR
   EOF))

(define-lex-trans uinteger
  (syntax-rules () ((_ d) (:* d))))

(define-lex-abbrevs
  (digit            (char-range "0" "9"))
  (digit-non-zero   (char-range "1" "9"))
  (number  (:or "0"
                (:: digit-non-zero
                    (uinteger digit))))
  (identifier-char (:or (char-range "a" "z")
                        (char-range "A" "Z")))
  (identifier (:: identifier-char
                  (:* (:or identifier-char digit "_")))))
 
(define small-c-lexer
  (lexer-src-pos
   ("+"        (token-+))
   ("-"        (token--))
   ("*"        (token-*))
   ("/"        (token-/))
   ("<"        (token-<))
   ("<="       (token-<=))
   (">"        (token->))
   (">="       (token->=))
   ("=="       (token-==))
   ("!="       (token-!=))
   ("&"        (token-&))
   ("&&"       (token-&&))
   ("||"       (token-||))
   ("="        (token-=))
   (";"        (token-SEMI))
   ("("        (token-LPAR))
   (")"        (token-RPAR))
   ("{"        (token-LBRA))
   ("}"        (token-RBRA))
   ("["        (token-LBBRA))
   ("]"        (token-RBBRA))
   (","        (token-COMMA))
   ("return"   (token-RETURN))
   ("if"       (token-IF))
   ("else"     (token-ELSE))
   ("while"    (token-WHILE))
   ("for"      (token-FOR))
   ("int"      (token-INT))
   ("void"     (token-VOID))
   (number     (token-NUM (string->number lexeme)))
   (identifier (token-ID (string->symbol lexeme)))
   (whitespace (return-without-pos (small-c-lexer input-port)))
   ((eof)      (token-EOF))))
 
(define small-c-parser
  (parser
   (start program)
   (end EOF)
   (src-pos)
   ;;(debug "small-c-parser.tbl")
   (suppress)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error (format "parse error:~a,~a: ~a"
                           (position-line start-pos)
                           (position-col start-pos)
                           (if tok-value tok-value tok-name)))))
   (tokens tokens-with-value tokens-without-value)
   (grammar
    (program
     ((external-declaration) (list $1))
     ((program external-declaration) `(,@$1 ,$2)))
    (external-declaration
     ((declaration) $1)
     ((function-prototype) $1)
     ((function-definition) $1))
    (declaration
     ((type-specifier declarator-list SEMI) 
      (stx:declaration
       (map (lambda (x) 
              (let ((param (cadr x))
                   (isarraylist (cddr x))
                   (pointer_ka (car x)))
                (if (null? isarraylist)
                    (cons param `('() ,pointer_ka ,$1)) ;not array
                    (cons param `(,(car isarraylist) ,pointer_ka ,$1 ,(cadr isarraylist)))))) $2) $1-start-pos)));array
     ;((type-specifier declarator-list SEMI) (stx:declaration (map (lambda (x) (list  x $1)) $2) $1-start-pos)))
    (declarator-list
     ((declarator) (list $1))
     ((declarator-list COMMA declarator) `(,@$1 ,$3)))
    (declarator
     ((direct-declarator) `('() ,@$1));(stx:declarator-notpointer $1 $1-start-pos ))
     ((* direct-declarator) `('* ,@$2)));(stx:declarator-pointer $2 $1-start-pos )))
    (direct-declarator
     ((ID) (list $1));(stx:direct-declarator-var $1 $1-start-pos))
     ((ID LBBRA NUM RBBRA) `(,$1'array ,$3)));(stx:direct-declarator-array $1 $3 $1-start-pos)))
    (function-prototype
     ((type-specifier function-declarator SEMI) (stx:func-prototype (list $1 (car $2)) (cadr $2) (caddr $2) $1-start-pos)))
    (function-declarator ;func(a,b) *func(x,y)
     ((ID LPAR parameter-type-list-opt RPAR) `('() ,$1 ,$3));(stx:func-declarator-notpointer $1 $3 $1-start-pos))
     ((* ID LPAR parameter-type-list-opt RPAR) `('* ,$2 ,$4)));(stx:func-declarator-pointer $2 $4 $1-start-pos)))
    (function-definition
     ((type-specifier function-declarator compound-statement) (stx:func-definition $1 $2 $3 $1-start-pos)))
    (parameter-type-list-opt
     (() '())
     ((parameter-type-list) $1))
    (parameter-type-list
     ((parameter-declaration) (list $1))
     ((parameter-type-list COMMA parameter-declaration) `(,@$1 ,$3)))
    (parameter-declaration
     ((type-specifier parameter-declarator) (list $2 $1)));(stx:param-declaration $1 $2 $1-start-pos))) ;;should modify
    (parameter-declarator
     ((ID) $1) 
     ((* ID) (list '* $2)))
    (type-specifier
     ((INT) 'int);(stx:int-id $1-start-pos))
     ((VOID)'void));(stx:void-id $1-start-pos)))
    (statement ;単文
     ((SEMI) '())
     ((expression SEMI) $1)
     ((compound-statement) $1)
     ((IF LPAR expression RPAR statement) (stx:if-stmt $3 $5 `(,`()) $1-start-pos))
     ((IF LPAR expression RPAR statement ELSE statement) (stx:if-stmt $3 $5 $7 $1-start-pos))
     ((WHILE LPAR expression RPAR statement) (stx:while-stmt $3 $5 $1-start-pos))
     ((FOR LPAR expression-opt SEMI expression-opt
           SEMI expression-opt RPAR statement) (stx:for-stmt $3 $5 $7 $9 $1-start-pos))
     ((RETURN expression-opt SEMI) (stx:return-stmt $2 $1-start-pos)))
    (compound-statement
     ((LBRA declaration-list-opt statement-list-opt RBRA) (stx:compound-stmt $2 $3 $1-start-pos)))
    (declaration-list-opt
     (() '())
     ((declaration-list) $1))
    (declaration-list
     ((declaration) (list $1))
     ((declaration-list declaration) `(,@$1 ,$2)))
    (statement-list-opt
     (() '())
     ((statement-list) $1))
    (statement-list
     ((statement) (list $1))
     ((statement-list statement) `(,@$1 ,$2)))
    (expression-opt
     (() '())
     ((expression) $1))
    (expression
     ((assign-expr) $1)
     ((expression COMMA assign-expr) (stx:expression $1 $3 $1-start-pos)))
    (assign-expr
     ((logical-or-expr) $1)
     ((logical-or-expr = assign-expr) (stx:assign-stmt $1 $3 $1-start-pos)));???
    (logical-or-expr
     ((logical-and-expr) $1)
     ((logical-or-expr || logical-and-expr) (stx:logical-and-or-expr 'or $1 $3 $1-start-pos)))
    (logical-and-expr
     ((equality-expr) $1)
     ((logical-and-expr && equality-expr) (stx:logical-and-or-expr 'and $1 $3 $1-start-pos)))
    (equality-expr
     ((relational-expr) $1)
     ((equality-expr == relational-expr) (stx:rop-exp '== $1 $3 $2-start-pos))
     ((equality-expr != relational-expr) (stx:rop-exp '!= $1 $3 $2-start-pos)))
    (relational-expr
     ((add-expr) $1)
     ((relational-expr < add-expr) (stx:rop-exp '< $1 $3 $2-start-pos))
     ((relational-expr > add-expr) (stx:rop-exp '> $1 $3 $2-start-pos))
     ((relational-expr <= add-expr) (stx:rop-exp '<= $1 $3 $2-start-pos))
     ((relational-expr >= add-expr) (stx:rop-exp '>= $1 $3 $2-start-pos)))
    (add-expr
     ((mult-expr) $1)
     ((add-expr + mult-expr) (stx:aop-exp '+ $1 $3 $2-start-pos))
     ((add-expr - mult-expr) (stx:aop-exp '- $1 $3 $2-start-pos)))
    (mult-expr
     ((unary-expr) $1)
     ((mult-expr * unary-expr) (stx:aop-exp '* $1 $3 $2-start-pos))
     ((mult-expr / unary-expr) (stx:aop-exp '/ $1 $3 $2-start-pos)))
    (unary-expr ;単項
     ((postfix-expr) $1)
     ((- unary-expr) (stx:neg-exp $2 $1-start-pos))
     ((& unary-expr) (stx:addr-exp $2 $1-start-pos))
     ((* unary-expr) (stx:deref-exp $2 $1-start-pos)))
    (postfix-expr ;付加
     ((primary-expr) $1)
     ((postfix-expr LBBRA expression RBBRA) (stx:array-exp $1 $3 $1-start-pos));配列
     ((ID LPAR argument-expression-list-opt RPAR) (stx:funccall-exp $1 $3 $1-start-pos)));関数呼び出し func(x,y) ID = func
    (primary-expr  ;整数即値,変数,カッコで挟まれた式
     ((ID) (stx:var-exp $1 $1-start-pos))
     ((NUM)  (stx:lit-exp $1 $1-start-pos))
     ((LPAR expression RPAR) $2))
    (argument-expression-list-opt ;引数リスト "" or"4,2" ..etc..
     (() '())
     ((argument-expression-list) $1))
    (argument-expression-list
     ((assign-expr) (list $1))
     ((argument-expression-list COMMA assign-expr) `(,@$1 ,$3))))))

(define (parse-port port)
  (port-count-lines! port)
  (small-c-parser (lambda () (small-c-lexer port))))

;; 文字列を受け取って構文解析
(define (parse-string str)
  (parse-port (open-input-string str)))

;; ファイルを受け取って構文解析
(define (parse-file fname)
  (parse-port (open-input-file fname)))

;; 抽象構文木(実は任意のRacketデータ)を見やすく表示
(define (pretty-print-ast ast)
  (pretty-print ast))




(define (syntax-sugar tree)
  (cond ((list? tree) (map (lambda (x) (syntax-sugar x)) tree))
        ;for,単項のマイナス演算,配列参照式はシンタックスシュガーを適用する。
        ((stx:for-stmt? tree)
         (stx:compound-stmt
          '()
          (list
           (syntax-sugar (stx:for-stmt-initial tree))
           (stx:while-stmt (syntax-sugar (stx:for-stmt-test tree))
                               (stx:compound-stmt '() (list  (syntax-sugar (stx:for-stmt-body tree)) (syntax-sugar (stx:for-stmt-repeat tree))) (stx:for-stmt-pos tree))
                               (stx:for-stmt-pos tree)))
          (stx:for-stmt-pos tree)))
        ((stx:neg-exp? tree) (stx:aop-exp '- (stx:lit-exp 0  (stx:neg-exp-pos tree)) (syntax-sugar (stx:neg-exp-arg tree)) (stx:neg-exp-pos tree)))
        ((stx:deref-exp? tree)(stx:deref-exp (syntax-sugar (stx:deref-exp-arg tree)) (stx:deref-exp-pos tree)))
        ((stx:addr-exp? tree) 
         (let ((x (syntax-sugar (stx:addr-exp-var tree))))
             (if (stx:deref-exp? x)
                 (syntax-sugar (stx:deref-exp-arg x))
                 (stx:addr-exp x (stx:addr-exp-pos tree)))))
         ;(stx:addr-exp (syntax-sugar (stx:addr-exp-var tree)) (stx:addr-exp-pos tree)))
        ((stx:array-exp? tree) 
         (stx:deref-exp 
          (stx:aop-exp '+ (syntax-sugar (stx:array-exp-tgt tree)) (syntax-sugar (stx:array-exp-index tree)) (stx:array-exp-pos tree)) (stx:array-exp-pos tree)))
        
        ((stx:lit-exp? tree) (stx:lit-exp (syntax-sugar (stx:lit-exp-val tree)) (stx:lit-exp-pos tree)))
        ((stx:var-exp? tree) (stx:var-exp (syntax-sugar (stx:var-exp-tgt tree)) (stx:var-exp-pos tree)))
        ((stx:funccall-exp? tree) (stx:funccall-exp (syntax-sugar (stx:funccall-exp-tgt tree)) (syntax-sugar (stx:funccall-exp-paramlist tree)) (stx:funccall-exp-pos tree)))
        ((stx:declaration? tree) (stx:declaration (syntax-sugar (stx:declaration-declist tree)) (stx:declaration-pos tree)))
        ((stx:func-prototype? tree) (stx:func-prototype (syntax-sugar (stx:func-prototype-type tree)) (syntax-sugar (stx:func-prototype-id tree))
                                                        (syntax-sugar (stx:func-prototype-declarator tree)) (stx:func-prototype-pos tree)))
        ((stx:func-definition? tree) (stx:func-definition (syntax-sugar (stx:func-definition-type tree)) (syntax-sugar (stx:func-definition-declarator tree))
                                                          (syntax-sugar (stx:func-definition-statement tree)) (stx:func-definition-pos tree)))
        ((stx:param-declaration? tree) (stx:param-declaration (syntax-sugar (stx:param-declaration-type tree)) (syntax-sugar (stx:param-declaration-declarator tree))
                                                              (stx:param-declaration-pos tree)))
        ((stx:aop-exp? tree) (stx:aop-exp (syntax-sugar (stx:aop-exp-op tree)) (syntax-sugar (stx:aop-exp-left tree)) (syntax-sugar (stx:aop-exp-right tree))
                                          (stx:aop-exp-pos tree)))
        ((stx:rop-exp? tree) (stx:rop-exp (syntax-sugar (stx:rop-exp-op tree)) (syntax-sugar (stx:rop-exp-left tree)) (syntax-sugar (stx:rop-exp-right tree))
                                          (stx:rop-exp-pos tree)))
        ((stx:assign-stmt? tree) (stx:assign-stmt (syntax-sugar (stx:assign-stmt-var tree)) (syntax-sugar (stx:assign-stmt-src tree)) (stx:assign-stmt-pos tree)))
        ((stx:if-stmt? tree) (stx:if-stmt (syntax-sugar (stx:if-stmt-test tree)) (syntax-sugar (stx:if-stmt-tbody tree)) (syntax-sugar (stx:if-stmt-ebody))
                                          (stx:if-stmt-pos tree)))
        ((stx:while-stmt? tree) (stx:while-stmt (syntax-sugar (stx:while-stmt-test tree)) (syntax-sugar (stx:while-stmt-body tree)) (stx:while-stmt-pos tree)))
        ((stx:return-stmt? tree) (stx:return-stmt (syntax-sugar (stx:return-stmt-var tree)) (stx:return-stmt-pos tree)))
        ;((stx:int-id? tree)
        ;((stx:void-id? tree)
        ((stx:logical-and-or-expr? tree) (stx:logical-and-or-expr (syntax-sugar (stx:logical-and-or-expr-op tree)) (syntax-sugar (stx:logical-and-or-expr-log1 tree))
                                                                  (syntax-sugar (stx:logical-and-or-expr-log2 tree)) (stx:logical-and-or-expr-pos tree)))
        ((stx:expression? tree) (stx:expression (syntax-sugar (stx:expression exp)) (syntax-sugar (stx:expression-assign-exp tree)) (stx:expression-pos tree)))
        ((stx:compound-stmt? tree) (stx:compound-stmt (syntax-sugar (stx:compound-stmt-declaration-list-opt tree))
                                                                (syntax-sugar (stx:compound-stmt-statement-list-opt tree))
                                                                (stx:compound-stmt-pos tree)))
        (else tree)))



