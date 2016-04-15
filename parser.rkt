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
     ((LBRA declaration-list-opt statement-list-opt RBRA) `(,@$2 ,@$3)))
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
     ((& ID) (stx:addr-exp $2 $1-start-pos))
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
