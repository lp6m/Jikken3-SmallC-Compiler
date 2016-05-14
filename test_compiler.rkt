#lang racket
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))
 
(define-empty-tokens tokens-without-value
  (+ * EOF))

(define-tokens tokens-with-value
  (NUM))

(define-lex-abbrevs
  (digit   (char-range "0" "9")))

(define calc-lexer ;lexer = 字句解析 各アクションにおいてtokenオブジェクト生成 
  (lexer-src-pos
   ((:+ digit) (token-NUM (string->number lexeme)))
   ("+"        (token-+))
   ("*"        (token-*))
   (whitespace (return-without-pos (calc-lexer input-port)))
   ((eof)      (token-EOF))))

(define (mydisplay a)
  (begin (display "running action ") (display a) a))

(define calc-parser ;parser = 構文解析
  (parser
   (start program)
   (end EOF)
   (src-pos)
   ;;(debug "simple-parser.tbl")   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error "parse error:" tok-name tok-value)))
   (tokens tokens-with-value tokens-without-value)
   (grammar
    (program ((expression) $1))
    (expression ((multiplicative-expr) $1)
                 ((multiplicative-expr + expression) (+ $1 $3)))
    (multiplicative-expr ((NUM) $1)
                          ((NUM * multiplicative-expr) (* $1 $3))))))

(define (parse-string s)
  (let ((p (open-input-string s)))
    (calc-parser (lambda () (calc-lexer p)))))