> (define ast (parse-file "chukan_report1/latex/code/sample2.sc"))
> (define ast-removed (remove-syntax-sugar ast))
> (parse-reverse-file ast "out.c")
> (parse-reverse-file ast-removed "outremoved.c")