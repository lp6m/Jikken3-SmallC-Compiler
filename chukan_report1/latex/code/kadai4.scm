> (parse-file "chukan_report1/latex/code/sample1.sc")
(#(struct:func-definition
   (int ())
   func
   ((() a int) (() b int))
   #(struct:compound-stmt
     ()
     (#(struct:return-stmt
        #(struct:expression
          (#(struct:aop-exp
             +
             #(struct:var-exp a #(struct:position 32 2 15))
             #(struct:var-exp b #(struct:position 36 2 19))
             #(struct:position 34 2 17)))
          #(struct:position 32 2 15))
        #(struct:position 25 2 8)))
     #(struct:position 22 1 21))
   #(struct:position 1 1 0))
 #(struct:func-definition
   (int ())
   main
   ()
   #(struct:compound-stmt
     (#(struct:declaration ((i () () int) (a () * int) (b array () int 10)) #(struct:position 54 5 8)))
     (#(struct:for-stmt
        #(struct:expression
          (#(struct:assign-stmt
             #(struct:var-exp i #(struct:position 75 6 12))
             #(struct:lit-exp 0 #(struct:position 79 6 16))
             #(struct:position 75 6 12)))
          #(struct:position 75 6 12))
        #(struct:expression
          (#(struct:rop-exp
             <
             #(struct:var-exp i #(struct:position 82 6 19))
             #(struct:lit-exp 10 #(struct:position 86 6 23))
             #(struct:position 84 6 21)))
          #(struct:position 82 6 19))
        #(struct:expression
          (#(struct:assign-stmt
             #(struct:var-exp i #(struct:position 90 6 27))
             #(struct:aop-exp
               +
               #(struct:var-exp i #(struct:position 94 6 31))
               #(struct:lit-exp 1 #(struct:position 98 6 35))
               #(struct:position 96 6 33))
             #(struct:position 90 6 27)))
          #(struct:position 90 6 27))
        #(struct:compound-stmt
          ()
          (#(struct:expression
             (#(struct:funccall-exp print (#(struct:var-exp i #(struct:position 110 7 22))) #(struct:position 104 7 16)))
             #(struct:position 104 7 16)))
          #(struct:position 100 6 37))
        #(struct:position 71 6 8)))
     #(struct:position 51 4 10))
   #(struct:position 41 4 0)))