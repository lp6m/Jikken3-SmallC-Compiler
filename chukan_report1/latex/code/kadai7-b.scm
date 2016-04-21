(#(struct:func-definition
   (int ())
   main
   ()
   #(struct:compound-stmt
     (#(struct:declaration ((i () () int) (a array () int 10) (b () * int) (c () () int)) #(struct:position 14 2 8)))
     (#(struct:expression
        (#(struct:assign-stmt
           #(struct:var-exp i #(struct:position 37 3 12))
           #(struct:lit-exp 0 #(struct:position 41 3 16))
           #(struct:position 37 3 12)))
        #(struct:position 37 3 12))
      #(struct:while-stmt
        #(struct:expression
          (#(struct:rop-exp
             <
             #(struct:var-exp i #(struct:position 44 3 19))
             #(struct:lit-exp 10 #(struct:position 48 3 23))
             #(struct:position 46 3 21)))
          #(struct:position 44 3 19))
        #(struct:compound-stmt
          ()
          (#(struct:expression
             (#(struct:assign-stmt
                #(struct:var-exp i #(struct:position 52 3 27))
                #(struct:aop-exp
                  +
                  #(struct:var-exp i #(struct:position 56 3 31))
                  #(struct:lit-exp 1 #(struct:position 60 3 35))
                  #(struct:position 58 3 33))
                #(struct:position 52 3 27)))
             #(struct:position 52 3 27)))
          #(struct:position 33 3 8))
        #(struct:position 33 3 8))
      #(struct:if-else-stmt
        #(struct:expression
          (#(struct:rop-exp
             ==
             #(struct:deref-exp
               #(struct:aop-exp
                 +
                 #(struct:var-exp a #(struct:position 71 5 11))
                 #(struct:expression (#(struct:lit-exp 0 #(struct:position 73 5 13))) #(struct:position 73 5 13))
                 #(struct:position 71 5 11))
               #(struct:position 71 5 11))
             #(struct:lit-exp 2 #(struct:position 79 5 19))
             #(struct:position 76 5 16)))
          #(struct:position 71 5 11))
        #(struct:compound-stmt () () #(struct:position 81 5 21))
        #(struct:compound-stmt () () #(struct:position 68 5 8))
        #(struct:position 68 5 8))
      #(struct:expression
        (#(struct:assign-stmt
           #(struct:var-exp b #(struct:position 87 7 8))
           #(struct:addr-exp
             #(struct:expression
               (#(struct:deref-exp #(struct:var-exp c #(struct:position 94 7 15)) #(struct:position 93 7 14)))
               #(struct:position 93 7 14))
             #(struct:position 91 7 12))
           #(struct:position 87 7 8)))
        #(struct:position 87 7 8)))
     #(struct:position 11 1 10))
   #(struct:position 1 1 0)))