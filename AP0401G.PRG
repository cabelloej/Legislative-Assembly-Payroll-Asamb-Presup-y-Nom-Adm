SET TALK OFF
SET ECHO OFF
store .t. to subveXX
do while subveXX
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 17,55 CLEAR TO 22,76
   @ 17,55 TO 22,76
   @ 18,56   PROMPT "CENTRO DE PAGO      "
   @ 19,56   PROMPT "CONTRATOS COLECTIVOS"
   @ 20,56   PROMPT "SINDICATOS          "
   @ 21,56   PROMPT "BANCOS DEL PERSONAL "
   save scre to sXX
   menu to WSUB04XX
   do case
      case WSUB04XX= 1
           DO AP0401GA
      case WSUB04XX= 2
           DO AP0401GB
      case WSUB04XX= 3
           DO AP0401GC
      case WSUB04XX= 4
           DO AP0401GD
      case WSUB04XX= 0
           store .f. to subveXX
   endcase
   restore scre from sXX
enddo

