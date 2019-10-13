SET TALK OFF
SET ECHO OFF
store .t. to subveXX
do while subveXX
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 16,55 CLEAR TO 21,76
   @ 16,55 TO 21,76
   @ 17,56   PROMPT "FICHA PERSONAL      "
   @ 18,56   PROMPT "GRUPOS Y NOMINAS    "
   @ 19,56   PROMPT "UBICACION ADMINIST. "
   @ 20,56   PROMPT "RELACION PARA FIRMA "
   save scre to sXX
   menu to WSUB04XX
   do case
      case WSUB04XX= 1
         * DO AP0401FA
      case WSUB04XX= 2
           DO AP0401FB
      case WSUB04XX= 3
           DO AP0401FC
      case WSUB04XX= 4
           DO AP0401FD
      case WSUB04XX= 0
           store .f. to subveXX
   endcase
   restore scre from sXX
enddo
