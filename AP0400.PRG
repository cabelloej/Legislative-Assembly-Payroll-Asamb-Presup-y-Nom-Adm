SET TALK OFF
SET ECHO OFF
if WAPINFOR  = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   PROMPT "BASE DE DATOS           "
   @ 09,54   PROMPT "HISTORICO INDIVIDUAL    "
   @ 10,54   PROMPT "HISTORICO NOMINAL       "
   @ 11,54   PROMPT "HISTORICO CONCEPTUAL    "
   @ 12,54   PROMPT "INFORMES ESPECIALES     "
   save scre to s3
   menu to WSUB0400
   do case
      case WSUB0400= 1
           do ap0401
      case WSUB0400= 2
           do ap0402
      case WSUB0400= 3
           do ap0403
      case WSUB0400= 4
           do ap0404
      case WSUB0400= 5
           do ap0405
      case WSUB0400= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
