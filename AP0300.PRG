SET TALK OFF
SET ECHO OFF
if WAPOPERA  = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   PROMPT "CAMBIO DE CEDULA        "
   @ 09,54   PROMPT "CAMBIO DE COD.CONCEPTO  "
   @ 10,54   PROMPT "VACACIONES              "
   @ 11,54   PROMPT "UTILIDADES              "
   @ 12,54   PROMPT "LIQUIDACION             "
   save scre to s3
   menu to WSUB0300
   do secure
   do case
      case WSUB0300= 1
           do ap0301
      case WSUB0300= 2
           do ap0302
      case WSUB0300= 3
           do ap0303
      case WSUB0300= 4
           do ap0304
      case WSUB0300= 5
           do ap0305
      case WSUB0300= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
