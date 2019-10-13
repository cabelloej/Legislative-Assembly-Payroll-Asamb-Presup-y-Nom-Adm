SET TALK OFF
SET ECHO OFF
if WAPPROCE  = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   PROMPT "APERTURA DE PERIODO     "
   @ 09,54   PROMPT "VARIACION PERSONAL      "
   @ 10,54   PROMPT "VARIACION GENERAL       "
   @ 11,54   PROMPT "INFORMES DEL PERIODO    "
   @ 12,54   PROMPT "CIERRE DE PERIODO       "
   save scre to s3
   menu to WSUB0200
   do secure
   do case
      case WSUB0200= 1
           do ap0201
      case WSUB0200= 2
           do ap0202
      case WSUB0200= 3
           do ap0203
      case WSUB0200= 4
           do ap0204
      case WSUB0200= 5
           do ap0205
      case WSUB0200= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
