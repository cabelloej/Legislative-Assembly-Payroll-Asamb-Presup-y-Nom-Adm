SET TALK OFF
SET ECHO OFF
if WAPARCHI  <> "A"
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   PROMPT "GRUPOS DE NOMINAS       "
   @ 09,54   PROMPT "NOMINAS                 "
   @ 10,54   PROMPT "UBICACION ADMINISTRATIVA"
   @ 11,54   prompt "CONCEPTOS NOMINALES     "
   @ 12,54   PROMPT "CARGOS                  "
   @ 13,54   prompt "PERSONAL                "
   @ 14,54   prompt "TABLA DE LIQUIDACION    "
   @ 15,54   prompt "TABLAS DE CODIGOS       "
   @ 16,54   prompt "HISTORICOS DE PAGO      "

   save scre to s3
   menu to WSUB0100
   do case
      case WSUB0100= 1
           do ap0101
      case WSUB0100= 2
           do ap0102
      case WSUB0100= 3
           do ap0103
      case WSUB0100= 4
           do ap0104
      case WSUB0100= 5
           do ap0105
      case WSUB0100= 6
           do ap0106
      case WSUB0100= 7
           do ap0107
      case wsub0100=8
           do ap0108
      case WSUB0100= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
