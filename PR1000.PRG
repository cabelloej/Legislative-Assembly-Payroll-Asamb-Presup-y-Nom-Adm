SET TALK OFF
SET ECHO OFF
if wprinfor = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   prompt "TABLAS DE CODIGOS       "
   @ 09,54   prompt "FORMULACIONES           "
   @ 10,54   prompt "REFORMULACIONES         "
   @ 11,54   prompt "INCREMENTOS             "
   @ 12,54   prompt "DISMINUCIONES           "
   @ 13,54   prompt "ASIGNACIONES            "
   @ 14,54   prompt "COMPROMISOS             "
   @ 15,54   prompt "GASTOS CAUSADOS         "
   @ 16,54   prompt "PAGOS                   "
   @ 17,54   prompt "EJECUCION PRESUPUESTARIA"
   save scre to s3
   menu to WSUB1000
   do case
      case WSUB1000= 1
           DO PR1001
      case WSUB1000= 2
           DO PR1002
      case WSUB1000= 3
        *  DO PR1003
      case WSUB1000= 4
        *  DO PR1004
      case WSUB1000= 5
        *  DO PR1005
      case WSUB1000= 6
        *  DO PR1006
      case WSUB1000= 7
        *  DO PR1007
      case WSUB1000= 8
        *  DO PR1008
      case WSUB1000= 9
        *  DO PR1009
      case WSUB1000= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
