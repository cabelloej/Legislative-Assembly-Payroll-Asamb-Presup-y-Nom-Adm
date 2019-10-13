SET TALK OFF
SET ECHO OFF
if WAPOTROS  = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   prompt "REORGANIZAR INDICES     "
   @ 09,54   prompt "CAMBIO DE CEDULA        "
   @ 10,54   prompt "CAMBIO CODIGO CONCEPTO  "
   save scre to s3
   menu to WSUB0500
   do case
      case WSUB0500= 1
           if wapotros = "A"
               DO INDICES
           endif
      case WSUB0500= 2
           DO AP0502
      case WSUB0500= 3
           DO AP0503
      case WSUB0500= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
