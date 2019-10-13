if wprotros = " "
   return
endif
SET TALK OFF
SET ECHO OFF
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   prompt "REORGANIZAR INDICES     "
   @ 09,54   prompt "REPROCESAR TRANSACCIONES"
   save scre to s3
   menu to WSUB1100
   do case
      case WSUB1100= 1
            DO INDICES
      case WSUB1100= 2
            do PRREPRO
      case WSUB1100= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
