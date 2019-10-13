SET TALK OFF
SET ECHO OFF
if wprformu = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   prompt "ELABORAR PLAN DE CUENTAS"
   @ 09,54   PROMPT "CIERRE DE FORMULACION   "
   save scre to s3
   menu to WSUB0200
   do case
      case WSUB0200= 1
           select 1
           use prctas   index prctas1,prctas2,prctas3,prctas4
           select 2
           use prparti  index prparti
           select 3
           use prunid   index prunid
           select 4
           use prcomp   index prcomp1,prcomp2,prcomp3
           select 5
           use prmunici index prmunici
           do prformu
           close data
           close index
      CASE WSUB0200= 2
           *DO PRCLOSE
      case WSUB0200= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
