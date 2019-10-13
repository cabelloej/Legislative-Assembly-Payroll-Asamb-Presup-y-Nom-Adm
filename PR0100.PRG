SET TALK OFF
SET ECHO OFF
if wprtabla = space(1)
   return
endif
store .t. to subve3
do while subve3
   set color to g/n
   STORE T7 TO MES
   DO MENSAJE WITH MES
   @ 08,54   prompt "CLASIFICADOR DE PARTIDAS"
   @ 09,54   PROMPT "UNIDADES EJECUTORAS     "
   @ 10,54   PROMPT "MUNICIPIOS              "
   @ 11,54   PROMPT "SOPORTES DE OPERACIONES "
   @ 12,54   PROMPT "BENEFICIARIOS           "
   save scre to s3
   menu to WSUB0100
   do case
      case WSUB0100= 1
           select 1
           use prparti index prparti
           DO PRPARTI
           close data
           close index
      CASE WSUB0100= 2
           SELECT 1
           use prunid index prunid
           DO PRUNID
           CLOSE DATA
           CLOSE INDEX
      CASE WSUB0100= 3
           SELECT 1
           USE PRMUNICI INDEX PRMUNICI
           DO PRMUNI
           CLOSE DATA
           CLOSE INDEX
      CASE WSUB0100= 4
           SELECT 1
           USE PRSOPOR INDEX PRSOPOR
           DO PRSOPOR
           CLOSE DATA
           CLOSE INDEX
      CASE WSUB0100= 5
           SELECT 1
           USE PRBENEF  INDEX PRBENEF1,PRBENEF2
           SELECT 2
           USE PRCOMP INDEX PRCOMP1,PRCOMP2,PRCOMP3
           DO PRBENEF
           CLOSE DATA
           CLOSE INDEX
      case WSUB0100= 0
           store .f. to subve3
   endcase
   restore scre from s3
enddo
