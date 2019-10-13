set talk off
set echo off
set dele on
set exact off
set proc to approc
select 1
use apperson
select 2
use apconper index apconper
select 1
go top
do while .not. eof()
   if tipo <> "E"
      select 1
      skip
      loop
   endif
   if grupo <> "01"
      select 1
      skip
      loop
   endif
   if nomina <> "02"
      select 1
      skip
      loop
   endif
   store cedula to wcedula
   * bono DE TRANSPORTE
   store wcedula+"1013" to wclave
   SELECT 2
   FIND &WCLAVE
   if .NOT. eof()
      DO RECLOC
      replace cantidad with 15  
      replace factor with 10
      replace minimo with 0
      replace maximo with  9999999.99  
      UNLOCK
   endif
   select 1
   skip
enddo
close all
return
