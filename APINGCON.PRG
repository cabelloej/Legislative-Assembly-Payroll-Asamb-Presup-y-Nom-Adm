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
   if grupo <> "02"
      select 1
      skip
      loop
   endif
   if nomina <> "08"
      select 1
      skip
      loop
   endif
   store cedula to wcedula
   * sueldo
   store wcedula+"1001" to wclave
   SELECT 2
   FIND &WCLAVE
   if eof()
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula with wcedula
      replace concepto with "1001"
   endif
   replace cantidad with 15  
   replace factor with 100
   replace minimo with 0
   replace maximo with  9999999.99  

    * bono de transporte
    store wcedula+"1006" to wclave
    SELECT 2
    FIND &WCLAVE
    if eof()
       do filloc
       append blank
       unlock 
       do recloc
       replace cedula with wcedula
       replace concepto with "1006"
    endif
    replace cantidad with 15  
    replace factor with 450/15
    replace minimo with 0
    replace maximo with  9999999.99   

 * bono de transporte
 *  store wcedula+"1012" to wclave
 *  SELECT 2
 *  FIND &WCLAVE
 *  if eof()
 *     do filloc
 *     append blank
 *     unlock 
 *     do recloc
 *     replace cedula with wcedula
 *     replace concepto with "1012"
 *  endif
 *  replace cantidad with 15  
 *  replace factor with 187.5/15
 *  replace minimo with 0
 *  replace maximo with  9999999.99  

   * BONO COMEDOR
*   store wcedula+"1013" to wclave
*   SELECT 2
*   FIND &WCLAVE
*   if eof()
*      do filloc
*     append blank
*      unlock 
*      do recloc
*      replace cedula with wcedula
*      replace concepto with "1013"
*   endif
*   replace cantidad with 15  
*   replace factor with 187.5/15
*   replace minimo with 0
*   replace maximo with  9999999.99  

* bono medicinal
*   store wcedula+"1014" to wclave
*   SELECT 2
*   FIND &WCLAVE
*   if eof()
*      do filloc
*      append blank
*      unlock 
*      do recloc
*      replace cedula with wcedula
*      replace concepto with "1014"
*   endif
*   replace cantidad with 15  
*   replace factor with 150/15
*   replace minimo with 0
*   replace maximo with  9999999.99  

   * sso
   store wcedula+"3001" to wclave
   SELECT 2
   FIND &WCLAVE
   if eof()
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula with wcedula
      replace concepto with "3001"
   endif
   replace cantidad with 1   
   replace factor with 1  
   replace minimo with 0
   replace maximo with  450         

   * spf
   store wcedula+"3002" to wclave
   SELECT 2
   FIND &WCLAVE
   if eof()
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula with wcedula
      replace concepto with "3002"
   endif
   replace cantidad with 1   
   replace factor with .125
   replace minimo with 0
   replace maximo with  9999999.99  

   * ahorro hab.
   store wcedula+"3003" to wclave
   SELECT 2
   FIND &WCLAVE
   if eof()
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula with wcedula
      replace concepto with "3003"
   endif
   replace cantidad with 1   
   replace factor with .5 
   replace minimo with 0
   replace maximo with  9999999.99  

   * fondo jub.
   store wcedula+"3007" to wclave
   SELECT 2
   FIND &WCLAVE
   if eof()
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula with wcedula
      replace concepto with "3007"
   endif
   replace cantidad with 1   
   replace factor with 1  
   replace minimo with 0
   replace maximo with  9999999.99  

   unlock
   flush
   select 1
   skip
enddo
close all
return
