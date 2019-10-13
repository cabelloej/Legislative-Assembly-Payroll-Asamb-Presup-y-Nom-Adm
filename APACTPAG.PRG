set date ital
set talk off
set echo off
set dele on
set exact off
set excl off
set proc to approc
select 1
use apperson
select 2
use appagcon index appagco1,appagco2
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
   if nomina <> "07" 
      select 1
      skip
      loop
   endif
   store cedula  to wcedula
   select 2
   find &wcedula
   do while .not. eof() .and. cedula = wcedula
      if concepto = "1240"
         DO RECLOC
         *replace cantidad with 28 
         if apperson->sueldop=32587
            replace monto    with 25000
         endif
         if apperson->sueldop=31293
            replace monto    with 23750
         endif
         if apperson->sueldop=30000
            replace monto    with 22500 
         endif
         if apperson->sueldop=24000
            replace monto    with 16250
         endif
         if apperson->sueldop=17969
            replace monto    with 12500
         endif
         if apperson->sueldop=21344
            replace monto    with 13750
         endif
         if apperson->sueldop=14229
            replace monto    with 11250
         endif
         if apperson->sueldop=21151
            replace monto    with 15000
         endif
         if apperson->sueldop=16755
            replace monto    with 11250
         endif
         if apperson->sueldop=24883
            replace monto    with 15000
         endif
         if apperson->sueldop=17000
            replace monto    with 13750
         endif
         if apperson->sueldop=25804
            replace monto    with 18750
         endif
         UNLOCK
         EXIT
      endif
      select 2
      skip
   enddo   
   select 1
   skip
enddo
close data
close index
return
