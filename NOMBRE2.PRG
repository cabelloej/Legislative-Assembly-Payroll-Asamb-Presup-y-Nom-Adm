set talk off
set echo off
set dele on

select 1
use tempord1
select 2
use rengform
do while .not. eof()
   store nombre01r to wnombre01r
   store nombre02r to wnombre02r
   store nombre03r to wnombre03r
   store nombre04r to wnombre04r
   select tempord1
   replace descri1 with wnombre01r
   replace descri2 with wnombre02r
   replace descri3 with wnombre03r
   replace descri4 with wnombre04r
   skip
   select rengform
   skip
enddo
close data
return

