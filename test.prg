set talk off
set echo off
set date ital
STORE 0                                       TO WNUMLUN
STORE MONTH(date())                           TO WMESACT
STORE  YEAR(date())                           TO WANOACT

? wmesact
wait
? wanoact
wait

STORE CTOD("01-"+STR(WMESACT,2)+"-"+STR(WANOACT,2))    TO WFECTEST

? wfectest
wait
DO WHILE WMESACT=MONTH(WFECTEST)
   IF DOW(WFECTEST)=2
      STORE WNUMLUN+1 TO WNUMLUN
      WAIT
   ENDIF
   STORE WFECTEST + 1 TO WFECTEST
ENDDO
STORE INT((((APPERSON->SUELDOP*12)/52)*(4/100))) TO XMONTO
IF XMONTO > 3461
   STORE 3461 TO XMONTO
ENDIF
STORE XMONTO * WNUMLUN TO XMONTO
RETURN

