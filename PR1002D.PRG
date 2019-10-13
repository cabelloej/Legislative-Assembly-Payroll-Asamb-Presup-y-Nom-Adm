*** PARTIDAS GLOBALES
SELECT 1
USE PRCTAS INDEX PRCTAS6
DO WHILE .T.
   @ 15,55 CLEAR TO 20,76
   @ 15,55 to 20,76
   @ 16,56 say "A¥O     :"
   @ 17,56 say "TIPO    :"
   @ 18,56 say "ORIGEN  :"
   @ 19,56 say "SECTOR  :"
   @ 16,66 GET WPREANO
   @ 17,66 GET WPRETIP
   @ 18,66 GET WPREORI
   @ 19,66 GET WSECTOR
   READ
   IF READKEY() = 12 .OR. READKEY() = 268
      EXIT
   ENDIF
   IF WPREANO=SPACE(2).OR.;
      WPRETIP=SPACE(2).OR.;
      WPREORI=SPACE(3).OR.;
      WSECTOR=SPACE(2)
      STORE "SELECCION DE CODIGOS INCOMPLETA, REINTENTE" TO MES
      DO AVISO WITH MES
      LOOP
   ENDIF
   STORE "OPCIONES: (C)ONTINUAR, (S)ALIR" TO TEX
   STORE "CS" TO WCH
   DO PREGUNTA
   IF WCH = "S"
      EXIT
   ENDIF
   *** DATOS DEL SECTOR
   STORE WPREANO+WPRETIP+WPREORI+WSECTOR TO WCLAVE
   SEEK WCLAVE
   IF .NOT. FOUND()
      STORE "LA SELECCION DE CODIGOS DEL PLAN DE CUENTAS NO EXISTE. OPRIMA <ENTER>" TO MES
      DO AVISO WITH MES
      LOOP
   ELSE
      STORE DESCRI1 TO WDESSEC1
      STORE DESCRI2 TO WDESSEC2
      STORE DESCRI3 TO WDESSEC3
      STORE DESCRI4 TO WDESSEC4
      STORE CORR    TO WCORR
      STORE INVE    TO WINVE
      STORE ORIG    TO WORIG
   ENDIF
   store 0       to corrpar
   store 0       to invepar
   store 0       to origpar
   store "    "  to wruppartid
   SET DEVI TO PRINT
   STORE 40 TO FINAL
   store 100    to wline
   store 0      to wpage
   DO WHILE .NOT. EOF()
      IF PREANO<>WPREANO.OR.;
         PRETIP<>WPRETIP.OR.;
         PREORI<>WPREORI.OR.;
         SECTOR<>WSECTOR
         EXIT
      ENDIF
      IF SECTOR ="  "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF PARTID ="    "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF GENERI<>"  "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      ****
      IF PARTID<>WRUPPARTID
         if wruppartid<>"    ".and.origpar>0
            DO LINDAT
         endif
         store 0       to corrpar
         store 0       to invepar
         store 0       to origpar
         store partid  to wruppartid
         STORE DESCRI1 TO WDESCRI1
         STORE DESCRI2 TO WDESCRI2
         STORE DESCRI3 TO WDESCRI3
         STORE DESCRI4 TO WDESCRI4
      ENDIF
      store corrpar+corr to corrpar
      store invepar+inve to invepar
      store origpar+orig to origpar
      SKIP
   ENDDO
   do lindat
   store wline+1 to wline
   @ WLINE+0,00 SAY "+-----+----------------------------------------------+------------------+------------------+------------------+------+"
   @ WLINE+1,00 SAY "|      T O T A L E S                                 |                  |                  |                  |      |"
   @ WLINE+1,054 SAY wcorr picture "@Z ###,###,###,###.##"
   @ WLINE+1,073 SAY winve picture "@Z ###,###,###,###.##"
   @ WLINE+1,092 SAY worig picture "@Z ###,###,###,###.##"
   @ WLINE+1,111 SAY (worig*100)/WORIG picture "@Z ###.##"
   @ WLINE+2,00 SAY "+----------------------------------------------------+------------------+------------------+------------------+------+"
   SET DEVI TO SCRE
   EJECT
ENDDO
CLOSE DATA
CLOSE INDEX
RETURN
************
PROC TITULO
************
STORE WPAGE + 1 TO WPAGE
IF WPAGE <> 1 .and. WLINE<100
   @ WLINE,00 SAY "+-----+----------------------------------------------+------------------+------------------+------------------+"
ENDIF
@ 00,00 SAY CHR(18)
@ 00,05 say "CREDITOS PRESUPUESTARIOS DEL SECTOR A NIVEL DE PARTIDAS"
@ 02,00 SAY CHR(15)
@ 02,00 SAY "PRESUPUESTO: "+WPREANO+"  "+WPRETIP
@ 03,00 SAY "SECTOR     : "+WSECTOR+"  "+RTRIM(WDESSEC1)+" "+RTRIM(WDESSEC2)+" "+RTRIM(WDESSEC3)+" "+RTRIM(WDESSEC4)
@ 04,00 SAY "+-----+----------------------------------------------+---------------------------------------------------------------+"
@ 05,00 SAY "|     |                                              |                 A S I G N A C I O N   A N U A L               |"
@ 06,00 SAY "|PART.|D E S C R I P C I O N                         |------------------+------------------+------------------+------+"
@ 07,00 SAY "|     |                                              |         CORRIENTE|         INVERSION|             TOTAL|   %  |"
@ 08,00 SAY "+-----+----------------------------------------------+------------------+------------------+------------------+------+"
STORE 09 TO WLINE
RETURN
*************
PROC LINSEP
*************
@ WLINE,00 SAY "|     |                                              |                  |                  |                  |      |"
RETURN
*************
PROC LINDAT
*************
            store wline+1 to wline
            IF WLINE > FINAL
               DO TITULO
            ENDIF
            DO LINSEP
            @ WLINE,01 SAY WRUPPARTID
            @ wline,07 say wdescri1
            if wdescri2 <> space(44)
               store wline+1 to wline
               DO LINSEP
               @ wline,07 say wdescri2
            endif
            if wdescri3 <> space(44)
               store wline+1 to wline
               DO LINSEP
               @ wline,07 say wdescri3
            endif
            if wdescri4 <> space(44)
               store wline+1 to wline
               DO LINSEP
               @ wline,07 say wdescri4
            endif
            @ WLINE,054 SAY corrpar picture "@Z ###,###,###,###.##"
            @ WLINE,073 SAY invepar picture "@Z ###,###,###,###.##"
            @ WLINE,092 SAY origpar picture "@Z ###,###,###,###.##"
            @ WLINE,111 SAY (origpar*100)/WORIG picture "@Z ###.##"
            store wline+1 to wline
            DO LINSEP
            return

