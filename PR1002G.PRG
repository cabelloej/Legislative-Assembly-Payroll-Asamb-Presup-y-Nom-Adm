*** PARTIDAS X PROGRAMAS
SELECT 1
USE PRCTAS INDEX PRCTAS3
DO WHILE .T.
   @ 15,55 CLEAR TO 21,76
   @ 15,55 to 21,76
   @ 16,56 say "A¥O     :"
   @ 17,56 say "TIPO    :"
   @ 18,56 say "ORIGEN  :"
   @ 19,56 SAY "SECTOR  :"
   @ 20,56 SAY "PROGRAMA:"
   @ 16,66 GET WPREANO
   @ 17,66 GET WPRETIP
   @ 18,66 GET WPREORI
   @ 19,66 GET WSECTOR
   @ 20,66 GET WPROGRA
   READ
   IF READKEY() = 12 .OR. READKEY() = 268
      EXIT
   ENDIF
   IF WPREANO=SPACE(2).OR.;
      WPRETIP=SPACE(2).OR.;
      WPREORI=SPACE(3).OR.;
      WSECTOR=SPACE(2).OR.;
      WPROGRA=SPACE(2)
      STORE "SELECCION DE CODIGOS INCOMPLETA, REINTENTE" TO MES
      DO AVISO WITH MES
      LOOP
   ENDIF
   store "SELECCIONE : (M)ONITOR, (I)MPRESORA, (S)ALIR" TO TEX
   *STORE "MIS" TO WCH
   STORE "IS" TO WCH
   DO PREGUNTA
   STORE WCH TO WSALIDA
   IF WSALIDA = "S"
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
     STORE DESCRI1  TO WDESSEC1
     STORE DESCRI2  TO WDESSEC2
     STORE DESCRI3  TO WDESSEC3
     STORE DESCRI4  TO WDESSEC4
   ENDIF
   *** DATOS DEL PROGRAMA
   STORE WPREANO+WPRETIP+WPREORI+WSECTOR+WPROGRA TO WCLAVE
   SEEK WCLAVE
   IF .NOT. FOUND()
      STORE "LA SELECCION DE CODIGOS DEL PLAN DE CUENTAS NO EXISTE. OPRIMA <ENTER>" TO MES
      DO AVISO WITH MES
      LOOP
   ELSE
     STORE DESCRI1  TO WDESPRO1
     STORE DESCRI2  TO WDESPRO2
     STORE DESCRI3  TO WDESPRO3
     STORE DESCRI4  TO WDESPRO4
     STORE CORR     TO WCORR
     STORE INVE     TO WINVE
     STORE ORIG     TO WORIG
   ENDIF
   store 0       to corrpar
   store 0       to invepar
   store 0       to origpar
   store "    "  to wruppartid

   IF WSALIDA = "I"
      SET DEVI TO PRINT
      STORE 40 TO FINAL
   ELSE
      SET DEVI TO SCRE
      STORE 18 TO FINAL
   ENDIF
   store 100    to wline
   store 0      to wpage
   DO WHILE .NOT. EOF()
      IF PREANO<>WPREANO.OR.;
         PRETIP<>WPRETIP.OR.;
         PREORI<>WPREORI.OR.;
         SECTOR<>WSECTOR.OR.;
         PROGRA<>WPROGRA
         EXIT
      ENDIF
      **** SOLO PARA QUE SE PARESCA EL QUE USAN  (ELIMINAR SI SUBPRO O PROYEC <> 0)
      IF SUBPRO<>"  ".AND.PROYEC="  "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF PROYEC<>"  ".AND.ACTIVI="  "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF ACTIVI<>"  ".AND.PARTID="    "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF GENERI<>SPACE(2)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      ****
      IF WLINE > FINAL
         STORE WPAGE + 1 TO WPAGE
         IF WPAGE <> 1 .and. WLINE<100
            @ WLINE,00 SAY "+-----+----------------------------------------------+------------------+------------------+------------------+"
         ENDIF
         @ 00,00 SAY CHR(18)
         @ 00,05 say "CREDITOS PRESUPUESTARIOS DEL PROGRAMA A NIVEL DE PARTIDAS"
         @ 02,00 SAY CHR(15)
         @ 02,00 SAY "PRESUPUESTO: "+WPREANO+"  "+WPRETIP
         @ 03,00 SAY "SECTOR     : "+WSECTOR+" "+RTRIM(WDESSEC1)+" "+RTRIM(WDESSEC2)+" "+RTRIM(WDESSEC3)+" "+RTRIM(WDESSEC4)
         @ 04,00 SAY "PROGRAMA   : "+WPROGRA+" "+RTRIM(WDESPRO1)+" "+RTRIM(WDESPRO2)+" "+RTRIM(WDESPRO3)+" "+RTRIM(WDESPRO4)
         @ 06,00 SAY "+-----+----------------------------------------------+--------------------------------------------------------+"
         @ 07,00 SAY "|     |                                              |             A S I G N A C I O N    A N U A L           |"
         @ 08,00 SAY "|PART.|D E N O M I N A C I O N                       |------------------+------------------+------------------+"
         @ 09,00 SAY "|     |                                              |         CORRIENTE|         INVERSION|             TOTAL|"
         @ 10,00 SAY "|     |                                              |                  |                  |                  |"
         @ 11,00 SAY "+-----+----------------------------------------------+------------------+------------------+------------------+"
         STORE 11 TO WLINE
      ENDIF
      IF PARTID<>WRUPPARTID
         if wruppartid<>"    ".and.origpar>0
            @ WLINE,054 SAY corrpar picture "@Z ###,###,###,###.##"
            @ WLINE,073 SAY invepar picture "@Z ###,###,###,###.##"
            @ WLINE,092 SAY origpar picture "@Z ###,###,###,###.##"
         endif
         store 0       to corrpar
         store 0       to invepar
         store 0       to origpar
         store partid  to wruppartid
         store wline+1 to wline
         @ WLINE,00 SAY "|     |                                              |                  |                  |                  |"
         @ WLINE,01 SAY PARTID
         @ wline,07 say descri1
         if descri2 <> space(44)
            store wline+1 to wline
            @ WLINE,00 SAY "|     |                                              |                  |                  |                  |"
            @ wline,07 say descri2
         endif
         if descri3 <> space(44)
            store wline+1 to wline
            @ WLINE,00 SAY "|     |                                              |                  |                  |                  |"
            @ wline,07 say descri3
         endif
         if descri4 <> space(44)
            store wline+1 to wline
            @ WLINE,00 SAY "|     |                                              |                  |                  |                  |"
            @ wline,07 say descri4
         endif
         store wline+1 to wline
         @ WLINE,00 SAY "|     |                                              |                  |                  |                  |"
         store corrpar+corr to corrpar
         store invepar+inve to invepar
         store origpar+orig to origpar
      ELSE
         store corrpar+corr to corrpar
         store invepar+inve to invepar
         store origpar+orig to origpar
      ENDIF
      SKIP
   ENDDO
   @ WLINE,054 SAY corrpar picture "@Z ###,###,###,###.##"
   @ WLINE,073 SAY invepar picture "@Z ###,###,###,###.##"
   @ WLINE,092 SAY origpar picture "@Z ###,###,###,###.##"
   store wline+1 to wline
   @ WLINE+0,00 SAY "+-----+----------------------------------------------+------------------+------------------+------------------+"
   @ WLINE+1,00 SAY "|      T O T A L E S                                 |                  |                  |                  |"
   @ WLINE+1,054 SAY wcorr picture "@Z ###,###,###,###.##"
   @ WLINE+1,073 SAY winve picture "@Z ###,###,###,###.##"
   @ WLINE+1,092 SAY worig picture "@Z ###,###,###,###.##"
   @ WLINE+2,00 SAY "+----------------------------------------------------+------------------+------------------+------------------+"
   IF WSALIDA = "M"
      STORE "OPRIMA <ENTER> PARA SALIR" TO MES
      DO AVISO WITH MES
   ELSE
      SET DEVI TO SCRE
      EJECT
   ENDIF
ENDDO
CLOSE DATA
CLOSE INDEX
RETURN

