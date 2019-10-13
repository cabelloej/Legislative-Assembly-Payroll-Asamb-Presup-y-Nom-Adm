*** PROYEC/ACTIVI X PROGRAMAS
SELECT 1
USE PRCTAS INDEX PRCTAS1
DO WHILE .T.
   @ 14,55 CLEAR TO 19,76
   @ 14,55 to 19,76
   @ 15,56 say "A¥O     :"
   @ 16,56 say "TIPO    :"
   @ 17,56 say "ORIGEN  :"
   @ 18,56 SAY "SECTOR  :"
   @ 15,66 GET WPREANO
   @ 16,66 GET WPRETIP
   @ 17,66 GET WPREORI
   @ 18,66 GET WSECTOR
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
   store "SELECCIONE : (M)ONITOR, (I)MPRESORA, (S)ALIR" TO TEX
   *STORE "MIS" TO WCH
   STORE "IS" TO WCH
   DO PREGUNTA
   STORE WCH TO WSALIDA
   IF WSALIDA = "S"
      EXIT
   ENDIF

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
     STORE " "      TO WDESPRO1
     STORE " "      TO WDESPRO2
     STORE " "      TO WDESPRO3
     STORE " "      TO WDESPRO4
     STORE PROGRA   TO WRUPPROGRA
     STORE SUBPRO   TO WRUPSUBPRO
     STORE CORR     TO WCORR
     STORE INVE     TO WINVE
     STORE ORIG     TO WORIG
   ENDIF
   IF WSALIDA = "I"
      SET DEVI TO PRINT
      STORE 40 TO FINAL
   ELSE
      SET DEVI TO SCRE
      STORE 18 TO FINAL
   ENDIF
   *set printer to eduardo.txt
   store 100    to wline
   store 0      to wpage
   DO WHILE .NOT. EOF()
      IF PREANO<>WPREANO.OR.;
         PRETIP<>WPRETIP.OR.;
         PREORI<>WPREORI.OR.;
         SECTOR<>WSECTOR
         EXIT
      ENDIF
      IF PARTID<>SPACE(4)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF

      *** SOLO PARA QUE SE PARESCA EL QUE USAN  (ELIMINAR SI SUBPRO O PROYEC <> 0)
      IF SUBPRO="00".AND.PROYEC="  "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF PROYEC="00".AND.ACTIVI="  "
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      ****

      STORE .F. TO WFLAGPROSUB
      IF PROGRA<>WRUPPROGRA
         STORE DESCRI1 TO WDESPRO1
         STORE DESCRI2 TO WDESPRO2
         STORE DESCRI3 TO WDESPRO3
         STORE DESCRI4 TO WDESPRO4
         STORE PROGRA  TO WRUPPROGRA
         IF WLINE<>0.AND.WLINE<100
            STORE WLINE+1 TO WLINE
            @ WLINE+0,00 SAY "+--+--+----------------------------------------------+------------------+------------------+------------------+"
            @ WLINE+1,00 SAY "|      T O T A L E S                                 |                  |                  |                  |"
            @ WLINE+1,054 SAY wcorr picture "@Z ###,###,###,###.##"
            @ WLINE+1,073 SAY winve picture "@Z ###,###,###,###.##"
            @ WLINE+1,092 SAY worig picture "@Z ###,###,###,###.##"
            @ WLINE+2,00 SAY "+----------------------------------------------------+------------------+------------------+------------------+"
            STORE CORR    TO WCORR
            STORE INVE    TO WINVE
            STORE ORIG    TO WORIG
            STORE 100 TO WLINE
         ENDIF
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF

      STORE WLINE + 1 TO WLINE
      IF WLINE > FINAL
         STORE WPAGE + 1 TO WPAGE
         IF WPAGE <> 1 .and. WLINE<100
            @ WLINE,00 SAY "+--+--+----------------------------------------------+------------------+------------------+------------------+"
         ENDIF
         IF WPAGE > 1 .AND. WSALIDA = "M"
            STORE "SELECCIONE: (C)ONTINUAR, (S)ALIR" TO tex
            STORE "CS" TO WCH
            DO PREGUNTA
            IF WCH = "S"
               EXIT
            ENDIF
         ENDIF
         @ 00,00 SAY CHR(18)
         @ 00,10 say "RESUMEN DE CREDITOS ASIGNADOS AL PROGRAMA"
         @ 02,00 SAY CHR(15)
         @ 02,00 SAY "PRESUPUESTO: "+WPREANO+"  "+WPRETIP
         @ 03,00 SAY "SECTOR     : "+WSECTOR+" "+RTRIM(WDESSEC1)+" "+RTRIM(WDESSEC2)+" "+RTRIM(WDESSEC3)+" "+RTRIM(WDESSEC4)
         @ 04,00 SAY "PROGRAMA   : "+WRUPPROGRA+" "+RTRIM(WDESPRO1)+" "+RTRIM(WDESPRO2)+" "+RTRIM(WDESPRO3)+" "+RTRIM(WDESPRO4)
         IF WRUPSUBPRO<>"  ".AND.WRUPSUBPRO<>"00"
            @ 05,00 SAY "SUBPROGRAMA: "+WRUPSUBPRO
         ENDIF
         @ 06,00 SAY "+--+--+----------------------------------------------+--------------------------------------------------------+"
         @ 07,00 SAY "|P |A |                                              |           C R E D I T O S    A S I G N A D O S         |"
         @ 08,00 SAY "|R |C |D E N O M I N A C I O N                       |------------------+------------------+------------------+"
         @ 09,00 SAY "|O |T |                                              |         CORRIENTE|         INVERSION|             TOTAL|"
         @ 10,00 SAY "|Y |I |                                              |                  |                  |                  |"
         @ 11,00 SAY "+--+--+----------------------------------------------+------------------+------------------+------------------+"
         STORE 12 TO WLINE
      ENDIF
      @ WLINE,00 SAY "|  |  |                                              |                  |                  |                  |"
      IF PROYEC<>"00"
         @ WLINE,01 SAY PROYEC
      ENDIF
      IF ACTIVI<>"00"
         *** PARA AJUSTAR AL QUE USAN
         IF ACTIVI<"50"
            @ WLINE,01 SAY ACTIVI
         ELSE
            @ WLINE,04 SAY ACTIVI
         ENDIF
      ENDIF
      store descri1 to wdescri1
      store descri2 to wdescri2
      store descri3 to wdescri3
      store descri4 to wdescri4
      ***
      @ wline,07 say wdescri1
      @ WLINE,054 SAY corr picture "@Z ###,###,###,###.##"
      @ WLINE,073 SAY inve picture "@Z ###,###,###,###.##"
      @ WLINE,092 SAY orig picture "@Z ###,###,###,###.##"
      if wdescri2 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |                                              |                  |                  |                  |"
         @ wline,07 say wdescri2
      endif
      if wdescri3 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |                                              |                  |                  |                  |"
         @ wline,07 say wdescri3
      endif
      if wdescri4 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |                                              |                  |                  |                  |"
         @ wline,07 say wdescri4
      endif
      store wline+1 to wline
      @ WLINE,00 SAY "|  |  |                                              |                  |                  |                  |"
      SKIP
   ENDDO
   store wline+1 to wline
   @ WLINE+0,00 SAY "+--+--+----------------------------------------------+------------------+------------------+------------------+"
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