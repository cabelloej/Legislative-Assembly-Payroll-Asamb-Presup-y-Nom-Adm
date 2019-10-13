***  "RESUMEN DE CREDITOS ASIGNADOS A LOS PROGRAMAS DEL SECTOR"
***
SELECT 1
USE PRCTAS INDEX PRCTAS1
SELECT 2
USE PRUNID INDEX PRUNID
SELECT 1
DO WHILE .T.
   STORE 0 TO WTOTSEC
   @ 13,55 CLEAR TO 18,76
   @ 13,55 to 18,76
   @ 14,56 say "A¥O     :"
   @ 15,56 say "TIPO    :"
   @ 16,56 say "ORIGEN  :"
   @ 17,56 SAY "SECTOR  :"
   @ 14,66 GET WPREANO
   @ 15,66 GET WPRETIP
   @ 16,66 GET WPREORI
   @ 17,66 GET WSECTOR
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
     STORE PROGRA   TO WRUPPROGRA
     STORE SUBPRO   TO WRUPSUBPRO
     STORE PROYEC   TO WRUPPROYEC
     STORE ACTIVI   TO WRUPACTIVI
     STORE SPACE(1) TO WUNIDES
     STORE ORIG     TO WTOTSEC
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
      *** SOLO PARA AJUSTARLO AL FORMATO USADO
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
      ***
      IF PARTID<>SPACE(4)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      STORE WLINE + 1 TO WLINE
      IF WLINE > FINAL
         STORE WPAGE + 1 TO WPAGE
         IF WPAGE <> 1 .and. WLINE<>100
            @ WLINE,0 SAY "+--+--+--+--+----------------------------------------------+-----------------------------------------------------------------+--------------------------------------------------------+------+"
         ENDIF
         IF WPAGE > 1 .AND. WSALIDA = "M"
            STORE "SELECCIONE: (C)ONTINUAR, (S)ALIR" TO tex
            STORE "CS" TO WCH
            DO PREGUNTA
            IF WCH = "S"
               EXIT
            ENDIF
         ENDIF
         @ 00,0 SAY CHR(18)
         @ 01,0 say "RESUMEN DE CREDITOS ASIGNADOS A LOS PROGRAMAS DEL SECTOR"
         @ 03,0 SAY CHR(15)
         @ 03,0 SAY "PRESUPUESTO: "+WPREANO+"  "+WPRETIP
         @ 04,0 SAY "SECTOR     : "+WSECTOR+" "+RTRIM(WDESSEC1)+" "+RTRIM(WDESSEC2)+" "+RTRIM(WDESSEC3)+" "+RTRIM(WDESSEC4)
         @ 05,0 SAY "+--+--+--+--+----------------------------------------------+-----------------------------------------------------------------+---------------------------------------------------------------+"
         @ 06,0 SAY "|P |S |P |A |                                              |                                                                 |        C L A S I F I C A C I O N    E C O N O M I C A         |"
         @ 07,0 SAY "|R |U |R |C |D E N O M I N A C I O N                       |U N I D A D   E J E C U T O R A                                  |------------------+------------------+------------------+------|"
         @ 08,0 SAY "|O |B |O |T |                                              |                                                                 |         CORRIENTE|         INVERSION|             TOTAL|  %   |"
         @ 09,0 SAY "|G |P |Y |I |                                              |                                                                 |                  |                  |                  |      |"
         @ 10,0 SAY "+--+--+--+--+--+--+--+---+---------------------------------+-----------------------------------------------------------------+------------------+------------------+------------------+------+"
         STORE 11 TO WLINE
      ENDIF
      @ WLINE,00 SAY "|  |  |  |  |                                              |                                                                 |                  |                  |                  |      |"
      IF PROGRA<>"00" .AND. PROGRA<>WRUPPROGRA
         @ WLINE,01 SAY PROGRA
         STORE PROGRA TO WRUPPROGRA
      ENDIF
      IF SUBPRO<>"00" .AND. SUBPRO<>WRUPSUBPRO
         @ WLINE,04 SAY SUBPRO
         STORE SUBPRO TO WRUPSUBPRO
      ENDIF
      IF PROYEC<>"00" .AND. PROYEC<>WRUPPROYEC
         @ WLINE,07 SAY PROYEC
         STORE PROYEC TO WRUPPROYEC
      ENDIF
      IF ACTIVI<>"00" .AND. ACTIVI<>WRUPACTIVI
         IF ACTIVI<"50"
            @ WLINE,07 SAY ACTIVI
         ELSE
            @ WLINE,10 SAY ACTIVI
         ENDIF
         STORE ACTIVI TO WRUPACTIVI
      ENDIF
      store descri1 to wdescri1
      store descri2 to wdescri2
      store descri3 to wdescri3
      store descri4 to wdescri4
      ***
      select 2
      seek prctas->unieje
      if found()
         store descri    to wunides
      else
         store space(60) to wunides
      endif
      select 1
      @ wline,13 say wdescri1
      @ wline,60 say wunides
      @ WLINE,126 SAY corr picture "@Z ###,###,###,###.##"
      @ WLINE,145 SAY inve picture "@Z ###,###,###,###.##"
      @ WLINE,164 SAY orig picture "@Z ###,###,###,###.##"
      @ WLINE,183 SAY (orig*100)/wtotsec picture "@Z ###.##"
      if wdescri2 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |  |                                              |                                                                 |                  |                  |                  |      |"
         @ wline,13 say wdescri2
      endif
      if wdescri3 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |  |                                              |                                                                 |                  |                  |                  |      |"
         @ wline,13 say wdescri3
      endif
      if wdescri4 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |  |                                              |                                                                 |                  |                  |                  |      |"
         @ wline,13 say wdescri4
      endif
      store wline+1 to wline
      @ WLINE,00 SAY "|  |  |  |  |                                              |                                                                 |                  |                  |                  |      |"
      SKIP
   ENDDO
   store wline+1 to wline
   @ WLINE,0 SAY "+--+--+--+--+----------------------------------------------+-----------------------------------------------------------------+--------------------------------------------------------+------+"
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