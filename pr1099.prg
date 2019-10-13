SELECT 1
USE PRCTAS INDEX PRCTAS1
DO WHILE .T.
   @ 9,54 clear to 16,77
   @ 9,54 to 16,77
   @ 10,55 say "A¥O      :"
   @ 11,55 say "TIPO     :"
   @ 12,55 say "ORIGEN   :"
   @ 13,55 SAY "SECTOR   :"
   @ 10,65 GET WPREANO
   @ 11,65 GET WPRETIP
   @ 12,65 GET WPREORI
   @ 13,65 GET WSECTOR
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
   IF WSALIDA = "I"
      SET DEVI TO PRINT
      STORE 44 TO FINAL
   ELSE
      SET DEVI TO SCRE
      STORE 18 TO FINAL
   ENDIF

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
     STORE " "     TO WDESPRO1
     STORE " "     TO WDESPRO2
     STORE " "     TO WDESPRO3
     STORE " "     TO WDESPRO4
     STORE SUBPRO  TO WRUPSUBPRO
     STORE PROYEC  TO WRUPPROYEC
     STORE ACTIVI  TO WRUPACTIVI
     STORE PARTID  TO WRUPPARTID
     STORE GENERI  TO WRUPGENERI
     STORE ESPECI  TO WRUPESPECI
     STORE SUBESP  TO WRUPSUBESP
     STORE ORDINA  TO WRUPORDINA
   ENDIF
   set printer to eduardo.txt
   store 100    to wline
   store 0      to wpage
   store progra to wprogra
   do while .not. eof()
      STORE WLINE + 1 TO WLINE
      IF PREANO<>WPREANO.OR.;
         PRETIP<>WPRETIP.OR.;
         PREORI<>WPREORI.OR.;
         SECTOR<>WSECTOR
         EXIT
      ENDIF
      if progra <> wprogra
         STORE DESCRI1 TO WDESPRO1
         STORE DESCRI2 TO WDESPRO2
         STORE DESCRI3 TO WDESPRO3
         STORE DESCRI4 TO WDESPRO4
         store progra to wprogra
         if wline<>0.and.wline<>100
            @ WLINE,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
            STORE 100 TO WLINE
         endif
      endif
      if wline > final
         STORE WPAGE + 1 TO WPAGE
         IF WPAGE <> 1 .and. WLINE<>100
            @ WLINE,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
         ENDIF
         IF WPAGE > 1 .AND. WSALIDA = "M"
            STORE "SELECCIONE: (C)ONTINUAR, (S)ALIR" TO tex
            STORE "CS" TO WCH
            DO PREGUNTA
            IF WCH = "S"
               EXIT
            ENDIF
         ENDIF
         @ 0,0 clear
         @ 0,0 SAY CHR(18)
         @ 0,0 say  CHR(14)+(QQWW)
         @ 1,0 say "CONTROL DE PRESUPUESTOS DE EGRESOS"
         @ 2,0 say "CREDITOS PRESUPUESTARIOS A NIVEL DE PARTIDAS Y SUB-PARTIDAS ESPECIFICAS"
         @ 3,0 SAY "PRESUPUESTO: "+WPREANO+"-"+WPRETIP+"-"+WPREORI
         @ 4,0 SAY "SECTOR     :"+WSECTOR+" "+RTRIM(WDESSEC1)+" "+RTRIM(WDESSEC2)+" "+RTRIM(WDESSEC3)+" "+RTRIM(WDESSEC4)
         @ 5,0 SAY "PROGRAMA   :"+WPROGRA+" "+RTRIM(WDESPRO1)+" "+RTRIM(WDESPRO2)+" "+RTRIM(WDESPRO3)+" "+RTRIM(WDESPRO4)
         @ 6,0 SAY CHR(15)
         @ 6,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
         @ 7,0 SAY "|SP|PY|AC|PART|GE|ES|SE|ORD|DENOMINACION                                  |         CORRIENTE|         INVERSION|             TOTAL|"
         @ 8,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
         STORE 9 TO WLINE
      ENDIF
      @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
      IF SUBPRO<>"00" .AND. SUBPRO<>WRUPSUBPRO
         @ WLINE,01 SAY SUBPRO
         STORE SUBPRO TO WRUPSUBPRO
      ENDIF
      IF PROYEC<>"00" .AND. PROYEC<>WRUPPROYEC
         @ WLINE,04 SAY PROYEC
         STORE PROYEC TO WRUPPROYEC
      ENDIF
      IF ACTIVI<>"00" .AND. ACTIVI<>WRUPACTIVI
         @ WLINE,07 SAY ACTIVI
         STORE ACTIVI TO WRUPACTIVI
      ENDIF
      IF PARTID <> SPACE(4) .AND. PARTID<>WRUPPARTID
         @ WLINE,10 SAY PARTID
         STORE PARTID TO WRUPPARTID
      ENDIF
      IF GENERI<>"00" .AND. GENERI<>WRUPGENERI
         @ WLINE,15 SAY GENERI
         STORE GENERI TO WRUPGENERI
      ENDIF
      IF ESPECI<>"00" .AND. ESPECI<>WRUPESPECI
         @ WLINE,18 SAY ESPECI
         STORE ESPECI TO WRUPESPECI
      ENDIF
      IF SUBESP<>"00" .AND. SUBESP<>WRUPSUBESP
         @ WLINE,21 SAY SUBESP
         STORE SUBESP TO WRUPSUBESP
      ENDIF
      *IF ORDINA<>"000" .AND. ORDINA<>WRUPORDINA
         @ WLINE,24 SAY ORDINA
      *   STORE ORDINA TO WRUPORDINA
      *ENDIF
      store descri1 to wdescri1
      store descri2 to wdescri2
      store descri3 to wdescri3
      store descri4 to wdescri4
      if wdescri1<>space(44)
         @ wline,28 say wdescri1
         @ WLINE,075 SAY corr picture "@Z ###,###,###,###.##"
         @ WLINE,094 SAY inve picture "@Z ###,###,###,###.##"
         @ WLINE,113 SAY orig picture "@Z ###,###,###,###.##"
         if ordina=space(3)
            @ wline,28 say wdescri1
            @ WLINE,075 SAY corr picture "@Z ###,###,###,###.##"
            @ WLINE,094 SAY inve picture "@Z ###,###,###,###.##"
            @ WLINE,113 SAY orig picture "@Z ###,###,###,###.##"
         endif
      endif
      if wdescri2 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
         @ wline,28 say wdescri2
         if ordina=space(3)
            @ wline,28 say wdescri2
         endif
      endif
      if wdescri3 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
         @ wline,28 say wdescri3
         if ordina=space(3)
            @ wline,28 say wdescri3
         endif
      endif
      if wdescri4 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
         @ wline,28 say wdescri4
         if ordina=space(3)
            @ wline,28 say wdescri4
         endif
      endif
      SKIP
   ENDDO
   @ WLINE,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
   IF WSALIDA = "M"
      store "OPRIMA <ENTER> PARA SALIR" TO MES
      DO AVISO WITH MES
   ELSE
      SET DEVI TO SCRE
      EJECT
   ENDIF
ENDDO
CLOSE DATA
CLOSE INDEX
RETURN
@ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"

