*** CUENTAS DEL PRESUPUESTO
SELECT 1
USE PRCTAS INDEX PRCTAS1
DO WHILE .T.
   @ 12,55 CLEAR TO 17,76
   @ 12,55 to 17,76
   @ 13,56 say "A¥O     :"
   @ 14,56 say "TIPO    :"
   @ 15,56 say "ORIGEN  :"
   @ 16,56 SAY "SECTOR  :"
   @ 13,66 GET WPREANO
   @ 14,66 GET WPRETIP
   @ 15,66 GET WPREORI
   @ 16,66 GET WSECTOR
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
     STORE DESCRI1 TO WDESSEC1
     STORE DESCRI2 TO WDESSEC2
     STORE DESCRI3 TO WDESSEC3
     STORE DESCRI4 TO WDESSEC4
     STORE " "     TO WDESPRO1
     STORE " "     TO WDESPRO2
     STORE " "     TO WDESPRO3
     STORE " "     TO WDESPRO4
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
         @ 2,0 say "PLAN DE CUENTAS DEL PRESUPUESTO"
         @ 3,0 SAY CHR(15)
         @ 3,0 SAY "PRESUPUESTO: "+WPREANO+"-"+WPRETIP+"-"+WPREORI
         @ 4,0 SAY "SECTOR     : "+WSECTOR+" "+RTRIM(WDESSEC1)+" "+RTRIM(WDESSEC2)+" "+RTRIM(WDESSEC3)+" "+RTRIM(WDESSEC4)
         @ 5,0 SAY "PROGRAMA   : "+WPROGRA+" "+RTRIM(WDESPRO1)+" "+RTRIM(WDESPRO2)+" "+RTRIM(WDESPRO3)+" "+RTRIM(WDESPRO4)
         @ 6,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
         @ 7,0 SAY "|SP|PY|AC|PART|GE|ES|SE|ORD|DENOMINACION                                  |         CORRIENTE|         INVERSION|             TOTAL|"
         @ 8,0 SAY "+--+--+--+----+--+--+--+---+----------------------------------------------+------------------+------------------+------------------+"
         STORE 9 TO WLINE
      ENDIF
      @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
      @ WLINE,01 SAY SUBPRO
      @ WLINE,04 SAY PROYEC
      @ WLINE,07 SAY ACTIVI
      @ WLINE,10 SAY PARTID
      @ WLINE,15 SAY GENERI
      @ WLINE,18 SAY ESPECI
      @ WLINE,21 SAY SUBESP
      @ WLINE,24 SAY ORDINA
      store descri1 to wdescri1
      store descri2 to wdescri2
      store descri3 to wdescri3
      store descri4 to wdescri4
      @ wline,28 say wdescri1
      @ WLINE,075 SAY corr picture "@Z ###,###,###,###.##"
      @ WLINE,094 SAY inve picture "@Z ###,###,###,###.##"
      @ WLINE,113 SAY orig picture "@Z ###,###,###,###.##"
      if wdescri2 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
         @ wline,28 say wdescri2
      endif
      if wdescri3 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
         @ wline,28 say wdescri3
      endif
      if wdescri4 <> space(44)
         store wline+1 to wline
         @ WLINE,00 SAY "|  |  |  |    |  |  |  |   |                                              |                  |                  |                  |"
         @ wline,28 say wdescri4
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

