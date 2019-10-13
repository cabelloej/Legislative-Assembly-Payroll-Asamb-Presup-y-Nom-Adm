*** RESUMEN DE LOS CREDITOS PRESUPUESTARIOS
***    A NIVEL DE SECTORES Y PROGRAMAS
***
SELECT 1
USE PRCTAS INDEX PRCTAS1
SELECT 2
USE PRUNID INDEX PRUNID
SELECT 1
DO WHILE .T.
   STORE 0 TO WTOTSEC
   @ 14,55 CLEAR TO 18,76
   @ 14,55 to 18,76
   @ 15,56 say "A¥O     :"
   @ 16,56 say "TIPO    :"
   @ 17,56 say "ORIGEN  :"
   @ 15,66 GET WPREANO
   @ 16,66 GET WPRETIP
   @ 17,66 GET WPREORI
   READ
   IF READKEY() = 12 .OR. READKEY() = 268
      EXIT
   ENDIF
   IF WPREANO=SPACE(2).OR.;
      WPRETIP=SPACE(2).OR.;
      WPREORI=SPACE(3)
      STORE "SELECCION DE CODIGOS INCOMPLETA, REINTENTE" TO MES
      DO AVISO WITH MES
      LOOP
   ENDIF
   store "SELECCIONE : (C)ONTINUAR, (S)ALIR" TO TEX
   STORE "CS" TO WCH
   DO PREGUNTA
   IF WCH = "S"
      EXIT
   ENDIF
   STORE WPREANO+WPRETIP+WPREORI TO WCLAVE
   SEEK WCLAVE
   IF .NOT. FOUND()
      STORE "LA SELECCION DE CODIGOS DEL PLAN DE CUENTAS NO EXISTE. OPRIMA <ENTER>" TO MES
      DO AVISO WITH MES
      LOOP
   ELSE
      STORE CORR TO WTOTCORR
      STORE INVE TO WTOTINVE
      STORE ORIG TO WTOTORIG
   ENDIF
   SET DEVI   TO PRINT
   STORE 40   TO FINAL
   STORE 100  TO WLINE
   STORE 0    TO WPAGE
   DO WHILE .NOT. EOF()
      IF PREANO<>WPREANO.OR.;
         PRETIP<>WPRETIP.OR.;
         PREORI<>WPREORI
         EXIT
      ENDIF
      IF SECTOR=SPACE(2)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF SUBPRO<>SPACE(2)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      STORE WLINE + 1 TO WLINE
      IF WLINE > FINAL
         DO TITULO
      ENDIF
      DO LINSEP
      @ WLINE,02  SAY SECTOR
      @ WLINE,07  SAY PROGRA
      @ WLINE,11  SAY descri1
      @ WLINE,58  SAY corr picture "@Z ###,###,###,###.##"
      @ WLINE,77  SAY inve picture "@Z ###,###,###,###.##"
      @ WLINE,96  SAY orig picture "@Z ###,###,###,###.##"
      if descri2 <> space(44)
         store wline+1 to wline
         do linsep
         @ wline,11 say descri2
      endif
      if descri3 <> space(44)
         store wline+1 to wline
         do linsep
         @ wline,11 say descri3
      endif
      if descri4 <> space(44)
         store wline+1 to wline
         do linsep
         @ wline,11 say descri4
      endif
      STORE WLINE + 1 TO WLINE
      DO LINSEP
      SKIP
   ENDDO
   store wline+1 to wline
   do linext
   store wline+1 to wline
   do linsep
   @ wline,11  say "T O T A L E S ...."
   @ WLINE,58  SAY wtotcorr picture "@Z ###,###,###,###.##"
   @ WLINE,77  SAY wtotinve picture "@Z ###,###,###,###.##"
   @ WLINE,96  SAY wtotorig picture "@Z ###,###,###,###.##"
   store wline+1 to wline
   do linext
   SET DEVI TO SCRE
   EJECT
ENDDO
CLOSE DATA
CLOSE INDEX
RETURN
***********
PROC LINSEP
***********
@ WLINE,00  SAY "|    |    |                                              |                  |                  |                  |"
RETURN
***********
PROC LINEXT
***********
@ WLINE,0 SAY "+----+----+----------------------------------------------+------------------+------------------+------------------+"
RETURN
***********
PROC TITULO
STORE WPAGE + 1 TO WPAGE
IF WPAGE <> 1 .and. WLINE<>100
   DO LINEXT
ENDIF
@ 00,00 SAY CHR(18)
@ 00,10 say "             TITULO III                "
@ 01,10 say "RESUMEN DE LOS CREDITOS PRESUPUESTARIOS"
@ 02,10 say "   A NIVEL DE SECTORES Y PROGRAMAS     "
@ 04,00 SAY CHR(15)
@ 04,00 SAY "PRESUPUESTO: "+WPREANO+"  "+WPRETIP
STORE 05 TO WLINE
DO LINEXT
@ 06,0 SAY "|    |    |                                              |             A S I G N A C I O N   A N U A L            |"
@ 07,0 SAY "|SECT|PROG|D E N O M I N A C I O N                       |------------------+------------------+------------------+"
@ 08,0 SAY "|    |    |                                              |         CORRIENTE|         INVERSION|             TOTAL|"
STORE 09 TO WLINE
DO LINEXT
STORE 10 TO WLINE
RETURN

