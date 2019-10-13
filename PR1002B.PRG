*** INDICE DE CATEGORIA PROGRAMATICAS
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
      IF SUBPRO<>SPACE(2).AND.PROYEC=SPACE(2)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF PROYEC<>SPACE(2).AND.ACTIVI=SPACE(2)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      IF PARTID<>SPACE(4)
         SELECT PRCTAS
         SKIP
         LOOP
      ENDIF
      STORE WLINE + 1 TO WLINE
      IF WLINE > FINAL
         DO TITULO
      ENDIF
      DO LINSEP
      select 2
      seek prctas->unieje
      if found()
         store descri    to wuniejedes
      else
         store space(60) to wuniejedes
      endif
      select 1
      IF SECTOR<>"00"
         @ WLINE,01  SAY SECTOR
      ENDIF
      IF PROGRA<>"00"
         @ WLINE,05  SAY PROGRA
      ENDIF
      IF SUBPRO<>"00"
         @ WLINE,09  SAY SUBPRO
      ENDIF
      IF PROYEC<>"00"
         @ WLINE,13  SAY PROYEC
      ENDIF
      IF ACTIVI<>"00"
         IF ACTIVI<"50"
            @ WLINE,13  SAY ACTIVI
         ELSE
            @ WLINE,17  SAY ACTIVI
         ENDIF
      ENDIF
      @ WLINE,21  SAY DESCRI1
      @ WLINE,68  SAY WUNIEJEDES
      if descri2 <> space(44)
         store wline+1 to wline
         do linsep
         @ wline,21 say descri2
      endif
      if descri3 <> space(44)
         store wline+1 to wline
         do linsep
         @ wline,21 say descri3
      endif
      if descri4 <> space(44)
         store wline+1 to wline
         do linsep
         @ wline,21 say descri4
      endif
      STORE WLINE+1 TO WLINE
      DO LINSEP
      SKIP
   ENDDO
   STORE WLINE+1 TO WLINE
   DO LINEXT
   SET DEVI TO SCRE
   EJECT
ENDDO
CLOSE DATA
CLOSE INDEX
RETURN
***********
PROC LINSEP
***********
@ WLINE,00  SAY "|   |   |   |   |   |                                              |                                                                  |"
RETURN
***********
PROC LINEXT
***********
@ WLINE,00  SAY "+---+---+---+---+---+----------------------------------------------+------------------------------------------------------------------+"
RETURN
***********
PROC TITULO
STORE WPAGE + 1 TO WPAGE
IF WPAGE <> 1 .and. WLINE<>100
   DO LINEXT
ENDIF
@ 00,00 SAY CHR(18)
@ 01,01 say "INDICE DE CATEGORIAS PROGRAMATICAS"
@ 04,00 SAY CHR(15)
@ 04,00 SAY "PRESUPUESTO: "+WPREANO+"  "+WPRETIP
STORE 05 TO WLINE
DO LINEXT
@ 06,0 SAY "| S | P | S | P | A |                                              |                                                                  |"
@ 07,0 SAY "| E | R | U | R | C |D E N O M I N A C I O N                       | U N I D A D   E J E C U T O R A                                  |"
@ 08,0 SAY "| C | O | B | O | T |                                              |                                                                  |"
@ 09,0 SAY "| T | G | P | Y | I |                                              |                                                                  |"
STORE 10 TO WLINE
DO LINEXT
STORE 11 TO WLINE
RETURN

