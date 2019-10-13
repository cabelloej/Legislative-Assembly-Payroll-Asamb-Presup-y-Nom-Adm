              if wprformu <> "A"
                return
              endif
              @ 21,0 CLEAR TO 24,79
              STORE 100 TO WLINER1 
              STORE .T. TO WACTX
              DO WHILE WACTX
                 SELECT PRCTAS
                 STORE SPACE(LEN(DESCRI1))   TO WDESCRI1
                 STORE SPACE(LEN(DESCRI2))   TO WDESCRI2
                 STORE SPACE(LEN(DESCRI3))   TO WDESCRI3
                 STORE SPACE(LEN(DESCRI4))   TO WDESCRI4
                 STORE SPACE(LEN(UNIEJE))    TO WUNIEJE
                 STORE SPACE(LEN(TIPOINV))   TO WTIPOINV
                 STORE SPACE(LEN(MUNICI))    TO WMUNICI
                 STORE 0                     TO WMONTO
                 STORE WLINER1  + 1 TO WLINER1 
                 IF WLINER1  > 18
                    SET COLOR TO G/N
                    @ 7,1 CLEAR TO 18,78
                    STORE 7 TO WLINER1 
                 ENDIF
                 @ WLINER1 ,01  GET WPREANO
                 @ WLINER1 ,04  GET WPRETIP
                 @ WLINER1 ,07  GET WPREORI
                 @ WLINER1 ,13  GET WSECTOR
                 @ WLINER1 ,16  GET WPROGRA
                 @ WLINER1 ,19  GET WSUBPRO
                 @ WLINER1 ,22  GET WPROYEC
                 @ WLINER1 ,25  GET WACTIVI
                 @ WLINER1 ,28  GET WPARTID PICT "#.##"
                 @ WLINER1 ,33  GET WGENERI
                 @ WLINER1 ,36  GET WESPECI
                 @ WLINER1 ,39  GET WSUBESP
                 @ WLINER1 ,42  GET WORDINA
                 READ
                 IF WPARTID=" .  "
                    STORE SPACE(4) TO WPARTID
                 ENDIF
                 STORE SPACE(30) TO WCUENTA0
                 DO WARMACOD0
                 IF WCUENTA0 = SPACE(30).OR.READKEY()=12.OR.READKEY()=268
                    *GO TOP
                    STORE RECNO() TO WINICIO0
                    STORE 0       TO WREVERSE0
                    STORE .T.     TO FLAGJUMP0
                    STORE .F.     TO WACTX
                    LOOP
                 ENDIF
                 STORE LEN(RTRIM(WCUENTA0)) TO WLARCOD
                 *IF WLARCOD >=18 .AND. WLARCOD <=29
                 *   STORE "CODIGO INVALIDO, FAVOR COMPLETAR CLASIFICADOR DE PARTIDA. OPRIMA <ENTER>" TO MES
                 *   DO AVISO WITH MES
                 *   STORE WLINER1  - 1  TO WLINER1
                 *   LOOP
                 *ENDIF
                 STORE LTRIM(RTRIM(WCUENTA0)) TO XCUENTA0
                 IF AT(" ",XCUENTA0) > 0
                    STORE "ERROR 1. NO DEBEN EXISTIR ESPACIOS EN BLANCO DENTRO DEL CODIGO" TO MES
                    DO AVISO WITH MES
                    STORE WLINER1  - 1  TO WLINER1
                    LOOP
                 ENDIF
                 IF (LEN(WCUENTA0) <> LEN(LTRIM(WCUENTA0)))
                    STORE "ERROR 2. NO DEBEN EXISTIR ESPACIOS EN BLANCO A LA IZQUIERDA DEL CODIGO" TO MES
                    DO AVISO WITH MES
                    STORE WLINER1  - 1  TO WLINER1
                    LOOP
                 ENDIF
                 SEEK WCUENTA0
                 IF .NOT. EOF()
                    STORE "NUMERO DE CUENTA PRESUPUESTARIA YA EXISTE. <ENTER>" TO MES
                    DO AVISO WITH MES
                    STORE WLINER1  - 1  TO WLINER1 
                    LOOP
                 ENDIF
                 *** VALIDACION DE PARTIDAS ***
                 *STORE WPARTID+WGENERI+WESPECI+WSUBESP TO WWPPAA
                 STORE WPARTID+WGENERI+WESPECI          TO WWPPAA
                 IF WPARTID<>"4.04".AND.WSUBESP<>"00"
                    STORE WWPPAA+WSUBESP TO WWPPAA
                 ENDIF
                 *IF WWPPAA = SPACE(10)
                 IF WWPPAA = SPACE(08)
                    STORE SPACE(44) TO WDESCRI1
                    STORE SPACE(44) TO WDESCRI2
                    STORE SPACE(44) TO WDESCRI3
                    STORE SPACE(44) TO WDESCRI4
                 ELSE
                    SELECT PRPARTI
                    SEEK WWPPAA
                    IF EOF()
                       STORE "PARTIDA NO DEFINIDA EN CLASIFICADOR DE PARTIDAS, INGRESAR? (S/N)" TO TEX
                       STORE "SN" TO WCH
                       DO PREGUNTA
                       IF WCH = "N"
                          STORE WLINER1  - 1  TO WLINER1
                          SELECT PRCTAS
                          LOOP
                       ELSE
                          save scre to wemil
                          SELECT PRPARTI
                          DO PRPARTI
                          restore scre from wemil
                          STORE WLINER1  - 1  TO WLINER1
                          SELECT PRCTAS
                          LOOP
                       ENDIF
                    ELSE
                       STORE DESCRI    TO WDESCRI1
                       STORE DESCRI2   TO WDESCRI2
                       STORE SPACE(44) TO WDESCRI3
                       STORE SPACE(44) TO WDESCRI4
                    ENDIF
                 ENDIF
                 ***
                 SELECT PRCTAS
                 IF WLARCOD = 2
                    STORE "*******" TO WBUSCAR
                    STORE 0 TO WFRACCION
                 ELSE
                    IF WLARCOD = 4
                       STORE "A¥O" TO WBUSCAR
                       STORE 2 TO WFRACCION
                    ELSE
                       IF WLARCOD = 7
                          STORE "TIPO" TO WBUSCAR
                          STORE 4 TO WFRACCION
                       ELSE
                          IF WLARCOD = 9
                             STORE "ORIGEN" TO WBUSCAR
                             STORE 7 TO WFRACCION
                          ELSE
                             IF WLARCOD = 11
                                STORE "SECTOR" TO WBUSCAR
                                STORE 9 TO WFRACCION
                             ELSE
                                IF WLARCOD = 13
                                   STORE "PROGRAMA" TO WBUSCAR
                                   STORE 11 TO WFRACCION
                                ELSE
                                   IF WLARCOD = 15
                                      STORE "SUBPROGRAMA" TO WBUSCAR
                                      STORE 13 TO WFRACCION
                                    ELSE
                                       IF WLARCOD = 17
                                          STORE "PROYECTO" TO WBUSCAR
                                          STORE 15 TO WFRACCION
                                       ELSE
                                          IF WLARCOD >= 21
                                             STORE "ACTIVIDAD" TO WBUSCAR
                                             STORE 17 TO WFRACCION
                                          ENDIF
                                       ENDIF
                                    ENDIF
                                ENDIF
                             ENDIF
                          ENDIF
                       ENDIF
                    ENDIF
                 ENDIF
                 IF WLARCOD >= 4 .AND. WLARCOD <=21
                    STORE SUBSTR(WCUENTA0,1,WFRACCION) TO WNEWCOD
                    SEEK WNEWCOD
                    IF EOF()
                       STORE "CODIGO DE "+WBUSCAR+" NO ESTA DEFINIDO. <ENTER>" TO MES
                       DO AVISO WITH MES
                       STORE WLINER1  - 1  TO WLINER1
                       LOOP
                    ENDIF
                 ENDIF
                 SAVE SCRE TO S8
                 @ 0,0 CLEAR
                 SET COLOR TO N/G
                 @ 0,0
                 @ 0,0 SAY QQWW
                 @ 00,00  SAY "          P L A N   D E   C U E N T A S   D E L    P R E S U P U E S T O       "
                 SET COLOR TO G+/N
                 @ 02,00 SAY "+-----------------------------------------------------+------------------------+"
                 @ 03,00 SAY "|A¥O:                                                 |COR.:                   |"
                 @ 04,00 SAY "|TIP:                                                 |INV.:                   |"
                 @ 05,00 SAY "|ORI:                                                 |TOT.:                   |"
                 @ 06,00 SAY "|SEC:                                                 |------------------------|"
                 @ 07,00 SAY "|PRO:                                                 |TRA.:                   |"
                 @ 08,00 SAY "|SUP:                                                 |INC.:                   |"
                 @ 09,00 SAY "|PRO:                                                 |DIS.:                   |"
                 @ 10,00 SAY "|ACT:                                                 |MOD.:                   |"
                 @ 11,00 SAY "|PAR:                                                 |COM.:                   |"
                 @ 12,00 SAY "|GEN:                                                 |CAU.:                   |"
                 @ 13,00 SAY "|ESP:                                                 |PAG.:                   |"
                 @ 14,00 SAY "|SUE:                                                 |DIS.:                   |"
                 @ 15,00 SAY "+-----------------------------------------------+-----|------------------------|"
                 @ 16,00 SAY "|COD:                                           |T/PAR|EJE.:                   |"
                 @ 17,00 SAY "|                                               |     |                        |"
                 @ 18,00 SAY "|                                               |-----|------------------------|"
                 @ 19,00 SAY "|                                               |T/INV|MUN.:                   |"
                 @ 20,00 SAY "|                                               |     |                        |"
                 @ 21,00 SAY "+-----------------------------------------------+-----+------------------------+"
                 SET COLOR TO G/N
                 SELECT PRCTAS
                 STORE SUBSTR(WCUENTA0,1,2) TO WWPREANO
                 SEEK WWPREANO
                 IF EOF()
                    STORE SPACE(44) TO WDESPREANO
                 ELSE
                    STORE DESCRI1 TO WDESPREANO
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,4) TO WWPRETIP
                 SEEK WWPRETIP
                 IF EOF()
                    STORE SPACE(44) TO WDESPRETIP
                 ELSE
                    STORE DESCRI1 TO WDESPRETIP
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,7) TO WWPREORI
                 SEEK WWPREORI
                 IF EOF()
                    STORE SPACE(44) TO WDESPREORI
                 ELSE
                    STORE DESCRI1 TO WDESPREORI
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,9) TO WWSECTOR
                 SEEK WWSECTOR
                 IF EOF()
                    STORE SPACE(44) TO WDESSECTOR
                 ELSE
                    STORE DESCRI1 TO WDESSECTOR
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,11) TO WWPROGRA
                 SEEK WWPROGRA
                 IF EOF()
                    STORE SPACE(44) TO WDESPROGRA
                 ELSE
                    STORE DESCRI1 TO WDESPROGRA
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,13) TO WWSUBPRO
                 SEEK WWSUBPRO
                 IF EOF()
                    STORE SPACE(44) TO WDESSUBPRO
                 ELSE
                    STORE DESCRI1 TO WDESSUBPRO
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,15) TO WWPROYEC
                 SEEK WWPROYEC
                 IF EOF()
                    STORE SPACE(44) TO WDESPROYEC
                 ELSE
                    STORE DESCRI1 TO WDESPROYEC
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,17) TO WWACTIVI
                 SEEK WWACTIVI
                 IF EOF()
                    STORE SPACE(44) TO WDESACTIVI
                 ELSE
                    STORE DESCRI1 TO WDESACTIVI
                 ENDIF
                 IF WWPPAA = SPACE(10)
                    STORE SPACE(44) TO WDESPARTID
                    STORE SPACE(44) TO WDESGENERI
                    STORE SPACE(44) TO WDESESPECI
                    STORE SPACE(44) TO WDESSUBESP
                 ELSE
                    SELECT PRPARTI
                    STORE SUBSTR(WCUENTA0,18,4) TO WWPARTID
                    SEEK WWPARTID
                    IF EOF()
                       STORE SPACE(44) TO WDESPARTID
                    ELSE
                       STORE DESCRI TO WDESPARTID
                    ENDIF
                    STORE SUBSTR(WCUENTA0,18,6) TO WWGENERI
                    SEEK WWGENERI
                    IF EOF()
                       STORE SPACE(44) TO WDESGENERI
                    ELSE
                       STORE DESCRI TO WDESGENERI
                    ENDIF
                    STORE SUBSTR(WCUENTA0,18,8) TO WWESPECI
                    SEEK WWESPECI
                    IF EOF()
                       STORE SPACE(44) TO WDESESPECI
                    ELSE
                       STORE DESCRI TO WDESESPECI
                    ENDIF
                    STORE SUBSTR(WCUENTA0,18,10) TO WWSUBESP
                    SEEK WWSUBESP
                    IF EOF()
                       STORE SPACE(44) TO WDESSUBESP
                    ELSE
                       STORE DESCRI TO WDESSUBESP
                    ENDIF
                 ENDIF
                 SELECT PRCTAS
                 @ 03,05 SAY WPREANO+"   "+WDESPREANO
                 @ 04,05 SAY WPRETIP+"   "+WDESPRETIP
                 @ 05,05 SAY WPREORI+"  "+WDESPREORI
                 @ 06,05 SAY WSECTOR+"   "+WDESSECTOR
                 @ 07,05 SAY WPROGRA+"   "+WDESPROGRA
                 @ 08,05 SAY WSUBPRO+"   "+WDESSUBPRO
                 @ 09,05 SAY WPROYEC+"   "+WDESPROYEC
                 @ 10,05 SAY WACTIVI+"   "+WDESACTIVI
                 @ 11,05 SAY WPARTID+" "+WDESPARTID
                 @ 12,05 SAY WGENERI+"   "+WDESGENERI
                 @ 13,05 SAY WESPECI+"   "+WDESESPECI
                 @ 14,05 SAY WSUBESP+"   "+WDESSUBESP
                 @ 16,05 SAY WSECTOR+"."+WPROGRA+"."+WSUBPRO+"."+WPROYEC+"."+WACTIVI+"."+WPARTID+"."+WGENERI+"."+WESPECI+"."+WSUBESP+"."+WORDINA

                 @ 17,01 GET WDESCRI1
                 @ 18,01 GET WDESCRI2
                 @ 19,01 GET WDESCRI3
                 @ 20,01 GET WDESCRI4
                 READ

                 IF WLARCOD >= 11 .AND. WLARCOD <=17
                    @ 16,60 GET WUNIEJE
                    READ
                    SELECT PRUNID
                    SEEK WUNIEJE
                    IF EOF()
                       STORE "UNIDAD EJECUTORA NO REGISTRADA, INGRESAR ? (S/N)" TO TEX
                       STORE "SN" TO WCH
                       DO PREGUNTA
                       IF WCH = "S"
                          SAVE SCRE TO SCREEJC
                          STORE WUNIEJE TO WSEL
                          DO PRUNID
                          STORE WSEL TO WUNIEJE
                          RESTORE SCRE FROM SCREEJC
                          LOOP
                       ENDIF
                    ELSE
                       STORE ALLTRIM(DESCRI) TO TEX
                       DO AVISO WITH TEX
                       @ 17,55 SAY SUBSTR(DESCRI,1,24)
                    ENDIF
                 ENDIF

                 IF WLARCOD = 30
                    SELECT PRPARTI
                    STORE WPARTID+SPACE(6) TO WCLAVE
                    SEEK WCLAVE
                    IF FOUND()
                       STORE TIPO TO WTIPO
                    ELSE
                       STORE SPACE(1) TO WTIPO
                    ENDIF
                    DO WHILE .T.
                       STORE "TIPO DE EGRESO: (C)orriente, (I)nversion" TO MES
                       DO MENSAJE WITH MES
                       @ 17,51 GET WTIPO
                       READ
                       IF WTIPO="C".OR.WTIPO="I"
                          EXIT
                       ENDIF
                    ENDDO
                    SELECT PRCTAS
                    IF WTIPO="I"
                       *** TIPO DE INVERSION
                       DO WHILE .T.
                          STORE "TIPO DE INVERSION: (O)bra, o(R)dinaria" TO MES
                          DO MENSAJE WITH MES
                          @ 20,51 GET WTIPOINV
                          READ
                          IF WTIPOINV="O".OR.WTIPOINV="R"
                             EXIT
                          ENDIF
                       ENDDO
                       IF WTIPOINV="O"
                          DO WHILE .T.
                             @ 19,60 GET WMUNICI
                             READ
                             SELECT PRMUNICI
                             SEEK WMUNICI
                             IF .NOT.FOUND()
                                STORE "MUNICIPIO NO REGISTRADO, INGRESAR ? (S/N)" TO TEX
                                STORE "SN" TO WCH
                                DO PREGUNTA
                                IF WCH = "S"
                                   SAVE SCRE TO SCREEJC
                                   STORE SPACE(2) TO WSEL
                                   DO PRMUNI
                                   STORE WSEL TO WMUNICI
                                   RESTORE SCRE FROM SCREEJC
                                   LOOP
                                ENDIF
                             ELSE
                                @ 20,55 SAY SUBSTR(DESCRI,1,24)
                                EXIT
                             ENDIF
                             *IF WMUNICI<>WSUBESP
                             *   STORE "EN CODIGO DE OBRAS, LA SUB-ESPECIFICA DEBE SER IGUAL CODIGO DE MUNICIPIO" TO TEX
                             *   DO AVISO WITH TEX
                             *ELSE
                             *   EXIT
                             *ENDIF
                          ENDDO
                       ELSE
                          STORE SPACE(2) TO WMUNICI
                       ENDIF
                    ELSE
                       STORE SPACE(1) TO WTIPOINV
                       STORE SPACE(2) TO WMUNICI
                    ENDIF
                    IF WTIPO="C"
                       @ 03,62 GET Wmonto PICTURE "99,999,999,999.99"
                    ELSE
                       @ 04,62 GET Wmonto PICTURE "99,999,999,999.99"
                    ENDIF
                    READ
                    IF WTIPO="C"
                       STORE WMONTO TO WMONTOC
                       STORE 0      TO WMONTOI
                    ELSE
                       STORE 0      TO WMONTOC
                       STORE WMONTO TO WMONTOI
                    ENDIF
                 ENDIF

                 STORE "CONFORME ? (S/N)" TO TEX
                 STORE "SN" TO WCH
                 DO PREGUNTA
                 RESTORE SCRE FROM S8
                 IF WCH = "N"
                    STORE WLINER1  - 1  TO WLINER1 
                    LOOP
                 ENDIF
                 SELECT PRCTAS
                 DO FILLOC
                 APPEND BLANK
                 UNLOCK all 
                 DO RECLOC
                 REPLACE PREANO     WITH WPREANO
                 REPLACE PRETIP     WITH WPRETIP
                 REPLACE PREORI     WITH WPREORI
                 REPLACE SECTOR     WITH WSECTOR
                 REPLACE PROGRA     WITH WPROGRA
                 REPLACE SUBPRO     WITH WSUBPRO
                 REPLACE PROYEC     WITH WPROYEC
                 REPLACE ACTIVI     WITH WACTIVI
                 REPLACE PARTID     WITH WPARTID
                 REPLACE GENERI     WITH WGENERI
                 REPLACE ESPECI     WITH WESPECI
                 REPLACE SUBESP     WITH WSUBESP
                 REPLACE ORDINA     WITH WORDINA
                 REPLACE DESCRI1    WITH WDESCRI1
                 REPLACE DESCRI2    WITH WDESCRI2
                 REPLACE DESCRI3    WITH WDESCRI3
                 REPLACE DESCRI4    WITH WDESCRI4
                 REPLACE UNIEJE     WITH WUNIEJE
                 REPLACE TIPOINV    WITH WTIPOINV
                 REPLACE MUNICI     WITH WMUNICI
                 REPLACE USERNAME   WITH WUSERCODE
                 REPLACE USERDATE   WITH DATE()
                 FLUSH
                 IF WLARCOD = 30
                    STORE WCUENTA0 TO WNOCTA
                    ***
                    STORE "ORIG"   TO WCAMPO
                    STORE WMONTO   TO WMONTOPROC
                    DO ACTUALIZA
                    ***
                    IF WTIPO="C"
                       STORE "CORR"   TO WCAMPO
                       STORE WMONTOC  TO WMONTOPROC
                       DO ACTUALIZA
                    ELSE
                       STORE "INVE"   TO WCAMPO
                       STORE WMONTOI  TO WMONTOPROC
                       DO ACTUALIZA
                    ENDIF
                 ENDIF
                 UNLOCK ALL
              ENDDO
              SELECT PRCTAS
              RETURN
