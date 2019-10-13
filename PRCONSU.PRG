              store recno() to wstarting
              @ 21,0 CLEAR TO 24,79
              STORE .T. TO WACTX
              DO WHILE WACTX
                 SELECT PRCTAS
                 STORE DESCRI1    TO WDESCRI1
                 STORE DESCRI2    TO WDESCRI2
                 STORE DESCRI3    TO WDESCRI3
                 STORE DESCRI4    TO WDESCRI4
                 STORE UNIEJE     TO WUNIEJE
                 STORE ORIG       TO WORIG,WLASTORIG
                 STORE TRAS       TO WTRAS
                 STORE INCR       TO WINCR
                 STORE COMP       TO WCOMP
                 STORE CAUS       TO WCAUS
                 STORE PAGA       TO WPAGA
                 STORE SPACE(30)  TO WCUENTA0
                 DO ARMACOD0
                 STORE LEN(RTRIM(WCUENTA0)) TO WLARCOD
                 SAVE SCRE TO S8

                 @ 0,0 CLEAR
                 SET COLOR TO N/G
                 @ 0,0
                 @ 0,0 SAY QQWW
                 @ 00,00  SAY "          P L A N   D E   C U E N T A S   D E L    P R E S U P U E S T O       "
                 SET COLOR TO G+/N
                 @ 02,00 SAY "+-----------------------------------------------------+------------------------+"
                 @ 03,00 SAY "|A¥O:                                                 |ORG.:                   |"
                 @ 04,00 SAY "|TIP:                                                 |------------------------|"
                 @ 05,00 SAY "|ORI:                                                 |TRA.:                   |"
                 @ 06,00 SAY "|SEC:                                                 |------------------------|"
                 @ 07,00 SAY "|PRO:                                                 |INC.:                   |"
                 @ 08,00 SAY "|SUP:                                                 |------------------------|"
                 @ 09,00 SAY "|PRO:                                                 |MOD.:                   |"
                 @ 10,00 SAY "|ACT:                                                 |------------------------|"
                 @ 11,00 SAY "|PAR:                                                 |COM.:                   |"
                 @ 12,00 SAY "|GEN:                                                 |------------------------|"
                 @ 13,00 SAY "|ESP:                                                 |CAU.:                   |"
                 @ 14,00 SAY "|SUE:                                                 |------------------------|"
                 @ 15,00 SAY "+-----------------------------------------------------|PAG.:                   |"
                 @ 16,00 SAY "|COD:                                            TIPO |------------------------|"
                 @ 17,00 SAY "|                                                     |DIS.:                   |"
                 @ 18,00 SAY "|                                                     |------------------------|"
                 @ 19,00 SAY "|                                                     |EJE.:                   |"
                 @ 20,00 SAY "|                                                     |                        |"
                 @ 21,00 SAY "+-----------------------------------------------------+------------------------+"
                 SET COLOR TO G/N
                 SELECT PRCTAS
                 STORE SUBSTR(WCUENTA0,1,2) TO WWPREANO
                 seek WWPREANO
                 IF EOF()
                    STORE SPACE(25) TO WDESPREANO
                 ELSE
                    STORE DESCRI1 TO WDESPREANO
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,4) TO WWPRETIP
                 seek WWPRETIP
                 IF EOF()
                    STORE SPACE(25) TO WDESPRETIP
                 ELSE
                    STORE DESCRI1 TO WDESPRETIP
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,7) TO WWPREORI
                 seek WWPREORI
                 IF EOF()
                    STORE SPACE(25) TO WDESPREORI
                 ELSE
                    STORE DESCRI1 TO WDESPREORI
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,9) TO WWSECTOR
                 seek WWSECTOR
                 IF EOF()
                    STORE SPACE(25) TO WDESSECTOR
                 ELSE
                    STORE DESCRI1 TO WDESSECTOR
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,11) TO WWPROGRA
                 seek WWPROGRA
                 IF EOF()
                    STORE SPACE(25) TO WDESPROGRA
                 ELSE
                    STORE DESCRI1 TO WDESPROGRA
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,13) TO WWSUBPRO
                 seek WWSUBPRO
                 IF EOF()
                    STORE SPACE(25) TO WDESSUBPRO
                 ELSE
                    STORE DESCRI1 TO WDESSUBPRO
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,15) TO WWPROYEC
                 seek WWPROYEC
                 IF EOF()
                    STORE SPACE(25) TO WDESPROYEC
                 ELSE
                    STORE DESCRI1 TO WDESPROYEC
                 ENDIF
                 STORE SUBSTR(WCUENTA0,1,17) TO WWACTIVI
                 seek WWACTIVI
                 IF EOF()
                    STORE SPACE(25) TO WDESACTIVI
                 ELSE
                    STORE DESCRI1 TO WDESACTIVI
                 ENDIF
                 SELECT PRPARTI
                 STORE SUBSTR(WCUENTA0,18,4) TO WWPARTID
                 seek WWPARTID
                 IF EOF()
                    STORE SPACE(25) TO WDESPARTID
                 ELSE
                    STORE DESCRI TO WDESPARTID
                 ENDIF
                 STORE SUBSTR(WCUENTA0,18,6) TO WWGENERI
                 seek WWGENERI
                 IF EOF()
                    STORE SPACE(25) TO WDESGENERI
                 ELSE
                    STORE DESCRI TO WDESGENERI
                 ENDIF
                 STORE SUBSTR(WCUENTA0,18,8) TO WWESPECI
                 seek WWESPECI
                 IF EOF()
                    STORE SPACE(25) TO WDESESPECI
                 ELSE
                    STORE DESCRI TO WDESESPECI
                 ENDIF
                 STORE SUBSTR(WCUENTA0,18,10) TO WWSUBESP
                 seek WWSUBESP
                 IF EOF()
                    STORE SPACE(25) TO WDESSUBESP
                 ELSE
                    STORE DESCRI TO WDESSUBESP
                 ENDIF
                 SELECT PRUNID
                 seek WUNIEJE
                 IF EOF()
                    STORE SPACE(24) TO WDESUNIEJE
                 ELSE
                    STORE DESCRI    TO WDESUNIEJE
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

                 @ 03,62 SAY WORIG PICTURE "99,999,999,999.99"
                 @ 05,62 SAY WTRAS PICTURE "99,999,999,999.99"
                 @ 07,62 SAY WINCR PICTURE "99,999,999,999.99"
                 @ 09,62 SAY WORIG+WTRAS+WINCR PICTURE "99,999,999,999.99"
                 @ 11,62 SAY WCOMP PICTURE "99,999,999,999.99"
                 @ 13,62 SAY WCAUS PICTURE "99,999,999,999.99"
                 @ 15,62 SAY WPAGA PICTURE "99,999,999,999.99"
                 @ 17,62 SAY (WORIG+WTRAS+WINCR)-WCOMP PICTURE "99,999,999,999.99"
                 @ 19,62 SAY WUNIEJE
                 @ 20,55 SAY WDESUNIEJE

                 @ 17,01 GET WDESCRI1
                 @ 18,01 GET WDESCRI2
                 @ 19,01 GET WDESCRI3
                 @ 20,01 GET WDESCRI4
                 *** CONTROL DE ACSESO
                 IF WPRFORMU <> "A"
                    CLEAR GETS
                    STORE "OPRIMA <ENTER> PARA CONTINUAR" TO MES
                    DO AVISO WITH MES
                    SELECT PRCTAS
                    RESTORE SCRE FROM S8
                    STORE .F. TO WACTX
                    RETURN
                 ENDIF
                 ***
                 READ

                 IF READKEY() = 12 .OR. READKEY() = 268
                    SELECT PRCTAS
                    RESTORE SCRE FROM S8
                    STORE .F. TO WACTX
                    RETURN
                 ENDIF

                 IF WLARCOD=11
                    @ 19,62 GET WUNIEJE
                    READ
                    SELECT PRUNID
                    seek WUNIEJE
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
                       @ 20,55 SAY DESCRI
                    ENDIF
                 ENDIF
                 
                 store .f. to wrecalc
                 IF WLARCOD = 30
                    STORE WORIG TO WLASTORIG
                    @ 03,62 GET WORIG PICTURE "99,999,999,999.99"
                    READ
                    IF WORIG+WTRAS+WINCR < WCOMP
                       STORE "MONTO ORIGINAL+TRASPASOS+INCREMENTOS NO CUBRE TRANSACCIONES DE LA CUENTA" TO MES
                       DO AVISO WITH MES
                       STORE WLASTORIG TO WORIG
                       @ 03,62 SAY WORIG PICTURE "99,999,999,999.99"
                    ENDIF
                    IF WLASTORIG <> WORIG
                       STORE .T. TO WRECALC
                    ELSE
                       STORE .F. TO WRECALC
                    ENDIF
                 ENDIF
                 SELECT PRCTAS
                 GO WSTARTING

                 STORE "CONFORME ? (S/N)" TO TEX
                 STORE "SN" TO WCH
                 DO PREGUNTA
                 RESTORE SCRE FROM S8
                 IF WCH = "N"
                    LOOP
                 ENDIF
                 REPLACE DESCRI1    WITH WDESCRI1
                 REPLACE DESCRI2    WITH WDESCRI2
                 REPLACE DESCRI3    WITH WDESCRI3
                 REPLACE DESCRI4    WITH WDESCRI4
                 REPLACE UNIEJE     WITH WUNIEJE
                 REPLACE USERNAME   WITH WUSERCODE
                 REPLACE USERDATE   WITH DATE()
                 FLUSH
                 IF WRECALC
                    if  WLARCOD = 30
                       STORE .T. TO FLAGJUMP0
                       *** ELIMINA EL VIEJO MONTO
                       STORE WCUENTA0      TO WNOCTA
                       STORE "ORIG"        TO WCAMPO
                       STORE WLASTORIG*-1  TO WMONTOPROC
                       DO ACTUALIZA
                       *** INGRESA EL NUEVO MONTO
                       STORE WCUENTA0      TO WNOCTA
                       STORE "ORIG"        TO WCAMPO
                       STORE WORIG         TO WMONTOPROC
                       DO ACTUALIZA
                    endif
                 ENDIF
                 STORE .F. TO WACTX
              ENDDO
              SELECT PRCTAS
              RETURN


