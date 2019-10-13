IF WPRTABLA <> "A"
   RETURN
ENDIF
              STORE 100 TO WSCRELIN
              STORE .T. TO WACT
              STORE "    " TO WPARTIDA
              STORE "  "   TO WGENERICA
              STORE "  "   TO WESPECIFICA
              STORE "  "   TO WSUBESP
              DO WHILE WACT
                 STORE SPACE(LEN(DESCRI))  TO WDESCRI
                 STORE SPACE(LEN(DESCRI2)) TO WDESCRI2
                 STORE SPACE(LEN(TIPO))   TO WTIPO
                 STORE WSCRELIN + 1 TO WSCRELIN
                 IF WSCRELIN > 18
                    SET COLOR TO G/N
                    @ 7,1 CLEAR TO 18,78
                    STORE 7 TO WSCRELIN
                 ENDIF
                 @ WSCRELIN ,01  GET WPARTIDA    PICTURE "#.##"
                 @ WSCRELIN ,06  GET WGENERICA   PICTURE "##"
                 @ WSCRELIN ,09  GET WESPECIFICA PICTURE "##"
                 @ WSCRELIN ,12  GET WSUBESP     PICTURE "##"
                 READ                                                                                                                                                                                                                                         
                 STORE WPARTIDA+WGENERICA+WESPECIFICA+WSUBESP  TO WCODIGO
                 IF WCODIGO = SPACE(10).OR.READKEY()=12.OR.READKEY()=268
                    STORE .F. TO WACT                                                                                                                                                                                                                         
                    GO TOP                                                                                                                                                                                                                                    
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE 0 TO WREVERSE                                                                                                                                                                                                                       
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 STORE RTRIM(LTRIM(WCODIGO)) TO XCODIGO                                                                                                                                                                                                       
                 IF AT(" ",XCODIGO) > 0                                                                                                                                                                                                                       
                    STORE "ERROR DE CODIFICACION. NO DEBEN EXISTIR ESPACIOS EN BLANCO DENTRO DEL CODIGO" TO MES                                                                                                                                               
                    DO AVISO WITH MES                                                                                                                                                                                                                         
                    STORE WSCRELIN - 1  TO WSCRELIN                                                                                                                                                                                                           
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 IF (LEN(WCODIGO) <> LEN(LTRIM(WCODIGO)))                                                                                                                                                                                                     
                    STORE "ERROR DE CODIFICACION. NO DEBEN EXISTIR ESPACIOS EN BLANCO DENTRO DEL CODIGO" TO MES                                                                                                                                               
                    DO AVISO WITH MES                                                                                                                                                                                                                         
                    STORE WSCRELIN - 1  TO WSCRELIN                                                                                                                                                                                                           
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF
                 SELECT PRPARTI
                 SEEK WCODIGO
                 IF .NOT. EOF()
                    @ WSCRELIN ,20  SAY DESCRI
                    STORE "CODIGO YA EXISTE. <ENTER>" TO MES                                                                                                                                                                                                  
                    DO AVISO WITH MES                                                                                                                                                                                                                         
                    STORE WSCRELIN - 1  TO WSCRELIN                                                                                                                                                                                                           
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 @ WSCRELIN+0 ,20  GET WDESCRI
                 @ WSCRELIN+1 ,20  GET WDESCRI2
                 READ
                 IF LEN(RTRIM(WCODIGO))=4
                    DO WHILE .T.
                       STORE "TIPOS: (C)orriente, (I)nversion" TO MES
                       DO MENSAJE WITH MES
                       @ WSCRELIN+0 ,75  GET WTIPO
                       READ
                       IF WTIPO="C".OR.WTIPO="I"
                          EXIT
                       ENDIF
                    ENDDO
                 ENDIF
                 STORE "CONFORME ? (S/N)" TO TEX                                                                                                                                                                                                              
                 STORE "SN" TO WCH                                                                                                                                                                                                                            
                 DO PREGUNTA                                                                                                                                                                                                                                  
                 IF WCH = "N"                                                                                                                                                                                                                                 
                    STORE WSCRELIN - 1  TO WSCRELIN                                                                                                                                                                                                           
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                      
                 DO FILLOC
                 APPEND BLANK
                 UNLOCK
                 do recloc                                                                                                                                                                                                                                    
                 REPLACE PARTIDA    WITH WPARTIDA                                                                                                                                                                                                             
                 REPLACE GENERICA   WITH WGENERICA                                                                                                                                                                                                            
                 REPLACE ESPECIFICA WITH WESPECIFICA                                                                                                                                                                                                          
                 REPLACE SUBESP     WITH WSUBESP
                 REPLACE DESCRI     WITH WDESCRI                                                                                                                                                                                                              
                 REPLACE DESCRI2    WITH WDESCRI2
                 REPLACE TIPO       WITH WTIPO
                 REPLACE USERNAME   WITH WUSERCODE
                 REPLACE USERDATE   WITH DATE()
                 FLUSH                                                                                                                                                                                                                                        
                 unlock                                                                                                                                                                                                                                       
              ENDDO                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                              
