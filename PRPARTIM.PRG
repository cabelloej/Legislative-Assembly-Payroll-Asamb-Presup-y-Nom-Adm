if wprtabla <> "A"
   return
endif
              STORE .T. TO WACT                                                                                                                                                                                                                               
              DO WHILE WACT                                                                                                                                                                                                                                   
                 STORE DESCRI  TO WDESCRI
                 STORE DESCRI2 TO WDESCRI2
                 STORE TIPO   TO WTIPO
                 @ WSCRELIN+0,20  GET WDESCRI
                 @ 22        ,20  GET WDESCRI2
                 READ
                 STORE PARTIDA+GENERICA+ESPECIFICA+SUBESP TO WCODIGO
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
                 IF READKEY()=12.OR.READKEY()=268
                    STORE .F. TO WACT                                                                                                                                                                                                                         
                    GO TOP                                                                                                                                                                                                                                    
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE 0 TO WREVERSE                                                                                                                                                                                                                       
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 STORE "CONFORME ? (S/N)" TO TEX                                                                                                                                                                                                              
                 STORE "SN" TO WCH                                                                                                                                                                                                                            
                 DO PREGUNTA                                                                                                                                                                                                                                  
                 IF WCH = "N"                                                                                                                                                                                                                                 
                    STORE WSCRELIN - 1  TO WSCRELIN                                                                                                                                                                                                           
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                      
                 REPLACE DESCRI     WITH WDESCRI                                                                                                                                                                                                              
                 REPLACE DESCRI2    WITH WDESCRI2
                 REPLACE TIPO       WITH WTIPO
                 REPLACE USERNAME   WITH WUSERCODE
                 REPLACE USERDATE   WITH DATE()
                 FLUSH                                                                                                                                                                                                                                        
                 STORE .F. TO WACT
              ENDDO                                                                                                                                                                                                                                           

