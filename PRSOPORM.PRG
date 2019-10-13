if wprformu <> "A"
   return
endif
              STORE DESCRI TO WDESCRI
              STORE .T. TO WACT                                                                                                                                                                                                                               
              DO WHILE WACT                                                                                                                                                                                                                                   
                 @ WSCRELIN ,10  GET WDESCRI
                 READ                                                                                                                                                                                                                                         
                 IF READKEY()=12.OR.READKEY()=268
                    STORE .F. TO WACT                                                                                                                                                                                                                         
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 STORE "CONFORME ? (S/N)" TO TEX                                                                                                                                                                                                              
                 STORE "SN" TO WCH                                                                                                                                                                                                                            
                 DO PREGUNTA                                                                                                                                                                                                                                  
                 IF WCH = "N"                                                                                                                                                                                                                                 
                    LOOP 
                 ENDIF                                                                                                                                                                                                                                        
                 REPLACE DESCRI WITH WDESCRI                                                                                                                                                                                                                  
                 REPLACE USERNAME   WITH WUSERCODE
                 REPLACE USERDATE   WITH DATE()
                 FLUSH                                                                                                                                                                                                                                        
                 STORE .F. TO WACT
              ENDDO                                                                                                                                                                                                                                           

