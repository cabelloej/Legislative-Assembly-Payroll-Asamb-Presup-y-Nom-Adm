if wprtabla <> "A"
   return
endif

              STORE "ESTA SEGURO QUE DESEA ELIMINAR (S/N)" TO TEX                                                                                                                                                                                             
              STORE "NS" TO WCH                                                                                                                                                                                                                               
              DO PREGUNTA                                                                                                                                                                                                                                     
              IF WCH = "S"                                                                                                                                                                                                                                    
                 REPLACE USERNAME   WITH wusercode
                 REPLACE USERDATE   WITH DATE()
                 DELETE                                                                                                                                                                                                                                       
                 FLUSH
                 STORE .T. TO FLAGDEL                                                                                                                                                                                                                         
                 SKIP                                                                                                                                                                                                                                         
                 IF .NOT. EOF()                                                                                                                                                                                                                               
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE RECNO() TO WREVERSE                                                                                                                                                                                                                 
                    STORE RECNO() TO WCLEARREC                                                                                                                                                                                                                
                    STORE WLINE   TO WEDILIN                                                                                                                                                                                                                  
                    STORE WLINE   TO WCLEARLIN                                                                                                                                                                                                                
                 ELSE                                                                                                                                                                                                                                         
                    GO TOP                                                                                                                                                                                                                                    
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE 0 TO WREVERSE                                                                                                                                                                                                                       
                 ENDIF                                                                                                                                                                                                                                        
              ENDIF                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                              
