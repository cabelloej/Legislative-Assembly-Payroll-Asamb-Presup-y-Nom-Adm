SELECT prparti                                                                                                                                        
STORE .F. TO FLAGDEL                                                                                                                                  
SET COLOR TO G/N                                                                                                                                      
@ 4,0 CLEAR                                                                                                                                           
SET COLOR TO W+/B                                                                                                                                     
STORE "CLASIFICADOR DE PARTIDAS" TO WHEADER                                                                                                           
@ 4,40 - (LEN(WHEADER)/2) SAY WHEADER                                                                                                                 
SET COLOR TO W+/B                                                                                                                                     
@ 5,0                                                                                                                                                 
@ 5,01 SAY "G.PA.GE.ES.SE"
@ 5,20 SAY "DESCRIPCION"
@ 5,75 SAY "TIPO"
*@ 21,0 SAY "|PgUp=Pag.Arr|PgDn=Pag.Abj|Home=Inic|End=Final|ç=Ant.|é=Sig.|ESC=Salir|Y=Acep.|"
SET COLOR TO B+/N                                                                                                                                     
@ 6,0 TO 19,79                                                                                                                                        
SET COLOR TO G/N                                                                                                                                      
STORE  "INCLUIR    " TO XMENU1                                                                                                                        
STORE  "MODIFICAR  " TO XMENU2                                                                                                                        
STORE  "ELIMINAR   " TO XMENU3                                                                                                                        
STORE  "SELECCIONAR" TO XMENU4                                                                                                                        
STORE  "BUSCAR     " TO XMENU5                                                                                                                        
STORE 4 TO WMENU                                                                                                                                      
STORE .T. TO XFLAG                                                                                                                                    
STORE .F. TO WREGS                                                                                                                                    
GO TOP                                                                                                                                                
DO WHILE .NOT. EOF()                                                                                                                                  
   STORE .T. TO WREGS                                                                                                                                 
   EXIT                                                                                                                                               
ENDDO                                                                                                                                                 
IF .NOT. WREGS                                                                                                                                        
   DO PRPARTII
ENDIF
STORE .F. TO WREGS
GO TOP                                                                       
DO WHILE .NOT. EOF()                                                                                                                                  
   STORE .T. TO WREGS                                                                                                                                 
   EXIT                                                                                                                                               
ENDDO                                                                                                                                                 
IF .NOT. WREGS                                                                                                                                        
   RETURN
ENDIF
GO TOP                                                                                                                                                
STORE 12 TO WRANGO                                                                                                                                    
STORE RECNO() TO WINICIO                                                                                                                              
STORE 0 TO WREVERSE                                                                                                                                   
STORE 0 TO WEDILIN                                                                                                                                    
STORE .T. TO FLAGJUMP                                                                                                                                 
STORE .T. TO WEDITAR                                                                                                                                  
DO WHILE WEDITAR                                                                                                                                      
   IF FLAGJUMP                                                                                                                                        
      STORE 0       TO WCONT                                                                                                                          
      STORE 07      TO WLINE                                                                                                                          
      GO WINICIO                                                                                                                                      
      SET COLOR TO G/N                                                                                                                                
   *  @ 7,1 CLEAR TO 18,78                                                                                                                            
      DO WHILE .NOT. EOF() .AND. WCONT < WRANGO                                                                                                       
         STORE WLINE+WCONT TO WSCRELIN                                                                                                                
         STORE WLINE+WCONT TO WLASTLIN                                                                                                                
         STORE RECNO() TO WLASTREC                                                                                                                    
         @ WSCRELIN ,01  SAY PARTIDA+"."
         @ WSCRELIN ,06  SAY GENERICA+"."
         @ WSCRELIN ,09  SAY ESPECIFICA+"."
         @ WSCRELIN ,12  SAY SUBESP
         @ WSCRELIN ,20  SAY DESCRI
         @ WSCRELIN ,75  SAY TIPO
         STORE WCONT + 1 TO WCONT                                                                                                                     
         SKIP                                                                                                                                         
      ENDDO                                                                                                                                           
      STORE .F. TO FLAGJUMP                                                                                                                           
   ENDIF                                                                                                                                              
   *** RELLENO DE ESP. VACION (SI NO HAY SUF. REGISTROS)                                                                                              
   STORE WCONT TO WFILCON                                                                                                                             
   STORE WLASTLIN TO WFILLIN                                                                                                                          
   DO WHILE WFILCON < WRANGO                                                                                                                          
      STORE WFILLIN + 1 TO WFILLIN                                                                                                                    
      @ WFILLIN, 1 SAY SPACE(77)                                                                                                                      
      STORE WFILCON + 1 TO WFILCON                                                                                                                    
   ENDDO                                                                                                                                              
   ***                                                                                                                                                
   IF WREVERSE = 0                                                                                                                                    
      STORE WLASTREC TO WREVERSE                                                                                                                      
      STORE WLASTLIN TO WEDILIN                                                                                                                       
      STORE WINICIO  TO WCLEARREC                                                                                                                     
      STORE WLINE    TO WCLEARLIN                                                                                                                     
   ENDIF                                                                                                                                              
   ** PARA BORRAR EL REGISTRO ANTERIOR                                                                                                                
   SET COLOR TO G/N
   GO WCLEARREC                                                                                                                                       
   STORE WCLEARLIN TO WSCRELIN                                                                                                                        
   @ WSCRELIN ,01  SAY PARTIDA+"."
   @ WSCRELIN ,06  SAY GENERICA+"."
   @ WSCRELIN ,09  SAY ESPECIFICA+"."
   @ WSCRELIN ,12  SAY SUBESP
   @ WSCRELIN ,20  SAY DESCRI
   @ WSCRELIN ,75  SAY TIPO
   ** PARA MARCAR EL REGISTRO ACTUAL                                                                                                                  
   SET COLOR TO N/w                                                                                                                                   
   GO WREVERSE                                                                                                                                        
   STORE WEDILIN TO WSCRELIN                                                                                                                          
   @ WSCRELIN ,01  SAY PARTIDA+"."
   @ WSCRELIN ,06  SAY GENERICA+"."
   @ WSCRELIN ,09  SAY ESPECIFICA+"."
   @ WSCRELIN ,12  SAY SUBESP
   @ WSCRELIN ,20  SAY DESCRI
   @ WSCRELIN ,75  SAY TIPO
   @ 22,20         SAY DESCRI2
   STORE WREVERSE TO WCLEARREC                                                                                                                        
   STORE WSCRELIN TO WCLEARLIN                                                                                                                        
   SET COLOR TO G/N                                                                                                                                   
   *** PARA INDICAR LA ROTACION DEL MENU (SI LA HUBO)                                                                                                 
   IF XFLAG                                                                                                                                           
      SET COLOR TO G/N                                                                                                                                
      @ 20,05 SAY  XMENU1                                                                                                                             
      @ 20,20 SAY  XMENU2                                                                                                                             
      @ 20,35 SAY  XMENU3                                                                                                                             
      @ 20,50 SAY  XMENU4                                                                                                                             
      @ 20,65 SAY  XMENU5                                                                                                                             
      SET COLOR TO N/G                                                                                                                                
      STORE "XMENU"+STR(WMENU,1) TO WMACRO                                                                                                            
      @ 20,5+((WMENU-1)*15)  SAY &WMACRO                                                                                                              
      STORE .F. TO XFLAG                                                                                                                              
      SET COLOR TO G/N                                                                                                                                
   ENDIF                                                                                                                                              
   *** CAPTURA DE INTERACTIVIDAD                                                                                                                      
   CLEAR TYPEAHEAD                                                                                                                                    
   STORE INKEY(0) TO WMUEVE                                                                                                                           
   *** PAGE DOWN                                                                                                                                      
   IF WMUEVE = 3                                                                                                                                      
      GO WINICIO                                                                                                                                      
      SKIP +12                                                                                                                                        
      IF .NOT. EOF()                                                                                                                                  
         STORE RECNO() TO WINICIO                                                                                                                     
         STORE .T.     TO FLAGJUMP                                                                                                                    
         STORE WINICIO TO WREVERSE                                                                                                                    
         STORE WLINE   TO WEDILIN                                                                                                                     
         STORE WINICIO TO WCLEARREC                                                                                                                   
         STORE WLINE   TO WCLEARLIN                                                                                                                   
      ENDIF                                                                                                                                           
   ENDIF                                                                                                                                              
   *** PAGE UP                                                                                                                                        
   IF WMUEVE =  18                                                                                                                                    
      GO WINICIO                                                                                                                                      
      SKIP -12                                                                                                                                        
      STORE RECNO() TO WINICIO                                                                                                                        
      STORE .T. TO FLAGJUMP                                                                                                                           
      STORE 0 TO WREVERSE                                                                                                                             
   ENDIF                                                                                                                                              
   *** HOME                                                                                                                                           
   IF WMUEVE = 1                                                                                                                                      
      GO TOP                                                                                                                                          
      IF .NOT. EOF()                                                                                                                                  
         STORE RECNO() TO WINICIO                                                                                                                     
         STORE .T. TO FLAGJUMP                                                                                                                        
         STORE WINICIO TO WREVERSE                                                                                                                    
         STORE WLINE   TO WEDILIN                                                                                                                     
         STORE WINICIO TO WCLEARREC                                                                                                                   
         STORE WLINE   TO WCLEARLIN                                                                                                                   
      ENDIF                                                                                                                                           
   ENDIF                                                                                                                                              
   *** END                                                                                                                                            
   IF WMUEVE = 6                                                                                                                                      
      GO BOTT                                                                                                                                         
      SKIP -11                                                                                                                                        
      STORE RECNO() TO WINICIO                                                                                                                        
      STORE 0 TO WREVERSE                                                                                                                             
      STORE .T. TO FLAGJUMP                                                                                                                           
   ENDIF                                                                                                                                              
   *** DOWN ARROW                                                                                                                                     
   IF WMUEVE = 24                                                                                                                                     
      SKIP + 1                                                                                                                                        
      IF WEDILIN < (WLINE+(WCONT-1))                                                                                                                  
         STORE WREVERSE    TO WCLEARREC                                                                                                               
         STORE RECNO()     TO WREVERSE                                                                                                                
         STORE WEDILIN     TO WCLEARLIN                                                                                                               
         STORE WEDILIN + 1 TO WEDILIN                                                                                                                 
      ELSE                                                                                                                                            
         IF .NOT. EOF()                                                                                                                               
            GO WINICIO                                                                                                                                
            SKIP +1                                                                                                                                   
            STORE RECNO() TO WINICIO                                                                                                                  
            STORE .T.     TO FLAGJUMP                                                                                                                 
            STORE 0       TO WREVERSE                                                                                                                 
         ELSE                                                                                                                                         
            GO WINICIO                                                                                                                                
            STORE RECNO() TO WINICIO                                                                                                                  
            STORE .T.     TO FLAGJUMP                                                                                                                 
            STORE 0       TO WREVERSE                                                                                                                 
         ENDIF                                                                                                                                        
      ENDIF                                                                                                                                           
   ENDIF                                                                                                                                              
   *** UP ARROW                                                                                                                                       
   IF WMUEVE = 5                                                                                                                                      
      SKIP -1                                                                                                                                         
      IF WEDILIN > WLINE                                                                                                                              
         STORE WREVERSE    TO WCLEARREC                                                                                                               
         STORE RECNO()     TO WREVERSE                                                                                                                
         STORE WEDILIN     TO WCLEARLIN                                                                                                               
         STORE WEDILIN - 1 TO WEDILIN                                                                                                                 
      ELSE                                                                                                                                            
         GO WINICIO                                                                                                                                   
         SKIP -1                                                                                                                                      
         STORE RECNO() TO WINICIO                                                                                                                     
         STORE .T.     TO FLAGJUMP                                                                                                                    
         STORE WINICIO TO WREVERSE                                                                                                                    
         STORE WLINE   TO WEDILIN                                                                                                                     
         STORE WINICIO TO WCLEARREC                                                                                                                   
         STORE WLINE   TO WCLEARLIN                                                                                                                   
      ENDIF                                                                                                                                           
   ENDIF                                                                                                                                              
   *** RIGHT ARROW                                                                                                                                    
   IF WMUEVE = 4                                                                                                                                      
      STORE .T. TO XFLAG                                                                                                                              
      STORE WMENU+1 TO WMENU                                                                                                                          
      IF WMENU > 5                                                                                                                                    
         STORE 1 TO WMENU                                                                                                                             
      ENDIF                                                                                                                                           
   ENDIF                                                                                                                                              
   *** LEFT ARROW                                                                                                                                     
   IF WMUEVE = 19                                                                                                                                     
      STORE .T. TO XFLAG                                                                                                                              
      STORE WMENU-1 TO WMENU                                                                                                                          
      IF WMENU < 1                                                                                                                                    
         STORE 5 TO WMENU                                                                                                                             
      ENDIF                                                                                                                                           
   ENDIF                                                                                                                                              
   *** ESCAPE                                                                                                                                         
   IF WMUEVE = 27                                                                                                                                     
      STORE PARTIDA+GENERICA+ESPECIFICA+SUBESP  TO WCODIGO
      STORE WCODIGO TO WSEL                                                                                                                           
      RETURN                                                                                                                                          
   ENDIF                                                                                                                                              
   *** ENTER (INICIO DE OPERACIONES DE ACTUALIZACION DE DATOS                                                                                         
   IF WMUEVE = 13                                                                                                                                     
      DO CASE                                                                                                                                         
         CASE WMENU = 1                                                                                                                               
              DO PRPARTII
         CASE WMENU = 2                                                                                                                               
              DO RECLOC                                                                                                                               
              DO PRPARTIM
              UNLOCK ALL                                                                                                                              
         CASE WMENU = 3                                                                                                                               
              DO RECLOC                                                                                                                               
              DO PRPARTIE
              UNLOCK ALL                                                                                                                              
         CASE WMENU = 4                                                                                                                               
              RETURN                                                                                                                                  
         CASE WMENU = 5                                                                                                                               
              SAVE SCRE TO S9                                                                                                                         
              STORE "    "  TO WPARTID                                                                                                                
              STORE "  "    TO WGENERI
              STORE "  "    TO WESPECI
              STORE "  "    TO WSUBESP
              @ WSCRELIN ,01  GET WPARTID PICTURE "#.##"
              @ WSCRELIN ,06  GET WGENERI PICTURE "##"
              @ WSCRELIN ,09  GET WESPECI PICTURE "##"
              @ WSCRELIN ,12  GET WSUBESP PICTURE "##"
              READ                                                                                                                                    
              STORE WPARTID+WGENERI+WESPECI+WSUBESP TO WCODIGO
              SELECT PRPARTI
              SEEK WCODIGO
              IF .NOT. EOF()
                 STORE RECNO() TO WINICIO
                 STORE .T. TO FLAGJUMP
                 STORE RECNO() TO WREVERSE
                 STORE RECNO() TO WCLEARREC
                 STORE WLINE   TO WEDILIN
                 STORE WLINE   TO WCLEARLIN
              ELSE
                 RESTORE SCRE FROM S9
              ENDIF
      ENDCASE                                                                                                                                         
   ENDIF                                                                                                                                              
ENDDO                                                                                                                                                 
RETURN                                                                                                                                                
                                                                                                                                                      
