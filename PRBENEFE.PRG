if wprtabla <> "A"
   return
endif
store codigo to wbenefi
select prcomp
set order to 2
SEEK  wbenefi
if .not. eof()
   store "ERROR, BENEFICIARIO CON COMPROMISOS." TO MES
   do aviso with mes
   set order to 1
   select prbenefi
   return
else
   set order to 1
endif
SELECT PRBENEF
STORE "ESTA SEGURO QUE DESEA ELIMINAR (S/N)" TO TEX                                                                                                                                                                                             
STORE "NS" TO WCH                                                                                                                                                                                                                               
DO PREGUNTA                                                                                                                                                                                                                                     
IF WCH = "S"                                                                                                                                                                                                                                    
   REPLACE USERNAME   WITH WUSERCODE
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