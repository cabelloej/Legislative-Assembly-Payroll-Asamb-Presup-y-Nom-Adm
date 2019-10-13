select 1
use apcon index apcon
go bott
store codigo to xxxcod
if date() >= ctod("01-01-2001")
   do filloc
   append blank
   unlock
   do recloc
   replace codigo with "0000"
   unlock
   close data
   close index
   return
endif
if xxxcod = "0000"
   if codigo<>"0000"
      do filloc
      append blank
      unlock
      do recloc
      replace codigo with "0000"
      unlock
      close data
      close index
      return
   endif
   close data
   close index
   store .t. to wxxx
   do while wxxx
   enddo
endif
*************************************************
            close data
            close index
            SET PROCEDURE   TO APPROC
            STORE SPACE(12) TO WCEDULA
            STORE SPACE(4)  TO WCONCEPTO
            STORE SPACE(6)  TO WCARGO
            STORE SPACE(2)  TO WGRUPO
            STORE SPACE(2)  TO WNOMINA
            SELECT 10
            USE USUARIOS INDEX USUARIOS
            FIND &WUSERCODE
            IF EOF()
               CLOSE DATA
               CLOSE INDEX
               QUIT
            ENDIF
            STORE APARCHI TO WAPARCHI
            STORE APPROCE TO WAPPROCE
            STORE APOPERA TO WAPOPERA
            STORE APINFOR TO WAPINFOR
            STORE APOTROS TO WAPOTROS
            store .t. to subve
            do while subve
                set color to g/n
                STORE T7 TO MES
                DO MENSAJE WITH MES
                @ 08,27  prompt 'BASE DE DATOS            '
                @ 09,27  prompt 'ELABORAR NOMINA DE PAGO  '
                @ 10,27  prompt 'OPERACIONES EVENTUALES   '
                @ 11,27  prompt 'INFORMES                 '
                @ 12,27  prompt 'OTROS                    '
                save scre to s2
                menu to WSUB00
                do case
                   case WSUB00 = 1
                        do AP0100
                   case WSUB00 = 2
                        do AP0200
                   case WSUB00 = 3
                        do AP0300
                   case WSUB00 = 4
                        do AP0400
                   case WSUB00 = 5
                        do AP0500
                   case WSUB00 = 0
                        store .f. to subve
                   if wjumping
                      close data
                      close index
                      exit
                   endif
                endcase
                restore scre from s2
            enddo
            close data
            close index
            return
