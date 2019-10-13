            set proc to prproc
            close data
            close index
            STORE SPACE(2)  TO WPREANO
            STORE SPACE(2)  TO WPRETIP
            STORE SPACE(3)  TO WPREORI
            STORE SPACE(2)  TO WSECTOR
            STORE SPACE(2)  TO WPROGRA
            STORE SPACE(2)  TO WSUBPRO
            STORE SPACE(2)  TO WPROYEC
            STORE SPACE(2)  TO WACTIVI
            STORE SPACE(4)  TO WPARTID
            STORE SPACE(2)  TO WGENERI
            STORE SPACE(2)  TO WESPECI
            STORE SPACE(2)  TO WSUBESP
            STORE SPACE(3)  TO WORDINA
            STORE SPACE(30) TO WCUENTA0
            SELECT 10
            USE USUARIOS INDEX USUARIOS
            SEEK WUSERCODE
            IF EOF()
               CLOSE DATA
               CLOSE INDEX
               QUIT
            ENDIF
            store prtabla to wprtabla
            store prformu to wprformu
            store prrefor to wprrefor
            store princre to wprincre
            store prconsu to wprconsu
            store prcompr to wprcompr
            store prcausa to wprcausa
            store prpagos to wprpagos
            store prinfor to wprinfor
            store protros to wprotros
            USE
            store .t. to subve
            do while subve
                set color to g/n
                STORE T7 TO MES
                DO MENSAJE WITH MES
                @ 08,27  prompt 'TABLAS                   '
                @ 09,27  prompt 'FORMULAR                 '
                @ 10,27  prompt 'REFORMULAR               '
                @ 11,27  prompt 'INCREMENTAR              '
                @ 12,27  prompt 'ASIGNAR                  '
                @ 13,27  prompt 'COMPROMETER              '
                @ 14,27  prompt 'CAUSAR                   '
                @ 15,27  prompt 'PAGAR                    '
                @ 16,27  prompt 'CONSULTA                 '
                @ 17,27  prompt 'INFORMES                 '
                @ 18,27  prompt 'OTROS                    '
                save scre to s2
                menu to WSUB00
                DO SECURE
                do case
                   case WSUB00 = 1
                        do PR0100
                   case WSUB00 = 2
                        do PR0200
                   case WSUB00 = 3
                        *do PR0300
                   case WSUB00 = 4
                        *do PR0400
                   case WSUB00 = 5
                        *do PR0500
                   case WSUB00 = 6
                        *do PR0600
                   case WSUB00 = 7
                        *do PR0700
                   case WSUB00 = 8
                        *do PR0800
                   case WSUB00 = 9
                        do PR0900
                   case WSUB00 = 10
                        do PR1000
                   case WSUB00 = 11
                        do PR1100
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
