save scre to scre4
store .t. to wsub4
do while wsub4
           restore screen from scre4
           @ 15,54 clear to 20,77
           @ 15,54 to 20,77
           @ 16,55 PROMPT "CENTROS DE PAGO       "
           @ 17,55 PROMPT "CONTRATOS             "
           @ 18,55 PROMPT "SINDICATOS            "
           @ 19,55 PROMPT "BANCOS                "
           MENU TO WSUBSUB
           IF WSUBSUB = 1
              STORE "CENTROS DE PAGO" TO WHEADER
              SELECT 1
              USE APCENTRO INDEX APCENTRO
              DO APACTA
              UNLOCK ALL
              CLOSE DATA
              CLOSE INDEX
           ELSE
              IF WSUBSUB = 2
                 STORE "CONTRATOS" TO WHEADER
                 SELECT 1
                 USE APCONTRA INDEX APCONTRA
                 DO APACTA
                 UNLOCK ALL
                 CLOSE DATA
                 CLOSE INDEX
              ELSE
                 IF WSUBSUB = 3
                    STORE "SINDICATOS" TO WHEADER
                    SELECT 1
                    USE APSINDIC INDEX APSINDIC
                    DO APACTA
                    UNLOCK ALL
                    CLOSE DATA
                    CLOSE INDEX
                 ELSE
                    IF WSUBSUB = 4
                       STORE "BANCOS" TO WHEADER
                       SELECT 1
                       USE APBANCOS INDEX APBANCOS
                       DO APACTA
                       UNLOCK ALL
                       CLOSE DATA
                       CLOSE INDEX
                    ELSE
                       EXIT
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
enddo
