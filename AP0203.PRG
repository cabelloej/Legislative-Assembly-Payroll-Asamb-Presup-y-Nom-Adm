if wapproce <> "A"
   return
endif
SAVE SCRE TO SCRE3
store .t. to wsub3
do while wsub3
   RESTORE SCREEN FROM SCRE3
   @ 11,54 clear to 15,77
   @ 11,54 to 15,77
   @ 12,55 PROMPT "INCLUIR CONCEPTO      "
   @ 13,55 PROMPT "MODIFICAR CONCEPTO    "
   @ 14,55 PROMPT "ELIMINAR CONCEPTO     "
   MENU TO WSUB0203
   DO CASE
   CASE WSUB0203 = 1
        DO AP0203A
   CASE WSUB0203 = 2
        DO AP0203B
   CASE WSUB0203 = 3
        DO AP0203C
   CASE WSUB0203 = 0
        EXIT
   ENDCASE
enddo
