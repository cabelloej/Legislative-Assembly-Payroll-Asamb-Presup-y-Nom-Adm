SAVE SCRE TO SCRE4
store .t. to wsub4
do while wsub4
   RESTORE SCREEN FROM SCRE4
   @ 10,54 clear to 13,77
   @ 10,54 to 13,77
   @ 11,55 PROMPT "SOBRES DE PAGO        "
   @ 12,55 PROMPT "CONCEPTO              "
   MENU TO WSUBSUB4
   DO CASE
   CASE WSUBSUB4 = 1
        DO AP0402A
   CASE WSUBSUB4 = 2
        DO AP0402B
   CASE WSUBSUB4 = 0
        EXIT
   ENDCASE
enddo
