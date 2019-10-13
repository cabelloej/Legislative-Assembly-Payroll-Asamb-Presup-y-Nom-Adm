SAVE SCRE TO SCRE4
store .t. to wsub4
do while wsub4
   RESTORE SCREEN FROM SCRE4
   @ 11,54 clear to 16,77
   @ 11,54 to 16,77
   @ 12,55 PROMPT "ORDINARIA             "
   @ 13,55 PROMPT "PRESUPUESTARIA        "
   @ 14,55 PROMPT "CONCEPTUAL            "
   @ 15,55 PROMPT "VERTICAL              "
   MENU TO WSUBSUB4
   DO CASE
   CASE WSUBSUB4 = 1
        DO AP0403A
   CASE WSUBSUB4 = 2
        DO AP0403B
   CASE WSUBSUB4 = 3
        DO AP0403C
   CASE WSUBSUB4 = 4
        DO AP0403D
   CASE WSUBSUB4 = 0
        EXIT
   ENDCASE
enddo
