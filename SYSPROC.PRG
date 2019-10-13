PROCEDURE RECLOC
SAVE SCRE TO SCRE1
STORE .T. TO WRECLOC
STORE .T. TO WFLAGMES
DO WHILE WRECLOC
   IF RLOCK()
      EXIT
   ELSE
      IF WFLAGMES
         @ 12,05 CLEAR TO 18,75
         @ 12,05 TO 18,75
         @ 14,15 SAY "REGISTRO OCUPADO POR OTRO USUARIO, REINTENTANDO ..."
         @ 16,15 SAY "          OPRIMA [ESC] PARA ABANDONAR              "
         STORE .F. TO WFLAGMES
      ENDIF
      WVALUE = INKEY()
      IF WVALUE = 27
         store .t. to wjumping
         close data
         close index
         RETURN TO MASTER
      ENDIF
   ENDIF
ENDDO
RESTORE SCRE FROM SCRE1
RETURN
************************
PROCEDURE FILLOC
SAVE SCRE TO SCRE1
STORE .T. TO WFILLOC
STORE .T. TO WFLAGMES
DO WHILE WFILLOC
   IF FLOCK()
      EXIT
   ELSE
      IF WFLAGMES
         @ 12,05 CLEAR TO 18,75
         @ 12,05 TO 18,75
         @ 14,15 SAY "ARCHIVO OCUPADO POR OTRO USUARIO, REINTENTANDO ..."
         @ 16,15 SAY "          OPRIMA [ESC] PARA ABANDONAR             "
         STORE .F. TO WFLAGMES
      ENDIF
      WVALUE = INKEY()
      IF WVALUE = 27
         store .t. to wjumping
         close data
         close index
         RETURN TO MASTER
      ENDIF
   ENDIF
ENDDO
RESTORE SCRE FROM SCRE1
RETURN
***********************
PROCEDURE PREGUNTA
STORE .T. TO WPREG
DO WHILE WPREG
   @ 23,0
   STORE SUBSTR(WCH,1,1) TO WCHOICE
   @ 23,40- (LEN(TEX)/2) SAY TEX GET WCHOICE
   READ
   STORE UPPER(WCHOICE) TO WCHOICE
   IF AT(WCHOICE,WCH) > 0
      STORE .F. TO WPREG
      EXIT
   ENDIF
ENDDO
@ 23,0
STORE WCHOICE TO WCH
RETURN
*************************
PROCEDURE MENSAJE
PARAMETERS MES
SET COLOR TO g/n
@ 23,1
@ 23,40-(LEN(MES)/2) SAY MES
RETURN
**************************************************
PROCEDURE AVISO
PARAMETERS MES
STORE " " TO X
SET COLOR TO g/n
?? CHR(7)
@ 23,1 SAY SPACE(78)
@ 23,40-(LEN(MES)/2) SAY MES GET X
READ
@ 23,1
RETURN
*****************
PROCEDURE CHKPRINT
PARAMETERS SALTAR
store .t. to wprinting
do while wprinting
   store "Prepare la impresora y oprima <ENTTER> para continuar o (R)echazar" to Qmes
   @ 23,1
   @ 23,40-(len(Qmes)/2) say Qmes
   store " " to wstat
   @ 23,78 get wstat
   read
   if upper(wstat) = "R"
      store 1 to saltar
      store .f. to wprinting
   else
      store 0 to saltar
      if sys(13) = "READY"
         store .f. to wprinting
      endif
   endif
   @ 23,1
enddo
RETURN
******************************************************************
procedure informa
store "I" to qw1
store "U" to qw2
store "G" to qw3
store "E" to qw4
store "T" to qw5
store "A" to qw6
store "O" to qw7
store "Z" to qw8
store "N" to qw9
store "A" to qw10
store " " to qw11
store "." to qw12
store "O" to qw13
store "D" to qw14
store "E" to qw15
store " " to qw16
store "L" to qw17
store "E" to qw18
store "D" to qw19
store " " to qw20
store "A" to qw21
store "V" to qw22
store "I" to qw23
store "T" to qw24
store "A" to qw25
store "L" to qw26
store "S" to qw27
store "I" to qw28
store "G" to qw29
store "E" to qw30
store "L" to qw31
store " " to qw32
store "A" to qw33
store "E" to qw34
store "L" to qw35
store "B" to qw36
store "M" to qw37
store "A" to qw38
store "S" to qw39
store "A" to qw40
STORE QW40+QW39+QW38+QW37+QW36+QW35+QW34+QW33+QW32+QW31+QW30+QW29+QW28 TO QQWW
STORE QQWW+QW27+QW26+QW25+QW24+QW23+QW22+QW21+QW20+QW19+QW18+QW17+QW16 TO QQWW
STORE QQWW+QW15+QW14+QW13+QW12+QW11+QW10+QW9+QW8+QW7+QW6+QW5+QW4+QW3+QW2+QW1 TO QQWW
RETURN
***********************

