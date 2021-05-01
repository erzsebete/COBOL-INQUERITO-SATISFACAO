      ******************************************************************
      * Author: Zabet
      * Date:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDUSTRIAPORCO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 TABLES.
           05 TB-NOME           PIC X(12) OCCURS 10 TIMES.
           05 TB-DEP            PIC 9     OCCURS 10 TIMES.
              88 VALIDAR-TB-DEB     VALUE 1 THRU 5.
           05 TB-NOTA           PIC 9     OCCURS 10 TIMES.
              88 VALIDAR-TB-NOTA    VALUE 1 THRU 5.

        77 OPCAOMENU            PIC 9.
           88 VALIDAR-OPCAOMENU     VALUE 1,2,3,4,5,9.

        77 INDICE               PIC 9(2).
        77 LINHA                PIC 9(2).
        77 LINHA2               PIC 9(2).
        77 LINHA3               PIC 9(2).
        77 LINHA4               PIC 9(2).
        77 LINHA5               PIC 9(2).
        77 LINHA6               PIC 9(2).
        77 LINHA7               PIC 9(2).
        77 LINHA8               PIC 9(2).

        77 NOTA-ALTA            PIC 9.
        77 NOTA-BAIXA           PIC 9.
        77 SOMANOTA             PIC 9(2).
        77 MEDIANOTA            PIC 9(2)V9.
        77 SAIDA-MEDIANOTA      PIC Z.Z.
        77 DEPARTAMENTO         PIC 9.
           88 VALIDAR-DEPARTAMENTO  VALUE 1 THRU 5.

       SCREEN SECTION.
       01  CLS BLANK SCREEN.
       01  MENU.
         05 COL 1 VALUE "--------------------------------------" LINE 1
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------" LINE 1
           FOREGROUND-COLOR 1.

         05 COL 1 VALUE "Ind£stria do Porco, S.A."               LINE 2
            FOREGROUND-COLOR 3 HIGHLIGHT.
         05 COL 1 VALUE
                  "InquÇrito de SatisfaáÑo serviáos de contabilidade)"
                                                                 LINE 3
           FOREGROUND-COLOR 3 HIGHLIGHT.
         05 COL 1 VALUE "--------------------------------------" LINE 4
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------" LINE 4
           FOREGROUND-COLOR 1.
         05 COL 1 VALUE "                             MENU"      LINE 5
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 COL 1 VALUE "--------------------------------------" LINE 6
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------" LINE 6
           FOREGROUND-COLOR 1.
         05 COL 1 VALUE "1| Recolha de dados."                   LINE 7.
         05 COL 1 VALUE "2| Listagem de funcion†rios por departamento"
                                                                 LINE 8.
         05 COL 1 VALUE "3| Listagem de funcion†rios que atribu°ram ao"
                                                                 LINE 9.
         05 COL 47 VALUE "serviáo a nota mais alta."             LINE 9.

         05 COL 1 VALUE "4| Listagem de funcion†rios que atribu°ram ao"
                                                                LINE 10.
         05 COL 47 VALUE "serviáo a nota mais baixa."           LINE 10.
         05 COL 1 VALUE "5| MÇdia global de satisfaáÑo obtida"  LINE 11.
         05 COL 1 VALUE "9| Terminar o programa"                LINE 12.
         05 COL 1 VALUE "--------------------------------------"LINE 13
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------"LINE 13
           FOREGROUND-COLOR 1.
         05 COL 1 VALUE "[ ] DIGITE A SUA OPCAO"                LINE 14.


       01  CABECALHO.
         05 COL 1 VALUE "--------------------------------------" LINE 1
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------" LINE 1
           FOREGROUND-COLOR 1.

         05 COL 1 VALUE "Ind£stria do Porco, S.A."               LINE 2
           FOREGROUND-COLOR 3 HIGHLIGHT.
         05 COL 1 VALUE
                  "InquÇrito de SatisfaáÑo serviáos de contabilidade)"
                                                                 LINE 3
           FOREGROUND-COLOR 3 HIGHLIGHT.
         05 COL 1 VALUE "--------------------------------------" LINE 4
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------" LINE 4
           FOREGROUND-COLOR 1.
         05 COL 1  VALUE "FUNCIONARIO                     DEPARTAMENTO"
                                                                 LINE 5
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 COL 60 VALUE "  SATISFAÄéO"                          LINE 5
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 COL 1 VALUE "--------------------------------------" LINE 6
           FOREGROUND-COLOR 1.
         05 COL 35 VALUE"--------------------------------------" LINE 6
           FOREGROUND-COLOR 1.

       PROCEDURE DIVISION.
       INICIO.

           PERFORM UNTIL (OPCAOMENU = 9)
             DISPLAY CLS
             DISPLAY MENU
             PERFORM WITH TEST AFTER UNTIL (VALIDAR-OPCAOMENU)
               ACCEPT OPCAOMENU AT 1402
               IF (NOT VALIDAR-OPCAOMENU) THEN
                   DISPLAY "INSIRA: 1,2,3,4,5 ou 9" AT 1501
                   FOREGROUND-COLOR 4 HIGHLIGHT
               ELSE
                   DISPLAY " " ERASE EOL AT 1501
               END-IF
             END-PERFORM
               EVALUATE OPCAOMENU
                   WHEN 1 PERFORM RECOLHA-DADOS
                   WHEN 2 PERFORM LISTA-DEP
                   WHEN 3 PERFORM LISTA-NOTA-ALTA
                   WHEN 4 PERFORM LISTA-NOTA-BAIXA
                   WHEN 5 PERFORM MEDIA
                   WHEN 9 STOP RUN
               END-EVALUATE
           END-PERFORM.


      *________________________________________________________________________*
       RECOLHA-DADOS.
           DISPLAY CLS
           DISPLAY CABECALHO.
           MOVE 7 TO LINHA.
           MOVE 8 TO LINHA2.
           MOVE 10 TO LINHA3.
           MOVE 11 TO LINHA4.
           MOVE 12 TO LINHA5.
           MOVE 13 TO LINHA6.
           MOVE 14 TO LINHA7.
           MOVE 15 TO LINHA8.

           PERFORM RODAPE-LINHA.
           PERFORM RODAPE-INFO.

           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 10
                   ACCEPT TB-NOME (INDICE)            LINE LINHA  COL 1

            PERFORM WITH TEST AFTER UNTIL (VALIDAR-TB-DEB (INDICE) )
               ACCEPT TB-DEP  (INDICE)  LINE LINHA  COL 34
               IF (NOT VALIDAR-TB-DEB (INDICE)) THEN
                   DISPLAY "INSIRA: 1,2,3,4 ou 5" LINE LINHA  COL 36
                   FOREGROUND-COLOR 4 HIGHLIGHT
               ELSE
                   DISPLAY " " ERASE EOL AT LINE LINHA  COL 36
               END-IF
             END-PERFORM



            PERFORM WITH TEST AFTER UNTIL (VALIDAR-TB-NOTA (INDICE) )
               ACCEPT TB-NOTA  (INDICE)  LINE LINHA  COL 69
               IF (NOT VALIDAR-TB-NOTA (INDICE)) THEN
                   DISPLAY "INSIRA: 1,2,3,4 ou 5" LINE LINHA  COL 71
                   FOREGROUND-COLOR 4 HIGHLIGHT
               ELSE
                   DISPLAY " " ERASE EOL AT LINE LINHA  COL 71
               END-IF
             END-PERFORM

                   ADD TB-NOTA (INDICE) TO SOMANOTA
                   PERFORM LIMPA-CAMPOS

                   ADD 1 TO LINHA
                   ADD 1 TO LINHA2
                   ADD 1 TO LINHA3
                   ADD 1 TO LINHA4
                   ADD 1 TO LINHA5
                   ADD 1 TO LINHA6
                   ADD 1 TO LINHA7
                   ADD 1 TO LINHA8

                   PERFORM RODAPE-LINHA
                   PERFORM RODAPE-INFO

           END-PERFORM.
      *________________________________________________________________________*

       LISTA-DEP.
           DISPLAY CLS
           MOVE 8 TO LINHA2.
           MOVE 9 TO LINHA3.
           MOVE 10 TO LINHA4.
           MOVE 11 TO LINHA5.
           MOVE 12 TO LINHA6.
           MOVE 13 TO LINHA7.
           MOVE 14 TO LINHA8.
           PERFORM RODAPE-LINHA.
           PERFORM RODAPE-INFO.
           DISPLAY CABECALHO.
           DISPLAY "Indique o Departamento: [ ]" AT 0701.

           PERFORM WITH TEST AFTER UNTIL (VALIDAR-DEPARTAMENTO )
               ACCEPT DEPARTAMENTO AT 0726
               IF (NOT VALIDAR-DEPARTAMENTO) THEN
                   DISPLAY "INSIRA: 1,2,3,4 ou 5" AT 0728
                   FOREGROUND-COLOR 4 HIGHLIGHT
               ELSE
                   DISPLAY " " ERASE EOL AT 0728
               END-IF
             END-PERFORM


           DISPLAY CLS

           DISPLAY CABECALHO.
           MOVE 7 TO LINHA.


           PERFORM VARYING INDICE FROM 1 BY 1  UNTIL INDICE > 10

           IF TB-DEP (INDICE) = DEPARTAMENTO  THEN

               DISPLAY TB-NOME (INDICE)    AT LINE LINHA COL 1
               DISPLAY TB-DEP  (INDICE)    AT LINE LINHA COL 34
               DISPLAY TB-NOTA (INDICE)    AT LINE LINHA COL 70

               PERFORM LIMPA-CAMPOS
                   ADD 1 TO LINHA
                   ADD 1 TO LINHA2
                   ADD 1 TO LINHA3
                   ADD 1 TO LINHA4
                   ADD 1 TO LINHA5
                   ADD 1 TO LINHA6
                   ADD 1 TO LINHA7
                   ADD 1 TO LINHA8


                   PERFORM RODAPE-LINHA
                   PERFORM RODAPE-INFO

           END-PERFORM.
           ADD 2 TO LINHA8.
           DISPLAY "Para voltar ao MENU pressione ENTER "
                                                     LINE LINHA8 COL 01.
           ACCEPT OMITTED LINE LINHA8 COL 36.
      *________________________________________________________________________*
       LISTA-NOTA-ALTA.
           DISPLAY CLS.
           DISPLAY CABECALHO.
           MOVE 8 TO LINHA.
           MOVE TB-NOTA(1) TO NOTA-ALTA.

           PERFORM VARYING INDICE FROM 1 BY 1  UNTIL INDICE > 10

           IF TB-NOTA (INDICE) > NOTA-ALTA THEN
               MOVE TB-NOTA (INDICE) TO NOTA-ALTA
           END-IF

           END-PERFORM.

           PERFORM VARYING INDICE FROM 1 BY 1  UNTIL INDICE > 10

           IF TB-NOTA (INDICE) = NOTA-ALTA THEN
               DISPLAY TB-NOME (INDICE)    AT LINE LINHA COL 1
               DISPLAY TB-DEP  (INDICE)    AT LINE LINHA COL 34
               DISPLAY TB-NOTA (INDICE)    AT LINE LINHA COL 70
               ADD 1 TO LINHA
           END-IF
           END-PERFORM.
           ADD 1 TO LINHA.
           MOVE LINHA TO LINHA2.
           PERFORM RODAPE-LINHA.
           ADD 1 TO LINHA.
           DISPLAY "LISTA DE FUNCIONÜRIOS QUE DERAM A NOTA MAIS ALTA"
                                                   AT LINE LINHA COL 01
           FOREGROUND-COLOR 6 HIGHLIGHT.
           ADD 2 TO LINHA.
           DISPLAY "Para voltar ao MENU pressione ENTER "
                                                     LINE LINHA COL 01.
           ACCEPT OMITTED AT LINE LINHA COL 36.

      *________________________________________________________________________*
       LISTA-NOTA-BAIXA.

           DISPLAY CLS.
           DISPLAY CABECALHO.

           MOVE 8 TO LINHA.
           MOVE TB-NOTA(1) TO NOTA-BAIXA.

           PERFORM VARYING INDICE FROM 1 BY 1  UNTIL INDICE > 10

           IF TB-NOTA (INDICE) < NOTA-BAIXA THEN
               MOVE TB-NOTA (INDICE) TO NOTA-BAIXA
           END-IF

           END-PERFORM.

           PERFORM VARYING INDICE FROM 1 BY 1  UNTIL INDICE > 10

           IF TB-NOTA (INDICE) = NOTA-BAIXA THEN
               DISPLAY TB-NOME (INDICE)   AT LINE LINHA COL 1
               DISPLAY TB-DEP  (INDICE)   AT LINE LINHA COL 34
               DISPLAY TB-NOTA (INDICE)   AT LINE LINHA COL 70
               ADD 1 TO LINHA
           END-IF
           END-PERFORM.
           ADD 1 TO LINHA.
           MOVE LINHA TO LINHA2.
           PERFORM RODAPE-LINHA.
           ADD 1 TO LINHA.
           DISPLAY "LISTA DE FUNCIONÜRIOS QUE DERAM A NOTA MAIS BAIXA"
                                                   AT LINE LINHA COL 01
           FOREGROUND-COLOR 6 HIGHLIGHT.
           ADD 2 TO LINHA.
           DISPLAY "Para voltar ao MENU pressione ENTER "
                                                     LINE LINHA COL 01.
           ACCEPT OMITTED AT LINE LINHA COL 36.

      *________________________________________________________________________*
       MEDIA.

           DISPLAY CLS
           DISPLAY CABECALHO.
           MOVE 7 TO LINHA.


           PERFORM VARYING INDICE FROM 1 BY 1  UNTIL INDICE > 10

               DISPLAY TB-NOME (INDICE)   AT LINE LINHA COL 1
               DISPLAY TB-DEP  (INDICE)   AT LINE LINHA COL 38
               DISPLAY TB-NOTA (INDICE)   AT LINE LINHA COL 69
               ADD 1 TO LINHA
           END-PERFORM.
           MOVE LINHA TO LINHA2.
           PERFORM RODAPE-LINHA.
           ADD 1 TO LINHA.
           COMPUTE MEDIANOTA = (SOMANOTA / 5).
           MOVE MEDIANOTA TO SAIDA-MEDIANOTA.

           DISPLAY "MêDIA GLOBAL DA SATISFAÄéO OBTIDA"
                                                   AT LINE LINHA COL 01
           FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "MEDIA: " AT LINE LINHA COL 62
           FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY SAIDA-MEDIANOTA AT LINE LINHA COL 69.
           ADD 2 TO LINHA.
           DISPLAY "Para voltar ao MENU pressione ENTER "
                                                     LINE LINHA COL 01.
           ACCEPT OMITTED LINE LINHA COL 36.


      *_______________________________AUX______________________________________*


       RODAPE-LINHA.

           DISPLAY "--------------------------------------"
                                               AT LINE LINHA2 COL 1
           FOREGROUND-COLOR 1.
           DISPLAY "--------------------------------------"
                                               AT LINE LINHA2 COL 35
           FOREGROUND-COLOR 1.

       RODAPE-INFO.

           DISPLAY "NOME DO FUNCIONARIO"   AT LINE LINHA3 COL 1
           FOREGROUND-COLOR 1 HIGHLIGHT
           DISPLAY "DEPARTAMENTO [1-5]"    AT LINE LINHA3 COL 32
           FOREGROUND-COLOR 1 HIGHLIGHT
           DISPLAY "SATISFAÄéO [1-5]"      AT LINE LINHA3 COL 57
           FOREGROUND-COLOR 1 HIGHLIGHT

           DISPLAY "1 - RECURSOS HUMANOS        1- NADA"
                                           AT LINE LINHA4 COL 32.
           DISPLAY "2 - MARKETING               2- POUCO"
                                           AT LINE LINHA5 COL 32.
           DISPLAY "3 - COMERCIAL               3- SATISFEITO"
                                           AT LINE LINHA6 COL 32.
           DISPLAY "4 - PRODUÄéO                4- MUITO"
                                           AT LINE LINHA7 COL 32.
           DISPLAY "5 - LOGãSTICA               5- PERFEITO"
                                           AT LINE LINHA8 COL 32.

       LIMPA-CAMPOS.

           DISPLAY " " ERASE EOL LINE LINHA2 COL 1.
           DISPLAY " " ERASE EOL LINE LINHA3 COL 1.
           DISPLAY " " ERASE EOL LINE LINHA4 COL 1.
           DISPLAY " " ERASE EOL LINE LINHA5 COL 1.
           DISPLAY " " ERASE EOL LINE LINHA6 COL 1.
           DISPLAY " " ERASE EOL LINE LINHA7 COL 1.
           DISPLAY " " ERASE EOL LINE LINHA8 COL 1.


           END PROGRAM INDUSTRIAPORCO.
