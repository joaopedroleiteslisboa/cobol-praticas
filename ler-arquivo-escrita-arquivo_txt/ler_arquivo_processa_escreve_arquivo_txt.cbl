      ******************************************************************
      * Author: João Pedro Leite
      * Date: 22/09/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-PROCESSADOR-PRECOS.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT DB_FILE
              ASSIGN 'C:\path\files\FD_PRODUTOS.txt'
              ORGANIZATION       IS LINE SEQUENTIAL
              FILE STATUS        IS WS_STATUS_DB_FILE.

           SELECT ARQ_LOG_FILE
              ASSIGN 'C:\path\files\FD_LOGS.txt'
              ORGANIZATION       IS LINE SEQUENTIAL
              FILE STATUS        IS WS_STATUS_ARQ_LOG_FILE.

       DATA                                     DIVISION.

       FILE                                     SECTION.

       FD  DB_FILE.

           01  FD_LAYOUT_DB_PRODUTOS.
               04 FD1_CODPROD                PIC 9(4).
               04 FD1_NOME_PRODUTO           PIC X(9).
               04 FD1_PRECO                  PIC 9(6)V99.
               04 FD1_QTD_PRODUTO            PIC ZZ9(3).

       FD  ARQ_LOG_FILE.


           01  FD_LAYOUT_ARQ_LOG_FILE.
               03 FD2_COD_PRODUTO            PIC 9(4).
               03 FD2_ACAO_EXECUCAO          PIC X(55).
               03 FD2_STATUS_EXECUCAO        PIC X(15).
               03 FD2_NOME_PRODUTO           PIC X(15).
               03 FD2_VALOR_TOTAL_PRODUTOS   PIC ZZ9.

       WORKING-STORAGE SECTION.

       01  WS_STATUS_ARQ_LOG_FILE          PIC X(20).
       01  WS_STATUS_DB_FILE               PIC X(20).
       01  WS_QTD-REG                      PIC 9(10).
       01  WS_PRECO_UNITARIO_PRODUTO       PIC 9(6)V99.
       01  WS_PRECO_TOTAL_PRODUTO          PIC 9(6)V99.
       01  WS_QTD_PRODUTO                  PIC 9(3).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           OPEN INPUT DB_FILE.
           IF WS_STATUS_DB_FILE NOT = "00" AND "02"
               DISPLAY 'ERRO AO ABRIR O ARQUIVO DE PRODUTOS: STATUS: '
               WS_STATUS_DB_FILE
               STOP RUN
           END-IF.

           OPEN OUTPUT ARQ_LOG_FILE.
           IF WS_STATUS_ARQ_LOG_FILE NOT = "00" AND "02"
               DISPLAY 'ERRO AO REGISTRAR LOGS: ARQUIVO INVALIDO:'
               WS_STATUS_ARQ_LOG_FILE
               STOP RUN
           END-IF.

           INITIALIZE WS_QTD-REG.

           PERFORM WITH TEST AFTER UNTIL WS_STATUS_DB_FILE = 46
               PERFORM PROCESSADOR-ITENS
           END-PERFORM.

           DISPLAY 'QUANTIDADE DE ITENS PROCESSADOS:'
           WS_QTD-REG.

           CLOSE DB_FILE ARQ_LOG_FILE

           STOP RUN.

           PROCESSADOR-ITENS.
               READ DB_FILE NEXT
               IF WS_STATUS_DB_FILE = '00' OR '02'

                  DISPLAY 'INICIANDO PERFORM DE PRECO TOTAL DE PRODUTOS'

                  MOVE 'PROCESSAMENTO SOMA TOTAL DE ITENS' TO
                                                       FD2_ACAO_EXECUCAO
                  MOVE 'STATUS OK' TO FD2_STATUS_EXECUCAO



                  MOVE FD1_CODPROD TO FD2_COD_PRODUTO

                  MOVE FD1_NOME_PRODUTO TO FD2_NOME_PRODUTO

                  MOVE FD1_PRECO TO WS_PRECO_UNITARIO_PRODUTO

                  MOVE FD1_QTD_PRODUTO TO WS_QTD_PRODUTO


                 COMPUTE WS_PRECO_TOTAL_PRODUTO = WS_QTD_PRODUTO *
                                              WS_PRECO_UNITARIO_PRODUTO;

                  MOVE WS_PRECO_TOTAL_PRODUTO TO
                                           FD2_VALOR_TOTAL_PRODUTOS


                  PERFORM PROCESSADOR-LOGS

                  INITIALIZE WS_STATUS_ARQ_LOG_FILE
                  ADD 1 TO WS_QTD-REG

               END-IF.

               PERFORM TERMINO-PROCESSO.


           PROCESSADOR-LOGS.
               WRITE FD_LAYOUT_ARQ_LOG_FILE.

           TERMINO-PROCESSO.
               EXIT.

       END PROGRAM 01-PROCESSADOR-PRECOS.
