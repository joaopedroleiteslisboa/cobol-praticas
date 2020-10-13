      ******************************************************************
      * Author: JOAO PEDRO LEITE S LISBOA
      * Date: 11/10/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTURA_MAIOR.
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT                    SECTION.

       FILE-CONTROL.

       SELECT ARQ-DB
              ASSIGN 'C:\cobol\DBREGISTROS.TXT'
              ORGANIZATION       IS LINE SEQUENTIAL.

      *
       SELECT ARQREL
              ASSIGN 'C:\cobol\ARQRELATORIOS.TXT'
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD  ARQ-DB
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQFUNCIONARIO.TXT".
       01  REG-PESSOAS.
           03 FD-COD-P            PIC 9(03).
           03 FD-NOME-P           PIC X(20).
           03 FD-ALTURA-P         PIC 9(4).

       FD  ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQRELATORIO.TXT".
       01  REG-RELATORIO          PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF        PIC X(01) VALUE ' '.
       77  WS-LINHABRACO PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO PIC X(80) VALUE ALL '-'.
       77  WS-TOTSAL-AUX PIC 9(09)V99 VALUE ZEROS.
       77  WS-PAG-AUX    PIC 9(03) VALUE ZEROS.
       77  CONT-LIN      PIC 9(03) VALUE ZEROS.

       01  WS-CABECALHO.
           03 WS-LINHA1.
              05 FILLER  PIC X(11) VALUE 'CURSO COBOL'.
              05 FILLER  PIC X(60) VALUE SPACES.
              05 FILLER  PIC X(04) VALUE 'PAG:'.
              05 WS-PAG  PIC zzz9.

           03 WS-LINHA2.
              05 FILLER  PIC X(28) VALUE SPACES.
              05 FILLER  PIC X(24) VALUE 'Listagem de Pessoas'.
              05 FILLER  PIC X(28) VALUE SPACES.

           03 WS-LINHA3.
              05 FILLER  PIC X(18) VALUE 'Codigo Pessoa'.
              05 FILLER  PIC X(02) VALUE SPACES.
              05 FILLER  PIC X(20) VALUE 'Nome Pessoa    '.
              05 FILLER  PIC X(10) VALUE SPACES.
              05 FILLER  PIC X(13) VALUE 'Altura Pessoa'.
              05 FILLER  PIC X(11) VALUE SPACES.
              05 FILLER  PIC X(19) VALUE 'Alta'.

      *
       01  WS-DETALHE.
           03 WS-CODFUN            PIC 9(03) VALUE ZEROS.
           03 FILLER               PIC X(08) VALUE SPACES.
           03 FILLER               PIC X(11) VALUE SPACES.
           03 WS-NOMEFUN           PIC X(20) VALUE ZEROS.
           03 FILLER               PIC X(10) VALUE SPACES.
           03 WS-ALTURA-P          PIC Z.99.
           03 FILLER               PIC X(19) VALUE SPACES.
           03 WS-PESSOA-ALTA       PIC X(01) VALUE 'N'.
       01  WS-RODAPE.
           03 FILLER               PIC X(30) VALUE 'TOTAL PESSOAS'.
           03 WS-TOTAL-P           PIC 9(2).


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           MAIN-PROCEDURE.
            OPEN INPUT ARQ-DB
            OPEN OUTPUT ARQREL

            MOVE 70 TO CONT-LIN

            PERFORM 1000-LER-ARQUIVO

            PERFORM UNTIL WS-EOF = 'S'
               IF CONT-LIN >= 60
                 PERFORM 2000-TRATA-CABECALHO
               END-IF

               PERFORM 3000-TRATA-DETALHE
               PERFORM 1000-LER-ARQUIVO
            END-PERFORM

            PERFORM 4000-TRATA-RODAPE

            CLOSE ARQ-DB ARQREL
            STOP RUN.
      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQ-DB
                  AT END
                     MOVE 'S' TO WS-EOF
                  NOT AT END

                     CONTINUE
           END-READ
           .
           EXIT.
      *----------------------------------------------------------------*
       2000-TRATA-CABECALHO.
      *----------------------------------------------------------------*

           ADD 1 TO WS-PAG-AUX
           MOVE WS-PAG-AUX TO WS-PAG

           DISPLAY WS-LINHA1
           WRITE REG-RELATORIO FROM WS-LINHA1
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-LINHA2
           WRITE REG-RELATORIO FROM WS-LINHA2
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-LINHA3
           WRITE REG-RELATORIO FROM WS-LINHA3
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           MOVE 6 TO CONT-LIN
           .
           EXIT.
      *----------------------------------------------------------------*
       3000-TRATA-DETALHE.
      *----------------------------------------------------------------*

           MOVE FD-COD-P TO WS-CODFUN
           MOVE FD-NOME-P TO WS-NOMEFUN
           MOVE FD-ALTURA-P TO WS-ALTURA-P


           IF FD-ALTURA-P >= 0200
               MOVE 'S' TO WS-PESSOA-ALTA
           END-IF


           DISPLAY WS-DETALHE
           WRITE REG-RELATORIO FROM WS-DETALHE
           ADD 1 TO CONT-LIN

           ADD 1 TO WS-TOTAL-P


           IF CONT-LIN = 57
              PERFORM 4000-TRATA-RODAPE
       .
           EXIT.
      *----------------------------------------------------------------*
       4000-TRATA-RODAPE.
      *----------------------------------------------------------------*

           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-PONTILHADO
           DISPLAY WS-RODAPE
           WRITE REG-RELATORIO FROM WS-RODAPE
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-PONTILHADO

           ADD 3 TO CONT-LIN


           .
           EXIT.

       END PROGRAM ALTURA_MAIOR.
