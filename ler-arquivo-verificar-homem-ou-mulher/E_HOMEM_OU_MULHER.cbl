      ******************************************************************
      * Author: JOAO PEDRO LEITE S LISBOA
      * Date: 11/10/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEM_OU_MULHER.
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
              ASSIGN 'C:\cobol\DBREGISTROSHOMEMMULHE.TXT'
              ORGANIZATION       IS LINE SEQUENTIAL.

      *
       SELECT ARQREL
              ASSIGN 'C:\cobol\ARQRELATORIOSHOMENOUMULHER.TXT'
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD  ARQ-DB
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQRELATORIOSHOMENOUMULHER.TXT".
       01  REG-PESSOAS.
           03 FD-COD-P            PIC 9(03).
           03 FD-NOME-P           PIC X(20).
           03 FD-GENERO-P         PIC X(1).

       FD  ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQRELATORIOSHOMENOUMULHER.TXT".
       01  REG-RELATORIO          PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF          PIC X(01) VALUE ' '.
       77  WS-LINHABRACO   PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO   PIC X(80) VALUE ALL '-'.
       77  WS-TOTSAL-AUX   PIC 9(09)V99 VALUE ZEROS.
       77  WS-PAG-AUX      PIC 9(03) VALUE ZEROS.
       77  CONT-LIN        PIC 9(03) VALUE ZEROS.

       01  WS-VARIAVEIS.
           03 WS-DATA-HORA                PIC X(30).
           03 WS-TIMESTAMP.
               05 WS-DATA.
                   07 WS-ANO               PIC 9(04).
                   07 WS-MES               PIC 9(02).
                   07 WS-DIA               PIC 9(02).
               05 WS-HORA.
                   07 WS-HH                PIC 9(02).
                   07 WS-MM                PIC 9(02).
                   07 WS-SS                PIC 9(02).
                   07 WS-MS                PIC 9(02).

       01  WS-CABECALHO.
           03 WS-LINHA1.
              05 FILLER  PIC X(14) VALUE 'CURSO COBOL - '.
              05 FILLER  PIC X(06) VALUE 'DATA: '.
              05 WS-DATA-SISTEMA   PIC X(10).
              05 FILLER  PIC X(07) VALUE ' HORA: '.
              05 WS-HORA-SISTEMA    PIC X(08).
              05 FILLER  PIC X(26) VALUE SPACES.
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
              05 FILLER  PIC X(6) VALUE 'Genero'.
              05 FILLER  PIC X(11) VALUE SPACES.


      *
       01  WS-DETALHE.
           03 WS-COD-P             PIC 9(03) VALUE ZEROS.
           03 FILLER               PIC X(08) VALUE SPACES.
           03 FILLER               PIC X(11) VALUE SPACES.
           03 WS-NOME-P            PIC X(20) VALUE ZEROS.
           03 FILLER               PIC X(10) VALUE SPACES.
           03 WS-GENERO-P          PIC X(1).

       01  WS-RODAPE.
           03 FILLER               PIC X(14) VALUE 'TOTAL PESSOAS:'.
           03 FILLER               PIC X(1) VALUE SPACES.
           03 WS-TOTAL-P           PIC 9(2) VALUE ZEROS.
           03 FILLER               PIC X(3) VALUE SPACES.
           03 FILLER               PIC X(18) VALUE 'TOTAL DE MULHERES:'.
           03 FILLER               PIC X(1) VALUE SPACES.
           03 WS-QTD-MULHERES     PIC 9(2) VALUE ZEROS.
           03 FILLER               PIC X(3) VALUE SPACES.
           03 FILLER               PIC X(13) VALUE 'TOTAL HOMENS:'.
           03 FILLER               PIC X(1) VALUE SPACES.
           03 WS-QTD-HOMENS       PIC 9(2) VALUE ZEROS.


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           MAIN-PROCEDURE.

            OPEN INPUT ARQ-DB
            OPEN OUTPUT ARQREL

            MOVE 70 TO CONT-LIN

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP

           STRING WS-DIA '/' WS-MES '/' WS-ANO
               DELIMITED BY SIZE INTO WS-DATA-SISTEMA
           END-STRING

           STRING WS-HH ':' WS-MM ':' WS-SS
               DELIMITED BY SIZE INTO WS-HORA-SISTEMA
           END-STRING


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

           MOVE FD-COD-P TO WS-COD-P
           MOVE FD-NOME-P TO WS-NOME-P

           MOVE FD-GENERO-P TO WS-GENERO-P


           IF WS-GENERO-P = 'M'
               ADD 1 TO WS-QTD-MULHERES
               DISPLAY WS-NOME-P 'E UMA MULHER'

           END-IF

           IF WS-GENERO-P = 'H'
               ADD 1 TO WS-QTD-HOMENS
               DISPLAY WS-NOME-P 'E UM HOMEM'

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

       END PROGRAM HOMEM_OU_MULHER.
