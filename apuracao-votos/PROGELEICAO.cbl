      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGELEICAO.
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

       SELECT ARQVOTOS ASSIGN  TO DISK
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
       SELECT ARQREL ASSIGN  TO DISK
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD  ARQVOTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQELEITORES.TXT".
       01  REG-ELEITORES.
           03 FD-TITULO           PIC 9(03).
           03 FD-NOME-ELEITOR     PIC X(10).
           03 FD-VOTO             PIC 9(03).

       FD  ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQELEICAO.TXT".
       01  REG-RELATORIO          PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF        PIC X(01) VALUE ' '.
       77  WS-LINHABRACO PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO PIC X(80) VALUE ALL '-'.
       77  WS-PAG-AUX    PIC 9(03) VALUE ZEROS.
       77  CONT-LIN      PIC 9(03) VALUE ZEROS.
       77  WS-PRCT-AUX   PIC 9(03)V9(31) VALUE ZEROS.

       01  WS-VARIAVEIS.
           03 WS-DATA-HORA       PIC X(30).
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
              05 FILLER  PIC X(26) VALUE 'Listagem de Votos Apurados'.
              05 FILLER  PIC X(26) VALUE SPACES.

           03 WS-LINHA2-1.
              05 FILLER  PIC X(14) VALUE 'Titulo Eleitor'.
              05 FILLER  PIC X(15) VALUE SPACES.
              05 FILLER  PIC X(12) VALUE 'Nome Eleitor'.
              05 FILLER  PIC X(15) VALUE SPACES.
              05 FILLER  PIC X(04) VALUE 'Voto'.

           03 WS-LINHA2-2.
              05 WS-TITULO-E  PIC 9(03) VALUE ZEROS.
              05 FILLER       PIC X(26) VALUE SPACES.
              05 WS-NOME-E    PIC X(10) VALUE SPACES.
              05 FILLER       PIC X(17) VALUE SPACES.
              05 WS-VOTO-E    PIC 9(03) VALUE ZEROS.

           03 WS-LINHA3.
              05 FILLER  PIC X(07) VALUE SPACES.
              05 FILLER  PIC X(14) VALUE 'Numero Partido'.
              05 FILLER  PIC X(02) VALUE SPACES.
              05 FILLER  PIC X(17) VALUE 'Nome do Candidato'.
              05 FILLER  PIC X(04) VALUE SPACES.
              05 FILLER  PIC X(14) VALUE 'Total de Votos'.
              05 FILLER  PIC X(08) VALUE SPACES.
              05 FILLER  PIC X(13) VALUE 'Prct Votos(%)'.
      *
       01  WS-DETALHE1.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 001.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'THULIO'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-1     PIC 9(03) VALUE ZEROS.
           03 FILLER       PIC X(19) VALUE SPACES.
           03 WS-PRCT-1    PIC ZZ9,99 VALUE ZEROS.
           03 FILLER       PIC X(01) VALUE '%'.

       01  WS-DETALHE2.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 002.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'JOAOZINHO'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-2     PIC 9(03) VALUE ZEROS.
           03 FILLER       PIC X(19) VALUE SPACES.
           03 WS-PRCT-2    PIC ZZ9,99 VALUE ZEROS.
           03 FILLER       PIC X(01) VALUE '%'.

       01  WS-DETALHE3.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 003.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'ANA'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-3     PIC 9(03) VALUE ZEROS.
           03 FILLER       PIC X(19) VALUE SPACES.
           03 WS-PRCT-3    PIC ZZ9,99 VALUE ZEROS.
           03 FILLER       PIC X(01) VALUE '%'.

       01  WS-DETALHE4.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 004.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'JOANA'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-4     PIC 9(03) VALUE ZEROS.
           03 FILLER       PIC X(19) VALUE SPACES.
           03 WS-PRCT-4    PIC ZZ9,99.
           03 FILLER       PIC X(01) VALUE '%'.

       01  WS-DETALHE5.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 005.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'CARLOS'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-5     PIC 9(03) VALUE ZEROS.
           03 FILLER       PIC X(19) VALUE SPACES.
           03 WS-PRCT-5    PIC ZZ9,99.
           03 FILLER       PIC X(01) VALUE '%'.
      *
       01  WS-RODAPE.
           03 FILLER         PIC X(20) VALUE 'TOTAL DE ELEITORES: '.
           03 WS-TOTELEITOR  PIC 9(04) VALUE ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           MAIN-PROCEDURE.
           OPEN INPUT ARQVOTOS
           OPEN OUTPUT ARQREL
           MOVE 999 TO CONT-LIN

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP

           STRING WS-DIA '/' WS-MES '/' WS-ANO
               DELIMITED BY SIZE INTO WS-DATA-SISTEMA
           END-STRING

           STRING WS-HH ':' WS-MM ':' WS-SS
               DELIMITED BY SIZE INTO WS-HORA-SISTEMA
           END-STRING

           PERFORM 1000-LER-ARQUIVO

           PERFORM UNTIL WS-EOF = 'S'

               IF CONT-LIN >= 21
                  PERFORM 2000-TRATA-CABECALHO
               END-IF

               PERFORM 2100-CONTA-VOTO
               PERFORM 2200-GRAVA-ELEITOR
               PERFORM 1000-LER-ARQUIVO
           END-PERFORM

           IF CONT-LIN > 9
               ADD 1 TO WS-PAG-AUX
               MOVE WS-PAG-AUX TO WS-PAG

               DISPLAY WS-LINHA1
               WRITE REG-RELATORIO FROM WS-LINHA1
               DISPLAY WS-PONTILHADO
               WRITE REG-RELATORIO FROM WS-PONTILHADO
               DISPLAY WS-LINHA2
               WRITE REG-RELATORIO FROM WS-LINHA2
           END-IF

           PERFORM 3000-TRATA-CANDIDATO
           PERFORM 4000-TRATA-RODAPE

           CLOSE ARQVOTOS ARQREL
           STOP RUN.
      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQVOTOS
                  AT END
                     MOVE 'S' TO WS-EOF
                  NOT AT END
                     ADD 1 TO WS-TOTELEITOR
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
           DISPLAY WS-LINHA2-1
           WRITE REG-RELATORIO FROM WS-LINHA2-1
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           MOVE 6 TO CONT-LIN
           .
           EXIT.
      *----------------------------------------------------------------*
       2100-CONTA-VOTO.
      *----------------------------------------------------------------*
           EVALUATE FD-VOTO
               WHEN 001
                   ADD 1 TO WS-CNT-1
               WHEN 002
                   ADD 1 TO WS-CNT-2
               WHEN 003
                   ADD 1 TO WS-CNT-3
               WHEN 004
                   ADD 1 TO WS-CNT-4
               WHEN 005
                   ADD 1 TO WS-CNT-5
               WHEN 000
                   DISPLAY 'VOTO EM BRANCO'
               WHEN OTHER
                   DISPLAY 'VOTO NULO'
           END-EVALUATE

           .
           EXIT.
      *----------------------------------------------------------------*
       2200-GRAVA-ELEITOR.
      *----------------------------------------------------------------*     .
           MOVE FD-TITULO       TO WS-TITULO-E
           MOVE FD-NOME-ELEITOR TO WS-NOME-E
           MOVE FD-VOTO         TO WS-VOTO-E

           DISPLAY WS-LINHA2-2
           WRITE REG-RELATORIO FROM WS-LINHA2-2

           ADD 1 TO CONT-LIN

           EXIT.
      *----------------------------------------------------------------*
       3000-TRATA-CANDIDATO.
      *----------------------------------------------------------------*
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-LINHA3
           WRITE REG-RELATORIO FROM WS-LINHA3
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           PERFORM 3100-CALC-PORCENTAGEM

           DISPLAY WS-DETALHE1
           WRITE REG-RELATORIO FROM WS-DETALHE1
           DISPLAY WS-DETALHE2
           WRITE REG-RELATORIO FROM WS-DETALHE2
           DISPLAY WS-DETALHE3
           WRITE REG-RELATORIO FROM WS-DETALHE3
           DISPLAY WS-DETALHE4
           WRITE REG-RELATORIO FROM WS-DETALHE4
           DISPLAY WS-DETALHE5
           WRITE REG-RELATORIO FROM WS-DETALHE5

           ADD 9 TO CONT-LIN
           .
           EXIT.
     .*----------------------------------------------------------------*
       3100-CALC-PORCENTAGEM.
      *----------------------------------------------------------------*
           COMPUTE WS-PRCT-AUX ROUNDED = (WS-CNT-1*100)/WS-TOTELEITOR
           MOVE WS-PRCT-AUX TO WS-PRCT-1

           COMPUTE WS-PRCT-AUX ROUNDED = (WS-CNT-2*100)/WS-TOTELEITOR
           MOVE WS-PRCT-AUX TO WS-PRCT-2

           COMPUTE WS-PRCT-AUX ROUNDED = (WS-CNT-3*100)/WS-TOTELEITOR
           MOVE WS-PRCT-AUX TO WS-PRCT-3

           COMPUTE WS-PRCT-AUX ROUNDED = (WS-CNT-4*100)/WS-TOTELEITOR
           MOVE WS-PRCT-AUX TO WS-PRCT-4

           COMPUTE WS-PRCT-AUX ROUNDED = (WS-CNT-5*100)/WS-TOTELEITOR
           MOVE WS-PRCT-AUX TO WS-PRCT-5

           EXIT.
      *----------------------------------------------------------------*
       4000-TRATA-RODAPE.
      *----------------------------------------------------------------*

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-RODAPE
           WRITE REG-RELATORIO FROM WS-RODAPE
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           ADD 3 TO CONT-LIN

           .
           EXIT.

       END PROGRAM PROGELEICAO.
