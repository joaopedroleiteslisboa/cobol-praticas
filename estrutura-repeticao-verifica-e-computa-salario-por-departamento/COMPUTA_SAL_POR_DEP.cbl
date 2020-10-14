      ******************************************************************
      * Author: JOAO PEDRO LEITE S LISBOA
      * Date: 11/10/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUT_SALARIO_POR_DEP.
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
              ASSIGN 'C:\cobol\DBREGISTRODEP.TXT'
              ORGANIZATION       IS LINE SEQUENTIAL.

      *
       SELECT ARQREL
              ASSIGN 'C:\cobol\ARQRELATORIODEP.TXT'
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD  ARQ-DB
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "DBREGISTROSIDADE.TXT".
       01  REG-PESSOAS.
           03 FD-COD-P            PIC 9(09).
           03 FD-NOME-P           PIC X(30).
           03 FD-COD-DEPT         PIC 9(1).
           03 FD-SALARIO          PIC 9(9)V99.

       FD  ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQRELATORIOIDADE.TXT".
       01  REG-RELATORIO          PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF                     PIC X(01) VALUE ' '.
       77  WS-LINHABRACO              PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO              PIC X(80) VALUE ALL '-'.
       77  WS-TOTSAL-AUX              PIC 9(09)V99 VALUE ZEROS.
       77  WS-PAG-AUX                 PIC 9(03) VALUE ZEROS.
       77  CONT-LIN                   PIC 9(03) VALUE ZEROS.
       77  WS-AUX-SAL-DEP-1           PIC 9(9)V99.
       77  WS-AUX-SAL-DEP-2           PIC 9(9)V99.
       77  WS-AUX-SAL-DEP-3           PIC 9(9)V99.
       77  WS-AUX-SAL-DEP-4           PIC 9(9)V99.
       77  WS-AUX-TOTAL-GERAL-SAL-DEP PIC 9(9)V99.

       01  WS-VARIAVEIS.
           03 WS-DATA-HORA           PIC X(30).
           03 WS-TIMESTAMP.
               05 WS-DATA.
                   07 WS-ANO         PIC 9(04).
                   07 WS-MES         PIC 9(02).
                   07 WS-DIA         PIC 9(02).
               05 WS-HORA.
                   07 WS-HH          PIC 9(02).
                   07 WS-MM          PIC 9(02).
                   07 WS-SS          PIC 9(02).
                   07 WS-MS          PIC 9(02).

       01  WS-CABECALHO.
           03 WS-CAB-LINHA-1.
              05 FILLER  PIC X(14) VALUE 'CURSO COBOL - '.
              05 FILLER  PIC X(06) VALUE 'DATA: '.
              05 WS-DATA-SISTEMA   PIC X(10).
              05 FILLER  PIC X(07) VALUE ' HORA: '.
              05 WS-HORA-SISTEMA    PIC X(08).
              05 FILLER  PIC X(26) VALUE SPACES.
              05 FILLER  PIC X(04) VALUE 'PAG:'.
              05 WS-PAG  PIC zzz9.

           03 WS-CAB-LINHA-2.
              05 FILLER  PIC X(26) VALUE SPACES.
              05 FILLER  PIC X(23) VALUE 'RELACAO DE FUNCIONARIOS'.
              05 FILLER  PIC X(31) VALUE SPACES.

           03 WS-CAB-LINHA-3.
              05 FILLER  PIC X(9) VALUE 'Matricula'.
              05 FILLER  PIC X(14) VALUE SPACES.
              05 FILLER  PIC X(19) VALUE 'Nome do Funcionario'.
              05 FILLER  PIC X(20) VALUE SPACES.
              05 FILLER  PIC X(7)  VALUE 'Salario'.
              05 FILLER  PIC X(11) VALUE SPACES.

       01  WS-CAB-LINHA-4.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 WS-COD-P            PIC 9(03) VALUE ZEROS.
           05 FILLER              PIC X(08) VALUE SPACES.
           05 FILLER              PIC X(11) VALUE SPACES.
           05 WS-NOME-P           PIC X(20) VALUE ZEROS.
           05 FILLER              PIC X(16) VALUE SPACES.
           05 WS-SALFUN           PIC ZZZ.ZZ9,99.


       01  WS-RODAPE-TITULO.
           03 FILLER              PIC X(22)
                                        VALUE 'TOTAL POR DEPARTAMENTO'.
           03 FILLER              PIC X(6)  VALUE SPACES.

           03 FILLER              PIC X(3) VALUE 'QTD'.
           03 FILLER              PIC X(22)  VALUE SPACES.

           03 FILLER              PIC X(21)
                                          VALUE 'TOTAL SALARIO ALOCADO'.
           03 FILLER              PIC X(3)  VALUE SPACES.


       01  WS-RODAPE-DP-1.
           03 FILLER              PIC X(2)  VALUE SPACES.
           03 FILLER              PIC X(13) VALUE 'ALMOXARIFADO:'.
           03 FILLER              PIC X(13)  VALUE SPACES.
           03 WS-AUX-CONT-DEP-1   PIC 9(03) VALUE ZEROS.
           03 FILLER              PIC X(27)  VALUE SPACES.
           03 WS-TOTAL-SAL-DEP-1    PIC ZZZ.ZZ9,99.

       01  WS-RODAPE-DP-2.
           03 FILLER              PIC X(2)  VALUE SPACES.
           03 FILLER              PIC X(13) VALUE 'COMPRA/VENDA:'.
           03 FILLER              PIC X(13)  VALUE SPACES.
           03 WS-AUX-CONT-DEP-2   PIC 9(03) VALUE ZEROS.
           03 FILLER              PIC X(27)  VALUE SPACES.
           03 WS-TOTAL-SAL-DEP-2    PIC ZZZ.ZZ9,99.

       01  WS-RODAPE-DP-3.
           03 FILLER              PIC X(2)  VALUE SPACES.
           03 FILLER              PIC X(11) VALUE 'FINANCEIRO:'.
           03 FILLER              PIC X(15)  VALUE SPACES.
           03 WS-AUX-CONT-DEP-3   PIC 9(03) VALUE ZEROS.
           03 FILLER              PIC X(27)  VALUE SPACES.
           03 WS-TOTAL-SAL-DEP-3    PIC ZZZ.ZZ9,99.

       01  WS-RODAPE-DP-4.
           03 FILLER              PIC X(2)  VALUE SPACES.
           03 FILLER              PIC X(10) VALUE 'SEGURANCA:'.
           03 FILLER              PIC X(16)  VALUE SPACES.
           03 WS-AUX-CONT-DEP-4   PIC 9(03) VALUE ZEROS.
           03 FILLER              PIC X(27)  VALUE SPACES.
           03 WS-TOTAL-SAL-DEP-4    PIC ZZZ.ZZ9,99.

       01  WS-RODAPE-BALANCO-GERAL.
           03 FILLER                    PIC X(12) VALUE 'TOTAL GERAL:'.
           03 FILLER                    PIC X(16)  VALUE SPACES.
           03 WS-TOTAL-DPTS             PIC 9(03) VALUE ZEROS.
           03 FILLER                    PIC X(29)  VALUE SPACES.
           03 WS-TOTAL-GERAL-SAL-DEP    PIC ZZZ.ZZ9,99.


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

            PERFORM 4000-SUMARIZAR-GASTOS-DEP
            PERFORM 5000-TRATA-RODAPE

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

           DISPLAY WS-CABECALHO
           WRITE REG-RELATORIO FROM WS-CABECALHO

           DISPLAY WS-PONTILHADO
           DISPLAY WS-CAB-LINHA-2
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-CAB-LINHA-2

           DISPLAY WS-PONTILHADO
           DISPLAY WS-CAB-LINHA-3
           WRITE REG-RELATORIO FROM WS-CAB-LINHA-3

           MOVE 4 TO CONT-LIN

           .
           EXIT.
      *----------------------------------------------------------------*
       3000-TRATA-DETALHE.
      *----------------------------------------------------------------*

           MOVE FD-COD-P    TO WS-COD-P
           MOVE FD-NOME-P   TO WS-NOME-P
           MOVE FD-SALARIO  TO WS-SALFUN

           WRITE REG-RELATORIO FROM WS-CAB-LINHA-4
            ADD 1 TO CONT-LIN


           IF FD-COD-DEPT = 1
               ADD 1 TO WS-AUX-CONT-DEP-1
               ADD FD-SALARIO TO WS-AUX-SAL-DEP-1
           END-IF

           IF FD-COD-DEPT = 2
               ADD 1 TO WS-AUX-CONT-DEP-2
               ADD FD-SALARIO TO WS-AUX-SAL-DEP-2
           END-IF

           IF FD-COD-DEPT = 3
               ADD 1 TO WS-AUX-CONT-DEP-3
               ADD FD-SALARIO TO WS-AUX-SAL-DEP-3
           END-IF

           IF FD-COD-DEPT = 4
               ADD 1 TO WS-AUX-CONT-DEP-4
               ADD FD-SALARIO TO WS-AUX-SAL-DEP-4

           END-IF

           ADD 1 TO WS-TOTAL-DPTS
           ADD FD-SALARIO TO WS-AUX-TOTAL-GERAL-SAL-DEP

      *     IF CONT-LIN = 57
      *         PERFORM 5000-TRATA-RODAPE
           .
           EXIT.


      *----------------------------------------------------------------*
       4000-SUMARIZAR-GASTOS-DEP.
      *----------------------------------------------------------------*
           MOVE WS-AUX-SAL-DEP-1 TO WS-TOTAL-SAL-DEP-1
           MOVE WS-AUX-SAL-DEP-2 TO WS-TOTAL-SAL-DEP-2
           MOVE WS-AUX-SAL-DEP-3 TO WS-TOTAL-SAL-DEP-3
           MOVE WS-AUX-SAL-DEP-4 TO WS-TOTAL-SAL-DEP-4

           MOVE WS-AUX-TOTAL-GERAL-SAL-DEP TO WS-TOTAL-GERAL-SAL-DEP

           .
           EXIT.

      *----------------------------------------------------------------*
       5000-TRATA-RODAPE.
      *----------------------------------------------------------------*


           DISPLAY WS-PONTILHADO
           DISPLAY WS-RODAPE-TITULO

           WRITE REG-RELATORIO FROM WS-PONTILHADO

           WRITE REG-RELATORIO FROM WS-RODAPE-TITULO

           WRITE REG-RELATORIO FROM WS-RODAPE-DP-1

           WRITE REG-RELATORIO FROM WS-RODAPE-DP-2

           WRITE REG-RELATORIO FROM WS-RODAPE-DP-3

           WRITE REG-RELATORIO FROM WS-RODAPE-DP-4

           DISPLAY WS-PONTILHADO
           DISPLAY WS-RODAPE-BALANCO-GERAL

           WRITE REG-RELATORIO FROM WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-RODAPE-BALANCO-GERAL






           ADD 7 TO CONT-LIN

           .
           EXIT.

       END PROGRAM COMPUT_SALARIO_POR_DEP.
