      ******************************************************************
      * Author: JOAO PEDRO LEITE S LISBOA
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATORIO_FUNCIONARIO.
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

       SELECT ARQFUN
              ASSIGN 'C:\aula_cobol_thulio_cobol\ARQFUNCIONARIO.txt'
              ORGANIZATION       IS LINE SEQUENTIAL.

      *
       SELECT ARQREL
              ASSIGN 'C:\aula_cobol_thulio_cobol\ARQRELATORIO.txt'
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD  ARQFUN
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQFUNCIONARIO.TXT".
       01  REG-FUNCIONARIO.
           03 FD-COD-F            PIC 9(03).
           03 FD-NOME-F           PIC X(20).
           03 FD-SALFUN-F         PIC 9(06)V99.

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
              05 FILLER  PIC X(24) VALUE 'Listagem de Funcionarios'.
              05 FILLER  PIC X(28) VALUE SPACES.

           03 WS-LINHA3.
              05 FILLER  PIC X(18) VALUE 'Codigo Funcionario'.
              05 FILLER  PIC X(02) VALUE SPACES.
              05 FILLER  PIC X(20) VALUE 'Nome Funcionario    '.
              05 FILLER  PIC X(10) VALUE SPACES.
              05 FILLER  PIC X(19) VALUE 'Salario Funcionario'.
              05 FILLER  PIC X(11) VALUE SPACES.
      *
       01  WS-DETALHE.
           03 WS-CODFUN  PIC 9(03) VALUE ZEROS.
           03 FILLER     PIC X(07) VALUE SPACES.
           03 FILLER     PIC X(10) VALUE SPACES.
           03 WS-NOMEFUN PIC X(20) VALUE ZEROS.
           03 FILLER     PIC X(10) VALUE SPACES.
           03 FILLER     PIC X(02) VALUE 'R$'.
           03 WS-SALFUN  PIC zzz.zz9,99.
      *
       01  WS-RODAPE.
           03 FILLER    PIC X(30) VALUE 'TOTAL SALARIO ACUMULADO: R$'.
           03 WS-TOTSAL PIC ZZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           MAIN-PROCEDURE.
            OPEN INPUT ARQFUN
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

            CLOSE ARQFUN ARQREL
            STOP RUN.
      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQFUN
                  AT END
                     MOVE 'S' TO WS-EOF
                     DISPLAY '----- UMA VEZ SÓ OU MAIS'
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
           MOVE FD-COD-F TO WS-CODFUN
           MOVE FD-NOME-F TO WS-NOMEFUN
           MOVE FD-SALFUN-F TO WS-SALFUN
           DISPLAY WS-DETALHE
           WRITE REG-RELATORIO FROM WS-DETALHE
           ADD 1 TO CONT-LIN

           ADD FD-SALFUN-F TO WS-TOTSAL-AUX

           IF CONT-LIN = 57
              PERFORM 4000-TRATA-RODAPE
       .
           EXIT.
      *----------------------------------------------------------------*
       4000-TRATA-RODAPE.
      *----------------------------------------------------------------*
           MOVE WS-TOTSAL-AUX TO WS-TOTSAL
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-PONTILHADO
           DISPLAY WS-RODAPE
           WRITE REG-RELATORIO FROM WS-RODAPE
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-PONTILHADO

           ADD 3 TO CONT-LIN

           INITIALIZE WS-TOTSAL-AUX WS-TOTSAL
           .
           EXIT.

       END PROGRAM RELATORIO_FUNCIONARIO.
