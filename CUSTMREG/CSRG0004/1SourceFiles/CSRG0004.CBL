      *================================================================*
      *        I D E N T I F I C A T I O N      D I V I S I O N        *
      *================================================================*
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     CSRG0004.
       AUTHOR.         RENAN MUNIZ MERLO.
       DATE-WRITTEN.   10/04/2020.
       DATE-COMPILED   WHEN-COMPILED.
       REMARKS.
      *----------------------------------------------------------------*
      *                RENAN MUNIZ MERLO COBOL DEVELOPER               *
      *                LINKEDIN.COM/IN/RENAN-MUNIZ-MERLO
      *----------------------------------------------------------------*
      *    PROGRAM-ID..: CSRG0004.                                     *
      *    ANALYST.....: RENAN MUNIZ MERLO                             *
      *    PROGRAMMER..: RENAN MUNIZ MERLO                             *
      *    DATE........: 10/04/2020                                    *
      *----------------------------------------------------------------*
      *    PROJECT.....: CUSTOMER REGISTRATION PROJECT - CUSTMREG      *
      *----------------------------------------------------------------*
      *    GOAL........: READ CUSTOMER DATA FILE FOR DELETE FROM DB.   *
      *                                                                *
      *----------------------------------------------------------------*
      *    FILES.......:  DDNAME          LRECL       INCLUDE/BOOK     *
      *                   FILE0005        00015       CSTMRG01         *
      *                   FILEERR4        00015       CSTMRG01         *
      *----------------------------------------------------------------*
      *    TABLE DB2...:  TB_CUSTOMER                 BTB_CUSTOMER     *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *================================================================*
      *           E N V I R O N M E N T      D I V I S I O N           *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE0005      ASSIGN TO UTS-S-FILE0005
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILE0005.

           SELECT FILEERR4      ASSIGN TO UTS-S-FILEERR4
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILEERR4.

      *================================================================*
      *                  D A T A      D I V I S I O N                  *
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
      *
       FD FILE0005
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILE0005   PIC X(015).

       FD FILEERR4
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILEERR4   PIC X(15).

      *-----------------------------------------------------------------*
      *                  WORKING-STORAGE SECTION                        *
      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       77 WRK-FILE0005-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-DELETED-REGS-COUNTER               PIC 9(04) VALUE ZEROS .
       77 WRK-FILEERR4-REGS-COUNTER              PIC 9(04) VALUE ZEROS .

       77 WRK-FILE0005-EOF                       PIC X(03) VALUE SPACES.

      *DATA FOR ERROR LOG:
       01 WRK-ERROR-LOG.
          03 WRK-PROGRAM                         PIC X(08) VALUE
                                                           'CSRG0004'  .
          03 WRK-ERROR-MSG                       PIC X(30) VALUE SPACES.
          03 WRK-ERROR-CODE                      PIC X(30) VALUE SPACES.
          03 WRK-ERROR-DATE                      PIC X(10) VALUE SPACES.
          03 WRK-ERROR-TIME                      PIC X(08) VALUE SPACES.

      *ABENDING PROGRAM:
       77 WRK-ABEND-PGM                          PIC X(08) VALUE
                                                           'ABENDPGM'  .

       01 WRK-FILE-STATUS.
          03 WRK-FS-FILE0005                     PIC 9(02) VALUE ZEROS .
          03 WRK-FS-FILEERR4                     PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0005-REG.
          COPY 'CSTMRG01'.

       01 WRK-FILEERR4-REG.
          COPY 'CSTMRG01'.

      *WORKING DATA FOR THE SYSTEM DATE AND TIME.
       01 WRK-SYSTEM-DATE.
          03 YY                                  PIC 9(02) VALUE ZEROS .
          03 MM                                  PIC 9(02) VALUE ZEROS .
          03 DD                                  PIC 9(02) VALUE ZEROS .
      *
       01 WRK-DATE-FORMATTED.
          03 DD-FORMATTED                        PIC 9(02) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '-'   .
          03 MM-FORMATTED                        PIC 9(02) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '-'   .
          03 YYYY-FORMATTED                      PIC 9(04) VALUE ZEROS .
      *
       01 WRK-SYSTEM-TIME.
          03 HOUR                                PIC 9(02) VALUE ZEROS .
          03 MINUTE                              PIC 9(02) VALUE ZEROS .
          03 SECOND                              PIC 9(02) VALUE ZEROS .
          03 HUNDREDTH                           PIC 9(02) VALUE ZEROS .
      *
       01 WRK-TIME-FORMATTED.
          03 HOUR-FORMATTED                      PIC 9(02) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE ':'.
          03 MINUTE-FORMATTED                    PIC 9(02) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE ':'.
          03 SECOND-FORMATTED                    PIC 9(02) VALUE ZEROS .

       01 WRK-WHEN-COPILED.
          03 MM-COMPILED                         PIC X(02) VALUE SPACES.
          03 FILLER                              PIC X(01) VALUE '/'.
          03 DD-COMPILED                         PIC X(02) VALUE SPACES.
          03 FILLER                              PIC X(01) VALUE '/'.
          03 YY-COMPILED                         PIC X(02) VALUE SPACES.
          03 HOUR-COMPILED                       PIC X(02) VALUE SPACES.
          03 FILLER                              PIC X(01) VALUE '-'.
          03 MINUTE-COMPILED                     PIC X(02) VALUE SPACES.
          03 FILLER                              PIC X(01) VALUE '-'.
          03 SECOND-COMPILED                     PIC X(02) VALUE SPACES.


       01 WRK-SQLCODE-SINAL PIC +9(009) VALUE ZEROS.
       01 FILLER REDEFINES WRK-SQLCODE-SINAL.
          05 FILLER         PIC X(001).
          05 WRK-SQLCODE-NUM PIC 9(009).
       01 FILLER REDEFINES WRK-SQLCODE-SINAL.
          05 WRK-SQLCODE-X  PIC X(010).

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
              INCLUDE BTB_CUSTOMER
           END-EXEC.
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       0000-MAIN-PROCESS               SECTION.
      *----------------------------------------------------------------*
           MOVE WHEN-COMPILED          TO WRK-WHEN-COPILED.

           PERFORM 1000-INITIALIZE.

           PERFORM 2000-PROCESS      UNTIL WRK-FILE0005-EOF EQUAL 'END'.

           PERFORM 3000-FINALIZE.
      *----------------------------------------------------------------*
       0000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INITIALIZE                 SECTION.
      *----------------------------------------------------------------*
           PERFORM 9000-GET-DATE-TIME.

           INITIALIZE WRK-FILE0005-REG
                      WRK-FILEERR4-REG.

           OPEN INPUT  FILE0005
                OUTPUT FILEERR4.

           MOVE 'OPEN FILE FILE0005'   TO WRK-ERROR-MSG.
           PERFORM 8100-TEST-FS-FILE0005.

           MOVE 'OPEN FILE FILEERR4'   TO WRK-ERROR-MSG.
           PERFORM 8200-TEST-FS-FILEERR4.

           PERFORM 2100-READ-FILE0005.

      *----------------------------------------------------------------*
       1000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESS                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-MOVE-KEY-FOR-DELETE.

           PERFORM 2300-DELETE.

           IF SQLCODE EQUAL +100
               PERFORM 2500-WRITE-FILEERR4
           END-IF.

           PERFORM 2100-READ-FILE0005.

      *----------------------------------------------------------------*
       2000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-READ-FILE0005              SECTION.
      *----------------------------------------------------------------*
           MOVE 'READING FILE0005'     TO   WRK-ERROR-MSG.

           READ FILE0005               INTO WRK-FILE0005-REG.

           PERFORM  8100-TEST-FS-FILE0005.

           IF WRK-FS-FILE0005          EQUAL 10
              MOVE 'END'               TO   WRK-FILE0005-EOF
           ELSE
              ADD 1                    TO   WRK-FILE0005-REGS-COUNTER
           END-IF.
      *----------------------------------------------------------------*
       2100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-MOVE-KEY-FOR-DELETE        SECTION.
      *----------------------------------------------------------------*

           MOVE CSTMRG01-CPF           OF WRK-FILE0005-REG
           TO CPF                      OF BTB-CUSTOMER.

      *----------------------------------------------------------------*
       2200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2300-DELETE                     SECTION.
      *----------------------------------------------------------------*
           STRING 'DELETING: ',CSTMRG01-CPF OF WRK-FILE0005-REG
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-MSG.

           EXEC SQL
               DELETE FROM TB_CUSTOMER
                     WHERE CPF = :CPF
           END-EXEC.

           IF SQLCODE NOT EQUAL ZEROS AND +100
              MOVE SQLCODE             TO WRK-SQLCODE-SINAL
              STRING 'SQLCODE..:',WRK-SQLCODE-X
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.

           IF SQLCODE EQUAL ZEROS
              ADD 1                    TO WRK-DELETED-REGS-COUNTER
           END-IF.

      *----------------------------------------------------------------*
       2300-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2500-WRITE-FILEERR4            SECTION.
      *----------------------------------------------------------------*
           MOVE 'RECORDING ERROR FILE'
                                       TO WRK-ERROR-MSG.

           IF WRK-FILEERR4-REGS-COUNTER  EQUAL ZEROS
              MOVE 'NOT FOUND:'          TO FD-REG-FILEERR4
              WRITE FD-REG-FILEERR4
              PERFORM 8200-TEST-FS-FILEERR4
           END-IF.


           MOVE CSTMRG01-CPF           OF WRK-FILE0005-REG
           TO   CSTMRG01-CPF           OF WRK-FILEERR4-REG.

           MOVE  WRK-FILEERR4-REG      TO FD-REG-FILEERR4.

           WRITE FD-REG-FILEERR4.

           PERFORM 8200-TEST-FS-FILEERR4.

           IF WRK-FS-FILEERR4          EQUAL ZEROS
              ADD  1                   TO    WRK-FILEERR4-REGS-COUNTER
           END-IF.

      *----------------------------------------------------------------*
       2500-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-FINALIZE                   SECTION.
      *----------------------------------------------------------------*
           DISPLAY '***************************'.
           DISPLAY '*  SUCCESSFUL  EXECUTION  *'.
           DISPLAY '***************************'.
           DISPLAY '*MODULE..........:' WRK-PROGRAM '*'.
           DISPLAY '*COMPILED........:'
           DD-COMPILED '/' MM-COMPILED '/' YY-COMPILED '*'.
           DISPLAY '*.................'
           HOUR-COMPILED '.' MINUTE-COMPILED '.'SECOND-COMPILED '*'.
           DISPLAY '*-------------------------*'.
           DISPLAY '*RECORDS FILE0005....:' WRK-FILE0005-REGS-COUNTER
           '*'.
           DISPLAY '*DELETED OF DATABASE.:' WRK-DELETED-REGS-COUNTER
           '*'.

           DISPLAY '*RECORDS FILEERR4....:' WRK-FILEERR4-REGS-COUNTER
           '*'.
           DISPLAY '*-------------------------*'.
           DISPLAY '*EXECUTED......:' WRK-DATE-FORMATTED '*'.
           DISPLAY '*.................' WRK-TIME-FORMATTED '*'.
           DISPLAY '***************************'.

           STOP RUN.
      *----------------------------------------------------------------*
       3000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8100-TEST-FS-FILE0005           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILE0005          NOT EQUAL ZEROS AND 10
              MOVE WRK-FS-FILE0005     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8200-TEST-FS-FILEERR4           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILEERR4          NOT EQUAL ZEROS
              MOVE WRK-FS-FILEERR4     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9000-GET-DATE-TIME              SECTION.
      *----------------------------------------------------------------*
           ACCEPT WRK-SYSTEM-DATE      FROM DATE.
           MOVE YY                     TO YYYY-FORMATTED.
           MOVE MM                     TO MM-FORMATTED.
           MOVE DD                     TO DD-FORMATTED.
           ADD  2000                   TO YYYY-FORMATTED.

           ACCEPT WRK-SYSTEM-TIME      FROM TIME.
           MOVE HOUR                   TO HOUR-FORMATTED.
           MOVE MINUTE                 TO MINUTE-FORMATTED.
           MOVE SECOND                 TO SECOND-FORMATTED.
      *----------------------------------------------------------------*
       9000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-CALL-ABEND-PGM             SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
               ROLLBACK
           END-EXEC.

           MOVE WRK-DATE-FORMATTED     TO WRK-ERROR-DATE.
           MOVE WRK-TIME-FORMATTED     TO WRK-ERROR-TIME.
           CALL WRK-ABEND-PGM          USING WRK-ERROR-LOG.
      *----------------------------------------------------------------*
       9000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
