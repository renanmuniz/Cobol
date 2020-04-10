      *================================================================*
      *        I D E N T I F I C A T I O N      D I V I S I O N        *
      *================================================================*
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     CSRG0001.
       AUTHOR.         RENAN MUNIZ MERLO.
       DATE-WRITTEN.   29/03/2020.
       DATE-COMPILED   WHEN-COMPILED.
       REMARKS.
      *----------------------------------------------------------------*
      *                RENAN MUNIZ MERLO COBOL DEVELOPER               *
      *                LINKEDIN.COM/IN/RENAN-MUNIZ-MERLO
      *----------------------------------------------------------------*
      *    PROGRAM-ID..: CSRG0001.                                     *
      *    ANALYST.....: RENAN MUNIZ MERLO                             *
      *    PROGRAMMER..: RENAN MUNIZ MERLO                             *
      *    DATE........: 29/03/2020                                    *
      *----------------------------------------------------------------*
      *    PROJECT.....: CUSTOMER REGISTRATION PROJECT - CUSTMREG      *
      *----------------------------------------------------------------*
      *    GOAL........: GENERATE FILE WITH CUSTOMER DATA FOR REGISTRA-*
      *                  TION.                                         *
      *----------------------------------------------------------------*
      *    FILES.......:  DDNAME          LRECL       INCLUDE/BOOK     *
      *                   FILE0001        00015       FLBK0001         *
      *                   FILE0002        00222       FLBK0002         *
      *                   FILE0003        00222       FLBK0002         *
      *                   FILEERR1        00015       FLBK0001         *
      *----------------------------------------------------------------*
      *    TABLE DB2...:  NONE.                                        *
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

           SELECT FILE0001      ASSIGN TO UTS-S-FILE0001
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILE0001.

           SELECT FILE0002      ASSIGN TO UTS-S-FILE0002
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILE0002.

           SELECT FILE0003      ASSIGN TO UTS-S-FILE0003
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILE0003.

           SELECT FILEERR1      ASSIGN TO UTS-S-FILEERR1
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILEERR1.

      *================================================================*
      *                  D A T A      D I V I S I O N                  *
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
      *
       FD FILE0001
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILE0001   PIC X(15).

       FD FILE0002
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILE0002   PIC X(222).

       FD FILE0003
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILE0003   PIC X(222).

       FD FILEERR1
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILEERR1   PIC X(15).

      *-----------------------------------------------------------------*
      *                  WORKING-STORAGE SECTION                        *
      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       77 WRK-FILE0001-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-FILE0002-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-FILE0003-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-FILEERR1-REGS-COUNTER              PIC 9(04) VALUE ZEROS .

       77 WRK-FILE0001-EOF                       PIC X(03) VALUE SPACES.
       77 WRk-FILE0002-EOF                       PIC X(03) VALUE SPACES.

      *DATA FOR ERROR LOG:
       01 WRK-ERROR-LOG.
          03 WRK-PROGRAM                         PIC X(08) VALUE
                                                           'CSRG0001'  .
          03 WRK-ERROR-MSG                       PIC X(30) VALUE SPACES.
          03 WRK-ERROR-CODE                      PIC X(30) VALUE SPACES.
          03 WRK-ERROR-DATE                      PIC X(10) VALUE SPACES.
          03 WRK-ERROR-TIME                      PIC X(08) VALUE SPACES.

      *ABENDING PROGRAM:
       77 WRK-ABEND-PGM                          PIC X(08) VALUE
                                                           'ABENDPGM'  .
       01 WRK-FILE0001-KEY.
          03 WRK-PART01-K1                       PIC 9(04) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '.'   .
          03 WRK-PART02-K1                       PIC 9(03) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '.'   .
          03 WRK-PART03-K1                       PIC 9(03) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '-'   .
          03 WRK-PART04-K1                       PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0001-KEY-OK.
          03 WRK-PART01-K1-OK                    PIC 9(04) VALUE ZEROS .
          03 WRK-PART02-K1-OK                    PIC 9(03) VALUE ZEROS .
          03 WRK-PART03-K1-OK                    PIC 9(03) VALUE ZEROS .
          03 WRK-PART04-K1-OK                    PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0002-KEY.
          03 WRK-PART01-K2                       PIC 9(04) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '.'   .
          03 WRK-PART02-K2                       PIC 9(03) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '.'   .
          03 WRK-PART03-K2                       PIC 9(03) VALUE ZEROS .
          03 FILLER                              PIC X(01) VALUE '-'   .
          03 WRK-PART04-K2                       PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0002-KEY-OK.
          03 WRK-PART01-K2-OK                    PIC 9(04) VALUE ZEROS .
          03 WRK-PART02-K2-OK                    PIC 9(03) VALUE ZEROS .
          03 WRK-PART03-K2-OK                    PIC 9(03) VALUE ZEROS .
          03 WRK-PART04-K2-OK                    PIC 9(02) VALUE ZEROS .

       01 WRK-ORDER-VERIF-KEYS.
          03 WRK-KEY1-PREVIOUS                   PIC X(12) VALUE ZEROS .
          03 WRK-KEY2-PREVIOUS                   PIC X(12) VALUE ZEROS .

       01 WRK-FILE-STATUS.
          03 WRK-FS-FILE0001                     PIC 9(02) VALUE ZEROS .
          03 WRK-FS-FILE0002                     PIC 9(02) VALUE ZEROS .
          03 WRK-FS-FILE0003                     PIC 9(02) VALUE ZEROS .
          03 WRK-FS-FILEERR1                     PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0001-REG.
          COPY 'CSTMRG01'.

       01 WRK-FILE0002-REG.
          COPY 'CSTMRG01'.

       01 WRK-FILE0003-REG.
          COPY 'CSTMRG01'.

       01 WRK-FILEERR1-REG.
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
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       0000-MAIN-PROCESS               SECTION.
      *----------------------------------------------------------------*
           MOVE WHEN-COMPILED          TO WRK-WHEN-COPILED.

           PERFORM 1000-INITIALIZE.

           PERFORM 2000-PROCESS-FILE UNTIL WRK-FILE0001-EOF EQUAL 'END'
                                     AND   WRK-FILE0002-EOF EQUAL 'END'.

           PERFORM 3000-FINALIZE.
      *----------------------------------------------------------------*
       0000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INITIALIZE                 SECTION.
      *----------------------------------------------------------------*
           PERFORM 9000-GET-DATE-TIME.

           INITIALIZE WRK-FILE0001-REG
                      WRK-FILE0002-REG
                      WRK-FILE0003-REG
                      WRK-FILEERR1-REG.

           MOVE LOW-VALUES             TO WRK-FILE0001-KEY-OK.
           MOVE LOW-VALUES             TO WRK-FILE0002-KEY-OK.
           MOVE LOW-VALUES             TO WRK-KEY1-PREVIOUS.
           MOVE LOW-VALUES             TO WRK-KEY2-PREVIOUS.

           OPEN INPUT  FILE0001
                       FILE0002
                OUTPUT FILE0003
                       FILEERR1.

           MOVE 'OPEN FILE FILE0001'   TO WRK-ERROR-MSG.
           PERFORM 8100-TEST-FS-FILE0001.

           MOVE 'OPEN FILE FILE0002'   TO WRK-ERROR-MSG.
           PERFORM 8200-TEST-FS-FILE0002.

           MOVE 'OPEN FILE FILE0003'   TO WRK-ERROR-MSG.
           PERFORM 8300-TEST-FS-FILE0003.

           MOVE 'OPEN FILE FILEERR1'   TO WRK-ERROR-MSG.
           PERFORM 8400-TEST-FS-FILEERR1.

           MOVE 'READ FILE FILE0001'   TO WRK-ERROR-MSG.
           PERFORM 2100-READ-FILE0001.

           MOVE 'READ FILE FILE0002'   TO WRK-ERROR-MSG.
           PERFORM 2200-READ-FILE0002.

      *----------------------------------------------------------------*
       1000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESS-FILE               SECTION.
      *----------------------------------------------------------------*
           IF WRK-FILE0001-KEY-OK      EQUAL WRK-FILE0002-KEY-OK

              MOVE WRK-FILE0002-REG    TO    WRK-FILE0003-REG

              PERFORM 2300-WRITE-FILE0003

              PERFORM 2100-READ-FILE0001
              PERFORM 2200-READ-FILE0002

           ELSE
              IF WRK-FILE0001-KEY-OK   GREATER WRK-FILE0002-KEY-OK

                 PERFORM 2200-READ-FILE0002

              ELSE

                 MOVE WRK-FILE0001-REG    TO    WRK-FILEERR1-REG

                 PERFORM 2400-WRITE-FILEERR1

                 PERFORM 2100-READ-FILE0001

              END-IF
           END-IF.
      *----------------------------------------------------------------*
       2000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-READ-FILE0001              SECTION.
      *----------------------------------------------------------------*
           MOVE 'READING FILE 0001'
                                       TO   WRK-ERROR-MSG.

           READ FILE0001               INTO WRK-FILE0001-REG.

           PERFORM  8100-TEST-FS-FILE0001.

           IF WRK-FS-FILE0001          EQUAL 10
              MOVE 'END'               TO   WRK-FILE0001-EOF
              MOVE HIGH-VALUES         TO   WRK-FILE0001-KEY-OK
           ELSE
              MOVE CSTMRG01-CPF        OF   WRK-FILE0001-REG
                                       TO   WRK-FILE0001-KEY
              MOVE WRK-PART01-K1       TO   WRK-PART01-K1-OK
              MOVE WRK-PART02-K1       TO   WRK-PART02-K1-OK
              MOVE WRK-PART03-K1       TO   WRK-PART03-K1-OK
              MOVE WRK-PART04-K1       TO   WRK-PART04-K1-OK

              ADD 1                    TO   WRK-FILE0001-REGS-COUNTER

              PERFORM 7100-VERIFY-ORDER-KEY1
           END-IF.
      *----------------------------------------------------------------*
       2100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-READ-FILE0002              SECTION.
      *----------------------------------------------------------------*
           MOVE 'READING FILE 0002'
                                       TO   WRK-ERROR-MSG.

           READ FILE0002               INTO WRK-FILE0002-REG.

           PERFORM  8200-TEST-FS-FILE0002.

           IF WRK-FS-FILE0002          EQUAL 10
              MOVE 'END'               TO   WRK-FILE0002-EOF
              MOVE HIGH-VALUES         TO   WRK-FILE0002-KEY-OK
           ELSE
              MOVE CSTMRG01-CPF        OF   WRK-FILE0002-REG
                                       TO   WRK-FILE0002-KEY
              MOVE WRK-PART01-K2       TO   WRK-PART01-K2-OK
              MOVE WRK-PART02-K2       TO   WRK-PART02-K2-OK
              MOVE WRK-PART03-K2       TO   WRK-PART03-K2-OK
              MOVE WRK-PART04-K2       TO   WRK-PART04-K2-OK

              ADD 1                    TO   WRK-FILE0002-REGS-COUNTER

              PERFORM 7200-VERIFY-ORDER-KEY2
           END-IF.
      *----------------------------------------------------------------*
       2200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2300-WRITE-FILE0003             SECTION.
      *----------------------------------------------------------------*
           MOVE 'RECORDING RESULT FILE0003'
                                       TO    WRK-ERROR-MSG.

           MOVE  WRK-FILE0003-REG      TO    FD-REG-FILE0003.

           WRITE FD-REG-FILE0003.

           PERFORM 8300-TEST-FS-FILE0003.

           IF WRK-FS-FILE0003          EQUAL ZEROS
              ADD  1                   TO    WRK-FILE0003-REGS-COUNTER
           END-IF.

      *----------------------------------------------------------------*
       2300-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2400-WRITE-FILEERR1             SECTION.
      *----------------------------------------------------------------*
           MOVE 'RECORDING ERROR FILE'
                                       TO    WRK-ERROR-MSG.

           MOVE  WRK-FILEERR1-REG      TO    FD-REG-FILEERR1.

           WRITE FD-REG-FILEERR1.

           PERFORM 8400-TEST-FS-FILEERR1.

           IF WRK-FS-FILEERR1          EQUAL ZEROS
              ADD  1                   TO    WRK-FILEERR1-REGS-COUNTER
           END-IF.

      *----------------------------------------------------------------*
       2400-99-EXIT.                   EXIT.
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
           DISPLAY '*RECORDS FILE0001....:' WRK-FILE0001-REGS-COUNTER
           '*'.
           DISPLAY '*RECORDS FILE0002....:' WRK-FILE0002-REGS-COUNTER
           '*'.
           DISPLAY '*RECORDS FILE0003....:' WRK-FILE0003-REGS-COUNTER
           '*'.
           DISPLAY '*RECORDS FILEERR1....:' WRK-FILEERR1-REGS-COUNTER
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
       7100-VERIFY-ORDER-KEY1          SECTION.
      *----------------------------------------------------------------*
           IF WRK-FILE0001-KEY-OK      LESS WRK-KEY1-PREVIOUS
              STRING 'KEY...: ',WRK-FILE0001-KEY-OK
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-CODE
              MOVE 'FILE0001 IS OUT OF ORDER'
                                       TO WRK-ERROR-MSG
              PERFORM 9999-CALL-ABEND-PGM
           ELSE
              MOVE WRK-FILE0001-KEY-OK TO WRK-KEY1-PREVIOUS
           END-IF.
      *----------------------------------------------------------------*
       7100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       7200-VERIFY-ORDER-KEY2          SECTION.
      *----------------------------------------------------------------*
           IF WRK-FILE0002-KEY-OK      LESS WRK-KEY2-PREVIOUS
              STRING 'KEY...: ',WRK-FILE0002-KEY-OK
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-CODE
              MOVE 'FILE0002 IS OUT OF ORDER'
                                       TO WRK-ERROR-MSG
              PERFORM 9999-CALL-ABEND-PGM
           ELSE
              MOVE WRK-FILE0002-KEY-OK TO WRK-KEY2-PREVIOUS
           END-IF.
      *----------------------------------------------------------------*
       7200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8100-TEST-FS-FILE0001           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILE0001          NOT EQUAL ZEROS AND 10
              MOVE WRK-FS-FILE0001     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8200-TEST-FS-FILE0002           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILE0002          NOT EQUAL ZEROS AND 10
              MOVE WRK-FS-FILE0002     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8300-TEST-FS-FILE0003           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILE0003          NOT EQUAL ZEROS
              MOVE WRK-FS-FILE0003     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8300-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8400-TEST-FS-FILEERR1           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILEERR1          NOT EQUAL ZEROS
              MOVE WRK-FS-FILEERR1     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8400-99-EXIT.                   EXIT.
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
           MOVE WRK-DATE-FORMATTED     TO WRK-ERROR-DATE.
           MOVE WRK-TIME-FORMATTED     TO WRK-ERROR-TIME.
           CALL WRK-ABEND-PGM          USING WRK-ERROR-LOG.
      *----------------------------------------------------------------*
       9000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
