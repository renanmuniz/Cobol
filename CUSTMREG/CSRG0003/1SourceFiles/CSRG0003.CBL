      *================================================================*
      *        I D E N T I F I C A T I O N      D I V I S I O N        *
      *================================================================*
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     CSRG0003.
       AUTHOR.         RENAN MUNIZ MERLO.
       DATE-WRITTEN.   10/04/2020.
       DATE-COMPILED   WHEN-COMPILED.
       REMARKS.
      *----------------------------------------------------------------*
      *                RENAN MUNIZ MERLO COBOL DEVELOPER               *
      *          https://www.linkedin.com/in/renan-muniz-merlo         *
      *----------------------------------------------------------------*
      *    PROGRAM-ID..: CSRG0003.                                     *
      *    ANALYST.....: RENAN MUNIZ MERLO                             *
      *    PROGRAMMER..: RENAN MUNIZ MERLO                             *
      *    DATE........: 10/04/2020                                    *
      *----------------------------------------------------------------*
      *    PROJECT.....: CUSTOMER REGISTRATION PROJECT - CUSTMREG      *
      *----------------------------------------------------------------*
      *    GOAL........: READ CUSTOMER DATA FILE FOR UPDATE INTO DB.   *
      *                                                                *
      *----------------------------------------------------------------*
      *    FILES.......:  DDNAME          LRECL       INCLUDE/BOOK     *
      *                   FILE0004        00222       CSTMRG01         *
      *                   FILEERR3        00015       CSTMRG01         *
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

           SELECT FILE0004      ASSIGN TO UTS-S-FILE0004
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILE0004.

           SELECT FILEERR3      ASSIGN TO UTS-S-FILEERR3
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILEERR3.

      *================================================================*
      *                  D A T A      D I V I S I O N                  *
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
      *
       FD FILE0004
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILE0004   PIC X(222).

       FD FILEERR3
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILEERR3   PIC X(15).

      *-----------------------------------------------------------------*
      *                  WORKING-STORAGE SECTION                        *
      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       77 WRK-FILE0004-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-UPDATED-REGS-COUNTER               PIC 9(04) VALUE ZEROS .
       77 WRK-FILEERR3-REGS-COUNTER              PIC 9(04) VALUE ZEROS .

       77 WRK-FILE0004-EOF                       PIC X(03) VALUE SPACES.

      *DATA FOR ERROR LOG:
       01 WRK-ERROR-LOG.
          03 WRK-PROGRAM                         PIC X(08) VALUE
                                                           'CSRG0003'  .
          03 WRK-ERROR-MSG                       PIC X(30) VALUE SPACES.
          03 WRK-ERROR-CODE                      PIC X(30) VALUE SPACES.
          03 WRK-ERROR-DATE                      PIC X(10) VALUE SPACES.
          03 WRK-ERROR-TIME                      PIC X(08) VALUE SPACES.

      *ABENDING PROGRAM:
       77 WRK-ABEND-PGM                          PIC X(08) VALUE
                                                           'ABENDPGM'  .

       01 WRK-FILE-STATUS.
          03 WRK-FS-FILE0004                     PIC 9(02) VALUE ZEROS .
          03 WRK-FS-FILEERR3                     PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0004-REG.
          COPY 'CSTMRG01'.

       01 WRK-FILEERR3-REG.
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

           PERFORM 2000-PROCESS      UNTIL WRK-FILE0004-EOF EQUAL 'END'.

           PERFORM 3000-FINALIZE.
      *----------------------------------------------------------------*
       0000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INITIALIZE                 SECTION.
      *----------------------------------------------------------------*
           PERFORM 9000-GET-DATE-TIME.

           INITIALIZE WRK-FILE0004-REG
                      WRK-FILEERR3-REG.

           OPEN INPUT  FILE0004
                OUTPUT FILEERR3.

           MOVE 'OPEN FILE FILE0004'   TO WRK-ERROR-MSG.
           PERFORM 8100-TEST-FS-FILE0004.

           MOVE 'OPEN FILE FILEERR3'   TO WRK-ERROR-MSG.
           PERFORM 8200-TEST-FS-FILEERR3.

           PERFORM 2100-READ-FILE0004.

      *----------------------------------------------------------------*
       1000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESS                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 2150-MOVE-KEY-FOR-SELECT.

           PERFORM 2200-SELECT-REGISTRY-FOR-UPDT.

           IF SQLCODE EQUAL +100
               PERFORM 2500-WRITE-FILEERR3
           ELSE
               PERFORM 2300-MOVE-DATA-FOR-UPDT
               PERFORM 2400-UPDATE
           END-IF.

           PERFORM 2100-READ-FILE0004.

      *----------------------------------------------------------------*
       2000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-READ-FILE0004              SECTION.
      *----------------------------------------------------------------*
           MOVE 'READING FILE0004'     TO   WRK-ERROR-MSG.

           READ FILE0004               INTO WRK-FILE0004-REG.

           PERFORM  8100-TEST-FS-FILE0004.

           IF WRK-FS-FILE0004          EQUAL 10
              MOVE 'END'               TO   WRK-FILE0004-EOF
           ELSE
              ADD 1                    TO   WRK-FILE0004-REGS-COUNTER
           END-IF.
      *----------------------------------------------------------------*
       2100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2150-MOVE-KEY-FOR-SELECT        SECTION.
      *----------------------------------------------------------------*

           MOVE CSTMRG01-CPF           OF WRK-FILE0004-REG
           TO CPF                      OF BTB-CUSTOMER.

      *----------------------------------------------------------------*
       2150-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-SELECT-REGISTRY-FOR-UPDT   SECTION.
      *----------------------------------------------------------------*

           MOVE 'SELECTING EXISTING REGISTRY'
                                       TO WRK-ERROR-MSG.

           EXEC SQL
               SELECT CPF
                     ,NAME
                     ,BIRTH
                     ,STREET
                     ,HNUMBER
                     ,OTHER_INFO
                     ,CITY
                     ,STATE
                     ,COUNTRY
                     ,TIMEREGINC
               INTO  :CPF
                    ,:NAME
                    ,:BIRTH
                    ,:STREET
                    ,:HNUMBER
                    ,:OTHER-INFO
                    ,:CITY
                    ,:STATE
                    ,:COUNTRY
                    ,:TIMEREGINC
               FROM  TB_CUSTOMER
               WHERE CPF = :CPF
           END-EXEC.

           IF SQLCODE NOT EQUAL ZEROS AND +100
              MOVE SQLCODE             TO WRK-SQLCODE-SINAL
              STRING 'SQLCODE..:',WRK-SQLCODE-X
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.


      *----------------------------------------------------------------*
       2200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2300-MOVE-DATA-FOR-UPDT        SECTION.
      *----------------------------------------------------------------*
           IF CSTMRG01-NAME           OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-NAME      OF WRK-FILE0004-REG
                                      TO NAME
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-BIRTH          OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-BIRTH     OF WRK-FILE0004-REG
                                      TO BIRTH
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-STREET         OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-STREET    OF WRK-FILE0004-REG
                                      TO STREET
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-NUMBER         OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-NUMBER    OF WRK-FILE0004-REG
                                      TO HNUMBER
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-OTHER-INFO     OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-OTHER-INFO
                                      OF WRK-FILE0004-REG
                                      TO OTHER-INFO
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-CITY           OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-CITY      OF WRK-FILE0004-REG
                                      TO CITY
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-STATE          OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-STATE     OF WRK-FILE0004-REG
                                      TO STATE
                                      OF BTB-CUSTOMER
           END-IF.

           IF CSTMRG01-COUNTRY        OF WRK-FILE0004-REG
                                         NOT EQUAL SPACES
              MOVE CSTMRG01-COUNTRY   OF WRK-FILE0004-REG
                                      TO COUNTRY
                                      OF BTB-CUSTOMER
           END-IF.

      *----------------------------------------------------------------*
       2300-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2400-UPDATE                     SECTION.
      *----------------------------------------------------------------*

           MOVE 'UPDATING EXISTING REGISTRY'
                                       TO WRK-ERROR-MSG.

           EXEC SQL
               UPDATE TB_CUSTOMER
                  SET NAME         = :NAME
                     ,BIRTH        = :BIRTH
                     ,STREET       = :STREET
                     ,HNUMBER      = :HNUMBER
                     ,OTHER_INFO   = :OTHER-INFO
                     ,CITY         = :CITY
                     ,STATE        = :STATE
                     ,COUNTRY      = :COUNTRY
               WHERE  CPF          = :CPF
           END-EXEC.

           IF SQLCODE NOT EQUAL ZEROS AND +100
              MOVE SQLCODE             TO WRK-SQLCODE-SINAL
              STRING 'SQLCODE..:',WRK-SQLCODE-X
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.

           ADD 1                    TO WRK-UPDATED-REGS-COUNTER.

      *----------------------------------------------------------------*
       2400-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2500-WRITE-FILEERR3            SECTION.
      *----------------------------------------------------------------*
           MOVE 'RECORDING ERROR FILE'
                                       TO WRK-ERROR-MSG.

           IF WRK-FILEERR3-REGS-COUNTER  EQUAL ZEROS
              MOVE 'NOT FOUND:'          TO FD-REG-FILEERR3
              WRITE FD-REG-FILEERR3
              PERFORM 8200-TEST-FS-FILEERR3
           END-IF.


           MOVE CSTMRG01-CPF           OF WRK-FILE0004-REG
           TO   CSTMRG01-CPF           OF WRK-FILEERR3-REG.

           MOVE  WRK-FILEERR3-REG      TO FD-REG-FILEERR3.

           WRITE FD-REG-FILEERR3.

           PERFORM 8200-TEST-FS-FILEERR3.

           IF WRK-FS-FILEERR3          EQUAL ZEROS
              ADD  1                   TO    WRK-FILEERR3-REGS-COUNTER
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
           DISPLAY '*RECORDS FILE0004....:' WRK-FILE0004-REGS-COUNTER
           '*'.
           DISPLAY '*UPDATED IN DATABASE.:' WRK-UPDATED-REGS-COUNTER
           '*'.

           DISPLAY '*RECORDS FILEERR3....:' WRK-FILEERR3-REGS-COUNTER
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
       8100-TEST-FS-FILE0004           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILE0004          NOT EQUAL ZEROS AND 10
              MOVE WRK-FS-FILE0004     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8200-TEST-FS-FILEERR3           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILEERR3          NOT EQUAL ZEROS
              MOVE WRK-FS-FILEERR3     TO  WRK-ERROR-CODE
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
