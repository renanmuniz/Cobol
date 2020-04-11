      *================================================================*
      *        I D E N T I F I C A T I O N      D I V I S I O N        *
      *================================================================*
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     CSRG0002.
       AUTHOR.         RENAN MUNIZ MERLO.
       DATE-WRITTEN.   09/04/2020.
       DATE-COMPILED   WHEN-COMPILED.
       REMARKS.
      *----------------------------------------------------------------*
      *                RENAN MUNIZ MERLO COBOL DEVELOPER               *
      *          https://www.linkedin.com/in/renan-muniz-merlo         *
      *----------------------------------------------------------------*
      *    PROGRAM-ID..: CSRG0002.                                     *
      *    ANALYST.....: RENAN MUNIZ MERLO                             *
      *    PROGRAMMER..: RENAN MUNIZ MERLO                             *
      *    DATE........: 09/04/2020                                    *
      *----------------------------------------------------------------*
      *    PROJECT.....: CUSTOMER REGISTRATION PROJECT - CUSTMREG      *
      *----------------------------------------------------------------*
      *    GOAL........: READ CUSTOMER DATA FILE AND INSERT INTO DB.   *
      *                                                                *
      *----------------------------------------------------------------*
      *    FILES.......:  DDNAME          LRECL       INCLUDE/BOOK     *
      *                   FILE0003        00222       CSTMRG01         *
      *                   FILEERR2        00015       CSTMRG01         *
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

           SELECT FILE0003      ASSIGN TO UTS-S-FILE0003
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILE0003.

           SELECT FILEERR2      ASSIGN TO UTS-S-FILEERR2
            ORGANIZATION IS     SEQUENTIAL
            ACCESS MODE  IS     SEQUENTIAL
            FILE STATUS  IS     WRK-FS-FILEERR2.

      *================================================================*
      *                  D A T A      D I V I S I O N                  *
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
      *
       FD FILE0003
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILE0003   PIC X(222).

       FD FILEERR2
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 00 RECORDS.
       01 FD-REG-FILEERR2   PIC X(15).

      *-----------------------------------------------------------------*
      *                  WORKING-STORAGE SECTION                        *
      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       77 WRK-FILE0003-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-INSERTED-REGS-COUNTER              PIC 9(04) VALUE ZEROS .
       77 WRK-FILEERR2-REGS-COUNTER              PIC 9(04) VALUE ZEROS .

       77 WRK-FILE0003-EOF                       PIC X(03) VALUE SPACES.

      *DATA FOR ERROR LOG:
       01 WRK-ERROR-LOG.
          03 WRK-PROGRAM                         PIC X(08) VALUE
                                                           'CSRG0002'  .
          03 WRK-ERROR-MSG                       PIC X(30) VALUE SPACES.
          03 WRK-ERROR-CODE                      PIC X(30) VALUE SPACES.
          03 WRK-ERROR-DATE                      PIC X(10) VALUE SPACES.
          03 WRK-ERROR-TIME                      PIC X(08) VALUE SPACES.

      *ABENDING PROGRAM:
       77 WRK-ABEND-PGM                          PIC X(08) VALUE
                                                           'ABENDPGM'  .

       01 WRK-FILE-STATUS.
          03 WRK-FS-FILE0003                     PIC 9(02) VALUE ZEROS .
          03 WRK-FS-FILEERR2                     PIC 9(02) VALUE ZEROS .

       01 WRK-FILE0003-REG.
          COPY 'CSTMRG01'.

       01 WRK-FILEERR2-REG.
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

           PERFORM 2000-PROCESS      UNTIL WRK-FILE0003-EOF EQUAL 'END'.

           PERFORM 3000-FINALIZE.
      *----------------------------------------------------------------*
       0000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INITIALIZE                 SECTION.
      *----------------------------------------------------------------*
           PERFORM 9000-GET-DATE-TIME.

           INITIALIZE WRK-FILE0003-REG
                      WRK-FILEERR2-REG.

           OPEN INPUT  FILE0003
                OUTPUT FILEERR2.

           MOVE 'OPEN FILE FILE0003'   TO WRK-ERROR-MSG.
           PERFORM 8100-TEST-FS-FILE0003.

           MOVE 'OPEN FILE FILEERR2'   TO WRK-ERROR-MSG.
           PERFORM 8200-TEST-FS-FILEERR2.

           PERFORM 2100-READ-FILE0003.

      *----------------------------------------------------------------*
       1000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESS                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 2150-MOVE-DATA.

           PERFORM 2200-VERIFY-REGISTRY.

           IF SQLCODE EQUAL +100
               PERFORM 2300-INSERT-DB
           ELSE
               PERFORM 2500-WRITE-FILEERR2
           END-IF.

           PERFORM 2100-READ-FILE0003.

      *----------------------------------------------------------------*
       2000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-READ-FILE0003              SECTION.
      *----------------------------------------------------------------*
           MOVE 'READING FILE0003'     TO   WRK-ERROR-MSG.

           READ FILE0003               INTO WRK-FILE0003-REG.

           PERFORM  8100-TEST-FS-FILE0003.

           IF WRK-FS-FILE0003          EQUAL 10
              MOVE 'END'               TO   WRK-FILE0003-EOF
           ELSE
              ADD 1                    TO   WRK-FILE0003-REGS-COUNTER
           END-IF.
      *----------------------------------------------------------------*
       2100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2150-MOVE-DATA                  SECTION.
      *----------------------------------------------------------------*

           MOVE CSTMRG01-CPF           OF WRK-FILE0003-REG
           TO CPF                      OF BTB-CUSTOMER.

           MOVE CSTMRG01-NAME          OF WRK-FILE0003-REG
           TO NAME                     OF BTB-CUSTOMER.

           MOVE CSTMRG01-BIRTH         OF WRK-FILE0003-REG
           TO BIRTH                    OF BTB-CUSTOMER.

           MOVE CSTMRG01-STREET        OF WRK-FILE0003-REG
           TO STREET                   OF BTB-CUSTOMER.

           MOVE CSTMRG01-NUMBER        OF WRK-FILE0003-REG
           TO HNUMBER                  OF BTB-CUSTOMER.

           MOVE CSTMRG01-OTHER-INFO    OF WRK-FILE0003-REG
           TO OTHER-INFO               OF BTB-CUSTOMER.

           MOVE CSTMRG01-CITY          OF WRK-FILE0003-REG
           TO CITY                     OF BTB-CUSTOMER.

           MOVE CSTMRG01-STATE         OF WRK-FILE0003-REG
           TO STATE                    OF BTB-CUSTOMER.

           MOVE CSTMRG01-COUNTRY       OF WRK-FILE0003-REG
           TO COUNTRY                  OF BTB-CUSTOMER.

      *----------------------------------------------------------------*
       2150-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-VERIFY-REGISTRY            SECTION.
      *----------------------------------------------------------------*

           MOVE 'VERIFYING EXISTING REGISTRY'
                                       TO WRK-ERROR-MSG.

           EXEC SQL
               SELECT  CPF
                 INTO :CPF
                 FROM  TB_CUSTOMER
                WHERE  CPF = :CPF
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
       2300-INSERT-DB                  SECTION.
      *----------------------------------------------------------------*

           STRING 'INSERTING REGISTRY..:',
                  CPF               OF BTB-CUSTOMER
                                    DELIMITED BY SPACE
                                    INTO WRK-ERROR-MSG.

           EXEC SQL
               INSERT INTO TB_CUSTOMER
               (CPF
               ,NAME
               ,BIRTH
               ,STREET
               ,HNUMBER
               ,OTHER_INFO
               ,CITY
               ,STATE
               ,COUNTRY
               ,TIMEREGINC)
               VALUES
               (:CPF
               ,:NAME
               ,:BIRTH
               ,:STREET
               ,:HNUMBER
               ,:OTHER-INFO
               ,:CITY
               ,:STATE
               ,:COUNTRY
               ,CURRENT TIMESTAMP)
           END-EXEC.

           IF SQLCODE NOT EQUAL ZEROS
              MOVE SQLCODE             TO WRK-SQLCODE-SINAL
              STRING 'SQLCODE..:',WRK-SQLCODE-X
                                       DELIMITED BY SPACE
                                       INTO WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.

           ADD 1                    TO WRK-INSERTED-REGS-COUNTER.


      *----------------------------------------------------------------*
       2300-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2500-WRITE-FILEERR2            SECTION.
      *----------------------------------------------------------------*
           MOVE 'RECORDING ERROR FILE'
                                       TO WRK-ERROR-MSG.

           IF WRK-FILEERR2-REGS-COUNTER  EQUAL ZEROS
              MOVE 'ALREADY IN DB:'      TO FD-REG-FILEERR2
              WRITE FD-REG-FILEERR2
              PERFORM 8200-TEST-FS-FILEERR2
           END-IF.


           MOVE CSTMRG01-CPF           OF WRK-FILE0003-REG
           TO   CSTMRG01-CPF           OF WRK-FILEERR2-REG.

           MOVE  WRK-FILEERR2-REG      TO FD-REG-FILEERR2.

           WRITE FD-REG-FILEERR2.

           PERFORM 8200-TEST-FS-FILEERR2.

           IF WRK-FS-FILEERR2          EQUAL ZEROS
              ADD  1                   TO    WRK-FILEERR2-REGS-COUNTER
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
           DISPLAY '*RECORDS FILE0003....:' WRK-FILE0003-REGS-COUNTER
           '*'.
           DISPLAY '*INSERTED IN DATABASE:' WRK-INSERTED-REGS-COUNTER
           '*'.

           DISPLAY '*RECORDS FILEERR2....:' WRK-FILEERR2-REGS-COUNTER
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
       8100-TEST-FS-FILE0003           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILE0003          NOT EQUAL ZEROS AND 10
              MOVE WRK-FS-FILE0003     TO  WRK-ERROR-CODE
              PERFORM 9999-CALL-ABEND-PGM
           END-IF.
      *----------------------------------------------------------------*
       8100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8200-TEST-FS-FILEERR2           SECTION.
      *----------------------------------------------------------------*
           IF WRK-FS-FILEERR2          NOT EQUAL ZEROS
              MOVE WRK-FS-FILEERR2     TO  WRK-ERROR-CODE
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
