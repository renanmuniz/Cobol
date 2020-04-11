      *================================================================*
      *        I D E N T I F I C A T I O N      D I V I S I O N        *
      *================================================================*
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     ABENDPGM.
       AUTHOR.         RENAN MUNIZ MERLO.
       DATE-WRITTEN.   29/03/2020.
       DATE-COMPILED.
       REMARKS.
      *----------------------------------------------------------------*
      *                RENAN MUNIZ MERLO COBOL DEVELOPER               *
      *          https://www.linkedin.com/in/renan-muniz-merlo         *
      *----------------------------------------------------------------*
      *    PROGRAM-ID..: ABENDPGM.                                     *
      *    ANALYST.....: RENAN MUNIZ MERLO                             *
      *    PROGRAMMER..: RENAN MUNIZ MERLO                             *
      *    DATE........: 29/03/2020                                    *
      *----------------------------------------------------------------*
      *    PROJECT.....: CUSTOMER REGISTRATION PROJECT - CUSTMREG      *
      *----------------------------------------------------------------*
      *    GOAL........: ABNORMAL END PROGRAM                          *
      *----------------------------------------------------------------*
      *    FILES.......:  DDNAME          LRECL       INCLUDE/BOOK     *
      *----------------------------------------------------------------*
      *    TABLE DB2...:  NONE.                                        *
      *----------------------------------------------------------------*
      *================================================================*
      *           E N V I R O N M E N T      D I V I S I O N           *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *================================================================*
      *                  D A T A      D I V I S I O N                  *
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
      *
      *-----------------------------------------------------------------*
      *                  WORKING-STORAGE SECTION                        *
      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *-----------------------------------------------------------------*
      *                      LINKAGE SECTION                            *
      *-----------------------------------------------------------------*
       LINKAGE SECTION.
       01 WRK-ERROR-LOG.
          03 WRK-PROGRAM                         PIC X(08).
          03 WRK-ERROR-MSG                       PIC X(30).
          03 WRK-ERROR-CODE                      PIC X(30).
          03 WRK-ERROR-DATE                      PIC X(10).
          03 WRK-ERROR-TIME                      PIC X(08).
      *================================================================*
       PROCEDURE                       DIVISION  USING WRK-ERROR-LOG.
      *================================================================*
      *----------------------------------------------------------------*
       0000-MAIN-PROCESS               SECTION.
      *----------------------------------------------------------------*
           DISPLAY '**********************************'.
           DISPLAY '*  ABNORMAL END  PROGRAM CALLED  *'.
           DISPLAY '*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*'.
           DISPLAY '*DATE: 'WRK-ERROR-DATE'                *'.
           DISPLAY '*TIME: 'WRK-ERROR-TIME'                  *'.
           DISPLAY '*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*'.
           DISPLAY '*CALLER PROGRAM.....:'WRK-PROGRAM'    *'.
           DISPLAY '*ERROR CODE:                     *'.
           DISPLAY '* 'WRK-ERROR-CODE' *'.
           DISPLAY '*ERROR MESSAGE:                  *'.
           DISPLAY '* 'WRK-ERROR-MSG' *'.
           DISPLAY '**********************************'.

           STOP RUN.
      *----------------------------------------------------------------*
       0000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
