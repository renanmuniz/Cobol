           EXEC SQL DECLARE TUTORIAL.TB_CUSTOMER TABLE
           ( CPF               CHAR(15) NOT NULL,
             NAME              CHAR(50) NOT NULL,
             BIRTH             CHAR(10) NOT NULL,
             STREET            CHAR(50) NOT NULL,
             HNUMBER           CHAR(10) NOT NULL,
             OTHER_INFO        CHAR(15),
             CITY              CHAR(40) NOT NULL,
             STATE             CHAR(02) NOT NULL,
             COUNTRY           CHAR(30) NOT NULL,
             TIMEREGINC        TIMESTAMP
           ) END-EXEC.
       01  BTB-CUSTOMER.
           05 CPF        PIC X(15).
           05 NAME       PIC X(50).
           05 BIRTH      PIC X(10).
           05 STREET     PIC X(50).
           05 HNUMBER    PIC X(10).
           05 OTHER-INFO PIC X(15).
           05 CITY       PIC X(40).
           05 STATE      PIC X(02).
           05 COUNTRY    PIC X(30).
           05 TIMEREGINC PIC X(26).
