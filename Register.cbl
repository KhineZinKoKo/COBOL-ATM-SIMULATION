       IDENTIFICATION DIVISION.
       PROGRAM-ID. Register.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS USER-ACC-NO
               FILE STATUS IS USERS-STATUS.

           *> SELECT TRANSACTIONS-FILE ASSIGN TO "TRANSACTIONS.DAT"
               *> ORGANIZATION IS LINE SEQUENTIAL
               *> ACCESS MODE IS SEQUENTIAL
               *> FILE STATUS IS TRANS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD USERS-FILE.
       01 USER-RECORD.
           05 USER-ACC-NO     PIC 9(16).
           05 USER-NAME       PIC X(25).
           05 USER-NRC        PIC X(20).
           05 USER-ACC-TYPE   PIC X(10).
           05 USER-BALANCE    PIC 9(15)V99.
           05 USER-PIN        PIC 9(4).
           05 USER-STATUS     PIC X(8).
           05 USER-REG-DATE   PIC X(10).

       *> FD TRANSACTIONS-FILE.
       *> 01 TRANS-RECORD        PIC X(100).

       WORKING-STORAGE SECTION.
       01 USERS-STATUS        PIC XX.
       01 TRANS-STATUS        PIC XX.

       01 WS-CHOICE           PIC X.
       01 WS-MANUAL-ACCNO     PIC X(16).
       01 WS-DATE             PIC 9(8).
       01 WS-YEAR             PIC 9(4).
       01 WS-MONTH            PIC 9(2).
       01 WS-DAY              PIC 9(2).
       01 WS-REG-DATE         PIC X(10).
       01 WS-TIME    PIC 9(6).    *> Format: hhmmss
       01 ACC-COUNTER        PIC 9(4) VALUE 0.
       01 WS-AUTO-ACCNO       PIC 9(16).  *> Combined result: YYYYMMDDHHMMSS
       01 ADD-ANOTHER         PIC X VALUE 'Y'.
       01  FormatV            PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.

       77 CLEAN-NRC       PIC X(18).
       77 REST-NRC        PIC X(15).          *> Needed for UNSTRING
       77 REGION-CODE     PIC X(2).
       77 TOWNSHIP-CODE   PIC X(6).
       77 OPEN-BRACKET    PIC X.
       77 NATIONALITY     PIC X.
       77 CLOSE-BRACKET   PIC X.
       77 NRC-NUMBER      PIC X(6).
       77 NRC-LENGTH      PIC 99.
       77 VALID-NRC       PIC X VALUE "N".

       LINKAGE SECTION.
       01 Temp PIC 9.
       01 ADMIN-CHOICE PIC 9.
       PROCEDURE DIVISION USING TEMP,ADMIN-CHOICE.
       IF ADMIN-CHOICE=1
            MOVE "Y" TO ADD-ANOTHER
       OPEN I-O USERS-FILE
       PERFORM UNTIL ADD-ANOTHER NOT = 'Y'
       DISPLAY "=== REGISTER NEW USER ==="
       DISPLAY "Enter A for Auto Account Number,"WITH NO ADVANCING
       DISPLAY" M for Manual Account Number: "
       ACCEPT WS-CHOICE

           IF WS-CHOICE = "M" OR WS-CHOICE = "m"
                   DISPLAY "Enter Account Number : "
                   ACCEPT WS-MANUAL-ACCNO
                   MOVE WS-MANUAL-ACCNO TO USER-ACC-NO
           ELSE

             ACCEPT WS-TIME FROM TIME
             ACCEPT WS-DATE FROM DATE
             STRING WS-TIME DELIMITED SIZE
                    WS-DATE DELIMITED SIZE
           INTO WS-AUTO-ACCNO
           MOVE WS-AUTO-ACCNO TO USER-ACC-NO
           END-IF

       READ USERS-FILE
           INVALID KEY

                   DISPLAY "Enter Full Name       : "
                   ACCEPT USER-NAME
                   MOVE FUNCTION UPPER-CASE(USER-NAME) TO USER-NAME
       MOVE "N" TO VALID-NRC
       PERFORM UNTIL VALID-NRC = "Y"
                   DISPLAY "Enter NRC No          : "
                   ACCEPT USER-NRC

       MOVE FUNCTION TRIM(USER-NRC) TO CLEAN-NRC
       MOVE FUNCTION UPPER-CASE(CLEAN-NRC) TO CLEAN-NRC

        UNSTRING CLEAN-NRC
        DELIMITED BY "/"
        INTO REGION-CODE REST-NRC

        IF FUNCTION LENGTH(REST-NRC) = 15
        MOVE REST-NRC(1:6)   TO TOWNSHIP-CODE
        MOVE REST-NRC(7:1)   TO OPEN-BRACKET
        MOVE REST-NRC(8:1)   TO NATIONALITY
        MOVE REST-NRC(9:1)   TO CLOSE-BRACKET
        MOVE REST-NRC(10:6)  TO NRC-NUMBER

        IF OPEN-BRACKET = "(" AND
       CLOSE-BRACKET = ")" AND
       TOWNSHIP-CODE IS ALPHABETIC AND
       NATIONALITY = "N" AND
       NRC-NUMBER IS NUMERIC

       MOVE "Y" TO VALID-NRC
       ELSE
       DISPLAY "NRC must follow format like "WITH NO ADVANCING
       DISPLAY"12/KAYAYA(N)123456"
       END-IF
       ELSE
       DISPLAY "NRC must follow format like "WITH NO ADVANCING
       DISPLAY"12/KAYAYA(N)123456 — exactly 15 characters after '/'"
       END-IF
       END-PERFORM

                   DISPLAY "Enter Account Type(SPECIAL/SAVINGS)  : "
                   ACCEPT USER-ACC-TYPE

                   DISPLAY "Enter Initial Balance : "
                   ACCEPT USER-BALANCE
                   MOVE USER-BALANCE TO Formatv

                   MOVE 1234 TO USER-PIN

                   MOVE "ACTIVE" TO USER-STATUS

                      MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE
                       MOVE WS-DATE(1:4) TO WS-YEAR
                       MOVE WS-DATE(5:2) TO WS-MONTH
                       MOVE WS-DATE(7:2) TO WS-DAY
                       STRING WS-YEAR DELIMITED SIZE
                              "-" DELIMITED SIZE
                              WS-MONTH DELIMITED SIZE
                              "-" DELIMITED SIZE
                              WS-DAY DELIMITED SIZE
                           INTO WS-REG-DATE
                       MOVE WS-REG-DATE TO USER-REG-DATE

                       WRITE USER-RECORD
                  *> INVALID KEY
                       *> DISPLAY "Write failed. Could not register user."
                               *> CLOSE USERS-FILE
                               *> GOBACK
                       *> END-WRITE

                       *> OPEN EXTEND TRANSACTIONS-FILE
                  *> STRING "REGISTERED: ", USER-ACC-NO, " | ", USER-NAME,
                              *> " | ", USER-REG-DATE
                           *> DELIMITED BY SIZE INTO TRANS-RECORD
                       *> WRITE TRANS-RECORD
                       *> CLOSE TRANSACTIONS-FILE

                       DISPLAY "User registered successfully."

                   NOT INVALID KEY
              DISPLAY "Duplicate Account Number. Registration failed."

               END-READ
            DISPLAY "Register another user? (Y/N): "
               ACCEPT ADD-ANOTHER

               MOVE FUNCTION UPPER-CASE(ADD-ANOTHER) TO ADD-ANOTHER

           END-PERFORM
           CLOSE USERS-FILE


           END-IF
           GOBACK.

           END PROGRAM Register.
