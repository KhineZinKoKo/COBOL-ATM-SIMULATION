       IDENTIFICATION DIVISION.
       PROGRAM-ID. View.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS USER-ACC-NO
               FILE STATUS IS USERS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD USERS-FILE.
           01 USER-RECORD.
           05 USER-ACC-NO     PIC 9(16).
           05 USER-NAME       PIC X(25).
           05 USER-NRC        PIC X(20).
           05 USER-ACC-TYPE   PIC X(10).
           05 USER-BALANCE    PIC 9(15)V99.
           05 USER-PIN        PIC X(4).
           05 USER-STATUS     PIC X(8).
           05 USER-REG-DATE   PIC X(10).

       WORKING-STORAGE SECTION.

       01 USERS-STATUS        PIC XX.
       01 END-FILE            PIC X VALUE 'N'.
       01 USER-CHOICE         PIC X.
       01 SEARCH-ACCNO        PIC 9(16).
       01 formatedMessage     PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.

       01 cleanBalance      PIC X(20).


       01 WS-DISPLAY-BALANCE      PIC Z(9),ZZZ,ZZ9.99.

       01 WS-FORMATTED-LINE.
           05 FILLER               PIC X VALUE "|".
           05 F-ACC-NO             PIC X(16).
           05 FILLER               PIC X VALUE "|".
           05 F-NAME               PIC X(27).
           05 FILLER               PIC X VALUE "|".
           05 F-STATUS             PIC X(8).
           05 FILLER               PIC X VALUE "|".
           05 F-TYPE               PIC X(12).
           05 FILLER               PIC X VALUE "|".
           05 F-BALANCE            PIC X(23).
           05 FILLER               PIC X VALUE "|".

       LINKAGE SECTION.
       01 Temp PIC 9.
       01 ADMIN-CHOICE PIC 9.
       PROCEDURE DIVISION USING TEMP, ADMIN-CHOICE.
        MOVE "0" TO USER-CHOICE
           IF ADMIN-CHOICE=2

           PERFORM UNTIL USER-CHOICE = "3"
               DISPLAY "=============================="
               DISPLAY " 1. View All Users"
               DISPLAY " 2. Search User by Account No"
               DISPLAY " 3. Back to ATM Menu... "
               DISPLAY "=============================="
               DISPLAY "Enter choice (1/2/3): "
               ACCEPT USER-CHOICE

               EVALUATE USER-CHOICE
                   WHEN "1"
                       PERFORM VIEW-ALL-USERS

                   WHEN "2"
                       PERFORM SEARCH-USER

                   WHEN "3"
                       DISPLAY "Returning to ATM Menu... "
                       GOBACK
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM
           END-IF

           GOBACK.

       VIEW-ALL-USERS.

           MOVE "N" TO END-FILE
           OPEN INPUT USERS-FILE

           IF USERS-STATUS NOT = "00"
               DISPLAY "ERROR: Unable to open USERS.DAT."
               CLOSE USERS-FILE
               EXIT PARAGRAPH
           END-IF


       DISPLAY "+----------------+----------"WITH NO ADVANCING
       DISPLAY"-----------------+--------+--"WITH NO ADVANCING
       DISPLAY"----------+-----------------------+"
       DISPLAY "| ACC-NO         | "WITH NO ADVANCING
       DISPLAY"         NAME             |"WITH NO ADVANCING
       DISPLAY"STATUS  | TYPE       |          BALANCE      |"
       DISPLAY "+----------------+----------"WITH NO ADVANCING
       DISPLAY"-----------------+--------+--"WITH NO ADVANCING
       DISPLAY"----------+-----------------------+"

       READ USERS-FILE
               AT END
                   MOVE "Y" TO END-FILE

           END-READ

           PERFORM UNTIL END-FILE = "Y"

               MOVE USER-ACC-NO     TO F-ACC-NO
               MOVE USER-NAME       TO F-NAME
               MOVE USER-STATUS     TO F-STATUS
               MOVE USER-ACC-TYPE   TO F-TYPE
               MOVE USER-BALANCE    TO WS-DISPLAY-BALANCE
               MOVE WS-DISPLAY-BALANCE TO F-BALANCE

               DISPLAY WS-FORMATTED-LINE

               READ USERS-FILE
                   AT END
                       MOVE "Y" TO END-FILE
               END-READ
           END-PERFORM

       DISPLAY "+----------------+----------"WITH NO ADVANCING
       DISPLAY"-----------------+--------+--"WITH NO ADVANCING
       DISPLAY"----------+-----------------------+"

           DISPLAY "       END OF USER LIST"
           CLOSE USERS-FILE.

       SEARCH-USER.
           DISPLAY "Enter Account Number to search: "
           ACCEPT SEARCH-ACCNO

           MOVE SEARCH-ACCNO TO USER-ACC-NO
           OPEN INPUT USERS-FILE

           READ USERS-FILE
               INVALID KEY
                   DISPLAY "Account not found."
               NOT INVALID KEY
                   MOVE USER-BALANCE TO formatedMessage   *> This auto-converts numeric to string with spaces
            MOVE FUNCTION TRIM(formatedMessage LEADING) TO cleanBalance

                   DISPLAY "Account No   : " USER-ACC-NO
                   DISPLAY "Full Name    : " USER-NAME
                   DISPLAY "NRC No       : " USER-NRC
                   DISPLAY "Acc Type     : " USER-ACC-TYPE
                   DISPLAY "Balance      : " cleanBalance
                   DISPLAY "Status       : " USER-STATUS
                   DISPLAY "Reg Date     : " USER-REG-DATE
           END-READ

           CLOSE USERS-FILE.
           END PROGRAM View.
