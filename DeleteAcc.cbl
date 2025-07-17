
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DeleteAcc.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS USER-ACC-NO
               FILE STATUS IS FILE-STATUS.


       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-RECORD.
           05 USER-ACC-NO     PIC 9(16).
           05 USER-NAME       PIC X(25).
           05 USER-NRC        PIC X(20).
           05 USER-ACC-TYPE   PIC X(10).
           05 USER-BALANCE    PIC 9(15)V99.
           05 USER-PIN        PIC 9(4).
           05 USER-STATUS     PIC X(8).
           05 USER-REG-DATE   PIC X(10).


       WORKING-STORAGE SECTION.
       01  WS-ACCNO          PIC 9(16).
       01  WS-CONFIRM        PIC X(1).
       01  WS-BAL-NUMERIC    PIC 9(15).
       01  FILE-STATUS       PIC XX.
       01  FORMATTED-BAL     PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  DUMMY             PIC X.
       01  cleanBalance      PIC X(20).
       01  WS-AGAIN          PIC X VALUE 'Y'.


       LINKAGE SECTION.
       01 TEMP PIC 9.
       01 ADMIN-CHOICE    PIC 9.
       PROCEDURE DIVISION USING TEMP, ADMIN-CHOICE.
       MAIN-PROCEDURE.


            IF ADMIN-CHOICE = 4

           DISPLAY "==================================================="
           "============="
           DISPLAY "           DELETE USER ACCOUNT           "
           DISPLAY "==================================================="
           "============="
           OPEN I-O USER-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening USERS.DAT. Status: " FILE-STATUS
               GOBACK
           END-IF


           PERFORM UNTIL  WS-AGAIN NOT = 'Y'
             DISPLAY "Enter Account Number : "
             ACCEPT WS-ACCNO
             MOVE WS-ACCNO TO USER-ACC-NO
           DISPLAY "---------------------------------------------------"
           "-------------"

           READ USER-FILE KEY IS USER-ACC-NO
               INVALID KEY
                   DISPLAY "Account not found!"
           DISPLAY "---------------------------------------------------"
           "-------------"
               NOT INVALID KEY
               MOVE USER-BALANCE TO WS-BAL-NUMERIC
               MOVE USER-BALANCE TO FORMATTED-BAL

           IF USER-BALANCE > 0
           MOVE FUNCTION TRIM(FORMATTED-BAL LEADING) TO cleanBalance

           DISPLAY "Cannot delete: Account has balance " cleanBalance
           DISPLAY "---------------------------------------------------"
           "-------------"
           ELSE
           MOVE FUNCTION TRIM(FORMATTED-BAL LEADING) TO cleanBalance
           DISPLAY "==================================================="
           "============="
           DISPLAY "             Delete User Account Information       "
           DISPLAY "==================================================="
           "============="
           DISPLAY "Account NO   : " USER-ACC-NO
           DISPLAY "User Name    : " USER-NAME
           DISPLAY "Status       : " USER-STATUS
           DISPLAY "Balance      : " cleanBalance
           DISPLAY "---------------------------------------------------"
           "-------------"
           DISPLAY "Are you sure you want to PERMANENTLY DELETE this"
           " account? (Y/N): "
           ACCEPT WS-CONFIRM
           DISPLAY "---------------------------------------------------"
           "-------------"
           IF WS-CONFIRM = "Y" OR WS-CONFIRM = "y"
           DELETE USER-FILE
               INVALID KEY
                   DISPLAY "Error deleting record!"
               NOT INVALID KEY
                   DISPLAY "User account permanently deleted!"
           DISPLAY "---------------------------------------------------"
           "-------------"
           END-DELETE
            ELSE
           DISPLAY "Account deletion cancelled by admin."
           DISPLAY "---------------------------------------------------"
           "-------------"
           END-IF
           END-IF
           END-READ

           DISPLAY "Do you want to delete another user account? (Y/N): "
           ACCEPT  WS-AGAIN
           MOVE FUNCTION UPPER-CASE(WS-AGAIN) TO WS-AGAIN
           DISPLAY "---------------------------------------------------"
           "-------------"
           END-PERFORM

               CLOSE USER-FILE
               MOVE 'Y' TO  WS-AGAIN
               DISPLAY "Press ENTER to return to ATM Menu..."
               ACCEPT DUMMY
           END-IF

           GOBACK.
           END PROGRAM DeleteAcc.
