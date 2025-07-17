       IDENTIFICATION DIVISION.
       PROGRAM-ID. ResetPIN.

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
       01  WS-ACCNO              PIC 9(16).
       01  WS-NEW-PIN            PIC 9(4) VALUE 1234.
       01  WS-CONFIRM            PIC X(1).
       01  FILE-STATUS           PIC XX.
       01  WS-AGAIN              PIC X VALUE 'Y'.
       01  DUMMY                 PIC X.

       LINKAGE SECTION.
       01 TEMP       PIC 9.
       01 ADMIN-CHOICE    PIC 9.

       PROCEDURE DIVISION USING TEMP, ADMIN-CHOICE.
       MAIN-PROCEDURE.

           IF ADMIN-CHOICE = 7

           DISPLAY "==================================================="
           "============="
           DISPLAY "                        RESET USER PIN "
           DISPLAY "==================================================="
           "============="

           OPEN I-O USER-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening USERS.DAT. Status: " FILE-STATUS
               GOBACK
           END-IF

           PERFORM UNTIL WS-AGAIN NOT = 'Y' AND WS-AGAIN NOT = 'y'
           DISPLAY "Enter Account Number: "
           ACCEPT WS-ACCNO
           MOVE WS-ACCNO TO USER-ACC-NO

           READ USER-FILE KEY IS USER-ACC-NO
           INVALID KEY
               DISPLAY "Account not found!"
           DISPLAY "---------------------------------------------------"
           "-------------"
           NOT INVALID KEY
               IF FUNCTION UPPER-CASE(USER-STATUS) = "LOCKED"
           DISPLAY "PIN reset is not allowed: Account is LOCKED."
           DISPLAY "---------------------------------------------------"
           "-------------"
               ELSE
           DISPLAY "==================================================="
           "============="
           DISPLAY "             Reset User Account PIN Information    "
           DISPLAY "==================================================="
           "============="
                   DISPLAY "Account No    : " USER-ACC-NO
                   DISPLAY "User Name     : " USER-NAME
                   DISPLAY "Status        : " USER-STATUS
                   DISPLAY "Current PIN   : " USER-PIN
                   DISPLAY "PIN will be reset to default: 1234"
           DISPLAY "---------------------------------------------------"
           "-------------"

            DISPLAY "Are you sure you want to reset the PIN? (Y/N): "
                   ACCEPT WS-CONFIRM
           DISPLAY "---------------------------------------------------"
           "-------------"
                       IF WS-CONFIRM = "Y" OR WS-CONFIRM = "y"
                       MOVE 1234 TO USER-PIN
                       REWRITE USER-RECORD
                           INVALID KEY
                           DISPLAY "Error: Unable to update PIN!"
           DISPLAY "---------------------------------------------------"
           "-------------"
                           NOT INVALID KEY
                       DISPLAY "PIN reset successfully to 1234!"
           DISPLAY "---------------------------------------------------"
           "-------------"
                       END-REWRITE
                   ELSE
                       DISPLAY "PIN reset cancelled by admin."
           DISPLAY "---------------------------------------------------"
           "-------------"
                   END-IF
               END-IF
           END-READ

            DISPLAY "Do you want to reset PIN another user "
            "account? (Y/N): "
               ACCEPT WS-AGAIN
           DISPLAY "---------------------------------------------------"
           "-------------"
           END-PERFORM

           CLOSE USER-FILE
           MOVE 'Y' TO WS-AGAIN
           DISPLAY "Press ENTER to return to ATM Menu..."
           ACCEPT DUMMY
           END-IF

           GOBACK.
       END PROGRAM ResetPIN.
