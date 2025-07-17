       IDENTIFICATION DIVISION.
       PROGRAM-ID. UpdateStatus.

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
       01  ACCNO             PIC 9(16).
       01  NEW-STATUS        PIC X(8).
       01  FILE-STATUS       PIC XX.
       01  WS-CONFIRM        PIC X(1).
       01  DUMMY             PIC X.
       01  WS-AGAIN          PIC X VALUE 'Y'.
       01  WS-STATUS-CLEAN PIC X(8).
       01  CHOICE PIC 9(1).

       LINKAGE SECTION.
       01 TEMP PIC 9.
       01 ADMIN-CHOICE    PIC 9.
       PROCEDURE DIVISION USING TEMP, ADMIN-CHOICE.
       MAIN-PROCEDURE.

           IF ADMIN-CHOICE = 3
           DISPLAY "==============================================="
           "================="
           DISPLAY "             UPDATE ACCOUNT STATUS "
           DISPLAY "==============================================="
           "================="

           OPEN I-O USER-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening USERS.DAT. Status: " FILE-STATUS
               GOBACK
           END-IF

           PERFORM UNTIL WS-AGAIN NOT = 'Y'
                   DISPLAY "Enter Account Number:"
                   ACCEPT ACCNO
                   MOVE ACCNO TO USER-ACC-NO
           DISPLAY "---------------------------------------------------"
           "------------"
           READ USER-FILE KEY IS USER-ACC-NO
           INVALID KEY
               DISPLAY "Account not found!"
               DISPLAY "-----------------------------------------------"
           "----------------"
           NOT INVALID KEY
               DISPLAY "==============================================="
           "================="
               DISPLAY "             Update User Account Information "
               DISPLAY "==============================================="
           "================="
               DISPLAY "Account NO      : " USER-ACC-NO

               PERFORM ACCEPT-INPUT


               EVALUATE CHOICE
                    WHEN 1
                    MOVE 'ACTIVE' TO NEW-STATUS
                     WHEN 2
                    MOVE 'INACTIVE' TO NEW-STATUS
                     WHEN 3
                    MOVE 'LOCKED' TO NEW-STATUS

                    WHEN OTHER
                        DISPLAY "INVALID"
                        DISPLAY " "
                        PERFORM ACCEPT-INPUT
               END-EVALUATE
               MOVE NEW-STATUS TO USER-STATUS
               DISPLAY "-----------------------------------------------"
           "----------------"
               DISPLAY "Are you sure you want to update the status "
               "of this account? (Y/N) "
               ACCEPT WS-CONFIRM
               DISPLAY "-----------------------------------------------"
           "----------------"
               IF WS-CONFIRM = "Y" OR WS-CONFIRM = "y"
                   REWRITE USER-RECORD
                       INVALID KEY
                           DISPLAY "Error updating record!"
                       NOT INVALID KEY
                          DISPLAY "Account status updated successfully!"
                          DISPLAY "------------------------------------"
           "---------------------------"
                   END-REWRITE
               ELSE
                   DISPLAY "Account status update cancelled by admin."
                   DISPLAY "------------------------------------"
           "---------------------------"
               END-IF
           END-READ

               DISPLAY "Do you want to update the status of another "
               "user account? (Y/N): "
               ACCEPT WS-AGAIN
               DISPLAY "------------------------------------"
           "---------------------------"
           END-PERFORM

               CLOSE USER-FILE
               MOVE 'Y' TO WS-AGAIN
               DISPLAY "Press ENTER to return to ATM Menu..."
               ACCEPT DUMMY
           END-IF

           GOBACK.

               ACCEPT-INPUT.
               DISPLAY "Current Status  : " USER-STATUS
               DISPLAY "Enter New Status ("
               "1.Active 2.Inactive 3.Locked):"
               ACCEPT CHOICE.
       END PROGRAM UpdateStatus.
