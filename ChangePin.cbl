       IDENTIFICATION DIVISION.
       PROGRAM-ID. ChangePin.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "USERS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACC-NO
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-RECORD.
           05 ACC-NO         PIC 9(16).
           05 USER-NAME      PIC X(25).
           05 USER-NRC       PIC X(20).
           05 ACC-TYPE       PIC X(10).
           05 ACC-BALANCE    PIC 9(15)V99.
           05 ACC-PIN        PIC 9(4).
           05 ACC-STATUS     PIC X(8).
           05 ACC-DATE       PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-OLD-PIN        PIC 9(4).
       01  WS-NEW-PIN        PIC 9(4).
       01  WS-CONFIRM-PIN    PIC 9(4).
       01  WS-CONFIRM        PIC X(1).
       01  FILE-STATUS       PIC XX.
       01  DUMMY             PIC X.

       LINKAGE SECTION.
       01  SEARCH-ACCOUNT-NO PIC 9(16).

       PROCEDURE DIVISION USING SEARCH-ACCOUNT-NO.
       MAIN-PROCEDURE.
           OPEN I-O USER-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening USERS.DAT. Status: " FILE-STATUS
               STOP RUN
           END-IF

           MOVE SEARCH-ACCOUNT-NO TO ACC-NO

           READ USER-FILE KEY IS ACC-NO
               INVALID KEY
                   CLOSE USER-FILE
                   ACCEPT DUMMY
                   GOBACK
           END-READ

           DISPLAY "Old Pin No: "ACC-PIN
           DISPLAY "Enter Old PIN: "
           ACCEPT WS-OLD-PIN
           IF WS-OLD-PIN NOT = ACC-PIN
               DISPLAY "Incorrect Old PIN."
               CLOSE USER-FILE
               DISPLAY "Press enter to exit."
               ACCEPT DUMMY
               GOBACK
           END-IF
           DISPLAY "Enter New PIN: "
           ACCEPT WS-NEW-PIN
           DISPLAY "Confirm New PIN: "
           ACCEPT WS-CONFIRM-PIN
           IF WS-NEW-PIN NOT = WS-CONFIRM-PIN
               DISPLAY "PINs do not match."
               CLOSE USER-FILE
               ACCEPT DUMMY
               GOBACK
           END-IF

           DISPLAY "Confirm PIN change (Y/N): "
           ACCEPT WS-CONFIRM
           IF WS-CONFIRM = "Y" OR WS-CONFIRM = "y"
               MOVE WS-NEW-PIN TO ACC-PIN
               REWRITE USER-RECORD
                   INVALID KEY
                       DISPLAY "Failed to update PIN."
                   NOT INVALID KEY
                       DISPLAY "PIN changed successfully."
               END-REWRITE
           ELSE
               DISPLAY "PIN change cancelled."
           END-IF

           CLOSE USER-FILE
           GOBACK.

       END PROGRAM ChangePin.
