      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Balance.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS USER-ACC-NO
               FILE STATUS IS USERS-STATUS.
      ******************************************************************
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

      ******************************************************************
       WORKING-STORAGE SECTION.
      *>  01  WS-USER-STATUS        PIC XX.
       01  USERS-STATUS          PIC XX.
       01  WS-BALANCE-DISPLAY    PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.

       01  CLEAN-BALANCE         PIC X(18).

       01  WS-DUMMY-KEY          PIC X.
      ******************************************************************
       LINKAGE SECTION.
       01  LK-USER-ID           PIC 9(16).
      ******************************************************************
       PROCEDURE DIVISION USING LK-USER-ID.
       MAIN-PROCEDURE.

           PERFORM BALANCE-INQUIRY
           GOBACK.


      *>        *>      1.Balance Display

           BALANCE-INQUIRY.

           DISPLAY "============================================"
           DISPLAY "             BALANCE INQUIRY               "
           DISPLAY "============================================"

      *>      MOVE LK-USER-ID TO USER-ACC-NO
           IF LK-USER-ID IS NUMERIC
               MOVE LK-USER-ID TO USER-ACC-NO
           ELSE
               DISPLAY "Invalid user ID passed: " LK-USER-ID
           GOBACK
           END-IF

           OPEN INPUT USERS-FILE

           READ USERS-FILE KEY IS USER-ACC-NO

           IF USERS-STATUS = "00"
      *>          DISPLAY USER-BALANCE
               MOVE USER-BALANCE TO WS-BALANCE-DISPLAY

           MOVE FUNCTION TRIM(WS-BALANCE-DISPLAY, LEADING) TO
           CLEAN-BALANCE

               DISPLAY "--------------------------------------------"
               DISPLAY "User ID        : " USER-ACC-NO
               DISPLAY "Name           : " USER-NAME
               DISPLAY "Current Balance: " CLEAN-BALANCE
               DISPLAY "--------------------------------------------"
           ELSE
               DISPLAY "User ID not found! Status: " USERS-STATUS
           END-IF

           CLOSE USERS-FILE
            DISPLAY "Press ENTER to return to User Menu..."
            ACCEPT WS-DUMMY-KEY

           GOBACK.


       END PROGRAM Balance.
