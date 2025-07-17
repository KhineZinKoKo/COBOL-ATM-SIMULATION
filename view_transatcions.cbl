       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadTransactions.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "TRANSACTIONS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TRANSACTION-ID
               FILE STATUS IS TRANS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANSACTION-ID  PIC 9(5).
           05 TRANSACTION-CDE PIC 9(8).
           05 TR-ACC-NO       PIC 9(16).
           05 TR-RECEIVER-NO  PIC 9(16).
           05 TR-TYPE         PIC 9(1).
           05 TR-DATE         PIC 9(8).
           05 TR-TIME         PIC 9(6).
           05 TR-AMOUNT       PIC 9(15).
           05 TR-DECIMAL      PIC 9(2).

       WORKING-STORAGE SECTION.
       01 TRANS-STATUS        PIC XX.
       01 EOF-SWITCH          PIC X VALUE "N".

       PROCEDURE DIVISION.

       MAIN-PARA.
           OPEN INPUT TRANSACTION-FILE

           IF TRANS-STATUS NOT = "00"
               DISPLAY "[ERROR] Cannot open TRANSACTIONS.dat"
               STOP RUN
           END-IF

           PERFORM UNTIL EOF-SWITCH = "Y"
               READ TRANSACTION-FILE NEXT
                   AT END
                       MOVE "Y" TO EOF-SWITCH
                   NOT AT END
                       PERFORM DISPLAY-RECORD
               END-READ
           END-PERFORM

           CLOSE TRANSACTION-FILE
           STOP RUN.

       DISPLAY-RECORD.
           DISPLAY "-----------------------------"
           DISPLAY "Transaction Index:     " TRANSACTION-ID
           DISPLAY "Transaction Unique Id: " TRANSACTION-CDE
           DISPLAY "Sender Account:        " TR-ACC-NO
           DISPLAY "Receiver Account:      " TR-RECEIVER-NO
           DISPLAY "Type:                  " TR-TYPE
           DISPLAY "Date:                  " TR-DATE
           DISPLAY "Time:                  " TR-TIME
           DISPLAY "Amount:                " TR-AMOUNT"."TR-DECIMAL
           DISPLAY "-----------------------------".
