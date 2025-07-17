       IDENTIFICATION DIVISION.
       PROGRAM-ID. Monthly_Daily_Report.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "TRANSACTIONS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TRANSACTION-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT REPORT-FILE ASSIGN TO DYNAMIC REPORT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CSV-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANSACTION-ID     PIC 9(5).
           05 TRANSACTION-CODE   PIC 9(8).
           05 ACCOUNT-NO         PIC X(16).
           05 RECEIVER-NO        PIC X(16).
           05 TRANSFER-TYPE      PIC 9(1).
           05 TRANSFER-DATE      PIC 9(8).
           05 TRANSFER-TIME      PIC 9(6).
           05 TRANSFER-MONEY     PIC 9(15).
           05 TRANSFER-DECIMAL   PIC 9(2).

       FD REPORT-FILE.
       01 REPORT-LINE            PIC X(200).

       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS         PIC XX.
       01 WS-CSV-STATUS          PIC XX.
       01 WS-END-FILE            PIC X VALUE "N".
           88 END-FILE           VALUE "Y".
           88 NOT-END-FILE       VALUE "N".


       01 WS-REPORT-CHOICE       PIC X(1).
           88 DAILY-REPORT       VALUE 'D' 'd'.
           88 MONTHLY-REPORT     VALUE 'M' 'm'.

       01 WS-REPORT-DATE         PIC X(8).
       01 WS-REPORT-MONTH        PIC X(6).
       01 REPORT-FILENAME        PIC X(30).

       01 CURRENT-ACC            PIC X(16) VALUE SPACES.
       01 DISPLAY-AMOUNT         PIC ZZZZZZZZZZZ9.99.
       01 DISPLAY-AMOUNT1        PIC ZZZZZZZZZZZ9.99.
       01 DISPLAY-AMOUNT2        PIC ZZZZZZZZZZZ9.99.
       01 DISPLAY-AMOUNT3        PIC ZZZZZZZZZZZ9.99.
       01 WS-AMOUNT-NUMERIC      PIC 9(15)V99.

       01 ACC-TOTAL-DEPOSIT      PIC 9(15)V99 VALUE 0.
       01 ACC-TOTAL-WITHDRAW     PIC 9(15)V99 VALUE 0.
       01 ACC-TOTAL-TRANSFER     PIC 9(15)V99 VALUE 0.

       01 GTOTAL-DEPOSIT         PIC 9(15)V99 VALUE 0.
       01 GTOTAL-WITHDRAW        PIC 9(15)V99 VALUE 0.
       01 GTOTAL-TRANSFER        PIC 9(15)V99 VALUE 0.
       01 CHECK PIC 9(1).

       01 WS-LAST-ACC            PIC X(16) VALUE SPACES.

       01 RECORDS-TABLE.
           05 RECORD-ENTRY OCCURS 1000 TIMES.
              10 R-ACCOUNT-NO     PIC X(16).
              10 R-TRANSFER-TYPE  PIC 9.
              10 R-DATE           PIC X(8).
              10 R-AMOUNT         PIC 9(15)V99.

       01 RECORD-COUNT           PIC 9(4) VALUE 0.
       01 I                      PIC 9(4).
       01 J                      PIC 9(4).
       01 TEMP-ACC               PIC X(16).
       01 TEMP-TYPE              PIC 9.
       01 TEMP-DATE              PIC X(8).
       01 TEMP-AMOUNT            PIC 9(15)V99.
       LINKAGE SECTION.
       01 Temp PIC 9.
       01 ADMIN-CHOICE PIC 9.
       PROCEDURE DIVISION USING TEMP,ADMIN-CHOICE.
       MAIN-PROCEDURE.
       IF ADMIN-CHOICE=6

           PERFORM Monthly-Daily.
           GOBACK.
       Monthly-Daily.
           DISPLAY " "
           DISPLAY "                ATM TRANSACTION REPORT"
           DISPLAY "-----------------------------------------"
           "---------------"
           DISPLAY "Enter 'D' for Daily Report. "
           DISPLAY "Enter 'M' for Monlhty Report. "
           DISPLAY "Enter Other key for ATM Menu."

           ACCEPT WS-REPORT-CHOICE

           EVALUATE TRUE

               WHEN DAILY-REPORT
                   DISPLAY " "
                   DISPLAY "Enter Report Date (YYYYMMDD): "
                   ACCEPT WS-REPORT-DATE
                   STRING "REPORT-" WS-REPORT-DATE ".csv"
                       DELIMITED BY SIZE INTO REPORT-FILENAME
               WHEN MONTHLY-REPORT
                   DISPLAY " "
                   DISPLAY "Enter Report Month (YYYYMM): "
                   ACCEPT WS-REPORT-MONTH
                   STRING "REPORT-" WS-REPORT-MONTH ".csv"
                   DELIMITED BY SIZE INTO REPORT-FILENAME
               WHEN OTHER
                   DISPLAY "Returning to ATM Menu..."
                   CLOSE TRANSACTION-FILE
                   GOBACK *> To Go ADMIN MENU
           END-EVALUATE

           OPEN INPUT TRANSACTION-FILE

           PERFORM UNTIL END-FILE
               READ TRANSACTION-FILE
                   AT END SET END-FILE TO TRUE
                   NOT AT END PERFORM STORE-IN-TABLE
           END-PERFORM

           CLOSE TRANSACTION-FILE

           PERFORM SORT-TABLE
           PERFORM GENERATE-REPORT.

       STORE-IN-TABLE.

           IF (DAILY-REPORT AND TRANSFER-DATE = WS-REPORT-DATE) OR
              (MONTHLY-REPORT AND TRANSFER-DATE(1:6) = WS-REPORT-MONTH)
               ADD 1 TO RECORD-COUNT
               MOVE ACCOUNT-NO TO R-ACCOUNT-NO(RECORD-COUNT)
               MOVE TRANSFER-TYPE TO R-TRANSFER-TYPE(RECORD-COUNT)
               MOVE TRANSFER-DATE TO R-DATE(RECORD-COUNT)
               COMPUTE WS-AMOUNT-NUMERIC =
                    FUNCTION NUMVAL(TRANSFER-MONEY)
                  + FUNCTION NUMVAL(TRANSFER-DECIMAL) / 100
               MOVE WS-AMOUNT-NUMERIC TO R-AMOUNT(RECORD-COUNT)
               ADD 1 TO CHECK
           ELSE
               DISPLAY "========================="
               DISPLAY "DATA NOT FOUND........."
               DISPLAY "========================="
               PERFORM MAIN-PROCEDURE
           END-IF.

       SORT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= RECORD-COUNT
               PERFORM VARYING J FROM I BY 1 UNTIL J > RECORD-COUNT
                   IF R-ACCOUNT-NO(I) > R-ACCOUNT-NO(J)
                       MOVE R-ACCOUNT-NO(I) TO TEMP-ACC
                       MOVE R-TRANSFER-TYPE(I) TO TEMP-TYPE
                       MOVE R-DATE(I) TO TEMP-DATE
                       MOVE R-AMOUNT(I) TO TEMP-AMOUNT

                       MOVE R-ACCOUNT-NO(J) TO R-ACCOUNT-NO(I)
                       MOVE R-TRANSFER-TYPE(J) TO R-TRANSFER-TYPE(I)
                       MOVE R-DATE(J) TO R-DATE(I)
                       MOVE R-AMOUNT(J) TO R-AMOUNT(I)

                       MOVE TEMP-ACC TO R-ACCOUNT-NO(J)
                       MOVE TEMP-TYPE TO R-TRANSFER-TYPE(J)
                       MOVE TEMP-DATE TO R-DATE(J)
                       MOVE TEMP-AMOUNT TO R-AMOUNT(J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       GENERATE-REPORT.

           IF CHECK NOT EQUAL 0
           OPEN OUTPUT REPORT-FILE

           PERFORM WRITE-CSV-TITLE-AND-HEADER
           DISPLAY " "
           DISPLAY "+================================================="
           "=======================+"
           IF DAILY-REPORT
           DISPLAY "|                     USER TRANSACTION "
           "DAILY-REPORT                      |"
           ELSE
           DISPLAY "|                     USER TRANSACTION "
           "MONTHLY-REPORT                    |"
           END-IF
           DISPLAY "+--------------------------------------------------"
           "----------------------+"
           DISPLAY "|    ACCOUNT NO    |     DEPOSIT     |   "
           "WITHDRAW      |    TRANSFER     |"
           DISPLAY "+-------------------------------------------------"
           "-----------------------+"

           MOVE SPACES TO WS-LAST-ACC
           MOVE 0 TO ACC-TOTAL-DEPOSIT
           MOVE 0 TO ACC-TOTAL-WITHDRAW
           MOVE 0 TO ACC-TOTAL-TRANSFER

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RECORD-COUNT
               IF WS-LAST-ACC NOT = SPACES AND
                  R-ACCOUNT-NO(I) NOT = WS-LAST-ACC
                   PERFORM WRITE-SUMMARY
                   MOVE 0 TO ACC-TOTAL-DEPOSIT
                   MOVE 0 TO ACC-TOTAL-WITHDRAW
                   MOVE 0 TO ACC-TOTAL-TRANSFER
               END-IF

               MOVE R-ACCOUNT-NO(I) TO WS-LAST-ACC

               EVALUATE R-TRANSFER-TYPE(I)
                   WHEN 1
                       ADD R-AMOUNT(I) TO ACC-TOTAL-DEPOSIT
                       ADD R-AMOUNT(I) TO GTOTAL-DEPOSIT
                   WHEN 2
                       ADD R-AMOUNT(I) TO ACC-TOTAL-WITHDRAW
                       ADD R-AMOUNT(I) TO GTOTAL-WITHDRAW
                   WHEN 3
                       ADD R-AMOUNT(I) TO ACC-TOTAL-TRANSFER
                       ADD R-AMOUNT(I) TO GTOTAL-TRANSFER
               END-EVALUATE
           END-PERFORM

           PERFORM WRITE-SUMMARY

           DISPLAY "+----------------------------------------"
           "--------------------------------+"
           MOVE GTOTAL-DEPOSIT TO DISPLAY-AMOUNT1
           MOVE GTOTAL-WITHDRAW TO DISPLAY-AMOUNT2
           MOVE GTOTAL-TRANSFER TO DISPLAY-AMOUNT3
           DISPLAY "|   GRAND TOTAL    | "
           DISPLAY-AMOUNT1 " | " DISPLAY-AMOUNT2 " | "
           DISPLAY-AMOUNT3 " |"
           DISPLAY "+================================="
           "=======================================+"
           DISPLAY "Excel file is successfully created..."
           STRING
               "GRAND TOTAL,"
               DISPLAY-AMOUNT1 DELIMITED BY SIZE ","
               DISPLAY-AMOUNT2 DELIMITED BY SIZE ","
               DISPLAY-AMOUNT3 DELIMITED BY SIZE
               INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE

           CLOSE REPORT-FILE

           END-IF.

       WRITE-SUMMARY.
           MOVE ACC-TOTAL-DEPOSIT TO DISPLAY-AMOUNT1
           MOVE ACC-TOTAL-WITHDRAW TO DISPLAY-AMOUNT2
           MOVE ACC-TOTAL-TRANSFER TO DISPLAY-AMOUNT3

           DISPLAY "| " WS-LAST-ACC " | " DISPLAY-AMOUNT1 " | "
           DISPLAY-AMOUNT2 " | " DISPLAY-AMOUNT3 " |"

           STRING
               WS-LAST-ACC DELIMITED BY SIZE
               "," DISPLAY-AMOUNT1 DELIMITED BY SIZE
               "," DISPLAY-AMOUNT2 DELIMITED BY SIZE
               "," DISPLAY-AMOUNT3 DELIMITED BY SIZE
               INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE.

       WRITE-CSV-TITLE-AND-HEADER.
           IF DAILY-REPORT
               STRING
                   "ATM Transaction Report - Daily Report"
                   DELIMITED BY SIZE
                   INTO REPORT-LINE
               END-STRING
               WRITE REPORT-LINE
               STRING
                   "Report Date: " WS-REPORT-DATE
                   DELIMITED BY SIZE
                   INTO REPORT-LINE
               END-STRING
               WRITE REPORT-LINE
           ELSE
               STRING
                   "ATM Transaction Report - Monthly Report"
                   DELIMITED BY SIZE
                   INTO REPORT-LINE

               END-STRING
               WRITE REPORT-LINE
               STRING
                   "Report Month: " WS-REPORT-MONTH
                   DELIMITED BY SIZE
                   INTO REPORT-LINE
               END-STRING
               WRITE REPORT-LINE
           END-IF

           WRITE REPORT-LINE FROM "ACCOUNT,DEPOSIT,WITHDRAW,TRANSFER".
