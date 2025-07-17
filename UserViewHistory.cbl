       IDENTIFICATION DIVISION.
       PROGRAM-ID. UserViewHistory.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "TRANSACTIONS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRANSACTION-ID
               FILE STATUS IS TRANS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANSACTION-ID  PIC 9(5).
           05 TRANSACTION-CDE PIC 9(8).
           05 TR-ACC-NO       PIC 9(16).
           05 TR-RECEIVER-NO  PIC 9(16).
           05 TR-TYPE         PIC 9(1).
           05 TR-DATE         PIC 9(8).
           05 TR-TIME         PIC 9(6).
           05 TR-AMOUNT       PIC 9(15).
           05 TR-DECIMAL      PIC 99.

       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG           PIC X VALUE 'N'.
           88  EOF               VALUE 'Y'.
           88  NOT-EOF           VALUE 'N'.
       01  TRANS-STATUS        PIC XX.
       01  TR-YEAR               PIC 9(4).
       01  TR-MONTH              PIC 9(2).
       01  TR-DAY                PIC 9(2).
       01  BALANCE               PIC 9(15)V99.
       01  FormatV               PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  HEADER-DISPLAYED      PIC X VALUE 'N'.
       01  T-TYPE                PIC X(9).
       01  TR-HOUR               PIC 9(2).
       01  TR-MINUTES            PIC 9(2).
       01  TR-SECONDS            PIC 9(2).


       LINKAGE SECTION.
       01  SEARCH-ACCOUNT-NO     PIC 9(16).

       PROCEDURE DIVISION USING SEARCH-ACCOUNT-NO.
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO HEADER-DISPLAYED

           OPEN INPUT TRANSACTION-FILE

           PERFORM UNTIL EOF
               READ TRANSACTION-FILE NEXT
                   AT END
                       SET EOF TO TRUE
                   NOT AT END

                       IF SEARCH-ACCOUNT-NO = TR-ACC-NO
                           PERFORM DISPLAY-HEADER
                           PERFORM DISPLAY-RESULT
                       END-IF
               END-READ
           END-PERFORM

           CLOSE TRANSACTION-FILE

           IF HEADER-DISPLAYED = 'Y'
               DISPLAY "+----------------------------------------------"
                       "-----------------------------------------------"
                       "-----------------------------------------------"
                       "----+"
               DISPLAY "All transaction records have been shown."
               MOVE 'N' TO HEADER-DISPLAYED
           ELSE
               DISPLAY "Transaction is not found for this account: "
                       SEARCH-ACCOUNT-NO
           END-IF

           GOBACK.

           DISPLAY-HEADER.
           IF HEADER-DISPLAYED = 'N'
               DISPLAY "+=============================================="
                       "==============================================="
                       "==============================================="
                       "====+"
               DISPLAY "|                                              "
                       "       All USER ATM TRANSACTION HISTORY        "
                       "                                               "
                       "    |"
               DISPLAY "+=============================================="
                       "==============================================="
                       "==============================================="
                       "====+"
               DISPLAY "| TRANSFER-ID  |    ACCOUNT-NO    |"
                       "   RECEIVER-NO    | TRANSFER-TYPE |"
                       "TRANSFER DATE   |TRANSFER TIME |"
                       "TRANSACTION-CODE |"
                       "     TRANSFER MONEY      |"
               DISPLAY "+----------------------------------------------"
                       "-----------------------------------------------"
                       "-----------------------------------------------"
                       "----+"
               MOVE 'Y' TO HEADER-DISPLAYED
           END-IF.

           DISPLAY-RESULT.
           EVALUATE TR-TYPE
               WHEN 1
                   MOVE "DEPOSIT  " TO T-TYPE
               WHEN 2
                   MOVE "WITHDRAW " TO T-TYPE
               WHEN 3
                   MOVE "TRANSFER " TO T-TYPE
               WHEN OTHER
                   MOVE "UNKNOWN  " TO T-TYPE
           END-EVALUATE

           MOVE TR-DATE(1:4) TO TR-YEAR
           MOVE TR-DATE(5:2) TO TR-MONTH
           MOVE TR-DATE(7:2) TO TR-DAY

           MOVE TR-TIME(1:2) TO TR-HOUR
           MOVE TR-TIME(3:2) TO TR-MINUTES
           MOVE TR-TIME(5:2) TO TR-SECONDS
           MOVE TR-AMOUNT TO BALANCE
           COMPUTE BALANCE = BALANCE + (TR-DECIMAL / 100)
           MOVE BALANCE TO FormatV

           DISPLAY "| " TRANSACTION-ID "        | "
                   TR-ACC-NO " | "
                   TR-RECEIVER-NO " | "
                   T-TYPE "     | "
                   TR-YEAR "-" TR-MONTH "-" TR-DAY "     | "
                   TR-HOUR ":" TR-MINUTES ":" TR-SECONDS "     | "
                   TRANSACTION-CDE"        |  "
                   FormatV " | ".
