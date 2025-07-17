       IDENTIFICATION DIVISION.
       PROGRAM-ID. DepositMoney.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACC-NO
               FILE STATUS IS FILE-STATUS.

           SELECT TRANSACTION-FILE ASSIGN TO "TRANSACTIONS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRANSACTION-ID
               ALTERNATE RECORD KEY IS TRANSACTION-CDE
               FILE STATUS IS TRANS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD USER-FILE.
       01 USER-RECORD.
           05 ACC-NO         PIC 9(16).
           05 USER-NAME      PIC X(25).
           05 USER-NRC       PIC X(20).
           05 ACC-TYPE       PIC X(10).
           05 ACC-BALANCE    PIC 9(15)V99.
           05 ACC-PIN        PIC X(4).
           05 ACC-STATUS     PIC X(6).
           05 ACC-DATE       PIC X(10).

       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANSACTION-ID  PIC 9(5).
           05 TRANSACTION-CDE PIC 9(8).
           05 TR-ACC-NO       PIC 9(16).
           05 TR-RECEIVER-NO  PIC 9(16).
           05 TR-TYPE         PIC 9(1).
           05 TR-DATE         PIC 9(8).
           05 TR-TIME         PIC 9(6).
           05 TR-AMOUNT       PIC 9(15)V99.

       WORKING-STORAGE SECTION.
       01 WS-RANDOM         USAGE COMP-1 VALUE 0.
       01 UNIQUE-ID         PIC 9(8).
       01 FILE-STATUS       PIC XX.
       01 TRANS-STATUS      PIC XX.
       01 LAST-TRANSACTION-ID PIC 9(5) VALUE ZEROS.
       01 WS-TODAY            PIC 9(8).
       01 WS-DATE             PIC X(10).
       01 WS-TIME             PIC 9(8).
       01 WS-CUR-TIME         PIC X(6).
       01 UNIQUE-FOUND        PIC X VALUE 'N'.
       01 FORMAT-BALANCE      PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.ZZ.

       01 TEMP-BALANCE        PIC 9(15)V99.

       LINKAGE SECTION.
       01 LNK-ACCNO           PIC 9(16).
       01 LNK-AMOUNT          PIC 9(15)V99.
       01 RTN-AMOUNT          PIC 9(15)V99.

       PROCEDURE DIVISION USING LNK-ACCNO LNK-AMOUNT RTN-AMOUNT.

       MAIN-PARAGRAPH.
           OPEN I-O USER-FILE
           MOVE LNK-ACCNO TO ACC-NO
           READ USER-FILE
               INVALID KEY
                   DISPLAY "[WARNING]: Account not found."
                   CLOSE USER-FILE
                   STOP RUN
           END-READ

           ADD LNK-AMOUNT TO ACC-BALANCE
           REWRITE USER-RECORD
               INVALID KEY
                   DISPLAY "[ERROR]: Failed to update balance."
                   CLOSE USER-FILE
                   STOP RUN
           END-REWRITE
           CLOSE USER-FILE
           PERFORM WRITE-TRANSACTION

           MOVE ACC-BALANCE TO FORMAT-BALANCE
           WRITE USER-RECORD
           DISPLAY "============================================"
           DISPLAY "Account ID     : " LNK-ACCNO
           DISPLAY "Transaction ID : " TRANSACTION-CDE
           MOVE LNK-AMOUNT TO FORMAT-BALANCE
           DISPLAY "Deposit Amount : " FORMAT-BALANCE
           MOVE ACC-BALANCE TO FORMAT-BALANCE
           DISPLAY "New Balance    : " FORMAT-BALANCE
           DISPLAY "--------------------------------------------"
           DISPLAY "Deposit successful."
           DISPLAY "============================================"
           MOVE ACC-BALANCE TO RTN-AMOUNT
           GOBACK.

       GENERATE-RANDOM.
           COMPUTE WS-RANDOM = FUNCTION RANDOM
           COMPUTE UNIQUE-ID = (WS-RANDOM * 89999999) + 10000000.

       WRITE-TRANSACTION.

           ACCEPT WS-TODAY FROM DATE
           ACCEPT WS-CUR-TIME FROM TIME

           OPEN I-O TRANSACTION-FILE

           PERFORM UNTIL UNIQUE-FOUND = 'Y'
               PERFORM GENERATE-RANDOM
               MOVE UNIQUE-ID TO TRANSACTION-CDE

               START TRANSACTION-FILE KEY IS = TRANSACTION-CDE
                   INVALID KEY
                       MOVE 'Y' TO UNIQUE-FOUND
                   NOT INVALID KEY
                       PERFORM GENERATE-RANDOM

               END-START
           END-PERFORM

           MOVE 'N' TO UNIQUE-FOUND

           MOVE 0 TO TRANSACTION-ID
           START TRANSACTION-FILE KEY IS >= TRANSACTION-ID
               INVALID KEY CONTINUE
           END-START

           PERFORM UNTIL TRANS-STATUS NOT = "00"
               READ TRANSACTION-FILE NEXT
                   AT END EXIT PERFORM
                   NOT AT END
                       MOVE TRANSACTION-ID TO LAST-TRANSACTION-ID
               END-READ
           END-PERFORM

           ADD 1 TO LAST-TRANSACTION-ID


           ACCEPT WS-DATE FROM DATE
           STRING "20" WS-DATE(1:2) DELIMITED BY SIZE
                  WS-DATE(3:2) DELIMITED BY SIZE
                  WS-DATE(5:2) DELIMITED BY SIZE
                  INTO WS-TODAY

           ACCEPT WS-TIME FROM TIME
           STRING WS-TIME(1:2) DELIMITED BY SIZE
                  WS-TIME(3:2) DELIMITED BY SIZE
                  WS-TIME(5:2) DELIMITED BY SIZE
                  INTO WS-CUR-TIME

           MOVE UNIQUE-ID           TO TRANSACTION-CDE
           MOVE LAST-TRANSACTION-ID TO TRANSACTION-ID
           MOVE LNK-ACCNO           TO TR-ACC-NO
           MOVE 0                   TO TR-RECEIVER-NO
           MOVE 1                   TO TR-TYPE       *> 1 = Deposit
           MOVE WS-TODAY            TO TR-DATE
           MOVE WS-CUR-TIME         TO TR-TIME
           MOVE LNK-AMOUNT          TO TR-AMOUNT

           WRITE TRANSACTION-RECORD INVALID KEY
               DISPLAY "[ERROR]: Writing transaction record is failed."
           END-WRITE

           CLOSE TRANSACTION-FILE.
