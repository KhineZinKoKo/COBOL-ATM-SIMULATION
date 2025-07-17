       IDENTIFICATION DIVISION.
       PROGRAM-ID. TransferMoney.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "USERS.dat"
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
       01 FILE-STATUS         PIC XX.
       01 TRANS-STATUS        PIC XX.
       01 LAST-TRANSACTION-ID PIC 9(5) VALUE ZEROS.
       01 WS-TODAY            PIC 9(8).
       01 WS-DATE             PIC X(10).
       01 WS-TIME             PIC 9(8).
       01 WS-CUR-TIME         PIC X(6).
       01 TEMP-MONEY1         PIC 9(15)V99.
       01 TEMP-MONEY2         PIC 9(15)V99.
       01 UNIQUE-FOUND        PIC X VALUE 'N'.
       01 WS-MS               PIC 9(8).
       01 FORMAT-BALANCE        PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.ZZ.

       01 LNK-USER1.
           05 LNK1-ACC-NO         PIC 9(16).
           05 LNK1-USER-NAME      PIC X(25).
           05 LNK1-USER-NRC       PIC X(20).
           05 LNK1-ACC-TYPE       PIC X(10).
           05 LNK1-ACC-BALANCE    PIC 9(15)V99.
           05 LNK1-ACC-PIN        PIC X(4).
           05 LNK1-ACC-STATUS     PIC X(6).
           05 LNK1-ACC-DATE       PIC X(10).

       01 LNK-USER2.
           05 LNK2-ACC-NO         PIC 9(16).
           05 LNK2-USER-NAME      PIC X(25).
           05 LNK2-USER-NRC       PIC X(20).
           05 LNK2-ACC-TYPE       PIC X(10).
           05 LNK2-ACC-BALANCE    PIC 9(15)V99.
           05 LNK2-ACC-PIN        PIC X(4).
           05 LNK2-ACC-STATUS     PIC X(6).
           05 LNK2-ACC-DATE       PIC X(10).

       LINKAGE SECTION.
       01 LNK-ACC1            PIC 9(16).
       01 LNK-ACC2            PIC 9(16).
       01 TRANSFER-AMOUNT     PIC 9(15)V99.
       01 RTN-AMOUNT     PIC 9(15)V99.

       PROCEDURE DIVISION USING
           LNK-ACC1 LNK-ACC2 TRANSFER-AMOUNT RTN-AMOUNT.

       MAIN-PARH.
           PERFORM READ-FILE
           PERFORM TRANSFER
           GOBACK.

       READ-FILE.
           OPEN INPUT USER-FILE
           MOVE LNK-ACC1 TO ACC-NO
           READ USER-FILE
               INVALID KEY
                   PERFORM ZERO-USER1
               NOT INVALID KEY
                   PERFORM COPY-TO-LNK1
           END-READ

           MOVE LNK-ACC2 TO ACC-NO
           READ USER-FILE
               INVALID KEY
                   PERFORM ZERO-USER2
               NOT INVALID KEY
                   PERFORM COPY-TO-LNK2
           END-READ

           CLOSE USER-FILE.

       GENERATE-RANDOM.
           COMPUTE WS-RANDOM = FUNCTION RANDOM
           ACCEPT WS-MS FROM DATE
           COMPUTE WS-RANDOM = WS-RANDOM * WS-MS
           COMPUTE WS-RANDOM = WS-RANDOM / 10000000
           COMPUTE UNIQUE-ID = (WS-RANDOM * 89999999) + 10000000.

       TRANSFER.
           MOVE LNK1-ACC-BALANCE TO TEMP-MONEY1
           MOVE LNK2-ACC-BALANCE TO TEMP-MONEY2

           IF TEMP-MONEY1 >= TRANSFER-AMOUNT
               COMPUTE TEMP-MONEY1 = TEMP-MONEY1 - TRANSFER-AMOUNT
               COMPUTE TEMP-MONEY2 = TEMP-MONEY2 + TRANSFER-AMOUNT

               MOVE TEMP-MONEY1 TO LNK1-ACC-BALANCE
               MOVE TEMP-MONEY2 TO LNK2-ACC-BALANCE

               PERFORM WRITE-FILE
               PERFORM WRITE-TRANSACTION
               DISPLAY "============================================"
               DISPLAY "Sender ID       : " LNK1-ACC-NO
               DISPLAY "Receiver ID     : " LNK2-ACC-NO
               DISPLAY "Transaction ID  : " TRANSACTION-CDE
               MOVE TRANSFER-AMOUNT TO FORMAT-BALANCE
               DISPLAY "Transfer Amount : " FORMAT-BALANCE
               MOVE LNK1-ACC-BALANCE TO FORMAT-BALANCE
               DISPLAY "New Balance     : "FORMAT-BALANCE
               DISPLAY "--------------------------------------------"
               DISPLAY "Transfer successful."
               DISPLAY "============================================"
           ELSE
               DISPLAY "[WARNING]: Insufficient funds in sender "
               "account."
           END-IF
           MOVE LNK1-ACC-BALANCE TO RTN-AMOUNT
           GOBACK.

       WRITE-FILE.
           OPEN I-O USER-FILE

           MOVE LNK1-ACC-NO      TO ACC-NO
           MOVE LNK1-USER-NAME   TO USER-NAME
           MOVE LNK1-USER-NRC    TO USER-NRC
           MOVE LNK1-ACC-TYPE    TO ACC-TYPE
           MOVE LNK1-ACC-BALANCE TO ACC-BALANCE
           MOVE LNK1-ACC-PIN     TO ACC-PIN
           MOVE LNK1-ACC-STATUS  TO ACC-STATUS
           MOVE LNK1-ACC-DATE    TO ACC-DATE
           REWRITE USER-RECORD

           MOVE LNK2-ACC-NO      TO ACC-NO
           MOVE LNK2-USER-NAME   TO USER-NAME
           MOVE LNK2-USER-NRC    TO USER-NRC
           MOVE LNK2-ACC-TYPE    TO ACC-TYPE
           MOVE LNK2-ACC-BALANCE TO ACC-BALANCE
           MOVE LNK2-ACC-PIN     TO ACC-PIN
           MOVE LNK2-ACC-STATUS  TO ACC-STATUS
           MOVE LNK2-ACC-DATE    TO ACC-DATE
           REWRITE USER-RECORD

           CLOSE USER-FILE.

       WRITE-TRANSACTION.
           ACCEPT WS-DATE FROM DATE
           STRING "20" WS-DATE(1:2) DELIMITED BY SIZE
                  WS-DATE(3:2) DELIMITED BY SIZE
                  WS-DATE(5:2) DELIMITED BY SIZE
                  INTO WS-TODAY

           ACCEPT WS-TIME FROM TIME
           DISPLAY WS-TIME
           STRING WS-TIME(1:2) DELIMITED BY SIZE
                  WS-TIME(3:2) DELIMITED BY SIZE
                  WS-TIME(5:2) DELIMITED BY SIZE
                  INTO WS-CUR-TIME

           OPEN I-O TRANSACTION-FILE

           PERFORM UNTIL UNIQUE-FOUND = 'Y'
               PERFORM GENERATE-RANDOM
               DISPLAY UNIQUE-ID
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

           MOVE UNIQUE-ID           TO TRANSACTION-CDE
           MOVE LAST-TRANSACTION-ID TO TRANSACTION-ID
           MOVE LNK-ACC1            TO TR-ACC-NO
           MOVE LNK-ACC2            TO TR-RECEIVER-NO
           MOVE 3                   TO TR-TYPE
           MOVE WS-TODAY            TO TR-DATE
           MOVE WS-CUR-TIME         TO TR-TIME
           MOVE TRANSFER-AMOUNT     TO TR-AMOUNT

           WRITE TRANSACTION-RECORD INVALID KEY
               DISPLAY "[ERROR]: Writing transaction record is failed."
           END-WRITE

           CLOSE TRANSACTION-FILE.

       ZERO-USER1.
           MOVE 0 TO LNK1-ACC-NO LNK1-ACC-BALANCE
           MOVE SPACES TO
               LNK1-USER-NAME LNK1-USER-NRC LNK1-ACC-TYPE
               LNK1-ACC-PIN LNK1-ACC-STATUS LNK1-ACC-DATE.

       ZERO-USER2.
           DISPLAY "[ERROR]: Receiver account is not found..."
           GOBACK.

       COPY-TO-LNK1.
           MOVE ACC-NO         TO LNK1-ACC-NO
           MOVE USER-NAME      TO LNK1-USER-NAME
           MOVE USER-NRC       TO LNK1-USER-NRC
           MOVE ACC-TYPE       TO LNK1-ACC-TYPE
           MOVE ACC-BALANCE    TO LNK1-ACC-BALANCE
           MOVE ACC-PIN        TO LNK1-ACC-PIN
           MOVE ACC-STATUS     TO LNK1-ACC-STATUS
           MOVE ACC-DATE       TO LNK1-ACC-DATE.

       COPY-TO-LNK2.
           MOVE ACC-NO         TO LNK2-ACC-NO
           MOVE USER-NAME      TO LNK2-USER-NAME
           MOVE USER-NRC       TO LNK2-USER-NRC
           MOVE ACC-TYPE       TO LNK2-ACC-TYPE
           MOVE ACC-BALANCE    TO LNK2-ACC-BALANCE
           MOVE ACC-PIN        TO LNK2-ACC-PIN
           MOVE ACC-STATUS     TO LNK2-ACC-STATUS
           MOVE ACC-DATE       TO LNK2-ACC-DATE.
