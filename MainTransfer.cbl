       IDENTIFICATION DIVISION.
       PROGRAM-ID. MainTransfer.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SENDER-ACCNO      PIC 9(16).
       01 WS-RECEIVER-ACCNO    PIC 9(16).
       01 WS-AMOUNT            PIC 9(15)V99.
       01 DUMMY                PIC X.

       PROCEDURE DIVISION.
           DISPLAY "======================================"
           DISPLAY "           TRANSFER MONEY            "
           DISPLAY "======================================"

           MOVE 1000000000000008 TO WS-SENDER-ACCNO
           MOVE 1000000000000006 TO WS-RECEIVER-ACCNO
           MOVE 1000 TO WS-AMOUNT

           CALL 'TransferMoney'
           USING WS-SENDER-ACCNO WS-RECEIVER-ACCNO WS-AMOUNT

           DISPLAY "Press ENTER to exit..."
           ACCEPT DUMMY
           STOP RUN.
