       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main_Reg.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS USER-ACC-NO
               FILE STATUS IS USERS-STATUS.

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

       WORKING-STORAGE SECTION.
       01 WS-USER-ID                PIC 9(16).
       01 MAIN-CHOICE               PIC 9.
       01 EXIT-FLAG                 PIC X VALUE 'N'.
       01 ADMIN-CHOICE              PIC 9.
       01 USER-CHOICE               PIC 9.
       01 TEMP                      PIC X(20).
       01 ADMIN-USERNAME            PIC X(20).
       01 ADMIN-PASSWORD            PIC X(20).
       01 VALID-LOGIN               PIC X VALUE 'N'.
       01 STORED-ADMIN-USERNAME     PIC X(20) VALUE "admin".
       01 STORED-ADMIN-PASSWORD     PIC X(20) VALUE "1234".
       01 USERS-STATUS              PIC XX.
       01 USER-ID-INPUT             PIC 9(16).
       01 USER-PIN-INPUT            PIC 9(4).
       01 SESSION-USER-ID           PIC 9(16).
       01 SESSION-BALANCE           PIC 9(15).

       01 AMOUNTS                   PIC S9(15)V99.
       01 FORMAT-BALANCE            PIC $ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.ZZ.
       01 RECEIVER-ID               PIC 9(16).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY X'1B' & "[34m"
           PERFORM UNTIL EXIT-FLAG = 'Y'
               DISPLAY "=============================="
               DISPLAY "     MAIN MENU"
               DISPLAY "=============================="
               DISPLAY "1. Admin"
               DISPLAY "2. User"
               DISPLAY "3. Exit"
               DISPLAY "Enter your choice (1-3): "
               ACCEPT MAIN-CHOICE

               EVALUATE MAIN-CHOICE
                   WHEN 1
                       PERFORM ADMIN-LOGIN
                       IF VALID-LOGIN = 'Y'
                           PERFORM ADMIN-MENU
                       ELSE
                           DISPLAY "Returning to main menu..."
                       END-IF

                   WHEN 2
                       PERFORM USER-LOGIN
                       IF VALID-LOGIN = 'Y'
                           PERFORM USER-MENU
                       ELSE
                           DISPLAY "Returning to main menu..."
                       END-IF

                   WHEN 3
                       MOVE 'Y' TO EXIT-FLAG

                   WHEN OTHER
                       DISPLAY "Invalid choice, please try again."
               END-EVALUATE

               DISPLAY "Thank you for using ATM."
           END-PERFORM.

           STOP RUN.

       ADMIN-LOGIN.
           MOVE 'N' TO VALID-LOGIN
           DISPLAY "############################################"
           DISPLAY "         ADMIN LOGIN REQUIRED"
           DISPLAY "############################################"
           DISPLAY "Enter Admin Username: "
           ACCEPT ADMIN-USERNAME
           DISPLAY "Enter Admin Password: "
           ACCEPT ADMIN-PASSWORD

           IF FUNCTION TRIM(ADMIN-USERNAME) =
              FUNCTION TRIM(STORED-ADMIN-USERNAME)
              AND FUNCTION TRIM(ADMIN-PASSWORD) =
              FUNCTION TRIM(STORED-ADMIN-PASSWORD)
               MOVE 'Y' TO VALID-LOGIN
               DISPLAY "Login successful. Access granted."
           ELSE
               DISPLAY "Invalid username or password."
           END-IF.

       USER-LOGIN.
           MOVE 'N' TO VALID-LOGIN
           OPEN INPUT USERS-FILE

           PERFORM UNTIL VALID-LOGIN = 'Y'
               DISPLAY "---------- User Login ----------"
               DISPLAY "Enter User ID: "
               ACCEPT USER-ID-INPUT
               DISPLAY "Enter PIN: "
               ACCEPT USER-PIN-INPUT

               MOVE USER-ID-INPUT TO USER-ACC-NO

               READ USERS-FILE KEY IS USER-ACC-NO
                   INVALID KEY
                       DISPLAY "Login failed: User not found."
                   NOT INVALID KEY
                       IF USER-PIN = USER-PIN-INPUT
                           MOVE 'Y' TO VALID-LOGIN
                           MOVE USER-ID-INPUT TO SESSION-USER-ID
                           MOVE USER-ID-INPUT TO WS-USER-ID
                           DISPLAY "Login successful!"
                       ELSE
                           DISPLAY "Login failed: Incorrect PIN."
                       END-IF
               END-READ

           END-PERFORM
           CLOSE USERS-FILE.

       ADMIN-MENU.
           MOVE 0 TO ADMIN-CHOICE

           PERFORM UNTIL ADMIN-CHOICE = 8
               DISPLAY "############################################"
               DISPLAY "           WELCOME TO COBOL ATM"
               DISPLAY "############################################"
               DISPLAY "            ATM MENU"
               DISPLAY "############################################"
               DISPLAY "1. Register New User"
               DISPLAY "2. View  All Users"
               DISPLAY "3. Update account status: Active /Inactive/Lock"
               "ed"
               DISPLAY "4. Delete user accounts"
               DISPLAY "5. View User Transaction History"
               DISPLAY "6. Generate transaction reports (daily/monthly)"
               DISPLAY "7. Reset User PIN"
               DISPLAY "8. Logout"
               DISPLAY "############################################"
               DISPLAY "ENTER YOUR CHOICE (1-8)"
               ACCEPT ADMIN-CHOICE
               DISPLAY "############################################"

               EVALUATE ADMIN-CHOICE
                   WHEN 1
                       CALL "Register" USING TEMP ADMIN-CHOICE
                   WHEN 2
                       CALL "View" USING TEMP ADMIN-CHOICE
                   WHEN 3
                       CALL "UpdateStatus" USING TEMP ADMIN-CHOICE
                   WHEN 4
                       CALL "DeleteAcc" USING TEMP ADMIN-CHOICE
                   WHEN 5
                       CALL "Main_ATM" USING TEMP ADMIN-CHOICE
                   WHEN 6
                       CALL "Monthly_Daily_Report"
                       USING TEMP ADMIN-CHOICE
                   WHEN 7
                       CALL "ResetPIN" USING TEMP ADMIN-CHOICE
                   WHEN 8
                       DISPLAY "Logging out from admin menu..."
                   WHEN OTHER
                       DISPLAY "Invalid choice, try again."
               END-EVALUATE
           END-PERFORM.

       USER-MENU.
           MOVE 0 TO USER-CHOICE

           PERFORM UNTIL USER-CHOICE = 7
               DISPLAY "============================================"
               DISPLAY "             WELCOME TO COBOL ATM"
               DISPLAY "============================================"
               DISPLAY "               USER MAIN MENU"
               DISPLAY "--------------------------------------------"
               DISPLAY "  1. Balance Inquiry"
               DISPLAY "  2. Deposit Money"
               DISPLAY "  3. Withdraw Money"
               DISPLAY "  4. Transfer Money"
               DISPLAY "  5. Transaction History"
               DISPLAY "  6. Change PIN"
               DISPLAY "  7. Logout"
               DISPLAY "--------------------------------------------"
               DISPLAY "Please select an option (1-7): "
               DISPLAY "============================================"
               ACCEPT USER-CHOICE

               EVALUATE USER-CHOICE
                   WHEN 1
                       DISPLAY "Calling BalanceInquiry with ID: "
                       WS-USER-ID
                       CALL 'Balance' USING WS-USER-ID

                   WHEN 2
                       PERFORM UNTIL AMOUNTS >= 5000
                       MOVE USER-BALANCE TO FORMAT-BALANCE
                       DISPLAY "Your Balance: " FORMAT-BALANCE
                       DISPLAY "Enter the amount to deposit..."
                       ACCEPT AMOUNTS

                       IF AMOUNTS < 5000
                           DISPLAY "Deposit amount should be greater "
                           "than 0..."
                           "than 5000..."
                       END-IF
                       DISPLAY "======================================="
                       "====="
                       END-PERFORM
                       CALL 'DepositMoney'
                       USING WS-USER-ID AMOUNTS USER-BALANCE
                       MOVE 0 TO AMOUNTS

                   WHEN 3
                       PERFORM UNTIL AMOUNTS > 0
                       MOVE USER-BALANCE TO FORMAT-BALANCE
                       DISPLAY "Your Balance: " FORMAT-BALANCE
                       DISPLAY "Enter the amount to withdraw..."
                       ACCEPT AMOUNTS
                       IF AMOUNTS < 1
                           DISPLAY "Withdraw amount should be greater "
                           "than 0..."
                       END-IF
                       DISPLAY "======================================="
                       "====="
                       END-PERFORM
                       CALL 'WithdrawMoney'
                       USING WS-USER-ID AMOUNTS USER-BALANCE
                       MOVE 0 TO AMOUNTS

                   WHEN 4
                       PERFORM UNTIL AMOUNTS > 0 AND
                       RECEIVER-ID NOT EQUAL WS-USER-ID
                       MOVE USER-BALANCE TO FORMAT-BALANCE
                       DISPLAY "Your Balance: " FORMAT-BALANCE
                       DISPLAY "Enter the account ID for transfer..."
                       ACCEPT RECEIVER-ID
                       DISPLAY "Enter the amount to transfer..."
                       ACCEPT AMOUNTS
                       IF RECEIVER-ID EQUAL WS-USER-ID
                           DISPLAY "You cannot transfer to your own "
                           "account..."
                       END-IF
                       IF AMOUNTS < 1
                           DISPLAY "Transfer amount should be greater "
                           "than 0..."
                       END-IF
                       DISPLAY "======================================="
                       "====="
                       END-PERFORM
                       CALL 'TransferMoney'
                       USING WS-USER-ID RECEIVER-ID AMOUNTS
                       USER-BALANCE
                       MOVE 0 TO AMOUNTS

                   WHEN 5
                       CALL 'UserViewHistory' USING WS-USER-ID

                   WHEN 6
                       CALL 'ChangePin' USING WS-USER-ID
                   WHEN 7
                       DISPLAY "Log out..."

                   WHEN OTHER
                       DISPLAY "Invalid. Please enter number between 1 "
                       "to 6"
               END-EVALUATE
           END-PERFORM.

       END PROGRAM Main_Reg.
