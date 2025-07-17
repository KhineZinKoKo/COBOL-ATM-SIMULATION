      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main_ATM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 Choice PIC 9.
       01 Account-No PIC 9(16).
       01 Check PIC A VALUE 'Y'.
       LINKAGE SECTION.
       01 Temp PIC 9.
       01 ADMIN-CHOICE PIC 9.
       PROCEDURE DIVISION USING TEMP,ADMIN-CHOICE.
       IF ADMIN-CHOICE=5


           PERFORM TRAS-HISTORY.
           GOBACK.

           TRAS-HISTORY.
             PERFORM UNTIL Check  EQUAL 3
             DISPLAY "1. All User Transaction History."
             DISPLAY "2. One User Transaction History."
             DISPLAY "3. Back to ATM Menu... "
             ACCEPT Choice
             EVALUATE Choice
               WHEN 1
                   MOVE 00000000000000000 TO Account-No
                   CALL 'ATM_View_AllUser' USING Account-No , Choice
               WHEN 2
                   DISPLAY 'Enter Account-No.'
                   ACCEPT Account-No
                   CALL 'ATM_View_AllUser' USING Account-No , Choice
               WHEN 3
                   DISPLAY "Returning to ATM Menu... "
                   GOBACK
               WHEN OTHER
                   DISPLAY 'INVALID CHOICE.'
           END-EVALUATE
           PERFORM TRAS-HISTORY
      *>      DISPLAY 'Do You Want To Continue.(Y/N)?'
      *>      ACCEPT Check
           END-PERFORM.
      *>      IF Check NOT EQUAL 'Y'.
      *>          Write to go main menu

       END PROGRAM Main_ATM.
