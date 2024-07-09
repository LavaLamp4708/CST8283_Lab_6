      ******************************************************************
      * Author: Peter Stainforth
      * Date: XX-07-2024
      * Purpose: Lab 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB-6.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARLIAMENT-IN
               ASSIGN TO 'parliament.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PARLIAMENT-IN.
       01 PROVINCE-SEAT-DISTRIBUTION.
           05 PROVINCE-NAME PIC A(25).
           05 PARTIES.
               10 LIB PIC 99.
               10 CPC PIC 99.
               10 BQ  PIC 99.
               10 NDP PIC 99.
               10 GP  PIC 99.
               10 IND PIC 99.
               10 VAC PIC 99.
       WORKING-STORAGE SECTION.
       01 WS-PARLIAMENT-SEATS.
           05 WS-PROVINCE-SEAT-DISTRIBUTION OCCURS 14 TIMES.
               10 WS-PROVINCE-NAME PIC A(25).
               10 WS-PARTIES.
                   20 WS-LIB PIC 99.
                   20 WS-CPC PIC 99.
                   20 WS-BQ  PIC 99.
                   20 WS-NDP PIC 99.
                   20 WS-GP  PIC 99.
                   20 WS-IND PIC 99.
                   20 WS-VAC PIC 99.
       01 PARTY-CHOICE PIC A(3).
           88 IS-VALID VALUES
           "LIB" "CPC" "BQ " "NDP" "GP " "IND" "VAC".
       01 VALID-CHOICE PIC 9 VALUE 0.
       01 CAN-BEGIN PIC 9 VALUE 0.
       01 BEGIN-MESSAGE PIC A.
       01 CONTINUE-MESSAGE PIC A.
       01 CAN-CONTINUE PIC 9 VALUE 1.
       01 P-SUB PIC 99.
       01 VALID-ROW-NUM PIC 99 VALUE 0.
       01 SPACER PIC X(75) VALUE ALL "=".
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ROUTINE.
           PERFORM GREETING.
           PERFORM READ-PARLIAMENT
               VARYING P-SUB FROM 1 BY 1
               UNTIL P-SUB > 12.
           PERFORM IO-LOOP UNTIL CAN-CONTINUE = 0.
           PERFORM CLOSE-ROUTINE.
           STOP RUN.
       INITIALIZE-ROUTINE.
           OPEN INPUT PARLIAMENT-IN.
       GREETING.
           DISPLAY "Welcome to the parliament program."
           DISPLAY "This program allows you to search for the province".
           DISPLAY "that your chosen party listed in the provided".
           DISPLAY "table is the most popular in."
           PERFORM BEGIN-LOOP UNTIL CAN-BEGIN = 1.
       BEGIN-LOOP.
           DISPLAY "Would you like to begin? (Y/n)"
           ACCEPT BEGIN-MESSAGE.
           IF BEGIN-MESSAGE = "Y"
               MOVE 1 TO CAN-BEGIN
           ELSE IF BEGIN-MESSAGE = "n"
               PERFORM CLOSE-ROUTINE
           ELSE
               DISPLAY "ERROR: Incorrect answer."
               MOVE 0 TO CAN-BEGIN
           END-IF.
       READ-PARLIAMENT.
           READ PARLIAMENT-IN
               AT END MOVE 1 TO WS-EOF.
           MOVE PROVINCE-SEAT-DISTRIBUTION
               TO WS-PROVINCE-SEAT-DISTRIBUTION(P-SUB).
       IO-LOOP.
           INITIALIZE CAN-CONTINUE.
           INITIALIZE VALID-CHOICE.
           INITIALIZE PARTY-CHOICE.
           INITIALIZE VALID-ROW-NUM.
           PERFORM ACCEPT-PARTY-CHOICE UNTIL VALID-CHOICE = 1.
           PERFORM DISPLAY-HEADER.
           PERFORM SEARCH-MAJORITY-ROWS
               VARYING P-SUB
               FROM 1 BY 1
               UNTIL P-SUB = 12.
           PERFORM DISPLAY-FOOTER.
           PERFORM PROMPT-CONTINUE UNTIL CONTINUE-MESSAGE = "Y" OR "n".
       ACCEPT-PARTY-CHOICE.
           DISPLAY SPACES.
           DISPLAY "Party-choice options:  Codes:".
           DISPLAY SPACER.
           DISPLAY "Liberals:              LIB".
           DISPLAY "Conservatives:         CPC".
           DISPLAY "Bloc Quebecois:        BQ".
           DISPLAY "New Democratic Party:  NDP".
           DISPLAY "Green Party:           GP".
           DISPLAY "Independant:           IND".
           DISPLAY "Vacants:               VAC".
           ACCEPT PARTY-CHOICE.
           IF IS-VALID
               MOVE 1 TO VALID-CHOICE
           ELSE
               DISPLAY "INVALID CHOICE."
           END-IF.
       DISPLAY-HEADER.
           DISPLAY "Provinces with majority vote for: " PARTY-CHOICE.
           DISPLAY SPACES.
           DISPLAY "Province:                 LIB|CPC|BQ |NDP|GP |IND|"
               "VAC".
           DISPLAY SPACER.
       SEARCH-MAJORITY-ROWS.
           EVALUATE PARTY-CHOICE
               WHEN "LIB"
                   IF  WS-LIB(P-SUB) > WS-CPC(P-SUB)
                       AND WS-LIB(P-SUB) > WS-BQ(P-SUB)
                       AND WS-LIB(P-SUB) > WS-NDP(P-SUB)
                       AND WS-LIB(P-SUB) > WS-GP(P-SUB)
                       AND WS-LIB(P-SUB) > WS-IND(P-SUB)
                       AND WS-LIB(P-SUB) > WS-VAC(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
                   END-IF
               WHEN "CPC"
                   IF  WS-CPC(P-SUB) > WS-LIB(P-SUB)
                       AND WS-CPC(P-SUB) > WS-BQ(P-SUB)
                       AND WS-CPC(P-SUB) > WS-NDP(P-SUB)
                       AND WS-CPC(P-SUB) > WS-GP(P-SUB)
                       AND WS-CPC(P-SUB) > WS-IND(P-SUB)
                       AND WS-CPC(P-SUB) > WS-VAC(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
                   END-IF
               WHEN "BQ "
                   IF WS-BQ(P-SUB) > WS-LIB(P-SUB)
                       AND WS-BQ(P-SUB) > WS-CPC(P-SUB)
                       AND WS-BQ(P-SUB) > WS-NDP(P-SUB)
                       AND WS-BQ(P-SUB) > WS-GP(P-SUB)
                       AND WS-BQ(P-SUB) > WS-IND(P-SUB)
                       AND WS-BQ(P-SUB) > WS-VAC(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
                   END-IF
               WHEN "NDP"
                   IF WS-NDP(P-SUB) > WS-LIB(P-SUB)
                       AND WS-NDP(P-SUB) > WS-CPC(P-SUB)
                       AND WS-NDP(P-SUB) > WS-BQ(P-SUB)
                       AND WS-NDP(P-SUB) > WS-GP(P-SUB)
                       AND WS-NDP(P-SUB) > WS-IND(P-SUB)
                       AND WS-NDP(P-SUB) > WS-VAC(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
               WHEN "GP "
                   IF WS-GP(P-SUB) > WS-LIB(P-SUB)
                       AND WS-GP(P-SUB) > WS-CPC(P-SUB)
                       AND WS-GP(P-SUB) > WS-BQ(P-SUB)
                       AND WS-GP(P-SUB) > WS-NDP(P-SUB)
                       AND WS-GP(P-SUB) > WS-IND(P-SUB)
                       AND WS-GP(P-SUB) > WS-VAC(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
               WHEN "IND"
                   IF WS-IND(P-SUB) > WS-LIB(P-SUB)
                       AND WS-IND(P-SUB) > WS-CPC(P-SUB)
                       AND WS-IND(P-SUB) > WS-BQ(P-SUB)
                       AND WS-IND(P-SUB) > WS-NDP(P-SUB)
                       AND WS-IND(P-SUB) > WS-GP(P-SUB)
                       AND WS-IND(P-SUB) > WS-VAC(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
               WHEN "VAC"
                   IF WS-VAC(P-SUB) > WS-LIB(P-SUB)
                       AND WS-VAC(P-SUB) > WS-CPC(P-SUB)
                       AND WS-VAC(P-SUB) > WS-BQ(P-SUB)
                       AND WS-VAC(P-SUB) > WS-NDP(P-SUB)
                       AND WS-VAC(P-SUB) > WS-GP(P-SUB)
                       AND WS-VAC(P-SUB) > WS-IND(P-SUB)
                           MOVE P-SUB TO VALID-ROW-NUM
                           PERFORM DISPLAY-ROW
           END-EVALUATE.
       DISPLAY-ROW.
           DISPLAY WS-PROVINCE-NAME(VALID-ROW-NUM)
               " " WS-LIB(VALID-ROW-NUM) "  "
               WS-CPC(VALID-ROW-NUM) "  "
               WS-BQ(VALID-ROW-NUM)  "  "
               WS-NDP(VALID-ROW-NUM) "  "
               WS-GP(VALID-ROW-NUM)  "  "
               WS-IND(VALID-ROW-NUM) "  "
               WS-VAC(VALID-ROW-NUM) "  ".
       DISPLAY-FOOTER.
           IF VALID-ROW-NUM = 0
               DISPLAY "No provinces have majority seats for the chosen"
               " party".
           DISPLAY SPACER.
           DISPLAY SPACES.
       PROMPT-CONTINUE.
           INITIALIZE CONTINUE-MESSAGE.
           DISPLAY "Would you like to continue? (Y/n)".
           ACCEPT CONTINUE-MESSAGE.
           IF CONTINUE-MESSAGE = "Y"
               MOVE 1 TO CAN-CONTINUE
           ELSE IF CONTINUE-MESSAGE = "n"
               MOVE 0 TO CAN-CONTINUE
           ELSE
               DISPLAY "ERROR: Incorrect answer."
           END-IF.
       CLOSE-ROUTINE.
           CLOSE PARLIAMENT-IN.
           STOP RUN.
       END PROGRAM LAB-6.
