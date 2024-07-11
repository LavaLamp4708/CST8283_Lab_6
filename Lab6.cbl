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
       01 SUBS.
           05 PROVINCE-SUB PIC 99.
           05 PARTY-SUB PIC 9.
           05 PARTY-SUB-WITH-MAJORITY PIC 9.

       01 WS-PARLIAMENT.
           05 WS-PARLIAMENT-TABLE OCCURS 14 TIMES.
               10 WS-PROVINCE-NAME PIC A(25).
               10 WS-PARTIES PIC 99 OCCURS 7 TIMES.

       01 USER-INPUT.
           05 PARTY-CHOICE PIC A(3).
               88 IS-VALID VALUES
               "LIB" "CPC" "BQ " "NDP" "GP " "IND" "VAC".
           05 BEGIN-MESSAGE PIC A.
           05 CONTINUE-MESSAGE PIC A.

       01 FLAGS.
           05 VALID-CHOICE PIC 9 VALUE 0.
           05 CAN-BEGIN PIC 9 VALUE 0.
           05 CAN-CONTINUE PIC 9 VALUE 1.
           05 WS-EOF PIC 9 VALUE 0.

       01 VALID-ROW-COUNT PIC 99 VALUE 0.

       01 OUTPUT-FORMATS.
           05 DISPLAY-VALID-ROW-COUNT PIC ZZ.
           05 WS-DISPLAY-PARTIES.
               10 WS-DISPLAY-PARTIES-TABLE PIC ZZ OCCURS 7 TIMES.

       01 SPACER PIC X(75) VALUE ALL "-".

       PROCEDURE DIVISION.

      * Main procedure. Once it reaches the end, a close routine paragraph will close the file and stop the program from running.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ROUTINE.
           PERFORM GREETING.
           PERFORM READ-PARLIAMENT
               VARYING PROVINCE-SUB FROM 1 BY 1
               UNTIL PROVINCE-SUB > 12 OR WS-EOF=1.
           MOVE 1 TO CAN-CONTINUE.
           PERFORM IO-LOOP UNTIL CAN-CONTINUE = 0.
           PERFORM CLOSE-ROUTINE.

      * Opens the input file.
       INITIALIZE-ROUTINE.
           OPEN INPUT PARLIAMENT-IN.

      * Greets the user and engages the begin loop paragraph
       GREETING.
           DISPLAY "Welcome to the parliament program."
           DISPLAY "This program allows you to search for the province".
           DISPLAY "that your chosen party listed in the provided".
           DISPLAY "table is the most popular in."
           DISPLAY SPACES.
           PERFORM BEGIN-LOOP UNTIL CAN-BEGIN = 1.

      * Asks the user if he/she wants to begin. The only valid answers are "Y" or "n" (case-sensitive).
      * If neither answer is given, the user is asked again.
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

      * Fills table in the working storage section from the input file.
       READ-PARLIAMENT.
           READ PARLIAMENT-IN
               AT END MOVE 1 TO WS-EOF.
           MOVE PROVINCE-NAME TO WS-PROVINCE-NAME(PROVINCE-SUB).
           MOVE LIB TO WS-PARTIES(PROVINCE-SUB, 1).
           MOVE CPC TO WS-PARTIES(PROVINCE-SUB, 2).
           MOVE BQ  TO WS-PARTIES(PROVINCE-SUB, 3).
           MOVE NDP TO WS-PARTIES(PROVINCE-SUB, 4).
           MOVE GP  TO WS-PARTIES(PROVINCE-SUB, 5).
           MOVE IND TO WS-PARTIES(PROVINCE-SUB, 6).
           MOVE VAC TO WS-PARTIES(PROVINCE-SUB, 7).

      * 1. Promts user to pick a party from a set of codes.
      * 2. Displays provinces where the chosen party holds the majority vote, along with the seat counts for each party.
      * 3. Promps the user if he/she would like to continue.
       IO-LOOP.
           MOVE 1 TO CAN-CONTINUE.
           MOVE 0 TO VALID-CHOICE.
           PERFORM ACCEPT-PARTY-CHOICE UNTIL VALID-CHOICE = 1.
           PERFORM DISPLAY-HEADER.
           MOVE 0 TO VALID-ROW-COUNT.
           PERFORM SEARCH-MAJORITY-ROWS
               VARYING PROVINCE-SUB
               FROM 1 BY 1
               UNTIL PROVINCE-SUB = 12.
           PERFORM DISPLAY-FOOTER.
           INITIALIZE CONTINUE-MESSAGE.
           PERFORM PROMPT-CONTINUE UNTIL CONTINUE-MESSAGE = "Y"
               OR CONTINUE-MESSAGE = "n".

      * Displays choices for parties. Accepts user-input for party choice. If the choice is not valid, prompts the user again.
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
           DISPLAY SPACER.
           DISPLAY SPACES.
           DISPLAY "Enter party choice:"
           ACCEPT PARTY-CHOICE.
           IF IS-VALID
               MOVE 1 TO VALID-CHOICE
               DISPLAY SPACES
           ELSE
               DISPLAY "INVALID CHOICE."
               DISPLAY SPACES
           END-IF.

      * Displays header before loop to display all valid rows.
       DISPLAY-HEADER.
           DISPLAY "Provinces with majority vote for: " PARTY-CHOICE.
           DISPLAY SPACES.
           DISPLAY "Province:                 LIB|CPC|BQ |NDP|GP |IND|"
               "VAC".
           DISPLAY SPACER.

      * Searches for provinces where the selected party holds a majority vote.
       SEARCH-MAJORITY-ROWS.
           PERFORM VARYING PARTY-SUB FROM 1 BY 1 UNTIL PARTY-SUB > 7
               IF WS-PARTIES(PROVINCE-SUB, PARTY-SUB) >
                   WS-PARTIES(PROVINCE-SUB, PARTY-SUB-WITH-MAJORITY)
                   MOVE PARTY-SUB TO PARTY-SUB-WITH-MAJORITY
               END-IF
           END-PERFORM.
           EVALUATE PARTY-CHOICE
               WHEN "LIB"
                   IF PARTY-SUB-WITH-MAJORITY = 1
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
               WHEN "CPC"
                   IF PARTY-SUB-WITH-MAJORITY = 2
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
               WHEN "BQ "
                   IF PARTY-SUB-WITH-MAJORITY = 3
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
               WHEN "NDP"
                   IF PARTY-SUB-WITH-MAJORITY = 4
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
               WHEN "GP "
                   IF PARTY-SUB-WITH-MAJORITY = 5
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
               WHEN "IND"
                   IF PARTY-SUB-WITH-MAJORITY = 6
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
               WHEN "VAC"
                   IF PARTY-SUB-WITH-MAJORITY = 7
                       PERFORM DISPLAY-ROW
                       ADD 1 TO VALID-ROW-COUNT
                   END-IF
           END-EVALUATE.

      * Displays the provinces where the chosen party hold a majority vote.
       DISPLAY-ROW.
           MOVE WS-PARTIES(PROVINCE-SUB, 1)
               TO WS-DISPLAY-PARTIES-TABLE(1)
           MOVE WS-PARTIES(PROVINCE-SUB, 2)
               TO WS-DISPLAY-PARTIES-TABLE(2)
           MOVE WS-PARTIES(PROVINCE-SUB, 3)
               TO WS-DISPLAY-PARTIES-TABLE(3)
           MOVE WS-PARTIES(PROVINCE-SUB, 4)
               TO WS-DISPLAY-PARTIES-TABLE(4)
           MOVE WS-PARTIES(PROVINCE-SUB, 5)
               TO WS-DISPLAY-PARTIES-TABLE(5)
           MOVE WS-PARTIES(PROVINCE-SUB, 6)
               TO WS-DISPLAY-PARTIES-TABLE(6)
           MOVE WS-PARTIES(PROVINCE-SUB, 7)
               TO WS-DISPLAY-PARTIES-TABLE(7)
           DISPLAY WS-PROVINCE-NAME(PROVINCE-SUB)
               " "  WS-DISPLAY-PARTIES-TABLE(1)
               "  " WS-DISPLAY-PARTIES-TABLE(2)
               "  " WS-DISPLAY-PARTIES-TABLE(3)
               "  " WS-DISPLAY-PARTIES-TABLE(4)
               "  " WS-DISPLAY-PARTIES-TABLE(5)
               "  " WS-DISPLAY-PARTIES-TABLE(6)
               "  " WS-DISPLAY-PARTIES-TABLE(7).

      * Displays footer. If the no provinces were found with majority vote for the selected party, displays message that no provinces were found.
      * Displays count of provinces displayed.
       DISPLAY-FOOTER.
           IF VALID-ROW-COUNT = 0
               DISPLAY "No provinces have majority seats for the chosen"
               " party"
               DISPLAY SPACER
           ELSE
               DISPLAY SPACER
               MOVE VALID-ROW-COUNT TO DISPLAY-VALID-ROW-COUNT
               DISPLAY "Count: " DISPLAY-VALID-ROW-COUNT
           END-IF.
           DISPLAY SPACES.

      * Asks user if he/she would like to continue. If yes, continues the IO-LOOP. If not, ends the IO-LOOP. If neither answer is given, prompts the user again.
       PROMPT-CONTINUE.
           DISPLAY "Would you like to continue? (Y/n)".
           ACCEPT CONTINUE-MESSAGE.
           IF CONTINUE-MESSAGE = "Y"
               MOVE 1 TO CAN-CONTINUE
           ELSE IF CONTINUE-MESSAGE = "n"
               DISPLAY "Thank you for using the parliament program."
                   " Goodbye!"
               MOVE 0 TO CAN-CONTINUE
           ELSE
               DISPLAY "ERROR: Incorrect answer."
           END-IF.

      * Closes input file and ends the program.
       CLOSE-ROUTINE.
           CLOSE PARLIAMENT-IN.
           STOP RUN.
       END PROGRAM LAB-6.
