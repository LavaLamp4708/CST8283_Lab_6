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
               ASSIGN TO 'parliament.txt'.
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
       01 CAN-BEGIN PIC A.
       01 PARLIAMENT-SUB PIC 99.
       01 SPACER PIC X(50) VALUE ALL "=".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ROUTINE.
           PERFORM GREETING.
           PERFORM READ-PARLIAMENT
               VARYING PARLIAMENT-SUB FROM 1 BY 1
               UNTIL PARLIAMENT-SUB > 14.
           PERFORM IO-LOOP UNTIL CAN-CONTINUE = 0.
           STOP RUN.
       INITIALIZE-ROUTINE.
           OPEN INPUT PARLIAMENT-IN.
       GREETING.
           DISPLAY "Welcome to the parliament program."
           DISPLAY "This program allows you to search for the province".
           DISPLAY "that your chosen party listed in the provided".
           DISPLAY "table is the most popular in."
           DISPLAY "Would you like to begin? (Y/n)"
           ACCEPT CAN-BEGIN.
           IF CAN-BEGIN = "n"
               CLOSE PARLIAMENT-IN
               STOP RUN
           ELSE IF CAN-BEGIN = "Y"

       READ-PARLIAMENT.
           READ PARLIAMENT-IN.
           MOVE PROVINCE-SEAT-DISTRIBUTION
               TO WS-PROVINCE-SEAT-DISTRIBUTION(PARLIAMENT-SUB).
       IO-LOOP.
           PERFORM ACCEPT-PARTY-CHOICE UNTIL VALID-CHOICE = 1.
           PERFORM SEARCH-PROVINCE.
           PERFORM RETURN-ROW.
       ACCEPT-PARTY-CHOICE.
           DISPLAY "Party-choice options:".
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
               DISPLAY "INVALID CHOICE"

           END-IF.
       END PROGRAM LAB-6.
