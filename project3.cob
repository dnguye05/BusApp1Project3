      ******************************************************************
      *Author: David Nguyen
      *Due Date: December 9, 2021
      *Purpose: project3
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. project3.
           AUTHOR. David Nguyen.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'NEWEMP'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD.
       01  INPUT-REC PIC X(132).
       FD  PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01  PRNT-REC PIC X(132).
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03 I-EMPID PIC X(7).
           03 I-LNAME PIC X(15).
           03 I-FNAME PIC X(15).
           03 I-EMPTYPE PIC X(2).
           03 I-TITLE PIC X(17).
           03 I-SSN PIC X(9).
           03 I-EMPTYSPACES1 PIC X(24).
           03 I-DATE PIC X(8).
           03 I-EMPTYSPACES2 PIC X(2).
           03 I-EMPRATE.
               05 I-EMPRATEWHOLE PIC 9(4).
               05 I-EMPRATEDECIMAL PIC P9(2).
           03 I-EMPSTATUS PIC X(1).
           03 I-DEDUCT OCCURS 5 TIMES.
               05 I-DEDUCTWHOLE PIC 9(3).
               05 I-DEDUCTDECIMAL PIC P9(2).
       01  PRNT-DATA1.
           03 FILLER PIC X(3) VALUE SPACES.
           03 L-EMPID PIC X(8).
           03 FILLER PIC X(4) VALUE SPACES.
           03 L-SSN PIC XXXBXXBXXXX.
           03 FILLER PIC X(4) VALUE SPACES.
           03 L-LNAME PIC X(14).
           03 FILLER PIC X(5) VALUE SPACES.
           03 L-FNAME PIC X(13).
           03 FILLER PIC X(7) VALUE SPACES.
           03 L-EMPTYPE PIC X(2).
           03 FILLER PIC X(4) VALUE SPACES.
           03 L-TITLE PIC X(17) VALUE SPACES.
           03 FILLER PIC X(5) VALUE SPACES.
           03 L-DATE PIC 99/99/9999.
           03 FILLER PIC X(25) VALUE SPACES.
       01  PRNT-DATA2.
           03 FILLER PIC X(69) VALUE SPACES.
           03 FILLER PIC X(7) VALUE 'DEDUCT:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 L-DEDUCT1 PIC ZZ9.99.
           03 FILLER PIC X(5) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'RATE:'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 L-EMPRATE PIC Z,ZZ9.99.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'STATUS'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 L-EMPSTATUS PIC X(1).
           03 FILLER PIC X(12) VALUE SPACES.
       01  PRNT-DATA2-BONUS.
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(34) 
               VALUE 'DEDUCTIONS EXCEED MONTHLY EARNINGS'.
           03 FILLER PIC X(32) VALUE SPACES.
           03 FILLER PIC X(7) VALUE 'DEDUCT:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 L-DEDUCT1-BONUS PIC ZZ9.99.
           03 FILLER PIC X(5) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'RATE:'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 L-EMPRATE-BONUS PIC Z,ZZ9.99.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'STATUS'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 L-EMPSTATUS-BONUS PIC X(1).
           03 FILLER PIC X(12) VALUE SPACES.
       01  PRNT-DATA3.
           03 FILLER PIC X(81) VALUE SPACES.
           03 L-DEDUCTOTHERS PIC ZZ9.99.
           03 FILLER PIC X(45) VALUE SPACES.
       01  PRNT-DATA4.
           03 FILLER PIC X(69) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'TOTAL:'.
           03 FILLER PIC X(3) VALUE SPACES.
           03 L-DEDUCTTOTAL PIC $$,$$9.99.
           03 FILLER PIC X(45) VALUE SPACES.
       01  PRNT-HEADING1.
           03 FILLER PIC X(1) VALUE SPACES.
           03 H1-CURR-DATE PIC 99/99/99.
           03 FILLER PIC X(40) VALUE SPACES.
           03 FILLER PIC X(21) VALUE 'MASTERMIND COBOL, INC'.
           03 FILLER PIC X(36) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'PAGE'.
           03 H1-PAGENUM PIC ZZ9 VALUE 1.
       01  PRNT-HEADING2.
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'EMP ID'.
           03 FILLER PIC X(6) VALUE SPACES.
           03 FILLER PIC X(3) VALUE 'SSN'.
           03 FILLER PIC X(12) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'NAME'.
           03 FILLER PIC X(34) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'TYPE'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'TITLE'.
           03 FILLER PIC X(18) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'DATE'.
           03 FILLER PIC X(31) VALUE SPACES.
       01  PRNT-HEADING3.
           03 FILLER PIC X(34) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'LAST'.
           03 FILLER PIC X(13) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'FIRST'.
           03 FILLER PIC X(76) VALUE SPACES.
       01  PRNT-FOOTER1.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(32) VALUE 'NUMBER OF EMPLOYEE RECORDS READ:'.
           03 FILLER PIC X(13) VALUE SPACES.
           03 F1-EMPCOUNTER PIC ZZZ9.
           03 FILLER PIC X(82) VALUE SPACES.
       01  PRNT-FOOTER2.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(27) VALUE 'NUMBER OF HOURLY EMPLOYEES:'.
           03 FILLER PIC X(18) VALUE SPACES.
           03 F2-HEMPCOUNT PIC ZZZ9.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(20) VALUE 'AVERAGE HOURLY RATE:'.
           03 FILLER PIC X(9) VALUE SPACES.
           03 F2-AVGHRATE PIC $$$9.99.
           03 FILLER PIC X(10) VALUE SPACES.
           03 FILLER PIC X(17) VALUE 'TOTAL AVG DEDUCT:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 F2-TOTDEDUCT PIC $$,$$9.99.
           03 FILLER PIC X(1) VALUE SPACES.
       01  PRNT-FOOTER3.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(29) VALUE 'NUMBER OF SALARIED EMPLOYEES:'.
           03 FILLER PIC X(16) VALUE SPACES.
           03 F3-SEMPCOUNT PIC ZZZ9.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(22) VALUE 'AVERAGE SALARIED RATE:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 F3-AVGSRATE PIC $$,$$$.99.
           03 FILLER PIC X(10) VALUES SPACES.
           03 FILLER PIC X(13) VALUE 'TOTAL DEDUCT:'.
           03 FILLER PIC X(8) VALUE SPACES.
           03 F3-TOTDEDUCT PIC $$$,$$9.99.
           03 FILLER PIC X(1) VALUE SPACES.
       01  MISC.
           03 EOF-I PIC 9 VALUE 0.
           03 PGNUM PIC 999 VALUE 1.
           03 RECORDPAGECOUNTER PIC 99 VALUE 0.
           03 EMPCOUNTER PIC 9(4).
           03 EMPHCOUNT PIC 9(4).
           03 EMPSCOUNT PIC 9(4).
           03 TOTALHRATE PIC 9(8)V9(2).
           03 TOTALSRATE PIC 9(10)V9(2).
           03 EMPRATE-FORMATER PIC 9(4)V9(2).
           03 DEDUCTARRAY-FORMATOR OCCURS 5 TIMES.
               05 DEDUCT-FORMAT PIC 9(3)V9(2).
           03 SUB PIC 99.
           03 TOTALEMPDEDUCT PIC 9(6)V9(2).
           03 DEDUCT-COUNT PIC 9(4).
           03 TOTAL-AVG-DEDUCT PIC 9(6)V9(2).
           03 TOTAL-DEDUCT PIC 9(7)V9(2).
           03 MONTHLY-EMP-RATE-BONUS PIC 9(5)V9(2).
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
               OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEADER.
           PERFORM 1500-LOOP
               UNTIL EOF-I = 1;
           PERFORM 1700-PRINT-FOOTER.
           CLOSE INPUT-FILE
               PRNT-FILE.
           STOP RUN.
      ******************************************************************
      *    PRINT THE HEADER
      ******************************************************************
       1400-PRINT-HEADER.
           ACCEPT H1-CURR-DATE FROM DATE.
           IF PGNUM = 1
               WRITE PRNT-REC FROM PRNT-HEADING1
           ELSE
               MOVE SPACES TO PRNT-REC
               WRITE PRNT-REC
                   AFTER ADVANCING PAGE
               WRITE PRNT-REC FROM PRNT-HEADING1
                   AFTER ADVANCING 1 LINE
           END-IF.
           WRITE PRNT-REC FROM PRNT-HEADING2
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-HEADING3
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
           MOVE 0 TO RECORDPAGECOUNTER.
           ADD 1 TO PGNUM.
           MOVE PGNUM TO H1-PAGENUM.
      ******************************************************************
      *    LOOPING THROUGH THE RECORDS IN THE NEWEMP FILE
      ******************************************************************
       1500-LOOP.
           PERFORM 1600-PRINT-RECORDS.
           PERFORM 2000-READ-INPUT.
      ******************************************************************
      *    THIS KEEPS TRACK OF NUMBER OF LINES PRINTED
      ******************************************************************
       1590-PAGE-COUNTER.
           ADD 1 TO RECORDPAGECOUNTER.
           IF RECORDPAGECOUNTER = 25
               PERFORM 1400-PRINT-HEADER
           END-IF.
      ******************************************************************
      *    PRINT THE NORMAL EMPLOYEE RECORDS
      ******************************************************************
       1600-PRINT-RECORDS.
           MOVE I-EMPID TO L-EMPID.
           MOVE I-SSN TO L-SSN.
           INSPECT L-SSN REPLACING ALL ' ' BY '-'.
           MOVE I-LNAME TO L-LNAME.
           MOVE I-FNAME TO L-FNAME.
           MOVE I-EMPTYPE TO L-EMPTYPE.
           MOVE I-TITLE TO L-TITLE.
           MOVE I-DATE TO L-DATE.
           WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
           PERFORM 1590-PAGE-COUNTER.
           PERFORM 1620-PRINT-DEDUCT.
           ADD 1 TO EMPCOUNTER.
      ******************************************************************
      *    PRINT THE DUDUCT VALUES
      ******************************************************************
       1620-PRINT-DEDUCT.
           MOVE I-EMPRATE TO EMPRATE-FORMATER.
           MOVE I-DEDUCT(1) TO DEDUCT-FORMAT(1).
      *    PRE-PRINT DATA COMPUTATION
           IF I-EMPSTATUS = 'H'
               COMPUTE EMPHCOUNT = EMPHCOUNT + 1
               COMPUTE TOTALHRATE = TOTALHRATE + EMPRATE-FORMATER
              ELSE
               COMPUTE EMPSCOUNT = EMPSCOUNT + 1
               COMPUTE TOTALSRATE = TOTALSRATE + EMPRATE-FORMATER
           END-IF.
           COMPUTE MONTHLY-EMP-RATE-BONUS = (EMPRATE-FORMATER * 40) * 4.
           PERFORM VARYING SUB FROM 1 BY 1
               UNTIL SUB > 5
               COMPUTE TOTALEMPDEDUCT = 
                   TOTALEMPDEDUCT + DEDUCT-FORMAT(SUB)
           END-PERFORM.
      *    PRINT THE DEDUCT MESSAGE
           IF I-EMPSTATUS = 'H' AND
               TOTALEMPDEDUCT > MONTHLY-EMP-RATE-BONUS THEN
                   PERFORM 1640-PRINT-EXCEED-DEDUCT
               ELSE
                  IF I-EMPSTATUS = 'S' AND 
                      TOTALEMPDEDUCT > EMPRATE-FORMATER THEN
                       PERFORM 1640-PRINT-EXCEED-DEDUCT
                   ELSE
                       PERFORM 1630-PRINT-NORMAL-DEDUCT
                  END-IF
           END-IF
      *    PRINT THE DEDUCT OF EMP HAVE
           PERFORM VARYING SUB FROM 2 BY 1
               UNTIL SUB > 5
               MOVE I-DEDUCT(SUB) TO DEDUCT-FORMAT(SUB)
               MOVE DEDUCT-FORMAT(SUB) TO L-DEDUCTOTHERS
               WRITE PRNT-REC FROM PRNT-DATA3
                   AFTER ADVANCING 1 LINE
               PERFORM 1590-PAGE-COUNTER
           END-PERFORM.
      *    PRINT THE TOTAL EMP DEDUCT
           MOVE TOTALEMPDEDUCT TO L-DEDUCTTOTAL.
           WRITE PRNT-REC FROM PRNT-DATA4
               AFTER ADVANCING 1 LINE.
           PERFORM 1590-PAGE-COUNTER.
           MOVE SPACES TO PRNT-REC
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
           PERFORM 1590-PAGE-COUNTER.
           COMPUTE DEDUCT-COUNT = DEDUCT-COUNT + 1.
           COMPUTE TOTAL-DEDUCT = TOTAL-DEDUCT + TOTALEMPDEDUCT.
           MOVE 0 TO TOTALEMPDEDUCT.
      ******************************************************************
      *    PRINT THE NORMAL DEDUCT MESSAGE
      ******************************************************************
       1630-PRINT-NORMAL-DEDUCT.
           MOVE DEDUCT-FORMAT(1) TO L-DEDUCT1.
           MOVE EMPRATE-FORMATER TO L-EMPRATE.
           MOVE I-EMPSTATUS TO L-EMPSTATUS.
           WRITE PRNT-REC FROM PRNT-DATA2
               AFTER ADVANCING 1 LINE.
           PERFORM 1590-PAGE-COUNTER.
      ******************************************************************
      *    PRINT THE BONUS DEDUCT MESSAGE
      ******************************************************************
       1640-PRINT-EXCEED-DEDUCT.
           MOVE DEDUCT-FORMAT(1) TO L-DEDUCT1-BONUS.
           MOVE EMPRATE-FORMATER TO L-EMPRATE-BONUS.
           MOVE I-EMPSTATUS TO L-EMPSTATUS-BONUS.
           WRITE PRNT-REC FROM PRNT-DATA2-BONUS
               AFTER ADVANCING 1 LINE.
           PERFORM 1590-PAGE-COUNTER.
      ******************************************************************
      *    PRINT THE FOOTER DATA
      ******************************************************************
       1700-PRINT-FOOTER.
      *    PRINT FOOTER HEADER.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING PAGE.
           ACCEPT H1-CURR-DATE FROM DATE.
           WRITE PRNT-REC FROM PRNT-HEADING1
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
      *    PRINT FOOTER LINE 1.
           MOVE EMPCOUNTER TO F1-EMPCOUNTER.
           WRITE PRNT-REC FROM PRNT-FOOTER1
               AFTER ADVANCING 1 LINE.
      *    PRINT FOOTER LINE 2.
           MOVE EMPHCOUNT TO F2-HEMPCOUNT.
           COMPUTE TOTALHRATE = TOTALHRATE / EMPHCOUNT.
           MOVE TOTALHRATE TO F2-AVGHRATE.
           COMPUTE TOTAL-AVG-DEDUCT = TOTAL-DEDUCT / DEDUCT-COUNT.
           MOVE TOTAL-AVG-DEDUCT TO F2-TOTDEDUCT.
           WRITE PRNT-REC FROM PRNT-FOOTER2
               AFTER ADVANCING 1 LINE.
      *    PRINT FOOTER LINE 3.
           MOVE EMPSCOUNT TO F3-SEMPCOUNT
           COMPUTE TOTALSRATE = TOTALSRATE / EMPSCOUNT.
           MOVE TOTALSRATE TO F3-AVGSRATE.
           MOVE TOTAL-DEDUCT TO F3-TOTDEDUCT.
           WRITE PRNT-REC FROM PRNT-FOOTER3
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
      ******************************************************************
      *    READ IN NEWEMP FILE
      ******************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.
       END PROGRAM project3.
