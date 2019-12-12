       IDENTIFICATION DIVISION.
       program-id.     COBCMS01.
       DATE-WRITTEN.   12/2/2019.
       AUTHOR.         COLBY SNOW.
       DATE-COMPILED.
      ******************************************************************
      *THIS PROGRAM CALCULATE THE ESTIMATED COST OF HAVING A ROOM
      *PAINTED GIVEN THE WALL DIMENSIONS, DOOR DIMENSIONS, PAINT COST,
      *AND LABOR COST BY COLBY SNOW
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT PAINT-MASTER
           ASSIGN TO "C:\IHCC\COBOL\COBCMS01\PAINTEST.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
           ASSIGN TO "C:\IHCC\COBOL\PJOBEST.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PAINT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-PAINT-REC
           RECORD CONTAINS 23 CHARACTERS.

       01 I-PAINT-REC.
           05  PAINT-EST-NO        PIC X(4).
           05  PAINT-DATE.
               10 PAINT-YY         PIC 9(4).
               10 PAINT-MM         PIC 99.
               10 PAINT-DD         PIC 99.
           05  PAINT-WALL-SQ-FT    PIC 9(4).
           05  PAINT-DOOR-SQ-FT    PIC 9(3).
           05  PAINT-PRICE-GAL     PIC 99V99.

       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE                  PIC X(132).

       WORKING-STORAGE SECTION.
       01 WORK-AREA.
           05 C-PCTR               PIC 9(2)    VALUE 0.
           05 MORE-RECS            PIC X(3)    VALUE "YES".
           05 C-PAINT-EST          PIC 9(5)V99.
           05 C-LABOR-EST          PIC 9(5)V99.
           05 C-TOTAL-EST          PIC 9(6)V99.
           05 C-TOTAL-SQ-FT        PIC 9(4)V99.
           05 C-AMT-PAINT-GAL      PIC 9(3)V99.
           05 C-GT-PAINT-EST       PIC 9(8)V99 VALUE 0.
           05 C-GT-LABOR-EST       PIC 9(8)V99 VALUE 0.
           05 C-GT-PAINT-GAL       PIC 9(5)V99 VALUE 0.
           05 C-GT-RECORDS         PIC 999     VALUE 0.
           05 C-GT-TOTAL-EST       PIC 9(8)V99 VALUE 0.

       01 CURRENT-DATE-AND-TIME.
           05 I-DATE.
             10 I-YY               PIC 9(4).
             10 I-MM               PIC 99.
             10 I-DD               PIC 99.
           05 I-TIME               PIC X(11).

       01 COMPANY-TITLE.
           05 FILLER               PIC X(6)    VALUE "DATE".
           05 O-MM                 PIC 9(2).
           05 FILLER               PIC X       VALUE "/".
           05 O-DD                 PIC 9(2).
           05 FILLER               PIC X       VALUE "/".
           05 O-YY                 PIC 9(4).
           05 FILLER               PIC X(39)   VALUE SPACES.
           05 FILLER               PIC X(23)   VALUE 
                                       "SNOW'S PAINT ESTIMATION".
           05 FILLER               PIC X(47)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "PAGE:".
           05 O-PCTR               PIC Z9.

       01 COLUMN-HDINGS1.
           05 FILLER               PIC X(8)    VALUE "ESTIMATE".
           05 FILLER               PIC X(23)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "WALL".
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "DOOR".
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "TOTAL".
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(7)    VALUE "GALLONS".
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE "PRICE/".
           05 FILLER               PIC X(11)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "PAINT".
           05 FILLER               PIC X(12)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "LABOR".
           05 FILLER               PIC X(12)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "TOTAL".

       01 COLUMN-HDINGS2.
           05 FILLER               PIC X(1)    VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE "NUMBER".
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(13)   VALUE "ESTIMATE DATE".
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "SQ/FT".
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "SQ/FT".
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "SQ/FT".
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE "NEEDED".
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(7)    VALUE "GALLONS".
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE "ESTIMATE".
           05 FILLER               PIC X(9)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE "ESTIMATE".
           05 FILLER               PIC X(9)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE "ESTIMATE".

       01 DETAIL-LINE.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 O-PAINT-EST-NO       PIC X(4).
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-PAINT-DATE         PIC 99/99/9999.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-WALL-SQ-FT         PIC Z,ZZ9.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-DOOR-SQ-FT         PIC ZZ9.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-TOTAL-SQ-FT        PIC Z,ZZ9.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-AMT-PAINT-GAL      PIC ZZZ.99.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-PRICE-GAL          PIC ZZ.99.
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 O-PAINT-EST          PIC $ZZ,ZZZ.99.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-LABOR-EST          PIC $ZZ,ZZZ.99.
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 O-TOTAL-EST          PIC $ZZZ,ZZZ.99.
       
       01 TOTAL-LINE.
           05 FILLER               PIC X(34)   VALUE "GRAND TOTALS:".
           05 FILLER               PIC X(17)   VALUE "TOTAL ESTIMATES:".
           05 O-GT-RECORDS         PIC ZZ9.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 O-GT-PAINT-GAL       PIC ZZ,ZZZ.99.
           05 FILLER               PIC X(15)   VALUE SPACES.
           05 O-GT-PAINT-EST       PIC $$,$$$,$$$.99.
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 O-GT-LABOR-EST       PIC $$,$$$,$$$.99.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 O-GT-TOTAL-EST       PIC $$$,$$$,$$$.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.
       
       1000-INIT.
           OPEN INPUT PAINT-MASTER.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-MM TO O-MM.
           MOVE I-DD TO O-DD.
           
           PERFORM 9000-READ.
           PERFORM 9100-HEADINGS.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           SUBTRACT PAINT-DOOR-SQ-FT FROM PAINT-WALL-SQ-FT GIVING 
           C-TOTAL-SQ-FT.
           DIVIDE C-TOTAL-SQ-FT BY 115 GIVING C-AMT-PAINT-GAL.
           MULTIPLY C-AMT-PAINT-GAL BY PAINT-PRICE-GAL GIVING 
           C-PAINT-EST.
           COMPUTE C-LABOR-EST = C-AMT-PAINT-GAL * 3 * 23.55.
           ADD C-LABOR-EST TO C-PAINT-EST GIVING C-TOTAL-EST.

           ADD 1 TO C-GT-RECORDS.
           ADD C-AMT-PAINT-GAL TO C-GT-PAINT-GAL.
           ADD C-PAINT-EST TO C-GT-PAINT-EST.
           ADD C-LABOR-EST TO C-GT-LABOR-EST.
           ADD C-TOTAL-EST TO C-GT-TOTAL-EST.

       2200-OUTPUT.
           MOVE PAINT-EST-NO TO O-PAINT-EST-NO.
           MOVE PAINT-DATE TO O-PAINT-DATE.
           MOVE C-PAINT-EST TO O-PAINT-EST.
           MOVE C-LABOR-EST TO O-LABOR-EST.
           MOVE C-TOTAL-EST TO O-TOTAL-EST.
           MOVE PAINT-WALL-SQ-FT TO O-WALL-SQ-FT.
           MOVE PAINT-DOOR-SQ-FT TO O-DOOR-SQ-FT.
           MOVE C-TOTAL-SQ-FT TO O-TOTAL-SQ-FT.
           MOVE C-AMT-PAINT-GAL TO O-AMT-PAINT-GAL.
           MOVE PAINT-PRICE-GAL TO O-PRICE-GAL.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINES
               AT EOP
                   PERFORM 9100-HEADINGS.

       3000-CLOSING.
           PERFORM 3100-GRANDTOTALS.
           CLOSE PAINT-MASTER.
           CLOSE PRTOUT.

       3100-GRANDTOTALS.
           MOVE C-GT-RECORDS TO O-GT-RECORDS.
           MOVE C-GT-PAINT-GAL TO O-GT-PAINT-GAL.
           MOVE C-GT-PAINT-EST TO O-GT-PAINT-EST.
           MOVE C-GT-LABOR-EST TO O-GT-LABOR-EST.
           MOVE C-GT-TOTAL-EST TO O-GT-TOTAL-EST.

           WRITE PRTLINE FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES.

       9000-READ.
           READ PAINT-MASTER
           AT END
               MOVE "NO" TO MORE-RECS.

       9100-HEADINGS.
           ADD 1 TO C-PCTR
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COLUMN-HDINGS1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COLUMN-HDINGS2
               BEFORE ADVANCING 1 LINES.