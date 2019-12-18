       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBCMS02.
       DATE-WRITTEN.   12/12/2019.
       AUTHOR.         COLBY SNOW.
       DATE-COMPILED.

      ******************************************************************
      *THIS PROGRAM GENERATES A REPORT LISTING ALL PEOPLE WHO PURCHASED
      *BOATS FROM THE COMPANY.                       
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOATMASTER
           ASSIGN TO "C:\IHCC\COBOL\COBCMS02\CBLBOAT1.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
           ASSIGN TO "C:\IHCC\COBOL\COBCMS02\CBLBOAT.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD BOATMASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-BOAT-REC
           RECORD CONTAINS 23 CHARACTERS.

       01 I-BOAT-REC.
           05 I-LAST-NAME              PIC X(15).
           05 I-STATE                  PIC X(2).
           05 I-BOAT-COST              PIC 9(6)V99.
           05 I-PURCHASE-DATE.
               10 I-YY-PUR             PIC 99.
               10 I-PUR-YY             PIC 99.
               10 I-PUR-MM             PIC 99.
               10 I-PUR-DD             PIC 99.
           05 I-BOAT-TYPE              PIC X.
           05 I-ACCESSORY-PACKAGE      PIC 9.
           05 I-PREP-DELIVER-COST      PIC 9(5)V99.

       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE                      PIC X(132).

       WORKING-STORAGE SECTION.
       01 WORK-AREA.
           05 C-PCTR               PIC 9(2)        VALUE ZERO.
           05 MORE-RECS            PIC X(3)        VALUE "YES".
           05 C-TOT-COST           PIC 9(7)V99     VALUE ZERO.
           05 C-SOLD               PIC 9(4)        VALUE ZERO.
           05 C-MJ-TOT-COST        PIC 9(10)V99    VALUE ZERO.
           05 C-GT-TOT-COST        PIC 9(12)V99    VALUE ZERO.
           05 C-GT-SOLD            PIC 9(5)        VALUE ZERO.
           05 H-BOAT-TYPE          PIC X(13)       VALUE ZERO.

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
           05 FILLER               PIC X(41)   VALUE SPACES.
           05 FILLER               PIC X(16)   VALUE "SNOW'S BOATS INC".
           05 FILLER               PIC X(52)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "PAGE:".
           05 O-PCTR               PIC Z9.
       
       01 COLUMNHEADINGS1.
           05 FILLER               PIC X(8)    VALUE "CUSTOMER".
           05 FILLER               PIC X(36)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "BOAT".
           05 FILLER               PIC X(9)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE "PURCHASE".
           05 FILLER               PIC X(11)   VALUE SPACES.
           05 FILLER               PIC X(9)    VALUE "ACCESSORY".
           05 FILLER               PIC X(21)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "PREP".
           05 FILLER               PIC X(17)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "TOTAL".

       01 COLUMNHEADINGS2.
           05 FILLER               PIC X(9)    VALUE "LAST NAME".
           05 FILLER               PIC X(13)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE "STATE".
           05 FILLER               PIC X(16)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "COST".
           05 FILLER               PIC X(10)    VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "DATE".
           05 FILLER               PIC X(15)   VALUE SPACES.
           05 FILLER               PIC X(7)    VALUE "PACKAGE".
           05 FILLER               PIC X(23)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "COST".
           05 FILLER               PIC X(18)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "COST".

       01 BOATHEADINGS.
           05 FILLER               PIC X(10)   VALUE "BOAT TYPE".
           05 O-BOAT-TYPE          PIC X(13).
           05 FILLER               PIC X(109)  VALUE SPACES.

       01 DETAIL-LINE.
           05 O-LAST-NAME          PIC X(16).
           05 FILLER               PIC X(8)    VALUE SPACES.
           05 O-STATE              PIC X(2).
           05 FILLER               PIC X(12)   VALUE SPACES.
           05 O-BOAT-COST          PIC ZZZ,ZZZ.99.
           05 FILLER               PIC X(9)    VALUE SPACES.
           05 O-PURCHASE-DATE.
               10 O-PUR-MM         PIC 99.
               10 FILLER           PIC X       VALUE "/".
               10 O-PUR-DD         PIC 99.
               10 FILLER           PIC X       VALUE "/".
               10 O-PUR-YY         PIC 99.
           05 FILLER               PIC X(11)   VALUE SPACES.
           05 O-ACCESSORY-PACKAGE  PIC X(15).
           05 FILLER               PIC X(9)    VALUE SPACES.
           05 O-PREP-COST          PIC ZZZ,ZZZ.99.
           05 FILLER               PIC X(10)   VALUE SPACES.
           05 O-TOT-COST           PIC Z,ZZZ,ZZZ.99.

       01 SUBTOTAL-LINE.
           05 FILLER               PIC X(23)   VALUE SPACES.
           05 FILLER               PIC X(14)   VALUE "SUBTOTALS FOR ".
           05 O-BOAT-TYPE          PIC X(13).
           05 FILLER               PIC X(10)   VALUE SPACES.
           05 FILLER               PIC X(14)   VALUE "NUMBER SOLD:  ".
           05 O-MJ-SOLD            PIC Z,ZZ9.
           05 FILLER               PIC X(38).
           05 O-MJ-TOT-COST        PIC $$$$,$$$,$$$.99.

       01 GRANDTOTAL-LINE.
           05 FILLER               PIC X(24)   VALUE SPACES.
           05 FILLER               PIC X(12)   VALUE "GRAND TOTALS".
           05 FILLER               PIC X(24)   VALUE SPACES.
           05 FILLER               PIC X(13)   VALUE "NUMBER SOLD:  ".
           05 O-GT-SOLD            PIC ZZ,ZZ9.
           05 FILLER               PIC X(35).
           05 O-GT-TOT-COST        PIC $$$,$$$,$$$,$$$.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT BOATMASTER.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-MM TO O-MM.
           MOVE I-DD TO O-DD.

           PERFORM 9000-READ.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
           PERFORM 9200-HEADINGS.

       2000-MAINLINE.
           IF H-BOAT-TYPE NOT EQUAL TO I-BOAT-TYPE
               PERFORM 9100-MAJORSUBTOTALS
               PERFORM 9300-BOATHEADINGS.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.
       
       2100-CALCS.
           ADD I-BOAT-COST TO I-PREP-DELIVER-COST GIVING C-TOT-COST.
           ADD C-TOT-COST TO C-MJ-TOT-COST.
           ADD 1 TO C-SOLD.

       2200-OUTPUT.
           EVALUATE I-ACCESSORY-PACKAGE
               WHEN 1
                   MOVE "ELECTRONICS" TO O-ACCESSORY-PACKAGE
               WHEN 2
                   MOVE "SKI PACKAGE" TO O-ACCESSORY-PACKAGE
               WHEN 3
                   MOVE "FISHING PACKAGE" TO O-ACCESSORY-PACKAGE.

           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-PUR-YY TO O-PUR-YY.
           MOVE I-PUR-DD TO O-PUR-DD.
           MOVE I-PUR-MM TO O-PUR-MM.
           MOVE I-PREP-DELIVER-COST TO O-PREP-COST.
           MOVE C-TOT-COST TO O-TOT-COST.

           WRITE PRTLINE FROM DETAIL-LINE
               AT EOP
                   PERFORM 9200-HEADINGS.

       3000-CLOSING.
           PERFORM 9100-MAJORSUBTOTALS.
           PERFORM 3100-GRANDTOTALS.
           CLOSE BOATMASTER.
           CLOSE PRTOUT.
           

       3100-GRANDTOTALS.
           MOVE C-GT-SOLD TO O-GT-SOLD.
           MOVE C-GT-TOT-COST TO O-GT-TOT-COST.

           WRITE PRTLINE FROM GRANDTOTAL-LINE
               AFTER ADVANCING 3 LINES.

       9000-READ.
           READ BOATMASTER
               AT END
                   MOVE "NO" TO MORE-RECS.

       9100-MAJORSUBTOTALS.
           MOVE C-MJ-TOT-COST TO O-MJ-TOT-COST.
           MOVE C-SOLD TO O-MJ-SOLD.

           WRITE PRTLINE FROM SUBTOTAL-LINE
               AFTER ADVANCING 2
               AT EOP
                   PERFORM 9200-HEADINGS.

           ADD C-SOLD TO C-GT-SOLD.
           ADD C-MJ-TOT-COST TO C-GT-TOT-COST.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
           MOVE ZERO TO C-SOLD.
           MOVE ZERO TO C-MJ-TOT-COST.

       9200-HEADINGS.
           ADD 1 TO C-PCTR
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM  COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COLUMNHEADINGS1
               AFTER ADVANCING 2 LINES. 
           WRITE PRTLINE FROM COLUMNHEADINGS2
               BEFORE ADVANCING 2 LINES.
           PERFORM 9300-BOATHEADINGS.

       9300-BOATHEADINGS.
           EVALUATE I-BOAT-TYPE
               WHEN "B"
                   MOVE "BASS BOAT" TO O-BOAT-TYPE OF BOATHEADINGS
                   MOVE "BASS BOAT" TO O-BOAT-TYPE OF SUBTOTAL-LINE
               WHEN "P"
                   MOVE "PONTOON" TO O-BOAT-TYPE OF BOATHEADINGS
                   MOVE "PONTOON" TO O-BOAT-TYPE OF SUBTOTAL-LINE
               WHEN "S"
                   MOVE "SKI BOAT" TO O-BOAT-TYPE OF BOATHEADINGS
                   MOVE "SKI BOAT" TO O-BOAT-TYPE OF SUBTOTAL-LINE
               WHEN "J"
                   MOVE "JOHN BOAT" TO O-BOAT-TYPE OF BOATHEADINGS
                   MOVE "JOHN BOAT" TO O-BOAT-TYPE OF SUBTOTAL-LINE
               WHEN "C"
                   MOVE "CANOE" TO O-BOAT-TYPE OF BOATHEADINGS
                   MOVE "CANOE" TO O-BOAT-TYPE OF SUBTOTAL-LINE
               WHEN "R"
                   MOVE "CABIN CRUISER" TO O-BOAT-TYPE OF BOATHEADINGS
                   MOVE "CABIN CRUISER" TO O-BOAT-TYPE OF SUBTOTAL-LINE.
                   
           WRITE PRTLINE FROM BOATHEADINGS
               BEFORE ADVANCING 1.
