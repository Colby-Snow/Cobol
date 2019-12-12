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
           05 I-PURCHASE-DATE          PIC 9(8).
           05 I-BOAT-TYPE              PIC X.
           05 I-ACCESSORY-PACKAGE      PIC 9.
           05 I-PREP-DELIVER-COST      PIC 9(5)V99.

       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE                      PIC X(132).

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
           05 FILLER               PIC X(9)    VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "DATE".
           05 FILLER               PIC X(15)   VALUE SPACES.
           05 FILLER               PIC X(7)    VALUE "PACKAGE".
           05 FILLER               PIC X(23)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "PREP".
           05 FILLER               PIC X(18)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE "COST".

       01 BOATHEADINGS.
           05 FILLER               PIC X(10)   VALUE "BOAT TYPE".
           05 O-BOAT-TYPE          PIC X(13).
           05 FILLER               PIC X(109)  VALUE SPACES.

       01 DETAIL-LINE.
           05 