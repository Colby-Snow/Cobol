       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBCMS00.
       DATE-WRITTEN.   11/20/2019.
       AUTHOR.         COLBY SNOW.
       DATE-COMPILED.
      *----------------------------------------------------------------
      *    THIS PROGRAM READS A FILE AND CREATES A STUDENT ROSTER
      *    REPORT.
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STUDENT-MASTER
               ASSIGN TO "C:\COBOL\STDNTMST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO "C:\COBOL\STDNTRPT.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  STUDENT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 49 CHARACTERS.

       01 I-REC.
         05 I-ID           PIC X(7).
         05 I-NAME.
           10 I-LNAME      PIC X(15).
           10 I-FNAME      PIC X(15).
           10 I-INIT       PIC X.
         05 I-GPA          PIC 9V99.
         05 I-EX-STRT-SAL  PIC 9(6)V99.

       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE          PIC X(132).

       WORKING-STORAGE SECTION.
       01 WORK-AREA
           05 C-SCTR       PIC 9(3)    VALUE 0.
           05 C-PCTR       PIC 9(2)    VALUE 0.
           05 MORE-RECS    PIC X(3)    VALUE "NO".

       01 CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10  I-YY    PIC 9(4).
               10  I-MM    PIC 99.
               10  I-DD    PIC 99.
           05  I-TIME      PIC X(11).

       01 COMPANY-TITLE.
           05  FILLER      PIC X(6)    VALUE "DATE:".
           05  O-MM        PIC 9(2).
           05  FILLER      PIC X       VALUE "/".
           05  O-DD        PIC 9(2).
           05  FILLER      PIC X       VALUE "/".
           05  O-YY        PIC 9(4).
           05  FILLER      PIC X(35)   VALUE SPACES.
           05  FILLER      PIC X(29)
                           VALUE "SNOW'S COBOL STUDENT ROSTER".
           05  FILLER      PIC X(44)   VALUE SPACES.
           05  FILLER      PIC X(6)    VALUE "PAGE:".
           05  O-PCTR      PIC Z9.


