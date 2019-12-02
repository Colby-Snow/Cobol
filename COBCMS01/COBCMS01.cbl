       IDENTIFICATION DIVISION.
       program-id.     COBCMS01.
       DATE-WRITTEN.   12/2/2019.
       AUTHOR.         COLBY SNOW.
       DATE-COMPILED.
      ******************************************************************
      *COOL PROGRAM RULES HERE
      *LINE SEQUENTIAL IS ONLY FOR .DAT FILES
      *.PRT FILES USE RECORD SEQUENTIAL
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT PAINT-MASTER
           ASSIGN TO "C\IHCC\COBOL\PAINTEST.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
           ASSIGN TO "C\IHCC\COBOL\PJOBEST.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PAINT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-PAINT-REC
           RECORD CONTAINS 49 CHARACTERS.

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
           05 C-PAINT-EST          PIC 99V99.
           05 C-LABOR-EST          PIC 99V99.
           05 C-TOTAL-EST          PIC 99V99.
           05 C-AMT-PAINT-GAL      PIC 99.
           05 C-GT-PAINT-EST       PIC 999V99  VALUE 0.
           05 C-GT-LABOR-EST       PIC 999V99  VALUE 0.
           05 C-GT-PAINT-GAL       PIC 999     VALUE 0.
           05 C-GT-RECORDS         PIC 999     VALUE 0.

       01 CURRENT-DATE-AND-TIME.
           05 I-DATE.
             10 I-YY               PIC 9(4).
             10 I-MM               PIC 99.
             10 I-DD               PIC 99.
           05 I-TIME               PIC X(11).

