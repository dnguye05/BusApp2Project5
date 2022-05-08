      ******************************************************************
      *PROGRAM : PROJECT 4   MASTER/TRANSACTION PROCESSING             *
      *AUTHOR  : David Nguyen                                          *
      *DATE    : 02/08/2020                                            *
      *ABSTRACT: File Processing                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NGUYEN-P04-MSTR-TRANS. 
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-MST      ASSIGN TO 'p04-mstr.txt'
                                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS         ASSIGN TO 'p04-trans.txt'
                                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-WORK     ASSIGN TO 'p04-trans-sortwork.txt'.
           SELECT RPT-FILE      ASSIGN TO 'p04-report.rpt'
                                ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-MST.
       01  CUST-REC.
           03  CUST-ID                     PIC X(5).
           03  CUST-NAME                   PIC X(20).
           03  CUST-BAL                    PIC 9(5)V99.
       
       FD  TRANS.
       01  TRANS-REC.
           03  TRANS-ID                    PIC 9(5).
           03  TRANS-DATE.
               05  TRANS-YR                PIC 9999.
               05  TRANS-MO                PIC 99.
               05  TRANS-DAY               PIC 99.
           03  TRANS-DESC                  PIC X(20).
           03  TRANS-AMT                   PIC 9(5)V99.

       SD  SORT-WORK.
       01  SORT-REC.
           03  SORT-TRANS-ID               PIC 9(5).
           03  SORT-TRANS-DATE.
               05  SORT-TRANS-YR           PIC 9999.
               05  SORT-TRANS-MO           PIC 99.
               05  SORT-TRANS-DAY          PIC 99.
           03  SORT-TRANS-DESC             PIC X(20).
           03  SORT-TRANS-AMT              PIC 9(5)V99.
       
       FD  RPT-FILE.  
       01  RPT-REC                         PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       COPY SYS-DATE-TIME-WS.
       01  WS-TITLE-LN.
           03  FILLER                      PIC X(28) 
                     VALUE 'P04-NGUYEN'.
           03  FILLER                      PIC X(42) 
                     VALUE 'CUSTOMER ACCOUNT REPORT'.
           03  WS-TITLE-DATE               PIC X(10).
           
       01  WS-RPT-BEG-BAL-LN.    
           03  FILLER                      PIC X(2)        VALUE SPACES.
           03  WS-RPT-CUST-ID              PIC X(5).
           03  FILLER                      PIC X           VALUE SPACES.
           03  WS-RPT-CUST-NAME            PIC X(20).
           03  FILLER                      PIC X(33)       VALUE SPACES.
           03  WS-RPT-CUST-BEG-BAL         PIC ZZ,ZZ9.99.
           03  FILLER                      PIC X(10)   VALUE ' BEG BAL'.

       01  WS-RPT-TRANS-LN.
           03  FILLER                      PIC X(20)       VALUE SPACES.
           03  WS-RPT-TRANS-ID             PIC X(5).
           03  FILLER                      PIC XX          VALUE SPACES.
           03  WS-RPT-TRANS-MO             PIC 99.
           03  FILLER                      PIC X           VALUE '/'.
           03  WS-RPT-TRANS-DAY            PIC 99.
           03  FILLER                      PIC X           VALUE '/'.
           03  WS-RPT-TRANS-YR             PIC 9999.
           03  FILLER                      PIC XX          VALUE SPACES.
           03  WS-RPT-TRANS-DESC           PIC X(20).
           03  FILLER                      PIC XX          VALUE SPACES.
           03  WS-RPT-TRANS-AMT            PIC ZZ,ZZ9.99.
           03  FILLER                      PIC X(10)       VALUE SPACES.

       01  WS-RPT-END-BAL-LN.
           03  FILLER                      PIC X(61)   VALUE SPACES.
           03  WS-RPT-END-BAL              PIC ZZ,ZZ9.99.
           03  FILLER                      PIC X(10)   VALUE ' END BAL'.

       01  WS-FLAGS.
           03  WS-EOF-FLAG                 PIC X           VALUE 'N'.
               88  EOF-TRANS                               VALUE 'Y'.
           03  WS-FIRST-FLAG               PIC X           VALUE 'Y'.
               88  FIRST-REC                               VALUE 'Y'.

       01  WS-MISC-VARS.
           03  WS-CUST-RUNNING-BAL         PIC 9(5)V99     VALUE ZERO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT CUST-MST.
           OPEN OUTPUT RPT-FILE.
           
           DISPLAY 'Project 4 - David Nguyen'.
           SORT SORT-WORK ON ASCENDING KEY    SORT-TRANS-ID
                             ASCENDING KEY    SORT-TRANS-DATE
                             USING            TRANS
                             OUTPUT PROCEDURE 200-PRINT-OUTPUT.
           DISPLAY 'End of run'.
           
           CLOSE RPT-FILE
                 CUST-MST.
           STOP RUN.
      *-----------------------------------------------------------------
       200-PRINT-OUTPUT.
           PERFORM 300-PRINT-TITLE-LN.
           READ CUST-MST.
           PERFORM 400-WRITE-BEG-BAL-LN.
           PERFORM UNTIL EOF-TRANS
               RETURN SORT-WORK
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                       PERFORM 600-WRITE-END-BAL-LN
                   NOT AT END
                       IF SORT-TRANS-ID EQUAL WS-RPT-CUST-ID
                           PERFORM 500-WRITE-TRANSACTION-LN
                       ELSE
                           PERFORM 600-WRITE-END-BAL-LN
                           READ CUST-MST
                           PERFORM 400-WRITE-BEG-BAL-LN
                           PERFORM 500-WRITE-TRANSACTION-LN
                       END-IF

      *                I FOUND ANOTHER SOLUTION
      *                IF SORT-TRANS-ID NOT EQUAL TO WS-RPT-TRANS-ID
      *                    AND WS-FIRST-FLAG EQUAL 'N'
      *                    PERFORM 600-WRITE-END-BAL-LN
      *                    READ CUST-MST
      *                    PERFORM 400-WRITE-BEG-BAL-LN
      *                END-IF
      *                IF FIRST-REC
      *                    MOVE 'N' TO WS-FIRST-FLAG
      *                END-IF
      *                PERFORM 500-WRITE-TRANSACTION-LN
      *        END-RETURN
      
           END-PERFORM.
      *-----------------------------------------------------------------
       300-PRINT-TITLE-LN.
           COPY  SYS-DATE-TIME-MOVE.
           MOVE  WS-FMTD-DATE TO WS-TITLE-DATE.
           WRITE RPT-REC FROM WS-TITLE-LN.
           WRITE RPT-REC FROM SPACES.
      *-----------------------------------------------------------------
       400-WRITE-BEG-BAL-LN.
           MOVE  CUST-ID               TO   WS-RPT-CUST-ID.
           MOVE  CUST-NAME             TO   WS-RPT-CUST-NAME.
           MOVE  CUST-BAL              TO   WS-RPT-CUST-BEG-BAL.
           WRITE RPT-REC               FROM WS-RPT-BEG-BAL-LN.
           MOVE  CUST-BAL              TO   WS-CUST-RUNNING-BAL.
      *-----------------------------------------------------------------
       500-WRITE-TRANSACTION-LN.
           MOVE  SORT-TRANS-ID         TO   WS-RPT-TRANS-ID.
           MOVE  SORT-TRANS-MO         TO   WS-RPT-TRANS-MO.
           MOVE  SORT-TRANS-DAY        TO   WS-RPT-TRANS-DAY.
           MOVE  SORT-TRANS-YR         TO   WS-RPT-TRANS-YR.
           MOVE  SORT-TRANS-DESC       TO   WS-RPT-TRANS-DESC.
           MOVE  SORT-TRANS-AMT        TO   WS-RPT-TRANS-AMT.
           WRITE RPT-REC               FROM WS-RPT-TRANS-LN.
           ADD   SORT-TRANS-AMT        TO   WS-CUST-RUNNING-BAL.
      *-----------------------------------------------------------------
       600-WRITE-END-BAL-LN.
           MOVE  WS-CUST-RUNNING-BAL   TO   WS-RPT-END-BAL.
           WRITE RPT-REC               FROM WS-RPT-END-BAL-LN.
           WRITE RPT-REC               FROM SPACES.             
           WRITE RPT-REC               FROM SPACES.             
      *-----------------------------------------------------------------
