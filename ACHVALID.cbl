       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACHVALID.
      *================================================================
      * ACHVALID - NACHA ACH FILE VALIDATION
      *
      * READS AN INCOMING NACHA-FORMAT ACH FILE AND VALIDATES IT AT
      * TWO LEVELS:
      *
      *  FILE LEVEL  - RECORD SEQUENCE (1-5-6-7-8-9), HEADER FIELDS,
      *                BATCH CONTROL TOTALS, FILE CONTROL TOTALS AND
      *                BLOCKING.  ANY FILE-LEVEL ERROR REJECTS THE
      *                WHOLE FILE (RETURN CODE 12) AND POSTING IS
      *                SKIPPED BY THE JCL.
      *
      *  ENTRY LEVEL - TRANSACTION CODE, ROUTING NUMBER CHECK DIGIT,
      *                AMOUNT, ACCOUNT NUMBER, TRACE NUMBER, ADDENDA
      *                INDICATOR.  BAD ENTRIES GO TO THE REJECT FILE
      *                (RETURN CODE 4); GOOD ENTRIES GO TO VALIDOUT
      *                FOR POSTING.
      *
      * CONTROL TOTALS ARE ACCUMULATED OVER EVERY ENTRY READ
      * (INCLUDING REJECTED ONES), BECAUSE THE ORIGINATOR'S CONTROL
      * RECORDS COVER EVERY ENTRY IN THE BATCH.
      *
      * FILES:  ACHIN    - INPUT ACH FILE          FB  94
      *         VALIDOUT - VALID ENTRIES           FB 100
      *         REJECTS  - REJECTED ENTRIES        FB 130
      *         VALRPT   - VALIDATION REPORT       FB 133
      *
      * RETURN CODES: 0  FILE ACCEPTED, NO REJECTS
      *               4  FILE ACCEPTED, SOME ENTRIES REJECTED
      *               12 FILE REJECTED (FILE-LEVEL ERRORS)
      *               16 I/O ERROR
      *
      * PORTFOLIO PROJECT - ALL DATA IS SYNTHETIC.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACH-IN-FILE   ASSIGN TO ACHIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-ACHIN-STATUS.
           SELECT VALID-FILE    ASSIGN TO VALIDOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-VALID-STATUS.
           SELECT REJECT-FILE   ASSIGN TO REJECTS
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-REJECT-STATUS.
           SELECT REPORT-FILE   ASSIGN TO VALRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACH-IN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 94 CHARACTERS.
       01  ACH-IN-REC                      PIC X(94).

       FD  VALID-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  VALID-OUT-REC                   PIC X(100).

       FD  REJECT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 130 CHARACTERS.
       01  REJECT-OUT-REC                  PIC X(130).

       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REPORT-REC                      PIC X(133).

       WORKING-STORAGE SECTION.
      *--- FILE STATUS CODES -------------------------------------------
       01  WS-FILE-STATUSES.
           05  WS-ACHIN-STATUS             PIC XX.
               88  ACHIN-OK                VALUE '00'.
           05  WS-VALID-STATUS             PIC XX.
               88  VALID-OK                VALUE '00'.
           05  WS-REJECT-STATUS            PIC XX.
               88  REJECT-OK               VALUE '00'.
           05  WS-REPORT-STATUS            PIC XX.
               88  REPORT-OK               VALUE '00'.

      *--- SWITCHES AND RECORD-SEQUENCE STATE --------------------------
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  END-OF-FILE                       VALUE 'Y'.
           05  WS-STATE                    PIC X     VALUE '0'.
               88  ST-START                          VALUE '0'.
               88  ST-AFTER-FHDR                     VALUE 'F'.
               88  ST-IN-BATCH                       VALUE 'B'.
               88  ST-AFTER-ENTRY                    VALUE 'E'.
               88  ST-AFTER-BCTL                     VALUE 'C'.
               88  ST-AFTER-FCTL                     VALUE 'Z'.
           05  WS-ADDENDA-SW               PIC X     VALUE 'N'.
               88  ADDENDA-EXPECTED                  VALUE 'Y'.
               88  ADDENDA-NOT-EXPECTED              VALUE 'N'.
           05  WS-ENTRY-SW                 PIC X     VALUE 'Y'.
               88  ENTRY-IS-VALID                    VALUE 'Y'.
               88  ENTRY-IS-REJECTED                 VALUE 'N'.

      *--- RECORD LAYOUTS ----------------------------------------------
           COPY NACHAREC.
           COPY ACHVREC.

       01  WS-REJECT-REC.
           05  RJ-RAW                      PIC X(94).
           05  FILLER                      PIC X     VALUE SPACE.
           05  RJ-REASON-CODE              PIC X(4).
           05  FILLER                      PIC X     VALUE SPACE.
           05  RJ-REASON-TEXT              PIC X(30).

      *--- COUNTERS AND TOTALS -----------------------------------------
       01  WS-COUNTERS.
           05  WS-REC-COUNT                PIC 9(8)       VALUE 0.
           05  WS-BATCH-COUNT              PIC 9(6)       VALUE 0.
           05  WS-FILE-ERR-COUNT           PIC 9(6)       VALUE 0.
           05  WS-ERRS-AT-BATCH-START      PIC 9(6)       VALUE 0.
           05  WS-VALID-COUNT              PIC 9(8)       VALUE 0.
           05  WS-REJECT-COUNT             PIC 9(8)       VALUE 0.
           05  WS-VALID-CREDIT-AMT         PIC 9(10)V99   VALUE 0.
           05  WS-VALID-DEBIT-AMT          PIC 9(10)V99   VALUE 0.

       01  WS-BATCH-TOTALS.
           05  WB-ENTRY-COUNT              PIC 9(8)       VALUE 0.
           05  WB-ENTRY-HASH               PIC 9(15)      VALUE 0.
           05  WB-DEBIT                    PIC 9(10)V99   VALUE 0.
           05  WB-CREDIT                   PIC 9(10)V99   VALUE 0.
           05  WB-SVC-CLASS                PIC X(3)       VALUE SPACES.
           05  WB-BATCH-NO                 PIC X(7)       VALUE SPACES.
           05  WB-ODFI                     PIC X(8)       VALUE SPACES.
           05  WB-EFF-DATE                 PIC X(6)       VALUE SPACES.
           05  WB-CO-NAME                  PIC X(16)      VALUE SPACES.

       01  WS-FILE-TOTALS.
           05  WF-ENTRY-COUNT              PIC 9(8)       VALUE 0.
           05  WF-ENTRY-HASH               PIC 9(15)      VALUE 0.
           05  WF-DEBIT                    PIC 9(10)V99   VALUE 0.
           05  WF-CREDIT                   PIC 9(10)V99   VALUE 0.
           05  WF-BLOCK-COUNT-CTL          PIC 9(6)       VALUE 0.

       01  WS-WORK-FIELDS.
           05  WS-HASH-10                  PIC 9(10)      VALUE 0.
           05  WS-CHK-SUM                  PIC 9(4)       VALUE 0.
           05  WS-CHK-CALC                 PIC 9          VALUE 0.
           05  WS-EXP-BLOCKS               PIC 9(8)       VALUE 0.
           05  WS-REMAINDER                PIC 9(4)       VALUE 0.
           05  WS-ERR-MSG                  PIC X(60)      VALUE SPACES.
           05  WS-RC                       PIC 9(4)       VALUE 0.

      *--- REPORT LINES ------------------------------------------------
       01  RPT-TITLE-1.
           05  FILLER                      PIC X(45)      VALUE
               'ACHVALID - NACHA ACH FILE VALIDATION REPORT'.
       01  RPT-TITLE-2.
           05  FILLER                      PIC X(45)      VALUE
               '============================================='.
       01  RPT-FILE-INFO.
           05  FILLER                      PIC X(10)      VALUE
               'ORIGIN:  '.
           05  RPT-FI-ORIGIN               PIC X(23).
           05  FILLER                      PIC X(16)      VALUE
               '  CREATED (YMD):'.
           05  RPT-FI-DATE                 PIC X(6).
           05  FILLER                      PIC X(7)       VALUE
               '  TIME:'.
           05  RPT-FI-TIME                 PIC X(4).
       01  RPT-ERROR-LINE.
           05  FILLER                      PIC X(24)      VALUE
               '*** FILE ERROR  RECORD '.
           05  RPT-ERR-RECNO               PIC Z(7)9.
           05  FILLER                      PIC X(3)       VALUE ' : '.
           05  RPT-ERR-MSG                 PIC X(60).
       01  RPT-REJECT-LINE.
           05  FILLER                      PIC X(16)      VALUE
               '    REJECT TRACE'.
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-RJ-TRACE                PIC X(15).
           05  FILLER                      PIC X(6)       VALUE
               ' ACCT '.
           05  RPT-RJ-ACCOUNT              PIC X(17).
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-RJ-CODE                 PIC X(4).
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-RJ-TEXT                 PIC X(30).
       01  RPT-BATCH-LINE.
           05  FILLER                      PIC X(6)       VALUE
               'BATCH '.
           05  RPT-BT-BATCH-NO             PIC X(7).
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-BT-CO-NAME              PIC X(16).
           05  FILLER                      PIC X(5)       VALUE
               ' SVC '.
           05  RPT-BT-SVC                  PIC X(3).
           05  FILLER                      PIC X(6)       VALUE
               '  CNT '.
           05  RPT-BT-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(5)       VALUE
               '  DR '.
           05  RPT-BT-DEBIT                PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(5)       VALUE
               '  CR '.
           05  RPT-BT-CREDIT               PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(2)       VALUE SPACES.
           05  RPT-BT-STATUS               PIC X(15).
       01  RPT-COUNT-LINE.
           05  RPT-CT-LABEL                PIC X(40).
           05  RPT-CT-VALUE                PIC ZZ,ZZZ,ZZ9.
       01  RPT-AMOUNT-LINE.
           05  RPT-AM-LABEL                PIC X(40).
           05  RPT-AM-VALUE                PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  RPT-TEXT-LINE                   PIC X(133).

       PROCEDURE DIVISION.
      *================================================================
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-RECORD
               UNTIL END-OF-FILE
           PERFORM 8000-END-OF-FILE-CHECKS
           PERFORM 9000-FINALIZE
           GOBACK.

      *================================================================
       1000-INITIALIZE.
           OPEN INPUT ACH-IN-FILE
           IF NOT ACHIN-OK
               DISPLAY 'ACHVALID: OPEN ACHIN FAILED, STATUS '
                       WS-ACHIN-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT VALID-FILE
                       REJECT-FILE
                       REPORT-FILE
           IF NOT VALID-OK OR NOT REJECT-OK OR NOT REPORT-OK
               DISPLAY 'ACHVALID: OPEN OUTPUT FAILED, STATUS '
                       WS-VALID-STATUS ' ' WS-REJECT-STATUS ' '
                       WS-REPORT-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           WRITE REPORT-REC FROM RPT-TITLE-1
           WRITE REPORT-REC FROM RPT-TITLE-2
           PERFORM 1100-READ-ACH.

       1100-READ-ACH.
           READ ACH-IN-FILE INTO WS-ACH-RECORD
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-REC-COUNT
           END-READ
           IF NOT END-OF-FILE AND NOT ACHIN-OK
               DISPLAY 'ACHVALID: READ ERROR, STATUS '
                       WS-ACHIN-STATUS ' AT RECORD ' WS-REC-COUNT
               MOVE 16 TO RETURN-CODE
               PERFORM 9100-CLOSE-FILES
               STOP RUN
           END-IF.

      *================================================================
      * ROUTE EACH RECORD BY ITS NACHA RECORD TYPE CODE
      *================================================================
       2000-PROCESS-RECORD.
           EVALUATE ACH-REC-TYPE
               WHEN '1'
                   PERFORM 2100-FILE-HEADER
               WHEN '5'
                   PERFORM 2200-BATCH-HEADER
               WHEN '6'
                   PERFORM 2300-ENTRY-DETAIL
               WHEN '7'
                   PERFORM 2400-ADDENDA
               WHEN '8'
                   PERFORM 2500-BATCH-CONTROL
               WHEN '9'
                   PERFORM 2600-FILE-CONTROL-OR-FILLER
               WHEN OTHER
                   MOVE 'INVALID RECORD TYPE CODE' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
           END-EVALUATE
           PERFORM 1100-READ-ACH.

      *----------------------------------------------------------------
       2100-FILE-HEADER.
           IF NOT ST-START
               MOVE 'FILE HEADER OUT OF SEQUENCE' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF FH-PRIORITY NOT = '01'
               MOVE 'FILE HEADER PRIORITY CODE IS NOT 01'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF FH-REC-SIZE NOT = '094'
               MOVE 'FILE HEADER RECORD SIZE IS NOT 094'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF FH-BLOCK-FACTOR NOT = '10'
               MOVE 'FILE HEADER BLOCKING FACTOR IS NOT 10'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF FH-FORMAT-CODE NOT = '1'
               MOVE 'FILE HEADER FORMAT CODE IS NOT 1'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           MOVE FH-ORIGIN-NAME  TO RPT-FI-ORIGIN
           MOVE FH-CREATE-DATE  TO RPT-FI-DATE
           MOVE FH-CREATE-TIME  TO RPT-FI-TIME
           WRITE REPORT-REC FROM RPT-FILE-INFO
           MOVE SPACES TO RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TEXT-LINE
           SET ST-AFTER-FHDR TO TRUE.

      *----------------------------------------------------------------
       2200-BATCH-HEADER.
           IF NOT (ST-AFTER-FHDR OR ST-AFTER-BCTL)
               MOVE 'BATCH HEADER OUT OF SEQUENCE' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF NOT BH-SVC-VALID
               MOVE 'BATCH HEADER SERVICE CLASS CODE INVALID'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF NOT BH-SEC-SUPPORTED
               MOVE 'BATCH HEADER SEC CODE NOT SUPPORTED'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           MOVE ZERO            TO WB-ENTRY-COUNT
                                   WB-ENTRY-HASH
                                   WB-DEBIT
                                   WB-CREDIT
           MOVE BH-SVC-CLASS    TO WB-SVC-CLASS
           MOVE BH-BATCH-NO     TO WB-BATCH-NO
           MOVE BH-ODFI-ID      TO WB-ODFI
           MOVE BH-EFF-DATE     TO WB-EFF-DATE
           MOVE BH-CO-NAME      TO WB-CO-NAME
           MOVE WS-FILE-ERR-COUNT TO WS-ERRS-AT-BATCH-START
           ADD 1 TO WS-BATCH-COUNT
           SET ST-IN-BATCH TO TRUE.

      *----------------------------------------------------------------
       2300-ENTRY-DETAIL.
           IF NOT (ST-IN-BATCH OR ST-AFTER-ENTRY)
               MOVE 'ENTRY DETAIL RECORD OUTSIDE A BATCH'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF ADDENDA-EXPECTED
               MOVE 'ADDENDA RECORD MISSING' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
               SET ADDENDA-NOT-EXPECTED TO TRUE
           END-IF
           ADD 1 TO WB-ENTRY-COUNT
           PERFORM 2310-ACCUMULATE-CONTROLS
           PERFORM 2320-EDIT-ENTRY
           IF ENTRY-IS-VALID
               PERFORM 2330-WRITE-VALID
           ELSE
               PERFORM 2340-WRITE-REJECT
           END-IF
           IF EN-HAS-ADDENDA
               SET ADDENDA-EXPECTED TO TRUE
           END-IF
           SET ST-AFTER-ENTRY TO TRUE.

      *----------------------------------------------------------------
      * ADD THIS ENTRY TO THE BATCH CONTROL TOTALS.
      * ENTRY HASH = SUM OF THE 8-DIGIT RDFI ROUTING NUMBERS.
      *----------------------------------------------------------------
       2310-ACCUMULATE-CONTROLS.
           IF EN-RDFI-ROUTING IS NUMERIC
               ADD EN-RDFI-NUM TO WB-ENTRY-HASH
           END-IF
           IF EN-AMOUNT IS NUMERIC
               IF EN-TXN-CREDIT
                   ADD EN-AMOUNT-N TO WB-CREDIT
               END-IF
               IF EN-TXN-DEBIT
                   ADD EN-AMOUNT-N TO WB-DEBIT
               END-IF
           END-IF.

      *----------------------------------------------------------------
      * ENTRY-LEVEL EDITS.  THE FIRST FAILED EDIT REJECTS THE ENTRY.
      *----------------------------------------------------------------
       2320-EDIT-ENTRY.
           SET ENTRY-IS-VALID TO TRUE
           MOVE SPACES TO RJ-REASON-CODE
                          RJ-REASON-TEXT
           IF NOT EN-TXN-POSTABLE
               MOVE 'E001' TO RJ-REASON-CODE
               MOVE 'TRANSACTION CODE NOT SUPPORTED'
                 TO RJ-REASON-TEXT
               SET ENTRY-IS-REJECTED TO TRUE
           END-IF
           IF ENTRY-IS-VALID
               IF EN-RDFI-ROUTING IS NOT NUMERIC
                  OR EN-CHECK-DIGIT IS NOT NUMERIC
                   MOVE 'E002' TO RJ-REASON-CODE
                   MOVE 'ROUTING NUMBER NOT NUMERIC'
                     TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF
           IF ENTRY-IS-VALID
               PERFORM 2325-COMPUTE-CHECK-DIGIT
               IF WS-CHK-CALC NOT = EN-CHECK-DIGIT
                   MOVE 'E003' TO RJ-REASON-CODE
                   MOVE 'ROUTING CHECK DIGIT INVALID'
                     TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF
           IF ENTRY-IS-VALID
               IF EN-AMOUNT IS NOT NUMERIC
                   MOVE 'E004' TO RJ-REASON-CODE
                   MOVE 'AMOUNT NOT NUMERIC' TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF
           IF ENTRY-IS-VALID
               IF EN-AMOUNT-N = ZERO
                   MOVE 'E005' TO RJ-REASON-CODE
                   MOVE 'ZERO AMOUNT ON LIVE ENTRY'
                     TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF
           IF ENTRY-IS-VALID
               IF EN-ACCOUNT = SPACES
                   MOVE 'E006' TO RJ-REASON-CODE
                   MOVE 'ACCOUNT NUMBER MISSING' TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF
           IF ENTRY-IS-VALID
               IF EN-TRACE-ODFI NOT = WB-ODFI
                   MOVE 'E007' TO RJ-REASON-CODE
                   MOVE 'TRACE NUMBER ODFI MISMATCH'
                     TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF
           IF ENTRY-IS-VALID
               IF NOT EN-ADDENDA-VALID
                   MOVE 'E008' TO RJ-REASON-CODE
                   MOVE 'ADDENDA INDICATOR INVALID'
                     TO RJ-REASON-TEXT
                   SET ENTRY-IS-REJECTED TO TRUE
               END-IF
           END-IF.

      *----------------------------------------------------------------
      * ABA ROUTING CHECK DIGIT: WEIGHTS 3 7 1 3 7 1 3 7 ON DIGITS 1-8
      * CHECK DIGIT = (10 - (WEIGHTED SUM MOD 10)) MOD 10
      *----------------------------------------------------------------
       2325-COMPUTE-CHECK-DIGIT.
           COMPUTE WS-CHK-SUM =
                 EN-RDFI-DIGIT (1) * 3
               + EN-RDFI-DIGIT (2) * 7
               + EN-RDFI-DIGIT (3) * 1
               + EN-RDFI-DIGIT (4) * 3
               + EN-RDFI-DIGIT (5) * 7
               + EN-RDFI-DIGIT (6) * 1
               + EN-RDFI-DIGIT (7) * 3
               + EN-RDFI-DIGIT (8) * 7
           COMPUTE WS-CHK-CALC =
               FUNCTION MOD (10 - FUNCTION MOD (WS-CHK-SUM, 10), 10).

      *----------------------------------------------------------------
       2330-WRITE-VALID.
           MOVE SPACES          TO VALID-ENTRY-REC
           MOVE EN-ACCOUNT      TO VE-ACCOUNT
           MOVE EN-TXN-CODE     TO VE-TXN-CODE
           MOVE EN-AMOUNT-N     TO VE-AMOUNT
           MOVE EN-RDFI-ROUTING TO VE-RDFI-ID
           MOVE EN-CHECK-DIGIT  TO VE-RDFI-CHECK
           MOVE EN-TRACE-NO     TO VE-TRACE-NO
           MOVE EN-INDIV-NAME   TO VE-INDIV-NAME
           MOVE WB-BATCH-NO     TO VE-BATCH-NO
           MOVE WB-EFF-DATE     TO VE-EFF-DATE
           WRITE VALID-OUT-REC FROM VALID-ENTRY-REC
           IF NOT VALID-OK
               DISPLAY 'ACHVALID: WRITE VALIDOUT FAILED, STATUS '
                       WS-VALID-STATUS
               MOVE 16 TO RETURN-CODE
               PERFORM 9100-CLOSE-FILES
               STOP RUN
           END-IF
           ADD 1 TO WS-VALID-COUNT
           IF EN-TXN-CREDIT
               ADD EN-AMOUNT-N TO WS-VALID-CREDIT-AMT
           ELSE
               ADD EN-AMOUNT-N TO WS-VALID-DEBIT-AMT
           END-IF.

      *----------------------------------------------------------------
       2340-WRITE-REJECT.
           MOVE ACH-RAW TO RJ-RAW
           WRITE REJECT-OUT-REC FROM WS-REJECT-REC
           IF NOT REJECT-OK
               DISPLAY 'ACHVALID: WRITE REJECTS FAILED, STATUS '
                       WS-REJECT-STATUS
               MOVE 16 TO RETURN-CODE
               PERFORM 9100-CLOSE-FILES
               STOP RUN
           END-IF
           ADD 1 TO WS-REJECT-COUNT
           MOVE EN-TRACE-NO     TO RPT-RJ-TRACE
           MOVE EN-ACCOUNT      TO RPT-RJ-ACCOUNT
           MOVE RJ-REASON-CODE  TO RPT-RJ-CODE
           MOVE RJ-REASON-TEXT  TO RPT-RJ-TEXT
           WRITE REPORT-REC FROM RPT-REJECT-LINE.

      *----------------------------------------------------------------
       2400-ADDENDA.
           IF NOT ST-AFTER-ENTRY OR ADDENDA-NOT-EXPECTED
               MOVE 'UNEXPECTED ADDENDA RECORD' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               SET ADDENDA-NOT-EXPECTED TO TRUE
           END-IF
      *    ADDENDA RECORDS COUNT IN THE ENTRY/ADDENDA COUNT
           ADD 1 TO WB-ENTRY-COUNT.

      *----------------------------------------------------------------
      * COMPARE THE ORIGINATOR'S BATCH CONTROL TOTALS WITH OUR OWN.
      * NUMERIC TESTS ARE NESTED (NOT COMBINED WITH OR) SO A BAD
      * FIELD IS NEVER USED IN A NUMERIC COMPARE.
      *----------------------------------------------------------------
       2500-BATCH-CONTROL.
           IF NOT (ST-IN-BATCH OR ST-AFTER-ENTRY)
               MOVE 'BATCH CONTROL OUT OF SEQUENCE' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF ADDENDA-EXPECTED
               MOVE 'ADDENDA RECORD MISSING' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
               SET ADDENDA-NOT-EXPECTED TO TRUE
           END-IF
           IF BC-SVC-CLASS NOT = WB-SVC-CLASS
               MOVE 'BATCH CONTROL SERVICE CLASS MISMATCH'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF BC-BATCH-NO NOT = WB-BATCH-NO
               MOVE 'BATCH CONTROL BATCH NUMBER MISMATCH'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           END-IF
           IF BC-ENTRY-COUNT IS NOT NUMERIC
               MOVE 'BATCH CONTROL ENTRY COUNT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF BC-ENTRY-COUNT NOT = WB-ENTRY-COUNT
                   MOVE 'BATCH ENTRY/ADDENDA COUNT MISMATCH'
                     TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           COMPUTE WS-HASH-10 =
               FUNCTION MOD (WB-ENTRY-HASH, 10000000000)
           IF BC-ENTRY-HASH IS NOT NUMERIC
               MOVE 'BATCH CONTROL ENTRY HASH NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF BC-ENTRY-HASH NOT = WS-HASH-10
                   MOVE 'BATCH ENTRY HASH MISMATCH' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           IF BC-TOTAL-DEBIT IS NOT NUMERIC
               MOVE 'BATCH CONTROL TOTAL DEBIT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF BC-TOTAL-DEBIT NOT = WB-DEBIT
                   MOVE 'BATCH TOTAL DEBIT MISMATCH' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           IF BC-TOTAL-CREDIT IS NOT NUMERIC
               MOVE 'BATCH CONTROL TOTAL CREDIT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF BC-TOTAL-CREDIT NOT = WB-CREDIT
                   MOVE 'BATCH TOTAL CREDIT MISMATCH' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           PERFORM 2510-WRITE-BATCH-LINE
           ADD WB-ENTRY-COUNT   TO WF-ENTRY-COUNT
           ADD WB-ENTRY-HASH    TO WF-ENTRY-HASH
           ADD WB-DEBIT         TO WF-DEBIT
           ADD WB-CREDIT        TO WF-CREDIT
           SET ST-AFTER-BCTL TO TRUE.

       2510-WRITE-BATCH-LINE.
           MOVE WB-BATCH-NO     TO RPT-BT-BATCH-NO
           MOVE WB-CO-NAME      TO RPT-BT-CO-NAME
           MOVE WB-SVC-CLASS    TO RPT-BT-SVC
           MOVE WB-ENTRY-COUNT  TO RPT-BT-COUNT
           MOVE WB-DEBIT        TO RPT-BT-DEBIT
           MOVE WB-CREDIT       TO RPT-BT-CREDIT
           IF WS-FILE-ERR-COUNT = WS-ERRS-AT-BATCH-START
               MOVE 'CONTROLS OK'     TO RPT-BT-STATUS
           ELSE
               MOVE 'CONTROL ERRORS'  TO RPT-BT-STATUS
           END-IF
           WRITE REPORT-REC FROM RPT-BATCH-LINE
           MOVE SPACES TO RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TEXT-LINE.

      *----------------------------------------------------------------
      * A TYPE-9 RECORD IS THE FILE CONTROL IF IT FOLLOWS A BATCH
      * CONTROL; AFTER THE FILE CONTROL, TYPE-9 RECORDS MUST BE
      * ALL-9 BLOCK FILLER.
      *----------------------------------------------------------------
       2600-FILE-CONTROL-OR-FILLER.
           EVALUATE TRUE
               WHEN ST-AFTER-BCTL
                   PERFORM 2610-FILE-CONTROL
               WHEN ST-AFTER-FCTL
                   IF ACH-RAW NOT = ALL '9'
                       MOVE 'INVALID FILLER RECORD AFTER FILE CONTROL'
                         TO WS-ERR-MSG
                       PERFORM 7000-LOG-FILE-ERROR
                   END-IF
               WHEN OTHER
                   MOVE 'FILE CONTROL OUT OF SEQUENCE' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
           END-EVALUATE.

       2610-FILE-CONTROL.
           IF FC-BATCH-COUNT IS NOT NUMERIC
               MOVE 'FILE CONTROL BATCH COUNT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF FC-BATCH-COUNT NOT = WS-BATCH-COUNT
                   MOVE 'FILE CONTROL BATCH COUNT MISMATCH'
                     TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           IF FC-ENTRY-COUNT IS NOT NUMERIC
               MOVE 'FILE CONTROL ENTRY COUNT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF FC-ENTRY-COUNT NOT = WF-ENTRY-COUNT
                   MOVE 'FILE ENTRY/ADDENDA COUNT MISMATCH'
                     TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           COMPUTE WS-HASH-10 =
               FUNCTION MOD (WF-ENTRY-HASH, 10000000000)
           IF FC-ENTRY-HASH IS NOT NUMERIC
               MOVE 'FILE CONTROL ENTRY HASH NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF FC-ENTRY-HASH NOT = WS-HASH-10
                   MOVE 'FILE ENTRY HASH MISMATCH' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           IF FC-TOTAL-DEBIT IS NOT NUMERIC
               MOVE 'FILE CONTROL TOTAL DEBIT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF FC-TOTAL-DEBIT NOT = WF-DEBIT
                   MOVE 'FILE TOTAL DEBIT MISMATCH' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           IF FC-TOTAL-CREDIT IS NOT NUMERIC
               MOVE 'FILE CONTROL TOTAL CREDIT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF FC-TOTAL-CREDIT NOT = WF-CREDIT
                   MOVE 'FILE TOTAL CREDIT MISMATCH' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               END-IF
           END-IF
           IF FC-BLOCK-COUNT IS NOT NUMERIC
               MOVE 'FILE CONTROL BLOCK COUNT NOT NUMERIC'
                 TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               MOVE FC-BLOCK-COUNT TO WF-BLOCK-COUNT-CTL
           END-IF
           SET ST-AFTER-FCTL TO TRUE.

      *================================================================
      * CHECKS THAT CAN ONLY BE MADE AFTER THE LAST RECORD IS READ
      *================================================================
       8000-END-OF-FILE-CHECKS.
           IF WS-REC-COUNT = ZERO
               MOVE 'INPUT FILE IS EMPTY' TO WS-ERR-MSG
               PERFORM 7000-LOG-FILE-ERROR
           ELSE
               IF NOT ST-AFTER-FCTL
                   MOVE 'FILE CONTROL RECORD MISSING' TO WS-ERR-MSG
                   PERFORM 7000-LOG-FILE-ERROR
               ELSE
                   DIVIDE WS-REC-COUNT BY 10
                       GIVING WS-EXP-BLOCKS
                       REMAINDER WS-REMAINDER
                   IF WS-REMAINDER NOT = ZERO
                       MOVE 'RECORD COUNT IS NOT A MULTIPLE OF 10'
                         TO WS-ERR-MSG
                       PERFORM 7000-LOG-FILE-ERROR
                       ADD 1 TO WS-EXP-BLOCKS
                   END-IF
                   IF WF-BLOCK-COUNT-CTL NOT = WS-EXP-BLOCKS
                       MOVE 'FILE CONTROL BLOCK COUNT MISMATCH'
                         TO WS-ERR-MSG
                       PERFORM 7000-LOG-FILE-ERROR
                   END-IF
               END-IF
           END-IF.

      *================================================================
       7000-LOG-FILE-ERROR.
           ADD 1 TO WS-FILE-ERR-COUNT
           MOVE WS-REC-COUNT TO RPT-ERR-RECNO
           MOVE WS-ERR-MSG   TO RPT-ERR-MSG
           WRITE REPORT-REC FROM RPT-ERROR-LINE
           MOVE SPACES TO WS-ERR-MSG.

      *================================================================
       9000-FINALIZE.
           MOVE SPACES TO RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TEXT-LINE
           MOVE 'SUMMARY' TO RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TITLE-2
           MOVE 'RECORDS READ'              TO RPT-CT-LABEL
           MOVE WS-REC-COUNT                TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE 'BATCHES'                   TO RPT-CT-LABEL
           MOVE WS-BATCH-COUNT              TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE 'FILE-LEVEL ERRORS'         TO RPT-CT-LABEL
           MOVE WS-FILE-ERR-COUNT           TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE 'ENTRIES ACCEPTED FOR POSTING' TO RPT-CT-LABEL
           MOVE WS-VALID-COUNT              TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE 'ENTRIES REJECTED'          TO RPT-CT-LABEL
           MOVE WS-REJECT-COUNT             TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE 'ACCEPTED CREDIT AMOUNT'    TO RPT-AM-LABEL
           MOVE WS-VALID-CREDIT-AMT         TO RPT-AM-VALUE
           WRITE REPORT-REC FROM RPT-AMOUNT-LINE
           MOVE 'ACCEPTED DEBIT AMOUNT'     TO RPT-AM-LABEL
           MOVE WS-VALID-DEBIT-AMT          TO RPT-AM-VALUE
           WRITE REPORT-REC FROM RPT-AMOUNT-LINE
           MOVE SPACES TO RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TEXT-LINE
           EVALUATE TRUE
               WHEN WS-FILE-ERR-COUNT > ZERO
                   MOVE 12 TO WS-RC
                   MOVE 'RESULT: FILE REJECTED - DO NOT POST'
                     TO RPT-TEXT-LINE
               WHEN WS-REJECT-COUNT > ZERO
                   MOVE 4 TO WS-RC
                   MOVE 'RESULT: FILE ACCEPTED WITH ENTRY REJECTS'
                     TO RPT-TEXT-LINE
               WHEN OTHER
                   MOVE 0 TO WS-RC
                   MOVE 'RESULT: FILE ACCEPTED' TO RPT-TEXT-LINE
           END-EVALUATE
           WRITE REPORT-REC FROM RPT-TEXT-LINE
           DISPLAY 'ACHVALID: RECORDS=' WS-REC-COUNT
                   ' VALID=' WS-VALID-COUNT
                   ' REJECTED=' WS-REJECT-COUNT
                   ' FILE ERRORS=' WS-FILE-ERR-COUNT
                   ' RC=' WS-RC
           PERFORM 9100-CLOSE-FILES
           MOVE WS-RC TO RETURN-CODE.

       9100-CLOSE-FILES.
           CLOSE ACH-IN-FILE
                 VALID-FILE
                 REJECT-FILE
                 REPORT-FILE.
