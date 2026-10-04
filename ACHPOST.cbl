       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACHPOST.
      *================================================================
      * ACHPOST - ACH ENTRY POSTING TO THE ACCOUNT MASTER
      *
      * READS VALIDATED ACH ENTRIES (SORTED BY ACCOUNT, THEN TRACE
      * NUMBER) AND POSTS EACH ONE TO THE VSAM KSDS ACCOUNT MASTER
      * USING RANDOM ACCESS BY ACCOUNT NUMBER.
      *
      * AN ENTRY THAT CANNOT BE POSTED IS RETURNED WITH A NACHA-STYLE
      * RETURN REASON CODE:
      *   R03  NO ACCOUNT / UNABLE TO LOCATE ACCOUNT
      *   R02  ACCOUNT CLOSED
      *   R16  ACCOUNT FROZEN
      *   X01  TXN CODE DOES NOT MATCH ACCOUNT TYPE  (INTERNAL CODE)
      *   X02  INVALID ACCOUNT STATUS                (INTERNAL CODE)
      *   R01  INSUFFICIENT FUNDS (DEBITS ONLY)
      *
      * FILES:  VALIDIN  - SORTED VALID ENTRIES     FB 100
      *         ACCTMAST - ACCOUNT MASTER           VSAM KSDS (I-O)
      *         RETURNS  - RETURNED ENTRIES         FB 100
      *         POSTRPT  - POSTING REPORT           FB 133
      *
      * RETURN CODES: 0  ALL ENTRIES POSTED
      *               4  ONE OR MORE ENTRIES RETURNED
      *               16 I/O ERROR
      *
      * PORTFOLIO PROJECT - ALL DATA IS SYNTHETIC.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VALID-IN-FILE ASSIGN TO VALIDIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-VALIN-STATUS.
           SELECT ACCT-MASTER   ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS AC-ACCOUNT-NO
                  FILE STATUS IS WS-ACCT-STATUS.
           SELECT RETURN-FILE   ASSIGN TO RETURNS
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RETURN-STATUS.
           SELECT REPORT-FILE   ASSIGN TO POSTRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  VALID-IN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  VALID-IN-REC                    PIC X(100).

       FD  ACCT-MASTER
           RECORD CONTAINS 80 CHARACTERS.
           COPY ACCTREC.

       FD  RETURN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  RETURN-OUT-REC                  PIC X(100).

       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REPORT-REC                      PIC X(133).

       WORKING-STORAGE SECTION.
      *--- FILE STATUS CODES -------------------------------------------
       01  WS-FILE-STATUSES.
           05  WS-VALIN-STATUS             PIC XX.
               88  VALIN-OK                VALUE '00'.
           05  WS-ACCT-STATUS              PIC XX.
               88  ACCT-OK                 VALUE '00'.
               88  ACCT-NOT-FOUND          VALUE '23'.
               88  ACCT-OPEN-OK            VALUE '00' '97'.
           05  WS-RETURN-STATUS            PIC XX.
               88  RETURN-OK               VALUE '00'.
           05  WS-REPORT-STATUS            PIC XX.
               88  REPORT-OK               VALUE '00'.

       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  END-OF-FILE                       VALUE 'Y'.

      *--- RECORD LAYOUTS ----------------------------------------------
           COPY ACHVREC.
           COPY RETREC.

      *--- WORK FIELDS -------------------------------------------------
       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE-DATA        PIC X(21).
           05  WS-RUN-DATE                 PIC X(8).
           05  WS-RESULT-CODE              PIC X(3).
               88  RESULT-NONE                       VALUE SPACES.
               88  RESULT-POSTED                     VALUE 'OK '.
           05  WS-RESULT-TEXT              PIC X(30).
           05  WS-RC                       PIC 9(4)       VALUE 0.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(8)       VALUE 0.
           05  WS-CREDIT-COUNT             PIC 9(8)       VALUE 0.
           05  WS-CREDIT-AMT               PIC 9(10)V99   VALUE 0.
           05  WS-DEBIT-COUNT              PIC 9(8)       VALUE 0.
           05  WS-DEBIT-AMT                PIC 9(10)V99   VALUE 0.
           05  WS-RETURN-COUNT             PIC 9(8)       VALUE 0.
           05  WS-RETURN-AMT               PIC 9(10)V99   VALUE 0.
           05  WS-R01-COUNT                PIC 9(8)       VALUE 0.
           05  WS-R02-COUNT                PIC 9(8)       VALUE 0.
           05  WS-R03-COUNT                PIC 9(8)       VALUE 0.
           05  WS-R16-COUNT                PIC 9(8)       VALUE 0.
           05  WS-X01-COUNT                PIC 9(8)       VALUE 0.
           05  WS-X02-COUNT                PIC 9(8)       VALUE 0.

      *--- REPORT LINES ------------------------------------------------
       01  RPT-TITLE-1.
           05  FILLER                      PIC X(40)      VALUE
               'ACHPOST - ACH POSTING REPORT   RUN DATE '.
           05  RPT-RUN-DATE                PIC X(8).
       01  RPT-RULE.
           05  FILLER                      PIC X(100)     VALUE ALL '='.
       01  RPT-COL-HEAD.
           05  FILLER                      PIC X(18)      VALUE
               'ACCOUNT'.
           05  FILLER                      PIC X(5)       VALUE
               'TXN'.
           05  FILLER                      PIC X(17)      VALUE
               '        AMOUNT'.
           05  FILLER                      PIC X(4)       VALUE
               'RES'.
           05  FILLER                      PIC X(31)      VALUE
               'DESCRIPTION'.
           05  FILLER                      PIC X(20)      VALUE
               '         NEW BALANCE'.
       01  RPT-DETAIL.
           05  RPT-DT-ACCOUNT              PIC X(17).
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-DT-TXN                  PIC X(2).
           05  FILLER                      PIC X(3)       VALUE SPACES.
           05  RPT-DT-AMOUNT               PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(4)       VALUE SPACES.
           05  RPT-DT-RESULT               PIC X(3).
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-DT-TEXT                 PIC X(30).
           05  FILLER                      PIC X          VALUE SPACE.
           05  RPT-DT-BALANCE              PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  RPT-DT-BALANCE-X REDEFINES RPT-DT-BALANCE
                                           PIC X(18).
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
           PERFORM 2000-PROCESS-ENTRY
               UNTIL END-OF-FILE
           PERFORM 9000-FINALIZE
           GOBACK.

      *================================================================
       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           MOVE WS-CURRENT-DATE-DATA (1:8) TO WS-RUN-DATE
           OPEN INPUT  VALID-IN-FILE
           OPEN I-O    ACCT-MASTER
           OPEN OUTPUT RETURN-FILE
                       REPORT-FILE
           IF NOT VALIN-OK OR NOT ACCT-OPEN-OK
              OR NOT RETURN-OK OR NOT REPORT-OK
               DISPLAY 'ACHPOST: OPEN FAILED, STATUS VALIDIN='
                       WS-VALIN-STATUS ' ACCTMAST=' WS-ACCT-STATUS
                       ' RETURNS=' WS-RETURN-STATUS
                       ' POSTRPT=' WS-REPORT-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           MOVE WS-RUN-DATE TO RPT-RUN-DATE
           WRITE REPORT-REC FROM RPT-TITLE-1
           WRITE REPORT-REC FROM RPT-RULE
           WRITE REPORT-REC FROM RPT-COL-HEAD
           WRITE REPORT-REC FROM RPT-RULE
           PERFORM 1100-READ-VALID.

       1100-READ-VALID.
           READ VALID-IN-FILE INTO VALID-ENTRY-REC
               AT END
                   SET END-OF-FILE TO TRUE
           END-READ
           IF NOT END-OF-FILE AND NOT VALIN-OK
               DISPLAY 'ACHPOST: READ VALIDIN FAILED, STATUS '
                       WS-VALIN-STATUS
               PERFORM 9900-FATAL-ERROR
           END-IF.

      *================================================================
      * LOOK UP THE ACCOUNT, APPLY THE POSTING RULES, THEN EITHER
      * POST THE ENTRY OR WRITE IT TO THE RETURNS FILE.
      *================================================================
       2000-PROCESS-ENTRY.
           ADD 1 TO WS-READ-COUNT
           MOVE SPACES     TO WS-RESULT-CODE
                              WS-RESULT-TEXT
           MOVE VE-ACCOUNT TO AC-ACCOUNT-NO
           READ ACCT-MASTER
               INVALID KEY
                   MOVE 'R03' TO WS-RESULT-CODE
                   MOVE 'NO ACCOUNT / UNABLE TO LOCATE'
                     TO WS-RESULT-TEXT
               NOT INVALID KEY
                   PERFORM 2100-APPLY-RULES
           END-READ
           IF NOT ACCT-OK AND NOT ACCT-NOT-FOUND
               DISPLAY 'ACHPOST: READ ACCTMAST FAILED, STATUS '
                       WS-ACCT-STATUS ' KEY ' VE-ACCOUNT
               PERFORM 9900-FATAL-ERROR
           END-IF
           IF RESULT-NONE
               PERFORM 2200-POST-ENTRY
           ELSE
               PERFORM 2300-WRITE-RETURN
           END-IF
           PERFORM 2400-WRITE-DETAIL
           PERFORM 1100-READ-VALID.

      *----------------------------------------------------------------
      * POSTING RULES, CHECKED IN ORDER.  NO RESULT CODE = POSTABLE.
      *----------------------------------------------------------------
       2100-APPLY-RULES.
           EVALUATE TRUE
               WHEN AC-CLOSED
                   MOVE 'R02' TO WS-RESULT-CODE
                   MOVE 'ACCOUNT CLOSED' TO WS-RESULT-TEXT
               WHEN AC-FROZEN
                   MOVE 'R16' TO WS-RESULT-CODE
                   MOVE 'ACCOUNT FROZEN' TO WS-RESULT-TEXT
               WHEN NOT AC-ACTIVE
                   MOVE 'X02' TO WS-RESULT-CODE
                   MOVE 'INVALID ACCOUNT STATUS' TO WS-RESULT-TEXT
               WHEN VE-CHECKING-TXN AND NOT AC-CHECKING
                   MOVE 'X01' TO WS-RESULT-CODE
                   MOVE 'TXN CODE / ACCOUNT TYPE MISMATCH'
                     TO WS-RESULT-TEXT
               WHEN VE-SAVINGS-TXN AND NOT AC-SAVINGS
                   MOVE 'X01' TO WS-RESULT-CODE
                   MOVE 'TXN CODE / ACCOUNT TYPE MISMATCH'
                     TO WS-RESULT-TEXT
               WHEN VE-DEBIT AND AC-BALANCE < VE-AMOUNT
                   MOVE 'R01' TO WS-RESULT-CODE
                   MOVE 'INSUFFICIENT FUNDS' TO WS-RESULT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------
       2200-POST-ENTRY.
           IF VE-CREDIT
               ADD VE-AMOUNT      TO AC-BALANCE
               ADD 1              TO WS-CREDIT-COUNT
               ADD VE-AMOUNT      TO WS-CREDIT-AMT
           ELSE
               SUBTRACT VE-AMOUNT FROM AC-BALANCE
               ADD 1              TO WS-DEBIT-COUNT
               ADD VE-AMOUNT      TO WS-DEBIT-AMT
           END-IF
           MOVE WS-RUN-DATE TO AC-LAST-POST-DATE
           REWRITE ACCT-REC
               INVALID KEY
                   DISPLAY 'ACHPOST: REWRITE INVALID KEY '
                           AC-ACCOUNT-NO
                   PERFORM 9900-FATAL-ERROR
           END-REWRITE
           IF NOT ACCT-OK
               DISPLAY 'ACHPOST: REWRITE FAILED, STATUS '
                       WS-ACCT-STATUS
               PERFORM 9900-FATAL-ERROR
           END-IF
           MOVE 'OK '    TO WS-RESULT-CODE
           MOVE 'POSTED' TO WS-RESULT-TEXT.

      *----------------------------------------------------------------
       2300-WRITE-RETURN.
           MOVE SPACES          TO RETURN-REC
           MOVE VE-TRACE-NO     TO RT-TRACE-NO
           MOVE VE-ACCOUNT      TO RT-ACCOUNT
           MOVE VE-TXN-CODE     TO RT-TXN-CODE
           MOVE VE-AMOUNT       TO RT-AMOUNT
           MOVE WS-RESULT-CODE  TO RT-RETURN-CODE
           MOVE WS-RESULT-TEXT  TO RT-REASON
           MOVE VE-INDIV-NAME   TO RT-INDIV-NAME
           WRITE RETURN-OUT-REC FROM RETURN-REC
           IF NOT RETURN-OK
               DISPLAY 'ACHPOST: WRITE RETURNS FAILED, STATUS '
                       WS-RETURN-STATUS
               PERFORM 9900-FATAL-ERROR
           END-IF
           ADD 1         TO WS-RETURN-COUNT
           ADD VE-AMOUNT TO WS-RETURN-AMT
           EVALUATE WS-RESULT-CODE
               WHEN 'R01'  ADD 1 TO WS-R01-COUNT
               WHEN 'R02'  ADD 1 TO WS-R02-COUNT
               WHEN 'R03'  ADD 1 TO WS-R03-COUNT
               WHEN 'R16'  ADD 1 TO WS-R16-COUNT
               WHEN 'X01'  ADD 1 TO WS-X01-COUNT
               WHEN 'X02'  ADD 1 TO WS-X02-COUNT
           END-EVALUATE.

      *----------------------------------------------------------------
       2400-WRITE-DETAIL.
           MOVE VE-ACCOUNT      TO RPT-DT-ACCOUNT
           MOVE VE-TXN-CODE     TO RPT-DT-TXN
           MOVE VE-AMOUNT       TO RPT-DT-AMOUNT
           MOVE WS-RESULT-CODE  TO RPT-DT-RESULT
           MOVE WS-RESULT-TEXT  TO RPT-DT-TEXT
           IF RESULT-POSTED
               MOVE AC-BALANCE  TO RPT-DT-BALANCE
           ELSE
               MOVE SPACES      TO RPT-DT-BALANCE-X
           END-IF
           WRITE REPORT-REC FROM RPT-DETAIL.

      *================================================================
       9000-FINALIZE.
           WRITE REPORT-REC FROM RPT-RULE
           MOVE 'SETTLEMENT SUMMARY' TO RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-TEXT-LINE
           WRITE REPORT-REC FROM RPT-RULE
           MOVE 'ENTRIES READ'               TO RPT-CT-LABEL
           MOVE WS-READ-COUNT                TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE 'CREDITS POSTED'             TO RPT-CT-LABEL
           MOVE WS-CREDIT-COUNT              TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  CREDIT AMOUNT POSTED'     TO RPT-AM-LABEL
           MOVE WS-CREDIT-AMT                TO RPT-AM-VALUE
           WRITE REPORT-REC FROM RPT-AMOUNT-LINE
           MOVE 'DEBITS POSTED'              TO RPT-CT-LABEL
           MOVE WS-DEBIT-COUNT               TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  DEBIT AMOUNT POSTED'      TO RPT-AM-LABEL
           MOVE WS-DEBIT-AMT                 TO RPT-AM-VALUE
           WRITE REPORT-REC FROM RPT-AMOUNT-LINE
           MOVE 'ENTRIES RETURNED'           TO RPT-CT-LABEL
           MOVE WS-RETURN-COUNT              TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  RETURNED AMOUNT'          TO RPT-AM-LABEL
           MOVE WS-RETURN-AMT                TO RPT-AM-VALUE
           WRITE REPORT-REC FROM RPT-AMOUNT-LINE
           MOVE '  R01 INSUFFICIENT FUNDS'   TO RPT-CT-LABEL
           MOVE WS-R01-COUNT                 TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  R02 ACCOUNT CLOSED'       TO RPT-CT-LABEL
           MOVE WS-R02-COUNT                 TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  R03 NO ACCOUNT'           TO RPT-CT-LABEL
           MOVE WS-R03-COUNT                 TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  R16 ACCOUNT FROZEN'       TO RPT-CT-LABEL
           MOVE WS-R16-COUNT                 TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  X01 TYPE MISMATCH'        TO RPT-CT-LABEL
           MOVE WS-X01-COUNT                 TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           MOVE '  X02 INVALID STATUS'       TO RPT-CT-LABEL
           MOVE WS-X02-COUNT                 TO RPT-CT-VALUE
           WRITE REPORT-REC FROM RPT-COUNT-LINE
           IF WS-RETURN-COUNT > ZERO
               MOVE 4 TO WS-RC
           ELSE
               MOVE 0 TO WS-RC
           END-IF
           DISPLAY 'ACHPOST: READ=' WS-READ-COUNT
                   ' CREDITS=' WS-CREDIT-COUNT
                   ' DEBITS=' WS-DEBIT-COUNT
                   ' RETURNS=' WS-RETURN-COUNT
                   ' RC=' WS-RC
           PERFORM 9100-CLOSE-FILES
           MOVE WS-RC TO RETURN-CODE.

       9100-CLOSE-FILES.
           CLOSE VALID-IN-FILE
                 ACCT-MASTER
                 RETURN-FILE
                 REPORT-FILE.

       9900-FATAL-ERROR.
           MOVE 16 TO RETURN-CODE
           PERFORM 9100-CLOSE-FILES
           STOP RUN.
