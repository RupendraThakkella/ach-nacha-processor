       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTLOAD.
      *================================================================
      * ACCTLOAD - LOAD THE ACCOUNT MASTER VSAM KSDS
      *
      * READS A READABLE TEXT ACCOUNT FILE (ACCTIN), VALIDATES EACH
      * RECORD, CONVERTS THE BALANCE TO PACKED DECIMAL (COMP-3) AND
      * WRITES IT TO A NEWLY DEFINED, EMPTY KSDS (ACCTMAST).
      * INPUT MUST BE IN ASCENDING ACCOUNT NUMBER ORDER.
      *
      * FILES:  ACCTIN   - ACCOUNT LOAD FILE        FB 80
      *         ACCTMAST - ACCOUNT MASTER           VSAM KSDS (OUTPUT)
      *
      * RETURN CODES: 0  ALL RECORDS LOADED
      *               4  SOME RECORDS SKIPPED (SEE SYSOUT)
      *               16 I/O ERROR
      *
      * PORTFOLIO PROJECT - ALL DATA IS SYNTHETIC.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-IN-FILE  ASSIGN TO ACCTIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-IN-STATUS.
           SELECT ACCT-MASTER   ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS AC-ACCOUNT-NO
                  FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-IN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  ACCT-IN-AREA                    PIC X(80).

       FD  ACCT-MASTER
           RECORD CONTAINS 80 CHARACTERS.
           COPY ACCTREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-IN-STATUS                PIC XX.
               88  IN-OK                   VALUE '00'.
           05  WS-ACCT-STATUS              PIC XX.
               88  ACCT-OK                 VALUE '00'.

       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  END-OF-FILE                       VALUE 'Y'.
           05  WS-REC-SW                   PIC X     VALUE 'Y'.
               88  REC-IS-VALID                      VALUE 'Y'.
               88  REC-IS-INVALID                    VALUE 'N'.

           COPY ACCTIN.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(8)  VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(8)  VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(8)  VALUE 0.
           05  WS-RC                       PIC 9(4)  VALUE 0.

       PROCEDURE DIVISION.
      *================================================================
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-LOAD-RECORD
               UNTIL END-OF-FILE
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT  ACCT-IN-FILE
           OPEN OUTPUT ACCT-MASTER
           IF NOT IN-OK OR NOT ACCT-OK
               DISPLAY 'ACCTLOAD: OPEN FAILED, ACCTIN='
                       WS-IN-STATUS ' ACCTMAST=' WS-ACCT-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1100-READ-INPUT.

       1100-READ-INPUT.
           READ ACCT-IN-FILE INTO ACCT-IN-REC
               AT END
                   SET END-OF-FILE TO TRUE
           END-READ
           IF NOT END-OF-FILE AND NOT IN-OK
               DISPLAY 'ACCTLOAD: READ FAILED, STATUS ' WS-IN-STATUS
               MOVE 16 TO RETURN-CODE
               PERFORM 9100-CLOSE-FILES
               STOP RUN
           END-IF.

       2000-LOAD-RECORD.
           ADD 1 TO WS-READ-COUNT
           SET REC-IS-VALID TO TRUE
           IF AI-ACCOUNT-NO = SPACES
               DISPLAY 'ACCTLOAD: SKIPPED RECORD ' WS-READ-COUNT
                       ' - ACCOUNT NUMBER MISSING'
               SET REC-IS-INVALID TO TRUE
           END-IF
           IF REC-IS-VALID AND NOT AI-TYPE-VALID
               DISPLAY 'ACCTLOAD: SKIPPED ' AI-ACCOUNT-NO
                       ' - INVALID ACCOUNT TYPE ' AI-TYPE
               SET REC-IS-INVALID TO TRUE
           END-IF
           IF REC-IS-VALID AND NOT AI-STATUS-VALID
               DISPLAY 'ACCTLOAD: SKIPPED ' AI-ACCOUNT-NO
                       ' - INVALID ACCOUNT STATUS ' AI-STATUS
               SET REC-IS-INVALID TO TRUE
           END-IF
           IF REC-IS-VALID AND AI-BALANCE IS NOT NUMERIC
               DISPLAY 'ACCTLOAD: SKIPPED ' AI-ACCOUNT-NO
                       ' - BALANCE NOT NUMERIC'
               SET REC-IS-INVALID TO TRUE
           END-IF
           IF REC-IS-VALID
               PERFORM 2100-WRITE-MASTER
           ELSE
               ADD 1 TO WS-SKIP-COUNT
           END-IF
           PERFORM 1100-READ-INPUT.

       2100-WRITE-MASTER.
           MOVE SPACES          TO ACCT-REC
           MOVE AI-ACCOUNT-NO   TO AC-ACCOUNT-NO
           MOVE AI-NAME         TO AC-NAME
           MOVE AI-TYPE         TO AC-TYPE
           MOVE AI-STATUS       TO AC-STATUS
           MOVE AI-BALANCE      TO AC-BALANCE
           MOVE SPACES          TO AC-LAST-POST-DATE
           WRITE ACCT-REC
               INVALID KEY
                   DISPLAY 'ACCTLOAD: SKIPPED ' AI-ACCOUNT-NO
                           ' - DUPLICATE OR OUT-OF-SEQUENCE KEY'
                   ADD 1 TO WS-SKIP-COUNT
               NOT INVALID KEY
                   ADD 1 TO WS-LOAD-COUNT
           END-WRITE
           IF NOT ACCT-OK
              AND WS-ACCT-STATUS NOT = '21'
              AND WS-ACCT-STATUS NOT = '22'
               DISPLAY 'ACCTLOAD: WRITE FAILED, STATUS '
                       WS-ACCT-STATUS
               MOVE 16 TO RETURN-CODE
               PERFORM 9100-CLOSE-FILES
               STOP RUN
           END-IF.

       9000-FINALIZE.
           IF WS-SKIP-COUNT > ZERO
               MOVE 4 TO WS-RC
           END-IF
           DISPLAY 'ACCTLOAD: READ=' WS-READ-COUNT
                   ' LOADED=' WS-LOAD-COUNT
                   ' SKIPPED=' WS-SKIP-COUNT
                   ' RC=' WS-RC
           PERFORM 9100-CLOSE-FILES
           MOVE WS-RC TO RETURN-CODE.

       9100-CLOSE-FILES.
           CLOSE ACCT-IN-FILE
                 ACCT-MASTER.
