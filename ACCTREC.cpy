      *================================================================
      * ACCTREC - ACCOUNT MASTER (VSAM KSDS)  LRECL 80, KEY 1-17
      *   BALANCE IS PACKED DECIMAL (COMP-3), AS BANKS STORE MONEY.
      *================================================================
       01  ACCT-REC.
           05  AC-ACCOUNT-NO               PIC X(17).
           05  AC-NAME                     PIC X(30).
           05  AC-TYPE                     PIC X.
               88  AC-CHECKING             VALUE 'C'.
               88  AC-SAVINGS              VALUE 'S'.
           05  AC-STATUS                   PIC X.
               88  AC-ACTIVE               VALUE 'A'.
               88  AC-CLOSED               VALUE 'C'.
               88  AC-FROZEN               VALUE 'F'.
           05  AC-BALANCE                  PIC S9(11)V99 COMP-3.
           05  AC-LAST-POST-DATE           PIC X(8).
           05  FILLER                      PIC X(16).
