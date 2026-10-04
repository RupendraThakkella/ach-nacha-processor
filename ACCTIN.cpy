      *================================================================
      * ACCTIN - ACCOUNT LOAD RECORD (READABLE TEXT)  LRECL 80
      *   BALANCE: SIGN + 13 DIGITS, 2 IMPLIED DECIMALS
      *   EXAMPLE: +0000000250000 = 2,500.00
      *================================================================
       01  ACCT-IN-REC.
           05  AI-ACCOUNT-NO               PIC X(17).
           05  AI-NAME                     PIC X(30).
           05  AI-TYPE                     PIC X.
               88  AI-TYPE-VALID           VALUE 'C' 'S'.
           05  AI-STATUS                   PIC X.
               88  AI-STATUS-VALID         VALUE 'A' 'C' 'F'.
           05  AI-BALANCE                  PIC S9(11)V99
                                           SIGN IS LEADING SEPARATE.
           05  FILLER                      PIC X(17).
