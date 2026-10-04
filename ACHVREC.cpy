      *================================================================
      * ACHVREC - VALIDATED ACH ENTRY (OUTPUT OF ACHVALID, INPUT TO
      *           ACHPOST).  FIXED LENGTH 100 BYTES.
      *   POS  1-17  ACCOUNT NUMBER (DFSORT KEY 1)
      *   POS 39-53  TRACE NUMBER   (DFSORT KEY 2)
      *================================================================
       01  VALID-ENTRY-REC.
           05  VE-ACCOUNT                  PIC X(17).
           05  VE-TXN-CODE                 PIC X(2).
               88  VE-CREDIT               VALUE '22' '32'.
               88  VE-DEBIT                VALUE '27' '37'.
               88  VE-CHECKING-TXN         VALUE '22' '27'.
               88  VE-SAVINGS-TXN          VALUE '32' '37'.
           05  VE-AMOUNT                   PIC 9(8)V99.
           05  VE-RDFI-ROUTING.
               10  VE-RDFI-ID              PIC X(8).
               10  VE-RDFI-CHECK           PIC X.
           05  VE-TRACE-NO                 PIC X(15).
           05  VE-INDIV-NAME               PIC X(22).
           05  VE-BATCH-NO                 PIC X(7).
           05  VE-EFF-DATE                 PIC X(6).
           05  FILLER                      PIC X(12).
