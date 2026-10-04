      *================================================================
      * RETREC - ACH RETURN ENTRY (OUTPUT OF ACHPOST)  LRECL 100
      *   R01 INSUFFICIENT FUNDS      R02 ACCOUNT CLOSED
      *   R03 NO ACCOUNT              R16 ACCOUNT FROZEN
      *   X01 TXN CODE / ACCOUNT TYPE MISMATCH (INTERNAL CODE)
      *   X02 INVALID ACCOUNT STATUS            (INTERNAL CODE)
      *================================================================
       01  RETURN-REC.
           05  RT-TRACE-NO                 PIC X(15).
           05  RT-ACCOUNT                  PIC X(17).
           05  RT-TXN-CODE                 PIC X(2).
           05  RT-AMOUNT                   PIC 9(8)V99.
           05  RT-RETURN-CODE              PIC X(3).
           05  RT-REASON                   PIC X(30).
           05  RT-INDIV-NAME               PIC X(22).
           05  FILLER                      PIC X(1).
