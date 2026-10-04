      *================================================================
      * NACHAREC - NACHA ACH RECORD LAYOUTS (94-BYTE FIXED RECORDS)
      *   ONE 94-BYTE AREA REDEFINED FOR EACH NACHA RECORD TYPE:
      *   1 = FILE HEADER      5 = BATCH HEADER     6 = ENTRY DETAIL
      *   7 = ADDENDA          8 = BATCH CONTROL    9 = FILE CONTROL
      *================================================================
       01  WS-ACH-RECORD.
           05  ACH-RAW                     PIC X(94).
      *--- COMMON VIEW: RECORD TYPE CODE ONLY --------------------------
           05  ACH-COMMON REDEFINES ACH-RAW.
               10  ACH-REC-TYPE            PIC X.
               10  FILLER                  PIC X(93).
      *--- TYPE 1: FILE HEADER -----------------------------------------
           05  ACH-FILE-HEADER REDEFINES ACH-RAW.
               10  FH-REC-TYPE             PIC X.
               10  FH-PRIORITY             PIC X(2).
               10  FH-IMM-DEST             PIC X(10).
               10  FH-IMM-ORIGIN           PIC X(10).
               10  FH-CREATE-DATE          PIC X(6).
               10  FH-CREATE-TIME          PIC X(4).
               10  FH-ID-MODIFIER          PIC X.
               10  FH-REC-SIZE             PIC X(3).
               10  FH-BLOCK-FACTOR         PIC X(2).
               10  FH-FORMAT-CODE          PIC X.
               10  FH-DEST-NAME            PIC X(23).
               10  FH-ORIGIN-NAME          PIC X(23).
               10  FH-REF-CODE             PIC X(8).
      *--- TYPE 5: BATCH HEADER ----------------------------------------
           05  ACH-BATCH-HEADER REDEFINES ACH-RAW.
               10  BH-REC-TYPE             PIC X.
               10  BH-SVC-CLASS            PIC X(3).
                   88  BH-SVC-VALID        VALUE '200' '220' '225'.
               10  BH-CO-NAME              PIC X(16).
               10  BH-CO-DISC-DATA         PIC X(20).
               10  BH-CO-ID                PIC X(10).
               10  BH-SEC-CODE             PIC X(3).
                   88  BH-SEC-SUPPORTED    VALUE 'PPD' 'CCD'.
               10  BH-ENTRY-DESC           PIC X(10).
               10  BH-DESC-DATE            PIC X(6).
               10  BH-EFF-DATE             PIC X(6).
               10  BH-SETTLE-DATE          PIC X(3).
               10  BH-ORIG-STATUS          PIC X.
               10  BH-ODFI-ID              PIC X(8).
               10  BH-BATCH-NO             PIC X(7).
      *--- TYPE 6: ENTRY DETAIL ----------------------------------------
           05  ACH-ENTRY-DETAIL REDEFINES ACH-RAW.
               10  EN-REC-TYPE             PIC X.
               10  EN-TXN-CODE             PIC X(2).
                   88  EN-TXN-CREDIT       VALUE '21' '22' '23' '24'
                                                 '31' '32' '33' '34'.
                   88  EN-TXN-DEBIT        VALUE '26' '27' '28' '29'
                                                 '36' '37' '38' '39'.
                   88  EN-TXN-POSTABLE     VALUE '22' '27' '32' '37'.
               10  EN-RDFI-ROUTING         PIC X(8).
               10  EN-RDFI-NUM REDEFINES EN-RDFI-ROUTING
                                           PIC 9(8).
               10  EN-RDFI-DIGITS REDEFINES EN-RDFI-ROUTING.
                   15  EN-RDFI-DIGIT       PIC 9 OCCURS 8 TIMES.
               10  EN-CHECK-DIGIT          PIC 9.
               10  EN-ACCOUNT              PIC X(17).
               10  EN-AMOUNT               PIC X(10).
               10  EN-AMOUNT-N REDEFINES EN-AMOUNT
                                           PIC 9(8)V99.
               10  EN-INDIV-ID             PIC X(15).
               10  EN-INDIV-NAME           PIC X(22).
               10  EN-DISC-DATA            PIC X(2).
               10  EN-ADDENDA-IND          PIC X.
                   88  EN-ADDENDA-VALID    VALUE '0' '1'.
                   88  EN-HAS-ADDENDA      VALUE '1'.
               10  EN-TRACE-NO.
                   15  EN-TRACE-ODFI       PIC X(8).
                   15  EN-TRACE-SEQ        PIC X(7).
      *--- TYPE 7: ADDENDA ---------------------------------------------
           05  ACH-ADDENDA REDEFINES ACH-RAW.
               10  AD-REC-TYPE             PIC X.
               10  AD-TYPE-CODE            PIC X(2).
               10  AD-PAYMENT-INFO         PIC X(80).
               10  AD-SEQ-NO               PIC X(4).
               10  AD-ENTRY-SEQ-NO         PIC X(7).
      *--- TYPE 8: BATCH CONTROL ---------------------------------------
           05  ACH-BATCH-CONTROL REDEFINES ACH-RAW.
               10  BC-REC-TYPE             PIC X.
               10  BC-SVC-CLASS            PIC X(3).
               10  BC-ENTRY-COUNT          PIC 9(6).
               10  BC-ENTRY-HASH           PIC 9(10).
               10  BC-TOTAL-DEBIT          PIC 9(10)V99.
               10  BC-TOTAL-CREDIT         PIC 9(10)V99.
               10  BC-CO-ID                PIC X(10).
               10  BC-MAC                  PIC X(19).
               10  BC-RESERVED             PIC X(6).
               10  BC-ODFI-ID              PIC X(8).
               10  BC-BATCH-NO             PIC X(7).
      *--- TYPE 9: FILE CONTROL ----------------------------------------
           05  ACH-FILE-CONTROL REDEFINES ACH-RAW.
               10  FC-REC-TYPE             PIC X.
               10  FC-BATCH-COUNT          PIC 9(6).
               10  FC-BLOCK-COUNT          PIC 9(6).
               10  FC-ENTRY-COUNT          PIC 9(8).
               10  FC-ENTRY-HASH           PIC 9(10).
               10  FC-TOTAL-DEBIT          PIC 9(10)V99.
               10  FC-TOTAL-CREDIT         PIC 9(10)V99.
               10  FC-RESERVED             PIC X(39).
