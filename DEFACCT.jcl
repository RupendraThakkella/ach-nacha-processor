//ACHDEFAC JOB 1,NOTIFY=&SYSUID
//*================================================================
//* ACHDEFAC - (RE)DEFINE AND LOAD THE ACCOUNT MASTER VSAM KSDS
//*
//*  >>> BEFORE SUBMITTING: REPLACE EVERY  ZXXXXX  IN THE IDCAMS
//*  >>> STATEMENTS BELOW WITH YOUR OWN USER ID (IDCAMS CONTROL
//*  >>> STATEMENTS DO NOT RESOLVE &SYSUID).
//*
//*  RERUN THIS JOB ANY TIME TO RESET BALANCES TO THE STARTING DATA.
//*================================================================
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE ZXXXXX.ACH.ACCTMAST CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER (NAME(ZXXXXX.ACH.ACCTMAST)          -
                  INDEXED                             -
                  KEYS(17 0)                          -
                  RECORDSIZE(80 80)                   -
                  TRACKS(5 5)                         -
                  FREESPACE(10 10)                    -
                  SHAREOPTIONS(2 3))                  -
         DATA    (NAME(ZXXXXX.ACH.ACCTMAST.DATA))     -
         INDEX   (NAME(ZXXXXX.ACH.ACCTMAST.INDEX))
/*
//*--- LOAD THE KSDS FROM THE TEXT ACCOUNT FILE -------------------
//LOAD     EXEC PGM=ACCTLOAD,COND=(0,NE,DEFINE)
//STEPLIB  DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTIN   DD DSN=&SYSUID..ACH.ACCTIN,DISP=SHR
//ACCTMAST DD DSN=&SYSUID..ACH.ACCTMAST,DISP=OLD
//SYSOUT   DD SYSOUT=*
//*--- PRINT THE KSDS IN HEX - SEE THE PACKED (COMP-3) BALANCES ---
//PRINT    EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//ACCT     DD DSN=&SYSUID..ACH.ACCTMAST,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(ACCT) DUMP
/*
