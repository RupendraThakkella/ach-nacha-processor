# ACH (NACHA) File Validation and Posting System

A mainframe batch application in **COBOL, JCL, VSAM, and DFSORT** that processes an inbound ACH file the way a receiving bank (RDFI) does. It validates a NACHA-format file, sorts the good entries, posts them to a VSAM account master, and returns entries that can't be posted with NACHA return reason codes.

All data in this project is synthetic. No real names, accounts, or routing numbers are used.

## How it works

```
  ACH.ACHIN (NACHA file, FB 94)
        |
        v
  [ACHVALID]  ---> ACH.REJECTS   (bad entries + reason code)
        |     ---> VALRPT        (validation report)
        |  RC 0/4 continue, RC 12 = whole file rejected, stop
        v
  ACH.VALID (FB 100)
        |
        v
  [DFSORT]  sort by account number, then trace number
        |
        v
  [ACHPOST] <--> ACH.ACCTMAST (VSAM KSDS, random access)
        |   ---> ACH.RETURNS  (R01/R02/R03/R16 returns)
        |   ---> POSTRPT      (posting + settlement summary)
        v
  [IDCAMS PRINT]  updated account master
```

## What each piece does

| Member | Type | Purpose |
|---|---|---|
| `ACHVALID` | COBOL | Validates the NACHA file at file level and entry level |
| `ACHPOST` | COBOL | Posts valid entries to the account master; writes returns |
| `ACCTLOAD` | COBOL | Loads the VSAM KSDS from a text file, converting balances to COMP-3 |
| `NACHAREC` | Copybook | All six NACHA record layouts as REDEFINES of one 94-byte area |
| `ACHVREC` | Copybook | Validated entry passed from ACHVALID to ACHPOST |
| `ACCTREC` | Copybook | Account master record (packed-decimal balance) |
| `ACCTIN` | Copybook | Readable account load record |
| `RETREC` | Copybook | Returned entry record |
| `ALLOC` | JCL | One-time data set allocation |
| `COMPILE` | JCL | Compiles and links all three programs (IGYWCL) |
| `DEFACCT` | JCL | Defines the KSDS with IDCAMS, loads it, prints it |
| `ACHDAILY` | JCL | The daily run: cleanup, validate, sort, post, print |

## Validation rules (ACHVALID)

**File level.** Any failure rejects the whole file with RC 12, and the JCL skips posting.

- Record sequence: 1 → (5 → 6/7 … → 8) … → 9, then optional all-9 block filler
- File header: priority code 01, record size 094, blocking factor 10, format code 1
- Batch header: service class 200/220/225, SEC code PPD or CCD
- Batch control must match computed entry/addenda count, entry hash, total debits, total credits, service class, and batch number
- File control must match batch count, entry/addenda count, entry hash, debits, credits, and block count
- Addenda records must follow an entry whose addenda indicator is 1

**Entry level.** A failed entry is written to REJECTS with RC 4, and processing continues.

| Code | Rule |
|---|---|
| E001 | Transaction code must be 22, 27, 32, or 37 (live checking/savings credits and debits) |
| E002 | RDFI routing number and check digit must be numeric |
| E003 | ABA check digit must be valid (weights 3-7-1-3-7-1-3-7) |
| E004 | Amount must be numeric |
| E005 | Live entry cannot have a zero amount |
| E006 | Account number cannot be blank |
| E007 | Trace number must start with the batch's ODFI |
| E008 | Addenda indicator must be 0 or 1 |

The entry hash is the sum of the 8-digit RDFI routing numbers, keeping the rightmost 10 digits. Control totals include every entry, rejected ones too, because the originator's control records cover the whole batch.

## Posting rules (ACHPOST)

| Code | Meaning |
|---|---|
| R03 | No account / unable to locate account |
| R02 | Account closed |
| R16 | Account frozen |
| X01 | Transaction code doesn't match account type (internal code) |
| X02 | Invalid account status (internal code) |
| R01 | Insufficient funds (debits only) |

Credits add to the balance and debits subtract. Each posted account gets its last-posted date updated.

## Running it on IBM Z Xplore

[IBM Z Xplore](https://www.ibm.com/z/resources/zxplore) gives you free access to a real z/OS system. The steps below assume VS Code with Zowe Explorer, which Z Xplore sets up for you.

1. **Allocate data sets.** Upload `jcl/ALLOC.jcl` and submit it. It creates `ACH.COBOL`, `ACH.COPYLIB`, `ACH.JCL`, and the three input data sets.
2. **Upload source.** Put each file from `cobol/` into `ACH.COBOL` and each file from `copylib/` into `ACH.COPYLIB`, using the file name without the extension as the member name (for example, `ACHVALID`, `NACHAREC`). Put the JCL members into `ACH.JCL`.
3. **Upload data.** Copy the contents of `data/ACHIN.txt` into `ACH.ACHIN`, `data/ACHBAD.txt` into `ACH.ACHBAD`, and `data/ACCTIN.txt` into `ACH.ACCTIN`.
4. **Compile.** Submit `COMPILE`. Every step should end with RC 0 or 4. If not, read the COBOL SYSPRINT output for the message.
5. **Create the account master.** Edit `DEFACCT`, replace every `ZXXXXX` with your user ID, and submit it. The PRINT step dumps the KSDS in hex so you can see the packed balances.
6. **Run the daily job.** Submit `ACHDAILY`. Expect VALIDATE RC 4, SORTACH RC 0, and POST RC 4.
7. **Check the results** against [`docs/EXPECTED_RESULTS.md`](docs/EXPECTED_RESULTS.md).
8. **Test the reject path.** Rerun `DEFACCT` to reset balances, change `ACH.ACHIN` to `ACH.ACHBAD` in `ACHDAILY`, and submit. VALIDATE should end RC 12 with `BATCH TOTAL CREDIT MISMATCH`, and the sort and post steps should be skipped.

`ACHDAILY` updates the account master, so rerun `DEFACCT` before each fresh test.

## Regenerating test data

```
python3 tools/make_test_data.py     # rebuilds data/ and docs/EXPECTED_RESULTS.md
python3 tools/check_cobol.py        # column-72 and record-length checks
```

Edit the `ACCOUNTS` and `BATCHES` lists in `make_test_data.py` to add your own scenarios. The script rebuilds valid control totals and recalculates the expected results.

## Ideas to extend it

- Store each day's returns in a **GDG** so you keep a history of runs
- Report addenda payment information on the posting report
- Generate an outbound **return file** in NACHA format (batch with return addenda type 99)
- Move the account master to **DB2** and rewrite ACHPOST with embedded SQL
- Add a **REXX** exec that reads the job output and summarizes return codes
- Add **CICS** balance-inquiry and transaction-history screens on top of the account master

## Skills demonstrated

COBOL (REDEFINES, OCCURS, 88-levels, COMP-3, file status handling), VSAM KSDS (IDCAMS DEFINE, random I-O, REWRITE), JCL (IF/THEN/ELSE, COND, IEFBR14 cleanup, IGYWCL), DFSORT, NACHA file structure, ABA routing validation, ACH return processing.
