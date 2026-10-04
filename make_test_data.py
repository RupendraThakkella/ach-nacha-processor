#!/usr/bin/env python3
"""
make_test_data.py - builds the synthetic test data for the ACH project
and computes the results the COBOL programs should produce.

Outputs (relative to the project root):
  data/ACCTIN.txt   account master load file   (80-byte records)
  data/ACHIN.txt    good ACH file              (94-byte records)
  data/ACHBAD.txt   ACH file with a bad batch control total
  docs/EXPECTED_RESULTS.md

All names, accounts and routing numbers are made up.
Run:  python3 tools/make_test_data.py
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


# ---------------------------------------------------------------- helpers
def chk(r8: str) -> str:
    """ABA routing check digit for an 8-digit routing prefix."""
    w = [3, 7, 1, 3, 7, 1, 3, 7]
    s = sum(int(d) * w[i] for i, d in enumerate(r8))
    return str((10 - s % 10) % 10)


def fx(value, width, kind="X"):
    """Fixed-width field: X = left/space pad, 9 = right/zero pad."""
    s = str(value)
    if kind == "9":
        assert s.isdigit() and len(s) <= width, (value, width)
        return s.rjust(width, "0")
    assert len(s) <= width, (value, width)
    return s.ljust(width)


def cents(amount: str) -> int:
    d, c = amount.split(".")
    return int(d) * 100 + int(c)


def money(c: int) -> str:
    return f"{c // 100:,}.{c % 100:02d}"


ODFI = "09100001"          # our bank (ODFI) - 8 digits
DEST = "07100000"          # immediate destination (fictional)
CREDIT_CODES = {"21", "22", "23", "24", "31", "32", "33", "34"}
DEBIT_CODES = {"26", "27", "28", "29", "36", "37", "38", "39"}
POSTABLE = {"22", "27", "32", "37"}

# -------------------------------------------------------------- accounts
ACCOUNTS = [
    # account,     name,              type, status, balance
    ("1001234567", "JAMES CARTER",    "C", "A", "2500.00"),
    ("1002345678", "MARIA LOPEZ",     "C", "A", "150.00"),
    ("1003456789", "DAVID KIM",       "S", "A", "10000.00"),
    ("1004567890", "LINDA BROWN",     "C", "C", "0.00"),
    ("1005678901", "ROBERT CHEN",     "S", "F", "3200.00"),
    ("1006789012", "PRIYA NAIR",      "C", "A", "780.25"),
    ("1007890123", "AHMED HASSAN",    "S", "A", "45.10"),
    ("2001112223", "EMILY DAVIS",     "C", "A", "0.00"),
]

# ---------------------------------------------------------------- entries
# Each entry: txn, rdfi8, account, amount, name, plus optional overrides.
R1, R2, R3 = "10100000", "10200001", "10300002"   # fictional RDFI prefixes
BATCHES = [
    {
        "svc": "220", "co_name": "ACME PAYROLL", "co_id": "1234567890",
        "desc": "PAYROLL", "eff": "261005",
        "entries": [
            dict(txn="22", rdfi=R1, acct="1001234567", amt="1250.00",
                 name="JAMES CARTER"),
            dict(txn="22", rdfi=R2, acct="1006789012", amt="980.50",
                 name="PRIYA NAIR"),
            dict(txn="32", rdfi=R3, acct="1003456789", amt="500.00",
                 name="DAVID KIM"),
            dict(txn="22", rdfi=R1, acct="1004567890", amt="300.00",
                 name="LINDA BROWN"),
            dict(txn="22", rdfi=R2, acct="2001112223", amt="410.00",
                 name="EMILY DAVIS", bad_check=True),
            dict(txn="32", rdfi=R3, acct="1009999999", amt="75.00",
                 name="UNKNOWN PAYEE"),
        ],
    },
    {
        "svc": "225", "co_name": "CITY UTILITIES", "co_id": "9876543210",
        "desc": "UTILITY", "eff": "261005",
        "entries": [
            dict(txn="27", rdfi=R1, acct="1002345678", amt="210.75",
                 name="MARIA LOPEZ"),
            dict(txn="27", rdfi=R1, acct="1001234567", amt="89.99",
                 name="JAMES CARTER",
                 addenda="UTILITY BILL ACCT 55512 OCT 2026"),
            dict(txn="37", rdfi=R3, acct="1005678901", amt="120.00",
                 name="ROBERT CHEN"),
            dict(txn="27", rdfi=R3, acct="1003456789", amt="60.00",
                 name="DAVID KIM"),
            dict(txn="37", rdfi=R2, acct="1007890123", amt="0.00",
                 name="AHMED HASSAN"),
            dict(txn="27", rdfi=R2, acct="", amt="15.00",
                 name="NO ACCOUNT GIVEN"),
            dict(txn="27", rdfi=R2, acct="1006789012", amt="25.00",
                 name="PRIYA NAIR", trace_odfi="09100002"),
        ],
    },
]


# ------------------------------------------------------- record builders
def file_header():
    r = ("1" + "01" + " " + DEST + chk(DEST) + " " + ODFI + chk(ODFI)
         + "261004" + "2130" + "A" + "094" + "10" + "1"
         + fx("FICTIONAL FED RESERVE", 23) + fx("SAMPLE COMMUNITY BANK", 23)
         + fx("", 8))
    assert len(r) == 94
    return r


def batch_header(b, no):
    r = ("5" + b["svc"] + fx(b["co_name"], 16) + fx("", 20)
         + fx(b["co_id"], 10) + "PPD" + fx(b["desc"], 10) + fx("", 6)
         + b["eff"] + fx("", 3) + "1" + ODFI + fx(no, 7, "9"))
    assert len(r) == 94
    return r


def entry_detail(e, seq):
    check = chk(e["rdfi"])
    if e.get("bad_check"):
        check = str((int(check) + 1) % 10)
    trace = e.get("trace_odfi", ODFI) + fx(seq, 7, "9")
    r = ("6" + e["txn"] + e["rdfi"] + check + fx(e["acct"], 17)
         + fx(cents(e["amt"]), 10, "9") + fx("", 15) + fx(e["name"], 22)
         + "  " + ("1" if e.get("addenda") else "0") + trace)
    assert len(r) == 94
    return r, trace


def addenda(text, seq):
    r = "7" + "05" + fx(text, 80) + "0001" + fx(seq, 7, "9")
    assert len(r) == 94
    return r


def batch_control(b, no, count, hsh, dr, cr):
    r = ("8" + b["svc"] + fx(count, 6, "9") + fx(hsh % 10**10, 10, "9")
         + fx(dr, 12, "9") + fx(cr, 12, "9") + fx(b["co_id"], 10)
         + fx("", 19) + fx("", 6) + ODFI + fx(no, 7, "9"))
    assert len(r) == 94
    return r


def file_control(batches, blocks, count, hsh, dr, cr):
    r = ("9" + fx(batches, 6, "9") + fx(blocks, 6, "9")
         + fx(count, 8, "9") + fx(hsh % 10**10, 10, "9")
         + fx(dr, 12, "9") + fx(cr, 12, "9") + fx("", 39))
    assert len(r) == 94
    return r


# ------------------------------------------------- build ACH + simulate
def build_ach(credit_tamper_cents=0):
    """Return (records, valid_entries, rejects). credit_tamper_cents is
    added to batch 1's control credit total to create a bad file."""
    recs = [file_header()]
    valid, rejects = [], []
    seq = 0
    f_count = f_hash = f_dr = f_cr = 0
    for bi, b in enumerate(BATCHES, start=1):
        recs.append(batch_header(b, bi))
        count = hsh = dr = cr = 0
        for e in b["entries"]:
            seq += 1
            rec, trace = entry_detail(e, seq)
            recs.append(rec)
            count += 1
            amt = cents(e["amt"])
            hsh += int(e["rdfi"])
            if e["txn"] in CREDIT_CODES:
                cr += amt
            if e["txn"] in DEBIT_CODES:
                dr += amt
            # --- same edit order as ACHVALID 2320-EDIT-ENTRY
            reason = None
            check_ok = rec[11] == chk(e["rdfi"])
            if e["txn"] not in POSTABLE:
                reason = ("E001", "TRANSACTION CODE NOT SUPPORTED")
            elif not check_ok:
                reason = ("E003", "ROUTING CHECK DIGIT INVALID")
            elif amt == 0:
                reason = ("E005", "ZERO AMOUNT ON LIVE ENTRY")
            elif not e["acct"].strip():
                reason = ("E006", "ACCOUNT NUMBER MISSING")
            elif trace[:8] != ODFI:
                reason = ("E007", "TRACE NUMBER ODFI MISMATCH")
            if reason:
                rejects.append((trace, e["acct"], *reason))
            else:
                valid.append(dict(acct=e["acct"], txn=e["txn"], amt=amt,
                                  trace=trace, name=e["name"]))
            if e.get("addenda"):
                recs.append(addenda(e["addenda"], seq))
                count += 1
        ctl_cr = cr + (credit_tamper_cents if bi == 1 else 0)
        recs.append(batch_control(b, bi, count, hsh, dr, ctl_cr))
        f_count += count
        f_hash += hsh
        f_dr += dr
        f_cr += cr
    total = len(recs) + 1                       # + file control
    blocks = -(-total // 10)
    recs.append(file_control(len(BATCHES), blocks, f_count, f_hash,
                             f_dr, f_cr))
    while len(recs) % 10:
        recs.append("9" * 94)
    return recs, valid, rejects


def simulate_post(valid):
    bal = {a[0]: cents(a[4]) for a in ACCOUNTS}
    info = {a[0]: a for a in ACCOUNTS}
    results = []
    for v in sorted(valid, key=lambda x: (x["acct"].ljust(17), x["trace"])):
        a = info.get(v["acct"])
        credit = v["txn"] in ("22", "32")
        if a is None:
            res = ("R03", "NO ACCOUNT / UNABLE TO LOCATE")
        elif a[3] == "C":
            res = ("R02", "ACCOUNT CLOSED")
        elif a[3] == "F":
            res = ("R16", "ACCOUNT FROZEN")
        elif v["txn"] in ("22", "27") and a[2] != "C":
            res = ("X01", "TXN CODE / ACCOUNT TYPE MISMATCH")
        elif v["txn"] in ("32", "37") and a[2] != "S":
            res = ("X01", "TXN CODE / ACCOUNT TYPE MISMATCH")
        elif not credit and bal[v["acct"]] < v["amt"]:
            res = ("R01", "INSUFFICIENT FUNDS")
        else:
            bal[v["acct"]] += v["amt"] if credit else -v["amt"]
            res = ("OK", "POSTED")
        results.append((v, res, bal.get(v["acct"])))
    return results, bal


def acct_record(a):
    acct, name, typ, status, balance = a
    c = cents(balance)
    r = fx(acct, 17) + fx(name, 30) + typ + status + "+" + fx(c, 13, "9")
    r = fx(r, 80)
    assert len(r) == 80
    return r


# --------------------------------------------------------------- main
def main():
    (ROOT / "data").mkdir(exist_ok=True)
    (ROOT / "docs").mkdir(exist_ok=True)

    good, valid, rejects = build_ach()
    bad, _, _ = build_ach(credit_tamper_cents=10000)   # +100.00
    bad_recno = next(i for i, r in enumerate(bad, 1) if r[0] == "8")
    for name, recs in (("ACHIN.txt", good), ("ACHBAD.txt", bad)):
        (ROOT / "data" / name).write_text("\n".join(recs) + "\n")
    (ROOT / "data" / "ACCTIN.txt").write_text(
        "\n".join(acct_record(a) for a in ACCOUNTS) + "\n")

    results, bal = simulate_post(valid)
    posted = [r for r in results if r[1][0] == "OK"]
    returned = [r for r in results if r[1][0] != "OK"]

    md = ["# Expected Results", "",
          "Generated by `tools/make_test_data.py`. Compare your job output "
          "with these values.", "",
          "## ACHVALID with data/ACHIN.txt", "",
          f"- Records read: {len(good)}",
          f"- Batches: {len(BATCHES)}",
          "- File-level errors: 0",
          f"- Entries accepted: {len(valid)}",
          f"- Entries rejected: {len(rejects)}",
          "- Return code: **4** (accepted with entry rejects)", "",
          "| Trace | Account | Code | Reason |", "|---|---|---|---|"]
    for t, a, c, why in rejects:
        md.append(f"| {t} | {a or '(blank)'} | {c} | {why} |")
    md += ["", "## ACHPOST (after DFSORT by account, trace)", "",
           "| Account | Txn | Amount | Result | New balance |",
           "|---|---|---|---|---|"]
    for v, res, b in results:
        nb = money(b) if res[0] == "OK" else ""
        md.append(f"| {v['acct']} | {v['txn']} | {money(v['amt'])} | "
                  f"{res[0]} {res[1]} | {nb} |")
    cr = [r for r in posted if r[0]["txn"] in ("22", "32")]
    dr = [r for r in posted if r[0]["txn"] in ("27", "37")]
    md += ["",
           f"- Credits posted: {len(cr)} totaling "
           f"{money(sum(r[0]['amt'] for r in cr))}",
           f"- Debits posted: {len(dr)} totaling "
           f"{money(sum(r[0]['amt'] for r in dr))}",
           f"- Entries returned: {len(returned)} totaling "
           f"{money(sum(r[0]['amt'] for r in returned))}",
           "- Return code: **4** (one or more returns)", "",
           "## Final account balances", "",
           "| Account | Name | Balance |", "|---|---|---|"]
    for a in ACCOUNTS:
        md.append(f"| {a[0]} | {a[1]} | {money(bal[a[0]])} |")
    md += ["", "## ACHVALID with data/ACHBAD.txt", "",
           "Batch 1's control record overstates the credit total by "
           "100.00, so the file is rejected:", "",
           f"- `*** FILE ERROR  RECORD {bad_recno} : BATCH TOTAL CREDIT "
           "MISMATCH`",
           "- Return code: **12** - the JCL skips the SORT and POST steps "
           "and the account master is not touched.", ""]
    (ROOT / "docs" / "EXPECTED_RESULTS.md").write_text("\n".join(md))
    print("\n".join(md))


if __name__ == "__main__":
    main()
