#!/usr/bin/env python3
"""
check_cobol.py - quick static checks before uploading to the mainframe:
  * every source line fits in columns 1-72 (fixed format), no tabs
  * column 7 is blank, '*' or '-'
  * each copybook redefinition adds up to its record length
This is NOT a compiler; it only catches layout slips early.
Run:  python3 tools/check_cobol.py
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PIC = re.compile(r"PIC\s+(S?)([X9V()0-9]+)", re.I)


def pic_len(sign, pic, usage):
    n = 0
    for m in re.finditer(r"([X9])(?:\((\d+)\))?|V", pic):
        if m.group(0) == "V":
            continue
        n += int(m.group(2) or 1)
    if "COMP-3" in usage:
        return n // 2 + 1
    if "SEPARATE" in usage:
        return n + 1
    return n


def check_columns(path):
    errs = []
    for i, line in enumerate(path.read_text().splitlines(), 1):
        if "\t" in line:
            errs.append(f"{path.name}:{i} tab character")
        if len(line) > 72:
            errs.append(f"{path.name}:{i} beyond column 72 ({len(line)})")
        if len(line) >= 7 and line[6] not in " *-":
            errs.append(f"{path.name}:{i} bad indicator column 7")
        if len(line) > 7 and line[6] == " " and line[:6].strip():
            errs.append(f"{path.name}:{i} text in sequence area")
    return errs


def group_lengths(path):
    """Length of each level-05 item under the 01 (REDEFINES included)."""
    lines = [l[7:72] for l in path.read_text().splitlines()
             if len(l) > 7 and l[6] == " "]
    text = " ".join(lines)
    stmts = [s.strip() for s in text.split(".") if s.strip()]
    out, cur, cur_len = {}, None, 0
    skip_level = None            # inside a nested REDEFINES group
    for s in stmts:
        m = re.match(r"(\d\d)\s+(\S+)", s)
        if not m:
            continue
        lvl, name = int(m.group(1)), m.group(2)
        if lvl == 88:
            continue
        if skip_level is not None:
            if lvl > skip_level:
                continue
            skip_level = None
        if lvl == 5:
            if cur:
                out[cur] = cur_len
            cur, cur_len = name, 0
        redefines = "REDEFINES" in s.upper()
        if redefines and lvl > 5:
            skip_level = lvl     # skip it and its children
            continue
        p = PIC.search(s)
        if p and cur:
            ln = pic_len(p.group(1), p.group(2), s.upper())
            occurs = re.search(r"OCCURS\s+(\d+)", s)
            if occurs:
                ln *= int(occurs.group(1))
            cur_len += ln
    if cur:
        out[cur] = cur_len
    return out


def main():
    errs = []
    for f in sorted((ROOT / "cobol").glob("*.cbl")) + \
            sorted((ROOT / "copylib").glob("*.cpy")):
        errs += check_columns(f)
    expected = {"NACHAREC.cpy": 94}
    for name, size in expected.items():
        for item, ln in group_lengths(ROOT / "copylib" / name).items():
            status = "ok" if ln == size else "MISMATCH"
            print(f"{name:13} {item:22} {ln:4}  {status}")
            if ln != size:
                errs.append(f"{name}: {item} is {ln} bytes, expected {size}")
    for name, size in {"ACHVREC.cpy": 100, "ACCTREC.cpy": 80,
                       "ACCTIN.cpy": 80, "RETREC.cpy": 100}.items():
        total = sum(group_lengths(ROOT / "copylib" / name).values())
        status = "ok" if total == size else "MISMATCH"
        print(f"{name:13} {'(record)':22} {total:4}  {status}")
        if total != size:
            errs.append(f"{name}: {total} bytes, expected {size}")
    print("\n".join(errs) if errs else "\nAll checks passed.")
    sys.exit(1 if errs else 0)


if __name__ == "__main__":
    main()
