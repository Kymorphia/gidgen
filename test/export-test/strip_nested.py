#!/usr/bin/env python3
"""Copy a GIR file without the records and unions nested in records, which g-ir-compiler 1.84 can't parse (it
aborts with "state_switch: assertion failed"). Used for the typelib of the GidgenFeat test library."""
import re
import sys

src, dst = sys.argv[1:3]
out, depth, skip = [], 0, 0

for line in open(src):
    s = line.strip()
    if skip:
        if re.match(r'</(record|union)>', s):
            skip -= 1
        elif re.match(r'<(record|union)\b', s) and not s.endswith('/>'):
            skip += 1
        continue
    if depth > 0 and re.match(r'<(record|union)\b', s) and not s.endswith('/>'):
        skip = 1
        continue
    if re.match(r'<record\b', s) and not s.endswith('/>'):
        depth += 1
    elif s == '</record>':
        depth -= 1
    out.append(line)

open(dst, 'w').write(''.join(out))
