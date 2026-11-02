#!/bin/sh
# Print a per source file line coverage summary of gidgen from D coverage .lst files.
# .lst line format: "<count>|<source>" where count is blank for non executable lines and 0000000 for missed lines.
# The .lst file names are the source paths with '/' replaced by '-' (for example "..-source-gir-func.lst", note the
# leading dots make them hidden files).
# Usage: coverage-summary.sh <lst directory>

dir=${1:-coverage/lst}

find "$dir" -name '*source-*.lst' | while read -r f; do
  name=$(basename "$f" .lst | sed -e 's/^.*source-//' -e 's|-|/|g').d
  awk -F'|' -v name="$name" '
    /^ *[0-9]+\|/ { total++; if ($1 + 0 > 0) hit++ }
    END { printf "%-28s %5d / %5d  %5.1f%%\n", name, hit, total, total ? 100 * hit / total : 0 }' "$f"
done | sort | awk '{ print; hit += $2; total += $4 }
  END { printf "%-28s %5d / %5d  %5.1f%%\n", "TOTAL", hit, total, total ? 100 * hit / total : 0 }'
