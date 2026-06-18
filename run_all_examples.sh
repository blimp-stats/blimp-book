#!/usr/bin/env bash
#
# run_ch2to4.sh
# Runs every analysis script in Ch2–Ch4, each in its own fresh Rscript
# process, sequentially and at low priority so RStudio stays usable.
#
# Excludes "4.X Interaction w Sum Score.R" by design.
# Each script's console output (and any errors) goes to run_logs/<name>.log.
#
# Usage:
#   cd to the repo, then:   bash run_ch2to4.sh
#   To keep the Mac awake for a long run:   caffeinate -i bash run_ch2to4.sh
#   To run in the background:                nohup bash run_ch2to4.sh &>/dev/null &

set -u

# repo root = the folder containing this script
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$ROOT/run_logs"
mkdir -p "$LOG_DIR"

DIRS=("Ch2_Regression" "Ch3_Categorical" "Ch4_Interactions")
EXCLUDE_REGEX='4\.X'                       # skip "4.X Interaction w Sum Score.R"

RSCRIPT="$(command -v Rscript || echo /usr/local/bin/Rscript)"
if [[ ! -x "$RSCRIPT" ]]; then
  echo "ERROR: Rscript not found. Install R or edit RSCRIPT in this script." >&2
  exit 1
fi

echo "Rscript : $RSCRIPT"
echo "Repo    : $ROOT"
echo "Logs    : $LOG_DIR"
echo

ok=0; fail=0; failed_list=()
batch_start=$(date +%s)

for d in "${DIRS[@]}"; do
  [[ -d "$ROOT/$d" ]] || { echo "MISSING dir: $d"; continue; }
  while IFS= read -r -d '' f; do
    base="$(basename "$f")"
    if [[ "$base" =~ $EXCLUDE_REGEX ]]; then
      echo "SKIP  $base"
      continue
    fi
    log="$LOG_DIR/${base%.R}.log"
    printf "RUN   %-55s " "$base"
    start=$(date +%s)
    # run in LOG_DIR so any stray Rplots.pdf lands there, not in the repo;
    # nice -n 10 keeps it below RStudio in CPU priority.
    if ( cd "$LOG_DIR" && nice -n 10 "$RSCRIPT" "$f" ) >"$log" 2>&1; then
      echo "ok ($(( $(date +%s) - start ))s)"
      ok=$((ok+1))
    else
      echo "FAILED (see ${log##*/})"
      fail=$((fail+1)); failed_list+=("$base")
    fi
  done < <(find "$ROOT/$d" -maxdepth 1 -name "*.R" -print0 | sort -z)
done

echo
echo "Done in $(( ($(date +%s) - batch_start) / 60 )) min:  $ok ok, $fail failed."
if (( fail )); then
  printf '  FAILED: %s\n' "${failed_list[@]}"
  echo "  Check the matching .log files in $LOG_DIR"
fi
