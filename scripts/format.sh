#!/usr/bin/env bash
#
# Format and lint all Haskell sources in reflect-cc.
#
#   ./scripts/format.sh            Format in place (fourmolu) and lint (hlint).
#   ./scripts/format.sh --check    Verify formatting without writing; fail if
#                                  anything is unformatted or hlint complains.
#                                  Suitable for CI.
#
# Reads fourmolu.yaml and .hlint.yaml from the repository root.

set -euo pipefail

# Resolve the repository root (the parent of this script's directory) so the
# script works regardless of the current working directory.
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

MODE="inplace"
case "${1:-}" in
    --check) MODE="check" ;;
    "")      MODE="inplace" ;;
    -h | --help)
        sed -n '3,12p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
    *)
        echo "error: unknown argument '$1' (try --check or --help)" >&2
        exit 2
        ;;
esac

# Require the tools up front with a clear message rather than a cryptic failure.
for tool in fourmolu hlint; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo "error: '$tool' not found in PATH (install with: cabal install $tool)" >&2
        exit 127
    fi
done

# Collect every .hs file under the package source directories. Uses a NUL-safe
# loop so the script is portable to the bash 3.2 that ships with macOS.
SRC_DIRS=()
for d in src test benchmark; do
    [ -d "$d" ] && SRC_DIRS+=("$d")
done

HS_FILES=()
while IFS= read -r -d '' f; do
    HS_FILES+=("$f")
done < <(find "${SRC_DIRS[@]}" -type f -name '*.hs' -print0)

if [ "${#HS_FILES[@]}" -eq 0 ]; then
    echo "No Haskell source files found; nothing to do."
    exit 0
fi

echo "==> fourmolu (--mode ${MODE}) on ${#HS_FILES[@]} file(s)"
fourmolu --mode "$MODE" "${HS_FILES[@]}"

echo "==> hlint on ${#HS_FILES[@]} file(s)"
hlint "${HS_FILES[@]}"

echo "==> done"
