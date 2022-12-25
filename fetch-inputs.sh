#!/usr/bin/env bash

set -e

if [ -z "$2" ]; then
  echo "Use ./fetch-inputs.sh <year> <session-cookie-value>"
  exit 1
fi

YEAR="${1}"
SESSION="${2}"

curl --fail --cookie "session=${SESSION}" \
  "https://adventofcode.com/${YEAR}/day/[1-25]/input" \
  --output "src/main/resources/aoc${YEAR}/Day#1.input"
