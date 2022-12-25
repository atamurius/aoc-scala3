#!/usr/bin/env bash

SCALA_VERSION="3.0.0"

set -e

if [ -z "$1" ]; then
  echo "Use ./run.sh [--assembly] <YEAR> [--both] [--skip-tests] {Day<day-number>} | \\*"
  echo "  --both means run 1 and 2 tasks"
  echo "Available solutions:"
  for aoc in src/main/scala/aoc*; do
    echo $(basename "${aoc}"):
    DAYS=$(ls -1 ${aoc}/Day*.scala | grep -o "Day[0-9]*" | sort -V | tr '\n' ' ')
    echo "  ${DAYS}"
  done
  exit 2
fi

if [ "$1" == "--assembly" ]; then
  YEAR="${2#aoc}"
  sbt assembly
  exec java -Xmx6g -jar "target/scala-${SCALA_VERSION}/advent-of-code-scala3-assembly-1.0.0.jar" \
     "${YEAR}" ${@:3}
else
  YEAR="${1#aoc}"
  exec sbt --warn "run ${YEAR} ${@:2}"
fi
