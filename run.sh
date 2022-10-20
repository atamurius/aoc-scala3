#!/usr/bin/env bash

if [ -z "$1" ]; then
  echo "Use ./run.sh aoc<YEAR> [--both] {Day<day-number>} | \\*"
  echo "  --both means run 1 and 2 tasks"
  echo "Available solutions:"
  for aoc in src/main/scala/aoc*; do
    echo $(basename "${aoc}"):
    DAYS=$(ls -1 ${aoc}/Day*.scala | grep -o "Day[0-9]*" | sort -V | tr '\n' ' ')
    echo "  ${DAYS}"
  done
  exit 2
fi

exec sbt --warn "runMain ${1}.Main ${@:2}"
