#!/usr/bin/env bash

if [ -z "$1" ]; then
  echo "Use ./run.sh aoc<YEAR> [--both] {Day<day-number>} | \\*"
  echo "  --both means run 1 and 2 tasks"
  echo "Available solutions:"
  for aoc in src/main/scala/aoc*; do
    echo $(basename "${aoc}"):
    for day in ${aoc}/Day*.scala; do
      echo -n " $(echo "${day}" | grep -o "Day[0-9]*")"
    done
    echo
  done
  exit 2
fi

exec sbt --warn "runMain ${1}.Main ${@:2}"
