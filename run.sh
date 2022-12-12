#!/usr/bin/env bash

SCALA_VERSION="3.0.0"

if [ -z "$1" ]; then
  echo "Use ./run.sh [--assembly] aoc<YEAR> [--both] {Day<day-number>} | \\*"
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
  sbt assembly
  exec java -cp "target/scala-${SCALA_VERSION}/advent-of-code-scala3-assembly-1.0.0.jar" "${2}.Main" ${@:3}
else
  exec sbt --warn "runMain ${1}.Main ${@:2}"
fi
