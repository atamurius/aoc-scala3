## [Advent Of Code](https://adventofcode.com/) solutions with Scala 3

### Usage

Run with `run.sh`

### Adding more solutions

- For new advent year add `aoc<YEAR>` package with `object Main extends common.Runner(Day1, Day2, ...)`
- For new day solution create `case object DayN extends common.Day`
- Place input files if any into `src/main/resources/aoc<YEAR>/Day<N>.input`
