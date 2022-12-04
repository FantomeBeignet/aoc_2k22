# aoc_2k22
My solutions for the 2022 Advent of Code

## Language choice

I chose to do this year's Advent of Code in OCaml because I like the language a lot but haven't had an occasion to practice in more than a year. I also wanted to get more familiar with the `dune` build system, as I haven't really had the occasion to use it previously.

## Project structure

All my solutions can be found under the `bin/` folder as `dayX.ml` files. Some utility functions that are often reused are grouped under the `lib/aoc_2k22.ml` file.

To build the whole thing, you simply have to run the `dune build` command. To run a specific day, use the `dune exec dayX` command.
