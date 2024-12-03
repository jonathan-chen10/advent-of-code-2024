# Advent of Code 2024 — Jonathan Chen

My solutions to the 2024 [Advent of Code](https://adventofcode.com/2024), in Racket.

## Strategy

This is my first time using Racket, so my solutions may not be the most idiomatic. It is exciting to try, though. (Sometimes I ask Chat GPT for idiomatic syntax pointers; it is not very good at this but it gives me a starting point to look further.)

| Day                       | Part 1                                                        | Part 2                                                                                                |
| ------------------------- | ------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| [Day 1](./day_1/soln.rkt) | Apply difference pairwise, then add.                          | Map each element in first list to its similarity score and add. (not memoized)                        |
| [Day 2](./day_2/soln.rkt) | Andmap over difference list, if all within [-3, -1] or [1, 3] | Run through list ascending, if difference between item k and k-1 out of range try removing either one |
| [Day 2](./day_3/soln.rkt) | Find `mul(x,y)` with regex and sum the products               | Find commands with regex and evaluate one by one                                                      |
