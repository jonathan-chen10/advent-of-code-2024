# Advent of Code 2024 — Jonathan Chen

My solutions to the 2024 [Advent of Code](https://adventofcode.com/2024), in Racket.

## Strategy

This is my first time using Racket, so my solutions may not be the most idiomatic. It is exciting to try, though. (Sometimes I ask Chat GPT for idiomatic syntax pointers; it is not very good at this but it gives me a starting point to look further.)

| Day                       | Part 1                               | Part 2                                                                         |
| ------------------------- | ------------------------------------ | ------------------------------------------------------------------------------ |
| [Day 1](./day_1/soln.rkt) | Apply difference pairwise, then add. | Map each element in first list to its similarity score and add. (not memoized) |
