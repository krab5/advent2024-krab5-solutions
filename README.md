# krab5's solution for the Advent of Code 2024

_(check out the [AoC Page](https://adventofcode.com/2024))_


## Foreword

Another year another AoC. I hope to make it further than last year; we'll see
how this end of year treats me I guess...

This year I decided to do it in Scala (3). This is a language I want to delve
into more, but didn't have the opportunity to do so. During this AoC, I want to
explore what can be done both in term of algorithm and architecture (this is why
you may see over-engineered solutions here and there).

The unique intersection of object-oriented and functional programming of Scala
enables making large scale software with design patterns while writing super
efficient iterators... I am eager to see where it's going uwu.

I decided to do everything in the same project for convenience. This will also
force me to think a bit about the structure of problems and the way to execute
a given problem.


## Day 01: location identification

As often during the first days of the AoC, the main time I spent coding was not
on the challenge itself, which is quite easy as it is day 1. Instead, I wrote an
entire parser combinator library; first because I just _know_ I'm gonna need it
at some point, and also because this is a task with which I am very familiar but
is also not very trivial (and which details depend heavily on the language).

From what I tested, it seems to be a success functionality-wise. We'll have to
see how it runs on more complex/larger examples.

<br>

For now I am still struggling a bit with the intersection of various concepts
from the world of (mutable) OOP and strict FP... Nonetheless, I find the
language to be quite comfortable and convenient. I am afraid though it suffers
from the same problems as C++, in particular the fact that there an immense
variety of solutions to the same problem (meaning keeping consistency in the
code is not always easy), and also the spectre of the original language is
looming all over the code (C for C++, and Java for Scala).

Having access to Java is both a blessing and a curse: I am glad I can just open
a file with `File` and `FileReader`; but on the other hand the rigour of Scala
makes the interfacing a bit awkward sometimes, meaning you have to do a lot of
sensible wrapping (not that it bothers me, this is how you should do it in any
way).


## Day 02: report checking

A nice little problem; I solve part 2 with a kinda brute force approach,
noticing that the state space was reasonably small (sequence of no more than a
dozen of numbers or so). Had it not been the case, I would have tried to find
interesting heuristics to skim down solution browsing, but oh well...





