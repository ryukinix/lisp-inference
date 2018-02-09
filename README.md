# Lisp Inference

A non-full featured Lisp Inference Engine because I didn't implemented
the reductor yet. The algorithm commonly used it's the [Wang
Algorithm], maybe can you help me?

[Wang Algorithm]: https://www.cs.bham.ac.uk/research/projects/poplog/doc/popteach/wang

![screenshot](lisp-inference.png)

# Usage

Load it as a regular ASDF system as you wish and call `main` after
`(in-package :lisp-inference)` for see a truth table, the better feature
for now of this project. `tests` should show some related stuff about inference
and equivalence rules.

A better presentation and documentation will be write one day, because
in soon it's too fast and this probably doesn't happen.

# License
BSD

# Author
Manoel Vilela
