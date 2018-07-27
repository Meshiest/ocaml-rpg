## OCaml Text Based RPG

This is a very rudimentary text based RPG built in OCaml. The "game" part is in `rpg.ml` while the rest of the core mechanics lie in other files. This project was originally created to be extensible and modifiable similar to a game engine. If you peruse through the `rpg.ml` file, you can get the basic idea of how a game built in this engine is structured.

Cool features:

* Quadtree room storage (see `quadtree.ml` for implementation) - Room addition and lookup are O(log n) time with O(n) worst
* Undecided option - Various components in the game can change based on the current state with a fancy "Undecided" option that returns a `Some` or `None` when the function it holds is run
* Functional - There are no for loops, while loops, or classes in this project. Only recursion and algebraic data types!

To run this, you need OCaml installed.

### Build

    make

### Run

    make run

### Cleanup

    make clean
