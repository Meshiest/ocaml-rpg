## OCaml Text Based RPG

This is a very rudimentary text based RPG built in OCaml. The "game" part is in `rpg.ml` while the rest of the core mechanics lie in other files. This project was originally created to be extensible and modifiable similar to a game engine. If you peruse through the `rpg.ml` file, you can get the basic idea of how a game built in this engine is structured.

Cool features:

* Quadtree room storage (see `quadtree.ml` for implementation) - Room addition and lookup are O(log n) time with O(n) worst
* Undecided option - Various components in the game can change based on the current state with a fancy "Undecided" option that returns a `Some` or `None` when the function it holds is run
* Functional - There are no for loops, while loops, or classes in this project. Only recursion and algebraic data types!

To run this, you need OCaml and ocamlbuild installed.

Potential extensions:

* More than one interaction per room
* Elemental weapons that deal more damage to weaker typed monsters (fire sword critically damages plant monster)
* Shopkeepers with limited inventory as part of room interaction (also where you can sell loot items)
* Interactive puzzles similar to how monster battles work (solve a simple math or word puzzle)
* Weapon durability (Weapons break after a certain number of uses or battles)
* Health potion items that do not overflow health over a certain maximum
* Leveling and experience: killing monsters -> get experience, progressively more experience -> level
* A map command that prints all visited rooms in a grid (not a list of coords)
* Saving to file, Loading from file
* Battle more than one monster at a time
* Checkpoints to return to when the player dies
* Mining resources / farming: ore can be mined, plants can be picked rate limited by real time

### Build

    make

### Run

    make run

### Cleanup

    make clean

### Screenshots

<img src="https://i.imgur.com/B9lzmmG.png" width="250">
<img src="https://i.imgur.com/zp8HPVc.png" width="250">
