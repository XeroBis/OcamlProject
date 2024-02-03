    

# Game Of Life

This project is an implementation of Conway's Game of Life, a cellular automaton devised by the British mathematician John Horton Conway. The game is a zero-player game, meaning that its evolution is determined by its initial state, requiring no further input. The project was coded, tested and used on ubuntu.

# Implemented in Python and OCaml

The Game of Life has been implemented in two different programming languages: Python and OCaml. This provides an interesting comparison of the two languages and their respective approaches to solving the same problem. The Python implementation is way faster, probably due to the way I coded the OCaml version.

## Python Implementation

### Installation

Python 3.10.12 was used, others version may work but it is not guaranteed.

```
pip install tk
```

### Usage

To run the simulation, execute the life.py file. This will launch a graphical interface where you can choose different settings for the simulation, including the version (Sequential, Parallel, or With Barrier), grid size, and initial pattern.

### Versions
- Sequential: The simulation evolves generation by generation in a sequential manner.
- Parallel: Each cell's neighbors are calculated concurrently using multiple threads.
- With Barrier: Similar to the parallel version but with an additional barrier for synchronization.


## OCaml Implementation

### Installation 

OCaml 4.07.0 was used.

### Usage

````
ocamlc -o build/game_of_life graphics.cma main.ml
./build/game_of_life
````
ou en ajoutant à la ligne 6
````
#load "graphics.cma";; 
````
et en lançant :
````
ocaml main.ml
````
