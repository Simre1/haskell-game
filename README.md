# haskell-game

A game in progress using [apecs](https://github.com/jonascarpay/apecs) for handling the game state as well as physics, [polysemy](https://github.com/polysemy-research/polysemy.git) for algebraic effects, a slightly modified version of [GPipe](https://github.com/tobbebex/GPipe-Core.git) for graphics and my own interpretation of FRP Signals*(Sigma)* for data flow management.

## Getting Started

### Prerequisites

This haskell project is using [stack](https://www.haskellstack.org), which makes it easy to reliably build haskell projects on different machines. You will also need at least [OpenGL 3.3](https://www.opengl.org/).

### Installing

The following should get you the project installed and running.

```bash
git clone https://github.com/Simre1/haskell-game.git
cd haskell-game
stack build
stack exec haskell-game-exe
```


## Checking out the code

A good place to start would be the file *./app/Main.hs*, which is the main file for the executable. If you are interested in the Signal implementation **Sigma** or how **GPipe** and **apecs** work together with **polysemy**, look no further than *./src/Sigma.hs*, *./src/Window/GPipe.hs* and *./src/ECS/Apecs.hs* respectively.
