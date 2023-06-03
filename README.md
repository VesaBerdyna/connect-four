Connect 4 Game in Haskell with Gloss and Minimax AI

This repository contains an implementation of the popular Connect 4 game using the Haskell programming language. It utilizes the Gloss library for rendering the game's graphical interface and incorporates a Minimax AI algorithm to provide a challenging opponent.


Table of Contents

- Requirements
- Installation
- Usage
- Game Rules


Requirements

To run this Connect 4 game, you need the following:
- Haskell (GHC) installed on your machine.
- The Gloss library for Haskell.


Installation

1. Clone this repository to your local machine using `git clone`.
	
	git clone https://github.com/your-username/connect-4-haskell.git


2. Change into the cloned directory.

	cd connect-4-haskell



Usage

To play the Connect 4 game, follow these steps:

1. Open a terminal or command prompt and navigate to the repository's directory.

2. Compile the Haskell source code using the GHC compiler.

		ghc --make Main.hs

3. Run the compiled executable.
		
		./Main

4. The game window will open, and you can now start playing Connect 4 against the Minimax AI opponent.

5. Use the left and right arrow keys to move the cursor and the space bar to drop a disc into the desired column.

6. Play the game by taking turns with the AI opponent, and aim to connect four discs of your color either vertically, horizontally, or diagonally to win the game.

7. To quit the game, close the game window or press the escape key.



Game Rules

Connect 4 is a two-player game played on a 6x7 grid. The players take turns dropping colored discs into the columns of the grid. The discs fall down to the lowest available position within the chosen column.

The objective of the game is to be the first to connect four of your colored discs in a row, either horizontally, vertically, or diagonally. The game ends in a draw if the grid is completely filled without any player achieving a winning combination.

