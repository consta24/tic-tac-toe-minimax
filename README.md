# Tic Tac Toe Minimax AI Algorithm

This is a simple implementation of Tic Tac Toe in Scala. It allows you to play against an AI or watch the AI play against itself.

## Getting Started

To play the game, run the `Main` object in the root directory of the project.

You will be prompted to choose between playing as Player One against the AI, playing as Player Two against the AI, or watching the AI play against itself.

## How to Play

When it's your turn to play, you will be prompted to enter the coordinates of the cell you want to place your symbol in. Enter the coordinates as two space-separated integers (e.g. `1 1` for the top-left cell).

## How the AI Works

The AI uses a simple minimax algorithm with alpha-beta pruning to determine the best move to make. The algorithm assigns a score to each possible move based on the number of sequences (i.e. rows, columns, or diagonals) that could be completed by placing a symbol in that cell. The AI chooses the move with the highest score.

