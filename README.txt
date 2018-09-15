# Texas Hold'Em

## Developed By
Elaine Hwang \
Benjamin Steeper \
Eric Sun \
Kristi Fok \
For CS3110 in the Spring 2018 Semester

## Information
Texas Hold'Em is a Notty-based game implemented in OCaml for the classic
poker game, allowing for anywhere between 2 and 8 players in each game.
In this game, three different AI difficulties can be found, and different
AI levels may be used in a single game.

## How to Run the Game

In order to run this game, you will need to have OCaml installed, as well as
bash. If you do not have these installed, please follow the instructions
[here](http://www.cs.cornell.edu/courses/cs3110/2018sp/install.html). Next, you
will need to install external packages. \
Install notty and ANSITerminal using the following commands in bash:

```
opam install ansiterminal
opam install notty
```
In order to run the game, type the following command into bash, while in the
directory that contains all the source code for this game:
```
make play
```
