# Texas Hold'Em

## Developed By
Elaine Hwang \
Benjamin Steeper \
Eric Sun \
Kristi Fok \
For CS3110 in the Spring 2018 Semester

## Information
Texas Hold'Em is a Notty-based game implemented in OCaml for the classic
poker game, allowing for anywhere between 2 and 6 players in each game.
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
Once you have downloaded the game, ensure that you have the correct environment
and packages installed by typing the following command in bash, while in the
directory containing source code for the game:
```
make check
```
After typing this command, you should get a series of messages, ending with:
"Your function names....Congratulations!" If you get this message, you're good
to go! If not, something has gone wrong. Check your OPAM installation, or
install the packages that are not active.

In order to run the game, type the following command into bash, while in the
directory that contains all the source code for this game:
```
make play
```
