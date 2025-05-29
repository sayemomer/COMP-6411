"""
Simply parses
the command line parameter and starts the guessing game
"""
import sys

from Guess import Guess

PLAY = "play"
TEST = "test"

def start_game(mode):
    game = Guess()
    game.main_menu(mode)

if __name__ == "__main__":

    if len(sys.argv) == 2 :
        if sys.argv[1] == PLAY:
            start_game(PLAY)
        elif sys.argv[1] == TEST:
            start_game(TEST)
        else:
            print("Invalid Option")
    else:
        print("Invalid perameters")



