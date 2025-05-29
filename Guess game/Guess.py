"""
It will represent the core
application logic itself (menu display, user input, scoring logic, etc.).
"""

import os

from Game import Game
from StringDatabase import get_random_word

SUCCESS = "Success"
GAVE_UP = "Gave up"
PLAYED = "played"

class Guess:
    def __init__(self):
        self.curr_game = Game(get_random_word()) # This will fetch a new word
        self.rounds = []
        self.set_round()

    def set_round(self):
        self.rounds.append(self.curr_game)
    
    def reset(self):
        self.calculate_score()
        self.curr_game = Game(get_random_word())
        self.set_round()

    def get_freq_score(self,letter):
        letter_frequencies = {
            'a': 8.17,
            'b': 1.49,
            'c': 2.78,
            'd': 4.25,
            'e': 12.70,
            'f': 2.23,
            'g': 2.02,
            'h': 6.09,
            'i': 6.97,
            'j': 0.15,
            'k': 0.77,
            'l': 4.03,
            'm': 2.41,
            'n': 6.75,
            'o': 7.51,
            'p': 1.93,
            'q': 0.10,
            'r': 5.99,
            's': 6.33,
            't': 9.06,
            'u': 2.76,
            'v': 0.98,
            'w': 2.36,
            'x': 0.15,
            'y': 1.97,
            'z': 0.07
        }
        return letter_frequencies[letter]


    def calculate_score(self):
        #letter still hidden
        curr_score = self.curr_game.score;
        for i,c in enumerate(self.curr_game.current_guess):
            if c == "-":
               curr_score+=self.get_freq_score(self.curr_game.current_word[i])
               self.curr_game.missed_letters += 1
        
        if self.curr_game.status == GAVE_UP:
            #neg for giving up 
            curr_score = - curr_score
        else:
            #Division
            if len(self.curr_game.letters_guessed) > 0:
                curr_score = curr_score/len(self.curr_game.letters_guessed)
            #Wrong-word penalties
            curr_score = curr_score * (100 - self.curr_game.bad_guesses * 10)/100

        self.curr_game.score = curr_score

    def try_guess(self):
        word_guessed = input("Make your guess: ").lower()
        if word_guessed == self.curr_game.current_word:
                print("@@\n"+
                  "@@ FEEDBACK:You are right , Einstein\n"+
                  "@@")
                self.curr_game.set_status(SUCCESS)
                self.reset()
        else:
            print("@@\n"+
                  "@@ FEEDBACK: Try again , Loser\n"+
                  "@@")
            self.curr_game.bad_guesses += 1
        input("Press Enter to continue...")


    def try_letter(self):
    
        letter_guessed = input("Enter a letter: ").lower()

        while len(letter_guessed) != 1 or not letter_guessed.isalpha():
            letter_guessed = input("Enter a letter: ").lower()
           
        if letter_guessed in self.curr_game.current_word:
            count = self.curr_game.current_word.count(letter_guessed)
            print(f"@@\n@@ FEEDBACK: Woo hoo, you found {count} letters\n@@")
            self.curr_game.letters_guessed.append(''.join(letter_guessed))

            for index , curr in enumerate(self.curr_game.current_word):
                if curr == letter_guessed:
                    self.curr_game.current_guess[index] = letter_guessed
            curr = ''.join(self.curr_game.current_guess)
            if curr == self.curr_game.current_word:
                    self.curr_game.set_status(SUCCESS)
                    self.reset()
                    
        else:
            print("@@\n"+
                  "@@ FEEDBACK: Not a single match, genius\n"+
                  "@@")
            self.curr_game.letters_guessed.append(''.join(letter_guessed))
        input("Press Enter to continue...")

    def give_up(self):
        print(f"@@\n@@ FEEDBACK: You should have guessed this... '{self.curr_game.current_word}'\n@@")
        input("Press Enter to continue...")
        self.curr_game.set_status(GAVE_UP)
        self.reset()


    def print_score(self):
        self.clear_screen()

        print("++")
        print("++ Game Report")
        print("++")
        print("{:<5} {:<6} {:<8} {:<13} {:<15} {:<6}".format(
            "Game", "Word", "Status", "Bad Guesses", "Missed Letters", "Score"
        ))
        print("-" * 60)
        total_score = 0
        for i, game in enumerate(self.rounds, start=1):
                if game.is_played == True:
                    print("{:<5} {:<6} {:<8} {:<13} {:<15} {:>5.2f}".format(
                        i,
                        game.current_word,
                        game.status,
                        game.bad_guesses,
                        game.missed_letters,
                        game.score
                    ))
                    total_score += game.score

        print(f"\nFinal Score: {total_score:.2f}")
        

    def clear_screen(self):
        os.system('cls' if os.name == 'nt' else 'clear')


    def print_menu(self,mode):
            print("++\n" \
            "++ The great guesssing game \n" \
            "++")
            
            if mode.lower() == "play" :
                if self.curr_game.current_guess == []:
                    print("Current guess: ---- "  )
                else:
                    print(f"Current guess: {''.join(self.curr_game.current_guess)}" )
            if mode.lower() == "test":

                print(f"Current Word: {self.curr_game.current_word}")
                print(f"Current guess: {''.join(self.curr_game.current_guess)}" )
                    

            if self.curr_game.letters_guessed == []:
                print(f"Letters guessed:")
            else:
                print(f"Letters guessed: {','.join(self.curr_game.letters_guessed)}")
            
            print("g = guess, t=tell me, l for letter, and q to quit")

    def main_menu(self,mode):

        while True:
            self.clear_screen()
            self.print_menu(mode)

            match input("Enter option: ").lower():
                case "g":
                    self.curr_game.is_played = True
                    self.try_guess()
                case "t":
                    self.curr_game.is_played = True
                    self.give_up()
                case "l":
                    self.curr_game.is_played = True
                    self.try_letter()
                case "q":
                        self.curr_game.set_status(GAVE_UP)
                        self.calculate_score()
                        self.print_score()
                        break
                case _:
                    input("Invalid Option. Please re-enter: ")




