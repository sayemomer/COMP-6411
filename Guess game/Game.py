"""
Maintain information about a specific game (needed
for the final report
"""

class Game:
    def __init__(self,current_word):
        self.current_word = current_word
        self.current_guess = ["-","-","-","-"]
        self.letters_guessed = []
        self.status = ""
        self.bad_guesses = 0
        self.missed_letters =0
        self.score = 0
        self.is_played = bool

    def set_status(self,status):
        self.status = status