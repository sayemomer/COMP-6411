"""
Random selection of the word for a new game. It will be stored in StringDatabase.py.
"""

import random

def get_random_word(filename='four_letters.txt'):
    with open(filename, "r") as file:
        text = file.read()
        words = text.split()
        return random.choice(words)