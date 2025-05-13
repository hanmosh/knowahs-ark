from enum import Enum, auto

class CardType(Enum):
    """Enumeration of card types in Clue."""
    SUSPECT = auto()
    WEAPON = auto()
    ROOM = auto()

class Card:
    """Representation of a Clue card."""

    def __init__(self, name, card_type):
        """
        Initialize a card.

        Args:
            name (str): The name of the card
            card_type (CardType): The type of the card
        """
        self.name = name
        self.card_type = card_type

    def __eq__(self, other):
        if not isinstance(other, Card):
            return False
        return self.name == other.name and self.card_type == other.card_type

    def __hash__(self):
        return hash((self.name, self.card_type))

    def __str__(self):
        return f"{self.name}"

    def __repr__(self):
        return f"Card({self.name}, {self.card_type.name})"

# Standard Clue cards
SUSPECTS = [
    Card("Colonel Mustard", CardType.SUSPECT),
    Card("Miss Scarlet", CardType.SUSPECT),
    Card("Professor Plum", CardType.SUSPECT),
    Card("Mr. Green", CardType.SUSPECT),
    Card("Mrs. Peacock", CardType.SUSPECT),
    Card("Dr. Orchid", CardType.SUSPECT)
]

WEAPONS = [
    Card("Knife", CardType.WEAPON),
    Card("Candlestick", CardType.WEAPON),
    Card("Revolver", CardType.WEAPON),
    Card("Rope", CardType.WEAPON),
    Card("Lead Pipe", CardType.WEAPON),
    Card("Wrench", CardType.WEAPON)
]

ROOMS = [
    Card("Hall", CardType.ROOM),
    Card("Lounge", CardType.ROOM),
    Card("Dining Room", CardType.ROOM),
    Card("Kitchen", CardType.ROOM),
    Card("Ballroom", CardType.ROOM),
    Card("Conservatory", CardType.ROOM),
    Card("Billiard Room", CardType.ROOM),
    Card("Library", CardType.ROOM),
    Card("Study", CardType.ROOM)
]

ALL_CARDS = SUSPECTS + WEAPONS + ROOMS

def get_card_by_name(name):
    """Get a card by its name."""
    for card in ALL_CARDS:
        if card.name.lower() == name.lower():
            return card
    return None
