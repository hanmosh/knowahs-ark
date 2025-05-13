from dataclasses import dataclass
from enum import Enum, auto
from typing import Optional, Tuple, List
from .card import Card

class ActionType(Enum):
    """Types of actions a player can take in Clue."""
    SUGGESTION = auto()  # Regular suggestion/accusation within a room
    ACCUSATION = auto()  # Final accusation to win the game

@dataclass
class Action:
    """Representation of a player action in Clue."""

    action_type: ActionType
    suspect: Card
    weapon: Card
    room: Card

    def __str__(self):
        action_name = "Suggestion" if self.action_type == ActionType.SUGGESTION else "Accusation"
        return f"{action_name}: {self.suspect.name} with the {self.weapon.name} in the {self.room.name}"

@dataclass
class ActionResult:
    """Result of an action in Clue."""

    action: Action
    player_id: int  # Player who took the action
    refuted: bool  # Whether the action was refuted
    refuter_id: Optional[int] = None  # Player who refuted, if any
    card_shown: Optional[Card] = None  # Card shown to refute, if any
    is_winning_accusation: Optional[bool] = None  # For ACCUSATION actions
