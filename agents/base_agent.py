from abc import ABC, abstractmethod
from typing import List, Optional, Tuple

from game.card import Card
from game.action import Action, ActionType, ActionResult
from game.game_state import GameState

class BaseAgent(ABC):
    """Abstract base class for all Clue agents."""

    def __init__(self, player_id: int):
        """
        Initialize the agent.

        Args:
            player_id: The ID of the player this agent controls
        """
        self.player_id = player_id
        self.hand = []  # Cards in the agent's hand

    def set_hand(self, cards: List[Card]):
        """Set the agent's hand of cards."""
        self.hand = cards

    @abstractmethod
    def make_move(self, game_state: GameState) -> Action:
        """
        Decide on an action to take.

        Args:
            game_state: The current state of the game

        Returns:
            An Action object representing the agent's move
        """
        pass

    @abstractmethod
    def respond_to_suggestion(self, suggestion: Action, game_state: GameState) -> Optional[Card]:
        """
        Respond to another player's suggestion.

        Args:
            suggestion: The suggestion action
            game_state: The current state of the game

        Returns:
            A card to show, or None if the agent cannot refute
        """
        pass

    @abstractmethod
    def observe_result(self, result: ActionResult, game_state: GameState):
        """
        Observe the result of an action (own or other player's).

        Args:
            result: The result of the action
            game_state: The current state of the game
        """
        pass

    @abstractmethod
    def game_over(self, winner: Optional[int], solution: Tuple[Card, Card, Card]):
        """
        Called when the game is over.

        Args:
            winner: The ID of the winning player, or None if no one won
            solution: The correct solution (murder cards)
        """
        pass
