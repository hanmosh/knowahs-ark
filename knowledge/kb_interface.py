from abc import ABC, abstractmethod
from typing import List, Tuple, Dict, Set, Optional
from game.card import Card
from game.action import Action
from game.game_state import GameState

class KnowledgeBase(ABC):
    """Abstract base class for knowledge base implementations."""

    @abstractmethod
    def initialize(self, num_players: int, all_cards: List[Card], player_id: int):
        """Initialize the knowledge base with game information."""
        pass

    @abstractmethod
    def add_card_knowledge(self, player_id: int, card: Card, has_card: bool):
        """
        Add knowledge about a player having or not having a card.

        Args:
            player_id: The ID of the player
            card: The card
            has_card: True if player has the card, False otherwise
        """
        pass

    @abstractmethod
    def add_negative_knowledge(self, player_id: int, card: Card):
        """
        Add knowledge that a player does not have a card.

        Args:
            player_id: The ID of the player
            card: The card
        """
        pass

    @abstractmethod
    def add_disjunctive_knowledge(self, cards: List[Card]):
        """
        Add disjunctive knowledge: at least one of these cards is in the solution.

        Args:
            cards: The list of cards
        """
        pass

    @abstractmethod
    def add_disjunctive_player_knowledge(self, player_id: int, cards: List[Card]):
        """
        Add disjunctive knowledge: player has at least one of these cards.

        Args:
            player_id: The ID of the player
            cards: The list of cards
        """
        pass

    @abstractmethod
    def update_solution_knowledge(self, card: Card, in_solution: bool):
        """
        Update knowledge about whether a card is in the solution.

        Args:
            card: The card
            in_solution: True if the card is in the solution, False otherwise
        """
        pass

    @abstractmethod
    def can_solve_case(self, confidence_threshold: float = 0.95) -> bool:
        """
        Determine if the case can be solved with sufficient confidence.

        Args:
            confidence_threshold: The confidence threshold (0.0 to 1.0)

        Returns:
            True if the case can be solved, False otherwise
        """
        pass

    @abstractmethod
    def get_solution(self) -> Tuple[Card, Card, Card]:
        """
        Get the solution (murder cards) based on current knowledge.

        Returns:
            A tuple of (suspect, weapon, room) cards
        """
        pass

    @abstractmethod
    def get_best_suggestion(self) -> Tuple[Card, Card, Card]:
        """
        Get the best suggestion to make based on current knowledge.

        Returns:
            A tuple of (suspect, weapon, room) cards
        """
        pass

    @abstractmethod
    def choose_card_to_show(self, cards: List[Card], suggestion: Action, game_state: GameState) -> Card:
        """
        Choose which card to show to refute a suggestion.

        Args:
            cards: The list of cards that can refute the suggestion
            suggestion: The suggestion action
            game_state: The current game state

        Returns:
            The card to show
        """
        pass

    @abstractmethod
    def check_for_bluffing(self, player_id: int, action: Action, game_state: GameState):
        """
        Check if a player might be bluffing in their suggestion.

        Args:
            player_id: The ID of the player who made the suggestion
            action: The suggestion action
            game_state: The current game state
        """
        pass

    @abstractmethod
    def export_to_neo4j(self, uri: str, user: str, password: str):
        """
        Export the knowledge graph to Neo4j.

        Args:
            uri: The Neo4j URI
            user: The Neo4j username
            password: The Neo4j password
        """
        pass
