import random
from typing import List, Optional, Tuple, Dict, Set

from .base_agent import BaseAgent
from game.card import Card, CardType, ALL_CARDS
from game.action import Action, ActionType, ActionResult
from game.game_state import GameState

class HeuristicAgent(BaseAgent):
    """A simple rule-based agent that uses basic elimination."""

    def __init__(self, player_id: int):
        """
        Initialize the heuristic agent.

        Args:
            player_id: The ID of the player this agent controls
        """
        super().__init__(player_id)
        self.possible_cards = {
            CardType.SUSPECT: set(),
            CardType.WEAPON: set(),
            CardType.ROOM: set()
        }
        self.eliminated_cards = set()
        self.player_cards = {}  # What we know other players have

    def set_hand(self, cards: List[Card]):
        """Set the agent's hand and initialize knowledge."""
        super().set_hand(cards)

        # Add own cards to eliminated
        for card in cards:
            self.eliminated_cards.add(card)

    def make_move(self, game_state: GameState) -> Action:
        """Make a move based on current knowledge."""
        # If we're confident about the solution, make an accusation
        if len(self.possible_cards[CardType.SUSPECT]) == 1 and \
           len(self.possible_cards[CardType.WEAPON]) == 1 and \
           len(self.possible_cards[CardType.ROOM]) == 1:

            return Action(
                action_type=ActionType.ACCUSATION,
                suspect=next(iter(self.possible_cards[CardType.SUSPECT])),
                weapon=next(iter(self.possible_cards[CardType.WEAPON])),
                room=next(iter(self.possible_cards[CardType.ROOM]))
            )

        # Otherwise, make a suggestion with one card from each category
        # Preferring cards we haven't seen yet
        suspect = self._choose_card(CardType.SUSPECT)
        weapon = self._choose_card(CardType.WEAPON)
        room = self._choose_card(CardType.ROOM)

        return Action(
            action_type=ActionType.SUGGESTION,
            suspect=suspect,
            weapon=weapon,
            room=room
        )

    def _choose_card(self, card_type: CardType) -> Card:
        """Choose a card for a suggestion, preferring those not eliminated."""
        all_cards = [card for card in ALL_CARDS if card.card_type == card_type]
        unknown_cards = [card for card in all_cards if card not in self.eliminated_cards]

        if unknown_cards:
            return random.choice(unknown_cards)

        # If all cards are known, just pick randomly
        return random.choice(all_cards)

    def respond_to_suggestion(self, suggestion: Action, game_state: GameState) -> Optional[Card]:
        """Respond to a suggestion based on cards in hand."""
        matching_cards = [
            card for card in self.hand
            if card == suggestion.suspect or card == suggestion.weapon or card == suggestion.room
        ]

        if not matching_cards:
            return None

        # Simple strategy: return the first matching card
        return matching_cards[0]

    def observe_result(self, result: ActionResult, game_state: GameState):
        """Update knowledge based on an action result."""
        if result.player_id == self.player_id:
            # Our suggestion was refuted
            if result.refuted and result.card_shown:
                # We saw a card
                self.eliminated_cards.add(result.card_shown)

                # Remember which player has this card
                if result.refuter_id not in self.player_cards:
                    self.player_cards[result.refuter_id] = set()
                self.player_cards[result.refuter_id].add(result.card_shown)

            elif not result.refuted:
                # No one refuted our suggestion, these could be solution cards
                for card in [result.action.suspect, result.action.weapon, result.action.room]:
                    if card not in self.eliminated_cards:
                        self.possible_cards[card.card_type].add(card)

        else:
            # Someone else's suggestion
            if result.refuted:
                if result.refuter_id == self.player_id:
                    # We refuted it, nothing new to learn
                    pass
                else:
                    # Someone else refuted, remember they have at least one of these cards
                    if result.card_shown:
                        # We saw which card was shown (variant rule)
                        self.eliminated_cards.add(result.card_shown)

                        if result.refuter_id not in self.player_cards:
                            self.player_cards[result.refuter_id] = set()
                        self.player_cards[result.refuter_id].add(result.card_shown)
            else:
                # No one refuted, these could be solution cards
                for card in [result.action.suspect, result.action.weapon, result.action.room]:
                    if card not in self.eliminated_cards:
                        self.possible_cards[card.card_type].add(card)

    def game_over(self, winner: Optional[int], solution: Tuple[Card, Card, Card]):
        """Process game outcome."""
        # Basic agent doesn't learn from game outcomes
        pass
