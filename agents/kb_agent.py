from typing import List, Optional, Tuple, Dict, Set

from .base_agent import BaseAgent
from ..game.card import Card, CardType
from ..game.action import Action, ActionType, ActionResult
from ..game.game_state import GameState
from ..knowledge.kb_interface import KnowledgeBase

class KnowledgeBasedAgent(BaseAgent):
    """An agent that uses a knowledge base for reasoning."""

    def __init__(self, player_id: int, kb_class):
        """
        Initialize the knowledge-based agent.

        Args:
            player_id: The ID of the player this agent controls
            kb_class: The knowledge base class to use
        """
        super().__init__(player_id)
        self.kb = kb_class()

    def initialize_kb(self, num_players: int, all_cards: List[Card]):
        """Initialize the knowledge base with game information."""
        self.kb.initialize(num_players, all_cards, self.player_id)

        # Add knowledge about own cards
        for card in self.hand:
            self.kb.add_card_knowledge(self.player_id, card, True)

    def make_move(self, game_state: GameState) -> Action:
        """Make a move based on knowledge base reasoning."""
        # If we can solve the case with high confidence, make an accusation
        if self.kb.can_solve_case(confidence_threshold=0.95):
            solution = self.kb.get_solution()
            return Action(
                action_type=ActionType.ACCUSATION,
                suspect=solution[0],
                weapon=solution[1],
                room=solution[2]
            )

        # Otherwise, make a suggestion that will gain the most information
        best_suggestion = self.kb.get_best_suggestion()
        return Action(
            action_type=ActionType.SUGGESTION,
            suspect=best_suggestion[0],
            weapon=best_suggestion[1],
            room=best_suggestion[2]
        )

    def respond_to_suggestion(self, suggestion: Action, game_state: GameState) -> Optional[Card]:
        """Respond to a suggestion based on cards in hand."""
        matching_cards = [
            card for card in self.hand
            if card == suggestion.suspect or card == suggestion.weapon or card == suggestion.room
        ]

        if not matching_cards:
            return None

        # Let the KB decide which card to show (strategic decision)
        return self.kb.choose_card_to_show(matching_cards, suggestion, game_state)

    def observe_result(self, result: ActionResult, game_state: GameState):
        """Update knowledge base with the result of an action."""
        if result.player_id == self.player_id:
            # Process result of our own action
            if result.refuted:
                # Someone showed us a card
                self.kb.add_card_knowledge(result.refuter_id, result.card_shown, True)

                # Update disjunctive knowledge for other cards in the suggestion
                cards_in_suggestion = [result.action.suspect, result.action.weapon, result.action.room]
                cards_in_suggestion.remove(result.card_shown)
                for card in cards_in_suggestion:
                    # We now know these cards are not in the solution
                    self.kb.update_solution_knowledge(card, False)
            else:
                # No one could refute our suggestion
                # This gives us disjunctive knowledge: one or more cards might be in the solution
                self.kb.add_disjunctive_knowledge(
                    [result.action.suspect, result.action.weapon, result.action.room]
                )
        else:
            # Process another player's action
            if result.refuted:
                if result.refuter_id == self.player_id:
                    # We refuted the suggestion
                    pass  # We already know we have the card
                else:
                    # Someone else refuted
                    if result.card_shown is not None:
                        # We saw the card (happens in some variants)
                        self.kb.add_card_knowledge(result.refuter_id, result.card_shown, True)
                    else:
                        # We don't know which card was shown
                        # Add disjunctive knowledge: refuter has at least one of these cards
                        self.kb.add_disjunctive_player_knowledge(
                            result.refuter_id,
                            [result.action.suspect, result.action.weapon, result.action.room]
                        )

                    # Check for bluffing
                    self.kb.check_for_bluffing(result.player_id, result.action, game_state)
            else:
                # No one could refute
                # Add negative knowledge for all players who had a chance to refute
                player_count = len(game_state.player_hands)
                current_player = result.player_id
                for i in range(1, player_count):
                    player_to_check = (current_player + i) % player_count
                    if player_to_check != self.player_id:  # We already know about our own cards
                        for card in [result.action.suspect, result.action.weapon, result.action.room]:
                            self.kb.add_negative_knowledge(player_to_check, card)

    def game_over(self, winner: Optional[int], solution: Tuple[Card, Card, Card]):
        """Process game outcome."""
        # Could be used for learning across multiple games
        pass
