import os
from typing import List, Tuple, Dict, Set, Optional
import subprocess
from knowledge.kb_interface import KnowledgeBase
from game.card import Card, CardType
from game.action import Action
from game.game_state import GameState

class PrologKnowledgeBase(KnowledgeBase):
    """A knowledge base implementation that uses Prolog for reasoning."""

    def __init__(self):
        """Initialize the Prolog knowledge base."""
        self.prolog_file = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'clueKB.pl')
        self.num_players = 0
        self.player_id = 0
        self.all_cards = []
        self._initialize_prolog()

    def _initialize_prolog(self):
        """Initialize the Prolog environment."""
        # Start Prolog process
        self.prolog_process = subprocess.Popen(
            ['swipl', '-s', self.prolog_file, '-g', 'initialize_game'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

    def _query_prolog(self, query: str) -> str:
        """Execute a Prolog query and return the result."""
        try:
            self.prolog_process.stdin.write(f"{query}.\n")
            self.prolog_process.stdin.flush()
            result = self.prolog_process.stdout.readline().strip()
            return result
        except Exception as e:
            print(f"Error executing Prolog query: {e}")
            return ""

    def initialize(self, num_players: int, all_cards: List[Card], player_id: int):
        """Initialize the knowledge base with game information."""
        self.num_players = num_players
        self.player_id = player_id
        self.all_cards = all_cards

        # Initialize Prolog game state
        self._query_prolog("initialize_game")

        # Add knowledge about own cards
        for card in all_cards:
            if card.owner_id == player_id:
                self.add_card_knowledge(player_id, card, True)

    def add_card_knowledge(self, player_id: int, card: Card, has_card: bool):
        """Add knowledge about a player having or not having a card."""
        if has_card:
            self._query_prolog(f"assertz(holds(player{player_id}, {card.name.lower().replace(' ', '_')}))")
        else:
            self._query_prolog(f"assertz(eliminated(player{player_id}, {card.name.lower().replace(' ', '_')}))")

    def add_negative_knowledge(self, player_id: int, card: Card):
        """Add knowledge that a player does not have a card."""
        self.add_card_knowledge(player_id, card, False)

    def add_disjunctive_knowledge(self, cards: List[Card]):
        """Add disjunctive knowledge: at least one of these cards is in the solution."""
        card_names = [card.name.lower().replace(' ', '_') for card in cards]
        self._query_prolog(f"assertz(possible_solution_cards({card_names}))")

    def add_disjunctive_player_knowledge(self, player_id: int, cards: List[Card]):
        """Add disjunctive knowledge: player has at least one of these cards."""
        card_names = [card.name.lower().replace(' ', '_') for card in cards]
        self._query_prolog(f"assertz(possible_player_cards(player{player_id}, {card_names}))")

    def update_solution_knowledge(self, card: Card, in_solution: bool):
        """Update knowledge about whether a card is in the solution."""
        if in_solution:
            self._query_prolog(f"assertz(solution_card({card.name.lower().replace(' ', '_')}))")
        else:
            self._query_prolog(f"assertz(eliminated_solution_card({card.name.lower().replace(' ', '_')}))")

    def can_solve_case(self, confidence_threshold: float = 0.95) -> bool:
        """Determine if the case can be solved with sufficient confidence."""
        result = self._query_prolog("can_solve_case")
        return result == "true"

    def get_solution(self) -> Tuple[Card, Card, Card]:
        """Get the solution (murder cards) based on current knowledge."""
        result = self._query_prolog("solution(Suspect, Weapon, Room)")
        if result == "true":
            # Parse the result and convert to Card objects
            suspect = next(card for card in self.all_cards if card.name.lower().replace(' ', '_') == "suspect" and card.card_type == CardType.SUSPECT)
            weapon = next(card for card in self.all_cards if card.name.lower().replace(' ', '_') == "weapon" and card.card_type == CardType.WEAPON)
            room = next(card for card in self.all_cards if card.name.lower().replace(' ', '_') == "room" and card.card_type == CardType.ROOM)
            return (suspect, weapon, room)
        return None

    def get_best_suggestion(self) -> Tuple[Card, Card, Card]:
        """Get the best suggestion to make based on current knowledge."""
        # Get all cards of each type that the agent doesn't have
        suspects = [card for card in self.all_cards 
                   if card.card_type == CardType.SUSPECT 
                   and card.owner_id != self.player_id]
        weapons = [card for card in self.all_cards 
                  if card.card_type == CardType.WEAPON 
                  and card.owner_id != self.player_id]
        rooms = [card for card in self.all_cards 
                if card.card_type == CardType.ROOM 
                and card.owner_id != self.player_id]
        
        if suspects and weapons and rooms:
            return (suspects[0], weapons[0], rooms[0])
        return None

    def choose_card_to_show(self, cards: List[Card], suggestion: Action, game_state: GameState) -> Card:
        """Choose which card to show to refute a suggestion."""
        # Prefer showing cards that are less likely to be in the solution
        # For now, just return the first card that matches the suggestion
        for card in cards:
            if card.name == suggestion.suspect.name:
                return card
            if card.name == suggestion.weapon.name:
                return card
            if card.name == suggestion.room.name:
                return card
        return cards[0]  # Fallback to first card if no match found

    def check_for_bluffing(self, player_id: int, action: Action, game_state: GameState):
        """Check if a player might be bluffing in their suggestion."""
        self._query_prolog(f"check_for_bluffing(player{player_id}, {action.suspect.name.lower().replace(' ', '_')}, {action.weapon.name.lower().replace(' ', '_')}, {action.room.name.lower().replace(' ', '_')})")

    def export_to_neo4j(self, uri: str, user: str, password: str):
        """Export the knowledge graph to Neo4j."""
        # This could be implemented by converting Prolog facts to Neo4j format
        pass

    def __del__(self):
        """Clean up the Prolog process when the object is destroyed."""
        if hasattr(self, 'prolog_process'):
            self.prolog_process.terminate() 