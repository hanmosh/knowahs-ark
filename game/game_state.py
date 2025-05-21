from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Tuple
from game.card import Card, CardType

@dataclass
class GameState:
    """Class for tracking the state of a Clue game."""

    # The solution (murder cards)
    solution: Tuple[Card, Card, Card] = None

    # Maps player ID to their hand of cards
    player_hands: Dict[int, List[Card]] = field(default_factory=dict)

    # Maps player ID to whether they can still make accusations
    can_accuse: Dict[int, bool] = field(default_factory=dict)

    # Current player's turn
    current_player: int = 0

    # Game history: (player_id, accusation, who_refuted, card_shown)
    # If no one refuted, who_refuted and card_shown are None
    history: List[Tuple[int, Tuple[Card, Card, Card], Optional[int], Optional[Card]]] = field(default_factory=list)

    # Game is over flag
    game_over: bool = False

    # Winner, if any
    winner: Optional[int] = None

    def initialize_game(self, num_players, solution, player_hands):
        """Initialize the game state."""
        self.solution = solution
        self.player_hands = player_hands
        self.can_accuse = {i: True for i in range(num_players)}
        self.current_player = 0
        self.history = []
        self.game_over = False
        self.winner = None

    def add_accusation(self, player_id, accusation, who_refuted=None, card_shown=None):
        """Add an accusation to the game history."""
        self.history.append((player_id, accusation, who_refuted, card_shown))

    def make_final_accusation(self, player_id, accusation):
        """Make a final accusation and determine if the player wins."""
        if not self.can_accuse[player_id]:
            raise ValueError(f"Player {player_id} cannot make accusations anymore")

        is_correct = (
            accusation[0] == self.solution[0] and
            accusation[1] == self.solution[1] and
            accusation[2] == self.solution[2]
        )

        if is_correct:
            self.game_over = True
            self.winner = player_id
        else:
            self.can_accuse[player_id] = False

        self.add_accusation(player_id, accusation)
        return is_correct

    def next_turn(self):
        """Advance to the next player who can still play."""
        if self.game_over:
            return

        self.current_player = (self.current_player + 1) % len(self.player_hands)

        # Skip players who can't accuse and have been eliminated
        while not self.can_accuse[self.current_player] and any(self.can_accuse.values()):
            self.current_player = (self.current_player + 1) % len(self.player_hands)

        # Check if game is over because no one can accuse
        if not any(self.can_accuse.values()):
            self.game_over = True
