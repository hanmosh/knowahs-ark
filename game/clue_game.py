import random
from typing import Dict, List, Optional, Tuple

from .card import Card, CardType, SUSPECTS, WEAPONS, ROOMS, ALL_CARDS
from .game_state import GameState
from .action import Action, ActionType, ActionResult
from agents.base_agent import BaseAgent

class ClueGame:
    """The main Clue game simulation."""

    def __init__(self, num_players=6):
        """
        Initialize the Clue game.

        Args:
            num_players: The number of players in the game
        """
        if num_players < 2 or num_players > 6:
            raise ValueError("Number of players must be between 2 and 6")

        self.num_players = num_players
        self.game_state = GameState()
        self.agents: Dict[int, BaseAgent] = {}

    def add_agent(self, player_id: int, agent: BaseAgent):
        """
        Add an agent to the game.

        Args:
            player_id: The ID of the player this agent controls
            agent: The agent instance
        """
        if player_id < 0 or player_id >= self.num_players:
            raise ValueError(f"Invalid player ID: {player_id}")
        self.agents[player_id] = agent

    def setup_game(self):
        """Set up the game with random cards."""
        # Select murder cards
        suspect = random.choice(SUSPECTS)
        weapon = random.choice(WEAPONS)
        room = random.choice(ROOMS)
        solution = (suspect, weapon, room)

        # Create deck with remaining cards
        remaining_suspects = [c for c in SUSPECTS if c != suspect]
        remaining_weapons = [c for c in WEAPONS if c != weapon]
        remaining_rooms = [c for c in ROOMS if c != room]

        deck = remaining_suspects + remaining_weapons + remaining_rooms
        random.shuffle(deck)

        # Deal cards to players
        player_hands = {}
        cards_per_player = len(deck) // self.num_players
        remainder = len(deck) % self.num_players

        current_card = 0
        for i in range(self.num_players):
            num_cards = cards_per_player + (1 if i < remainder else 0)
            player_hands[i] = deck[current_card:current_card + num_cards]
            current_card += num_cards

        # Initialize game state
        self.game_state.initialize_game(self.num_players, solution, player_hands)

        # Set player hands in agents
        for player_id, agent in self.agents.items():
            agent.set_hand(player_hands[player_id])

            # Initialize knowledge base if agent supports it
            if hasattr(agent, 'initialize_kb'):
                agent.initialize_kb(self.num_players, ALL_CARDS)

        return self.game_state

    def play_turn(self) -> ActionResult:
        """Play a single turn of the game."""
        if self.game_state.game_over:
            raise ValueError("Game is already over")

        current_player_id = self.game_state.current_player
        agent = self.agents[current_player_id]

        # Get agent's action
        action = agent.make_move(self.game_state)

        if action.action_type == ActionType.ACCUSATION:
            # Handle final accusation
            is_correct = self.game_state.make_final_accusation(
                current_player_id,
                (action.suspect, action.weapon, action.room)
            )

            result = ActionResult(
                action=action,
                player_id=current_player_id,
                refuted=not is_correct,
                is_winning_accusation=is_correct
            )

            # Notify all agents of the result
            for agent in self.agents.values():
                agent.observe_result(result, self.game_state)

            if is_correct:
                # Game over, notify all agents
                for agent in self.agents.values():
                    agent.game_over(current_player_id, self.game_state.solution)

            if not is_correct and not any(self.game_state.can_accuse.values()):
                # All players have made incorrect accusations, game over
                for agent in self.agents.values():
                    agent.game_over(None, self.game_state.solution)

            return result

        elif action.action_type == ActionType.SUGGESTION:
            # Handle suggestion
            refuter_id = None
            card_shown = None

            # Check each player in clockwise order
            for i in range(1, self.num_players):
                player_to_check = (current_player_id + i) % self.num_players
                checking_agent = self.agents[player_to_check]

                response = checking_agent.respond_to_suggestion(action, self.game_state)
                if response is not None:
                    refuter_id = player_to_check
                    card_shown = response
                    break

            result = ActionResult(
                action=action,
                player_id=current_player_id,
                refuted=refuter_id is not None,
                refuter_id=refuter_id,
                card_shown=card_shown
            )

            # Add to game history
            self.game_state.add_accusation(
                current_player_id,
                (action.suspect, action.weapon, action.room),
                refuter_id,
                card_shown
            )

            # Notify all agents of the result
            for pid, agent in self.agents.items():
                # Only show the card to the suggesting player
                agent_result = result
                if pid != current_player_id and card_shown is not None:
                    # Other players don't see which card was shown
                    agent_result = ActionResult(
                        action=action,
                        player_id=current_player_id,
                        refuted=refuter_id is not None,
                        refuter_id=refuter_id,
                        card_shown=None
                    )
                agent.observe_result(agent_result, self.game_state)

            # Advance to next player
            self.game_state.next_turn()

            return result

    def play_game(self) -> Tuple[Optional[int], GameState]:
        """
        Play a complete game.

        Returns:
            A tuple of (winner_id, final_game_state)
        """
        self.setup_game()

        max_turns = 100  # Prevent infinite loops
        turn_count = 0

        while not self.game_state.game_over and turn_count < max_turns:
            self.play_turn()
            turn_count += 1

        return self.game_state.winner, self.game_state
