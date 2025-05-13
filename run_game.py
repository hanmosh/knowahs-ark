from typing import List, Optional, Dict
import random

from game.clue_game import ClueGame
from agents.heuristic_agent import HeuristicAgent
from game.game_state import GameState

def run_game(num_players: int = 6) -> Optional[int]:
    """
    Run a complete Clue game simulation.

    Args:
        num_players: Number of players in the game

    Returns:
        The ID of the winning player, or None if there was no winner
    """
    game = ClueGame(num_players=num_players)

    # Add agents (all heuristic for now)
    for player_id in range(num_players):
        game.add_agent(player_id, HeuristicAgent(player_id))

    # Run the game
    winner, final_state = game.play_game()

    # Print game summary
    print(f"Game over! {'Player ' + str(winner) + ' won!' if winner is not None else 'No winner.'}")
    print(f"The solution was: {final_state.solution[0].name}, {final_state.solution[1].name}, {final_state.solution[2].name}")

    return winner

def run_multiple_games(num_games: int = 100, num_players: int = 6) -> Dict[int, int]:
    """
    Run multiple Clue games and track statistics.

    Args:
        num_games: Number of games to run
        num_players: Number of players per game

    Returns:
        Dictionary mapping player IDs to number of wins
    """
    win_counts = {i: 0 for i in range(num_players)}
    win_counts[None] = 0  # No winner

    for i in range(num_games):
        winner = run_game(num_players)
        win_counts[winner] += 1

        # Print progress
        if (i + 1) % 10 == 0:
            print(f"Completed {i + 1} games...")

    # Print final statistics
    print("\nGame Statistics:")
    for player_id, wins in win_counts.items():
        if player_id is not None:
            print(f"Player {player_id}: {wins} wins ({wins / num_games * 100:.1f}%)")
        else:
            print(f"No winner: {wins} games ({wins / num_games * 100:.1f}%)")

    return win_counts

if __name__ == "__main__":
    run_multiple_games(num_games=10)
