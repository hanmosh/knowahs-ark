import os
import sys

# Add the project root directory to Python path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from game.card import Card, CardType
from game.action import Action, ActionType
from game.game_state import GameState
from agents.kb_agent import KnowledgeBasedAgent
from knowledge.prolog_kb import PrologKnowledgeBase

def main():
    # Create some test cards
    cards = [
        Card("Colonel Mustard", CardType.SUSPECT, 0),
        Card("Professor Plum", CardType.SUSPECT, 1),
        Card("Knife", CardType.WEAPON, 0),
        Card("Candlestick", CardType.WEAPON, 1),
        Card("Hall", CardType.ROOM, 0),
        Card("Lounge", CardType.ROOM, 1)
    ]

    # Create a game state
    game_state = GameState()
    game_state.initialize_game(
        num_players=2,
        solution=(cards[0], cards[2], cards[4]),  # Colonel Mustard with Knife in Hall
        player_hands={
            0: [cards[0], cards[2], cards[4]],  # Player 0's hand
            1: [cards[1], cards[3], cards[5]]   # Player 1's hand
        }
    )

    # Create the Prolog-based knowledge base agent
    agent = KnowledgeBasedAgent(player_id=0, kb_class=PrologKnowledgeBase)
    agent.hand = game_state.player_hands[0]  # Set the agent's hand
    agent.initialize_kb(num_players=2, all_cards=cards)

    # Print agent's hand for debugging
    print("\nAgent's hand:")
    for card in agent.hand:
        print(f"- {card.name} ({card.card_type})")

    # Test making a move
    action = agent.make_move(game_state)
    print(f"\nAgent's move: {action.action_type}")
    if action.action_type == ActionType.SUGGESTION:
        print(f"Suggestion: {action.suspect.name} with {action.weapon.name} in {action.room.name}")
    elif action.action_type == ActionType.ACCUSATION:
        print(f"Accusation: {action.suspect.name} with {action.weapon.name} in {action.room.name}")

    # Test responding to a suggestion
    suggestion = Action(
        action_type=ActionType.SUGGESTION,
        suspect=cards[0],  # Colonel Mustard (in agent's hand)
        weapon=cards[2],   # Knife (in agent's hand)
        room=cards[4]      # Hall (in agent's hand)
    )
    print(f"\nTesting response to suggestion: {suggestion.suspect.name} with {suggestion.weapon.name} in {suggestion.room.name}")
    response = agent.respond_to_suggestion(suggestion, game_state)
    if response:
        print(f"Agent can refute with: {response.name}")
    else:
        print("Agent cannot refute the suggestion")

if __name__ == "__main__":
    main() 