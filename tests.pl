% Basic tests for game initialization and core mechanics
test_initialization :-
    initialize_game,
    solution(Murderer, Weapon, Room),
    suspect(Murderer),
    weapon(Weapon),
    room(Room).

test_player_locations :-
    initialize_game,
    player_location(player1, hall),
    player_location(player2, study),
    player_location(player3, lounge).

test_player_cards :-
    initialize_game,
    holds(player1, _),
    holds(player2, _),
    holds(player3, _).

test_turns :-
    initialize_game,
    current_turn(player1),
    next_turn,
    current_turn(player2),
    next_turn,
    current_turn(player3).

test_player_knowledge :-
    initialize_game,
    holds(player1, Card),
    player_knows_not_solution(player1, Card).

test_disprove :-
    initialize_game,
    can_disprove(player1, colonel_mustard, rope, kitchen).

test_accusation_correct :-
    initialize_game,
    solution(Murderer, Weapon, Room),
    make_accusation(player1, Murderer, Weapon, Room).

test_accusation_incorrect :-
    initialize_game,
    solution(Murderer, Weapon, Room),
    Murderer \= mrs_white,
    make_accusation(player1, mrs_white, Weapon, Room),
    eliminated(player1).

% Run all tests
run_tests :-
    test_initialization,
    test_player_locations,
    test_player_cards,
    test_turns,
    test_player_knowledge,
    test_disprove,
    test_accusation_correct,
    test_accusation_incorrect.