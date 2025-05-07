% Core game entities
suspect(colonel_mustard).
suspect(professor_plum).
suspect(reverend_green).
suspect(mrs_peacock).
suspect(miss_scarlet).
suspect(mrs_white).

weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(lead_pipe).
weapon(wrench).

room(hall).
room(lounge).
room(dining_room).
room(kitchen).
room(ballroom).
room(conservatory).
room(billiard_room).
room(library).
room(study).

player(player1).
player(player2).
player(player3).

% Dynamic predicates for game state
:- dynamic solution/3.
:- dynamic holds/2.
:- dynamic shown/3.
:- dynamic player_location/2.
:- dynamic current_turn/1.
:- dynamic eliminated/1.
:- dynamic winner/1.


% Game setup
create_case_file(Murderer, Murder_Weapon, Murder_Room) :-
    suspect(Murderer),
    weapon(Murder_Weapon),
    room(Murder_Room),
    retractall(solution(_, _, _)),
    assertz(solution(Murderer, Murder_Weapon, Murder_Room)).

initialize_locations :-
    retractall(player_location(_, _)),
    assertz(player_location(player1, hall)),
    assertz(player_location(player2, study)),
    assertz(player_location(player3, lounge)).

% Card handling
all_cards(Cards) :-
    findall(X, suspect(X), Suspects),
    findall(X, weapon(X), Weapons),
    findall(X, room(X), Rooms),
    append(Suspects, Weapons, Temp),
    append(Temp, Rooms, Cards).

card_type(X, suspect) :- suspect(X).
card_type(X, weapon) :- weapon(X).
card_type(X, room) :- room(X).

% Player knowledge
player_knows_not_solution(Player, Card) :-
    holds(Player, Card);
    shown(_, Player, Card).

possible_solution_card(Player, Card) :-
    card_type(Card, _),
    \+ player_knows_not_solution(Player, Card).

% Movement
move_player(Player, Room) :-
    room(Room),
    retractall(player_location(Player, _)),
    assertz(player_location(Player, Room)).

% Turn management
start_game(FirstPlayer) :-
    player(FirstPlayer),
    retractall(current_turn(_)),
    assertz(current_turn(FirstPlayer)).

next_turn :-
    current_turn(CurrentPlayer),
    next_player(CurrentPlayer, NextPlayer),
    retractall(current_turn(_)),
    assertz(current_turn(NextPlayer)).

next_player(CurrentPlayer, NextPlayer) :-
    player(CurrentPlayer),
    player(NextPlayer),
    NextPlayer @> CurrentPlayer,
    \+ eliminated(NextPlayer),
    \+ (player(P), P @> CurrentPlayer, P @< NextPlayer, \+ eliminated(P)).
next_player(CurrentPlayer, NextPlayer) :-
    player(CurrentPlayer),
    player(NextPlayer),
    \+ eliminated(NextPlayer),
    \+ (player(P), P @> CurrentPlayer, \+ eliminated(P)),
    \+ (player(P), P @< NextPlayer, \+ eliminated(P)).

% Core game actions
make_suggestion(Player, Suspect, Weapon, Room) :-
    current_turn(Player),
    player_location(Player, Room),
    suspect(Suspect),
    weapon(Weapon),
    room(Room).

can_disprove(Player, Suspect, Weapon, Room) :-
    (holds(Player, Suspect);
     holds(Player, Weapon);
     holds(Player, Room)).

show_card(FromPlayer, ToPlayer, Card) :-
    holds(FromPlayer, Card),
    assertz(shown(FromPlayer, ToPlayer, Card)).

make_accusation(Player, Suspect, Weapon, Room) :-
    current_turn(Player),
    suspect(Suspect),
    weapon(Weapon),
    room(Room),
    (solution(Suspect, Weapon, Room) -> 
        true  % Correct accusation
    ; 
        eliminate_player(Player)  % Incorrect accusation
    ).
    
eliminate_player(Player) :-
    assertz(eliminated(Player)),
    next_turn.

% Simple game initialization
initialize_game :-
    % Clear any previous game state
    retractall(solution(_, _, _)),
    retractall(holds(_, _)),
    retractall(shown(_, _, _)),
    retractall(player_location(_, _)),
    retractall(current_turn(_)),
    retractall(eliminated(_)),
    
    % Set up a simple solution
    create_case_file(professor_plum, rope, conservatory),
    
    % Assign some cards to players
    assertz(holds(player1, colonel_mustard)),
    assertz(holds(player1, knife)),
    assertz(holds(player1, hall)),
    
    assertz(holds(player2, mrs_peacock)),
    assertz(holds(player2, candlestick)),
    assertz(holds(player2, ballroom)),
    
    assertz(holds(player3, miss_scarlet)),
    assertz(holds(player3, wrench)),
    assertz(holds(player3, kitchen)),
    
    % Initialize player locations
    initialize_locations,
    
    % Start with player1
    start_game(player1).