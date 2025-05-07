% clue_kb.pl - Knowledge base for Clue/Cluedo game

% Define the categories
category(suspect).
category(weapon).
category(room).

% Define all cards in the game
card(colonel_mustard, suspect).
card(professor_plum, suspect).
card(reverend_green, suspect).
card(mrs_peacock, suspect).
card(miss_scarlet, suspect).
card(mrs_white, suspect).

card(candlestick, weapon).
card(knife, weapon).
card(lead_pipe, weapon).
card(revolver, weapon).
card(rope, weapon).
card(wrench, weapon).

card(kitchen, room).
card(ballroom, room).
card(conservatory, room).
card(dining_room, room).
card(billiard_room, room).
card(library, room).
card(lounge, room).
card(hall, room).
card(study, room).

% Define all players
player(player1).
player(player2).
player(player3).
player(ai_bot).

% Core predicates for representing knowledge
% has_card(Player, Card) - Player definitely has this card
% eliminated_card(Player, Card) - Player definitely doesn't have this card
% possible_card(Player, Card) - Player might have this card
% solution_card(Card) - Card is part of the solution

% Define a helper to get all cards of a specific category
all_cards_of_category(Category, Cards) :-
    findall(Card, card(Card, Category), Cards).

% Check if a card might be in the solution
possible_solution(Card) :-
    card(Card, _),
    \+ has_card(_, Card).

% Define rules for deduction

% If a player has made a suggestion and no one can disprove it,
% then those cards are possible solutions
suggestion_not_disproven(Suggester, Suspect, Weapon, Room) :-
    assertz(possible_solution(Suspect)),
    assertz(possible_solution(Weapon)),
    assertz(possible_solution(Room)).

% If a player has a card, they cannot have eliminated that card
:- has_card(Player, Card), eliminated_card(Player, Card), 
   throw(error(inconsistent_knowledge(Player, Card), _)).

% If a player has a card, that card is not part of the solution
:- has_card(_, Card), solution_card(Card), 
   throw(error(inconsistent_knowledge(solution, Card), _)).

% Update possible cards based on definite knowledge
update_possible_cards :-
    retractall(possible_card(_, _)),
    findall(Player-Card, 
            (player(Player), card(Card, _), 
             \+ has_card(Player, Card), 
             \+ eliminated_card(Player, Card)), 
            Pairs),
    maplist(assert_possible, Pairs).

assert_possible(Player-Card) :-
    assertz(possible_card(Player, Card)).

% Player makes a suggestion
make_suggestion(Suggester, Suspect, Weapon, Room, Responder, Shown) :-
    % If Responder is not false, they showed a card
    (Responder \= false ->
        % The shown card is in Responder's hand
        assertz(has_card(Responder, Shown)),
        % The other two cards are eliminated for this player
        (Suspect \= Shown -> assertz(eliminated_card(Responder, Suspect)); true),
        (Weapon \= Shown -> assertz(eliminated_card(Responder, Weapon)); true),
        (Room \= Shown -> assertz(eliminated_card(Responder, Room)); true)
    ;
        % No one could disprove, so all cards are potential solutions
        assertz(possible_solution(Suspect)),
        assertz(possible_solution(Weapon)),
        assertz(possible_solution(Room))
    ),
    % Update possible cards
    update_possible_cards.
