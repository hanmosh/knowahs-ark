% sample_game_state.pl - Sample game state for testing

% Reset knowledge base
:- dynamic has_card/2, eliminated_card/2, possible_card/2, solution_card/1, possible_solution/1.
:- retractall(has_card(_, _)).
:- retractall(eliminated_card(_, _)).
:- retractall(possible_card(_, _)).
:- retractall(solution_card(_)).
:- retractall(possible_solution(_)).

% Define our own cards
:- assertz(has_card(ai_bot, colonel_mustard)).
:- assertz(has_card(ai_bot, reverend_green)).
:- assertz(has_card(ai_bot, rope)).
:- assertz(has_card(ai_bot, lead_pipe)).
:- assertz(has_card(ai_bot, kitchen)).
:- assertz(has_card(ai_bot, ballroom)).

% Information about player1's cards from shown cards
:- assertz(has_card(player1, professor_plum)).
:- assertz(has_card(player1, knife)).
:- assertz(has_card(player1, dining_room)).

% Information about player2's cards from shown cards
:- assertz(has_card(player2, mrs_peacock)).
:- assertz(has_card(player2, wrench)).

% Information about player3's cards from shown cards
:- assertz(has_card(player3, candlestick)).
:- assertz(has_card(player3, billiard_room)).
:- assertz(has_card(player3, library)).

% Information about eliminated cards from suggestions
:- assertz(eliminated_card(player1, miss_scarlet)).
:- assertz(eliminated_card(player1, revolver)).
:- assertz(eliminated_card(player1, conservatory)).

:- assertz(eliminated_card(player2, mrs_white)).
:- assertz(eliminated_card(player2, lounge)).
:- assertz(eliminated_card(player2, hall)).

:- assertz(eliminated_card(player3, study)).

% Deduce possible solutions
:- assertz(possible_solution(miss_scarlet)).
:- assertz(possible_solution(mrs_white)).
:- assertz(possible_solution(revolver)).
:- assertz(possible_solution(lounge)).
:- assertz(possible_solution(hall)).
:- assertz(possible_solution(conservatory)).
:- assertz(possible_solution(study)).

% Initialize possible cards
:- update_possible_cards.
