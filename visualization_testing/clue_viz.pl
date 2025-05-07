% clue_viz.pl - Visualization logic for Clue/Cluedo game

:- use_module(library(http/http_open)).
:- use_module(library(process)).

% Import the knowledge base
:- consult('clue_kb.pl').

% Generate DOT representation of the game state
generate_game_graph(DotStr) :-
    % Start the DOT string
    string_concat("digraph ClueGame {\n", 
                  "  rankdir=LR;\n  bgcolor=\"transparent\";\n  node [style=filled, fillcolor=lightblue];\n", 
                  Header),
    
    % Add nodes for players
    findall(PlayerNode, 
            (player(P), 
             format(string(PlayerNode), "  \"~w\" [shape=box, fillcolor=lightgreen];\n", [P])),
            PlayerNodes),
    atomics_to_string(PlayerNodes, PlayerNodesStr),
    
    % Add nodes for cards by category
    findall(CardNode, 
            (card(C, Cat), 
             label_color(C, LabelColor),
             format(string(CardNode), "  \"~w\" [fillcolor=~w, label=\"~w\\n(~w)\"];\n", 
                    [C, LabelColor, C, Cat])),
            CardNodes),
    atomics_to_string(CardNodes, CardNodesStr),
    
    % Add solution subgraph
    string_concat("  subgraph cluster_solution {\n",
                  "    label=\"Possible Solution\";\n    style=filled;\n    color=lightgrey;\n    node [style=filled, fillcolor=gold];\n",
                  SolutionHeader),
    findall(SolutionNode, 
            (possible_solution(C), 
             format(string(SolutionNode), "    \"solution_~w\" [label=\"~w\"];\n", [C, C])),
            SolutionNodes),
    atomics_to_string(SolutionNodes, SolutionNodesStr),
    string_concat(SolutionHeader, SolutionNodesStr, SolutionPart1),
    string_concat(SolutionPart1, "  }\n", SolutionPart),
    
    % Add edges for has_card
    findall(HasEdge, 
            (has_card(P, C), 
             format(string(HasEdge), "  \"~w\" -> \"~w\" [color=green, penwidth=2, label=\"has\"];\n", [P, C])),
            HasEdges),
    atomics_to_string(HasEdges, HasEdgesStr),
    
    % Add edges for eliminated_card
    findall(EliminatedEdge, 
            (eliminated_card(P, C), 
             format(string(EliminatedEdge), "  \"~w\" -> \"~w\" [color=red, style=dashed, label=\"eliminated\"];\n", [P, C])),
            EliminatedEdges),
    atomics_to_string(EliminatedEdges, EliminatedEdgesStr),
    
    % Add edges for possible_card
    findall(PossibleEdge, 
            (possible_card(P, C), 
             format(string(PossibleEdge), "  \"~w\" -> \"~w\" [color=blue, style=dotted, label=\"possible\"];\n", [P, C])),
            PossibleEdges),
    atomics_to_string(PossibleEdges, PossibleEdgesStr),
    
    % Add edges for solution cards
    findall(SolutionEdge, 
            (possible_solution(C), 
             format(string(SolutionEdge), "  \"solution_~w\" -> \"~w\" [style=dotted, dir=both];\n", [C, C])),
            SolutionEdges),
    atomics_to_string(SolutionEdges, SolutionEdgesStr),
    
    % Combine all parts
    atomics_to_string([Header, PlayerNodesStr, CardNodesStr, SolutionPart, 
                       HasEdgesStr, EliminatedEdgesStr, PossibleEdgesStr, 
                       SolutionEdgesStr, "}"], DotStr).

% Helper to assign colors to cards based on their category
label_color(Card, Color) :-
    card(Card, Category),
    category_color(Category, Color).

category_color(suspect, "lightsalmon").
category_color(weapon, "lightblue").
category_color(room, "lightgreen").

% Generate and save a visualization of the game state
visualize_game_state(OutputFile) :-
    generate_game_graph(DotStr),
    % Write DOT to a temporary file
    tmp_file_stream(text, TmpFile, TmpStream),
    write(TmpStream, DotStr),
    close(TmpStream),
    
    % Determine the output format
    file_name_extension(_, Ext, OutputFile),
    (Ext = "" -> Format = "png"; format_from_extension(Ext, Format)),
    
    % Run Graphviz to generate the image
    process_create(path('dot'), ['-T', Format, TmpFile, '-o', OutputFile], []),
    
    % Clean up the temporary file
    delete_file(TmpFile).

% Map file extensions to GraphViz formats
format_from_extension(png, png).
format_from_extension(svg, svg).
format_from_extension(pdf, pdf).
format_from_extension(jpg, jpg).
format_from_extension(jpeg, jpg).
format_from_extension(_, png).  % Default to PNG

% API predicate for Python to call
visualize_current_game(OutputFile) :-
    visualize_game_state(OutputFile).

% Define the HTTP handler for SWISH
:- http_handler('/visualize', handle_visualize, []).

handle_visualize(_Request) :-
    visualize_game_state('current_game.png'),
    http_reply_file('current_game.png', [], _).
