:- module day22.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool, char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    Ex1 = "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
",
    Example1 = parse_input(Ex1),
    S1 = part1(Example1),
    A1: int = score(S1),
    E1 = 306,
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    util.read_file_as_string("../input/day22.txt", InputString, !IO),
    GameStarts = parse_input(InputString),
    GameEnds = part1(GameStarts),
    P1 = score(GameEnds),
    io.format("P1: got %d\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(game) = game.
part1(Game) = play(Game).

:- func play(game) = game.
play(Game) = Result :-
    Next = play_round(Game),
    (if
        Game = Next
     then
        Result = Game
     else
         Result = play(Next)
    ).

:- func play_round(game) = game.
play_round(D1 - D2) = G :-
    (if
        [C1 | R1] = D1,
        [C2 | R2] = D2
     then
        (if
            C1 > C2
         then
            NextD1 = append(R1, [C1, C2]),
            NextD2 = R2,
            G = NextD1 - NextD2
         else
            NextD1 = R1,
            NextD2 = append(R2, [C2, C1]),
             G = NextD1 - NextD2
        )
     else
        % Game is already over!
         G = D1 - D2
    ).

% Head is top, tail is bottom.
:- type deck == list(int).
:- type game == pair(deck, deck).
:- func parse_input(string) = game.
parse_input(Input) = Game :-
    Sections = split_at_string("\n\n", strip(Input)),
    (if
        [S1, S2] = map(split_at_string("\n"), Sections),
        P1 = tail(S1),
        P2 = tail(S2)
    then
        Deck1 = map(det_to_int, P1),
        Deck2 = map(det_to_int, P2),
        Game = Deck1 - Deck2
    else
        unexpected($module, $pred, "Failed to split input into sections")
    ).

:- func score(game) = int.
score(D1 - D2) = Score :-
    (if
        D1 = []
    then
        Score = score_deck(D2)
    else
        expect(unify(D2, []), $module, $pred, "Neither deck is empty. Cannot score incomplete game."),
        Score = score_deck(D1)
    ).

:- func score_deck(deck) = int.
score_deck(Cards) = Score :-
    length(Cards, N),
    Multipliers = reverse(1 .. N),
    foldl2((pred(Card::in, Ms::in, NextMs::out, A0::in, A::out) is det :-
        M = det_head(Ms),
        A = A0 + M * Card,
        NextMs = det_tail(Ms)), Cards, Multipliers, _, 0, Score).
