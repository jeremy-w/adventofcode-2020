:- module day22.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool, char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges, set.

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
    io.format("P1: got %d (expected 30780)\n", [i(P1)], !IO),

    S2 = part2(Example1),
    io.write_line({"S2", S2}, !IO),
    A2: int = score_recursive(Example1, S2),
    E2 = 291,
    io.format("P2 test: expected %d, got %d\n", [i(E2), i(A2)], !IO),

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

%-----------------------------------------------------------------------------%
% PART 2

:- func part2(game) = game.
part2(Game) = play_recursive(Game, set.init).

:- func play_recursive(game, set(game)) = game.
play_recursive(Game, Seen) = Result :-
    Next - NextSeen = play_recursive_round(Game, Seen),
    (if
        Game = Next
     then
        Result = Game
     else
         Result = play_recursive(Next, NextSeen)
    ).

:- func play_recursive_round(game, set(game)) = pair(game, set(game)).
play_recursive_round(Game, Seen) = Final - NextSeen :-
    trace [io(!IO)] (io.write_line({"Playing round", count(Seen): int, Game}, !IO)),
    (if
        member(Game, Seen)
     then
        % Loop avoidance rule kicks in: winner is player 1
        trace [io(!IO)] (io.write_line({"Recursion! Player 1 wins by fiat."}, !IO)),
        D1 - _D2 = Game,
        Final = D1 - [],
        NextSeen = Seen
     else
         % Round not previously seen in this game. Remember it.
         NextSeen = insert(Seen, Game),
         D1 - D2 = Game,
        (if
            [C1 | R1] = D1,
            [C2 | R2] = D2
        then
            (if
                take(C1, R1, SubDeck1),
                take(C2, R2, SubDeck2)
            then
                % Recursive combat!
                SubGame = SubDeck1 - SubDeck2,
                trace [io(!IO)] (io.write_line({"Playing a subgame", SubGame}, !IO)),
                W1 - W2 = play_recursive(SubGame, set.init),
                (if
                    W2 = []
                 then
                    trace [io(!IO)] (io.write_line({"Player 1 won subgame", W1}, !IO)),
                    NextD1 = append(R1, [C1, C2]),
                    NextD2 = R2,
                    Final = NextD1 - NextD2
                 else
                    trace [io(!IO)] (io.write_line({"Player 2 won subgame", W2}, !IO)),
                    NextD1 = R1,
                    NextD2 = append(R2, [C2, C1]),
                    Final = NextD1 - NextD2
                )
            else if
                C1 > C2
            then
                NextD1 = append(R1, [C1, C2]),
                NextD2 = R2,
                Final = NextD1 - NextD2
            else
                NextD1 = R1,
                NextD2 = append(R2, [C2, C1]),
                Final = NextD1 - NextD2
            )
        else
            % Game is already over!
            Final = D1 - D2
        )
    ).

% Recursive combat awards the winner the score of their initial deck.
:- func score_recursive(game, game) = int.
score_recursive(Initial, Final) = N :-
    I1 - I2 = Initial,
    F1 - _F2 = Final,
    (if
        F1 = []
     then
        N = score_deck(I2)
     else
         N = score_deck(I1)
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
