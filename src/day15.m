:- module day15.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = [0, 3, 6],
    E1 = 1,
    A1 = part1(Example),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    % Input = [2,0,1,7,4,14,18],
    % P1 = part1(Input),
    % io.format("P1: got %d\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- type game ---> game(
    input :: list(int),
    spoken :: list(int),
    turn :: int
).
:- func game_init(list(int)) = game.
game_init(Input) = game(Input, [], 0).

:- func part1(list(int)) = int.
part1(Input) = Spoken2020 :-
    G: game = foldl(game_step, 1 .. 10, game_init(Input)),
    Spoken2020 = det_head(G^spoken).

:- func game_step(int, game) = game.
game_step(_, G) = GNext :-
    G1 = G^turn := G^turn + 1,
    ( if
        index1(G1^input, G1^turn, N)
    then
        % Start: Run through the input.
        GNext = G1^spoken := [N | G1^spoken]
    else % Input exhausted, now look at prev item.
        Prev = det_head(G1^spoken),
        (if
            index1_of_first_occurrence(tail(G1^spoken), Prev, Index)
         then
            GNext = G1^spoken := [Index | G1^spoken]
         else
             GNext = G1^spoken := [0 | G1^spoken]
        )
    ),
    trace [io(!IO)] (io.write_line({"G", GNext}, !IO)).
