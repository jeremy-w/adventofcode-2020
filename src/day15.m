:- module day15.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = [1,3,2],
    E1 = 1,
    A1 = part1(Example),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    Input = [2,0,1,7,4,14,18],
    P1 = part1(Input),
    io.format("P1: got %d (expected 496)\n", [i(P1)], !IO),

    % io.format("P2 test: expected %d, got %d\n", [i(175594), i(part2([0,3,6]))], !IO),
    P2 = part2(Input),
    io.format("P2: got %d\n", [i(P2)], !IO),

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
    G: game = foldl(game_step, 1 .. 2020, game_init(Input)),
    Spoken2020 = det_head(G^spoken).

:- func part2(list(int)) = int.
part2(Input) = Spoken30000000 :-
    % 30 millionth, sure, sounds great.
    % Probably should use a map from number => turn at that point, together with a last-turn value.
    G: game = foldl(game_step, 1 .. 30000000, game_init(Input)),
    Spoken30000000 = det_head(G^spoken).

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
    (if G1^turn mod 10000 = 0 then trace [io(!IO)] (io.write_line({"finished turn:", G1^turn}, !IO)) else true),
    trace [io(!IO), runtime(env("TRACE"))] (io.write_line({"G", GNext}, !IO)).
