:- module day12.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, require.
:- import_module solutions.

:- type action ---> n; s; e; w; l; r; f.
:- type cmd ---> cmd(action, int).

:- pred cmds_from_string(string::in, list(cmd)::out) is semidet.
cmds_from_string(Input, Cmds) :-
    Lines = split_at_string("\n", strip(Input)),
    map(cmd_from_string, Lines, Cmds).
:- func det_cmds_from_string(string) = list(cmd).
det_cmds_from_string(Input) = Result :-
    ( if cmds_from_string(Input, R)
    then Result = R
    else unexpected($module, $pred, "failed parsing input")).

:- pred cmd_from_string(string::in, cmd::out) is semidet.
cmd_from_string(Line, Cmd) :-
    split(Line, 1, ActionStr, NStr),
    to_int(NStr, N),
    action_from_string(ActionStr, A),
    Cmd = cmd(A, N).

:- pred action_from_string(string::in, action::out) is semidet.
action_from_string("N", n).
action_from_string("S", s).
action_from_string("E", e).
action_from_string("W", w).
action_from_string("L", l).
action_from_string("R", r).
action_from_string("F", f).

:- type bearing ---> north; south; east; west.

:- type sea ---> sea(easty :: int, southy :: int, dir :: bearing).
:- func init_sea = sea.
init_sea = sea(0, 0, east).

:- func manhattan(sea) = int.
manhattan(sea(X, Y, _)) = abs(X) + abs(Y).

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = "F10
N3
F7
R90
F11
",
    E1 = 25,
    A1 = part1(det_cmds_from_string(Example)),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    util.read_file_as_string("../input/day12.txt", Input, !IO),
    ( if cmds_from_string(Input, _Cmds)
    then true %io.write_line(Cmds, !IO)
    else io.print_line("Failed parsing input", !IO)
    ),
    P1 = part1(det_cmds_from_string(Input)),
    io.format("P1: got %d\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(list(cmd)) = int.
part1(Cmds) = Result :-
    Sea0 = init_sea,
    foldl(step, Cmds, Sea0, Sea),
    Result = manhattan(Sea).

:- pred step(cmd::in, sea::in, sea::out) is det.
step(cmd(n, N), Sea, SeaNext) :-
    SeaNext = Sea^southy := Sea^southy - N.
step(cmd(s, N), Sea, SeaNext) :-
    SeaNext = Sea^southy := Sea^southy + N.
step(cmd(e, N), Sea, SeaNext) :-
    SeaNext = Sea^easty := Sea^easty + N.
step(cmd(w, N), Sea, SeaNext) :-
    SeaNext = Sea^easty := Sea^easty - N.
step(cmd(f, N), Sea, SeaNext) :-
    Bearing = Sea^dir,
    ( if Bearing = north
    then step(cmd(n, N), Sea, SeaNext)
    else if Bearing = east
    then step(cmd(e, N), Sea, SeaNext)
    else if Bearing = south
    then step(cmd(s, N), Sea, SeaNext)
    else % Bearing = west then
        step(cmd(w, N), Sea, SeaNext)
    ).
step(cmd(l, N), Sea, SeaNext) :-
    Bearing = Sea^dir,
    TurnCount = N / 90,
    NextBearing = foldl((func(_, S0) = Result :-
        Result = turn_left(S0)), 1 .. TurnCount, Bearing),
    SeaNext = Sea^dir := NextBearing.

:- func turn_left(bearing) = bearing.
turn_left(Bearing) = NextBearing :-
    ( if Bearing = north
    then NextBearing = west
    else if Bearing = east
    then NextBearing = north
    else if Bearing = south
    then NextBearing = east
    else % Bearing = west then
        NextBearing = south
    ).

step(cmd(r, N), Sea, SeaNext) :-
    Bearing = Sea^dir,
    TurnCount = N / 90,
    Iterations = 1 .. TurnCount,
    NextBearing = foldl((func(_, S0) = Result :-
        Result = turn_right(S0)), Iterations, Bearing),
    % trace [io(!IO)] (io.write_line({"N", N, "Bearing", Bearing, "TurnCount", TurnCount, "Its", Iterations, "NextBearing", NextBearing}, !IO)),
    SeaNext = Sea^dir := NextBearing.

:- func turn_right(bearing) = bearing.
turn_right(Bearing) = NextBearing :-
    ( if Bearing = north
    then NextBearing = east
    else if Bearing = east
    then NextBearing = south
    else if Bearing = south
    then NextBearing = west
    else % Bearing = west then
        NextBearing = north
    ).
