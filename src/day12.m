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

:- type sea ---> sea(easty :: int, southy :: int, dir :: bearing, ship_easty::int, ship_southy::int).
:- func init_sea = sea.
init_sea = sea(10, -1, east, 0, 0).

:- func manhattan(sea) = int.
manhattan(S) = abs(S^ship_easty) + abs(S^ship_southy).

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = "F10
N3
F7
R90
F11
",
    E1 = 286,
    A1 = part2(det_cmds_from_string(Example)),
    io.format("P2 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    util.read_file_as_string("../input/day12.txt", Input, !IO),
    ( if cmds_from_string(Input, _Cmds)
    then true %io.write_line(Cmds, !IO)
    else io.print_line("Failed parsing input", !IO)
    ),
    P2 = part2(det_cmds_from_string(Input)),
    io.format("P2: got %d\n", [i(P2)], !IO),

    io.print_line("=== * ===", !IO).

:- func part2(list(cmd)) = int.
part2(Cmds) = Result :-
    Sea0 = init_sea,
    foldl((pred(Cmd::in, S0::in, S::out) is det :-
        step(Cmd, S0, S)
        %, trace [io(!IO)] (io.write_line({"step", Cmd, S0, S}, !IO))
    ), Cmds, Sea0, Sea),
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
    % forward_by_bearing(N, Sea, SeaNext). % PART1
    SeaNext = forward_to_waypoint(Sea, N).

:- func forward_to_waypoint(sea, int) = sea.
forward_to_waypoint(Sea, N) = SeaNext :-
    Sea1 = Sea^ship_easty := Sea^ship_easty + N*Sea^easty,
    SeaNext = Sea1^ship_southy := Sea^ship_southy + N*Sea1^southy.

:- pred forward_by_bearing(int::in, sea::in, sea::out) is det.
forward_by_bearing(N, Sea, SeaNext) :-
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
    TurnCount = N / 90,
    SeaNext = foldl((func(_, S0) = Result :-
        Result = waypoint_left(S0)), 1 .. TurnCount, Sea).

:- func waypoint_left(sea) = sea.
waypoint_left(Sea) = SeaNext :-
    % easty 1, southy 1 becomes easty 1, southy -1 becomes -1, -1 becomes -1, 1 becomes 1, 1 again.
    NextEasty = Sea^southy,
    NextSouthy = -1 * Sea^easty,
    SeaNext0 = Sea^easty := NextEasty,
    SeaNext = SeaNext0^southy := NextSouthy.

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
    TurnCount = N / 90,
    SeaNext = foldl((func(_, S0) = Result :-
        Result = waypoint_right(S0)), 1 .. TurnCount, Sea).

:- func waypoint_right(sea) = sea.
waypoint_right(Sea) = SeaNext :-
    % easty 1, southy 1 becomes easty -1, southy 1 becomes -1, -1 becomes 1, -1 becomes 1, 1 again.
    NextEasty = -1 * Sea^southy,
    NextSouthy = Sea^easty,
    SeaNext0 = Sea^easty := NextEasty,
    SeaNext = SeaNext0^southy := NextSouthy.

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
