:- module day11.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

:- func floor = char.
floor = det_from_int(0'.). %'

:- func occupied = char.
occupied = '#'.

:- func empty = char.
empty = 'L'.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    % Test: Part 2
    SeatMapE1 = parse_seat_map(example1),
    RayCastEx = parse_seat_map(".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....
"),
    Around = raycast_around(RayCastEx, 4, 3),
    io.write_line({"raycast test: expected 8 occupied seats #, found:", Around}, !IO),
    % (if at(RayCastEx, 4, 3, C) then io.write_line({C, Around}, !IO) else true),
    Expected2 = 26,
    % Actual2 = part2(SeatMapE1),
    % io.format("P2 test: expected %d, got %d\n", [i(Expected2), i(Actual2)], !IO),

    % util.read_file_as_string("../input/day11.txt", Input, !IO),
    % io.format("P2: got %d\n", [i(part2(parse_seat_map(Input)))], !IO),
    io.print_line("=== * ===", !IO).

:- func example1 = string.
example1 = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
".

:- func part2(seat_map) = int.
part2(S) = OccupiedSeatCount :-
    repeat_till_stable(S, StableSeatMap),
    OccupiedSeatCount = occupied_seat_count(StableSeatMap).

:- pred repeat_till_stable(seat_map::in, seat_map::out) is det.
repeat_till_stable(S0, S) :-
    S1 = step(S0),
    (if S0 = S1
    then S = S1
    else repeat_till_stable(S1, S)).

:- func occupied_seat_count(seat_map) = int.
occupied_seat_count(S) = OccupiedSeatCount :-
    RowLists = map(to_char_list, S^rows),
    RowCounts = map((func(Seats) = Count :-
        OccupiedOnly = filter(unify(occupied), Seats),
        Count = length(OccupiedOnly)), RowLists),
    OccupiedSeatCount = foldl(plus, RowCounts, 0).

:- func step(seat_map) = seat_map.
step(S) = S1 :-
    AllRows = 0 .. (S^nrows - 1),
    S1 = foldl(step_row(S), AllRows, S).

:- func step_row(seat_map, int, seat_map) = seat_map.
step_row(S, R0, SAcc) = SNext :-
    AllCols = 0 .. (S^ncols - 1),
    SNext = foldl(step_at(S, R0), AllCols, SAcc).

:- func step_at(seat_map, int, int, seat_map) = seat_map.
step_at(S, R0, C0, SAcc) = SNext :-
    ( if at(S, R0, C0, Was)
    then
        Around = raycast_around(S, R0, C0),
        OccupiedAround = length(filter(unify(occupied), Around)),
        WillBe = next(Was, OccupiedAround),
        update(SAcc, R0, C0, WillBe, SNext)
    else
        SNext = SAcc
    ).

:- func next(char, int) = char.
next(C, N) = Next :-
    (if
        C = empty
    then
        Next = (if N = 0 then occupied else empty)
    else if C = occupied
    then
        Next = (if N >= 5 then empty else occupied)
    else
        Next = C).


% List of rows.
:- type seat_map ---> seat_map(nrows::int, ncols::int, rows::list(string)).

% Tiles around the given 0-indexed r,c location.
:- func around(seat_map, int, int) = list(char).
around(S, R0, C0) = Around :-
    filter_map((pred({RAt, CAt}::in, C::out) is semidet :-
        at(S, RAt, CAt, C)), [
            {R0 - 1, C0 - 1}, {R0 - 1, C0}, {R0 - 1, C0 + 1},
            {R0, C0 - 1}, /* SKIP */ {R0, C0 + 1},
            {R0 + 1, C0 - 1}, {R0 + 1, C0}, {R0 + 1, C0 + 1}], Around).

% Seats, occupied only, visible in all 8 directions from the location.
:- func raycast_around(seat_map, int, int) = list(char).
raycast_around(S, R0, C0) = Around :-
    % Here are all the r,c indexes we need to check for a seat, in order. The order of scanning is key!
    Left = map(((func(C) = Result :-
        Result = {R0, C})), reverse(0 .. (C0 - 1))),
    Right = map(((func(C) = Result :-
        Result = {R0, C})), (C0 + 1) .. (S^ncols - 1)),
    Up = map((func(R) = Result :-
        Result = {R, C0}), reverse(0 .. (R0 - 1))),
    Down = map((func(R) = Result :-
        Result = {R, C0}), (R0 + 1) .. (S^nrows - 1)),
    % Forgot the diagonals to start!
    Builder = diag(S, R0, C0),
    UpLeft = Builder({-1, -1}),
    UpRight = Builder({-1, +1}),
    DownLeft = Builder({+1, -1}),
    DownRight = Builder({+1, +1}),
    % trace [io(!IO)] (io.write_line({"R,C", {R0, C0}, "Left", Left, "Right", Right, "Up", Up, "Down", Down}, !IO)),
    Around = map(raycast(S), [Left, Right, Up, Down, UpLeft, UpRight, DownLeft, DownRight]).

:- func diag(seat_map, int, int, {int, int}) = list({int, int}).
diag(S, R0, C0, {DeltaR, DeltaC}) = Indexes :-
    (if
        diag_step(S, R0, C0, {DeltaR, DeltaC}, {R1, C1})
    then
        Indexes = [{R1, C1} | diag(S, R1, C1, {DeltaR, DeltaC})]
    else
        Indexes = []
    ).

:- pred diag_step(seat_map::in, int::in, int::in, {int, int}::in, {int, int}::out) is semidet.
diag_step(S, R0, C0, {DeltaR, DeltaC}, {R1, C1}) :-
    R1 = R0 + DeltaR,
    R1 >= 0,
    R1 < S^nrows,
    C1 = C0 + DeltaC,
    C1 >= 0,
    C1 < S^ncols.

% Returns first seat found, whether occupied or empty.
% Returns floor if no seats.
:- func raycast(seat_map, list({int, int})) = char.
raycast(S, Indexes) = Seat :-
    filter_map((pred({R0, C0}::in, Result::out) is semidet :-
        at(S, R0, C0, Result)), Indexes, Tiles : list(char)),
    ( if find_first_match((pred(X::in) is semidet :-
        X = occupied; X = empty), Tiles, C)
    then
        Seat = C
    else
        Seat = floor
    ).

% Fails if off map.
:- pred at(seat_map::in, int::in, int::in, char::out) is semidet.
at(seat_map(_NRows, _NCols, Rows), R0, C0, C) :-
    list.index0(Rows, R0, Cols),
    string.index(Cols, C0, C).

:- pred update(seat_map::in, int::in, int::in, char::in, seat_map::out) is det.
update(S0, R0, C0, C, S) :-
    % trace [io(!IO)] (io.write_line({"update", S0, R0, C0, C}, !IO)),
    list.det_index0(S0^rows, R0, Row),
    NewRow = det_set_char(C, C0, Row),
    NewRows = det_replace_nth(S0^rows, R0+1, NewRow),
    S = S0^rows := NewRows.

:- func parse_seat_map(string) = seat_map.
parse_seat_map(Input) = seat_map(NRows, NCols, Rows) :-
    Rows = words(strip(Input)),
    NRows = length(Rows),
    NCols = length(det_head(Rows)).
