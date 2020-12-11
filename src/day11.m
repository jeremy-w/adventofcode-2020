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
    % Test: Part 1
    SeatMapE1 = parse_seat_map(example1),
    % io.write_line(step(SeatMapE1), !IO),
    Expected1 = 37,
    Actual1 = part1(SeatMapE1),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    util.read_file_as_string("../input/day11.txt", Input, !IO),
    io.format("P1: got %d\n", [i(part1(parse_seat_map(Input)))], !IO),

    % io.format("P2 test: expected %d, got %d\n", [i(8), i(part2(ints(example1)))], !IO),
    % io.format("P2 test 2: expected %d, got %d\n", [i(19208), i(part2(ints(example1b)))], !IO),
    % io.format("P2: got %d\n", [i(part2(ints(Input)))], !IO),
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

:- func afterNRoundsExpect = list(string).
afterNRoundsExpect = [
"#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
",

"#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
",

"#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##
",

"#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##
",

"#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##
"
].

:- func part1(seat_map) = int.
part1(S) = OccupiedSeatCount :-
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
        Around = around(S, R0, C0),
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
        Next = (if N >= 4 then empty else occupied)
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
