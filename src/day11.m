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
    Expected1 = 37,
    Actual1 = part1(parse_seat_map(example1)),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    % io.format("P1 test b: expected 22 1-jolt, 10 3-jolt, for %d, got %d\n", [i(22*10), i(part1(ints(example1b)))], !IO),

    % util.read_file_as_string("../input/day11.txt", Input, !IO),
    % io.format("P1: got %d\n", [i(part1(ints(Input)))], !IO),

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
part1(seat_map(_NRows, _NCols, Rows)) = OccupiedSeatCount :-
    % repeat(step, seat_map, StableSeatMap),
    RowLists = map(to_char_list, Rows),
    RowCounts = map((func(Seats) = Count :-
        OccupiedOnly = filter(unify(occupied), Seats),
        Count = length(OccupiedOnly)), RowLists),
    OccupiedSeatCount = foldl(plus, RowCounts, 0).

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

:- func parse_seat_map(string) = seat_map.
parse_seat_map(Input) = seat_map(NRows, NCols, Rows) :-
    Rows = words(strip(Input)),
    NRows = length(Rows),
    NCols = length(det_head(Rows)).
