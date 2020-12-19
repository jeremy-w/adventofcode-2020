:- module day17.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = ".#.
..#
###
",
    E1 = 112,
    A1 = part1(parse_input(Example)),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    util.read_file_as_string("../input/day17.txt", Input, !IO),
    % P1 = part1(parse_input(Input)),
    % io.format("P1: got %d (expected ?)\n", [i(P1)], !IO),

    % P2 = part2(parse_input(Input)),
    % io.format("P2: got %d (expected ?)\n", [i(P2)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(input) = int.
part1(Input) = ActiveCubesAfter6Steps :-
    AfterSixSteps = foldl((func(_, A0) = step(A0)), 1 .. 6, Input),
    filter(unify(active), values(AfterSixSteps), ActiveCubes),
    ActiveCubesAfter6Steps = length(ActiveCubes).

:- func cactive = char.
cactive = '#'.

:- func cinactive = char.
cinactive = det_from_int(0'.). %'

:- type activity ---> active; inactive.
:- type point ---> point(x::int, y::int, z::int).
:- type input == map(point, activity).

:- func parse_input(string) = input.
parse_input(String) = Input :-
    Lines = map(to_char_list, split_at_string("\n", strip(String))),
    NRows: int = length(Lines),
    NCols: int = length(det_head(Lines)),
    Activities = map((func(Line) = map((func(X) = (if X = cactive then active else inactive)), Line)), Lines),

    Z = 0,

    Xs = 0 .. (NCols - 1),
    XToAs: list(assoc_list(int, activity)) = map(from_corresponding_lists(Xs), Activities),

    Ys = 0 .. (NRows - 1),
    InsertRow = (func(Y, XToA, M0) = foldl(insert_item(Z, Y), XToA, M0)),
    Input = foldl_corresponding(InsertRow, Ys, XToAs, map.init).

:- func insert_item(int, int, pair(int, activity), input) = input.
insert_item(Z, Y, X - A, M0) = set(M0, point(X, Y, Z), A).

:- func step(input) = input.
step(Prev) = Next :-
    % FIXME: this doesn't consider the frontier, only already-extant cubes. :(
    % We really need to just generate a list of coords all around (min/max then grow by -1/+1) and then walk through them all and update, I guess.
    % This "map and simulate" thing shows up a lot, and I'm not happy with how clumsy my approach to them has been.
    foldl2(update_cube, Prev, Prev, _, Prev, Next).

:- pred update_cube(point::in, activity::in, input::in, input::out, input::in, input::out) is det.
update_cube(P, active, Prev, Prev, N0, N) :-
    ActiveCount = active_around_in(P, Prev),
    (if
        (ActiveCount = 2; ActiveCount = 3)
     then
        N = N0  % still active
     else
         N = N0^elem(P) := inactive
    ).
update_cube(P, inactive, Prev, Prev, N0, N) :-
    ActiveCount = active_around_in(P, Prev),
    (if
        ActiveCount = 3
     then
        N = N0^elem(P) := active
     else
        N = N0  % still inactive
    ).

:- func active_around_in(point, input) = int.
active_around_in(P, M) = 0.  % TODO: impl
