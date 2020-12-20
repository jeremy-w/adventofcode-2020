:- module day17.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.
:- import_module solutions, set.
:- import_module regex.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = ".#.
..#
###
",
    E1 = 848,
    % A1 = part2(parse_input(Example)),
    A1 = -42,
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    Reg = regex("abc"),

    util.read_file_as_string("../input/day17.txt", Input, !IO),
    P1 = part2(parse_input(Input)),
    io.format("P2: got %d (expected 1972)\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part2(input) = int.
part2(Input) = ActiveCubesAfter6Steps :-
    trace [io(!IO), runtime(env("TRACE"))] (io.print_line(render(Input), !IO)),
    AfterSixSteps = foldl((func(_, A0) = step(A0)), 1 .. 6, Input),
    filter(unify(active), values(AfterSixSteps), ActiveCubes),
    ActiveCubesAfter6Steps = length(ActiveCubes).

:- func cactive = char.
cactive = '#'.

:- func cinactive = char.
cinactive = det_from_int(0'.). %'

:- type activity ---> active; inactive.
:- type point ---> point(x::int, y::int, z::int, w::int).
:- type input == map(point, activity).

:- func parse_input(string) = input.
parse_input(String) = Input :-
    Lines = map(to_char_list, split_at_string("\n", strip(String))),
    NRows: int = length(Lines),
    NCols: int = length(det_head(Lines)),
    Activities = map((func(Line) = map((func(X) = (if X = cactive then active else inactive)), Line)), Lines),

    W = 0,
    Z = 0,

    Xs = 0 .. (NCols - 1),
    XToAs: list(assoc_list(int, activity)) = map(from_corresponding_lists(Xs), Activities),

    Ys = 0 .. (NRows - 1),
    InsertRow = (func(Y, XToA, M0) = foldl(insert_item(W, Z, Y), XToA, M0)),
    Input = foldl_corresponding(InsertRow, Ys, XToAs, map.init).

:- func insert_item(int, int, int, pair(int, activity), input) = input.
insert_item(W, Z, Y, X - A, M0) = set(M0, point(X, Y, Z, W), A).

:- func step(input) = input.
step(Prev) = Next :-
    % Extend the map with all neighbors set to inactive.
    WithFrontier = union_list(map(set.from_list, map(neighbors, keys(Prev)))),
    JustFrontier = set.delete_list(WithFrontier, keys(Prev): list(point)),
    FrontierList = to_sorted_list(JustFrontier),
    Init: input = det_insert_from_corresponding_lists(Prev, FrontierList, map(constantly(inactive), FrontierList): list(activity)),
    foldl2(update_cube, Init, Init, _, Init, Next),
    trace [io(!IO)] (io.print_line("==== STEP ====", !IO)),
    trace [io(!IO), runtime(env("TRACE"))] (io.print_line(render(Next), !IO)).

:- func neighbors(point) = list(point).
neighbors(P) = Ps :-
    solutions(neighbor(P), Ps).

:- pred neighbor(point::in, point::out) is nondet.
neighbor(point(X, Y, Z, W)@P1, point(X2, Y2, Z2, W2)@P2) :-
    R = -1 .. 1,
    member(Dx, R),
    member(Dy, R),
    member(Dz, R),
    member(Dw, R),
    X2 = X + Dx,
    Y2 = Y + Dy,
    Z2 = Z + Dz,
    W2 = W + Dw,
    not P1 = P2.

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
active_around_in(P, M) = Count :-
    Neighbors = neighbors(P),
    % trace [io(!IO)] (io.write_line({"P", P, "M", keys(M): list(point)}, !IO)),
    % apply_to_list throws if an elem is not in the list, but we instead want to return inactive.
    Activities = apply_to_list_with_default(Neighbors, M, inactive),
    filter(unify(active), Activities, Actives),
    length(Actives, Count).  % TODO: impl

:- func apply_to_list_with_default(list(K), map(K, V), V) = list(V).
apply_to_list_with_default([], _M, _Default) = [].
apply_to_list_with_default([H | T], M, Default) = (if
    V = search(M, H)
 then
    [V | apply_to_list_with_default(T, M, Default)]
 else
     [Default | apply_to_list_with_default(T, M, Default)]
).
%-----------------------------------------------------------------------------%
% RENDERING
:- func render(input) = string.
render(Input) = Output :-
    {ZMin, ZMax} = range_in_axis(((func(P) = z(P))), Input),
    {XMin, XMax} = range_in_axis(((func(P) = x(P))), Input),
    {YMin, YMax} = range_in_axis(((func(P) = y(P))), Input),
    Sections = map(render_z(Input, YMin .. YMax, XMin .. XMax), ZMin .. ZMax),
    Output = join_list("\n\n", Sections).

:- func range_in_axis(func(point) = int, input) = {int, int}.
range_in_axis(Selector, Input) = {Min, Max} :-
    Zs = sort(map(Selector, keys(Input))),
    Min = det_head(Zs),
    Max = det_last(Zs).

:- func render_z(input, list(int), list(int), int) = string.
render_z(Input, Ys, _Xs, Z) = Level :-
    Label = string.format("z=%d", [i(Z)]),
    filter((pred(P::in) is semidet :- z(P) = Z), keys(Input), Points),
    Rows = map((func(Y) = sort(filter((pred(P::in) is semidet :- y(P) = Y), Points))), Ys),
    RowsActivities = map((func(Row) = apply_to_list(Row, Input)), Rows),
    RowChars = map(map(activity_char), RowsActivities),
    RowStrings = map(from_char_list, RowChars),
    Level = join_list("\n", [Label | RowStrings]).

:- func activity_char(activity) = char.
activity_char(active) = cactive.
activity_char(inactive) = cinactive.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
