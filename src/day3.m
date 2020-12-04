:- module day3.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string, char, int, require, pair.

:- func tree = char.
tree = '#'.

:- func snow = char.
snow = det_from_int(0'.).  %' Syntax highlighting does not handle character literals.
/* ???: If I write this using '.', then I get a compiler error:
../src/day3.m:013: Error: no clauses for function `snow'/0.
../src/day3.m:014: Syntax error at token '.': operator precedence error.
TODO: Ask in mercury-users list.
*/

:- func done = char.
done = 'D'.

:- type panel == list(list(char)).

:- type motion
    ---> motion(right :: int, down :: int).

:- func panel_from_string(string) = panel.
panel_from_string(Input) = Panel :-
    Strings = string.words(Input),
    Panel = list.map(string.to_char_list, Strings).

% Uses base-1 indexing, so upper-left corner is row 1, col 1.
% Columns are infinite (we tile the panel horizontally). Access fails if row < 1 or col < 1.
:- pred tile_at(panel::in, int::in, int::in, char::out) is det.
tile_at(Panel, Row, Col, TreeOrSnowOrDone) :-
    require(
        Row > 0, string.format("Row must be > 0, but given %i", [i(Row)])
    ),
    require(Col > 0, string.format("Col must be >0, bu given %d", [i(Col)])),
    (
        if Row > length(Panel)
        then TreeOrSnowOrDone = done
        else
            EffectiveCol = (Col - 1) rem (length(det_head(Panel))) + 1,
            trace [io(!IO), runtime(env("TRACE"))] (io.print("Col-EffCol: ", !IO), io.write_line(pair(Col, EffectiveCol), !IO)),
            RowCols = det_index1(Panel, Row),
            trace [io(!IO), runtime(env("TRACE"))] (io.write_line(RowCols, !IO)),
            TreeOrSnowOrDone = det_index1(RowCols, EffectiveCol)
    ).

:- func indexes_along(panel, motion) = list(pair(int, int)).
indexes_along(Panel, motion(RightBy, DownBy)) = Indexes :-
    Indexes = list.series(1 - 1, AtOrPastBottom, NextIndex),
    AtOrPastBottom = (pred((R - _)::in) is semidet :- trace [io(!IO), runtime(env("TRACE"))] (io.format("R = %d, length(Panel) = %d\n", [i(R), i(length(Panel))], !IO)), R > 0, R =< length(Panel)),
    NextIndex = (func(R - C) = Next :- trace [io(!IO), runtime(env("TRACE"))] (io.write_line(pair(R, C), !IO)), pair(R + DownBy, C + RightBy) = Next).

:- func path_along(panel, motion) = list(char).
path_along(Panel, Motion) = Path :-
    Indexes = indexes_along(Panel, Motion),
    Path = list.map((func(Pair) = Result is det :-
            tile_at(Panel, Pair^fst, Pair^snd, Result)
        ), Indexes).

main(!IO) :-
    part1_test(!IO),
    part2_test(!IO),
    io.read_file_as_string(MaybeInput, !IO),
    (if ok(Input) = MaybeInput
    then
        Panel = panel_from_string(Input),
        TreeCount = part1(Panel),
        io.format("Part 1: TreeCount = %d", [i(TreeCount)], !IO), io.nl(!IO),
        ProdCounts = part2(Panel),
        io.format("Part2: ProdCounts = %d\n", [i(ProdCounts)], !IO)
    else
        unexpected($module, $pred, "failed to read stdin")
    ).

%---%
%---%

:- func part1(panel) = int is det.
part1(Panel) = TreeCount :-
    TreeCount = treeCountOfSlope(Panel, motion(3, 1)).

:- func allSlopes = list(motion).
allSlopes = [
    motion(1, 1),
    motion(3, 1),
    motion(5, 1),
    motion(7, 1),
    motion(1, 2)
].

:- func treeCountOfSlope(panel, motion) = int.
treeCountOfSlope(Panel, Motion) = TreeCount :-
    Path = path_along(Panel, Motion),
    TreeCount = length(filter(unify(tree), Path)).

:- func part2(panel) = int.
part2(Panel) = foldl(times, map(treeCountOfSlope(Panel), allSlopes), 1).

%----%

:- func exampleInput = string.
exampleInput =
        "..##.......\n" ++
        "#...#...#..\n" ++
        ".#....#..#.\n" ++
        "..#.#...#.#\n" ++
        ".#...##..#.\n" ++
        "..#.##.....\n" ++
        ".#.#.#....#\n" ++
        ".#........#\n" ++
        "#.##...#...\n" ++
        "#...##....#\n" ++
        ".#..#...#.#\n".

:- pred part1_test(io.io::di, io.io::uo) is det.
part1_test(!IO) :-
    io.print_line("# BEGIN part 1 test", !IO),

    TinyPanel = ".#\n" ++
     "#.\n",
    TinyPanelActual = panel_from_string(TinyPanel),
    TinyPanelExpected = [[snow, tree], [tree, snow]],
    (
        if TinyPanelActual = TinyPanelExpected
        then io.print_line("ok - tiny panel from string", !IO)
        else io.print("not ok - tiny panel from string gave", !IO), io.write(TinyPanelActual, !IO), io.print(", expected ", !IO), io.write(TinyPanelExpected, !IO), io.nl(!IO)
    ),

     tile_at(TinyPanelExpected, 2, 2, TinyIndexActual),
    (
        if TinyIndexActual = tree
        then io.print_line("ok - tiny panel indexing", !IO)
        else io.format("not ok - tiny panel indexing gave %c but expected %c.\n", [c(TinyIndexActual), c(tree)], !IO)
    ),

    % OK, the basics are probably working!
    StraightDownIndexesExpected = [1-1, 2-1],
    StraightDownIndexesActual = indexes_along(TinyPanelActual, motion(0, 1)),
    (
        if StraightDownIndexesActual = StraightDownIndexesExpected
        then io.print_line("ok - straight down indexes", !IO)
        else io.print("not ok - straight down indexes gave ", !IO), io.write(StraightDownIndexesActual, !IO), io.print(", expected ", !IO), io.write_line(StraightDownIndexesExpected, !IO)
    ),

    Panel = panel_from_string(exampleInput),
    StraightDownExpected = to_char_list(".#......##."),
    StraightDownActual = path_along(Panel, motion(0, 1)),
    (
        if StraightDownActual = StraightDownExpected
        then io.print_line("ok - straight down", !IO)
        else io.format("not ok - straight down gave %s, expected %s", [s(from_char_list(StraightDownActual)), s(from_char_list(StraightDownExpected))], !IO)
    ),

    Expected = 7,
    Answer = part1(Panel),
    (if Answer = Expected
    then io.print_line("ok - part1 example", !IO)
    else io.format("not ok - part1 exmple gave %d, expected %d\n", [i(Answer), i(Expected)], !IO)
    ).

%----%

:- pred part2_test(io.state::di, io.state::uo) is det.
part2_test(!IO) :-
    io.print_line("# BEGIN part 2 tests", !IO),
    ExamplePanel = panel_from_string(exampleInput),
    ExpectedTreeCounts = [2, 7, 3, 4, 2],
    ActualTreeCounts : list(int) = map(treeCountOfSlope(ExamplePanel), allSlopes),
    (if
        ExpectedTreeCounts = ActualTreeCounts
    then
        io.print_line("ok - tree counts on all slopes", !IO)
    else
        io.print_line("not ok - tree counts on all slopes - given ", !IO), io.write(ActualTreeCounts, !IO),
        io.print(" expected ", !IO), io.write_line(ExpectedTreeCounts, !IO)
    ).
