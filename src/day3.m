:- module day3.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string, char, int, require.

:- func tree = char.
tree = '#'.

:- func snow = char.
snow = det_from_int(0'.).  %' Syntax highlighting does not handle character literals.
/* ???: If I write this using '.', then I get a compiler error:
../src/day3.m:013: Error: no clauses for function `snow'/0.
../src/day3.m:014: Syntax error at token '.': operator precedence error.
TODO: Ask in mercury-users list.
*/

:- type panel == list(list(char)).

:- type motion
    ---> motion(right :: int, down :: int).

:- func string_to_panel(string) = panel.
string_to_panel(Input) = Panel :-
    % TODO: linesplit, strip, drop any empty final line.
    Row = [],
    Rows = [Row],
    Panel = Rows.

% Uses base-1 indexing, so upper-left corner is row 1, col 1.
% Columns are infinite (we tile the panel horizontally). Access fails if row < 1 or row > length(panel).
:- pred tile_at(panel::in, int::in, int::in, char::out) is erroneous.
tile_at(Panel, Row, Col, TreeOrSnow) :-
    sorry($module, "not yet implemented").

main(!IO) :-
    part1_test(!IO),
    io.print_line("howdy", !IO).

:- func part1(string) = int is det.
part1(Input) = TreeCount :-
    % TODO: Implement.
    TreeCount = 0.

:- pred part1_test(io.io::di, io.io::uo) is det.
part1_test(!IO) :-
    Input =
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
        ".#..#...#.#\n",
    io.print_line("# BEGIN part 1 test", !IO),
    Expected = 7,
    Answer = part1(Input),
    (if Answer = Expected
    then io.print_line("ok 1", !IO)
    else io.format("not ok 1 - got %d, expected %d\n", [i(Answer), i(Expected)], !IO)
    ).
    %path_along(Input, motion(3, 1))
