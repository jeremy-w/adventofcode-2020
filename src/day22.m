:- module day22.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool, char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    Ex1 = "",
    Example1 = parse_input(Ex1),
    S1 = part1(Example1),
    A1: int = length(S1),
    E1 = 1,
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    % util.read_file_as_string("../input/day22.txt", InputString, !IO),
    io.print_line("=== * ===", !IO).

:- func part1(problem) = solution.
part1(Problem) = Solution :-
    Solution = Problem.

:- type problem == string.
:- func parse_input(string) = problem.
parse_input(Input) = Problem :-
    Problem = Input.

:- type solution == string.
