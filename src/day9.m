:- module day9.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    % Test: Part 1
    Expected1 = 23,
    Actual1 = part1(example1),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    util.read_file_as_string("../input/day8.txt", Input, !IO),
    Input = _.

:- func example1 = string.
example1 = "TODO".

:- func part1(string) = int.
part1(_) = 42.
