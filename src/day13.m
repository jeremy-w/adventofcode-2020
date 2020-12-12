:- module day13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = "",
    E1 = 35,
    A1 = part1(Example),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    % util.read_file_as_string("../input/day13.txt", Input, !IO),
    % P1 = part1(Input),
    % io.format("P1: got %d\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(string) = int.
part1(_) = 10.
