:- module day15.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = [1,3,2],
    E1 = 1,
    A1 = part1(Example),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    % Input = [2,0,1,7,4,14,18],
    % P1 = part1(Input),
    % io.format("P1: got %d\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(list(int)) = int.
part1(_) = 10.
