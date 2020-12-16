:- module day14.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module uint.
:- import_module util.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
",
    E1 = 165u,
    A1 = part1(Example),
    io.format("P1 test: expected %u, got %u\n", [u(E1), u(A1)], !IO),

    % util.read_file_as_string("../input/day14.txt", Input, !IO),
    % P1 = part1(Input),
    % io.format("P1: got %d\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(string) = uint.
part1(Input) = SumOfMemValues :-
    SumOfMemValues = 0u.


:- type addr == uint.

    % Values and memory addresses are both 36-bit unsigned integers.
:- type machine
    ---> machine(
                % addr => value
            mem :: map(addr, uint),
                % for anding
            mask_set :: uint,
                % for and-notting
            mask_clear :: uint
        ).
