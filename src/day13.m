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

/*
I did part 1 on my phone with Pythonista because the input was teensy.

t=1008141
s='17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,23,x,x,x,x,x,x,x,787,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29'
y = s.split(',')
ys = [int(n) for n in y if n != 'x']
zs = [(n * math.ceil(t / n) - t, n) for n in ys]
sorted(zs)
6*787
# 4722

*/
