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
# [(6, 787), (8, 41), (9, 13), (10, 17), (15, 29), (18, 19), (18, 23), (35, 37), (203, 523)]
6*787
# 4722

part 2:

What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?

find the earliest time T such that each bus departs at a time N after T such that N equals its 0-based index in the list. so 17 at T, 41 at T+7.

So we want that same zs list, but we require it to match a specific value of their index:

indexed = [(x[1], int(x[0])) for x in zip(y, range(len(y))) if x[0] != 'x']
# [(0, 17), (7, 41), (17, 523), (35, 13), (36, 19), (40, 23), (48, 787), (54, 37), (77, 29)]

ah, but it's not quite the list from part 1, because we are not necessarily looking for the FIRST departure after T, just that A departure matches index = departure - T.

Yeah, this seems easier with Prolog. Letms just format as a term for now.

IndexAndIds = [{0, 17}, {7, 41}, {17, 523}, {35, 13}, {36, 19}, {40, 23}, {48, 787}, {54, 37}, {77, 29}].

Hmm we need only check T that are multiples of 17. And the ones whose index are less than their ID are also conveniently just modulo operations as constraints. It's cases like {35, 13} that get messy. The large IDs should let us jump a lot in the search - 523 and 787.

These are also primes, as are many of the others. All of them, even! That is certainly interesting.

*/
