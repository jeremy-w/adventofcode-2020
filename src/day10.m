:- module day10.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    % Test: Part 1
    Expected1 = 7*5,
    Actual1 = part1(example1),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    io.format("P1 test b: expected 22 1-jolt, 10 3-jolt, for %d, got %d\n", [i(22*10), i(part1(example1b))], !IO),

    util.read_file_as_string("../input/day10.txt", Input, !IO),
    Input = _.

:- func example1 = string.
example1 = "16
10
15
5
1
11
7
19
6
12
4
".

:- func example1b = string.
example1b = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
".

:- func part1(string) = int.
part1(_) = 42.
