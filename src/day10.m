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
    Actual1 = part1(ints(example1)),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    io.format("P1 test b: expected 22 1-jolt, 10 3-jolt, for %d, got %d\n", [i(22*10), i(part1(ints(example1b)))], !IO),

    util.read_file_as_string("../input/day10.txt", Input, !IO),
    io.format("P1: got %d\n", [i(part1(ints(Input)))], !IO),

    io.format("P2 test: expected %d, got %d\n", [i(8), i(part2(ints(example1)))], !IO),
    % io.format("P2 test 2: expected %d, got %d\n", [i(19208), i(part2(ints(example1b)))], !IO),
    % io.format("P2: got %d\n", [i(part2(ints(Input)))], !IO),
    io.print_line("=== * ===", !IO).

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

:- func part1(list(int)) = int.
part1(Joltages) = ProductOf1And3Deltas :-
    InOrder = sort(Joltages),
    Pairs = chunk(zip(InOrder, det_tail(InOrder)), 2),
    Deltas = filter_map((func([H, T]) = T - H is semidet), Pairs),
    foldl2((pred(Delta::in, Ones0::in, Ones::out, Threes0::in, Threes::out) is det :-
        ( if Delta = 1
        then
            Ones = Ones0 + 1,
            Threes = Threes0
        else if Delta = 3
        then
            Threes = Threes0 + 1,
            Ones = Ones0
        else
            Threes = Threes0,
            Ones0 = Ones
        )), Deltas, 0, OnesCount, 0, ThreesCount),
    trace [io(!IO), runtime(env("TRACE"))] (
        io.write_line({"input", Joltages}, !IO),
        io.write_line({"in order", InOrder}, !IO),
        io.write_line({"pairs", Pairs}, !IO),
        io.write_line({"deltas", Deltas}, !IO),
        io.write_line({"stats", 1, OnesCount, 3, ThreesCount}, !IO)
    ),
    OnesCountIncludingOutlet = OnesCount + 1,
    ThreesCountIncludingDevice = ThreesCount + 1,
    ProductOf1And3Deltas = OnesCountIncludingOutlet * ThreesCountIncludingDevice.

:- func part2(list(int)) = int.
part2(AdapterJoltages) = CountOfDistinctArrangements :-
    -1 = CountOfDistinctArrangements.
