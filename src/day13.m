:- module day13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, math, float.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = [{0,7},{1,13},{4,59},{6,31},{7,19}],
    E1 = 1068781,
    A1 = solve(Example),
    io.format("P2 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    IndexAndIds = [{48, 787}, {17, 523}, {0, 17}, {7, 41}, {35, 13}, {36, 19}, {40, 23}, {54, 37}, {77, 29}],
    P2 = solve(IndexAndIds),
    io.format("P2: got %d\n", [i(P2)], !IO),

    io.print_line("=== * ===", !IO).

% input is a list of index, busId pairs.
% output is the earliest minute where each busId arrives busId minutes after that time.
% the busId is the minute the bus returns to the origin.
% they all seem to be primes.
:- func solve(list({int, int})) = int.
solve(Input) = Time :-
    sort(sortByBusIdDesc, Input, SortedBusIdDesc : list({int, int})),
    % trace [io(!IO)] (io.write_line({"sorted", SortedBusIdDesc}, !IO)),
    {Offset, BusId} = det_head(SortedBusIdDesc),
    solve_loop({Offset, BusId}, SortedBusIdDesc, 0, Time).

:- pred sortByBusIdDesc({int, int}::in, {int, int}::in, comparison_result::out) is det.
sortByBusIdDesc({_, LeftId}, {_, RightId}, Result) :-
        compare(Result, RightId, LeftId).
/*
    for each multiple of the largest bus id:
        add that bus id's offset
        check valid time for all

if it takes too long, then find a way to skip to the next candidate for the first two, maybe.
*/
:- pred solve_loop({int, int}::in, list({int, int})::in, int::in, int::out) is det.
solve_loop({Offset, BusId}, Inputs, Multiple, Time) :-
    CandidateTime = Multiple*BusId - Offset,
    % trace [io(!IO)] (io.write_line({"checking", CandidateTime, "per bus ID", BusId, "with offset", Offset}, !IO)),
    (if Multiple `mod` 100000000 = 0 then trace [io(!IO)] (io.write_line({"checking", CandidateTime, "multiple", Multiple}, !IO)) else true),
    ( if valid_time_for_all(Inputs, CandidateTime)
    then Time = CandidateTime
    else solve_loop({Offset, BusId}, Inputs, Multiple + 1, Time)).

:- pred valid_time_for_all(list({int, int})::in, int::in) is semidet.
valid_time_for_all(Inputs, T) :-
    all_true(valid_time_for_one(T), Inputs).

:- pred valid_time_for_one(int::in, {int, int}::in) is semidet.
valid_time_for_one(T, {Index0, BusId}) :-
    Ceil = ceiling(float(T) / float(BusId)),
    ArrivalTime = float(BusId) * Ceil,
    WaitTime = ArrivalTime - float(T),
    WaitTime = float(Index0).

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
