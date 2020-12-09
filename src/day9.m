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
    Expected1 = expected1,
    Actual1 = part1(example1, 5),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    util.read_file_as_string("../input/day9.txt", Input, !IO),
    P1 = part1(Input, 25),
    io.format("P1: %d\n", [i(P1)], !IO).

:- func example1 = string.
example1 = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
".

:- func expected1 = int.
expected1 = 127.

% find the first number in the list (after the preamble) which is not the sum of two of the 25 numbers before it. What is the first number that does not have this property?
:- func part1(string, int) = int.
part1(Input, PrefixLength) = Result :-
    Ns = ints(Input),
    Tails = tails(Ns),
    filter_map(take(PrefixLength+1), [Ns | Tails], PrefixAndToCheck),
    % trace [io(!IO)] (io.write_line(PrefixAndToCheck, !IO)),
    ( if find_first_match(oddball, PrefixAndToCheck, Oddball) then
        Result = det_last(Oddball)
    else
        Result = -1
    ).

:- pred oddball(list(int)::in) is semidet.
oddball(L) :-
    Ns = reverse(L),
    split_list(1, Ns, ToCheck, Prefix),
    not sum_of(det_head(ToCheck), Prefix).

:- pred sum_of(int::in, list(int)::in) is semidet.
sum_of(N, Ns) :-
    member(X, Ns),
    delete_first(Ns, X, Rest),
    member(Y, Rest),
    N = X + Y.

:- func ints(string) = list(int).
ints(Input) = Ns :-
    filter_map(string.to_int, words(strip(Input)), Ns).

:- func tails(list(N)) = list(list(N)).
tails([]) = [].
tails([_ | T]) = [T | tails(T)].
