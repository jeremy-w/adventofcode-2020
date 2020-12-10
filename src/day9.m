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
    Actual1 = part1(ints(example1), 5),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    util.read_file_as_string("../input/day9.txt", Input, !IO),
    Ns = ints(Input),
    P1 = part1(Ns, 25),
    io.format("P1: %d\n", [i(P1)], !IO),

    E2 = 62,
    A2 = part2(ints(example1), 5),
    io.format("P2 test: expected %d, got %d\n", [i(E2), i(A2)], !IO),
    P2 = part2(Ns, 25),
    io.format("P2: %d\n", [i(P2)], !IO).

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
:- func part1(list(int), int) = int.
part1(Ns, PrefixLength) = Result :-
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

%---%

:- func part2(list(int), int) = int.
part2(Ns, PrefixLength) = EncryptionWeakness :-
    Key = part1(Ns, PrefixLength),
    ( if ContiguousRange = range_summing_to(Key, Ns) then
        % To find the encryption weakness, add together the smallest and largest number in this contiguous range; in this example, these are 15 and 47, producing 62.
        SortedRange = sort(ContiguousRange),
        Min = det_head(SortedRange),
        Max = det_last(SortedRange),
        EncryptionWeakness = Min + Max
    else
        EncryptionWeakness = -1
    ).

% find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1
% TODO: find this contiguous range
:- func range_summing_to(int, list(int)) = list(int) is semidet.
range_summing_to(Key, Ns) = R :-
    WindowLengths = 2 .. length(Ns),
    map((pred(WindowLength::in, WindowsOfLength::out) is det :- filter_map(take(WindowLength), [Ns | tails(Ns)], WindowsOfLength)), WindowLengths, Windowses),
    condense(Windowses, Windows),
    find_first_match((pred(Window::in) is semidet :- foldl(plus, Window, 0) = Key), Windows, R).
