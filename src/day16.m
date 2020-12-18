:- module day16.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    % Expected union range: 1-3, 5-11, 13-50
    Example = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
",
    E1 = 71,
    A1 = part1(parse_input(Example)),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    util.read_file_as_string("../input/day16.txt", Input, !IO),
    P1 = part1(parse_input(Input)),
    io.format("P1: got %d (expected 26941)\n", [i(P1)], !IO),

    E2: assignment = map.from_assoc_list(["row" - 0, "class" - 1, "seat" - 2]),
    A2 = find_assignments(parse_input(Example)),
    io.write_line({"P2 test", "got", to_sorted_assoc_list(A2): assoc_list(field, int), "--- expected", to_sorted_assoc_list(E2): assoc_list(field, int)}, !IO),

    % P2 = part2(parse_input(Input)),
    % io.format("P2: got %d (expected ?)\n", [i(P2)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(input) = int.
part1(Input) = SumOfOutOfAllRanges :-
    % trace [io(!IO)] (io.write_line({"Input", Input}, !IO)),
    MegaRange = megarange(Input),
    AllValues = condense(Input^nearby),
    OutOfRange = negated_filter(in_range(MegaRange), AllValues),
    % trace [io(!IO)] (io.write_line({"OutOfRange", OutOfRange, "MegaRange", MegaRange}, !IO)),
    SumOfOutOfAllRanges = foldl(plus, OutOfRange, 0).

:- func megarange(input) = ranges.
megarange(I) = foldl(union, map.values(I^fields), empty).

:- pred in_range(ranges::in, int::in) is semidet.
in_range(R, N) :- member(N, R).

:- func part2(input) = int.
part2(Input) = ProductOfMyDepartureFields :-
    DepartureFields: list(string) = filter((pred(X::in) is semidet :-
        prefix(X, "departure")), keys(Input^fields)),
    trace [io(!IO)] (io.write_line({"DepartureFields", DepartureFields}, !IO)),

    Assignment = find_assignments(Input),
    AllValues = ticket_fields(Input^mine, Assignment),
    DepartureValues = apply_to_list(DepartureFields, AllValues),

    ProductOfMyDepartureFields = foldl(times, DepartureValues, 1).

    % Maps a field to its index0 in a ticket.
:- type assignment == map(field, int).

:- func find_assignments(input) = assignment.
find_assignments(Input) = Assignment :-
    % Throw out invalid tickets.
    SaneInput = Input^nearby := filter(all_true(in_range(megarange(Input))), Input^nearby),
    % Solve for field assignments by testing permutations till one of them succeeds for all remaining tickets.
    Assignment = map_values_only(constantly(0), Input^fields).

:- pred fits(map(field, ranges)::in, assignment::in, ticket::in) is semidet.
fits(Fields, Assignment, Ticket) :-
    FieldToValue = ticket_fields(Ticket, Assignment),
    all_true((pred(Key::in) is semidet :- member(FieldToValue^elem(Key), Fields^elem(Key))), keys(Fields)).

:- func ticket_fields(ticket, assignment) = map(field, int).
ticket_fields(Ticket, Assignment) = map_values_only(det_index0(Ticket), Assignment).

:- type field == string.
:- type ticket == list(int).
:- type input
    ---> input(
        fields :: map(field, ranges),
        mine :: ticket,
        nearby :: list(ticket)
    ).

:- func parse_input(string) = input.
parse_input(String) = Input :-
    Sections = split_at_string("\n\n", strip(String)),
    (if
        [FieldLines, MineLines, NearbyLines] = map(split_at_string("\n"), Sections)
     then
        Fields = map.set_from_assoc_list(map.init, condense(map(parse_field, FieldLines))),
        Mine = parse_ticket(det_index0(MineLines, 1)),
        Nearby = list.map(parse_ticket, det_tail(NearbyLines)),
        Input = input(Fields, Mine, Nearby)
     else
         unexpected($module, $pred, "Bad Sections")
    ).

% Field like "arrival track: 40-512 or 519-964"
:- func parse_field(string) = assoc_list(field, ranges).
parse_field(Line) = Field :-
    (if
        [Name, Rules] = split_at_string(": ", strip(Line))
     then
        RangeStrings = split_at_string(" or ", Rules),
        RangeList = filter_map((func(X) = Result is semidet :-
            [Lo, Hi] = map(det_to_int, split_at_string("-", X)),
            Result = range(Lo, Hi)), RangeStrings),
        Ranges = foldl(union, RangeList, empty),
        Field = [Name-Ranges]
     else
         unexpected($module, $pred, string.format("Bad field line %s", [s(Line)]))
    ).

% Ticket like "123,456,789"
:- func parse_ticket(string) = ticket.
parse_ticket(Line) = map(det_to_int, split_at_string(",", strip(Line))).
