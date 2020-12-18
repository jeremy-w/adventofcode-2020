:- module day16.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

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

    % util.read_file_as_string("../input/day16.txt", Input, !IO),
    % P1 = part1(Input),
    % io.format("P1: got %d (expected 496)\n", [i(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(input) = int.
part1(Input) = SumOfOutOfAllRanges :-
    trace [io(!IO)] (io.write_line({"Input", Input}, !IO)),
    SumOfOutOfAllRanges = -1.

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
