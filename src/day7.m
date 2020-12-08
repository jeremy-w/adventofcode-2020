:- module day7.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, list, char, int, util.

% Lets us say "BagX contained_by BagY-WithMultiplicity for multiple BagY-WithMultiplicity pairs.
% Or ColorY contains ColorX-WithMultiplicity.
:- import_module multi_map.
:- import_module set.

:- func my_bag = string.
my_bag = "shiny gold".

main(!IO) :-
    TestCount = part1(my_bag, example_rules),
    io.format("Part1 Test: Expected 4, found %d\n", [i(TestCount)], !IO),
    util.read_file_as_string("../input/day7.txt", Input, !IO),
    Part1Count = part1(my_bag, Input),
    io.format("Part1 Answer: %d\n", [i(Part1Count)], !IO).

:- func example_rules = string.
example_rules = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
".
:- type contains_map == multi_map(string, {int, string}).  % "light red" => {1, "bright white"}
:- type contained_by_map == multi_map(string, {string, int}). % "bright white" => {"light red", 1}
:- func compile_rules(string) = {contains_map, contained_by_map}.
compile_rules(RuleText) = {Contains, ContainedBy} :-
    Rules = condense(filter_map(parse_rule, split_at_string("\n", RuleText))),
    trace [io(!IO), runtime(env("TRACE_RULES"))] (io.write_line(Rules, !IO)),
    foldl2((pred({Container, N, Contained}::in, !.ContainsMap::in, !:ContainsMap::out, !.ContainedMap::in, !:ContainedMap::out) is det :-
        add(Container, {N, Contained}, !ContainsMap),
        add(Contained, {Container, N}, !ContainedMap)), Rules, init, Contains, init, ContainedBy).

:- func parse_rule(string) = list({string, int, string}) is semidet.
parse_rule(Line) = Items :-
    [ContainingColor, ContainedColors] = split_at_string(" bags contain ", Line),
    Words = words(ContainedColors),
    ByBag = chunk(Words, 4 /* "9 adj color bags" */),
    CountBags = filter_map(parse_count_color, ByBag),
    Items = map((func({N, Color}) = {ContainingColor, N, Color}), CountBags),
    not Items = [].

:- func parse_count_color(list(string)) = {int, string} is semidet.
parse_count_color([Count, Adj, Color, Bags]) = Result :-
    string.to_int(Count, N),
    Result = {N, string.format("%s %s", [s(Adj), s(Color)])}.

:- func part1(string, string) = int.
part1(BagToHold, RuleText) = ColorCount :-
    Maps = compile_rules(RuleText),
    {Contains, ContainedBy} = Maps,
    ColorCount = length(reaches(ContainedBy, BagToHold)).

% Returns the colors that contain the color.
:- func reaches(contained_by_map, string) = list(string).
reaches(M, Color) = Colors :-
    trace [io(!IO), runtime(env("TRACE_REACHES"))] (io.format("what reaches Color=%s?\n", [s(Color)], !IO)),
    (if
        search(M, Color, Match)
    then
        Seed = map((func({C, N}) = C), Match),
        Reaches = Seed ++ condense(map(reaches(M), Seed)),
        Colors = remove_adjacent_dups(sort(Reaches)),
        trace [io(!IO), runtime(env("TRACE_REACHES"))] (io.format("reaching Color=%s: ", [s(Color)], !IO), io.write_line(Colors, !IO))
    else
        Colors = []).
