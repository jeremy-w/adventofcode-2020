:- module day7.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, list, char, int.

% Lets us say "BagX contained_by BagY-WithMultiplicity for multiple BagY-WithMultiplicity pairs.
% Or BagY contains BagX-WithMultiplicity.
:- import_module multi_map.

:- func my_bag = string.
my_bag = "shiny gold".

main(!IO) :-
    TestCount = part1(""),
    io.format("Part1 Test: Expected 4, found %d", [i(TestCount)], !IO).

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

:- func part1(string) = int.
part1(BagToHold) = -1.
