:- module day19.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util, require.
:- import_module map, assoc_list, one_or_more.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb
",
    E1 = 2,
    A1 = part1(parse_input(Example)),
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    % util.read_file_as_string("../input/day19.txt", Input, !IO),
    % P1 = part1(parse_input(Input)),
    % io.format("P1: got %d (expected ?)\n", [i(P1)], !IO),

    % P2 = part2(parse_input(Input)),
    % io.format("P2: got %d (expected ?)\n", [i(P2)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(input) = int.
part1(Input) = ValidMessageCount :-
    trace [io(!IO)] (io.write_line({"Input", to_sorted_assoc_list(Input^rules): assoc_list(int, a_rule)}, !IO)),
    ValidMessageCount = 0.

% ???: it barfed on a type named "rule"? ah, it's a builtin operator. https://mercurylang.org/information/doc-release/mercury_ref/Builtin-operators.html#Builtin-operators
:- type a_rule
    ---> literally(char)
    ;       either(a_rule, a_rule)
                % ID 1 then 2 OR 3 then 4.
    ;       seq(one_or_more(int))
                % ID 1 then ID 2 then…
    .
:- type rules == map(int, a_rule).
:- type messages == list(string).
:- type input ---> input(rules :: rules, messages :: messages).

:- func parse_input(string) = input.
parse_input(String) = Input :-
    (if parse(String, Result)
    then Input = Result
    else unexpected($module, $pred, string.format("bogus input: %s", [s(String)]))).

:- pred parse(string::in, input::out) is semidet.
parse(String, Input) :-
    trace [io(!IO)] (io.write_line({"String", String}, !IO)),
    [RuleSec, MessageSec] = split_at_string("\n\n", strip(String)),
    RuleLines = split_at_string("\n", RuleSec),
    foldl(parse_rule, RuleLines, map.init, Rules),
    Messages = split_at_string("\n", MessageSec),
    Input = input(Rules, Messages).

/* Like:
102: 100 47 | 76 84
108: 55 100
8: 42
47: "a"
*/
:- pred parse_rule(string::in, rules::in, rules::out) is semidet.
parse_rule(Line, M0, M) :-
    trace [io(!IO)] (io.write_line({"Line", Line}, !IO)),
    [Id, Body] = split_at_string(": ", Line),
    to_int(Id, I),
    parse_body(Body, Rule),
    M = M0^elem(I) := Rule.

:- pred parse_body(string::in, a_rule::out) is det.
parse_body(Body, Rule) :-
    trace [io(!IO)] (io.write_line({"Body", Body}, !IO)),
    (if
        [H1, H2] = split_at_string(" | ", Body),
        parse_body(H1, S1),
        parse_body(H2, S2)
     then
        Rule = either(S1, S2)
     else if
         Numbers = split_at_string(" ", Body),
         map(to_int, Numbers, IDs),
         list_to_one_or_more(IDs, OneOrMoreIDs)
      then
         Rule = seq(OneOrMoreIDs)
      else if
            to_char_list(Body, ['"', C, '"'])
        then
            Rule = literally(C)
        else
            unexpected($module, $pred, format("Bogus rule body: %s", [s(Body)]))
    ).
