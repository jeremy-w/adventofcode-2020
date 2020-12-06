:- module day6.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module util, string, char, list, int, set.

:- func group_to_answers(string) = string.
group_to_answers(Group) = Answers :-
    WithoutNewlines = replace_all(Group, "\n", ""),
    Uniqued = remove_adjacent_dups(sort(to_char_list(WithoutNewlines))),
    Answers = from_char_list(Uniqued).

:- func group_to_all_answers(string) = string.
group_to_all_answers(Group) = Answers :-
    PerPerson: list(list(char)) = map(to_char_list, split_at_string("\n", strip(Group))),
    Sets = map(list_to_set, PerPerson),
    trace [io(!IO), runtime(env("TRACE"))] (io.write_line(Sets, !IO)),
    % XXX: Initially folded with set.init, but that gave the empty set each time!
    AllAnsweredYesSet = foldl(set.intersect, Sets, det_head(Sets)),
    Answers = from_char_list(set.to_sorted_list(AllAnsweredYesSet)).

main(!IO) :-
    util.read_file_as_string("../input/day6.txt", Input, !IO),
    Groups = split_at_string("\n\n", Input),
    YesAnswers = map(group_to_answers, Groups),
    Lengths = map(string.length, YesAnswers),
    % ???: Blows up if I use (+) rather than spelt-out plus.
    Sum = foldl(plus, Lengths, 0),
    io.format("Part 1 answer: they sum to: %d\n", [i(Sum)], !IO),

    EveryAnswers = map(group_to_all_answers, Groups),
    Part2 = foldl(plus, map(string.length, EveryAnswers), 0),
    io.format("Part 2 answer: they sum to: %d\n", [i(Part2)], !IO).
