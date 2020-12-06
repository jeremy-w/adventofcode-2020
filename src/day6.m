:- module day6.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module util, string, char, list, int.

:- func group_to_answers(string) = string.
group_to_answers(Group) = Answers :-
    WithoutNewlines = replace_all(Group, "\n", ""),
    Uniqued = remove_adjacent_dups(sort(to_char_list(WithoutNewlines))),
    Answers = from_char_list(Uniqued).

:- func plus(int, int) = int.
plus(X, Y) = X + Y.

main(!IO) :-
    util.read_file_as_string("../input/day6.txt", Input, !IO),
    Groups = split_at_string("\n\n", Input),
    YesAnswers = map(group_to_answers, Groups),
    Lengths = map(string.length, YesAnswers),
    Sum = list.foldl(day6.plus, Lengths, 0),
    io.format("Part 1 answer: they sum to: %d", [i(Sum)], !IO).
