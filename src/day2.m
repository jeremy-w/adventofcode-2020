:- module day2.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module require, string, char, list, maybe, int.

:- pred is_newline(char::in) is semidet.
is_newline('\n').
is_newline('\r').

:- type policy
    ---> policy(
        min::int,
        max::int,
        c::char
    ).

:- type entry
    ---> entry(
        policy,
        password::string
    ).

:- pred is_valid(entry::in) is semidet.
is_valid(entry(policy(Min, Max, C), Password)) :-
    string.to_char_list(Password, Letters),
    list.filter(unify(C), Letters, ControlledLetters),
    Count = list.length(ControlledLetters),
    Count >= Min,
    Count =< Max.

:- func string_to_policy(string) = policy is semidet.
string_to_policy(String) = Policy :-
    [Range, CStr] = string.split_at_char(' ', String),
    string.first_char(CStr, C, _),
    [Min, Max] = map(det_to_int, string.split_at_char('-', Range)),
    policy(Min, Max, C) = Policy.

:- func line_to_entry(string) = entry is semidet.
line_to_entry(Line) = Entry :-
    [PolicyPart, Password] = string.split_at_char(':', Line),
    Policy = string_to_policy(PolicyPart),
    entry(Policy, string.strip(Password)) = Entry.

main(!IO) :-
    io.read_file_as_string(Result, !IO),
    ( if
        Result = ok(Input)
    then
        Lines = string.words_separator(is_newline, Input),
        Entries = list.filter_map(line_to_entry, Lines),
        io.format("Have %d entries.\n", [i(list.length(Entries))], !IO),
        ValidEntries = list.filter(is_valid, Entries),
        io.format("Part 1 Answer: Have %d valid entries.\n", [i(list.length(ValidEntries))], !IO)
    else
        error("whoopsy")
    ).
