:- module day4.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, char, list, require, util, pair, int.

main(!IO) :-
    test_example(!IO),
    util.read_file_as_string("../input/day4.txt", Input, !IO),
    PassportBlocks = split_at_string("\n\n", Input),
    Passports = map(passport_from_string, PassportBlocks),
    filter(passport_is_valid, Passports, ActualValids),
    io.format("Part 1: Valid count: %d\n", [i(length(ActualValids))], !IO),
    filter(passport_values_are_valid, Passports, TrulyValid),
    io.format("Part 2: Valid count: %d\n", [i(length(TrulyValid))], !IO).

%----%
%----%

:- type key == string.
:- func requiredKeys = list(key).
requiredKeys = [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
    % "cid",  % conveniently not required
].

:- type value == string.
:- type field == list(string).
:- type passport == list(field).

:- func passport_from_string(string) = passport.
passport_from_string(String) = Passport :-
    Fields = words(String),
    Passport = map(split_at_string(":"), Fields).

:- pred passport_is_valid(passport::in) is semidet.
passport_is_valid(Passport) :-
    Keys = map(det_head, Passport),
    % Valid if removing all present keys from the required keys list gives the empty list.
    delete_elems(requiredKeys, Keys, []).

:- pred is_valid_field(list(string)::in) is semidet.
is_valid_field(["byr", Year]) :-
    to_int(Year, N),
    N =< 2002,
    N >= 1920.
is_valid_field(["iyr", Year]) :-
    to_int(Year, N),
    N =< 2020,
    N >= 2010.
is_valid_field(["eyr", Year]) :-
    to_int(Year, N),
    N =< 2030,
    N >= 2020.
is_valid_field(["hgt", S]) :-
    (
         remove_suffix(S, "cm", H),
         to_int(H, N),
        N =< 193,
        N >= 150
    ;
         remove_suffix(S, "in", H),
         to_int(H, N),
        N =< 76,
        N >= 59
    ).
is_valid_field(["hcl", S]) :-
    remove_prefix("#", S, C),
    length(C) = 6,
    all_match(contains_char("0123456789abcdef"), C).

is_valid_field(["ecl", "amb"]).
is_valid_field(["ecl", "blu"]).
is_valid_field(["ecl", "brn"]).
is_valid_field(["ecl", "gry"]).
is_valid_field(["ecl", "grn"]).
is_valid_field(["ecl", "hzl"]).
is_valid_field(["ecl", "oth"]).

is_valid_field(["pid", S]) :-
    length(S) = 9,
    is_all_digits(S).

:- pred passport_values_are_valid(passport::in) is semidet.
passport_values_are_valid(Passport) :-
    passport_is_valid(Passport),
    all_true(is_valid_field, Passport).

%---%

:- pred test_example(io::di, io::uo) is det.
test_example(!IO) :-
    util.read_file_as_string("../input/day4.example.txt", Input, !IO),
    PassportBlocks = split_at_string("\n\n", Input),
    Passports = map(passport_from_string, PassportBlocks),
    filter(passport_is_valid, Passports, ActualValids),
    ExpectedValids = [det_index1(Passports, 1), det_index1(Passports, 3)],
    (if
        ActualValids = ExpectedValids
    then
        io.print_line("ok - part 1 example", !IO)
    else
        io.print_line("not ok - part 1 example", !IO),
        io.write("All passports: ", !IO),
        io.write_line(Passports, !IO),
        io.write("Given: ", !IO),
        io.write_line(ActualValids, !IO),
        io.write("Expected: ", !IO),
        io.write_line(ExpectedValids, !IO)
    ).
