:- module day4.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, char, list, require, util, pair.

main(!IO) :-
    test_example(!IO),
    io.print_line("it builds", !IO).

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
