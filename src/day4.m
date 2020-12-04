:- module day4.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, char, list, require, util, pair, int, bool.

main(!IO) :-
    test_example(!IO),
    util.read_file_as_string("../input/day4.txt", Input, !IO),
    PassportBlocks = split_at_string("\n\n", Input),
    Passports = map(passport_from_string, PassportBlocks),
    filter(passport_is_valid, Passports, ActualValids),
    io.format("Part 1: Valid count: %d\n", [i(length(ActualValids))], !IO),

    test_part2(!IO),

    filter(passport_values_are_valid, Passports, TrulyValid),
    % io.write_line(TrulyValid, !IO),
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

:- func passports_from_string(string) = list(passport).
passports_from_string(Input) = Passports :-
    PassportBlocks = split_at_string("\n\n", Input),
    Passports = map(passport_from_string, PassportBlocks).

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

%---%

:- pred fok(string::in, string::in, bool::in, io::di, io::uo) is det.
fok(Key, Value, Expected, !IO) :-
    if is_valid_field([Key, Value])
    then
        (Expected = yes
        ;
        Expected = no,
        io.format("failed %s:%s - expected valid, reported invalid", [s(Key), s(Value)], !IO)
        )
    else
        (Expected = yes,
        io.format("failed %s:%s - expected invalid, reported valid", [s(Key), s(Value)], !IO)
        ;
        Expected = no
        ).

:- pred test_part2(io::di, io::uo) is det.
test_part2(!IO) :-
    fok("byr", "2002", yes, !IO),
    fok("byr", "2003", no, !IO),
    fok("hgt"   , "60in", yes, !IO),
    fok("hgt"   , "190cm", yes, !IO),
    fok("hgt" , "190in", no, !IO),
    fok("hgt" , "190", no, !IO),

    fok("hcl"    ,"#123abc", yes, !IO),
    fok("hcl"  ,"#123abz", no, !IO),
    fok("hcl" , "123abc", no, !IO),

    fok("ecl"   , "brn", yes, !IO),
    fok("ecl" , "wat", no, !IO),

    fok("pid"   , "000000001", yes, !IO),
    fok("pid" , "0123456789", no, !IO),

    InvalidPassports = passports_from_string("eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"),
    filter(passport_values_are_valid, InvalidPassports, FalsePositives),
    io.print("False positives: ", !IO),
    io.write_line(FalsePositives, !IO),

    ValidPassports = passports_from_string("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"),
negated_filter(passport_values_are_valid, ValidPassports, FalseNegatives),
io.print("False negatives: ", !IO),
io.write_line(FalseNegatives, !IO).
