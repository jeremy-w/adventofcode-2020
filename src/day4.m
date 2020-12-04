:- module day4.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, char, list, require, util.

main(!IO) :-
    test_example(!IO),
    io.print_line("it builds", !IO).

%----%
%----%

:- func requiredFields = list(string).
requiredFields = [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
    % "cid",  % conveniently not required
].

%---%

:- pred test_example(io::di, io::uo) is det.
test_example(!IO) :-
    util.read_file_as_string("../input/day4.example.txt", Input, !IO),
    io.write_line(Input, !IO).
