/*
https://adventofcode.com/2020/day/1

Problem, part 1: Given a list of numbers, find the two entries summing to 2020. The result is the product of those two.

Test case:

1721
979
366
299
675
1456

==>

514579
*/
:- module day1.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, int, list, stream, char.

:- pred cons_as_int(string::in, list(int)::in, list(int)::out) is det.
cons_as_int(Line, Accu0, Accu) :-
    Num = string.det_to_int(strip(Line)),
    Accu = [Num | Accu0].

:- pred char_list_cons(char::in, list(char)::in, list(char)::out) is det.
char_list_cons(X, Xs, [ X | Xs ]).

main(!IO) :-
    % read stdin, one number per line, into a list.
    Stream = io.stdin_stream `with_type` io.input_stream,
    read_line_as_string(Stream, MaybeLine, !IO),
    (
        MaybeLine = ok(Line),
        cons_as_int(Line, [], Numbers)
    ;
        MaybeLine = eof,
        Numbers = []
    ;
        MaybeLine = error(Error),
        io.write(Error, !IO),
        io.nl(!IO),
        Numbers = []
    ),

    % smart bruteforce search:
    % - sort the array large => small
    % - for each from start to end, start summing from end to start, and bail when exceeding 2020
    io.write(Numbers, !IO),
    io.nl(!IO).
