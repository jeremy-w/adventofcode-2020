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
cons_as_int(Line, !Accu) :-
    Num = string.det_to_int(strip(Line)),
    !:Accu = [Num | !.Accu].

% XXX: There should be some way to do this using a fold, rather than manually.
% And since we have the slurp the whole array in, there's probably a better way to parse out whitespace-separated stuff, too. Ah well.
:- pred cons_lines_as_ints(input_stream::in, list(int)::in, list(int)::out, io::di, io::uo) is det.
cons_lines_as_ints(Stream, !Accu, !IO) :-
    read_line_as_string(Stream, MaybeLine, !IO),
    (
        MaybeLine = ok(Line),
        cons_as_int(Line, !Accu),
        cons_lines_as_ints(Stream, !Accu, !IO)
    ;
        MaybeLine = eof,
        !:Accu = !.Accu
    ;
        MaybeLine = error(Error),
        io.write(Error, !IO),
        io.nl(!IO),
        !:Accu = !.Accu
    ).

main(!IO) :-
    % read stdin, one number per line, into a list.
    Stream = io.stdin_stream `with_type` io.input_stream,
    cons_lines_as_ints(Stream, [], UnsortedNumbers, !IO),

    % smart bruteforce search:
    % - sort the array large => small
    Desc = (pred(Left::in, Right::in, Result::out) is det :-
        compare(Result, Right, Left)),
    sort(Desc, UnsortedNumbers, Numbers),

    % - for each from start to end, start summing from end to start, and bail when exceeding 2020
    io.write(Numbers, !IO),
    io.nl(!IO).
