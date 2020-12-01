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

main(!IO) :-
    % read stdin, one number per line, into an array
    % smart bruteforce search:
    % - sort the array large => small
    % - for each from start to end, start summing from end to start, and bail when exceeding 2020
    io.write_string("Hi!\n", !IO).
