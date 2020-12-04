:- module day4.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, char, list.

main(!IO) :-
    io.print_line("it builds", !IO).
