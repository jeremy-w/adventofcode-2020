:- module day3.

:- interface.
:- use_module io.
:- pred main(io.io::di, io.io::uo) is det.

:- implementation.
main(!IO) :-
    io.print_line("howdy", !IO).
