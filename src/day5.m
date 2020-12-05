:- module day5.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
main(!IO) :-
    io.print_line("yup", !IO).
