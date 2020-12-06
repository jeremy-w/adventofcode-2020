:- module example_io_mostly_unique.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
main(!IO) :-
    Path = "does not exist",
    (if
        io.open_input(Path, ok(_), !IO)
    then
        io.write_line("ok", !IO)
    else
        io.write_line("err", !IO)
     ).

/*
Fails with:

>
mmc --make --use-subdirs -E example_io_mostly_unique
Making Mercury/int3s/example_io_mostly_unique.int3
Making Mercury/ints/example_io_mostly_unique.int
Making Mercury/cs/example_io_mostly_unique.c
example_io_mostly_unique.m:011: In clause for `main(di, uo)':
example_io_mostly_unique.m:011:   in argument 3 of call to predicate
example_io_mostly_unique.m:011:   `io.open_input'/4:
example_io_mostly_unique.m:011:   mode error: variable `STATE_VARIABLE_IO_0'
example_io_mostly_unique.m:011:   has instantiatedness `mostly_unique',
example_io_mostly_unique.m:011:   expected instantiatedness was `unique'.
** Error making `Mercury/cs/example_io_mostly_unique.c'.

*/
