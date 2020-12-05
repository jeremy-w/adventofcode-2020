:- module day5.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module util, string, int.

:- func nrows = int.
nrows = 128.

:- func ncols = int.
ncols = 8.

:- type info ---> info(row::int, col::int).
:- func seat_id(info) = int.
seat_id(Info) = 8*(Info^row) + Info^col.

:- func pass_to_info(string) = info.
pass_to_info(Pass) = info(Row, Col) :-
    Row = 0,
    Col = 0.

main(!IO) :-
    util.read_file_as_string("../input/day5.txt", Input, !IO),
    Passes = words(Input).
