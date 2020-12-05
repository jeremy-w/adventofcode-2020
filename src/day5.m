:- module day5.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module util, string, int, list.

:- func nrows = int.
nrows = 128.

:- func ncols = int.
ncols = 8.

:- type info ---> info(row::int, col::int).
:- func seat_id(info) = int.
seat_id(Info) = 8*(Info^row) + Info^col.

:- func pass_to_info(string) = info.
pass_to_info(Pass) = info(Row, Col) :-
    parse_row(Pass, Row, Rest),
    parse_col(Rest, Col, _).

:- pred parse_row(string::in, int::out, string::out) is det.
parse_row(String, Row, Rest) :-
    do_parse_row(String, Row, Rest, 0, nrows - 1).

:- pred do_parse_row(string::in, int::out, string::out, int::in, int::in) is det.
do_parse_row(String0, Row, Rest, MinRow, MaxRow) :-
    trace [io(!IO), runtime(env("TRACE"))] (io.write_line({String0, MinRow, MaxRow}, !IO)),
    NextWidth = (MaxRow - MinRow) / 2,
    (if
        remove_prefix("F", String0, String)
    then
        do_parse_row(String, Row, Rest, MinRow, MinRow + NextWidth)
    else (
        if
            remove_prefix("B", String0, String)
        then
            do_parse_row(String, Row, Rest, 1 + MinRow + NextWidth, MaxRow)
        else
            Rest = String0,
            Row = MinRow
    )
    ).

:- pred parse_col(string::in, int::out, string::out) is det.
parse_col(String0, Col, Rest) :-
    replace_all(String0, "L", "F", String1),
    replace_all(String1, "R", "B", String2),
    do_parse_row(String2, Col, Rest, 0, ncols - 1).

main(!IO) :-
    Test1 = "FBFBBFFRLR",
    Expected1 = info(44, 5),
    Actual1 = pass_to_info(Test1),
    io.format("Test1: %s, expected => given:\n", [s(Test1)], !IO),
    io.write_line(Expected1, !IO),
    io.write_line(Actual1, !IO),

    util.read_file_as_string("../input/day5.txt", Input, !IO),
    Passes = words(Input),
    Infos = map(pass_to_info, Passes),
    SeatIDs: list(int) = sort(map(seat_id, Infos)),
    io.print("Part 1 answer: ", !IO),
    io.write_line(det_head(reverse(SeatIDs)), !IO).
