:- module util.

:- interface.
:- import_module io.
    % Reads the file at the path into a string.
    % Throws if any error occurs.
:- pred read_file_as_string(string::in, string::out, io::di, io::uo) is det.

:- implementation.
:- import_module string, require, list.

read_file_as_string(Path, FullText) -->
    io.open_input(Path, OpenResult),
    (
        { ok(Stream) = OpenResult },
        io.read_file_as_string(Stream, ReadResult),
        (
            { ok(FullText) = ReadResult }
        ;
            { error(_, _) = ReadResult },
            {unexpected($module, $pred, string.format("Failed reading file %s", [s(Path)]))}
        ),
        io.close_input(Stream)
    ;
        { error(_) = OpenResult },
        {unexpected($module, $pred, string.format("Failed opening file %s", [s(Path)]))}
    ).
