:- module util.

:- interface.
:- import_module io.
:- import_module list.

    % Reads the file at the path into a string.
    % Throws if any error occurs.
:- pred read_file_as_string(string::in, string::out, io::di, io::uo) is det.

    % Returns the whitespace-separated numbers in the string.
    % Non-numbers will be ignored - only and all numbers will be returned.
:- func ints(string) = list(int).

    % Returns a list of the tails of the list.
:- func tails(list(N)) = list(list(N)).

:- implementation.
:- import_module string, require.

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

%---%
% LIST UTILITIES

ints(Input) = Ns :-
    filter_map(string.to_int, words(strip(Input)), Ns).

tails([]) = [].
tails([_ | T]) = [T | tails(T)].
