:- module day23.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string, util, require.
:- import_module array.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = [3, 2, 4, 1, 5],
    io.write({"Example after 10 rounds", run(Example, 10)}),

    % io.write({"Example after 100 rounds", run(Example, 100)}),

    Input = [7, 1, 6, 8, 9, 2, 5, 4, 3],
    % Input,
    % io.write({"Input after 100 rounds", run(Input, 100)}),

    io.print_line("=== * ===", !IO).

:- func run(list(int), int) = list(int).
run(Cups, Rounds) = FinalCups :-
    FinalCups = foldl(move, 0 .. (Rounds - 1), array.from_list(Cups)).

:- func move(int, array_di(int)) = array_uo(int).
move(Round, Cups) = NextCups :-
    CurrentIndex0 = Round `mod` length(Cups),
    CurrentCup = Cups^elem(CurrentIndex0),
    Target = CurrentCup - 1,
    % ugh this gets super messy at the end of the array.
    % we basically need to add our own "circular index" operations to make it sane.
    % would our python solution be faster if we used "copy up by 3 destructively" rather than "reassemble with slice"? or even reassignmen?
