%-----------------------------------------------------------------------------%
% Day 20: Jurassic Jigsaw
% 10x10 tiles need to be flipped and/or rotated to align their edges into an overall square.
% My full day input has 144 tiles, so we're looking for a 12x12 arrangement of 10x10 tiles.
:- module day20.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool, char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.
:- import_module solutions.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    util.read_file_as_string("../input/day20e1.txt", Ex1, !IO),
    Example1 = parse_input(Ex1),
    S1 = part1(Example1),
    A1 = foldl(times, corner_tile_ids(S1), 1),
    E1 = 1951 * 3079 * 2971 * 1171,
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),
    io.print_line("=== * ===", !IO).

%-----------------------------------------------------------------------------%

:- func part1(input) = solution.
part1(_Input) = S :-
    S = init.


%-----------------------------------------------------------------------------%
% INPUT HANDLING
:- type tile_id == int.
:- type tile ---> tile(id :: tile_id, lines :: list(string)).
:- type input == list(tile).

:- func parse_input(string) = input.
parse_input(String) = Result :-
    Result = [].

%-----------------------------------------------------------------------------%
% OUTPUT HANDLING
:- type loc ---> loc(
            % 0-0 through 11-11.
        x :: int, y :: int,
            % Clockwise rotation count, 0 .. 3.
        rotated :: int,
            % Whether it's flipped over or not.
        is_flipped :: bool
    ).
:- type solution == map(loc, int).

:- func corner_tile_ids(solution) = list(tile_id).
corner_tile_ids(_Solution) = [0].
