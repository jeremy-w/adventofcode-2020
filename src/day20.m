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

/*
In part 1, we actually only care about edge tiles, so we could just treat a tile as a list of 4 rows matching the outer edge spots.
Rotation is then rotation of the list, and flipping is swapping top/bottom or left/right indexes.
(And uh I guess flipping left-to-right is different from top-to-bottom, whoops.)
If we're lucky, this is like day 16 part 2 where there's always an unambiguous compatibility.
Sure looks like it from a quick skim.
*/

%-----------------------------------------------------------------------------%
% INPUT HANDLING
:- type tile_id == int.
:- type tile ---> tile(id :: tile_id, lines :: list(string)).
:- type input == list(tile).

:- func parse_input(string) = input.
parse_input(String) = Tiles :-
    Secs = split_at_string("\n\n", strip(String)),
    Tiles = filter_map(parse_tile, Secs),
    expect(unify(length(Tiles): int, length(Secs)), $module, "Failed to parse a tile").

:- func parse_tile(string) = tile is semidet.
parse_tile(String) = Tile :-
    [Label | TileLines] = split_at_string("\n", String),
    [_, IDColon] = words(Label),
    left(IDColon, length(IDColon) - 1, IDString),
    ID = det_to_int(IDString),
    Tile = tile(ID, TileLines).

%-----------------------------------------------------------------------------%
% OUTPUT HANDLING
% 0-0 through 11-11.
:- type point ---> point(x::int, y::int).
:- type loc ---> loc(
            % The tile at the point.
        tile_id :: tile_id,
            % Clockwise rotation count, 0 .. 3.
        rotated :: int,
            % Whether it's flipped over or not.
        is_flipped :: bool
        ).
:- func tile_id(loc) = tile_id.
:- type solution == map(point, loc).

:- func corner_tile_ids(solution) = list(tile_id).
corner_tile_ids(Solution) = map(tile_id,
    apply_to_list(
        [point(0, 0), point(0, 11), point(11, 0), point(11, 11)],
        Solution)).
