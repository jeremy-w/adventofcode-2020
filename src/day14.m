:- module day14.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module util.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),

    Example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
",
    E1 = 165u,
    A1 = part1(Example),
    io.format("P1 test: expected %u, got %u\n", [u(E1), u(A1)], !IO),

    util.read_file_as_string("../input/day14.txt", Input, !IO),
    P1 = part1(Input),
    io.format("P1: got %u\n", [u(P1)], !IO),

    io.print_line("=== * ===", !IO).

:- func part1(string) = uint.
part1(Input) = SumOfMemValues :-
    Lines = split_at_string("\n", strip(Input)),
    M0 = machine(init, 0u, 0u),
    M = foldl(step, Lines, M0),
    foldl_values((pred(X::in, Y::in, S::out) is det :-
        S = X + Y), M^mem, 0u, SumOfMemValues).

:- type addr == uint.

    % Values and memory addresses are both 36-bit unsigned integers.
:- type machine
    ---> machine(
                % addr => value
            mem :: map(addr, uint),
                % for anding
            mask_set :: uint,
                % for and-notting
            mask_clear :: uint
        ).

:- func step(string, machine) = machine.
step(Line, M0) = M :-
    (if
        [Target, ValueStr] = split_at_string(" = ", Line)
    then
        (if
            Target = "mask"
        then
            M = update_masks(M0, ValueStr)
        else if
            remove_prefix("mem[", Target, Target1)
        then
            AddrStr = det_remove_suffix(Target1, "]"),
            Addr : uint = cast_from_int(det_to_int(AddrStr)),
            Value = cast_from_int(det_to_int(ValueStr)),
            M = store(M0, Addr, Value)
        else
            trace [io(!IO)] (io.write_line({"Failed parsing line", Line}, !IO)),
            M = M0
        )
    else
        trace [io(!IO)] (io.write_line({"Failed parsing line", Line}, !IO)),
        M = M0
    ).

:- func store(machine, addr, uint) = machine.
store(M0, Addr, UnmaskedValue) = M :-
        WithSets = UnmaskedValue \/ M0^mask_set,
        WithClears = WithSets /\ (\ M0^mask_clear),
        M = M0^mem^elem(Addr) := WithClears.

:- func update_masks(machine, string) = machine.
update_masks(M0, S) = M :-
    Chars: list(char) = reverse(to_char_list(S)),
    member_indexes0('1', Chars, OneIndexes),
    member_indexes0('0', Chars, ZeroIndexes),
    Build = (func(Ix, A0) = A :-
        A = twopow(Ix) + A0),
    SetMask = foldl(Build, OneIndexes, 0u),
    ClearMask = foldl(Build, ZeroIndexes, 0u),
    M1 = M0^mask_set := SetMask,
    M = M1^mask_clear := ClearMask.

:- func twopow(int) = uint.
twopow(N) =
    (if N =< 0 then 1u else 2u*twopow(N - 1)).
