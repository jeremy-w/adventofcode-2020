:- module day8.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.

main(!IO) :-
    Expected1 = 5,
    Actual1 = part1(from_string(example1)),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO).

:- func example1 = string.
example1 = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
".

% Immediately before any instruction is executed a second time, what value is in the accumulator?
:- func part1(boot_code) = int.
part1(BootCode) = PreRepeatAccValue :-
    PreRepeatAccValue = -1.

:- type boot_code == list(instruction).
:- type instruction ---> instruction(cmd :: command, arg :: int).
:- type command
        --->        acc  % adds its argument to the current value of the acc register
        ;           jmp  % adds its argument to the ip register, which is pointing at it
        ;           nop  % ignores its argument
        .

:- type machine ---> machine(code :: boot_code, ip :: int, acc :: int).
:- func init(boot_code) = machine.
init(Code) = machine(Code, 0, 0).

:- func from_string(string) = boot_code.
from_string(Str) = [].
