:- module day8.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, string, util.
:- import_module solutions.

main(!IO) :-
    % Test: Part 1
    Expected1 = 5,
    BootCode1 = boot_code_from_string(example1),
    % io.write_line(BootCode1, !IO),
    Actual1 = part1(BootCode1),
    io.format("P1 test: expected %d, got %d\n", [i(Expected1), i(Actual1)], !IO),

    util.read_file_as_string("../input/day8.txt", Input, !IO),
    BootCode = boot_code_from_string(Input),
    P1Result = part1(BootCode),
    io.format("P1: got %d\n", [i(P1Result)], !IO),

    % Test: Part 2
    Expected2 = 8,
    Actual2 = part2(BootCode1),
    io.format("P2 test: expected %d, got %d\n", [i(Expected2), i(Actual2)], !IO),

    P2Result = part2(BootCode),
    io.format("P2: got %d\n", [i(P2Result)], !IO).

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
    InitialMachine = init(BootCode),
    run_while(not_a_repeat, InitialMachine, FinalMachine, [], _VisitedList),
    % trace [io(!IO)] (sort(VisitedList, V), io.write_line(V, !IO)),
    PreRepeatAccValue = FinalMachine ^ acc.

% Fails when execution would repeat an instruction.
:- pred not_a_repeat(machine::in, list(int)::in, list(int)::out) is semidet.
not_a_repeat(M, !State) :-
    trace [io(!IO), runtime(env("TRACE_REPEAT"))] (io.write_line(!.State, !IO)),
    not member(M ^ ip, !.State),
    trace [io(!IO), runtime(env("TRACE_REPEAT"))] (io.print_line("continuing execution - not a repeat", !IO)),
    cons(M^ip, !State).

%---%

% Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp).
% What is the value of the accumulator after the program terminates?
:- func part2(boot_code) = int.
part2(BootCode) = TerminalValue :-
    BootCodes = mutate(BootCode),
    (if
        find_first_match(machine_terminates, BootCodes, TerminatingBootCode)
    then
        InitialMachine = init(TerminatingBootCode),
        run_while(not_a_repeat, InitialMachine, FinalMachine, [], _VisitedList),
        % trace [io(!IO)] (sort(_VisitedList, V), io.write_line(V, !IO)),
        TerminalValue = FinalMachine^acc
    else
        TerminalValue = -1
    ).

:- pred machine_terminates(boot_code::in) is semidet.
machine_terminates(BootCode) :-
    run_while(not_a_repeat, init(BootCode), FinalMachine, [], _),
    FinalMachine^ip = length(BootCode).

:- func mutate(boot_code) = list(boot_code).
mutate(BaseCode) = Mutants :-
    solutions((pred(Mutant::out) is nondet :-
        swap_nop_jmp(BaseCode, Mutant)), Mutants).

:- pred swap_nop_jmp(boot_code::in, boot_code::out) is nondet.
swap_nop_jmp([instruction(nop, N) | Rest], [instruction(jmp, N) | Rest]).
swap_nop_jmp([instruction(jmp, N) | Rest], [instruction(nop, N) | Rest]).
swap_nop_jmp([I | Rest0], [I | Rest]) :-
    swap_nop_jmp(Rest0, Rest).

%---%

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

:- func boot_code_from_string(string) = boot_code.
boot_code_from_string(Str) = BootCode :-
    Lines = split_at_string("\n", strip(Str)),
    BootCode = filter_map(instruction_from_string, Lines).

:- func instruction_from_string(string) = instruction is semidet.
instruction_from_string(Line) = Insn :-
    [Word, Arg] = words(Line),
    Cmd = command_from_string(Word),
    to_int(Arg, N),
    Insn = instruction(Cmd, N).

:- func command_from_string(string) = command is semidet.
command_from_string("acc") = acc.
command_from_string("jmp") = jmp.
command_from_string("nop") = nop.

:- pred run_while(pred(machine, A, A)::in(pred(in, in, out) is semidet), machine::in, machine::out, A::in, A::out) is det.
run_while(ShouldContinue, !Machine, !Accu) :-
    (if !.Machine^ip < length(!.Machine^code),
        ShouldContinue(!.Machine, !Accu)
    then
        step(!Machine),
        run_while(ShouldContinue, !Machine, !Accu)
    else
        !:Machine = !.Machine,
        !:Accu = !.Accu
    ).

:- pred step(machine::in, machine::out) is det.
step(!Machine) :-
    I = next_instruction(!.Machine),
    trace [io(!IO), runtime(env("TRACE_STEP"))] (io.nl(!IO), io.write_line(I, !IO), io.write_line(!.Machine, !IO)),
    step(I, !Machine).

:- func next_instruction(machine) = instruction.
next_instruction(M) = det_index0(M^code, M^ip).

:- pred step(instruction::in, machine::in, machine::out) is det.
step(instruction(acc, N), !M) :-
    M = !.M^acc := !.M^acc + N,
    !:M = M^ip := M^ip + 1.
step(instruction(jmp, N), !M) :-
    !:M = !.M^ip := !.M^ip + N.
step(instruction(nop, _), !M) :-
    M = !.M,
    !:M = M^ip := M^ip + 1.
