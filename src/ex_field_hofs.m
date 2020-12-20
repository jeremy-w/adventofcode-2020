% Is there some way to reference field accessors and mutators as functions in themselves?
% They seem to work fine when applied directly, e.g. f(HasField),
% and with record syntax, e.g. HasField^f, but I can't work out how to pass either flavor of field function directly to map or fold, whether with or without currying.
% I wind up having to manually declare a function to shuttle the args in and out to the fully-applied field function as a workaround.
:- module ex_field_hofs.

:- interface.
:- type has_field ---> has_field(f :: int).

:- implementation.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%
% ACCESS EXAMPLES
% Several attempts, all unsuccessful, to use `f` as a function in its own right.

    % Example of attempting to use a field accessor as an argument to a higher-order function.
    %
    % Triggers unification error when written directly as `f` - it seems `f` is being interpreted as a `char`:
    %       type error in unification of argument and constant `f'.
    %       argument has type `((func ex_field_hofs.has_field) = int)',
    %       constant `f' has type `character'.
:- func access_example1(list(has_field)) = list(int).
access_example1(Hs) = map(f, Hs).

    % Attempting to qualify it as ex_field_hofs.f complains:
    %       error: undefined symbol `ex_field_hofs.f'/0.
:- func access_example2(list(has_field)) = list(int).
access_example2(Hs) = map(ex_field_hofs.f, Hs).

    % Attempting to include the ^ shown in "Field access examples" (https://mercurylang.org/information/doc-release/mercury_ref/Field-access-examples.html#Field-access-examples) fails as well:
    %       error: undefined symbol `^ f'/0
    % Trying without the single quotes (not shown here) instead gives:
    %       error: undefined symbol `^'/1.
:- func access_example3(list(has_field)) = list(int).
access_example3(Hs) = map('^ f', Hs).

%-----------------------------------------------------------------------------%
% UPDATE EXAMPLE

    % Example of attempting to use the field setter as an argument to a higher-order function.
    % Per
:- func update_example(has_field, list(int)) = list(has_field).
update_example(H, Ns) = map('f :='(H), Ns).
