:- module example_dot_char.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char.

:- func ex = char.
ex = 'x'.

:- func colon = char.
colon = ':'. % this does not work.

:- func full_stop = char.
full_stop = '.'. % this does not work either.

:- func workaround_colon = char.
workaround_colon = det_from_int(0':). % this does, but that's a tch wordy, no?

main(!IO) :-
    true.
