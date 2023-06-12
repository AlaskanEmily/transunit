%% Some basic examples of unit tests

:- module example.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module transunit.
:- import_module transunit.compare.
:- import_module transunit.compare_test.
:- import_module unit.
:- import_module int.
:- import_module maybe.
:- import_module require.

%% This test succeeds if the expression-under-test (2 + 2) and expected result (4) unify.
:- pred t0(unit::in) is semidet.
t0(_) :-
    Res = 2 + 2,
    Res = 4.

%% This test is intended to be called from the higher-order "run_test" predicate.
:- pred t1(int::in, int::out) is det.
t1(X, Y) :-
    Y = X - 2.

:- pred addTwo(int::in, int::out) is det.
addTwo(X, Y) :-
    Y = X + 2.

%% This is my favourite way to write tests.
%% Not as much higher-order syntax as the above example, but run_result_test helps us out.
:- pred checkMaths(maybe_error::out, io::di, io::uo) is det.
checkMaths(Result, !IO) :-
    run_test(addTwo, 2, 5, Result).

%% This predicate runs through the different test cases.
%% The unit test harness is just an executable, built using "mmc --make".
main(!IO) :-
    run_test(t0, "Addition", !IO),
    run_result_test((pred(Result::out, IO::di, IO::uo) is det :- run_test(t1, 5, 3, Result)), "t1", !IO),
    run_result_test(checkMaths, "Maths", !IO).
