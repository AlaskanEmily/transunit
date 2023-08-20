% This work is hereby placed into the public domain.
%

:- module examples.

%==============================================================================%
% Some basic examples of unit tests
:- interface.
%==============================================================================%

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%==============================================================================%
:- implementation.
%==============================================================================%

:- use_module transunit.
:- use_module transunit.compare.
:- use_module transunit.compare_test.
:- use_module unit.
:- import_module int.
:- use_module maybe.

%-----------------------------------------------------------------------------%
% This test succeeds if the expression-under-test (2 + 2) and expected result
% (4) unify.
:- pred t0(unit.unit::in) is semidet.
t0(_) :-
    Res = 2 + 2,
    Res = 4.

%-----------------------------------------------------------------------------%
% This test is intended to be called from the higher-order "run_test"
% predicate.
:- pred t1(int::in, int::out) is det.
t1(X, Y) :-
    Y = X - 2.

%-----------------------------------------------------------------------------%

:- pred add_two(int::in, int::out) is det.
add_two(X, Y) :-
    Y = X + 2.

%-----------------------------------------------------------------------------%
% This is my favourite way to write tests.
% Not as much higher-order syntax as the above example, but run_result_test
% helps us out.
:- pred check_maths(maybe.maybe_error::out, io::di, io::uo) is det.
check_maths(Result, !IO) :-
    transunit.compare_test.run_test(add_two, 2, 5, Result).

%-----------------------------------------------------------------------------%
% This predicate runs through the different test cases.
% The unit test harness is just an executable, built using "mmc --make".
main(!IO) :-
    transunit.run_test(t0, "Addition", !IO),
    transunit.run_result_test((pred(Result::out, IO::di, IO::uo) is det :-
        transunit.compare_test.run_test(t1, 5, 3, Result)), "t1", !IO),
    transunit.run_result_test(check_maths, "Maths", !IO).
