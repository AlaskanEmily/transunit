% Copyright (C) 2019 Alaskan Emily, Transnat Games.
%
% This software is provided 'as-is', without any express or implied warranty.
% In no event will the authors be held liable for any damages arising from
% the use of this software.
%
% Permission is granted to anyone to use this software for any purpose,
% including commercial applications, and to alter it and redistribute it
% freely, subject to the following restrictions:
%
%   1. The origin of this software must not be misrepresented; you must not
%      claim that you wrote the original software. If you use this software
%      in a product, an acknowledgment in the product documentation would be
%      appreciated but is not required.
%
%   2. Altered source versions must be plainly marked as such, and must not
%      be misrepresented as being the original software.
%
%   3. This notice may not be removed or altered from any source distribution.
%

:- module transunit.compare_test.

%=============================================================================%
% Implements test preds where the result is compared to an expected result.
:- interface.
%=============================================================================%

:- use_module maybe.
:- use_module io.

%-----------------------------------------------------------------------------%
% NOTE: this runner does NOT catch exceptions!
:- pred run_test(pred(A, B), A, B, maybe.maybe_error)
    <= compare(B).
:- mode run_test(pred(in, out) is det, in, in, out) is det.
:- mode run_test(pred(di, out) is det, di, in, out) is det.
:- mode run_test(pred(in, out) is cc_multi, in, in, out) is det.
:- mode run_test(pred(di, out) is cc_multi, di, in, out) is det.
:- mode run_test(pred(in, out) is semidet, in, in, out) is det.
:- mode run_test(pred(mdi, out) is semidet, mdi, in, out) is det.
:- mode run_test(pred(in, out) is cc_nondet, in, in, out) is det.
:- mode run_test(pred(mdi, out) is cc_nondet, mdi, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred run_test(pred(A, B, State, State), A, B, maybe.maybe_error, State, State)
    <= compare(B).
:- mode run_test(pred(in, out, di, uo) is det, in, in, out, di, uo) is det.
:- mode run_test(pred(in, out, in, out) is det, in, in, out, in, out) is det.
:- mode run_test(pred(di, out, di, uo) is det, di, in, out, di, uo) is det.
:- mode run_test(pred(di, out, in, out) is det, di, in, out, in, out) is det.
:- mode run_test(pred(in, out, di, uo) is cc_multi, in, in, out, di, uo) is det.
:- mode run_test(pred(in, out, in, out) is cc_multi, in, in, out, in, out) is det.
:- mode run_test(pred(di, out, di, uo) is cc_multi, di, in, out, di, uo) is det.
:- mode run_test(pred(di, out, in, out) is cc_multi, di, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred run_test(pred(T, State, State), T, maybe.maybe_error, State, State)
    <= compare(T).
:- mode run_test(pred(out, di, uo) is det, in, out, di, uo) is det.
:- mode run_test(pred(out, in, out) is det, in, out, in, out) is det.
:- mode run_test(pred(out, di, uo) is cc_multi, in, out, di, uo) is det.
:- mode run_test(pred(out, in, out) is cc_multi, in, out, in, out) is det.

%-----------------------------------------------------------------------------%
% Tests a pred that results in an io.res. If the result is io.error, then that
% error is transformed into the maybe_error result using io.error_message.
% If the result is io.ok, the value is tested against the expected result as
% usual. 
:- pred run_io_test(pred(A, io.res(B), State, State), A, B, maybe.maybe_error, State, State)
    <= compare(B).
:- mode run_io_test(pred(in, out, di, uo) is det, in, in, out, di, uo) is det.
:- mode run_io_test(pred(di, out, di, uo) is det, di, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Similar to run_test/6, but supports semidet inputs.
:- pred run_backtrack_test(pred(A, B, State, State), A, B, maybe.maybe_error, State, State)
    <= compare(B).
:- mode run_backtrack_test(pred(in, out, in, out) is semidet, in, in, out, in, out) is det.
:- mode run_backtrack_test(pred(in, out, mdi, muo) is semidet, in, in, out, mdi, muo) is det.
:- mode run_backtrack_test(pred(mdi, out, in, out) is semidet, mdi, in, out, in, out) is det.
:- mode run_backtrack_test(pred(mdi, out, mdi, muo) is semidet, mdi, in, out, mdi, muo) is det.
:- mode run_backtrack_test(pred(in, out, in, out) is cc_nondet, in, in, out, in, out) is det.
:- mode run_backtrack_test(pred(in, out, mdi, muo) is cc_nondet, in, in, out, mdi, muo) is det.
:- mode run_backtrack_test(pred(mdi, out, in, out) is cc_nondet, mdi, in, out, in, out) is det.
:- mode run_backtrack_test(pred(mdi, out, mdi, muo) is cc_nondet, mdi, in, out, mdi, muo) is det.

:- pred run_backtrack_test(pred(T, State, State), T, maybe.maybe_error, State, State)
    <= compare(T).
:- mode run_backtrack_test(pred(out, in, out) is semidet, in, out, in, out) is det.
:- mode run_backtrack_test(pred(out, mdi, muo) is semidet, in, out, mdi, muo) is det.
:- mode run_backtrack_test(pred(out, in, out) is cc_nondet, in, out, in, out) is det.
:- mode run_backtrack_test(pred(out, mdi, muo) is cc_nondet, in, out, mdi, muo) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

%-----------------------------------------------------------------------------%

run_test(Pred, In, Out, Result) :-
    ( if
        promise_equivalent_solutions [Mid] (
            Pred(In, Mid)
        )
    then
        compare(Mid, Out) = Result
    else
        Result = maybe.error("Pred failed")
    ).

%-----------------------------------------------------------------------------%

run_test(Pred, In, Out, compare(Mid, Out), !State) :-
    promise_equivalent_solutions [!:State, Mid] (
        Pred(In, Mid, !State)
    ).

%-----------------------------------------------------------------------------%

run_test(Pred, Out, compare(Mid, Out), !State) :-
    promise_equivalent_solutions [!:State, Mid] (
        Pred(Mid, !State)
    ).

%-----------------------------------------------------------------------------%

run_io_test(Pred, In, Out, Result, !IO) :-
    Pred(In, MidResult, !IO),
    (
        MidResult = io.error(E),
        Result = maybe.error(io.error_message(E))
    ;
        MidResult = io.ok(Mid),
        Result = compare(Mid, Out)
    ).

%-----------------------------------------------------------------------------%

run_backtrack_test(Pred, In, Out, Result, StateIn, StateOut) :-
    ( if
        promise_equivalent_solutions [Mid, StateMid] (
            Pred(In, Mid, StateIn, StateMid)
        )
    then
        StateOut = StateMid,
        compare(Mid, Out) = Result
    else
        StateOut = StateIn,
        Result = maybe.error("Pred failed")
    ).

%-----------------------------------------------------------------------------%

run_backtrack_test(Pred, Out, Result, StateIn, StateOut) :-
    ( if
        promise_equivalent_solutions [Mid, StateMid] (
            Pred(Mid, StateIn, StateMid)
        )
    then
        StateOut = StateMid,
        compare(Mid, Out) = Result
    else
        StateOut = StateIn,
        Result = maybe.error("Pred failed")
    ).

