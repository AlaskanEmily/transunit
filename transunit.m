%   Copyright (C) 2018-2019 Alaskan Emily, Transnat Games
%
%   This software is provided 'as-is', without any express or implied
%   warranty.  In no event will the authors be held liable for any damages
%   arising from the use of this software.
%
%   Permission is granted to anyone to use this software for any purpose,
%   including commercial applications, and to alter it and redistribute it
%   freely, subject to the following restrictions:
%
%   1. The origin of this software must not be misrepresented; you must not
%      claim that you wrote the original software. If you use this software
%      in a product, an acknowledgment in the product documentation would be
%      appreciated but is not required.
%   2. Altered source versions must be plainly marked as such, and must not be
%      misrepresented as being the original software.
%  3. This notice may not be removed or altered from any source distribution.

:- module transunit.

%==============================================================================%
% Transnat Games Unit Testing Framework
% Provides some utilities to run unit tests on Mercury code.
% This is primarily meant to test purely functional code, preds with behavior
% strongly dependent on typeclasses, and parser/converter style code.
%
% Feathers in our cap:
%  - mock framework which makes it fairly easy to mock typeclasses
%  - Result comparison with acceptable error reporting
% Black eyes:
%  - No exception handling whatsoever
%  - No multi-process, meaning badly behaved native code can poison the well.
%
% But there are basically no other Mercury unit test frameworks to start with,
% so at least this gives us a foundation.
:- interface.
%==============================================================================%

:- include_module transunit.compare.
:- include_module transunit.compare_test.
:- include_module transunit.mock.

:- use_module maybe.
:- import_module list.
:- use_module io.
:- use_module unit.

:- typeclass to_string(T) where [
    func to_string(T) = string   
].

:- typeclass compare(T) where [
    func compare(T, T) = maybe.maybe_error
].

%-----------------------------------------------------------------------------%

% run_test(TestPred, TestName, !IO)
% Runs a pred, reporting success or failure depending on if it is true of not.
% The input pred's parameter is a dummy parameter.
:- pred run_test(pred(unit.unit), string, io.io, io.io).
:- mode run_test(pred(in) is semidet, in, di, uo) is det.

%-----------------------------------------------------------------------------%
% run_result_test(TestPred, TestName, !IO)
% Runs a pred, reporting success or failure depending on the maybe_error which
% is produced.
% This is intended to be run with preds from the compare_test module as the
% test preds.
% See run_result_test/6 for using input preds that use a different state
% variable type than io.io.
:- pred run_result_test(pred(maybe.maybe_error, io.io, io.io),
    string,
    io.io, io.io).
:- mode run_result_test(pred(out, di, uo) is det,
    in,
    di, uo) is det.

%-----------------------------------------------------------------------------%
% run_result_test(TestPred, TestName, !State, !IO)
% Runs a pred, reporting success or failure depending on the maybe_error which
% is produced.
% This does not share the IO state as run_result_test/4 does.
:- pred run_result_test(pred(maybe.maybe_error, State, State),
    string,
    State, State,
    io.io, io.io).
:- mode run_result_test(pred(out, in, out) is det,
    in,
    in, out,
    di, uo) is det.
:- mode run_result_test(pred(out, di, uo) is det,
    in,
    di, uo,
    di, uo) is det.
:- mode run_result_test(pred(out, mdi, muo) is det,
    in,
    mdi, muo,
    di, uo) is det.

%-----------------------------------------------------------------------------%

:- func test_result(maybe.maybe_error, string) = string.

%-----------------------------------------------------------------------------%

:- func success_message = string.
:- func failure_message = string.

%==============================================================================%
:- implementation.
%==============================================================================%

:- use_module string.

%-----------------------------------------------------------------------------%

run_test(Pred, Name, !IO) :-
    ( Pred(unit.unit) -> Result = success_message ; Result = failure_message ),
    io.write_string(Result, !IO),
    io.write_char((' '), !IO),
    io.write_line(Name, !IO).

%-----------------------------------------------------------------------------%

run_result_test(Pred, Name, !IO) :-
    Pred(Result, !IO),
    io.write_line(test_result(Result, Name), !IO).

%-----------------------------------------------------------------------------%

run_result_test(Pred, Name, !State, !IO) :-
    Pred(Result, !State),
    io.write_line(test_result(Result, Name), !IO).

%-----------------------------------------------------------------------------%

test_result(maybe.ok, Name) = string.append(success_message, Suffix) :-
    string.first_char(Suffix, (' '), Name).

test_result(maybe.error(E), Name) = string.append(failure_message, Suffix) :-
    string.first_char(Start, (' '), Name),
    string.first_char(Rest, ('\n'), E),
    string.append(Start, Rest, Suffix).

%-----------------------------------------------------------------------------%

success_message = "Success ...".
failure_message = "Failure !!!".

