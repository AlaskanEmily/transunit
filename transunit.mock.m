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

:- module transunit.mock.

%=============================================================================%
% Implementation of a basic mock type. This is useful for implementing a
% typeclass you want to test, giving known results for typeclass preds/funcs.
:- interface.
%=============================================================================%

:- import_module list.
:- use_module array.
:- use_module maybe.
:- use_module io.

%-----------------------------------------------------------------------------%
% A mock represents a list of results that can be given as output.
% It is operationally equivalent to a list.
:- type mock(T).

%-----------------------------------------------------------------------------%
% Determines if a mock will repeat its contents or not.
:- type mock_option ---> repeat ; oneshot.

%-----------------------------------------------------------------------------%
% Determines if the mock considers any form of contention to be a fatal error.
% not_threadsafe will avoid a deadlock in incorrect usage of state_mocks, but
% is not suitable for multi-threaded use.
:- type state_option ---> threadsafe ; not_threadsafe.

%-----------------------------------------------------------------------------%
% state_mock's use IO instead of updates to change their inputs.
:- type state_mock(T).

%-----------------------------------------------------------------------------%
% maybe_mock's hold maybe's of values. Unlike a regular mock which always
% yields results until it is empty, a maybe mock will fail to yield when the
% item it would yield is maybe.no. This also unwraps the maybe's.
:- type maybe_mock(T) == mock(maybe.maybe(T)).

%-----------------------------------------------------------------------------%
% maybe_state_mock are a combination of state_mock and maybe_mock.
:- type maybe_state_mock(T) == state_mock(maybe.maybe(T)).

%-----------------------------------------------------------------------------%
% Creates a oneshot mock from a list of inputs.
:- func init_mock(list(T)) = mock(T).

%-----------------------------------------------------------------------------%
% Creates a mock from a list of inputs.
:- func init_mock(mock_option, list(T)) = mock(T).

%-----------------------------------------------------------------------------%
% Creates a oneshot mock from an array of inputs.
:- func init_mock_from_array(array.array(T)) = mock(T).

%-----------------------------------------------------------------------------%
% Creates a mock from an array of inputs.
:- func init_mock_from_array(mock_option, array.array(T)) = mock(T).

%-----------------------------------------------------------------------------%
% Creates a oneshot state_mock from a list of inputs.
:- pred init_state_mock(state_option,
    list(T),
    state_mock(T),
    io.io, io.io).
:- mode init_state_mock(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Creates a state_mock from a list of inputs.
:- pred init_state_mock(state_option,
    mock_option,
    list(T),
    state_mock(T),
    io.io, io.io).
:- mode init_state_mock(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Creates a state_mock from an array of inputs.
:- pred init_state_mock_from_array(state_option,
    mock_option,
    array.array(T),
    state_mock(T),
    io.io, io.io).
:- mode init_state_mock_from_array(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Creates a oneshot state_mock from an array of inputs.
:- pred init_state_mock_from_array(state_option,
    array.array(T),
    state_mock(T),
    io.io, io.io).
:- mode init_state_mock_from_array(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Removes the first item from the mock, updating the mock. If the mock was
% empty then it is left as-is.
% Yields maybe.no if no items remain. Otherwise yields maybe.yes of the item.
:- pred mock_retrieve(maybe.maybe(T), mock(T), mock(T)).
:- mode mock_retrieve(out, in, out) is det.

%-----------------------------------------------------------------------------%
% Removes the first item from the mock, updating the mock. If the mock was
% empty then it is left as-is.
% Yields maybe.no if no items remain or the item was maybe.no. Yields the item
% if it is a maybe.yes.
:- pred maybe_mock_retrieve(maybe.maybe(T), maybe_mock(T), maybe_mock(T)).
:- mode maybe_mock_retrieve(out, in, out) is det.

%-----------------------------------------------------------------------------%
% Removes the first item from the mock, updating the mock. Fails if the mock
% was empty.
% Yields maybe.no if no items remain. Otherwise yields maybe.yes of the item.
:- pred mock_try_retrieve(T, mock(T), mock(T)).
:- mode mock_try_retrieve(out, in, out) is semidet.

%-----------------------------------------------------------------------------%
% Removes the first item from the mock, updating the mock. If the mock was
% empty then it is left as-is.
% Yields maybe.no if no items remain. Otherwise yields maybe.yes of the item.
:- pred state_mock_retrieve(state_mock(T), maybe.maybe(T), io.io, io.io).
:- mode state_mock_retrieve(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Removes the first item from the mock, updating the mock with IO state. If the
% mock was empty then it is left as-is.
% Yields maybe.no if no items remain or the item was maybe.no. Yields the item
% if it is a maybe.yes.
:- pred maybe_state_mock_retrieve(maybe_state_mock(T), maybe.maybe(T), io.io, io.io).
:- mode maybe_state_mock_retrieve(in, out, di, uo) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module exception.
:- use_module int.
:- use_module thread.
:- use_module std_util.
:- use_module thread.mvar.

%-----------------------------------------------------------------------------%

:- type mock(T) --->
    repeat(array.array(T), int) ;
    oneshot(list(T)).

:- type state_mock(T) ---> state_mock(thread.mvar.mvar(mock(T)), state_option).

%-----------------------------------------------------------------------------%

init_mock(List) = oneshot(List).

%-----------------------------------------------------------------------------%

init_mock(oneshot, List) = oneshot(List).
init_mock(repeat, []) = oneshot([]).
init_mock(repeat, List) = repeat(array.array(List), 0) :- List = [_|_].

%-----------------------------------------------------------------------------%

init_mock_from_array(A) = init_mock(array.to_list(A)).

%-----------------------------------------------------------------------------%

init_mock_from_array(oneshot, A) = oneshot(array.to_list(A)).
init_mock_from_array(repeat, A) = Result :-
    array.bounds(A, Low, High),
    builtin.compare(Cmp, Low, High),
    (
        Cmp = (=), Result = oneshot([])
    ;
        Cmp = (<), Result = repeat(array.copy(A), Low)
    ;
        Cmp = (>),
        exception.throw(exception.software_error("Invalid array bounds"))
    ).

%-----------------------------------------------------------------------------%

init_state_mock(Option, List, state_mock(Mvar, Option), !IO) :-
    thread.mvar.init(init_mock(List), Mvar, !IO).

%-----------------------------------------------------------------------------%

init_state_mock(Option, MockOption, List, state_mock(Mvar, Option), !IO) :-
    thread.mvar.init(init_mock(MockOption, List), Mvar, !IO).

%-----------------------------------------------------------------------------%

init_state_mock_from_array(Option, A, state_mock(Mvar, Option), !IO) :-
    thread.mvar.init(init_mock_from_array(A), Mvar, !IO).

%-----------------------------------------------------------------------------%

init_state_mock_from_array(Option, MockOption, A, state_mock(Mvar, Option), !IO) :-
    thread.mvar.init(init_mock_from_array(MockOption, A), Mvar, !IO).

%-----------------------------------------------------------------------------%

:- pred repeat_mock_retrieve(array.array(T), int, T, mock(T)).
:- mode repeat_mock_retrieve(in, in, out, out) is det.

repeat_mock_retrieve(A, I, T, repeat(A, N)) :-
    array.unsafe_lookup(A, I, T),
    array.bounds(A, Low, High),
    M = int.plus(I, 1),
    builtin.compare(Cmp, M, High),
    (
        Cmp = (=), N = Low
    ;
        Cmp = (<), N = M
    ;
        Cmp = (>),
        exception.throw(exception.software_error("Invalid repeat index"))
    ).

%-----------------------------------------------------------------------------%

mock_retrieve(maybe.no, oneshot([]), oneshot([])).
mock_retrieve(maybe.yes(T), oneshot([T|List]), oneshot(List)).
mock_retrieve(maybe.yes(T), repeat(A, I), Out) :-
    repeat_mock_retrieve(A, I, T, Out).

%-----------------------------------------------------------------------------%

maybe_mock_retrieve(maybe.no, oneshot([]), oneshot([])).
maybe_mock_retrieve(T, oneshot([T|List]), oneshot(List)).
maybe_mock_retrieve(T, repeat(A, I), Out) :-
    repeat_mock_retrieve(A, I, T, Out).

%-----------------------------------------------------------------------------%

mock_try_retrieve(T, oneshot([T|List]), oneshot(List)).
mock_try_retrieve(T, In, Out) :-
    require_det (
        In = repeat(A, I),
        repeat_mock_retrieve(A, I, T, Out)
    ).

%-----------------------------------------------------------------------------%
% Gets the mock from a state_mock.
% - In threadsafe mode this will block for the mock to be available.
% - In not-threadsafe mode this will error if the mock is not available.
:- pred get_mock(state_mock(T), thread.mvar.mvar(mock(T)), mock(T), io.io, io.io).
:- mode get_mock(in, out, out, di, uo) is det.

get_mock(state_mock(Mvar, not_threadsafe), Mvar, Mock, !IO) :-
    thread.mvar.try_take(Mvar, MaybeMock, !IO),
    (
        MaybeMock = maybe.yes(Mock)
    ;
        % This indicates a horrible mistake was made somewhere.
        MaybeMock = maybe.no,
        exception.throw(exception.software_error("Invalid state in state_mock"))
    ).
get_mock(state_mock(Mvar, threadsafe), Mvar, Mock, !IO) :-
    thread.mvar.take(Mvar, Mock, !IO).

%-----------------------------------------------------------------------------%

state_mock_retrieve(StateMock, Out, !IO) :-
    get_mock(StateMock, Mvar, MockIn, !IO),
    mock_retrieve(Out, MockIn, MockOut),
    thread.mvar.put(Mvar, MockOut, !IO).

%-----------------------------------------------------------------------------%

maybe_state_mock_retrieve(StateMock, Out, !IO) :-
    get_mock(StateMock, Mvar, MockIn, !IO),
    maybe_mock_retrieve(Out, MockIn, MockOut),
    thread.mvar.put(Mvar, MockOut, !IO).
