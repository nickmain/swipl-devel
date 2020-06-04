:- module(test_monotonic,
          [ test_monotonic/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(tables)).

test_monotonic :-
    run_tests([ monotonic_tabling
              ]).

:- begin_tests(monotonic_tabling).

term_expansion((test(Name, Cond0) :- Body),
               (test(Name, Cond) :- Body)) :-
    (   is_list(Cond0)
    ->  Cond = [cleanup(cleanup)|Cond0]
    ;   Cond = [cleanup(cleanup),Cond0]
    ).

cleanup :-
    abolish_all_tables,
    retractall(da(_)),
    retractall(db(_)).

% Very basic test

:- dynamic da/1 as monotonic.
:- table pa/1 as monotonic.

pa(X) :- da(Y), X is Y+1.

test(pa1, X == 1) :-
    assertion(\+ pa(_)),
    assert(da(0)),
    assertion(get_returns_for_call(pa(_), pa(1))),
    pa(X).
test(pa2, X == 1) :-
    assert(da(0), Ref),
    pa(X),
    erase(Ref),
    assertion(\+ pa(_)).

% Combining monotonic and incremental

:- dynamic db/1 as (incremental,monotonic).
:- table pb/1 as monotonic.
:- table qb/1 as incremental.

pb(X) :- db(Y), X is Y+1.
qb(X) :- db(Y), X is Y+1.

test(pb, true) :-
    assertion(\+ pb(_)),
    assertion(\+ qb(_)),
    assert(db(0)),
    assertion((pb(X), X == 1)),
    assertion((qb(X), X == 1)).

:- end_tests(monotonic_tabling).
