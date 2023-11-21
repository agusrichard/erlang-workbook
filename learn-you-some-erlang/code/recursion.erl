-module(recursion).
-export([
    fac/1,
    sum/1,
    len/1,
    last/1,
    tail_fac/1,
    duplicate/2,
    tail_duplicate/2,
    reverse/1,
    tail_reverse/1,
    sublist/2,
    tail_sublist/2,
    zip/2,
    lenient_zip/2,
    tail_zip/2,
    tail_lenient_zip/2
]).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

sum(0) -> 0;
sum(N) when N > 0 -> N + sum(N - 1).

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

last([X]) -> X;
last([_ | T]) -> last(T).

tail_fac(N) -> tail_fac(N, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).

duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 -> [Term | duplicate(N - 1, Term)].

tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).
tail_duplicate(0, _, List) -> List;
tail_duplicate(N, Term, List) when N > 0 -> tail_duplicate(N - 1, Term, [Term | List]).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

tail_reverse(L) -> tail_reverse(L, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([H | T], Acc) -> tail_reverse(T, [H | Acc]).

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H | T], N) when N > 0 -> [H | sublist(T, N - 1)].

tail_sublist(L, N) -> tail_sublist(L, N, []).
tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], _, Acc) -> Acc;
tail_sublist([H | T], N, Acc) when N > 0 -> tail_sublist(T, N - 1, Acc ++ [H]).

zip([], []) -> [];
zip([X | Xs], [Y | Ys]) -> [{X, Y} | zip(Xs, Ys)].

lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X | Xs], [Y | Ys]) -> [{X, Y} | lenient_zip(Xs, Ys)].

tail_zip(Xs, Ys) -> tail_zip(Xs, Ys, []).
tail_zip([], [], Acc) -> Acc;
tail_zip([X | Xs], [Y | Ys], Acc) -> tail_zip(Xs, Ys, Acc ++ [{X, Y}]).

tail_lenient_zip(Xs, Ys) -> tail_lenient_zip(Xs, Ys, []).
tail_lenient_zip([], [], Acc) -> Acc;
tail_lenient_zip([], _, Acc) -> Acc;
tail_lenient_zip(_, [], Acc) -> Acc;
tail_lenient_zip([X | Xs], [Y | Ys], Acc) -> tail_lenient_zip(Xs, Ys, Acc ++ [{X, Y}]).

%% basic quicksort function.
quicksort([]) ->
    [];
quicksort([Pivot | Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
partition(Pivot, [H | T], Smaller, Larger) ->
    if
        H =< Pivot -> partition(Pivot, T, [H | Smaller], Larger);
        H > Pivot -> partition(Pivot, T, Smaller, [H | Larger])
    end.

%% quicksort built with list comprehensions rather than with a
%% partition function.
lc_quicksort([]) ->
    [];
lc_quicksort([Pivot | Rest]) ->
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot]) ++
        [Pivot] ++
        lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).

%% BESTEST QUICKSORT, YEAH!
%% (This is not really the bestest quicksort, because we do not do
%%  adequate pivot selection. It is the bestest of this book, alright?
%%  Thanks to literateprograms.org for this example. Give them a visit!
%%  http://en.literateprograms.org/Quicksort_(Erlang) )
bestest_qsort([]) -> [];
bestest_qsort(L = [_ | _]) -> bestest_qsort(L, []).

bestest_qsort([], Acc) -> Acc;
bestest_qsort([Pivot | Rest], Acc) -> bestest_partition(Pivot, Rest, {[], [Pivot], []}, Acc).

bestest_partition(_, [], {Smaller, Equal, Larger}, Acc) ->
    bestest_qsort(Smaller, Equal ++ bestest_qsort(Larger, Acc));
bestest_partition(Pivot, [H | T], {Smaller, Equal, Larger}, Acc) ->
    if
        H < Pivot ->
            bestest_partition(Pivot, T, {[H | Smaller], Equal, Larger}, Acc);
        H > Pivot ->
            bestest_partition(Pivot, T, {Smaller, Equal, [H | Larger]}, Acc);
        H == Pivot ->
            bestest_partition(Pivot, T, {Smaller, [H | Equal], Larger}, Acc)
    end.
