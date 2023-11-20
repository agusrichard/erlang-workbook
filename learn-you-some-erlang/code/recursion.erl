-module(recursion).
-export([fac/1, sum/1, len/1, last/1, tail_fac/1]).

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
