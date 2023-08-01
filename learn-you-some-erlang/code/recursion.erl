-module(recursion).
-export([fac/1, len/1, summ/1, tail_fac/1, tail_len/1, tail_summ/1]).

fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

summ(N) when N == 1 -> 1;
summ(N) when N > 1 -> N + summ(N - 1).

tail_fac(N) -> tail_fac(N, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) -> tail_fac(N - 1, N * Acc).

tail_len(T) -> tail_len(T, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).

tail_summ(N) -> tail_summ(N, 0).
tail_summ(0, Acc) -> Acc;
tail_summ(N, Acc) -> tail_summ(N - 1, Acc + N).
