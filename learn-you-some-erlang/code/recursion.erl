-module(recursion).
-export([fac/1]).

fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N * fac(N - 1).