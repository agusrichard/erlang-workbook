# Recursion

Source: https://learnyousomeerlang.com/recursion

### Hello recursion!

- Truth is, functional programming languages usually do not offer looping constructs like for and while. Instead, functional programmers rely on a silly concept named recursion.
- Factorial:

  ```erlang
  -module(recursive).
  -export([fac/1]).

  fac(N) when N == 0 -> 1;
  fac(N) when N > 0  -> N*fac(N-1).
  ```
