# Functionally Solving Problems

Source: https://learnyousomeerlang.com/functionally-solving-problems#rpn-calculator

### Reverse Polish Notation Calculator

- Polish notation: (2 + 2) / 5 would become (/ (+ 2 2) 5)
- If we decide to say + and / always take two arguments, then (/ (+ 2 2) 5) can simply be written as / + 2 2 5.
- The same example as above in RPN would be written 2 2 + 5 /. Other example expressions could be 9 _ 5 + 7 or 10 _ 2 _ (3 + 4) / 2 which get translated to 9 5 _ 7 + and 10 2 _ 3 4 + _ 2 /, respectively.
- Parse the tokens:
  ```erlang
  1> string:tokens("10 4 3 + 2 * -", " ").
  ["10","4","3","+","2","*","-"]
  ```
- Expression:

  ```erlang
  -module(calc).
  -export([rpn/1]).

  rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.
  ```

- The whole code:

```erlang
-module(calc).
-export([rpn/2]).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, string:tokens(L, " ")),
    Res.

read(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.

rpn("+", [N1, N2 | S]) -> [N2 + N1 | S];
rpn("-", [N1, N2 | S]) -> [N2 - N1 | S];
rpn("*", [N1, N2 | S]) -> [N2 * N1 | S];
rpn("/", [N1, N2 | S]) -> [N2 / N1 | S];
rpn("^", [N1, N2 | S]) -> [math:pow(N2, N1) | S];
rpn("ln", [N | S]) -> [math:log(N) | S];
rpn("log10", [N | S]) -> [math:log10(N) | S];
rpn(X, Stack) -> [read(X) | Stack].
```

- Given Erlang's policy is to let it crash, it's what was chosen here.

### Heathrow to London

- Code:

  ```erlang
  -module(road).
  -compile(export_all).

  main([FileName]) ->
  {ok, Bin} = file:read_file(FileName),
  Map = parse_map(Bin),
  io:format("~p~n",[optimal_path(Map)]),
  erlang:halt(0).

  %% Transform a string into a readable map of triples
  parse_map(Bin) when is_binary(Bin) ->
  parse_map(binary_to_list(Bin));
  parse_map(Str) when is_list(Str) ->
  Values = [list_to_integer(X) || X <- string:tokens(Str,"\r\n\t ")],
  group_vals(Values, []).

  group_vals([], Acc) ->
  lists:reverse(Acc);
  group_vals([A,B,X|Rest], Acc) ->
  group_vals(Rest, [{A,B,X} | Acc]).

  %% Picks the best of all paths, woo!
  optimal_path(Map) ->
  {A,B} = lists:foldl(fun shortest_step/2, {{0,[]}, {0,[]}}, Map),
  {_Dist,Path} = if hd(element(2,A)) =/= {x,0} -> A;
  hd(element(2,B)) =/= {x,0} -> B
  end,
  lists:reverse(Path).

  %% actual problem solving
  %% change triples of the form {A,B,X}
  %% where A,B,X are distances and a,b,x are possible paths
  %% to the form {DistanceSum, PathList}.
  shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
  OptA1 = {DistA + A, [{a,A}|PathA]},
  OptA2 = {DistB + B + X, [{x,X}, {b,B}|PathB]},
  OptB1 = {DistB + B, [{b,B}|PathB]},
  OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]},
  {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.
  ```
