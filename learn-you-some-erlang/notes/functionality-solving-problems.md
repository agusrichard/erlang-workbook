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
