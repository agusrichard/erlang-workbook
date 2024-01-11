-module(useless).
-export([add/2, hello/0, greet_and_add_two/1, greet_damara/0]).

add(A, B) -> A + B.

hello() -> io:format("Hello World!-n").

greet_and_add_two(X) ->
    hello(),
    add(X, 2).

greet_damara() -> io:format("I love Damara indeed!").
