-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H | T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H | T]) -> [H - 1 | decrement(T)].

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

greet(Name) ->
    io:format("Your name is ~s.~n", [Name]),
    fun(Greeting) -> io:format("~s, ~s. It's really nice to meet you", [Greeting, Name]) end.
