-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment(L) -> increment(L, []).
increment([], Acc) -> Acc;
increment([H | T], Acc) -> increment(T, Acc ++ [H + 1]).

decrement(L) -> decrement(L, []).
decrement([], Acc) -> Acc;
decrement([H | T], Acc) -> decrement(T, Acc ++ [H - 1]).

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

tail_map(F, L) -> tail_map(F, L, []).
tail_map(_, [], Acc) -> Acc;
tail_map(F, [H | T], Acc) -> tail_map(F, T, Acc ++ [F(H)]).

incr(X) -> X + 1.
decr(X) -> X - 1.

tell_damara() ->
    fun(Compliment) ->
        io:format("She's extremely ~s.~n", [Compliment]),
        fun(Message) ->
            io:format("Yes, indeed. ~s is a good compliment for her and she's also ~s.~n", [
                Compliment, Message
            ])
        end
    end.

prepare_alarm() ->
    fun(Room) ->
        io:format("Alarm set in ~s.~n", [Room]),
        fun Loop() ->
            io:format("You're in ~s. Wakey wakey sleepy head", [Room]),
            timer:sleep(500),
            Loop()
        end
    end.

even(L) -> lists:reverse(even(L, [])).
even([], Acc) -> Acc;
even([H | T], Acc) when H rem 2 == 0 -> even(T, [H | Acc]);
even([_ | T], Acc) -> even(T, Acc).

young_men(L) -> lists:reverse(young_men(L, [])).
young_men([], Acc) ->
    Acc;
young_men([Person = {male, Age} | People], Acc) when Age < 18 ->
    young_men(People, [Person | Acc]);
young_men([_ | People], Acc) ->
    young_men(People, Acc).

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).
filter(_, [], Acc) ->
    Acc;
filter(Pred, [H | T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H | Acc]);
        false -> filter(Pred, T, Acc)
    end.

%% find the maximum of a list
max([H | T]) -> max2(T, H).

max2([], Max) -> Max;
max2([H | T], Max) when H > Max -> max2(T, H);
max2([_ | T], Max) -> max2(T, Max).

%% find the minimum of a list
min([H | T]) -> min2(T, H).

min2([], Min) -> Min;
min2([H | T], Min) when H < Min -> min2(T, H);
min2([_ | T], Min) -> min2(T, Min).

%% sum of all the elements of a list
sum(L) -> sum(L, 0).

sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, H + Sum).

fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(H, Start), T).

reverse(L) ->
    fold(fun(X, Acc) -> [X | Acc] end, [], L).

map2(F, L) ->
    reverse(fold(fun(X, Acc) -> [F(X) | Acc] end, [], L)).

filter2(Pred, L) ->
    F = fun(X, Acc) ->
        case Pred(X) of
            true -> [X | Acc];
            false -> Acc
        end
    end,
    reverse(fold(F, [], L)).
