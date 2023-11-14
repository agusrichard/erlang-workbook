-module(functions).
-compile(export_all).

greet(male, Name) -> io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) -> io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) -> io:format("Hello, ~s!", [Name]).

head([X | _]) -> X.
second([_, X | _]) -> X.

same(X, X) -> true;
same(_, _) -> false.

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n", [Date, Y, M, D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time, H, Min, S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").

old_enough(X) when X >= 17 -> true;
old_enough(_) -> false.

right_age(X) when X >= 17, X =< 100 -> true;
right_age(_) -> false.

animal_talk(Animal) ->
    Talk =
        if
            Animal == cat -> "meow";
            Animal == beef -> "mooo";
            Animal == dog -> "bark";
            true -> "I don't know what to say"
        end,
    {Animal, "says" ++ Talk ++ "!"}.

is_she_hot(Name) ->
    Hotness =
        if
            Name == "Damara" -> "She's absolutely hot";
            Name == "Astiningtiyas" -> "Smoking hot indeed";
            true -> "Are you kidding me? She's the hottest of them all"
        end,
    io:format("OMG, " ++ Hotness ++ "!").

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
