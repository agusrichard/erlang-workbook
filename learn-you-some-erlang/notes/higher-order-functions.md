# Higher Order Functions

Source: https://learnyousomeerlang.com/higher-order-functions#get-functional

### Let's get functional

- A function that can accept other functions transported around that way is named a higher order function.
- Example:

  ```erlang
  -module(hhfuns).
  -compile(export_all).

  one() -> 1.
  two() -> 2.

  add(X,Y) -> X() + Y().
  ```

- Running:
  ```erlang
  1> c(hhfuns).
  {ok, hhfuns}
  2> hhfuns:add(one,two).
  ** exception error: bad function one
  in function  hhfuns:add/2
  3> hhfuns:add(1,2).
  ** exception error: bad function 1
  in function  hhfuns:add/2
  4> hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
  3
  ```
- This is what fun Module:Function/Arity is: it tells the VM to use that specific function, and then bind it to a variable.
- We'll add a few functions to hhfuns that work recursively over a list to add or subtract one from each integer of a list:

  ```erlang
  increment([]) -> [];
  increment([H|T]) -> [H+1|increment(T)].

  decrement([]) -> [];
  decrement([H|T]) -> [H-1|decrement(T)].
  ```

- We'll abstract all the similar parts in a single function (map/2) that will take another function as an argument:

  ```erlang
  map(_, []) -> [];
  map(F, [H|T]) -> [F(H)|map(F,T)].

  incr(X) -> X + 1.
  decr(X) -> X - 1.
  ```

- Tested in the shell:
  ```erlang
  1> c(hhfuns).
  {ok, hhfuns}
  2> L = [1,2,3,4,5].
  [1,2,3,4,5]
  3> hhfuns:increment(L).
  [2,3,4,5,6]
  4> hhfuns:decrement(L).
  [0,1,2,3,4]
  5> hhfuns:map(fun hhfuns:incr/1, L).
  [2,3,4,5,6]
  6> hhfuns:map(fun hhfuns:decr/1, L).
  [0,1,2,3,4]
  ```

### Anonymous functions

- Anonymous functions, or funs, address that problem by letting you declare a special kind of function inline, without naming them.
- They can do pretty much everything normal functions can do, except calling themselves recursively (how could they do it if they are anonymous?)
- Their syntax is:

```erlang
  fun(Args1) ->
  Expression1, Exp2, ..., ExpN;
  (Args2) ->
  Expression1, Exp2, ..., ExpN;
  (Args3) ->
  Expression1, Exp2, ..., ExpN
  end
```

- Example:
  ```erlang
  7> Fn = fun() -> a end.
  #Fun<erl_eval.20.67289768>
  8> Fn().
  a
  9> hhfuns:map(fun(X) -> X + 1 end, L).
  [2,3,4,5,6]
  10> hhfuns:map(fun(X) -> X - 1 end, L).
  [0,1,2,3,4]
  ```
- Anonymous functions are already pretty dandy for such abstractions but they still have more hidden powers:
  ```erlang
  1> PrepareAlarm = fun(Room) ->
  11>                     io:format("Alarm set in ~s.~n",[Room]),
  11>                     fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
  11>                   end.
  #Fun<erl_eval.20.67289768>
  12> AlarmReady = PrepareAlarm("bathroom").
  Alarm set in bathroom.
  #Fun<erl_eval.6.13229925>
  13> AlarmReady().
  Alarm tripped in bathroom! Call Batman!
  ok
  ```
- Hold the phone Batman! What's going on here? Well, first of all, we declare an anonymous function assigned to PrepareAlarm. This function has not run yet: it only gets executed when PrepareAlarm("bathroom"). is called. At that point, the call to io:format/2 is evaluated and the "Alarm set" text is output. The second expression (another anonymous function) is returned to the caller and then assigned to AlarmReady. Note that in this function, the variable Room's value is taken from the 'parent' function (PrepareAlarm). This is related to a concept called closures.
- To understand closures, one must first understand scope. A function's scope can be imagined as the place where all the variables and their values are stored.
- In the function base(A) -> B = A + 1., A and B are both defined to be part of base/1's scope. This means that anywhere inside base/1, you can refer to A and B and expect a value to be bound to them. And when I say 'anywhere', I ain't kidding, kid; this includes anonymous functions too:
  ```erlang
  base(A) ->
  B = A + 1,
  F = fun() -> A * B end,
  F().
  ```
- B and A are still bound to base/1's scope, so the function F can still access them. This is because F inherits base/1's scope. Like most kinds of real-life inheritance, the parents can't get what the children have:
  ```erlang
  base(A) ->
  B = A + 1,
  F = fun() -> C = A * B end,
  F(),
  C.
  ```
- In this version of the function, B is still equal to A + 1 and F will still execute fine. However, the variable C is only in the scope of the anonymous function in F. When base/1 tries to access C's value on the last line, it only finds an unbound variable. In fact, had you tried to compile this function, the compiler would have thrown a fit. Inheritance only goes one way.
- It is important to note that the inherited scope follows the anonymous function wherever it is, even when it is passed to another function:

  ```erlang
  a() ->
  Secret = "pony",
  fun() -> Secret end.

  b(F) ->
  "a/0's password is "++F()
  ```

- Compiled:
  ```erlang
  14> c(hhfuns).
  {ok, hhfuns}
  15> hhfuns:b(hhfuns:a()).
  "a/0's password is pony"
  ```
- You're most likely to use anonymous functions to carry state around when you have functions defined that take many arguments, but you have a constant one:
  ```erlang
  16> math:pow(5,2).
  25.0
  17> Base = 2.
  2
  18> PowerOfTwo = fun(X) -> math:pow(Base,X) end.
  #Fun<erl_eval.6.13229925>
  17> hhfuns:map(PowerOfTwo, [1,2,3,4]).
  [2.0,4.0,8.0,16.0]
  ```
- A little trap you might fall into when writing anonymous functions is when you try to redefine the scope:
  ```erlang
  base() ->
  A = 1,
  (fun() -> A = 2 end)().
  ```
- This will declare an anonymous function and then run it. As the anonymous function inherits base/0's scope, trying to use the = operator compares 2 with the variable A (bound to 1). This is guaranteed to fail. However it is possible to redefine the variable if it's done in the nested function's head:
  ```erlang
  base() ->
  A = 1,
  (fun(A) -> A = 2 end)(2).
  ```
- And this works. If you try to compile it, you'll get a warning about shadowing ("Warning: variable 'A' shadowed in 'fun'"). Shadowing is the term used to describe the act of defining a new variable that has the same name as one that was in the parent scope. This is there to prevent some mistakes (usually rightly so), so you might want to consider renaming your variables in these circumstances.
- Starting with version 17.0, the language supports using anonymous functions with an internal name. That's right, anonymous but named functions.
- The trick is that the name is visible only within the function's scope, not outside of it. The main advantage of this is that it makes it possible to define anonymous recursive functions. For example, we could make an anonymous function that keeps being loud forever:
  ```erlang
  18> f(PrepareAlarm), f(AlarmReady).
  ok
  19> PrepareAlarm = fun(Room) ->
  19>    io:format("Alarm set in ~s.~n",[Room]),
  19>     fun Loop() ->
  19>        io:format("Alarm tripped in ~s! Call Batman!~n",[Room]),
  19>        timer:sleep(500),
  19>         Loop()
  19>     end
  19> end.
  #Fun<erl_eval.6.71889879>
  20> AlarmReady = PrepareAlarm("bathroom").
  Alarm set in bathroom.
  #Fun<erl_eval.44.71889879>
  21> AlarmReady().
  Alarm tripped in bathroom! Call Batman!
  Alarm tripped in bathroom! Call Batman!
  Alarm tripped in bathroom! Call Batman!
  ...
  ```

### Maps, filters, folds and more

- Two more functions

  ```erlang
  %% only keep even numbers
  even(L) -> lists:reverse(even(L,[])).

  even([], Acc) -> Acc;
  even([H|T], Acc) when H rem 2 == 0 ->
  even(T, [H|Acc]);
  even([_|T], Acc) ->
  even(T, Acc).

  %% only keep men older than 60
  old_men(L) -> lists:reverse(old_men(L,[])).

  old_men([], Acc) -> Acc;
  old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
  old_men(People, [Person|Acc]);
  old_men([_|People], Acc) ->
  old_men(People, Acc).
  ```

- Generalization:

  ```erlang
  filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).

  filter(_, [], Acc) -> Acc;
  filter(Pred, [H|T], Acc) ->
  case Pred(H) of
  true  -> filter(Pred, T, [H|Acc]);
  false -> filter(Pred, T, Acc)
  end.
  ```

- Compiled:

  ```erlang
  1> c(hhfuns).
  {ok, hhfuns}
  2> Numbers = lists:seq(1,10).
  [1,2,3,4,5,6,7,8,9,10]
  3> hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).
  [2,4,6,8,10]
  4> People = [{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}].
  [{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}]
  5> hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).
  [{male,66},{male,74}]
  ```

- In the previous chapter, another kind of recursive manipulation we applied on lists was to look at every element of a list one after the other and reduce them to a single answer. This is called a fold and can be used on the following functions:

  ```erlang
  %% find the maximum of a list
  max([H|T]) -> max2(T, H).

  max2([], Max) -> Max;
  max2([H|T], Max) when H > Max -> max2(T, H);
  max2([_|T], Max) -> max2(T, Max).

  %% find the minimum of a list
  min([H|T]) -> min2(T,H).

  min2([], Min) -> Min;
  min2([H|T], Min) when H < Min -> min2(T,H);
  min2([_|T], Min) -> min2(T, Min).

  %% sum of all the elements of a list
  sum(L) -> sum(L,0).

  sum([], Sum) -> Sum;
  sum([H|T], Sum) -> sum(T, H+Sum).
  ```

- Fold function:
  ```erlang
  fold(_, Start, []) -> Start;
  fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).
  ```
- Compiled:

  ```erlang
  6> c(hhfuns).
  {ok, hhfuns}
  7> [H|T] = [1,7,3,5,9,0,2,3].
  [1,7,3,5,9,0,2,3]
  8> hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
  9
  9> hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
  0
  10> hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
  21
  ```

- Pretty much any function you can think of that reduces lists to 1 element can be expressed as a fold.
- What's funny there is that you can represent an accumulator as a single element (or a single variable), and an accumulator can be a list. Therefore, we can use a fold to build a list. This means fold is universal in the sense that you can implement pretty much any other recursive function on lists with a fold, even map and filter:

  ```erlang
  reverse(L) ->
  fold(fun(X,Acc) -> [X|Acc] end, [], L).

  map2(F,L) ->
  reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

  filter2(Pred, L) ->
  F = fun(X,Acc) ->
  case Pred(X) of
  true  -> [X|Acc];
  false -> Acc
  end
  end,
  reverse(fold(F, [], L)).
  ```
