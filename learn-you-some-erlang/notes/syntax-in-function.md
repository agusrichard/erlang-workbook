# Syntax in Functions

Source: https://learnyousomeerlang.com/syntax-in-functions#pattern-matching

### Pattern Matching

- In imperative programming a function would be like this:
  ```ruby
  function greet(Gender,Name)
  if Gender == male then
  print("Hello, Mr. %s!", Name)
  else if Gender == female then
  print("Hello, Mrs. %s!", Name)
  else
  print("Hello, %s!", Name)
  end
  ```
- In erlang it'd be like this:
  ```erlang
  greet(male, Name) ->
  io:format("Hello, Mr. ~s!", [Name]);
  greet(female, Name) ->
  io:format("Hello, Mrs. ~s!", [Name]);
  greet(_, Name) ->
  io:format("Hello, ~s!", [Name]).
  ```
- Pattern in imperative vs declarative:

  ```erlang
  function(Args)
    if X then
      Expression
    else if Y then
      Expression
    else
      Expression

  %% Erlang
  function(X) ->
    Expression;
  function(Y) ->
    Expression;
  function(_) ->
    Expression.
  ```

- Each of these function declarations is called a function clause. Function clauses must be separated by semicolons (;) and together form a function declaration.
- Note: io:format's formatting is done with the help of tokens being replaced in a string. The character used to denote a token is the tilde (~). Some tokens are built-in such as ~n, which will be changed to a line-break. Most other tokens denote a way to format data. The function call io:format("~s!~n",["Hello"]). includes the token ~s, which accepts strings and bitstrings as arguments, and ~n. The final output message would thus be "Hello!\n". Another widely used token is ~p, which will print an Erlang term in a nice way (adding in indentation and everything).
- Unbound variables are variables without any values attached to them (like our empty chair)
- Binding a variable is simply attaching a value to an unbound variable.
- In the case of Erlang, when you want to assign a value to a variable that is already bound, an error occurs unless the new value is the same as the old one.
- Back to our code: what happens when you call same(a,a) is that the first X is seen as unbound: it automatically takes the value a. Then when Erlang goes over to the second argument, it sees X is already bound. It then compares it to the a passed as the second argument and looks to see if it matches. The pattern matching succeeds and the function returns true.
- If the two values aren't the same, this will fail and go to the second function clause, which doesn't care about its arguments (when you're the last to choose, you can't be picky!) and will instead return false.
- As a rather advanced example, the following function prints a date, but only if it is formatted correctly:
  ```erlang
  valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
  valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").
  ```
- Note that it is possible to use the = operator in the function head, allowing us to match both the content inside a tuple ({Y,M,D}) and the tuple as a whole (Date).
- There is a problem though! This function could take anything for values, even text or atoms, as long as the tuples are of the form {{A,B,C}, {D,E,F}}. This denotes one of the limits of pattern matching: it can either specify really precise values such as a known number of atom, or abstract values such as the head|tail of a list, a tuple of N elements, or anything (\_ and unbound variables), etc. To solve this problem, we use guards.

### Guards, Guards!

- Start a new guards module so we can type in the "correct" solution to the driving question:
  ```erlang
  old_enough(X) when X >= 16 -> true;
  old_enough(_) -> false.
  ```
- Note that a basic rule for guard expression is they must return true to succeed. The guard will fail if it returns false or if it throws an exception.
- Snippet to use both conditions:
  ```erlang
  right_age(X) when X >= 16, X =< 104 ->
  true;
  right_age(_) ->
  false.
  ```
- The comma (,) acts in a similar manner to the operator andalso and the semicolon (;) acts a bit like orelse
- We could also represent the function the opposite way:
  ```erlang
  wrong_age(X) when X < 16; X > 104 ->
  true;
  wrong_age(_) ->
  false.
  ```
- One negative point about guards is that they will not accept user-defined functions because of side effects.
- I've compared , and ; in guards to the operators andalso and orelse. They're not exactly the same, though. The former pair will catch exceptions as they happen while the latter won't. What this means is that if there is an error thrown in the first part of the guard X >= N; N >= 0, the second part can still be evaluated and the guard might succeed; if an error was thrown in the first part of X >= N orelse N >= 0, the second part will also be skipped and the whole guard will fail.
- However (there is always a 'however'), only andalso and orelse can be nested inside guards. This means (A orelse B) andalso C is a valid guard, while (A; B), C is not. Given their different use, the best strategy is often to mix them as necessary.

### What the If!?

- To see how similar to guards the if expression is, look at the following examples:

  ```erlang
  -module(what_the_if).
  -export([heh_fine/0]).


  heh_fine() ->
  if 1 =:= 1 ->
  works
  end,
  if 1 =:= 2; 1 =:= 1 ->
  works
  end,
  if 1 =:= 2, 1 =:= 1 ->
  fails
  end.
  ```

- Result:
  ```erlang
  1> c(what_the_if).
  ./what_the_if.erl:12: Warning: no clause will ever match
  ./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
  {ok,what_the_if}
  2> what_the_if:heh_fine().
  ** exception error: no true branch found when evaluating an if expression
  in function  what_the_if:heh_fine/0
  ```
- Uh oh! the compiler is warning us that no clause from the if on line 12 (1 =:= 2, 1 =:= 1) will ever match because its only guard evaluates to false.
- Remember, in Erlang, everything has to return something, and if expressions are no exception to the rule.
- As such, when Erlang can't find a way to have a guard succeed, it will crash: it cannot not return something. As such, we need to add a catch-all branch that will always succeed no matter what.
- n most languages, this would be called an 'else'. In Erlang, we use 'true' (this explains why the VM has thrown "no true branch found" when it got mad):
  ```erlang
  oh_god(N) ->
  if N =:= 2 -> might_succeed;
  true -> always_does  %% this is Erlang's if's 'else!'
  end.
  ```
- Here's another function showing how to use many guards in an if expression. The function also illustrates how any expression must return something: Talk has the result of the if expression bound to it, and is then concatenated in a string, inside a tuple. When reading the code, it's easy to see how the lack of a true branch would mess things up, considering Erlang has no such thing as a null value (ie.: Lisp's NIL, C's NULL, Python's None, etc):
  ```erlang
  %% note, this one would be better as a pattern match in function heads!
  %% I'm doing it this way for the sake of the example.
  help_me(Animal) ->
  Talk = if Animal == cat  -> "meow";
  Animal == beef -> "mooo";
  Animal == dog  -> "bark";
  Animal == tree -> "bark";
  true -> "fgdadfgna"
  end,
  {Animal, "says " ++ Talk ++ "!"}.
  ```
- Result:
  ```erlang
  6> c(what_the_if).
  ./what_the_if.erl:12: Warning: no clause will ever match
  ./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
  {ok,what_the_if}
  7> what_the_if:help_me(dog).
  {dog,"says bark!"}
  8> what_the_if:help_me("it hurts!").
  {"it hurts!","says fgdadfgna!"}
  ```
- Else' or 'true' branches should be "avoided" altogether: ifs are usually easier to read when you cover all logical ends rather than relying on a "catch all" clause.
-
