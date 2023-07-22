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
