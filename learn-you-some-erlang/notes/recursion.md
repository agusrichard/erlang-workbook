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

### Length

- With most recursive functions, I find the base case easier to write first: what's the simplest input we can have to find a length from? Surely an empty list is the simplest one, with a length of 0
- It was mentioned earlier that lists are defined recursively as [1 | [2| ... [n | []]]].
- Given each value in a list counts as a length of 1, the function can be rewritten the following way:
  ```erlang
  len([]) -> 0;
  len([_|T]) -> 1 + len(T).
  ```

### Length of a Tail Recursion

- Tail recursion is a way to transform the above linear process (it grows as much as there are elements) to an iterative one (there is not really any growth).
- Let me explain: what made our previous calls grow is how the answer of the first part depended on evaluating the second part. The answer to 1 + len(Rest) needs the answer of len(Rest) to be found.
- The function len(Rest) itself then needed the result of another function call to be found. The additions would get stacked until the last one is found, and only then would the final result be calculated. Tail recursion aims to eliminate this stacking of operation by reducing them as they happen.
- In order to achieve this, we will need to hold an extra temporary variable as a parameter in our function. I'll illustrate the concept with the help of the factorial function, but this time defining it to be tail recursive. The aforementioned temporary variable is sometimes called accumulator and acts as a place to store the results of our computations as they happen in order to limit the growth of our calls:

  ```erlang
  tail_fac(N) -> tail_fac(N,1).

  tail_fac(0,Acc) -> Acc;
  tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).
  ```

- Here, I define both tail_fac/1 and tail_fac/2. The reason for this is that Erlang doesn't allow default arguments in functions (different arity means different function) so we do that manually.
- It will take as much space to calculate the factorial of 4 as it will take space to calculate the factorial of 1 million (if we forget 4! is a smaller number than 1M! in its complete representation, that is).
- Tail len:

  ```erlang
  tail_len(L) -> tail_len(L,0).

  tail_len([], Acc) -> Acc;
  tail_len([_|T], Acc) -> tail_len(T,Acc+1).
  ```

### More recursive functions

- Duplicate:

```erlang
duplicate(0,_) ->
[];
duplicate(N,Term) when N > 0 ->
[Term|duplicate(N-1,Term)].
```

- Tail Duplicate:

  ```erlang
  tail_duplicate(N,Term) ->
  tail_duplicate(N,Term,[]).

  tail_duplicate(0,_,List) ->
  List;
  tail_duplicate(N,Term,List) when N > 0 ->
  tail_duplicate(N-1, Term, [Term|List]).
  ```

- Reverse:

  ```erlang
  reverse([]) -> [];
  reverse([H|T]) -> reverse(T)++[H].
  ```

- Tail Reverse

  ```erlang
  tail_reverse(L) -> tail_reverse(L,[]).

  tail_reverse([],Acc) -> Acc;
  tail_reverse([H|T],Acc) -> tail_reverse(T, [H|Acc]).
  ```

- Sublist:

  ```erlang
  sublist(_, 0) -> [];
  sublist([], _) -> [];
  sublist([H | T], N) when N > 0 -> [H | sublist(T, N - 1)].
  ```

- Tail Sublist:

  ```erlang
  tail_sublist(L, N) -> reverse(tail_sublist(L, N, [])).

  tail_sublist(_, 0, SubList) ->
      SubList;
  tail_sublist([], _, SubList) ->
      SubList;
  tail_sublist([H | T], N, SubList) when N > 0 ->
      tail_sublist(T, N - 1, [H | SubList]).
  ```

- Zip:
  ```erlang
  zip([],[]) -> [];
  zip([X|Xs],[Y|Ys]) -> [{X,Y}|zip(Xs,Ys)].
  ```
- Lenient Zip:
  ```erlang
  lenient_zip([],_) -> [];
  lenient_zip(_,[]) -> [];
  lenient_zip([X|Xs],[Y|Ys]) -> [{X,Y}|lenient_zip(Xs,Ys)].
  ```

### Quick Sort

- Snippet:

  ```erlang
  quicksort([]) -> [];
  quicksort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot,Rest,[],[]),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

  partition(_,[], Smaller, Larger) -> {Smaller, Larger};
  partition(Pivot, [H|T], Smaller, Larger) ->
  if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
  H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
  end.

  lc_quicksort([]) -> [];
  lc_quicksort([Pivot|Rest]) ->
  lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
  ++ [Pivot] ++
  lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
  ```

### More Than Lists

- Nodes are tuples that contain a key, a value associated to the key, and then two other nodes.
- To represent nodes, tuples are an appropriate data structure. For our implementation, we can then define these tuples as {node, {Key, Value, Smaller, Larger}} (a tagged tuple!), where Smaller and Larger can be another similar node or an empty node ({node, nil}). We won't actually need a concept more complex than that.
- Snippet:
  ```erlang
  insert(Key, Val, {node, 'nil'}) ->
  {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
  insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
  {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
  insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
  {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
  insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
  {node, {Key, Val, Smaller, Larger}}.
  ```
- Lookup:
  ```erlang
  lookup(_, {node, 'nil'}) ->
  undefined;
  lookup(Key, {node, {Key, Val, _, _}}) ->
  {ok, Val};
  lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
  lookup(Key, Smaller);
  lookup(Key, {node, {_, _, _, Larger}}) ->
  lookup(Key, Larger).
  ```

### Thinking recursively

- A different aspect of recursive definitions when compared to their imperative counterparts (usually in while or for loops) is that instead of taking a step-by-step approach ("do this, then that, then this, then you're done"), our approach is more declarative ("if you get this input, do that, this otherwise").
