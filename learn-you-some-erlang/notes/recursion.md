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
