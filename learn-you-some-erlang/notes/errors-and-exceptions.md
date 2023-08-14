# Errors and Exceptions

Source: https://learnyousomeerlang.com/errors-and-exceptions

### Not so fast!

- That's a bit because Erlang has two main paradigms: functional and concurrent.
  - The functional subset is the one I've been explaining since the beginning of the book: referential transparency, recursion, higher order functions, etc.
  - The concurrent subset is the one that makes Erlang famous: actors, thousands and thousands of concurrent processes, supervision trees, etc.

### A Compilation of Errors

- Compile-time errors are often syntactic mistakes: check your function names, the tokens in the language (brackets, parentheses, periods, commas), the arity of your functions, etc.
- It is better to resolve compiler errors in the order they were reported to avoid being misled by errors which may not actually be errors at all.

### No, YOUR logic is wrong!

- Logical errors are the hardest kind of errors to find and debug.

### Run-time Errors

- Run-time errors are pretty destructive in the sense that they crash your code. While Erlang has ways to deal with them, recognizing these errors is always helpful.
- `function_clause`: All the guard clauses of a function failed, or none of the function clauses' patterns matched.

  ```erlang
  1> lists:sort([3,2,1]).
  [1,2,3]
  2> lists:sort(fffffff).
  ** exception error: no function clause matching lists:sort(fffffff)
  ```

- `case_clause`: Looks like someone has forgotten a specific pattern in their case, sent in the wrong kind of data, or needed a catch-all clause!
  ```erlang
  3> case "Unexpected Value" of
  3>    expected_value -> ok;
  3>    other_expected_value -> 'also ok'
  3> end.
  ** exception error: no case clause matching "Unexpected Value"
  ```
- `if_clause`: This is pretty similar to case_clause errors: it can not find a branch that evaluates to true. Ensuring you consider all cases or add the catch-all true clause might be what you need.

  ```erlang
  4> if 2 > 4 -> ok;
  4>    0 > 1 -> ok
  4> end.
  ** exception error: no true branch found when evaluating an if expression
  ```

- `badmatch`: Badmatch errors happen whenever pattern matching fails. This most likely means you're trying to do impossible pattern matches (such as above), trying to bind a variable for the second time, or just anything that isn't equal on both sides of the = operator (which is pretty much what makes rebinding a variable fail!). Note that this error sometimes happens because the programmer believes that a variable of the form _MyVar is the same as _. Variables with an underscore are normal variables, except the compiler won't complain if they're not used. It is not possible to bind them more than once.
  ```erlang
  5> [X,Y] = {4,5}.
  ** exception error: no match of right hand side value {4,5}
  ```
- `badarg`: This one is really similar to function_clause as it's about calling functions with incorrect arguments. The main difference here is that this error is usually triggered by the programmer after validating the arguments from within the function, outside of the guard clauses. I'll show how to throw such errors later in this chapter.

```erlang
6> erlang:binary_to_list("heh, already a list").
** exception error: bad argument
in function  binary_to_list/1
called as binary_to_list("heh, already a list")
```

- `undef`: This happens when you call a function that doesn't exist. Make sure the function is exported from the module with the right arity (if you're calling it from outside the module) and double check that you did type the name of the function and the name of the module correctly. Another reason to get the message is when the module is not in Erlang's search path. By default, Erlang's search path is set to be in the current directory. You can add paths by using code:add_patha/1 or code:add_pathz/1. If this still doesn't work, make sure you compiled the module to begin with!

  ```erlang
  7> lists:random([1,2,3]).
  ** exception error: undefined function lists:random/1
  ```

- And several others

### Raising Exceptions

#### Errors

- Calling erlang:error(Reason) will end the execution in the current process and include a stack trace of the last functions called with their arguments when you catch it. These are the kind of exceptions that provoke the run-time errors above.
- Now, errors aren't limited to the examples above. You can define your own kind of errors too:
  ```erlang
  1> erlang:error(badarith).
  ** exception error: bad argument in an arithmetic expression
  2> erlang:error(custom_error).
  ** exception error: custom_error
  ```

#### Exits

- There are two kinds of exits: 'internal' exits and 'external' exits. Internal exits are triggered by calling the function exit/1 and make the current process stop its execution. External exits are called with exit/2 and have to do with multiple processes in the concurrent aspect of Erlang; as such, we'll mainly focus on internal exits and will visit the external kind later on.
- Internal exits are pretty similar to errors. In fact, historically speaking, they were the same and only exit/1 existed. They've got roughly the same use cases. So how to choose one? Well the choice is not obvious. To understand when to use one or the other, there's no choice but to start looking at the concepts of actors and processes from far away.
- You can then decide whether what you've got is 'simply' an error or a condition worthy of killing the current process. This point is made stronger by the fact that erlang:error/1 returns a stack trace and exit/1 doesn't.

#### Throws

- A throw is a class of exceptions used for cases that the programmer can be expected to handle.
- In comparison with exits and errors, they don't really carry any 'crash that process!' intent behind them, but rather control flow. As you use throws while expecting the programmer to handle them, it's usually a good idea to document their use within a module using them.
  ```erlang
  1> throw(permission_denied).
  ** exception throw: permission_denied
  ```
- Throws can also be used for non-local returns when in deep recursion. An example of that is the ssl module which uses throw/1 as a way to push {error, Reason} tuples back to a top-level function. This function then simply returns that tuple to the user.
- This lets the implementer only write for the successful cases and have one function deal with the exceptions on top of it all.
- Another example could be the array module, where there is a lookup function that can return a user-supplied default value if it can't find the element needed. When the element can't be found, the value default is thrown as an exception, and the top-level function handles that and substitutes it with the user-supplied default value.
- This keeps the programmer of the module from needing to pass the default value as a parameter of every function of the lookup algorithm, again focusing only on the successful cases.
- As a rule of thumb, try to limit the use of your throws for non-local returns to a single module in order to make it easier to debug your code. It will also let you change the innards of your module without requiring changes in its interface.
