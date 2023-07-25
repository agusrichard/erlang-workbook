# Types (or lack thereof)

Source: https://learnyousomeerlang.com/types-or-lack-thereof

### Dynamite-strong Typing

- Erlang's dynamic type system is not a barrier to reliability and safety of programs.
- Dynamic typing was historically chosen for simple reasons; those who implemented Erlang at first mostly came from dynamically typed languages, and as such, having Erlang dynamic was the most natural option to them.
- Erlang is also strongly typed. A weakly typed language would do implicit type conversions between terms. If Erlang were to be weakly typed we could possibly do the operation 6 = 5 + "1". while in practice, an exception for bad arguments will be thrown:
  ```erlang
  1> 6 + "1".
  ** exception error: bad argument in an arithmetic expression
  in operator  +/2
  called as 6 + "1"
  ```

### Type conversions

- Erlang, like many languages, changes the type of a term by casting it into another one. This is done with the help of built-in functions, as many of the conversions could not be implemented in Erlang itself.
- Each of these functions take the form `<type>_to_<type>` and are implemented in the erlang module. Here are a few of them:
  ```erlang
  1> erlang:list_to_integer("54").
  54
  2> erlang:integer_to_list(54).
  "54"
  3> erlang:list_to_integer("54.32").
  ** exception error: bad argument
  in function  list_to_integer/1
  called as list_to_integer("54.32")
  4> erlang:list_to_float("54.32").
  54.32
  5> erlang:atom_to_list(true).
  "true"
  6> erlang:list_to_bitstring("hi there").
  <<"hi there">>
  7> erlang:bitstring_to_list(<<"hi there">>).
  "hi there"
  ```

### To Guard a Data Type

- Erlang basic data types are easy to spot, visually:
  - tuples have the curly brackets,
  - lists the square brackets,
  - strings are enclosed in double quotation marks, etc.
- There are functions dedicated to this task. They will take a single argument and return true if the type is right, false otherwise. They are part of the few functions allowed in guard expressions and are named the type test BIFs:
  ```text
  is_atom/1           is_binary/1
  is_bitstring/1      is_boolean/1        is_builtin/3
  is_float/1          is_function/1       is_function/2
  is_integer/1        is_list/1           is_number/1
  is_pid/1            is_port/1           is_record/2
  is_record/3         is_reference/1      is_tuple/1
  ```
- They can be used like any other guard expression, wherever guard expressions are allowed. You might be wondering why there is no function just giving the type of the term being evaluated (something akin to type_of(X) -> Type). The answer is pretty simple. Erlang is about programming for the right cases: you only program for what you know will happen and what you expect. Everything else should cause errors as soon as possible.

### For Type Junkies

- There are tools for type junkies but we'll visit them later on
