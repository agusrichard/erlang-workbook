# Modules

Source: https://learnyousomeerlang.com/modules#what-are-modules

### What are modules

- Every other function defined in a module you will ever use needs to be called with the form Module:Function(Arguments).
- Snippet:
  ```erlang
  1> erlang:element(2, {a,b,c}).
  b
  2> element(2, {a,b,c}).
  b
  3> lists:seq(1,4).
  [1,2,3,4]
  4> seq(1,4).
  ** exception error: undefined shell command seq/2
  ```
- You should avoid creating modules like erlang and instead focus on clean logical separations.

### Module Declaration
