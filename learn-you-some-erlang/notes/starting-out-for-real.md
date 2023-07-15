# Starting Out (for real)

Source: https://learnyousomeerlang.com/starting-out-for-real

### Numbers
- In the Erlang shell, expressions have to be terminated with a period followed by whitespace (line break, a space etc.), otherwise they won't be executed.
- Snippet:
  ```erlang
  1> 2 + 15.
  17
  2> 49 * 100.
  4900
  3> 1892 - 1472.
  420
  4> 5 / 2.
  2.5
  5> 5 div 2.
  2
  6> 5 rem 2.
  1
  ```
- If you want to express integers in other bases than base 10, just enter the number as Base#Value (given Base is in the range 2..36):
  ```erlang
  10> 2#101010.
  42
  11> 8#0677.
  447
  12> 16#AE.
  174
  ```

### Invariable Variables
- The basic behavior of variables can be demonstrated with these 7 expressions (note that variables begin with an uppercase letter):
  ```erlang
  1> One.
  * 1: variable 'One' is unbound
  2> One = 1.
  1
  3> Un = Uno = One = 1.
  1
  4> Two = One + One.
  2
  5> Two = 2.       
  2
  6> Two = Two + 1.
  ** exception error: no match of right hand side value 3
  7> two = 2.
  ** exception error: no match of right hand side value 2
  ```
- The first thing these commands tell us is that you can assign a value to a variable exactly once; then you can 'pretend' to assign a value to a variable if it's the same value it already has. If it's different, Erlang will complain.
- The = operator (not the variables) has the role of comparing values and complaining if they're different. If they're the same, it returns the value:
  ```erlang
  8> 47 = 45 + 2.
  47
  9> 47 = 45 + 3.
  ** exception error: no match of right hand side value 48
  ```
- What this operator does when mixed with variables is that if the left-hand side term is a variable and it is unbound (has no value associated to it), Erlang will automatically bind the right-hand side value to the variable on the left-hand side. The comparison will consequently succeed and the variable will keep the value in memory.
- This behavior of the = operator is the basis of something called 'Pattern matching', which many functional programming languages have, although Erlang's way of doing things is usually regarded as more flexible and complete than alternatives.
- The other thing the commands 1-7 told us is that variable names must begin with a capital letter. Command 7 failed because the word two had a lowercase letter to begin with. Technically, variables can start with an underscore ('_') too, but by convention their use is restricted to values you do not care about, yet you felt it was necessary to document what it contains.
- If you're testing in the shell and save the wrong value to a variable, it is possible to 'erase' that variable by using the function f(Variable).. If you wish to clear all variable names, do f()..
  
### Atoms
- There is a reason why variables names can't begin with a lowercase character: atoms. Atoms are literals, constants with their own name for value.
- What you see is what you get and don't expect more. The atom cat means "cat" and that's it. You can't play with it, you can't change it, you can't smash it to pieces; it's cat. Deal with it.
- While single words starting with a lowercase letter is a way to write an atom, there's more than one manner to do it:
  ```erlang
  1> atom.
  atom
  2> atoms_rule.
  atoms_rule
  3> atoms_rule@erlang.
  atoms_rule@erlang
  4> 'Atoms can be cheated!'.
  'Atoms can be cheated!'
  5> atom = 'atom'.
  atom
  ```
- An atom should be enclosed in single quotes (') if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore (_), or @.
- Atoms are really nice and a great way to send messages or represent constants. However there are pitfalls to using atoms for too many things: an atom is referred to in an "atom table" which consumes memory (4 bytes/atom in a 32-bit system, 8 bytes/atom in a 64-bit system). The atom table is not garbage collected, and so atoms will accumulate until the system tips over, either from memory usage or because 1048577 atoms were declared.
- This means atoms should not be generated dynamically for whatever reason; if your system has to be reliable and user input lets someone crash it at will by telling it to create atoms, you're in serious trouble. Atoms should be seen as tools for the developer because honestly, it's what they are.

### Boolean Algebra & Comparison operators
- Snippet:
  ```erlang
  1> true and false.
  false
  2> false or true.
  true
  3> true xor false.
  true
  4> not false.
  true
  5> not (true and true).
  false
  ```
- If you want to have the short-circuit operators (which will only evaluate the right-side argument if it needs to), use andalso and orelse.
- Testing for equality:
  ```erlang
  6> 5 =:= 5.
  true
  7> 1 =:= 0.
  false
  8> 1 =/= 0.
  true
  9> 5 =:= 5.0.
  false
  10> 5 == 5.0.
  true
  11> 5 /= 5.0.
  false
  ```
- First of all, if your usual language uses == and != to test for and against equality, Erlang uses =:= and =/=. The three last expressions (lines 9 to 11) also introduce us to a pitfall: Erlang won't care about floats and integers in arithmetic, but will do so when comparing them. No worry though, because the == and /= operators are there to help you in these cases. This is important to remember whether you want exact equality or not.
- Other operators for comparisons are < (less than), > (greater than), >= (greater than or equal to) and =< (less than or equal to). That last one is backwards (in my opinion) and is the source of many syntax errors in my code. Keep an eye on that =<.
  ```erlang
  12> 1 < 2.
  true
  13> 1 < 1.
  false
  14> 1 >= 1.
  true
  15> 1 =< 1.
  true
  ```
- Erlang has no such things as boolean true and false. The terms true and false are atoms, but they are integrated well enough into the language you shouldn't have a problem with that as long as you don't expect false and true to mean anything but false and true.
- The correct ordering of each element in a comparison is the following:
number < atom < reference < fun < port < pid < tuple < list < bit string
