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

### Tuples
- A tuple is a way to organize data. It's a way to group together many terms when you know how many there are. In Erlang, a tuple is written in the form {Element1, Element2, ..., ElementN}. As an example, you'd give me the coordinates (x,y) if you wanted to tell me the position of a point in a Cartesian graph. We can represent this point as a tuple of two terms:
  ```erlang
  1> X = 10, Y = 4.
  4
  2> Point = {X,Y}.
  {10,4}
  3> Point = {4,5}.
  {4,5}
  4> {X,Y} = Point.
  {4,5}
  5> X.
  4
  6> {X,_} = Point.
  {4,5}
  ```
- The _ variable is always seen as unbound and acts as a wildcard for pattern matching. Pattern matching to unpack tuples will only work if the number of elements (the tuple's length) is the same.
- This throws an error, but it's exactly what we want! This is, again, pattern matching at work. The = operator ends up comparing {kelvin, T} and {celsius, 23.213}: even if the variable T is unbound, Erlang won't see the celsius atom as identical to the kelvin atom when comparing them. An exception is thrown which stops the execution of code. By doing so, the part of our program that expects a temperature in Kelvin won't be able to process temperatures sent in Celsius. This makes it easier for the programmer to know what is being sent around and also works as a debugging aid. A tuple which contains an atom with one element following it is called a 'tagged tuple'. Any element of a tuple can be of any type, even another tuple:
  ```erlang
  10> PreciseTemperature = {celsius, 23.213}.
  {celsius,23.213}
  11> {kelvin, T} = PreciseTemperature.
  ** exception error: no match of right hand side value {celsius,23.213}
  ```

### Lists
- Lists can contain anything! Numbers, atoms, tuples, other lists; your wildest dreams in a single structure. The basic notation of a list is [Element1, Element2, ..., ElementN] and you can mix more than one type of data in it:
  ```erlang
  1> [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
  [1,2,3,{numbers,[4,5,6]},5.34,atom]
  ```
- This is one of the most disliked things in Erlang: strings! Strings are lists and the notation is absolutely the exact same! Why do people dislike it? Because of this:
  ```erlang
  3> [97,98,99,4,5,6].
  [97,98,99,4,5,6]
  4> [233].
  "é"
  ```
- Erlang will print lists of numbers as numbers only when at least one of them could not also represent a letter! There is no such thing as a real string in Erlang! This will no doubt come to haunt you in the future and you'll hate the language for it. Don't despair, because there are other ways to write strings we'll see later in this chapter.
- To glue lists together, we use the ++ operator. The opposite of ++ is -- and will remove elements from a list:
  ```erlang
  5> [1,2,3] ++ [4,5].
  [1,2,3,4,5]
  6> [1,2,3,4,5] -- [1,2,3].
  [4,5]
  7> [2,4,2] -- [2,4].
  [2]
  8> [2,4,2] -- [2,4,2].
  []
  ```
- Both ++ and -- are right-associative. This means the elements of many -- or ++ operations will be done from right to left, as in the following examples:
  ```erlang
  9> [1,2,3] -- [1,2] -- [3].
  [3]
  10> [1,2,3] -- [1,2] -- [2].
  [2,3]
  ```
- Let's keep going. The first element of a list is named the Head, and the rest of the list is named the Tail. We will use two built-in functions (BIF) to get them.
  ```erlang
  11> hd([1,2,3,4]).
  1
  12> tl([1,2,3,4]).
  [2,3,4]
  ```
- Accessing or adding the head is fast and efficient: virtually all applications where you need to deal with lists will always operate on the head first. As it's used so frequently, there is a nicer way to separate the head from the tail of a list with the help of pattern matching: [Head|Tail]. Here's how you would add a new head to a list:
  ```erlang
  13> List = [2,3,4].
  [2,3,4]
  14> NewList = [1|List].
  [1,2,3,4]
  15> [Head|Tail] = NewList.
  [1,2,3,4]
  16> Head.
  1
  17> Tail.
  [2,3,4]
  18> [NewHead|NewTail] = Tail.
  [2,3,4]
  19> NewHead.
  2
  ```
- The | we used is named the cons operator (constructor). In fact, any list can be built with only cons and values:
  ```erlang
  20> [1 | []].
  [1]
  21> [2 | [1 | []]].
  [2,1]
  22> [3 | [2 | [1 | []] ] ].
  [3,2,1]
  ```
- Lists can thus be defined recursively as a head preceding a tail, which is itself a head followed by more heads. In this sense we could imagine a list being a bit like an earthworm: you can slice it in half and you'll then have two worms.
  ![HeadTailWorm](https://learnyousomeerlang.com/static/img/worm.png)
- The ways Erlang lists can be built are sometimes confusing to people who are not used to similar constructors. To help you get familiar with the concept, read all of these examples (hint: they're all equivalent):
  ```erlang
  [a, b, c, d]
  [a, b, c, d | []]
  [a, b | [c, d]]
  [a, b | [c | [d]]]
  [a | [b | [c | [d]]]]
  [a | [b | [c | [d | [] ]]]]
  ```
- Using the form [1 | 2] gives what we call an 'improper list'. Improper lists will work when you pattern match in the [Head|Tail] manner, but will fail to be used with standard functions of Erlang (even length()). This is because Erlang expects proper lists. Proper lists end with an empty list as their last cell. When declaring an item like [2], the list is automatically formed in a proper manner. As such, [1|[2]] would work! Improper lists, although syntactically valid, are of very limited use outside of user-defined data structures.

### List Comprehensions
- List comprehensions in Erlang are about building sets from other sets. Given the set {2n : n in L} where L is the list [1,2,3,4], the Erlang implementation would be:
  ```erlang
  1> [2*N || N <- [1,2,3,4]].
  [2,4,6,8]
  ```
- You can also add constraints to a list comprehension by using operations that return boolean values. if we wanted all the even numbers from one to ten, we could write something like:
  ```erlang
  2> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
  [2,4,6,8,10]
  ```
- Of course, the decimals aren't rounded in a readable manner, but you get the point. The recipe for list comprehensions in Erlang is therefore NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]. The part Pattern <- List is named a Generator expression. You can have more than one!
  ```erlang
  5> [X+Y || X <- [1,2], Y <- [2,3]].
  [3,4,4,5]
  ```
- This runs the operations 1+2, 1+3, 2+2, 2+3. So if you want to make the list comprehension recipe more generic, you get: NewList = [Expression || GeneratorExp1, GeneratorExp2, ..., GeneratorExpN, Condition1, Condition2, ... ConditionM]. Note that the generator expressions coupled with pattern matching also act as a filter:
  ```erlang
  6> Weather = [{toronto, rain}, {montreal, storms}, {london, fog},
  6>            {paris, sun}, {boston, fog}, {vancouver, snow}].
  [{toronto,rain},
  {montreal,storms},
  {london,fog},
  {paris,sun},
  {boston,fog},
  {vancouver,snow}]
  7> FoggyPlaces = [X || {X, fog} <- Weather].
  [london,boston]
  ```
- If an element of the list 'Weather' doesn't match the {X, fog} pattern, it's simply ignored in the list comprehension whereas the = operator would have thrown an exception.


### Bit Syntax
- Bit syntax encloses binary data between << and >>, splits it in readable segments, and each segment is separated by a comma. A segment is a sequence of bits of a binary (not necessarily on a byte boundary, although this is the default behaviour). Say we want to store an orange pixel of true color (24 bits). If you've ever checked colors in Photoshop or in a CSS style sheet for the web, you know the hexadecimal notation has the format #RRGGBB. A tint of orange is #F09A29 in that notation, which could be expanded in Erlang to:
  ```erlang
  1> Color = 16#F09A29.
  15768105
  2> Pixel = <<Color:24>>.
  <<240,154,41>>
  ```
- This basically says "Put the binary values of #F09A29 on 24 bits of space (Red on 8 bits, Green on 8 bits and Blue also on 8 bits) in the variable Pixel." The value can later be taken to be written to a file. This doesn't look like much, but once written to a file, what you'd get by opening it in a text editor would be a bunch of unreadable characters. When you read back from the file, Erlang would interpret the binary into the nice <<240,151,41>> format again!
- What's more interesting is the ability to pattern match with binaries to unpack content:
  ```erlang
  3> Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
  <<213,45,132,64,76,32,76,0,0,234,32,15>>
  4> <<Pix1,Pix2,Pix3,Pix4>> = Pixels.
  ** exception error: no match of right hand side value <<213,45,132,64,76,32,76,
  0,0,234,32,15>>
  5> <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
  <<213,45,132,64,76,32,76,0,0,234,32,15>>
  ```

### Binary Comprehensions
- Binary comprehensions are to bit syntax what list comprehensions are to lists: a way to make code short and concise. They are relatively new in the Erlang world as they were there in previous revisions of Erlang, but required a module implementing them to use a special compile flag in order to work.




