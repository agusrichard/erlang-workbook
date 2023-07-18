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

- When writing a module, you can declare two kinds of things: functions and attributes. Attributes are metadata describing the module itself such as its name, the functions that should be visible to the outside world, the author of the code, and so on. This kind of metadata is useful because it gives hints to the compiler on how it should do its job, and also because it lets people retrieve useful information from compiled code without having to consult the source.
- All module attributes follow the form -Name(Attribute).. Only one of them is necessary for your module to be compilable:
- -module(Name).
  This is always the first attribute (and statement) of a file, and for good reason: it's the name of the current module, where Name is an atom. This is the name you'll use to call functions from other modules. The calls are made with the M:F(A) form, where M is the module name, F the function, and A the arguments.
- `-module(useless).` This line of text is a valid module.
- -export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).
  This is used to define what functions of a module can be called by the outside world. It takes a list of functions with their respective arity. The arity of a function is an integer representing how many arguments can be passed to the function. This is critical information, because different functions defined within a module can share the same name if and only if they have a different arity. The functions add(X,Y) and add(X,Y,Z) would thus be considered different and written in the form add/2 and add/3 respectively.
- The following -export attribute can be added after the module declaration:

  ```erlang
  -module(useless)
  -export([add/2])

  add(A+B) -> A+B.
  ```

- The syntax of a function follows the form Name(Args) -> Body., where Name has to be an atom and Body can be one or more Erlang expressions separated by commas. The function is ended with a period. Note that Erlang doesn't use the 'return' keyword. 'Return' is useless!
- Instead, the last logical expression of a function to be executed will have its value returned to the caller automatically without you having to mention it.
- Add the following function (why yes, every tutorial needs a 'Hello world' example! Even at the fourth chapter!), without forgetting to add it to the -export attribute.
  ```erlang
  %% Shows greetings.
  %% io:format/1 is the standard function used to output text.
  hello() ->
  io:format("Hello, world!~n").
  ```
- The hello/0 function also demonstrates how to call functions from foreign modules inside yours. In this case, io:format/1 is the standard function to output text, as written in the comments.
- A last function will be added to the module, using both functions add/2 and hello/0:
  ```erlang
  greet_and_add_two(X) ->
  hello(),
  add(X,2).
  ```
- More generally, the -import attribute follows this recipe:
  ```erlang
  -import(Module, [Function1/Arity, ..., FunctionN/Arity]).
  ```
- Importing a function is not much more than a shortcut for programmers when writing their code. Erlang programmers are often discouraged from using the -import attribute as some people find it reduces the readability of code. In the case of io:format/2, the function io_lib:format/2 also exists.
- Consequently, leaving the module name in is considered good practice. Usually, the only functions you'll see imported come from the lists module: its functions are used with a higher frequency than those from most other modules.
- Our useless module should look like this:

  ```erlang
  -module(useless).
  -export([add/2, hello/0, greet_and_add_two/1]).

  add(A,B) ->
  A + B.

  %% Shows greetings.
  %% io:format/1 is the standard function used to output text.
  hello() ->
  io:format("Hello, world!~n").

  greet_and_add_two(X) ->
  hello(),
  add(X,2).
  ```

- We are done with the "useless" module. You can save the file under the name useless.erl. The file name should be the module name as defined in the -module attribute, followed by '.erl', which is the standard Erlang source extension.

### Compiling the code

- You can call the compiler from many places: $ erlc flags file.erl when in the command line, compile:file(FileName) when in the shell or in a module, c() when in the shell, etc.
- It's time to compile our useless module and try it. Open the Erlang shell, type in:
  ```shell
  1> cd("/path/to/where/you/saved/the-module/").
  "Path Name to the directory you are in"
  ok
  2> c(useless).
  {ok,useless}
  ```
- By default, the shell will only look for files in the same directory it was started in and the standard library: cd/1 is a function defined exclusively for the Erlang shell, telling it to change the directory to a new one so it's less annoying to browse for our files.
- Once you successfully compile code, you'll notice that a useless.beam file was added next to useless.erl in your directory. This is the compiled module. Let's try our first functions ever:
  ```erlang
  3> useless:add(7,2).
  9
  4> useless:hello().
  Hello, world!
  ok
  5> useless:greet_and_add_two(-3).
  Hello, world!
  -1
  6> useless:not_a_real_function().
  ** exception error: undefined function useless:not_a_real_function/0
  ```
- To compile our useless module with some flags, we could do one of the following:
  ```erlang
  7> compile:file(useless, [debug_info, export_all]).
  {ok,useless}
  8> c(useless, [debug_info, export_all]).
  {ok,useless}
  ```
- Note: another option is to compile your Erlang module to native code. Native code compiling is not available for every platform and OS, but on those that support it, it can make your programs go faster (about 20% faster, based on anecdotal evidence). To compile to native code, you need to use the hipe module and call it the following way: hipe:c(Module,OptionsList). You could also use c(Module,[native]). when in the shell to achieve similar results. Note that the .beam file generated will contain both native and non-native code, and the native part will not be portable across platforms.

### More About Modules

- Had you decided to add -author("An Erlang Champ"). to your module, it would have ended up in the same section as vsn.
- vsn is an automatically generated unique value differentiating each version of your code, excluding comments. It is used in code hot-loading (upgrading an application while it runs, without stopping it) and by some tools related to release handling. You can also specify a vsn value yourself if you want: just add -vsn(VersionNumber) to your module.
