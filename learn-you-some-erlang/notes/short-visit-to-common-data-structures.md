# A Short Visit to Common Data Structures

Source: https://learnyousomeerlang.com/a-short-visit-to-common-data-structures

### Records

- As such, Erlang records are a lot like structs in C (if you know C.)
- They're declared as module attributes in the following manner:

  ```erlang
  -module(records).
  -compile(export_all).

  -record(robot, {name,
  type=industrial,
  hobbies,
  details=[]}).
  ```

- So here we have a record representing robots with 4 fields: name, type, hobbies and details.
- Here's how to declare a record in the module records:
  ```erlang
  first_robot() ->
  #robot{name="Mechatron",
  type=handmade,
  details=["Moved by a small man inside"]}.
  ```
- Running the code:
  ```erlang
  1> c(records).
  {ok,records}
  2> records:first_robot().
  {robot,"Mechatron",handmade,undefined,
  ["Moved by a small man inside"]}
  ```
- The Erlang shell has a command rr(Module) that lets you load record definitions from Module:
  ```erlang
  3> rr(records).
  [robot]
  4> records:first_robot().
  #robot{name = "Mechatron",type = handmade,
  hobbies = undefined,
  details = ["Moved by a small man inside"]}
  ```
- Writing records alone won't do much. We need a way to extract values from them. There are basically two ways to do this. The first one is with a special 'dot syntax'. Assuming you have the record definition for robots loaded:
  ```erlang
  5> Crusher = #robot{name="Crusher", hobbies=["Crushing people","petting cats"]}.
  #robot{name = "Crusher",type = industrial,
  hobbies = ["Crushing people","petting cats"],
  details = []}
  6> Crusher#robot.hobbies.
  ["Crushing people","petting cats"]
  ```
- Nested records:
  ```erlang
  7> NestedBot = #robot{details=#robot{name="erNest"}}.
  #robot{name = undefined,type = industrial,
  hobbies = undefined,
  details = #robot{name = "erNest",type = industrial,
  hobbies = undefined,details = []}}
  8> (NestedBot#robot.details)#robot.name.
  "erNest"
  ```
- One saving feature of records is the possibility to use them in function heads to pattern match and also in guards. Declare a new record as follows on top of the file, and then add the functions under:

  ```erlang
  -record(user, {id, name, group, age}).

  %% use pattern matching to filter
  admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowed!";
  admin_panel(#user{name=Name}) ->
  Name ++ " is not allowed".

  %% can extend user without problem
  adult_section(U = #user{}) when U#user.age >= 18 ->
  %% Show stuff that can't be written in such a text
  allowed;
  adult_section(_) ->
  %% redirect to sesame street site
  forbidden.
  ```

- The following function illustrates how to update a record (they wouldn't be very useful otherwise):

  ```erlang
  repairman(Rob) ->
  Details = Rob#robot.details,
  NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
  {repaired, NewRob}.
  ```

- Compiled:
  ```erlang
  16> c(records).
  {ok,records}
  17> records:repairman(#robot{name="Ulbert", hobbies=["trying to have feelings"]}).
  {repaired,#robot{name = "Ulbert",type = industrial,
  hobbies = ["trying to have feelings"],
  details = ["Repaired by repairman"]}}
  ```
- One last thing about records. Because they're pretty useful and code duplication is annoying, Erlang programmers frequently share records across modules with the help of header files. Erlang header files are pretty similar to their C counter-part: they're nothing but a snippet of code that gets added to the module as if it were written there in the first place. Create a file named records.hrl with the following content:
  ```erlang
  %% this is a .hrl (header) file.
  -record(included, {some_field,
  some_default = "yeah!",
  unimaginative_name}).
  ```
- To include it in records.erl, just add the following line to the module:
  ```erlang
  -include("records.hrl").
  included() -> #included{some_field="Some value"}.
  ```
- Usage:

```erlang
18> c(records).
{ok,records}
19> rr(records).
[included,robot,user]
20> records:included().
#included{some_field = "Some value",some_default = "yeah!",
unimaginative_name = undefined}
```

- You will often see open source software using the method shown here of having a project-wide .hrl file for records that are shared across all modules.
- I strongly recommend that you keep all record definitions local, within one module. If you want some other module to look at a record's innards, write functions to access its fields and keep its details as private as possible.
