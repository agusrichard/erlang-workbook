# Introduction

Source: https://learnyousomeerlang.com/introduction#about-this-tutorial

### So what's Erlang?
- Erlang is a functional programming language.
- Changing the value of any variable is strictly forbidden
- Functions always returning the same result for the same parameter is called referential transparency.
- Erlang has this very pragmatic approach with functional programming: obey its purest principles (referential transparency, avoiding mutable data, etc), but break away from them when real world problems pop up.
- To be able to have dozens of tasks being performed at the same time, Erlang uses the actor model, and each actor is a separate process in the virtual machine.
- In a nutshell, if you were an actor in Erlang's world, you would be a lonely person, sitting in a dark room with no window, waiting by your mailbox to get a message. Once you get a message, you react to it in a specific way: you pay the bills when receiving them, you respond to Birthday cards with a "Thank you" letter and you ignore the letters you can't understand.
- Erlang's actor model can be imagined as a world where everyone is sitting alone in their own room and can perform a few distinct tasks. Everyone communicates strictly by writing letters and that's it. While it sounds like a boring life (and a new age for the postal service), it means you can ask many people to perform very specific tasks for you, and none of them will ever do something wrong or make mistakes which will have repercussions on the work of others; they may not even know the existence of people other than you (and that's great).
- To escape this analogy, Erlang forces you to write actors (processes) that will share no information with other bits of code unless they pass messages to each other. Every communication is explicit, traceable and safe.

### The first case of this is related to Erlang's massive scaling abilities due to its lightweight processes. It is true that Erlang processes are very light: you can have hundreds of thousands of them existing at the same time, but this doesn't mean you have to use it that way just because you can.
- The first case of this is related to Erlang's massive scaling abilities due to its lightweight processes. It is true that Erlang processes are very light: you can have hundreds of thousands of them existing at the same time, but this doesn't mean you have to use it that way just because you can.
- For example, creating a shooter game where everything including bullets is its own actor is madness. The only thing you'll shoot with a game like this is your own foot. There is still a small cost in sending a message from actor to actor, and if you divide tasks too much, you will make things slower!
