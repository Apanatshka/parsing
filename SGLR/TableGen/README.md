Rule
====
The `Rule` ADT models the following options:

* `S -> a b T f C {"cons_name"}`, a normal BNF-like rule annotated with a constructor name
* `T -> C`, an alias (note no new tree node is constructed because there is no cons name)
* `S' -> S $`, that standard start rule that's implicitly added

The body of a `Rule` consists of a list of `RPart`s, which is an ADT that models:

* sorts
* literals
* the EOF

Wait, doesn't `Rule` take care of the start rule that need EOF?  
Well, yeah, but `SGLR.TableGen` uses it.

So if the standard start rule is supposed to be there always and EOF can't be used.. Yes, encapsulation! The `Rule'` and `RPart'` types as basically opaque aliases of the standard ones, with construction functions that don't include the start rule and the EOF. Instead you supply the start symbol along with the rules to `TableGen` and in `TableGen` the start rule is created and the EOF is used. 
That's basically the whole deal with the `Rule` module. There are some simple projection functions, but who cares ;)

Graph
=====
To copy some documentation from the file:

> Data.Graph from both the containers and the graph-core package don't support labels for nodes *and* edges. fgl /seems/ to have a bug where some edges aren't added (in both PatriciaTree and Tree implementations). So screw it, I'm rolling my own basic Graph.  
> This small graph lib is not tuned for performance. It bases some names of fgl. The Graph type is query-only. The LGraph type is for building the graph. 

So yeah, that's the motivation. It uses `Integer`s for the nodes so you don't have to worry about having more than 2^ however many bits your platform is. I would have used natural numbers, but too much trouble with downloading an extra package and having another dependency (all because they didn't see the demand for it [Data.Word](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Word.html#g:3) :( ).  
There are some extremely basic queries for in-edges and out-edges of a node, a list of all nodes, a list of all edges. There is also a pretty-printing scheme stolen from (I think) fgl. And you can have the nodes renumbered to consecutive number starting at 0, for if you have node numbers that lie far apart. Not that many people would have that unless they e.g. built a little automaton library on top of it that used powerset numbers for DFA states generated from NFA states. But who would do something like that right?

Automaton
=========
Right. So this is why I wanted a Graph library. To be able to build DFAs and NFAs. The DFA isn't really check with an alphabet and all of that. And the NFA is really an NFA-&#949;... Oops. But it supports my use case so what the heck :)  
Mostly useful because it defines the `NFA`/`DFA` types, epsilon transitions, normal/initial/final/initialAndFinal states and a NFA -> DFA transformation. And a renumbering scheme like in `Graph` but which numbers the initial state as number 0. 
