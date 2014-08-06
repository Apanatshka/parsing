Binary
======
The `Binary` typeclass has basic reading/writing facilities, so if you want to save your parsetable in a file on your harddisk this version of the model facilitates it. There are some tricks used to have automatic derivation of the `Binary` typeclass for the ADTs defined in the file. There are also instances defined for some types that don't define an instance themselves.  
It looks a lot like the tables you see in textbooks. A `ParseTable` consists of the parts `InstrTable`, `EOFTable`, `GotoTable` and `Rules`.  
Each of these table parts that may be sparse (when you leave out the Error instructions) use a `WordMap` instead of an `Array` (an improved version of the `IntMap` that uses patricia trees). 

`Rules` and `Action`s
---------------------
The table parts are really not that interesting as they are your normal table representations except for what is mentioned above about `WordMap`. The rules are different. As normal the rules are enumerated and therefore simply stored in an array. But they don't consist of the sorts and literals you see in other parts of this project. It stored the length of the rule body (the number of things to be popped from the stack) and the sort it generated. It also contains an `Action` ADT that specifies what to do with the popped things. 

An `Action` can be simply build a "cons" around it with the given name. I.e. build a parse tree node.  
There is also the version of "cons" where you only keep part of the things that were popped from the stack (encoded with a bitarray where `1` is keep and `0` is drop).  
Then there is the option not to create a new tree node but to pick one of the popped things and drop the other popped things.  
And of course you can also drop everything being popped, though I'm not sure if that's a normal action for a parser to perform. Still, I'm leaving it in there for now.  
