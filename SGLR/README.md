Engine
======
`Engine.hs` holds the (currently LR(1)) parsing engine. It uses stuff from `Model.hs`, because that's the model of a parsing table that it uses. It basically takes a `ByteString` (`Array` of `Word8`) and processes each `Word8` as a seperate input token. I had in mind that this at least covers ASCII or arbitrarily enumerated lexer tokens. 
Because the normal `stateNo -> input -> instruction` table doesn't include the EOF token, there are two functions `parse` and `eofParse` that both use `parse'` which handles the familiar shift/reduce/error/accept instructions. 
To be able to stop early with the parsing when there is still input, the `Either` type is used. A `Right` holds the stack of the parser. A `Left` holds a `Maybe` value, which is either `Just` the result of the parse (when accept was found) or the `Nothing` value when an `Error` was found. There is no error-recovery or trace or expected input message yet.

Model
=====
`Model.hs` is the model of the parsing table used in the parsing engine in `Engine.hs`. It's a slightly different representation compared to what you see in text-books because I'm experimenting with structures that *might* be more cache-friendly. It's best visible in the `State a` type, because state is not a number but a tuple of resolved pointers into the different tables.
These tables are:

1. `InstrTable` (instruction table: stateNo `Word` -> input `Word8` -> instruction `Instruction` ADT)
2. `EOFTable` (eof input instructions: stateNo `Word` -> instruction `Instruction` ADT)
3. `GotoTable` (goto instructions: stateNo `Word` -> sort `Word` -> state `State a`)

For a more traditional model of the parsing table (at least the part where you just have state numbers), look in the `Model` subdirectory where the `Binary.hs` model is defined. 

TableGen
========
`TableGen.hs` is the table generator for the project. It defines `Rule'`s in it's subdirectory `TableGen` (`Rule.hs`), for the user. The code `TableGen.hs` is kind of a mess so I'm not sure you want to read it. 
Basically the code suboptimally describe FIRST and FOLLOW sets and uses a little automaton library (`TableGen/Automaton.hs`) to build an NFA with epsilon moves. Then it turns the NFA into a DFA to get the closures that you can find being built in standard LR parse table algorithms. 
From the DFA is then changes the representation to that of the `Binary` Model mentioned before. The automaton library made sure that the initial state (start state / start symbol) in the DFA has number 0, as is assumed in the model. 

The modules in the subdirectory of `TableGen` are smaller and more readable. 
