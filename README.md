# 2016, January

I've read a lot more about grammars and parsing since I stopped writing code for this project. Reading is all I'm allowing myself time for, but whatever. I'm updating this readme with a few more names of interesting techniques as a reminder to myself. Consider them keywords for your internet searches, though I'll also provide a few links of things I've read:

* Recursive ascent parsing. It's the reverse of topdown LL parsing with recursive decent. So it's a kind of LR parsing. Only it uses the call stack as the parser's stack. (https://en.wikipedia.org/wiki/Recursive_ascent_parser)
* LALRPOP, a Rust library that does recursive ascent (http://smallcultfollowing.com/babysteps/blog/2015/09/14/lalrpop/)
* Data dependent grammars. The principled version of context dependent parsing, which doesn't support everything like parser combinators and raw coded parsers. It can do network protocol parsing like reading a number and using that to determine what it should parse as the data payload of the message. Makes total sense to me, really cool stuff. (http://www.cs.princeton.edu/~dpw/papers/ddgrammars-0709.pdf)
* Derivative parsing (http://matt.might.net/articles/parsing-with-derivatives/). I need to look into this further but the technique apparently supports all CFGs and does well in practice. It's still pretty new and there are many eyeballs on it and ideas for improvement. If only I had time...
* I found the above thing in the comments on Hacker News about automatic differentiation. The article was a super simple explanation of how it works and how cutting out an intermediate step can make it a lot faster. I wonder if the derivative parsing process is alike and whether they're using this. Perhaps that's what the compacting is about, though I imagine that's not quite the same thing. (http://h2.jaguarpaw.co.uk/posts/symbolic-expressions-can-be-automatically-differentiated/)

# 2014, August

## parsing

Ok, so I took a Compiler Construction course, learnt how LL and LR parsing work, heard about other things like SGLR, PEG, Attribute grammars then read some more and also found RNGLR and GLL. I figured I'd take up implementing SGLR in Haskell as a little summer project (I'm aware this has been done before, but I just want the learning experience). I downloaded some papers on LR/SGLR parsing. Though old, one paper mentioned that generating a full parser instead of tables for parsing engines resulting in faster parsing times. I got distracted by that, thinking that optimisations put into generated parsers should be transferable to parsing engines and table generators. Unless the problem was cache-behaviour (CPU caches were still stuck in my head from a video I watched recently). 

So now I don't know where I'm going with this repo. I want to try to make a cache-friendly LR parsing engine. But I don't know if that new or not. I also still want to study the details of SGLR a little better. The SRNGLR paper was also interesting. 
To actually find out if what I have in mind works --- to make an LR parse engine more cache-friendly --- will probably require that I implement it in some low-level language like C. But meh.. 

So I'm not sure where I'm going with this code. For now it's an LR(1) parsing engine and parse table generator. Don't have tests yet. The table generator is missing a piece. But the parsing engine seems to work, and I was able to remove explicit state numbers from the stack. 

### example files

There are two example files here:

1. `parseExample.hs` takes a parse table example from the book "Modern compiler implementation in Java" by Andrew W. Appel. In particular it's the table for grammar 3.1, table 3.19. 
  This example uses ADTs to describe the input tokens and the non-terminals. By deriving `Enum` instances, these can be changed into numbers for the table. The code isn't very interesting otherwise, but it's nice to see that the table works with the parsing engine. 
2. `tableGenExample.hs` takes a grammar --- this time from [wikipedia](https://en.wikipedia.org/wiki/Canonical_LR_parser) --- and transforms it into a table. This time the code is just a small amount that show how to use the exposed API from `SGLR.TableGen.Rule`. The grammar is slightly adapted from the one on wikipedia, as it also specifies some constructor names (so you don't have to define a whole AST type yourself like in `parseExample.hs`). 
