parsing
=======

Ok, so I took a Compiler Construction course, learnt how LL and LR parsing work, heard about other things like SGLR, PEG, Attribute grammars then read some more and also found RNGLR and GLL. I figured I'd take up implementing SGLR in Haskell as a little summer project. I downloaded some papers on LR/SGLR parsing. Though old, one paper mentioned that generating full parser instead of tables for parsing engines was faster. I got distracted by that, thinking that optimisations put into generated parsers should be transferable to parsing engines and table generators. Unless the problem was cache-behaviour. 

So now I don't know where I'm going with this repo. I want to try to make a cache-friendly LR parsing engine. But I don't know if that new or not. I also still want to study the details of SGLR a little better. The SRNGLR paper was also interesting. 
To actually find out if the changes I have in mind make an LR parse engine more cache-friendly will probably require that I implement it in some low-level language like C. But meh.. 

So I'm not sure where I'm going with this code. 
