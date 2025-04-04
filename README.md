## How to build: 
Basically just make sure you have Rust nightly installed: `rustup toolchain install nightly`
And Clippy installed also: https://doc.rust-lang.org/clippy/installation.html

## How to use
Run the following commands. We use clap for command line parsing.
```
cargo build
./target/debug/compiler <wacc-file>
or
./target/debug/compiler --help 
```
The --help option will give you the ability to peer into what the different IR's look like
at various stages and the --print-cfg flag can even print out the CFG before & after various optimizations
You can also enable various optimizations via --O0, --O1, --O2 etc. 

Example commands
```bash
cargo build
./target/debug/compiler test_cases/valid/expressions/longSplitExpr2.wacc --O2 --print-cfg
gcc -o exe -z noexecstack longSplitExpr2.s
./exe
```

# Things that I want to Improve
I'm building off the WACC Group project but there's some things that I'm not happy with or that could be improved. 
I'll list them in order of priority. 

### Compile Times
Currently, the compile times are both extremely long for both incremental and a clean compile. 
Further testing needs to be done but I suspect its to do with the way the frontend works with Chumsky or some other 
trait bounds that take forever to solve. I'd like to look into this as any changes to the frontend freezes up my machine
for 5-10 seconds when compiling which shouldn't happen. I suspect its due to the highly nested types in Chumsky so either I
try to fix that or use another type of parser.

### SSA Optimizations 
I want to include an SSA based IR so I can more easily do optimizations. I'm not sure if I want to use a Sea of Nodes (SoN)
based SSA IR because V8 switched over to a CFG based IR however WackyLang is mostly effect-free (little side-effects) 
so it should be suitable however my concern is that SoN will be harder to debug. 

### Other Optimizations
- Alias optimizations to optimize Pairs/Arrays (as currently they're ignored for optimizations)
- Inlining Function Calls - even a simple function that returns 2 won't be treated the same as x = 2, 
  This hampers the ability to optimize since I don't optimize around function boundaries
  and this should be a relatively simple fix once I figure out the heuristics for inlining. 
- x86-64 specific optimizations like using lea, or bit-shifts. I don't expect this to make a huge impact. 
- Loop-unrolling - even simple constant iteration loops aren't unrolled which prevents optimisation 
- Reordering Instructions - even more optimizations could happen if I could reorder instructions
- Commmon Subexpression Elimination 

### Refactoring of the optimizations
I currently do a lot of cloning instead of taking owernship in my optimization phase.
If I took ownership then I expect a decent speedup as there's less allocation and de-allocations happening.

### Changes to the Typechecker 
I want to make the typechecker more powerful ie be able to infer types instead of always having to specify it. 
This would make the language way more ergonomic to use. I also think the typechecker code currently is very ugly and unwieldly.
I also would like to learn more about bidirectional type checking which then could enable generics to be added to the language.

### Improvements to Parsing 
Andrei did all of the parsing, however now that I'm not restrained by a specification, I'd love to change the syntax for my language
to suit my needs and also learn how to do parsing myself. Currrently I'm not happy with the speed of the compiler 
(though it is over 50x faster than my lecturers implementation of the language in Scala). It can't parse long expressions like 
1+2+3+4+...+10000 even though GCC and Clang don't suffer with this at all. Benchmarking shows that the slowdowns happen in both the middle-end
and the parser. So I would like to improve the speed of this but I'm not sure how to yet. One thing I can do now is remove runtime errors 
and potentially evaluate expressions in the parser instead without worrying about semantics. 


## Credits
Credit to group members [Andrei Cravtov](https://github.com/AndreiCravtov), and [Mikhail Bazarsadaev](https://github.com/detterdfd16)
They were responsible for the initial Frontend of this project including the uni-directional typechecker and Parser with some 
parts on the middle-end namely support for arrays/pairs.
