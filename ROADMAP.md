# Roadmap

## Core compiler

### Definitely planned

 - [ ] Heap allocation and data structures (i.e. lists/vectors/arrays)
 - [ ] Strings (these are just lists of Unicode characters)
 - [ ] Tagged unions (these are definitely not in the specification, but they're great)
 - [ ] Record types (also not in the specification, but still great)
 - [ ] Pattern matching on tagged unions
 - [ ] Automatic memory management (ideally using
 [Perseus](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf),
 but I might implement a simple tracing garbage collector to begin with)
 - [ ] Function inlining
 - [ ] Mutual recursion (this isn't major - we'll just inline one of the functions)

### Planned, but not in the short term
 - [ ] Making the compiler query-based
 - [ ] Adding IDE support

### Things to experiment with
 - [ ] Automatic optimisation using a genetic algorithm or simulated annealing to find optimal
 settings

## Online playground

## Definitely planned

 - [ ] Add a simple online playground for people to use the compiler

## Things to experiment with

 - [ ] Allow people to optionally submit their error messages for further analysis
