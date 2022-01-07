# Pseudocompiler

A semi-working implementation of a slightly modified version of OCR's "Exam Reference Language"
(this was formerly known as the "Pseudocode specification"). I say slightly modified, because their
specification is not very clear about a lot of parts of the language (including the type system).

## Installation

You can currently build this from source using `cargo build`, but it is not yet ready for general
usage.

## Useful reading

Books that I have found helpful.

### Implementation
- [Rustc dev guide](https://rustc-dev-guide.rust-lang.org/) - contains useful details which are
also applicable to other languages
- [GNU reading list](https://gcc.gnu.org/wiki/ListOfCompilerBooks)
- Optimising Compilers for Modern Architectures (Randy Allen & Ken Kennedy)
- [A catalogue of optimising transformations](https://www.clear.rice.edu/comp512/Lectures/Papers/1971-allen-catalog.pdf)
- [Chris Fallin's series on code generation in Cranelift](https://cfallin.org/blog/2020/09/18/cranelift-isel-1/)
- [asmtutor.com](https://asmtutor.com)
- [Cornell lecture series on compilers](https://www.cs.cornell.edu/courses/cs6120/2020fa/self-guided/)
- [Cranelift demo JIT](https://github.com/bytecodealliance/cranelift-jit-demo)
- [Kixiron's rust-langdev compilation](https://github.com/Kixiron/rust-langdev)
- [Computer Organisation and Design - the hardware/software interface](http://home.ustc.edu.cn/~louwenqi/reference_books_tools/Computer_Organization_and_Design_3Rd.pdf) - David Patterson and John Hennessy

### Theory

These aren't actually _that_ relevant, but they were interesting.

- Practical Foundations for Programming Languages (Robert Harper)
- The Blind Spot (Jean-Yves Girard)

### Debugging
- Why Programs Fail - a guide to systematic debugging (Andreas Zeller)
- [Headcrab's reading list](https://github.com/headcrab-rs/headcrab/blob/master/Documentation/Resources.md)

### Performance
- [Flamegraph](https://github.com/flamegraph-rs/flamegraph)
- [DHAT](https://docs.rs/dhat/)

## Roadmap

See <ROADMAP.md> for details.
