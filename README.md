# Fubuki

Yet another 6502 emulator.

Done:
-   assemble
-   disassemble
-   execute

TODO:
- [ ]   Add params to ByteInstruction
- [x]   Implement pipeline
- [ ]   Add support for `#LO` and `#HI`
- [x]   Implement offset immediate
- [x]   Remove builder pattern in ByteInstruction to guarantee RVO
- [ ]   Add instruction timing
- [ ]   Properly handle interrupt
- [ ]   Custom varaint (The current one is super heavy)
- [ ]   Size reduction
- [ ]   Make types formattable
- [ ]   inline whenever possible
- [ ]   Constexpr whenever possible
- [ ]   Simplify constraints and concepts
- [ ]   Compiler compatibility check
- [ ]   Complete JIT

### Why?

It was Thanksgiving, and I don't see many examples for many of latest C++ features.
The project as a whole is not production-ready, but there are some good C++ snippet.

### Configuration

-   Windows
-   Clang 19
-   MSVC STL

### References

-   Andrew Jacobs' Obelisk: http://6502.org/users/obelisk/
-   mass:werk 6502 Instruction Set: https://www.masswerk.at/6502/6502_instruction_set.html