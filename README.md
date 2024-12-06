# Fubuki

Yet another 6502 emulator.

Done:
-   assemble
-   disassemble
-   execute

TODO:
- [ ]   Add params to ByteInstruction
- [ ]   Implement pipeline
- [ ]   Add support for `#LO` and `#HI`
- [ ]   Implement offset immediate
- [ ]   Custom varaint (The current one is super heavy)
- [ ]   Size reduction
- [ ]   Make types formattable
- [ ]   Complete JIT
- [ ]   inline whenever possible
- [ ]   Constexpr whenever possible
- [ ]   Simplify constraints and concepts
- [ ]   Compiler compatibility check

### Why?

It was Thanksgiving, and I don't see many examples for many of latest C++ features.

### Configuration

-   Windows
-   Clang 19
-   MSVC STL

### References

-   Andrew Jacobs' Obelisk: http://6502.org/users/obelisk/
-   mass:werk 6502 Instruction Set: https://www.masswerk.at/6502/6502_instruction_set.html