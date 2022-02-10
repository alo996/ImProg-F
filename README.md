# ImProg-F

Implementation of the functional programming language 'F' using Haskell. 


## Motivation

This group project was part of the practical 'Implementierung von Programmiersprachen' at the Ludwig-Maximilians-Universität in Munich, winter semester 2021/22. It introduced the participants to basic principles of formal languages, functional programming, parsing, compiler construction and abstract machines.


##  Getting started

- [GHC and Haskell](https://www.haskell.org/downloads/) (required)
- [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) (optional)


## Execution with Stack

Enter the directory where your Stack executable is located
```bash
stack run
```


## Highlights

- straightforward execution using the Stack build tool
- detailed error handling and error messages
- flag support for different levels of output verbosity
- coherent coding style (based on https://kowainik.github.io/posts/2019-02-06-style-guide)
- extensive commenting
- no known bugs


## Lowlight

This implementation surely is not the most efficient, e.g. O(n) list operations were often used in place of O(1) operations for convenience ((:) instead of (++)). Due to the nature of this project, which in our view serves to introduce students to the aforementioned theoretical concepts and not to perform resource intensive calculations, this aspect was deprioritized.


## Usage

- every F program needs to contain a definition 'main = ...;'
- 'main' is the only (lazyily) evaluated expression
- local definitions are possible but restricted to value definitions
- numeric values are restricted to integers of range [-2^29, 2^29-1]
- division is performed without remainder
- boolean values are lowercase
- no structured or enumerated types, and therefore also no pattern matching, are supported 
- lambda expressions and higher-order functions are not supported
- tail recursion is not supported
- no error handling for infinite recursion implemented


## Supported flags

- tokens
- ast
- instructions
- states

Use flags to print out generated tokens, the abstract syntax tree, MF instructions and intermediate MF states during program execution.

```bash
  stack run -- -flagName
```


## Authors

- [@hallers](https://gitlab2.cip.ifi.lmu.de/hallers)
- [@katzo](https://gitlab2.cip.ifi.lmu.de/katzo)
- [@ljubuncic](https://gitlab2.cip.ifi.lmu.de/ljubuncic)
- [@richard](https://gitlab2.cip.ifi.lmu.de/richard)
- [@suslov](https://gitlab2.cip.ifi.lmu.de/suslov)


## References

See the [homepage](https://uni2work.ifi.lmu.de/course/W21/IfI/ImProg) of the practical for more information, including the lecture notes 
[Übersetzerbau - Abstrakte Maschinen](https://uni2work.ifi.lmu.de/course/W21/IfI/ImProg/file/Skript/download/bry-eisinger-uebersaetzerbau--2004.pdf) on which this project heavily relies.

