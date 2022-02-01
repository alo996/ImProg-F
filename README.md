## ImProg-F

Implementation of the functional programming language 'F' using Haskell. 


## Motivation

This group project was part of the practical 'Implementierung von Programmiersprachen' at the Ludwig-Maximilians-Universität in Munich, winter semester 2021/22. It introduced the participants to basic principles of formal languages, functional programming, parsing, compiler construction and abstract machines.


##  Installation


## Highlights

- straightforward execution using the Stack build tool
- simple user interaction
- detailed error handling and error messages
- flag support for differnt levels of output verbosity 
- coherent coding style (based on https://kowainik.github.io/posts/2019-02-06-style-guide)
- extensive commenting 
- flags inside stack for every module, including tests ??? 
- no known bugs


## Lowlight

We are aware that this implementation might not be the most efficient, e.g. we often use O(n) list operations where one could have used O(1) operations instead (like (:) instead of (++)). Due to the nature of this project, which in our view serves to introduce students to the aforementioned theoretical concepts and not to perform resource intensive calculations, we depriorized this aspect and simlply had not enough time to take care of it before submission.


## Authors

- [@hallers](https://gitlab2.cip.ifi.lmu.de/hallers)
- [@katzo](https://gitlab2.cip.ifi.lmu.de/katzo)
- [@ljubuncic](https://gitlab2.cip.ifi.lmu.de/ljubuncic)
- [@richard](https://gitlab2.cip.ifi.lmu.de/richard)
- [@suslov](https://gitlab2.cip.ifi.lmu.de/suslov)

## Table of Contents

- Declarations.hs
- Store.hs
- Tokenizer.hs
- Parser.hs
- Compiler.hs
- MF.hs
- Main.hs
- Stack file version

## Running Tests

To run tests (by using Stack), run the following command 

```bash
  stack run --test
```

## References

See the [homepage](https://uni2work.ifi.lmu.de/course/W21/IfI/ImProg) of the practical for more information, including the script 
[Übersetzerbau - Abstrakte Maschinen](https://uni2work.ifi.lmu.de/course/W21/IfI/ImProg/file/Skript/download/bry-eisinger-uebersaetzerbau--2004.pdf) on which this project heavily relies.

