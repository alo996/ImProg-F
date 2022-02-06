# ImProg-F

Implementation of the functional programming language 'F' using Haskell. 


## Motivation

This group project was part of the practical 'Implementierung von Programmiersprachen' at the Ludwig-Maximilians-Universität in Munich, winter semester 2021/22. It introduced the participants to basic principles of formal languages, functional programming, parsing, compiler construction and abstract machines.

## Notes

- Int in Haskell is limited to the range of -2^29 to 2^29 - 1, and therefore, so are the values in our project
- the result of a division is always an Integer
- multiple operators without a bracket are not allowed (--, ++++,...)
- no higher-order functions allowed


##  Installations Needed for Execution 

### Basic Functionality:

- [GHC and Haskell](https://www.haskell.org/downloads/)

### Additional Functionality:

- [The Haskell Tool Stack](https://www.haskell.org/downloads/)

## Usage (with Stack)

- enter the directory where your Stack executable is located
```bash
stack run
```
(Output: "Please enter an F program and hit enter (end with an empty line):")
```bash
main = 9 + 10;

```
(Output:  "---> Result: 19"  
          "Another one? [y/n]")      

"y" input starts the program again, "n" ends the execution.


## Highlights

- straightforward execution using the Stack build tool
- detailed error handling and error messages
- flag support for different levels of output verbosity
- coherent coding style (based on https://kowainik.github.io/posts/2019-02-06-style-guide)
- extensive commenting
- no known bugs


## Lowlight

This implementation might not be the most efficient, e.g. O(n) list operations were often used in place of O(1) operations (e.g. (:) instead of (++)). Due to the nature of this project, which serves to introduce students to the aforementioned theoretical concepts and not to perform resource intensive calculations, this aspect was deprioritized.


## Authors

- [@hallers](https://gitlab2.cip.ifi.lmu.de/hallers)
- [@katzo](https://gitlab2.cip.ifi.lmu.de/katzo)
- [@ljubuncic](https://gitlab2.cip.ifi.lmu.de/ljubuncic)
- [@richard](https://gitlab2.cip.ifi.lmu.de/richard)
- [@suslov](https://gitlab2.cip.ifi.lmu.de/suslov)


## Using flags

Available flags:
- tokens
- parse 
- instructions
- states

The flags output the list of tokens, parsing output, instructions in MF, and states in MF respectively.

```bash
  stack run -- -flagName
```

## References

See the [homepage](https://uni2work.ifi.lmu.de/course/W21/IfI/ImProg) of the practical for more information, including the lecture notes 
[Übersetzerbau - Abstrakte Maschinen](https://uni2work.ifi.lmu.de/course/W21/IfI/ImProg/file/Skript/download/bry-eisinger-uebersaetzerbau--2004.pdf) on which this project heavily relies.

