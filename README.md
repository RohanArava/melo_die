# Melodie

My attempt at making a compiler

A minimal, experimental programming language and compiler focused on simplicity and clean grammar design. Written from scratch with the goal of being easy to parse, extend, and reason about.

---

## Features in `v0.1.0`

- Function definitions and recursive calls
- `if` / `else` conditionals
- `while` and `for` loops
- `let` variable declarations and reassignments
- Integer arithmetic and logical expressions
- Basic return statements
- Whitespace-insensitive syntax
- Simple parser-friendly grammar (no syntactic sugar)

---

## Example Program

```c
function fact(num) int {
    if num == 0 {
        return 1
    }
    return num * fact(num - 1)
}

function main() int {
    return fact(5)
}
````

---

## Grammar

Language syntax is intentionally minimal and straightforward, inspired by languages like C but with easy parsing in mind.

Grammar snapshot (simplified BNF):

```
<PROGRAM> ::= <FUNCTION_DECLARATION>*  
  
<FUNCTION_DECLARATION> ::= "function" <IDENTIFIER> "(" <PARAM_LIST>? ")" <RETURN_TYPE> "{" <STATEMENT>* "}"  
  
<STATEMENT> ::= <VARIABLE_DECLARATION>
              | <ASSIGNMENT>
              | <IF_STATEMENT>
              | <WHILE_STATEMENT>
              | <FOR_STATEMENT>
              | <RETURN_STATEMENT>
              | <EXPRESSION>
...
```

Full grammar details in [docs/grammar.md](docs/grammar.md).

---

## Roadmap

Planned features for future versions:

* [ ] Add more primitive types (`bool`, `float`, `char`)
* [ ] Better type checking
* [ ] Arrays and structs
* [ ] Improved error handling and diagnostics
* [ ] REPL and tooling

---

## Building and Running

```
cargo build
```
The compiler can be used like this:
```
Usage: melodie [OPTIONS] <SOURCE>

Arguments:
  <SOURCE>  Source file to compile

Options:
  -o, --output <OUTPUT>  Output file name [default: ]
      --ll               Produce LLVM IR (.ll file)
  -S                     Produce assembly (.s file)
  -c                     Compile to object file only (.o file)
  -h, --help             Print help
  -V, --version          Print version
```

---

## 📄 License

MIT License. See [LICENSE](LICENSE) for details.

---

## 🔗 Repository

GitHub: [github.com/RohanArava/melo\_die](https://github.com/RohanArava/melo_die)

---

