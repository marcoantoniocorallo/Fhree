# Fhree

![](https://github.com/marcoantoniocorallo/Fhree/blob/main/cover_1.png)

---

Fhree is a *reference implementation* of a small *strongly typed* functional language, it is *interpreted* and it implements the *big-step operational* semantics.

It is the result of some excercises for the [*Languages, Compilers and Interpreters*](https://github.com/lillo/compiler-course-unipi) course @ [*UniPi*](https://di.unipi.it/), and it's an extension of the *FUN* language shown by [Prof. Galletta](https://github.com/lillo) during the lectures.

The language is strongly inspired by [*OCaml*](https://ocaml.org/), with some syntactical differences and simplifications.

The name Fhree is a word pun derived from the original language's name *FUN*.

Fun probably states for "*Functional*", but it can also mean "*having fun*", so the word pun would be "*Fun for fun*", whose acronym is *FFF* $\rightarrow$ *F3* $\rightarrow$ *F Three* $\rightarrow$ *F(h)ree*.

---

#### Syntax

Unlike in OCaml, there are no free variables. So there is no *let* construct but only *let-in*.

```ocaml
let x = 5 in x
```

For (also recursive) functions there is a construct *fun* similar to the OCaml's *let-rec*.

```ocaml
fun fact n = 
    if n = 0 then 1
    else n * fact (n - 1)
in fact 5
```

In order to have a *strong type system*, functions declaration make use of mandatory *type annotations*.

```ocaml
fun fact ( n : int ) : int = 
    if n = 0 then 1
    else n * fact (n - 1)
in fact 5
```

```ocaml
(lambda (s : string) : string -> " with annotation") "lambda"
```

Type annotations are available in every construct, but optionally.

#### Data Types

Fhree provides the most common data types: *integers*, *floats*, *chars*, *booleans* and *strings.* 

Furthermore, it provides **homogeneous** *lists* of values and **heterogeneous** *tuples*.

| Type    | Literal examples                   | Operators                   | Meaning                                                                                       |
|:-------:| ---------------------------------- | --------------------------- | --------------------------------------------------------------------------------------------- |
| int     | `-5`, `0`, `42`                    | `+` `-` `*` `/` `%`         | Arithmetic operations on ints                                                                 |
| float   | `0.15`, `.0002`,`0.1e-22`,         | `+.` `-.` `*.` `/.`         | Arithmetic operations on floats                                                               |
| string  | `"Hello World"`                    | `^`                         | Concatenation of strings                                                                      |
| boolean | `true`, `false`                    | `&&` `\|\|`                 |                                                                                               |
| tuple   | `('a', 0, "hi!")`,`(0,1)`          | `proj t i`                  | Projection of the *i*-th element of *t*                                                       |
| list    | `[2, 4, 6, 8]`, `[]`, `["Hello!"]` | `hd l` <br/>`tl`<br/>`e::l` | Get the first element of *l*<br/>Get *l* without the first element<br/>Add *e* in head of *l* |

#### Comments

There are both *C/Java*-like single-line comments and *OCaml*-like multi-line nested comments

`// this is a comment`

```
(* also
    (* this *) 
is a comment *)
```

#### Values

Expressible and denotable values are 

- integers

- booleans

- characters

- floats

- strings

- function closures
* tuples of values

* list of values

#### Control Flow Analysis

In addition to the *type analysis*, Fhree does a step of *control-flow-analysis*, using a *fix-point* algorithm. The result of the analysis of a file *f* is writed into a file *f*.cfa.

The CFA can be skipped passing the option `--no-cfa`, as you can read in the usage message. 

There is also an option to do **only** the CFA, using Fhree as an analyzer.

#### Requirements

Fhree is developed using OCaml and some OCaml tools for generating lexer and parser. For building the project, you must have these tools installed in your environment.

- *OCaml* and *opam*: follow the [official instruction](https://ocaml.org/docs/up-and-running#installation-for-unix) for you platform

- *Menhir* once you have installed opam, just run `opam install menhir`

#### Usage

To build *Fhree* just move in the directory and run `make`.

After that, you can run the interpreter of the language `./Fhree` with the following options.

```
Usage: Fhree [--no-cfa | --cfa | --all] filename.F3
Options:
 --all
    default option: does a control-flow-analysis of the code and prints the result into filename.cfa then executes the program;
 --no-cfa 
    executes only the program, without analyzing the code;
 --cfa
    executes only the control-flow analyzer, without executing the program;
```

---

#### To Do:

- Hexadecimal integers

- Free variables

- Unification algorithm for removing the mandatory type annotation in fun definitions

- Another unification algorithm for an implementation of *pattern matching*

- REPL

- Code generation for a simple compilation

- ~~Uncurried functions definitions~~ 
  Multiple-argument function definition are now available!
  They make use of *currying*: they are parsed and converted in the corresponding *curried* - single-argument - functions: a function f 
  
  ```ocaml
  fun f (a : t1) (b : t2) (c :t3) : tf = body;;
  ```
  
  is internally converted into
  
  ```ocaml
  fun f (a : t1) : (t2 -> t3 -> tf) -> 
      lambda (b : t2) : (t3 -> tf) -> 
          lambda (c : t3) : tf -> body
  ```
