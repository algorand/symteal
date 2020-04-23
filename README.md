[![Build Status](https://travis-ci.com/algorand/symteal.svg?branch=master)](https://travis-ci.com/algorand/symteal)
# Symteal

Symteal is a symbolic execution engine for Algorand Smart Contract and Transactions

## Installing Dependencies 

Symteal is built using Rosette. The easiest way to install Rosette is from Racket's package manager:

* Download and install Racket 7.0 or later from http://racket-lang.org

* Use Racket's `raco` tool to install Rosette and lens

  ```
  $ raco pkg install rosette
  $ raco pkg install lens
  ```

* For better solver backend, install [Boolector 3.1.0](https://boolector.github.io/)