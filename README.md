[![Build Status](https://travis-ci.org/grin-compiler/idris-grin.svg?branch=master)](https://travis-ci.org/grin-compiler/idris-grin)

# Idris frontend

This is a proof of concept work. This repository sets an example how the grin compiler can be used to integrate with
compilers other than the lambda intermedate language which is used in the GHC-GRIN repository.

The 'test' directory contains the examples that the current version of the frontend supports, including both
the generated and optimised grin code for the idris programs. For example:
HelloWorld.idr, HellowWorld.grin, HelloWorld_opt.grin

How to run the idris frontend experiments on your own:
```
stack install
stack exec idris -- test/tdd/chapter01/01_HelloWorld.idr -i test/tdd/chapter01 --codegen grin -o helloworld.grin
```
