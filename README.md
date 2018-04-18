# CrowHttp
What it is suppose to do: Finding bugs in http implementations such as cohttp and httpaf.

What it actually do: Bugging on its own !

## Principle
Http simple grammar using [Crowbar](https://github.com/stedolan/crowbar/)

## Maybe two modes incoming ?
- stdin/out
  - more efficient
  - super annoying to code (cohttp ikr)

- network
  - less efficient
  - super easy to code

## Use me
### Install - Dependencies
- [Crowbar](https://github.com/stedolan/crowbar/) (use the latest version)
- [AFL](http://lcamtuf.coredump.cx/afl/) (or using opam)

### How to
1. Compile the source and run the server
```
jbuilder build main.exe
jbuilder build test_server.exe
## launch the server in an other terminal
```

2. Launch the tests
  - Simple execution, without AFL `_build/default/main.exe`
  - With AFL (not tried yet) `afl-fuzz -i test/input -o output _build/default/main.exe`

## Objective
Use it on:
- [Cohttp](https://github.com/mirage/ocaml-cohttp)
- [Httpaf](https://github.com/inhabitedtype/httpaf)

=> Find messages that are not supported. Compare the output to find bugs.
