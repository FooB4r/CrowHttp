# CrowHttp
What it is suppose to do: Finding bugs in http implementations such as cohttp and httpaf.

Being slow

## Principle
Http simple grammar using [Crowbar](https://github.com/stedolan/crowbar/)

## Use me
### Install - Dependencies
- [Crowbar](https://github.com/stedolan/crowbar/) (use the latest version on github)
- [AFL](http://lcamtuf.coredump.cx/afl/) (or using opam)

### How to
1. Compile the source
```
jbuilder build main.exe
```

2. Launch the tests
  - Simple execution, without AFL `_build/default/main.exe`
  - With AFL `afl-fuzz -i test/input -o output _build/default/main.exe`
  - With Parallel AFL using tmux `scripts/AFL_tmux.sh`

## Objective
Use it on:
- [Cohttp](https://github.com/mirage/ocaml-cohttp)
- [Httpaf](https://github.com/inhabitedtype/httpaf)

=> Find messages that are not supported. Compare the output to find bugs.
