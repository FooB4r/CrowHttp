# CrowHttp
What it is suppose to do: Finding bugs in http implementations such as cohttp and httpaf.

## Principle
Http simple grammar using [Crowbar](https://github.com/stedolan/crowbar/)

## Use me
### Install - Dependencies
*You'll need the right compiler with* `+afl`. -> `ocaml-variants.4.05.0+afl`

- [Crowbar](https://github.com/stedolan/crowbar/)
- [AFL](http://lcamtuf.coredump.cx/afl/) (or using opam)

- [Cohttp](https://github.com/mirage/ocaml-cohttp/)
- [Httpaf](https://github.com/inhabitedtype/httpaf/)

You'll need to use the latest version for Crowbar and Httpaf, the ones on opam are outdated.
```
opam install crowbar
opam pin add crowbar git://github.com/stedolan/crowbar.git

opam install httpaf
opam pin add httpaf git://github.com/inhabitedtype/httpaf
```

### How to
1. Compile the source
```
jbuilder build main.exe
```

2. Launch the tests
  - Simple execution, without AFL `_build/default/main.exe`
  - With AFL `afl-fuzz -i test/input -o output _build/default/main.exe @@`
  - With Parallel AFL using tmux `scripts/AFL_tmux.sh`
  - https://github.com/yomimono/ocaml-bun - Not tested (TODO)

## Objective
Use it on:
- [Cohttp](https://github.com/mirage/ocaml-cohttp)
- [Httpaf](https://github.com/inhabitedtype/httpaf)

=> Find messages that are not supported. Compare the output to find bugs.
