# CrowHttp
Http simple grammar using [Crowbar](https://github.com/stedolan/crowbar/)

## Use me
You need
- [Crowbar](https://github.com/stedolan/crowbar/)
- [AFL](http://lcamtuf.coredump.cx/afl/) (or using opam)
```
jbuilder build http_gen.exe
afl-fuzz -i test/input -o output _build/default/http_gen.exe
```

## Objective
Use it on:
- [Cohttp](https://github.com/mirage/ocaml-cohttp)
- [Httpaf](https://github.com/inhabitedtype/httpaf)

=> Find messages that are not supported. Compare the output to find bugs.
