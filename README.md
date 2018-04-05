# CrowHttp
Http simple grammar using [Crowbar](https://github.com/stedolan/crowbar/issues)

## Use me
You need
- [Crowbar](https://github.com/stedolan/crowbar/issues)
- [AFL](http://lcamtuf.coredump.cx/afl/) (or using opam)
```
jbuilder build http_gen.exe
_build/default/http_gen.exe
```

## Objective
Use it on:
- [Cohttp](https://github.com/mirage/ocaml-cohttp)
- [Httpaf](https://github.com/inhabitedtype/httpaf)

=> Find messages that are not supported. Compare the output to find bugs.
