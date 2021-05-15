# Lily

(works in progress)

## Dependencies

- LLVM
- ANSITerminal
- containers
- dune
- oUnit

## How to install dependencies?
```bash
opam install llvm ANSITerminal containers dune oUnit
```

## How to build?
``` bash
cd lily
make
```

## How to test?
```bash
cd lily
make
make test
```
## Examples

```
sum :: i32 -> i32
fun sum x y = 
	x + y
end
```
