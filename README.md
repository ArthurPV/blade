# Lily

(works in progress)

## Dependencies

- LLVM
- ANSITerminal
- dune
- oUnit
- stdint

## How to install dependencies?
```bash
opam install llvm ANSITerminal dune oUnit stdint
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

### Function

```
sum :: i32 -> i32
fun sum x y = 
	x + y
end

sub :: i32 -> i32
fun sub x y = 
	x - y
end
```

### Variables

```
var b = 10
const a = 3
```
