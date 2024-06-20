# Obx - Oberon+ Compiler
Obx is an implementation of the [Oberon+](https://oberon-lang.github.io/) compiler.

## Get Started
Obx is implemented in Go. You should have recent Go toolchain (Go >= 1.18) installed on your system. You can download
and install Go from the [website here](https://go.dev/doc/install).

### Build
```shell
# clone the project repository
$ git clone https://github.com/anthonyabeo/obx.git 

# change into the project directory root
$ cd obx

# run tests, build and install the executable 
$ go test ./... -v
$ go install cmd/obx.go
```
The executable should be installed in your `$GOBIN` directory. If you have not set the path for `$GOBIN`
do so before running the `install` command.


### Usage
```
$ obx -h

Usage:
  obx [command]

Available Commands:
  build       compile module or definition (along with its dependencies) into an object file
  completion  Generate the autocompletion script for the specified shell
  help        Help about any command

Flags:
  -h, --help   help for obx

Use "obx [command] --help" for more information about a command.
```

### Example
The program below is in the `examples/basics/Main.obx` file.
```
module Main
var a, b, total: integer

begin
    b := 10
    total := 0
    
    for a := 0 to b do
        total := total + 1
    end
    
    assert(total = 55)
end Main
```

```shell
$ obx build -e Main -p ./examples/basics --emit-ir
```
Output:
```
define i32 @main() {
%entry:
    br label %main

%main:
    %a = alloca i32
    %b = alloca i32
    %total = alloca i32
    store i32 10, ptr %b
    store i32 0, ptr %total
    %0 = load i32, ptr %b
    store i32 0, ptr %a
    %1 = load i32, ptr %a
    %2 = icmp slt i32 %1, %0
    br i1 %2, label %body, label %cont

%body:
    %3 = load i32, ptr %total
    %4 = add i32 %3, 1
    store i32 %4, ptr %total
    %5 = load i32, ptr %a
    %6 = add i32 %5, 1
    store i32 %6, ptr %a
    %7 = icmp slt i32 %a, %0
    br i1 %7, label %body, label %cont

%cont:
    %8 = load i32, ptr %total
    %9 = icmp eq i32 %8, 55
    %assert = call void assert(i1 %9)
    ret i32 0
}
```