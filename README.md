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
$ obx build -e Main -p ./examples/basics --emit-ir --opt mem2reg
```
Output:
```
define i32 @main() {
%entry:
        br label %main

%main:
        %2 = icmp slt i32 0, 10
        br i1 %2, label %body, label %cont

%body:
        %a = phi i32 [ 0, %main ], [ %6, %body ]
        %total = phi i32 [ 0, %main ], [ %4, %body ]
        %4 = add i32 %total, 1
        %6 = add i32 %a, 1
        %7 = icmp slt i32 %a, 10
        br i1 %7, label %body, label %cont

%cont:
        %a = phi i32 [ %6, %body ], [ 0, %main ]
        %total = phi i32 [ %4, %body ], [ 0, %main ]
        %9 = icmp eq i32 %total, 55
        %assert = call void assert(i1 %9)
        ret i32 0
}

```